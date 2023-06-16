;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "SASM", presented by the Esolang user "Fuck!" in the year
;; 2018, whose conception is founded upon simplified assembly code.
;; 
;; 
;; Concept
;; =======
;; The SASM programming language derives from the requirements of
;; assembly code, however, its capabilities having been inclemently
;; reduced to assignments, basic arithmetics, and conditional jumping,
;; while its memory model founds upon four registers and an infinite
;; memory, both invested with the storage of scalar integers of any sign
;; and magnitude.
;; 
;; == SASM: SIMPLE ASSEMBLY LANGUAGE ==
;; The agnomination "SASM" most probably derives from its state as a
;; (S)imple (ASM), or (S)imple (A)(S)e(M)bly language, a euonym that
;; bewrays the encompassing concept as thoroughly abbridged variant of
;; the assembly notions.
;; 
;; == FOUR REGISTERS: FOUR INTEGER SCALARS ==
;; SASM enables the utilization of four registers, in their
;; preponderance of Procrustean utility, except for the "bx" member's
;; particular use case that capacitates its supererogation as both a
;; dedicated salvatory and the sole medium for accessing the program
;; memory.
;; 
;;   ------------------------------------------------------------------
;;   Register | Note
;;   ---------+--------------------------------------------------------
;;   ax       | -
;;   ..................................................................
;;   bx       | Permits the assignment of an object's address as [bx].
;;   ..................................................................
;;   cx       | -
;;   ..................................................................
;;   dx       | -
;;   ------------------------------------------------------------------
;; 
;; == PROGRAM MEMORY: AN INFINITE INTEGER VECTOR ==
;; The program memory provides a composition of a theoretically
;; inexhaustible tally of integer-valued cells, amenable to non-negative
;; addresses.
;; 
;; == JUMPING VIA TAGS ==
;; Control flow helming mechanisms are realized in the language via the
;; conditional goto instructions "je" and "jne", both of which are
;; related to the notion of tags, defining labels in the code for
;; navigational referrals.
;; 
;; 
;; Architecture
;; ============
;; SASM's architectural design is defined by the coefficacy of four
;; registers and a theoretically infinite memory, both composed of
;; unbounded scalar integers.
;; 
;; == REGISTERS ==
;; Four registers offer themselves to the programmer's avail, enumerated
;; using the designations "ax", "bx", "cx", and "dx", ligated in
;; equipollence by storing a scalar integer of any sign and magnitude
;; each. A kenspeckle utility resides in the "bx" register, however, as
;; its invocation in brackets, requested through the syntax "[bx]",
;; homologates an adit to the memory for reading and writing purposes.
;; 
;; All registers assume at the program's inchoation the default value of
;; zero (0).
;; 
;; == MEMORY ==
;; The program memory is represented by a contingently infinite expanse
;; of integer-valued cells, generous in the numeric objects' extent
;; siclike to the registers. Every cell is amenable to a non-negative
;; integer address that refers to it in an unambiguous manner and
;; enumerates the units in a consecutive ordonnance.
;; 
;; The aefauld medium to the memory's indagation and manipulation is
;; vouchsafed by the "bx" register in its pointer form "[bx]".
;; 
;; All memory cells are initially set to the default value zero (0).
;; 
;; 
;; Data Types
;; ==========
;; Its very primitive nature endows SASM merely with one set of objects:
;; the infinite group of signed integers, unobstructed by any boundary
;; towards the lower or upper stratum.
;; 
;; 
;; Syntax
;; ======
;; A piece of SASM code ostends a composition of zero or more lines, the
;; non-vacant specimens among which encompass exactly a solitary
;; instruction, itself either defined by an identifier and zero or more
;; operands, or providing a tag, or label, followed by a colon (":").
;; 
;; == INSTRUCTIONS ==
;; Every instruction inhabits a line of its own, differentiated from its
;; accolent peers by at least one linebreak.
;; 
;; Instructions are introduced by adminiculum of their identifying name,
;; in the case of effective operations, followed by zero or more
;; comma-separated operands; tag definitions, as a specialized scion,
;; eschew such inputs and in lieu of these demand a colon (":"). The
;; instruction line is concluded either by a linebreak or the end of the
;; program.
;; 
;; == OPERANDS ==
;; The operands' presence and tally appertains to the context of the
;; applying instruction, permitting a contingency of four manifestations
;; for the same:
;; 
;;   (a) Literal integers
;;   (b) Registers
;;   (c) Memory-addressing registers
;;   (d) Tag names
;; 
;; Literal integers (a) assume the form of signed or unsigned decimal
;; digits.
;; 
;; Registers in their primary expression (b) are enumerated by the four
;; members "ax", "bx", "cd", and "dx".
;; 
;; Offering a particular use case, the "bx" register may be ensconed in
;; a bracket jumelle, "[" and "]", in order to signify a memory address
;; (c).
;; 
;; Tag names (d) constitute a character sequence inchoated by a Latin
;; letter or an underscore, succeeded by zero or more letters,
;; underscores, or decimal digits.
;; 
;; == NEWLINES ==
;; Linebreaks apply themselves to the segregation of instructions.
;; Vacant lines, comprehending spaces only, may be inserted liberally.
;; 
;; == SPACES ==
;; The insertion of spaces and horizontal tabs states an obligation in
;; betwixt an instruction and its incipient operand; on the other hand,
;; any other location administers tolerance but no imposition for such
;; sepiments.
;; 
;; == COMMENTS ==
;; The provision for comments does not tally among the facilities of the
;; current SASM language iteration.
;; 
;; == GRAMMAR ==
;; The following formulation in the Extended Backus-Naur Form (EBNF)
;; serves as a formal elucidation of the syntactical aspects:
;; 
;;   program            := emptyLines
;;                      ,  [ command , followingCommands ]
;;                      ,  emptyLines
;;                      ;
;;   subsequentCommands := { newline , emptyLines , command } ;
;;   command            := mov
;;                      |  add
;;                      |  sub
;;                      |  cmp
;;                      |  je
;;                      |  jnz
;;                      |  tagDefinition
;;                      ;
;;   mov                := "mov" , destination , "," , source ;
;;   add                := "add" , destination , "," , source ;
;;   sub                := "sub" , destination , "," , source ;
;;   cmp                := "cmp" , source "," , source , ","
;;                      ,          destination ;
;;   jne                := "jne" , source , "," , source , "," , tag ;
;;   je                 := "je"  , source , "," , source , "," , tag ;
;;   tagDefinition      := tag , ":" ;
;;   
;;   tag                := tagFirstChar , { tagSubsequentChar } ;
;;   tagFirstChar       := letter | "_" ;
;;   tagSubsequentChar  := letter | "_" | digit ;
;;   destination        := register | registerAsAddress ;
;;   source             := register | registerAsAddress | integer ;
;;   registerAsAddress  := "[" , "bx" , " ]" ;
;;   register           := "ax" | "bx" | "cx" | "dx" ;
;;   letter             := "a" | ... | "z" | "A" | ... | "Z" ;
;;   integer            := [ "+" | "-" ] , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   emptyLines         := { newline } ;
;;   newline            := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; SASM's competence is exhausted by assignments, addition and
;; subtraction, as well as two conditional program navigation
;; instructions; no participation by input and output conduits is
;; provided.
;; 
;; == OVERVIEW ==
;; An apercu anenst SASM's instruction set shall serve in the adhibition
;; of a basic nortelry. Please note that the referral of an "address" as
;; an operand refers to the "bx" register in its indirect memory
;; addressing form "[bx]".
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ------------------------+-----------------------------------------
;;   mov target, source      | Stores the SOURCE value in the TARGET
;;       ******  ******      | register.
;;                           |-----------------------------------------
;;                           | The TARGET must be a register name or an
;;                           | address.
;;                           |-----------------------------------------
;;                           | The SOURCE must be a literal integer, a
;;                           | register name, or an address.
;;   ..................................................................
;;   add target, source      | Increments the TARGET register by the
;;       ******  ******      | SOURCE value.
;;                           |-----------------------------------------
;;                           | The TARGET must be a register name or an
;;                           | address.
;;                           |-----------------------------------------
;;                           | The SOURCE must be a literal integer, a
;;                           | register name, or an address.
;;   ..................................................................
;;   sub target, source      | Decrements the TARGET register by the
;;       ******  ******      | SOURCE value.
;;                           |-----------------------------------------
;;                           | The TARGET must be a register name or an
;;                           | address.
;;                           |-----------------------------------------
;;                           | The SOURCE must be a literal integer, a
;;                           | register name, or an address.
;;   ..................................................................
;;   cmp left, right, target | Determines whether the LEFT operand
;;       ****  *****  ****** | equals the RIGHT operand, on
;;                           | confirmation storing the value 1 in the
;;                           | TARGET register, otherwise the value 0.
;;                           |-----------------------------------------
;;                           | The LEFT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The RIGHT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The TARGET must be a register name or an
;;                           | address.
;;   ..................................................................
;;   je left, right, tag     | Determines whether the LEFT operand
;;      ****  *****  ***     | equals the RIGHT operand, on
;;                           | confirmation relocating the instruction
;;                           | pointer (IP) to the TAG. Aliter,
;;                           | proceeds without effect.
;;                           | If the TAG cannot be found in the
;;                           | program, an error of an unspecified type
;;                           | is signaled.
;;                           |-----------------------------------------
;;                           | The LEFT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The RIGHT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The TAG must be a valid label name. If
;;                           | none such exists, an error of an
;;                           | unspecified type is signaled.
;;   ..................................................................
;;   jne left, right, tag    | Determines whether the LEFT operand
;;       ****  *****  ***    | differs from the RIGHT operand, on
;;                           | confirmation relocating the instruction
;;                           | pointer (IP) to the TAG. Aliter,
;;                           | proceeds without effect.
;;                           | If the TAG cannot be found in the
;;                           | program, an error of an unspecified type
;;                           | is signaled.
;;                           |-----------------------------------------
;;                           | The LEFT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The RIGHT operand must be a literal
;;                           | integer, a register name, or an address.
;;                           |-----------------------------------------
;;                           | The TAG must be a valid label name. If
;;                           | none such exists, an error of an
;;                           | unspecified type is signaled.
;;   ..................................................................
;;   tag:                    | Defines a tag (label) identified by the
;;   ***                     | name TAG.
;;                           |-----------------------------------------
;;                           | The TAG must be a valid label name.
;;   ------------------------------------------------------------------
;; 
;; == TAGS ==
;; Tags, or labels, can be defined in precedence of instructions as a
;; medium for demarcating jump points in a program.
;; 
;; A tag's validity inchoates with its statement location and propagates
;; to the subsequent tag definition, or the end of the code if none
;; follows. The delineation, however, induces merely a logical effect
;; and is not entalented with physical consequences. Even if relayed to
;; by a redirection aliunde, the program flow passes through all
;; instructions, tallying also those of contingent successor tag
;; sections.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; SASM's protolog, originally suspended in the state of compendious
;; treatment and scant exposition in examples, suffers from several
;; impositions concerning its details. The following sections shall
;; adduce merely a subset for disquisition.
;; 
;; == HOW ARE INTEGERS COMMUNICATED? ==
;; Literal integers establish the currency of the immediate addressing
;; mode; natheless, their concrete form, especially with respect to the
;; number system, constitutes a lacuna in the description.
;; 
;; It has been adjudged a vehicle of direct and convenient conveyance to
;; define all integers in the decimal form.
;; 
;; == WHAT REQUIREMENTS REFER TO THE TAG NAMING? ==
;; The tag's appropriation of the label role assigns to it the onus of
;; defining program jump points. The concrete syntax for such
;; identifiers meanwhile eludes the specification.
;; 
;; It has been adjudged, as a medium of alignment betwixt the SASM's
;; express simplicity and the syntactical department, to permit only
;; non-keyword identifiers as tag names, incited via a Latin letter or
;; an underscore, and proceeding with zero or more elements from the
;; same set, extended by decimal digits as a further option.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in Common Lisp, employing for
;; epideictic purposes special variables as surrogates for classes and
;; parameters to functions.
;; 
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; It constitutes a peisant element of gnarity to remember that special
;; variables, ligated into a consanguinity with global variables as a
;; general species, and exacerbated by their implicit and contingently
;; arbitrary declarations, merit the wite of encumbering programs with
;; superfluous complexity. For a more detailed treatise on the
;; contingency for detriments incurred by this feature please refer to
;; [stackoverflow2019q56725814].
;; 
;; == SPECIAL VARIABLES ==
;; The faculty contained in the notion of special variables, apprehended
;; in a very curtailed and simplified manner, introduces globally active
;; variables that can only be accessed if explicitly requested. In a
;; limited perspective, this kenspeckle dation to Common Lisp's
;; abilities entertains a loose correspondence to the C programming
;; language's static counterpart, where global and local availability
;; intrinsically cooperate.
;; 
;;   ------------------------------------------------------------------
;;   Special var.    | Role
;;   ==================================================================
;;                          LEXICAL ANALYZATION
;;   ------------------------------------------------------------------
;;   source          | TYPE: string
;;                   |-------------------------------------------------
;;                   | PURPOSE: The piece of SASM code to evaluate.
;;   ..................................................................
;;   position        | TYPE: fixnum
;;                   |-------------------------------------------------
;;                   | PURPOSE: The index of the current CHARACTER in
;;                   |   the source.
;;   ..................................................................
;;   character       | TYPE: character
;;                   |-------------------------------------------------
;;                   | PURPOSE: The currently processed character from
;;                   |   the SOURCE, located at the POSITION into the
;;                   |   same.
;;   ==================================================================
;;                                PARSING
;;   ------------------------------------------------------------------
;;   current-token   | TYPE: token
;;                   |-------------------------------------------------
;;                   | PURPOSE: The most recently queried token from
;;                   |   the lexical analyzation stage.
;;   ==================================================================
;;                             INTERPRETATION
;;   ------------------------------------------------------------------
;;   instructions    | TYPE: vector of instructions
;;                   |-------------------------------------------------
;;                   | PURPOSE: Represents the SASM SOURCE code in a
;;                   |   form transformed for facilitated evaluation.
;;   ..................................................................
;;   ip              | TYPE: fixnum
;;                   |-------------------------------------------------
;;                   | PURPOSE: The index into the INSTRUCTIONS vector,
;;                   |   designating the currently processed
;;                   |   instruction.
;;   ..................................................................
;;   tags            | TYPE: hash table
;;                   |-------------------------------------------------
;;                   | PURPOSE: Maps the name of each tag definition in
;;                   |   the INSTRUCTIONS vector to its index in the
;;                   |   same, thus permitting efficient navigation.
;;   ..................................................................
;;   registers       | TYPE: association list
;;                   |-------------------------------------------------
;;                   | PURPOSE: Maps each of the four register names to
;;                   |   its value, represented by a signed integer.
;;   ..................................................................
;;   memory          | TYPE: hash table
;;                   |-------------------------------------------------
;;                   | PURPOSE: Represents the program memory as a
;;                   |   sparse, potentially infinite memory of cells,
;;                   |   the addresses being represented by
;;                   |   non-negative integer, mapping to the arbitrary
;;                   |   signed integer cell values.
;;   ..................................................................
;;   maximum-address | TYPE: non-negative integer
;;                   |-------------------------------------------------
;;                   | PURPOSE: Maintains the largest address in the
;;                   |   program MEMORY accessed; utilized solely for
;;                   |   printing the MEMORY.
;;   ------------------------------------------------------------------
;; 
;; == ADDRESSING MODES ==
;; Several addressing modes experience their deployment in assembly
;; programming, the treble of exclusive utility shall be limned in a
;; cursory manner by the following table:
;; 
;;   ------------------------------------------------------------------
;;   Addressing mode            | Effect
;;   ---------------------------+--------------------------------------
;;   Immediate addressing       | Employs literal objects, frequently
;;                              | integers, as one or more operands.
;;   ..................................................................
;;   Register addressing        | Employs access to registers in one or
;;                              | more operands, either for reading
;;                              | from, writing to, or both directions.
;;   ..................................................................
;;   Indirect memory addressing | Employs registers in order to enable
;;                              | access to the program memory.
;;   ------------------------------------------------------------------
;; 
;; With a more concrete cynosure, the mapping of these addressing modes
;; to representative Common Lisp classes is administered by these
;; relationships:
;; 
;;   -------------------------------------------------
;;   Addressing mode            | Representative class
;;   ---------------------------+---------------------
;;   Immediate addressing       | Literal-Operand
;;   .................................................
;;   Register addressing        | Register-Operand
;;   .................................................
;;   Indirect memory addressing | Address-Operand
;;   -------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-06-13
;; 
;; Sources:
;;   [esolang2020SASM]
;;   The Esolang contributors, "SASM", 2020
;;   URL: "https://esolangs.org/wiki/SASM"
;;   
;;   [stackoverflow2012q41091118]
;;   The Stack Overflow contributors,
;;     "What's the canonical way to join strings in a list?", 2012
;;   URL: "https://stackoverflow.com/a/41091118"
;;   Notes:
;;     - Demonstrates the usance of special variables in the context of
;;       the ``format'' function.
;;   
;;   [stackoverflow2019q56725814]
;;   The Stack Overflow contributors, "Using Local Special Variables",
;;     2019
;;   URL: "https://stackoverflow.com/questions/56725814/
;;         using-local-special-variables"
;;   Notes:
;;     - Discusses the disadvantages of special variables, which
;;       comprehend:
;;        o Lack of referential transparency, ...
;;          ... which renders it more difficult to reason functionally
;;          about one's code, meaning that functions may produce
;;          different results with syntactically equivalent calls.
;;        o Introduction of bugs, ...
;;          ... as lexical variable at other locations in the code,
;;          e.g. in a system function, will be overwritten.
;;        o Confusion ...
;;          .. for readers unacquainted with special (dynamic) binding
;;        o Dubious necessity, ...
;;          ... as lexical binding or even anaphoric macros may be
;;          utilized instead.
;;   
;;   [tutorialspoint2023assemblyaddrmodes]
;;   The Tutorials Point contributors, "Assembly - Addressing Modes",
;;     2023
;;   URL: "https://www.tutorialspoint.com/assembly_programming/
;;         assembly_addressing_modes.htm"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Configuration of package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ascertain that reserved Common Lisp identifiers, such as "position"
;; and "character", may be utilized as special variable names.
#+sbcl (unlock-package :cl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   \"alist\", composed of zero or more entries, each key of which
   conforms to the KEY-TYPE, associated with a value of the VALUE-TYPE,
   both defaulting to the comprehensive ``T''."
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
                (or (null element)
                    (typep element `(cons ,key-type ,value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

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

(deftype token ()
  "The ``token'' type defines a token as a tuple of (type, value),
   realized by means of a cons, the left compartment of which stores the
   keyword symbol as its type, accompanied by the token value commorant
   in the dextral moiety."
  '(cons keyword T))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' defines a mapping of SASM language keywords
   to representative tokens, manifesting in the form of an association
   list (alist) that affiliates the name strings to ``token'' objects."
  '(association-list-of string token))

;;; -------------------------------------------------------

(deftype register ()
  "The ``register'' type enumerates the recognized register identifiers
   in the form of keyword symbols."
  '(member :ax :bx :cx :dx))

;;; -------------------------------------------------------

(deftype register-set ()
  "The ``register-set'' type defines a mapping of register identifiers
   to their current values, their reificiation chosen as an association
   list (alist) that maps ``register'' objects to signed integers."
  '(association-list-of register integer))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized variants of
   SASM instructions."
  '(member :mov :cmp :add :sub :je :jne :tag))

;;; -------------------------------------------------------

(deftype actual-parameter-list ()
  "The ``actual-parameter-list'' type defines an instruction's operands
   as a list of zero or more ``Operand'' instances."
  '(list-of Operand))

;;; -------------------------------------------------------

(deftype formal-parameter-list ()
  "The ``formal-parameter-list'' defines an instruction's signature with
   respect to the expected parameters as a list of zero or more Common
   Lisp forms, arbitrary in their nature by being of the ``T'' type, yet
   representing valid type specifiers."
  '(list-of T))

;;; -------------------------------------------------------

(deftype signature-table ()
  "The ``signature-table'' defines a mapping of instruction types to
   the formal parameters in the form of a hash table that affiliates
   ``instruction-type'' object to ``formal-parameter-list'' items."
  '(hash-table-of instruction-type formal-parameter-list))

;;; -------------------------------------------------------

(deftype sasm-program ()
  "The ``sasm-program'' type defines an executable SASM program as a
   vector of zero or more ``Instruction'' objects."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype address ()
  "The ``address'' type represents a memory address in the form of an
   unsigned non-negative integer, encumbered by no upper bound."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   integer-valued cells amenable to non-negative integer addresses,
   represented by a hash table that associated ``address'' objects to
   integer values."
  '(hash-table-of address integer)) 

;;; -------------------------------------------------------

(deftype tag-table ()
  "The ``tag-table'' type defines a mapping of tag names (labels) to
   their position in the SASM program, manifested as a hash table whose
   string keys provide the tags, affiliated with fixnum subscripts into
   a ``sasm-program''."
  '(hash-table-of string fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-type (token)
  "Returns the TOKEN's type."
  (declare (type token token))
  (the keyword
    (car token)))

;;; -------------------------------------------------------

(defun token-value (token)
  "Returns the TOKEN's value."
  (declare (type token token))
  (the T
    (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("mov" . (:mov      . "mov"))
    ("cmp" . (:cmp      . "cmp"))
    ("je"  . (:je       . "je"))
    ("jne" . (:jne      . "jne"))
    ("add" . (:add      . "add"))
    ("sub" . (:sub      . "sub"))
    ("ax"  . (:register . :ax))
    ("bx"  . (:register . :bx))
    ("cx"  . (:register . :cx))
    ("dx"  . (:register . :dx)))
  "Affiliates the recognized identifier names with representative tokens
   in an association list (alist).")

;;; -------------------------------------------------------

(defun get-identifier (name)
  "Returns the SASM keyword token associated with the NAME, or produces
   and responds with a fresh ``:identifier'' token enveloping the NAME."
  (declare (type string name))
  (the token
    (or (cdr (assoc name +IDENTIFIERS+ :test #'string=))
        (cons :identifier name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents an identifier
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (char= candidate #\_))))))

;;; -------------------------------------------------------

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents an arithmetic sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\+)
          (char= candidate #\-))))))

;;; -------------------------------------------------------

(defun initialize-lexer ()
  "Initializes the lexer for a fresh analyzation process and returns no
   value."
  (declare (special source))
  (declare (special position))
  (declare (special character))
  (setf character
    (when (array-in-bounds-p source position)
      (char source position)))
  (values))

;;; -------------------------------------------------------

(defun advance ()
  "Returns the current CHARACTER in the SOURCE, before moving the
   POSITION cursor to the next character in the SOURCE, if possible, and
   the current CHARACTER to the new location."
  (declare (special source))
  (declare (special position))
  (declare (special character))
  (the (or null character)
    (prog1 character
      (setf character
        (when (array-in-bounds-p source (1+ position))
          (char source (incf position)))))))

;;; -------------------------------------------------------

(defun read-identifier ()
  "Starting at the current CHARACTER in the SOURCE, reads an identifier
   and returns keyword token or a general ``:identifier'' token
   representation thereof."
  (declare (special character))
  (the token
    (get-identifier
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop
          while (and character (identifier-character-p character))
          do    (write-char (advance) identifier))))))

;;; -------------------------------------------------------

(defun read-integer ()
  "Starting at the current CHARACTER in the SOURCE, reads a signed or
   unsigned decimal number and returns an ``:integer'' token
   representation thereof."
  (declare (special character))
  (the token
    (cons :integer
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (when (sign-character-p character)
            (write-char (advance) digits))
          (loop while (and character (digit-char-p character)) do
            (write-char (advance) digits)))))))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Skips a sequence of zero or more adjacent spaces in the SOURCE and
   returns no value."
  (declare (special character))
  (loop while (and character (space-character-p character)) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun get-next-token ()
  "Returns the next token from the lexer.
   ---
   Upon its SOURCE's exhaustion, the lexer responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (special position))
  (declare (special character))
  (the token
    (cond
      ((null character)
        (cons :eof NIL))
      
      ((space-character-p character)
        (skip-spaces)
        (get-next-token))
      
      ((char= character #\Newline)
        (cons :newline (advance)))
      
      ((char= character #\,)
        (cons :comma (advance)))
      
      ((char= character #\[)
        (cons :left-bracket (advance)))
      
      ((char= character #\])
        (cons :right-bracket (advance)))
      
      ((char= character #\:)
        (cons :colon (advance)))
      
      ((or (alpha-char-p character)
           (char= character #\_))
        (read-identifier))
      
      ((digit-char-p character)
        (read-integer))
      
      ((sign-character-p character)
        (read-integer))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          character position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' abstract class represents the foundry for all species
   of instruction operands."
  (type (error "Missing operand type.") :type keyword))

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:include     Operand)
  (:constructor make-literal-operand (value &aux (type :literal))))
  "The ``Literal-Operand'' class represents a literal integer employed
   as an instruction operand."
  (value 0 :type integer))

;;; -------------------------------------------------------

(defstruct (Register-Operand
  (:include     Operand)
  (:constructor make-register-operand (name &aux (type :register))))
  "The ``Register-Operand'' class represents a register name employed in
   the agency of an instruction operand."
  (name (error "Missing register name.") :type register))

;;; -------------------------------------------------------

(defstruct (Address-Operand
  (:include     Operand)
  (:constructor make-address-operand
    (register-name
     &aux
       (name
         (if (eq register-name :bx)
           register-name
           (error "Invalid address register ~s, must be :bx."
             register-name)))
       (type :address))))
  "The ``Address-Operand'' class represents a memory address, conveyed
   by mediation of the \"bx\" register, as an instruction operand."
  (name (error "Missing register name.") :type register))

;;; -------------------------------------------------------

(defstruct (Tag-Operand
  (:include     Operand)
  (:constructor make-tag-operand (label &aux (type :tag))))
  "The ``Tag-Operand'' class represents a tag name, or label, in the
   agency of an instruction operand."
  (label (error "Missing tag label.") :type string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type operands)))
  "The ``Instruction'' class represents a SASM instruction,
   comprehending in this diorism tag definitions, and composed of a
   categorizing type and a list of zero or more operands."
  (type
    (error "Missing instruction type.")
    :type instruction-type)
  (operands
    NIL
    :type actual-parameter-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-parser ()
  "Initializes the parser and returns no value."
  (declare (special current-token))
  (setf current-token (get-next-token))
  (values))

;;; -------------------------------------------------------

(defun eat-token (token-type)
  "Determines whether the CURRENT-TOKEN conforms to the TOKEN-TYPE, on
   confirmation returning the same, while concomitantly querying and
   storing the lexer's next token; otherwise, an error of an unspecified
   type is signaled."
  (declare (special current-token))
  (declare (keyword token-type))
  (the token
    (if (eq (car current-token) token-type)
      (prog1 current-token
        (setf current-token
          (get-next-token)))
      (error "Expected a token of the type ~s, but encountered ~s."
        token-type current-token))))

;;; -------------------------------------------------------

(defun parse-operand ()
  "Parses a single operand token and returns an ``Operand''
   representation thereof."
  (declare (special current-token))
  (the Operand
    (case (token-type current-token)
      (:integer
        (make-literal-operand
          (token-value
            (eat-token :integer))))
      (:left-bracket
        (eat-token :left-bracket)
        (make-address-operand
          (prog1
            (token-value (eat-token :register))
            (eat-token :right-bracket))))
      (:register
        (make-register-operand
          (token-value
            (eat-token :register))))
      (:identifier
        (make-tag-operand
          (token-value
            (eat-token :identifier))))
      (otherwise
        (error "Invalid MOV source token: ~s." current-token)))))

;;; -------------------------------------------------------

(defun operand-token-p (candidate)
  "Determines whether the CANDIDATE belongs to the operand tokens, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type token candidate))
  (the boolean
    (not (null
      (member (token-type candidate)
        '(:integer :left-bracket :register :identifier)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun parse-operand-list ()
  "Parses a list of zero or more comma-separated operands and returns a
   list of ``Operand'' representations thereof."
  (declare (special current-token))
  (the list
    (loop
      while (operand-token-p current-token)
      collect (parse-operand)
        into operands
      if (token-type-p current-token :comma) do
        (eat-token :comma)
      else do
        (loop-finish)
      finally
        (return operands))))

;;; -------------------------------------------------------

(defun parse-instruction ()
  "Parses an instruction and returns an ``Instruction'' representation
   thereof."
  (declare (special current-token))
  (the Instruction
    (let ((instruction-type (token-type current-token)))
      (declare (type keyword instruction-type))
      (eat-token instruction-type)
      (the Instruction
        (make-instruction instruction-type
          (parse-operand-list))))))

;;; -------------------------------------------------------

(defun effective-instruction-token-p (candidate)
  "Determines whether the CANDIDATE represents an instruction expecting
   operands, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type token candidate))
  (the boolean
    (not (null
      (member (token-type candidate)
        '(:mov :cmp :je :jne :add :sub)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun expect-end-of-line ()
  "Determines whether the CURRENT-TOKEN represents a line terminator,
   that is, either a newline or an end-of-file (EOF) token, in the
   former case consuming the token, in the latter abstaining from any
   further actions, and in both cases returning no value. If
   encountering any other token, an error of an unspecified type is
   signaled."
  (declare (special current-token))
  (case (token-type current-token)
    (:eof
      NIL)
    (:newline
      (eat-token :newline))
    (otherwise
      (error "Expected the end of the line, but encountered ~s."
        current-token)))
  (values))

;;; -------------------------------------------------------

(defun parse-tag ()
  "Parses a tag definition, composed of a tag identifier token and a
   colon, and returns a ``:tag'' instruction representation thereof."
  (declare (special current-token))
  (the Instruction
    (prog1
      (make-instruction :tag
        (list (parse-operand)))
      (eat-token :colon))))

;;; -------------------------------------------------------

(defun parse-line ()
  "Parses an instruction line and returns the comprehended
   ``Instruction'' object."
  (declare (special current-token))
  (the Instruction
    (prog1
      (cond
        ((effective-instruction-token-p current-token)
          (parse-instruction))
        ((token-type-p current-token :identifier)
          (parse-tag))
        (T
          (error "No instruction token: ~s." current-token)))
      (expect-end-of-line))))

;;; -------------------------------------------------------

(defun skip-empty-lines ()
  "Skips a sequence of zero or more accolent newline tokens and returns
   no value."
  (declare (special current-token))
  (loop while (token-type-p current-token :newline) do
    (eat-token :newline))
  (values))

;;; -------------------------------------------------------

(defun parse-program ()
  "Assembles a ``sasm-program'', that is, an ``Instruction'' vector,
   from the lexer's tokens and returns the product."
  (declare (special current-token))
  (skip-empty-lines)
  (the sasm-program
    (coerce
      (loop
        until   (token-type-p current-token :eof)
        collect (parse-line)
        into    instructions
        do      (skip-empty-lines)
        finally (return instructions))
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of instruction signatures.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type signature-table +INSTRUCTION-SIGNATURES+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-SIGNATURES+
  (make-hash-table :test #'eq)
  "Associates the instruction types with the expected formal parameters,
   defined in terms of Common Lisp type specifiers.")

;;; -------------------------------------------------------

(flet ((register-formal-parameters (instruction-type
                                    &rest formal-parameters)
        "Associates the FORMAL-PARAMETERS with the INSTRUCTION-TYPE,
         contingently replacing any extant entry associated with the
         latter, and returns no value.
         ---
         Each element of the FORMAL-PARAMETERS list must constitute a
         type specifier whose compatibility with the Common Lisp
         ``typep'' function's second argument permits a juxtaposition of
         an ``Operand'' object to the same."
        (declare (type instruction-type      instruction-type))
        (declare (type formal-parameter-list formal-parameters))
        (setf (gethash instruction-type +INSTRUCTION-SIGNATURES+)
              formal-parameters)
        (values)))
  
  (register-formal-parameters :mov
    '(or Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand))
  
  (register-formal-parameters :add
    '(or Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand))
  
  (register-formal-parameters :sub
    '(or Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand))
  
  (register-formal-parameters :cmp
    '(or Literal-Operand Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand)
    '(or Register-Operand Address-Operand))
  
  (register-formal-parameters :je
    '(or Literal-Operand Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand)
    'Tag-Operand)
  
  (register-formal-parameters :jne
    '(or Literal-Operand Register-Operand Address-Operand)
    '(or Literal-Operand Register-Operand Address-Operand)
    'Tag-Operand)
  
  (values))

;;; -------------------------------------------------------

(defun get-formal-parameters (instruction-type)
  "Returns the formal parameter list defined for the INSTRUCTION-TYPE,
   or signals an error of an unspecified type if none has been
   registered for the same."
  (declare (type instruction-type instruction-type))
  (the formal-parameter-list
    (or (gethash instruction-type +INSTRUCTION-SIGNATURES+)
        (error "No formal parameters defined for ~s."
          instruction-type))))

;;; -------------------------------------------------------

(defun validate-instruction-signature (instruction)
  "Determines whether the INSTRUCTION's actual parameters match its
   formal parameter list as specified in the +INSTRUCTION-SIGNATURES+
   table, on confirmation returning the actual parameters, otherwise
   signaling an error of an unspecified type."
  (declare (type Instruction instruction))
  (let ((actual-parameters (instruction-operands instruction))
        (formal-parameters (get-formal-parameters
                             (instruction-type instruction))))
    (declare (type actual-parameter-list actual-parameters))
    (declare (type formal-parameter-list formal-parameters))
    (the actual-parameter-list
      (if (and (= (length actual-parameters)
                  (length formal-parameters))
               (every #'typep actual-parameters formal-parameters))
        actual-parameters
        (error "The actual parameters of the instruction ~s do not ~
                match the formal parameters ~s."
          instruction
          formal-parameters)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-register-set ()
  "Creates and returns a set of empty registers."
  (the register-set
    (list
      (cons :ax 0)
      (cons :bx 0)
      (cons :cx 0)
      (cons :dx 0))))

;;; -------------------------------------------------------

(defun reset-registers ()
  "Resets all REGISTERS to their default value of zero (0) and returns
   no value."
  (declare (special registers))
  (dolist (register-entry registers)
    (declare (type (cons register integer) register-entry))
    (setf (cdr register-entry) 0))
  (values))

;;; -------------------------------------------------------

(defun register-entry (name)
  "Returns the register entry amenable to the NAME, or signals an error
   of an unspecified type upon its disrespondency."
  (declare (special       registers))
  (declare (type register name))
  (the (cons register integer)
    (or (assoc name registers :test #'eq)
        (error "Unrecognized register name: ~s." name))))

;;; -------------------------------------------------------

(defun register-value (name)
  "Returns the value of the register with the specified NAME, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (special       registers))
  (declare (type register name))
  (the integer
    (cdr (register-entry name))))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value name)
  "Sets the value of the register with the NAME to the NEW-VALUE and
   returns no value."
  (declare (special       registers))
  (declare (type register name))
  (let ((register-entry
          (register-entry name)))
    (declare (type (cons register integer) register-entry))
    (setf (cdr register-entry) new-value)
    (values)))

;;; -------------------------------------------------------

(defun print-registers ()
  "Prints to the standard output the registers and their currenty values
   and returns no value."
  (declare (special registers))
  (format T "~&Registers:")
  (dolist (register-entry registers)
    (declare (type (cons register integer) register-entry))
    (format T "~&~2t~a => ~a"
      (car register-entry)
      (cdr register-entry)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-memory ()
  "Creates and returns an initially vacant, theoretically infinitely
   large memory instance."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun make-memory-of (&rest initial-content)
  "Creates and returns a theoretically infinitely large memory instance
   whose first cells store the INITIAL-CONTENTS."
  (declare (type (list-of integer) initial-content))
  (let ((memory (make-empty-memory)))
    (declare (type memory memory))
    (loop
      for initial-element of-type integer in   initial-content
      and cell-index      of-type address from 0
      do  (setf (gethash cell-index memory) initial-element))
    (the memory memory)))

;;; -------------------------------------------------------

(defun memory-at (address)
  "Returns the object at the ADDRESS in the MEMORY."
  (declare (special      memory))
  (declare (special      maximum-address))
  (declare (type address address))
  (setf maximum-address (max maximum-address address))
  (the integer
    (gethash address memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-at) (new-value address)
  "Embraces the NEW-VALUE in the MEMORY at the ADDRESS and returns no
   value."
  (declare (special      memory))
  (declare (special      maximum-address))
  (declare (type integer new-value))
  (declare (type address address))
  (setf (gethash address memory 0) new-value)
  (setf maximum-address (max maximum-address address))
  (values))

;;; -------------------------------------------------------

(defun print-memory ()
  "Prints to the standard output the current MEMORY state and returns no
   value."
  (declare (special memory))
  (declare (special maximum-address))
  (format T "~&Memory:")
  (loop for address of-type address from 0 to maximum-address do
    (format T "~&~2tmemory[~d] = ~d" address
      (memory-at address)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operand operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric read-from-operand (operand)
  (:documentation
    "Returns the register or memory object referred to by the
     OPERAND."))

;;; -------------------------------------------------------

(defgeneric write-to-operand (operand new-value)
  (:documentation
    "Sets the register or memory value referred to by the OPERAND to the
     NEW-VALUE and returns no value."))

;;; -------------------------------------------------------

(defmethod read-from-operand ((operand Literal-Operand))
  "Returns the literal integer value stored in the OPERAND."
  (declare (type Literal-Operand operand))
  (the integer
    (literal-operand-value operand)))

;;; -------------------------------------------------------

(defmethod read-from-operand ((operand Register-Operand))
  "Returns the value of the register maintained in the OPERAND."
  (declare (special               registers))
  (declare (type Register-Operand operand))
  (the integer
    (register-value
      (register-operand-name operand))))

;;; -------------------------------------------------------

(defmethod read-from-operand ((operand Address-Operand))
  "Returns the value of the memory whose address equals the value stored
   in the OPERAND's register."
  (declare (special              registers))
  (declare (type Address-Operand operand))
  (the integer
    (memory-at
      (register-value
        (address-operand-name operand)))))

;;; -------------------------------------------------------

(defmethod write-to-operand ((operand   Literal-Operand)
                             (new-value integer))
  "Signals an error of an unspecified type, as a literal object cannot
   be altered by reading operations."
  (declare (type Literal-Operand operand))
  (declare (type integer         new-value))
  (error "Cannot set the value of the literal object ~s to ~d."
    operand new-value))

;;; -------------------------------------------------------

(defmethod write-to-operand ((operand   Register-Operand)
                             (new-value integer))
  "Stores the NEW-VALUE in the register represented by the OPERAND and
   returns no value."
  (declare (type Register-Operand operand))
  (declare (type integer          new-value))
  (setf (register-value (register-operand-name operand)) new-value)
  (values))

;;; -------------------------------------------------------

(defmethod write-to-operand ((operand   Address-Operand)
                             (new-value integer))
  "Stores the NEW-VALUE in the memory location referenced by the
   OPERAND's \"bx\" register and returns no value."
  (declare (type Address-Operand operand))
  (declare (type integer         new-value))
  (setf (memory-at
          (register-value
            (address-operand-name operand)))
    new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tag management.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tag-table ()
  "Creates and returns an empty tag table."
  (the tag-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun register-tags ()
  "Associates each tag in the program with its zero-based position in
   the TAGS table and returns no value."
  (declare (special instructions))
  (declare (special tags))
  (loop
    for instruction of-type Instruction across instructions
    for ip          of-type fixnum      from   0
    when (eq (instruction-type instruction) :tag) do
      (let ((tag-label
              (tag-operand-label
                (first (instruction-operands instruction)))))
        (declare (type string tag-label))
        (setf (gethash tag-label tags) ip)))
  (values))

;;; -------------------------------------------------------

(defun tag-position (tag)
  "Returns the index of the TAG definition in the instruction vector, or
   signals an error of an unspecified type if none such could be
   detected."
  (declare (special     tags))
  (declare (type string tag))
  (the fixnum
    (or (gethash tag tags)
        (error "Unrecognized tag name: ~s." tag))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction processors.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dispatch-instruction (instruction-type instruction)
  (:documentation
    "Processes the INSTRUCTION, dispatched on its INSTRUCTION-TYPE, in
     the context of the executing interpreter and returns no value."))

;;; -------------------------------------------------------

(defun process-instruction (instruction)
  "Processes the INSTRUCTION in the context of the executing
   interpreter by invoking the eligible ``dispatch-instruction'' method
   and returns no value."
  (declare (type Instruction instruction))
  (dispatch-instruction (instruction-type instruction) instruction)
  (values))

;;; -------------------------------------------------------

(defmacro define-instruction-dispatch
    (instruction-type (instruction-variable) &body body)
  "Defines an implementation of the ``dispatch-instruction'' method,
   automatically generating for the first parameter a name whose type
   assumes the form ``(eql INSTRUCTION-TYPE)'', for the second parameter
   stating the agnomination as the INSTRUCTION-VARIABLE with the class
   name ``Instruction'', the body being composed of the BODY statements,
   concluding the same with no return value."
  (let ((instruction-type-variable (gensym)))
    (declare (type symbol instruction-type-variable))
    `(defmethod dispatch-instruction
         ((,instruction-type-variable (eql ,instruction-type))
          (,instruction-variable      Instruction))
       (declare (type keyword     ,instruction-type-variable))
       (declare (ignore           ,instruction-type-variable))
       (declare (type Instruction ,instruction-variable))
       (declare (ignorable        ,instruction-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-instruction-dispatch :mov (instruction)
  (declare (special registers))
  (declare (special ip))
  (destructuring-bind (target source)
      (validate-instruction-signature instruction)
    (declare (type Operand target))
    (declare (type Operand source))
    (write-to-operand target
      (read-from-operand source)))
  (incf ip))

;;; -------------------------------------------------------

(define-instruction-dispatch :add (instruction)
  (declare (special ip))
  (destructuring-bind (target source)
      (validate-instruction-signature instruction)
    (declare (type Operand target))
    (declare (type Operand source))
    (write-to-operand target
      (+ (read-from-operand target)
         (read-from-operand source))))
  (incf ip))

;;; -------------------------------------------------------

(define-instruction-dispatch :sub (instruction)
  (declare (special ip))
  (destructuring-bind (target source)
      (validate-instruction-signature instruction)
    (declare (type Operand target))
    (declare (type Operand source))
    (write-to-operand target
      (- (read-from-operand target)
         (read-from-operand source))))
  (incf ip))

;;; -------------------------------------------------------

(define-instruction-dispatch :cmp (instruction)
  (declare (special ip))
  (destructuring-bind (left-operand right-operand target)
      (validate-instruction-signature instruction)
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (declare (type Operand target))
    (write-to-operand target
      (if (= (read-from-operand left-operand)
             (read-from-operand right-operand))
        1
        0)))
  (incf ip))

;;; -------------------------------------------------------

(define-instruction-dispatch :je (instruction)
  (declare (special ip))
  (declare (special tags))
  (destructuring-bind (left-operand right-operand tag)
      (validate-instruction-signature instruction)
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (declare (type Operand tag))
    (declare (ignorable    tag))
    (if (= (read-from-operand left-operand)
           (read-from-operand right-operand))
      (setf ip (tag-position (tag-operand-label tag)))
      (incf ip))))

;;; -------------------------------------------------------

(define-instruction-dispatch :jne (instruction)
  (declare (special ip))
  (declare (special tags))
  (destructuring-bind (left-operand right-operand tag)
      (validate-instruction-signature instruction)
    (declare (type Operand left-operand))
    (declare (type Operand right-operand))
    (declare (type Operand tag))
    (declare (ignorable    tag))
    (if (/= (read-from-operand left-operand)
            (read-from-operand right-operand))
      (setf ip (tag-position (tag-operand-label tag)))
      (incf ip))))

;;; -------------------------------------------------------

(define-instruction-dispatch :tag (instruction)
  (declare (special ip))
  (incf ip))

;;; -------------------------------------------------------

(defun interpret-program ()
  "Interprets the SASM INSTRUCTIONS and returns no value."
  (declare (special instructions))
  (declare (special ip))
  (loop while (< ip (length instructions)) do
    (process-instruction
      (aref instructions ip)))
  (print-registers)
  (print-memory)
  (values))

;;; -------------------------------------------------------

(defun initialize-interpreter ()
  "Initializes the interpreter and returns no value."
  (declare (special ip))
  (declare (special tags))
  (declare (special registers))
  (setf ip 0)
  (clrhash tags)
  (reset-registers)
  (values))

;;; -------------------------------------------------------

(defun interpret-SASM (code
                       &key (initial-memory NIL))
  "Interprets the piece of SASM source CODE, permitting an optional
   INITIAL-MEMORY which, upon its provision, will be employed as the
   program's main storage and contingently destructively modified, this
   function ultimate returning no value."
  (declare (type string           code))
  (declare (type (or null memory) initial-memory))
  
  (let ((source    code)
        (position  0)
        (character NIL))
    (declare (special                  source))
    (declare (type string              source))
    (declare (special                  position))
    (declare (type fixnum              position))
    (declare (special                  character))
    (declare (type (or null character) character))
    
    (initialize-lexer)
    
    (let ((current-token (cons :eof NIL)))
      (declare (special    current-token))
      (declare (type token current-token))
      
      (initialize-parser)
      
      (let ((instructions    (parse-program))
            (ip              0)
            (tags            (make-tag-table))
            (registers       (make-register-set))
            (memory          (or initial-memory
                                 (make-empty-memory)))
            (maximum-address 0))
        (declare (special           instructions))
        (declare (type sasm-program instructions))
        (declare (special           ip))
        (declare (type fixnum       ip))
        (declare (special           registers))
        (declare (type register-set registers))
        (declare (special           tags))
        (declare (type tag-table    tags))
        (declare (special           memory))
        (declare (type memory       memory))
        (declare (special           maximum-address))
        (declare (type address      maximum-address))
        
        (initialize-interpreter)
        (register-tags)
        
        (interpret-program))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Count from ten (10) down to zero (0).
(interpret-SASM
  "mov ax, 10
   mov bx, ax
   
   countdown:
     sub bx, 1
     jne bx, 0, countdown")

;;; -------------------------------------------------------

;; Count from ten (10) down to zero (0) and store these descending
;; integers in the memory cells at the addresses zero to ten, that is:
;; 
;;   memory[0]  = 10
;;   memory[1]  =  9
;;   memory[2]  =  8
;;   memory[3]  =  7
;;   memory[4]  =  6
;;   memory[5]  =  5
;;   memory[6]  =  4
;;   memory[7]  =  3
;;   memory[8]  =  2
;;   memory[9]  =  1
;;   memory[10] =  0
(interpret-SASM
  "mov ax, 10
   mov bx, 0
   
   countdown:
     mov [bx], ax
     add bx,   1
     sub ax,   1
     jne ax, -1, countdown")

;;; -------------------------------------------------------

;; Store in the memory cell at the address 2 the value 100.
(interpret-SASM
  "mov bx, 2
   mov [bx], 100")

;;; -------------------------------------------------------

;; Truth-machine which simulates an input of zero (0).
(interpret-SASM
  "je [bx], 0, _halt
   
   _goback:
     add bx,   1
     mov [bx], 1
     je  [bx], 1, _goback
   
   _halt:"
  :initial-memory (make-memory-of 0))

;;; -------------------------------------------------------

;; Truth-machine which simulates an input of one (1).
;; 
;; Starting with the memory address 0, all cells will be set to the
;; value one (1), thus emulating the output of "1" for the equivalent
;; user input.
(interpret-SASM
  "je [bx], 0, _halt
   
   _goback:
     add bx, 1
     mov [bx], 1
     je  [bx], 1, _goback
   
   _halt:"
  :initial-memory (make-memory-of 1))
