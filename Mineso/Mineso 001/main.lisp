;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Mineso", invented by the Esolang user "DeadlyFugu" and
;; presented on December 24th, 2011, operating on a tape of byte-valued
;; cells, while the dioristic proprium of its principle resides in its
;; arithmetic, logical, and output competences' realization via the
;; dedication by special cells, rather than accommodated instructions.
;; 
;; 
;; Concept
;; =======
;; The Mineso programming language constitutes a specimen, operating on
;; a finite tape of byte-valued cells by an octuple instruction set,
;; whose veridical operational daintith resides in the involvement of
;; an enneadic membership among these storage units, eliciting in their
;; modifications such realizations of chrestomathics as arithmetic,
;; logical, and output responses.
;; 
;; == THE MEMORY COMPREHENDS 256 CELLS OF BYTE-VALUED CAPACITY ==
;; Mineso's standard memory model subscribes to the deployment of 256
;; cells, amenable to subscripts starting from zero (0), where each such
;; compartment lends itself to a scalar byte's stewardship.
;; 
;; A mobile cell pointer designates at any instant the currently
;; responsive unit, the active cell.
;; 
;; == THE EIGHT COMMANDS SUGGEST A PARVIPOTENT ADMINISTRATION ==
;; A compernage to the twissel of jump-based, conditional control flow
;; facilities, the remaining six instructions seem to ostend very little
;; competence, as their purview does not extend beyond the allowance to
;; both homologate a perquisition and modification applied to the active
;; or any other cell, utilizing either a direct setting or, as a
;; peculiarity, one that respects a cell's "rule", if such is in
;; existence.
;; 
;; The latter fact, however, endows the language with its hid
;; potentials.
;; 
;; == SPECIAL CELLS PROVIDE ARITHMETICS, LOGIC, AND OUTPUT ==
;; A kenspeckle attribute woning in the language, the octuple operative
;; warklumes ostend a scantiness in their utility, their paravaunt
;; agency the memory cells' and pointer's castaldy and intercourse, in
;; conjunction with the twain of jump facilities. The apercu upon this
;; parvipotent endowment, however, is encumbered with the wite of
;; acangening one's perception: The arithmetic, logical, and output
;; facilities' presence is elicited by the utility of a particular set
;; of cells.
;; 
;; These nine specimens enumerate the following:
;; 
;;   ------------------------------------------------------------------
;;   Cell index | Role
;;   -----------+------------------------------------------------------
;;   0          | Instruction pointer (IP) relocation
;;   ..................................................................
;;   1          | Incrementation
;;   ..................................................................
;;   2          | Decrementation
;;   ..................................................................
;;   3          | Multiplication
;;   ..................................................................
;;   4          | Division
;;   ..................................................................
;;   6          | Output of character by ASCII code
;;   ..................................................................
;;   9          | Left bit shift
;;   ..................................................................
;;   10         | Right bit shift
;;   ..................................................................
;;   11         | Output of numeric value
;;   ------------------------------------------------------------------
;; 
;; It is of a peisant impact to note that a several operations respect
;; these rules, while others entirely dismiss their effect.
;; 
;; 
;; Architecture
;; ============
;; The Mineso program memory, in its original implementation, naits a
;; one-dimensional array of 256 cells, each such an 8-bit element's
;; woning, operating in conjunction with a cell pointer which at any
;; instant designates the currently active unit.
;; 
;; Maugre its liberties for deviations in the implementation, the coming
;; treatise's focus applies to the Mineso creator's reference
;; implementation.
;; 
;; == THE MEMORY: A TAPE OF 256 CELLS ==
;; The program memory's perimeter appropriates a tally of 256 cells,
;; indexed with subscripts, or "positions", commencing from zero (0).
;; 
;; == EACH CELL: A BYTE STORAGE ==
;; Every cell's capacity allocates a scalar 8-bit octet vallidom, its
;; occupancy the integral range of [0, 255].
;; 
;; == SPECIAL CELLS DEVIATE IN THEIR RESPONSE TO MODIFICATIONS ==
;; A nonuplet subset of these cells enjoys a dioristic respondency's
;; dation upon their values' modulation, their concrete membership and
;; treatise shall please be extricated from the "Instructions" section
;; below.
;; 
;; == THE CELL POINTER REFERENCES THE CURRENT CELL ==
;; A dedicated marker, the "cell pointer", selects at any instant the
;; active cell, that unit of paravaunt significance for the reception of
;; inquisitions and modifications. An instruction's incorporation into
;; the Mineso acts such as to permit an arbitrary cell's occupation of
;; the cell pointer's attention.
;; 
;; == THE INITIAL CELL POINTER LOCATION IS IMPLEMENTATION-DEPENDENT ==
;; The initial cell pointer location eludes an imperative imposition,
;; permitting the implementation to present the developer or user with
;; its specification as a parasceve ere a Mineso program's execution.
;; 
;; == VARIATIONS ARE HOMOLOGATED ==
;; The protolog declaims with explicit diction the contingency for
;; alternative, potentially more potent memory models, such may enlist
;; greater quantities of cells, or administer a wider gamut to these
;; components; however, no further reification's statement embues the
;; standard with a manuduction along this subject.
;; 
;; 
;; Data Types
;; ==========
;; Mineso naits a twifaced exposition of a type system, with byte values
;; as a paravaunt objects of a program's deliberation, ensconed in the
;; memory cells and involved in all arithmetic, logical, and relational
;; operations; contemporaneous to ASCII characters attending to paravail
;; aspects engaged as currency of the output conduit.
;; 
;; 
;; Syntax
;; ======
;; A Mineso program permits any content, its faculty for recognition
;; impounded to operations whose identifiers amplect a singular symbol
;; and an optional unary integer argument.
;; 
;; == PROGRAM ==
;; A Mineso program in its diorism engages a sequence of zero or more
;; instruction, each such a compound of an imperative single-character
;; identifier and an optional unary inteeger argument encoding.
;; 
;; All characters eluding the capacity to accompass influence are
;; encountered with leniency and disregard, and may be inserted at any
;; location for commentary or formatting purposes.
;; 
;; == INSTRUCTIONS ==
;; A command's conformation issues from a single-character identifier,
;; desumed from an octuple symbol set, and an optional integer argument
;; whose diorism proceeds from a unary "1" sequence withal the tally of
;; repetitions encodes the state.
;; 
;; Characters not subsumed into this nonuplet category are tolerated at
;; dismissed irregardless of the spatial and quantitative intrusion.
;; 
;; == ARGUMENTS ==
;; All arguments, optional in their participation, amplect the aefauld
;; literal unsigned integer species, expressed in the unary numeral
;; system by a tally of occurrences which is tantamount to the
;; represented value. The lacuna's construe is equinumerant to a zero
;; (0) number.
;; 
;; As with any content in the Mineso language, non-command characters
;; may be inspersed in a liberal fashion without infringing nor
;; interfering the "1" sequence.
;; 
;; == WHITESPACES ==
;; Whitespaces ostend the acquisition of an ejusdem generis
;; administration to any symbol not reserved for the instruction
;; identification or the argument statement wikes, by being encountered
;; with the same mete in tolerance and impotence.
;; 
;; == COMMENTS ==
;; A tatic potential for comments wones in the language, as characters
;; not entrusted with the dever of the command identification or
;; argumentation provision are entirely neglected.
;; 
;; == GRAMMAR ==
;; The language donet shall be the following Extended Backus-Naur Form's
;; (ENBF) substance, the same disregards ineffectual characters in its
;; pursuit for a clean expression of the language:
;; 
;;   program           := { command } ;
;;   command           := commandIdentifier , argument ;
;;   commandIdentifier := "-"
;;                     |  "<"
;;                     |  ">"
;;                     |  "="
;;                     |  "?"
;;                     |  "["
;;                     |  "]"
;;                     |  "@"
;;                     ;
;;   argument          := { "1" } ;
;; 
;; 
;; Instructions
;; ============
;; Mineso's instruction set constitutes an octuple composition, the
;; circumference occupied by its competences enumerating preponderantly
;; direct cell value alternations and transfers betwixt these entities,
;; as well as a twissel of conditional jump-based control flow
;; mechanisms.
;; 
;; The commorancy of its more sophisticated capabilities ensues from the
;; kenspeckle agency apportioned to a particular subset of its program
;; memory cells, whose respondency to modifications incites enhanced
;; epiphenomena.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall entalent the interest party with a cursory
;; mete of gnarity anenst the language's instructive facilities.
;; 
;; Please heed the catena of asterisks ("*") whose dever its constitutes
;; to designate placeholders in the commands, expected to be substituted
;; by valid Mineso code in the actual program.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------ 
;;   - argument | Relocates the cell pointer to the {argument}-th
;;     ******** | cell.
;;   ..................................................................
;;   < argument | Stores the {argument} in the active cell, while
;;     ******** | respecting the cell's rules.
;;   ..................................................................
;;   = argument | Stores the {argument} in the active cell, while
;;     ******** | ignoring the cell's rules.
;;   ..................................................................
;;   > argument | Copies the value maintained by the active cell to the
;;     ******** | cell located at the {argument}-th position, while
;;              | respecting both cells' rules.
;;   ..................................................................
;;   @ argument | Copies the value of the cell at the {argument}-th
;;     ******** | position to the active cell, ignoring both cells'
;;              | rules.
;;   ..................................................................
;;   ? argument | Applies a logical operation to the active cell,
;;     ******** | setting the active cell's value to zero (0) or one
;;              | (1) depending on the relationship to the cell at the
;;              | position 5, ignoring the active cell's rules. The
;;              | {argument} state furnishes the relational operator:
;;              | 
;;              |   ---------------------------------------------------
;;              |   Argument | Causatum
;;              |   ---------+-----------------------------------------
;;              |   0        | Sets the active cell to 1 if its value
;;              |            | is equal to that of the cell at the
;;              |            | position 5; otherwise sets the active
;;              |            | cell's value to 0.
;;              |   ...................................................
;;              |   1        | Sets the active cell to 1 if its value
;;              |            | is less than that of the cell at the
;;              |            | position 5; otherwise sets the active
;;              |            | cell's value to 0.
;;              |   ...................................................
;;              |   2        | Sets the active cell to 1 if its value
;;              |            | is greater than that of the cell at the
;;              |            | position 5; otherwise sets the active
;;              |            | cell's value to 0.
;;              |   ...................................................
;;              |   3        | Inverts the active cell's data,
;;              |            | transforming a non-zero value to 0, and
;;              |            | a state of zero (0) to 1.
;;              |            | The cell at the position 5 does not
;;              |            | participate in this operation.
;;              |   ---------------------------------------------------
;;   ..................................................................
;;   [ argument | If the active cell's value does not equal zero (0),
;;     ******** | moves the instruction pointer (IP) a tally of
;;              | ({argument} + 1) commands forward. Otherwise
;;              | accompasses no effect.
;;   ..................................................................
;;   ] argument | If the active cell's value does not equal zero (0),
;;     ******** | moves the instruction pointer (IP) a tally of
;;              | ({argument} + 1) commands back. Otherwise accompasses
;;              | no effect.
;;   ------------------------------------------------------------------
;; 
;; == SPECIAL CELLS ==
;; An ennead of special cells, dedicated to a particular faculty each,
;; whose causatum ensues from a modification of the unit's state --- if
;; not suppressed in an express manner ---, engages in the language in
;; order to complement basic arithmetics, logical operations, and output
;; conduits.
;; 
;; Thee nine dedicated memory constituents and their rules shall now be
;; a cursory treatise's substance.
;; 
;; Please heed that the {argument} provides a placeholder for the value
;; transmitted to the respective cell by any of the octuple operations
;; admitted for such occassion, which please see aboon.
;; 
;;   ------------------------------------------------------------------
;;   Cell | Rule (response to setting of value {argument})
;;   -----+------------------------------------------------------------
;;   0    | Relocates the instruction pointer (IP) to the zero-based
;;        | command index {argument}.
;;        |------------------------------------------------------------
;;        | The value stored in this cell will always equal the current
;;        | instruction pointer (IP) position. As a consectary, an
;;        | imposition upon a Mineso implementation's upper bourne for
;;        | its cells concomitantly restricts the circumference of the
;;        | program's instruction count.
;;   ..................................................................
;;   1    | Increments the value of this cell by {argument}.
;;   ..................................................................
;;   2    | Decrements the value of this cell by {argument}.
;;   ..................................................................
;;   3    | Multiplies the value of this cell by {argument} and stores
;;        | the product in this cell.
;;   ..................................................................
;;   4    | Divides the value of this cell by the divisor {argument},
;;        | round the quotient accordingly, and stores the prepared
;;        | quotient in this cell.
;;   ..................................................................
;;   6    | Prints the character whose ASCII code corresponds to the
;;        | {argument} to the standard output and stores the code in
;;        | this cell.
;;   ..................................................................
;;   9    | Performs a left bit shift by a number of {argument}
;;        | positions on this cell's value and stores the result back
;;        | in it. The least significant bit (LSB) positions exposed
;;        | in the new cell state by this operation are filled with
;;        | zero-bits (0).
;;   ..................................................................
;;   10   | Performs a right bit shift by a number of {argument}
;;        | positions on this cell's value and stores the result back
;;        | in it. The most significant bit (MSB) positions exposed
;;        | in the new cell state by this operation are filled with
;;        | zero-bits (0).
;;   ..................................................................
;;   11   | Prints the {argument} in its verbatim numeric form to the
;;        | standard output and stores the {argument} in this cell.
;;   ------------------------------------------------------------------
;; 
;; == PSEUDOCODE FORMULATIONS ==
;; An augmentation in lucidity shall be accompassed by a pseudocode
;; formulation's more stringent adminculum.
;; 
;; As instruments for its unambiguous construe shall serve the following
;; identifier definitions to whom an engagement in the treatise is
;; allotted:
;; 
;;   ------------------------------------------------------------------
;;   Identifier | Role
;;   -----------+------------------------------------------------------
;;   memory     | The program memory, conceited as a vector of unsigned
;;              | bytes, enumerated with zero-based indices, such obeys
;;              | for an index "position" the forbisen
;;              |   memory[position]
;;   ..................................................................
;;   pointer    | The cell pointer, that is, the index or position of
;;              | the active cell.
;;   ..................................................................
;;   argument   | The value communicated by a command argument or
;;              | memory cell byte.
;;   ..................................................................
;;   ip         | The instruction pointer, which designates the
;;              | currently processed command among the sequence of the
;;              | program statements.
;;   ..................................................................
;;   applyRule  | A procedure which encapsulates the setting of a
;;              | memory cell's value under a concomitant involvement
;;              | of its special rule, as far as applicable. The
;;              | signature, for a memory reference "memoryReference"
;;              | to modify and the value to assign "newValue",
;;              | comprehends:
;;              |   applyRule(memoryReference, newValue)
;;   ------------------------------------------------------------------
;; 
;; Proceeding from these diorisms' presentation, the promised pseudocode
;; elucidations assume these:
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   - argument | pointer <- {argument}
;;     ******** | 
;;   ..................................................................
;;   < argument | applyRule(memory[pointer], {argument})
;;     ******** | 
;;   ..................................................................
;;   = argument | memory[pointer] <- {argument}
;;     ******** | 
;;   ..................................................................
;;   > argument | applyRule(memory[{argument}, memory[pointer])
;;     ******** | 
;;   ..................................................................
;;   @ argument | memory[pointer] <- memory[{argument}]
;;     ******** | 
;;   ..................................................................
;;   ? argument | if {argument} = 0 then
;;     ******** |   if memory[pointer] = memory[5] then
;;              |     memory[pointer] <- 1
;;              |   else
;;              |     memory[pointer] <- 0
;;              |   end if
;;              | 
;;              | else if {argument} = 1 then
;;              |   if memory[pointer] < memory[5] then
;;              |     memory[pointer] <- 1
;;              |   else
;;              |     memory[pointer] <- 0
;;              |   end if
;;              | 
;;              | else if {argument} = 2 then
;;              |   if memory[pointer] > memory[5] then
;;              |     memory[pointer] <- 1
;;              |   else
;;              |     memory[pointer] <- 0
;;              |   end if
;;              | 
;;              | else if {argument} = 3 then
;;              |   if memory[pointer] = 0 then
;;              |     memory[pointer] <- 1
;;              |   else
;;              |     memory[pointer] <- 0
;;              |   end if
;;              | end if
;;   ..................................................................
;;   [          | if memory[pointer] != 0 then
;;              |   ip <- ip + ({argument} + 1)
;;              | end if
;;   ..................................................................
;;   ]          | if memory[pointer] != 0 then
;;              |   ip <- ip - ({argument} + 1)
;;              | end if
;;   ------------------------------------------------------------------
;; 
;; == INTERCOURSE OF CELL VALUES AND LEALTY TO RULES ==
;; A parlecue of the operations to whom the castaldy over the involved
;; cells and a missive concerning their obedience to the cell rules is
;; imparted shall be illustrated below.
;; 
;; Please, iterum, heed that placeholder segments are underlined via a
;; catena of asterisks ("*"), intended for a supersession by actual
;; code in the final program.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Source(s)          | Destination        | Obeys cell
;;              |                    |                    | rules?
;;   -----------+--------------------+--------------------+------------
;;   < argument | {argument}         | active cell        | yes
;;     ******** |                    |                    | 
;;   ..................................................................
;;   = argument | {argument}         | active cell        | no
;;     ******** |                    |                    | 
;;   ..................................................................
;;   > argument | active cell        | {argument}-th cell | yes
;;     ******** |                    |                    | 
;;   ..................................................................
;;   @ argument | {argument}-th cell | active cell        | no
;;     ******** |                    |                    | 
;;   ..................................................................
;;   ? argument | active cell,       | active cell        | no
;;     ******** | cell at index 5    |                    | 
;;              | {argument}         |                    | 
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the relative mickleness invested into its explications, the
;; language protolog is inflicted with several inroads of ambiguities,
;; a subset therefrom shall be desumed for further perquisition.
;; 
;; == MAY THE CELL AT POSITION ZERO (0) BE MODIFIED? ==
;; An indirect manifestation of antilogy resides in the treatise on the
;; cell at the position zero (0), such is in its haecceity employed in
;; cases of modifications to the instruction pointer's (IP) redirection.
;; While an express apostille issues the constancy of comport in the
;; castaldy of the current instruction, operations exist with that
;; unbridled potence as to circumvent a cell's rule, whence is begotten
;; the inquisition into this particular specimen's engagement.
;; 
;; It has been chosen to resort to a stringent and total interdiction of
;; the zero-position cell's modification. While an attempt, of course,
;; does not ally with a fatal error's incitement, a causatum's absence
;; tacitly delivers the transpiration to ultimate otioseness.
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp, procuring, in concord with the Mineso's allowance in its
;; original specification, a fungible and extendable memory model.
;; 
;; == MEMORY AND CELLS ARE INTERFACES ==
;; Ensuing from the Mineso protolog's homologation of alternative memory
;; concepts, this implementation applies itself to the provision of
;; interfaces for both the commitment of the memory and its cell
;; components, the extant infrastructure of which may be augmented by
;; one's personal contributions.
;; 
;; The clavigers to this malleability appertain to a triad of entities:
;; 
;;   ------------------------------------------------------------------
;;   Interface/Class | Role
;;   ----------------+-------------------------------------------------
;;   Cell            | An interface that defines the faculties
;;                   | requisite to a memory cell's indagation and
;;                   | manipulation.
;;   ..................................................................
;;   Memory          | An interface whose specification provides
;;                   | operations for accessing the active cell or any
;;                   | such by its position, as well as accommodating
;;                   | cell pointer handling facilities.
;;   ..................................................................
;;   Cell-Factory    | A concrete class whose dever amplects the
;;                   | provision of cells by a callback function's
;;                   | adminiculum. Itself employed in a latreutical
;;                   | aspect, concrete "Memory" classes may receive
;;                   | their "Cell" instances from this provenance.
;;   ------------------------------------------------------------------
;; 
;; A set of basic implementation for these adjustable elements
;; constitutes the project's circumference:
;; 
;;   ------------------------------------------------------------------
;;   Class             | Role
;;   ------------------+-----------------------------------------------
;;   Byte-Cell         | An implementation of the "Cell" interface
;;                     | which operates on an unsigned byte datum,
;;                     | occupying the integral range [0, 255], and
;;                     | wrapping around along its bournes.
;;   ..................................................................
;;   Fixed-Size-Memory | A "Memory" interface implementation based upon
;;                     | a static array, relying on a "Cell-Factory"
;;                     | for its cells' reception.
;;   ..................................................................
;;   Infinite-Memory   | A "Memory" interface implementation based upon
;;                     | a theoretically infinite enumeration of cells
;;                     | by mediation of a hash table, the keys of
;;                     | which maintain the cell positions, associated
;;                     | with the actual "Cell" objects; again refers
;;                     | to a "Cell-Factory" for its cells supply.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-08
;; 
;; Sources:
;;   [esolang2020Mineso]
;;   The Esolang contributors, "Mineso", December 12th, 2020
;;   URL: "https://esolangs.org/wiki/Mineso"
;;   
;;   [stackoverflow2017q43539051]
;;   The Stack Overflow contributors,
;;     "*= operation in Common Lisp", April 21st, 2017
;;   URL: "https://stackoverflow.com/questions/43539051/
;;         operation-in-common-lisp"
;;   Notes:
;;     - Demonstrates the use of "define-modify-macro" in order to
;;       emulate operands in conjunction with assignments as in the
;;       programming language family C/C++; for instance:
;;         A *= 2
;;     - Mentions the "zap" macro, which enables the acceptance of a
;;       function which programmatically updates a place.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype size ()
  "The ``size'' type defines a non-negative integer number endowed with
   such concinnity as to represent a tally or a magnitude."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list composed of a SIZE cardinality of
   elements which comply to the ELEMENT-TYPE, where the SIZE, upon its
   omission or specification as ``*'' may assume any magnitude, and the
   ELEMENT-TYPE defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              ;; Either no SIZE imposed, ...
              (and (symbolp size) (eq size '*))
              ;; ... or the same must match the CANDIDATE list length.
              (= (length (the list candidate)) size))
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized variants on
   Mineso operation categories."
  '(member
    :select-cell                    ;; "-"
    :set-active-cell                ;; "<"
    :set-active-cell-directly       ;; "="
    :copy-active-cell-to            ;; ">"
    :copy-directly-to-active-cell   ;; "@"
    :compare-cells                  ;; "?"
    :jump-forward                   ;; "["
    :jump-back))                    ;; "]"

;;; -------------------------------------------------------

(deftype mineso-program ()
  "The ``mineso-program'' type defines an executable Mineso program as a
   vector of zero or more ``Command'' instances."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and each
   associated value to the VALUE-TYPE, both defaulting to the generic
   ``*'' sentinel."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type &optional (argument 0))))
  "The ``Command'' class encapsulates the notion of a Mineso operation,
   composed of its distinguishing command category and its numeric
   modifiable argument."
  (type     (error "Missing command type.")
            :type      command-type
            :read-only T)
  (argument (error "Missing command argument.")
            :type      size
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a Mineso command
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "-<>=?[]@" :test #'char=)))))

;;; -------------------------------------------------------

(defun parse-command-type (token)
  "Returns the ``command-type'' affiliated with the TOKEN, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command-type
    (case token
      (#\-      :select-cell)
      (#\<      :set-active-cell)
      (#\=      :set-active-cell-directly)
      (#\>      :copy-active-cell-to)
      (#\@      :copy-directly-to-active-cell)
      (#\?      :compare-cells)
      (#\[      :jump-forward)
      (#\]      :jump-back)
      (otherwise (error "No command type identifier: ~s." token)))))

;;; -------------------------------------------------------

(defun collect-argument (source start)
  "Proceeding from the START position into the SOURCE, tallies the zero
   or more occurrencies of the decimal digit one (1), contingently
   omitting ineffective tokens, and returns two values:
     (1) The number of encountered \"1\" tokens.
     (2) The position in the SOURCE immediately succeeding the argument
         segment, which is demarcated by either the next command token's
         instalment or the end of the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values size fixnum)
    (loop
      for position of-type fixnum    from start below (length source)
      for token    of-type character =    (char source position)
      
      if (char= token #\1)
        count 1
        into  argument
      else if (command-character-p token) do
        (loop-finish)
      end
      
      finally
        (return
          (values argument position)))))

;;; -------------------------------------------------------

(defun locate-next-command (source start)
  "Proceeding from the START position into the SOURCE, searches for the
   next command token and returns two values:
     (1) The detected ``comand-type'', or ``NIL'' upon its lacuna.
     (2) The position in the SOURCE immediately succeeding the detected
         command identifier, or the length of the SOURCE itself upon a
         failure to locate a command."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (or null command-type) fixnum)
    (loop
      for position of-type fixnum    from start below (length source)
      for token    of-type character =    (char source position)
      
      if (command-character-p token) do
        (return
          (values
            (parse-command-type token)
            (1+ position)))
      else if (char= token #\1) do
        (error "Unary argument token \"1\" detected outside of a ~
                command at position ~d."
          position)
      end
      
      finally
        (return
          (values NIL (length source))))))

;;; -------------------------------------------------------

(defun collect-command (source start)
  "Proceeding from the START position into the SOURCE, seeks the next
   Mineso command and returns two values:
     (1) The detected and parsed ``Command'' object, or ``NIL'' upon its
         lacuna.
     (2) The position in the SOURCE immediately succeeding the parsed
         command, or the SOURCE's length upon a failure to locate such."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((command-type NIL)
        (position     0))
    (declare (type (or null command-type) command-type))
    (declare (type fixnum                 position))
    (multiple-value-setq (command-type position)
      (locate-next-command source start))
    (the (values (or null Command) fixnum)
      (if command-type
        (multiple-value-bind (argument new-position)
            (collect-argument source position)
          (declare (type size   argument))
          (declare (type fixnum new-position))
          (values
            (make-command command-type argument)
            new-position))
        (values NIL position)))))

;;; -------------------------------------------------------

(defun collect-commands (source)
  "Extracts and returns from the piece of Mineso SOURCE code a vector of
   its embedded commands."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((process-command (new-command new-position)
            "Returns a list comprehending the NEW-COMMAND, or the empty
             list upon its lacuna, while concomitantly updating the
             POSITION cursor to the NEW-POSITION a potential subsequent
             iteration."
            (declare (type (or null Command) new-command))
            (declare (type fixnum            new-position))
            (the (or null (list-of Command 1))
              (prog1
                (when new-command
                  (list new-command))
                (setf position new-position)))))
      (the mineso-program
        (coerce
          (loop
            while (< position (length source))
            append
              (multiple-value-call #'process-command
                (collect-command source position)))
          '(simple-array Command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory cell.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cell ()
  ()
  (:documentation
    "The ``Cell'' interface establishes a substrate for classes intent
     on the representation of the program memory cells."))

;;; -------------------------------------------------------

(defgeneric cell-value (cell)
  (:documentation
    "Returns the CELL's value."))

;;; -------------------------------------------------------

(defgeneric (setf cell-value) (cell new-value)
  (:documentation
    "Stores the NEW-VALUE in the CELL and returns no value."))

;;; -------------------------------------------------------

(defclass Byte-Cell (Cell)
  ((value
    :initform      0
    :type          (unsigned-byte 8)
    :documentation "The unsigned byte value stored in this cell."))
  (:documentation
    "The ``Byte-Cell'' class implements the ``Cell'' interface in order
     to accommodate an unsigned byte storage."))

;;; -------------------------------------------------------

(defun make-byte-cell ()
  "Creates and returns a new ``Byte-Cell''."
  (the Byte-Cell
    (make-instance 'Byte-Cell)))

;;; -------------------------------------------------------

(defmethod cell-value ((cell Byte-Cell))
  "Returns the unsigned byte value stored in the CELL."
  (declare (type Byte-Cell cell))
  (the (unsigned-byte 8)
    (slot-value cell 'value)))

;;; -------------------------------------------------------

(defmethod (setf cell-value) ((new-value integer) (cell Byte-Cell))
  "Stores the NEW-VALUE in the CELL, contingently wrapping it around in
   order to ascertain its conformance with the CELL's unsigned byte
   range of [0, 255], and returns no value."
  (declare (type integer   new-value))
  (declare (type Byte-Cell cell))
  (setf (slot-value cell 'value)
        (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of cell factory.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cell-Factory ()
  ((producer
    :initarg       :producer
    :initform      (error "Missing cell producer.")
    :type          (function () Cell)
    :documentation "A callback function which upon each invocation
                    returns a new ``Cell'' object."))
  (:documentation
    "The ``Cell-Factory'' class' onus refers to its production and
     delivery of ``Cell'' subclass instances upon an issued request.
     ---
     The quesited desideratum's fashion of production ensues from a
     callback, realized as a niladic function which answers to any
     request with a fresh ``Cell'' object, thus maintaining its lealty
     to the signature
       function () => Cell"))

;;; -------------------------------------------------------

(defun make-cell-factory (producer)
  "Creates and returns a new ``Cell-Factory'' whose cell producing
   callback is realized in the PRODUCER function."
  (declare (type (function () Cell) producer))
  (the Cell-Factory
    (make-instance 'Cell-Factory :producer producer)))

;;; -------------------------------------------------------

(defun build-cell (cell-factory)
  "Queries the next cell from the CELL-FACTORY and returns the same."
  (declare (type Cell-Factory cell-factory))
  (the Cell
    (funcall
      (slot-value cell-factory 'producer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ()
  (:documentation
    "The ``Memory'' class establishes the base for all classes assigned
     the dever of the program memory's representation."))

;;; -------------------------------------------------------

(defgeneric cell-pointer (memory)
  (:documentation
    "Returns the position of the MEMORY's cell pointer."))

;;; -------------------------------------------------------

(defgeneric active-cell (memory)
  (:documentation
    "Returns the value stored in the MEMORY's active cell."))

;;; -------------------------------------------------------

(defgeneric (setf active-cell) (new-value memory)
  (:documentation
    "Stores the NEW-VALUE in the MEMORY's active cell, contingently
     applying accommodations during the transmission in order to
     ascertain the MEMORY state's validity, and returns no value."))

;;; -------------------------------------------------------

(defgeneric cell-at (memory position)
  (:documentation
    "Returns the value stored in the MEMORY cell at the specified
     POSITION."))

;;; -------------------------------------------------------

(defgeneric (setf cell-at) (new-value memory position)
  (:documentation
    "Stores the NEW-VALUE in the MEMORY cell at the specified POSITION,
     contingently applying accommodations during the transmission in
     order to ascertain the MEMORY state's validity, and returns no
     value."))

;;; -------------------------------------------------------

(defclass Fixed-Size-Memory (Memory)
  ((cell-factory
    :initarg       :cell-factory
    :initform      (error "Missing cell factory.")
    :type          Cell-Factory
    :documentation "The callback function responsible for the delivery
                    of cells for the CELLS vector.")
   (cells
    :initarg       :cells
    :initform      (error "Missing cell vector.")
    :type          (simple-array Cell (*))
    :documentation "A fixed-sized vector of cells, populated by the
                    CELL-FACTORY's ``Cell'' instances.")
   (pointer
    :initarg       :pointer
    :initform      0
    :accessor      cell-pointer
    :type          size
    :documentation "The index (position) of the currently active cell
                    in the CELLS array."))
  (:documentation
    "The ``Fixed-Size-Memory'' class implements the ``Memory'' interface
     in its pursuit of a standard memory model's replication, the same
     amplects a fixed tally of cells, the provision of which constitutes
     a ``Cell-Factory'' component's wike."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((memory Fixed-Size-Memory) &key)
  "Populates the MEMORY's cells vector with ``Cell'' instances supplied
   by the internally managed factory and returns no value."
  (declare (type Fixed-Size-Memory memory))
  (flet ((initialize-cell ()
          "Returns the next cell from the MEMORY's cell factory."
          (the Cell
            (build-cell
              (slot-value memory 'cell-factory)))))
    (map-into
      (slot-value memory 'cells)
      #'initialize-cell))
  (values))

;;; -------------------------------------------------------

(defun make-fixed-size-memory (size cell-factory)
  "Creates and returns a new ``Fixed-Size-Memory'', enumerating the SIZE
   account of whose, the concrete instance of which are supplied by the
   CELL-FACTORY."
  (declare (type fixnum       size))
  (declare (type Cell-Factory cell-factory))
  (the Fixed-Size-Memory
    (make-instance 'Fixed-Size-Memory
      :cells
        (make-array size
          :element-type    '(or null Cell)
          :initial-element NIL
          :adjustable      NIL
          :fill-pointer    NIL)
      :cell-factory cell-factory)))

;;; -------------------------------------------------------

(defmethod active-cell ((memory Fixed-Size-Memory))
  (declare (type Fixed-Size-Memory memory))
  (the integer
    (cell-value
      (aref
        (slot-value memory 'cells)
        (slot-value memory 'pointer)))))

;;; -------------------------------------------------------

(defmethod (setf active-cell) ((new-value integer)
                               (memory    Fixed-Size-Memory))
  (declare (type integer           new-value))
  (declare (type Fixed-Size-Memory memory))
  (the integer
    (setf
      (cell-value
        (aref
          (slot-value memory 'cells)
          (slot-value memory 'pointer)))
      new-value)))

;;; -------------------------------------------------------

(defmethod cell-at ((memory Fixed-Size-Memory) (position integer))
  (declare (type Fixed-Size-Memory memory))
  (declare (type size              position))
  (the integer
    (cell-value
      (aref (slot-value memory 'cells) position))))

;;; -------------------------------------------------------

(defmethod (setf cell-at) ((new-value integer)
                           (memory    Fixed-Size-Memory)
                           (position  integer))
  (declare (type integer           new-value))
  (declare (type Fixed-Size-Memory memory))
  (declare (type size              position))
  (the integer
    (setf
      (cell-value
        (aref (slot-value memory 'cells)
        position))
      new-value)))

;;; -------------------------------------------------------

(defun build-default-memory ()
  "Creates and returns the default Mineso ``Memory'' model, a fixed-size
   vector of 256 unsigned bytes."
  (the Fixed-Size-Memory
    (make-fixed-size-memory 256
      (make-cell-factory
        #'(lambda ()
            (the Cell
              (make-byte-cell)))))))

;;; -------------------------------------------------------

(defclass Infinite-Memory (Memory)
  ((cell-factory
    :initarg       :cell-factory
    :initform      (error "Missing cell factory.")
    :type          Cell-Factory
    :documentation "The callback function responsible for the delivery
                    of cells for the CELLS hash table.")
   (cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of size Cell)
    :documentation "A sparse vector of cells, represented by a hash
                    table whose keys accommodate the cell indices and
                    associate with ``Cell'' objects as salvatories to
                    the values.")
   
   (pointer
    :initform      0
    :accessor      cell-pointer
    :type          size
    :documentation "The active cell's index, represented by the key into
                    the CELLS hash table of the selected cell entry."))
  (:documentation
    "The ``Infinite-Memory'' class implements the ``Memory'' interface
     pursuing the telos of representing a unilaterally infinite
     dispansion of cells, its enumeration proceeding from the index
     zero (0), and representing this extreme mickleness by adminiculum
     of a sparse vector, its foundation a hash table."))

;;; -------------------------------------------------------

(defun make-infinite-memory (cell-factory)
  "Creates and returns a new ``Infinite-Memory'' instance whose ``Cell''
   objects are appropriated by inquisitions into the CELL-FACTORY."
  (declare (type Cell-Factory cell-factory))
  (the Infinite-Memory
    (make-instance 'Infinite-Memory :cell-factory cell-factory)))

;;; -------------------------------------------------------

(defun ensure-cell (memory position)
  "Ascertains the existence of a cell in the MEMORY at the desiderated
   POSITION by either returning the extant instance or, upon its lacuna,
   generating such ere its delivery."
  (declare (type Infinite-Memory memory))
  (declare (type size            position))
  (with-slots (cells) memory
    (declare (type (hash-table-of size Cell) cells))
    (the Cell
      (or
        (gethash position cells)
        (with-slots (cell-factory) memory
          (declare (type Cell-Factory cell-factory))
          (setf (gethash position cells)
            (build-cell cell-factory))
          (gethash position cells))))))

;;; -------------------------------------------------------

(defmethod active-cell ((memory Infinite-Memory))
  (declare (type Infinite-Memory memory))
  (the integer
    (cell-value
      (ensure-cell memory
        (slot-value memory 'pointer)))))

;;; -------------------------------------------------------

(defmethod (setf active-cell) ((new-value integer)
                               (memory    Infinite-Memory))
  (declare (type integer         new-value))
  (declare (type Infinite-Memory memory))
  (the integer
    (setf
      (cell-value
        (ensure-cell memory
          (slot-value memory 'pointer)))
      new-value)))

;;; -------------------------------------------------------

(defmethod cell-at ((memory Infinite-Memory) (position integer))
  (declare (type Infinite-Memory memory))
  (declare (type size            position))
  (the integer
    (cell-value
      (ensure-cell memory position))))

;;; -------------------------------------------------------

(defmethod (setf cell-at) ((new-value integer)
                           (memory    Infinite-Memory)
                           (position  integer))
  (declare (type integer         new-value))
  (declare (type Infinite-Memory memory))
  (declare (type size            position))
  (the integer
    (setf
      (cell-value
        (ensure-cell memory position))
      new-value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-modify-macro multiplyf (multiplicand) *
  "Multiplies a place by the MULTIPLICAND as the dextral operand, stores
   the product in the place, and returns its new state.")

;;; -------------------------------------------------------

(define-modify-macro dividef (divisor) round
  "Divides a place by the DIVISOR, rounds it according to the common
   mathematical diorism, stores the quotient in the place, and returns
   its new state.")

;;; -------------------------------------------------------

(define-modify-macro bit-shift-left (distance) ash
  "Shifts a place's bits by the DISTANCE tally of positions to the left,
   stores the result in the place, and returns its new state.")

;;; -------------------------------------------------------

(defun ash-right (bits count)
  "Performs a right bit shift of the integer-encoded BITS by a COUNT
   number of positions and returns the result without modification of
   either of its arguments."
  (declare (type integer bits))
  (declare (type integer count))
  (the integer
    (ash bits (- count))))

;;; -------------------------------------------------------

(define-modify-macro bit-shift-right (distance) ash-right
  "Shifts a place's bits by the DISTANCE tally of positions to the
   right, stores the result in the place, and returns its new state.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          mineso-program
    :documentation "The Mineso program to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) location in the
                    PROGRAM.")
   (memory
    :initarg       :memory
    :initform      (build-default-memory)
    :type          Memory
    :documentation "The program memory.")
   (jump-destination
    :initform      NIL
    :type          (or null integer)
    :documentation "Maintains the index of the subsequent command in the
                    PROGRAM to visit.
                    ---
                    If resolving to a non-``NIL'' value, the instruction
                    pointer IP is relocated to this position in
                    immediate prevenience to the next command processing
                    cyclem succeeded by a resetting to ``NIL'' in order
                    to assume the default state; otherwise the
                    instruction pointer IP simply increments to the
                    naturally following next PROGRAM location."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of accompassing
     effect to a Mineso program represented by a vector of commands."))

;;; -------------------------------------------------------

(defmacro with-interpreter
    ((interpreter
       &optional (program-variable          '$program)
                 (ip-variable               '$ip)
                 (memory-variable           '$memory)
                 (jump-destination-variable '$jump-destination))
     &body body)
  "Furnishes an convenient amenity for accessing the INTERPRETR's slots
   by evaluating the same, binding via local symbol macros its
   ``program'' slot the the PROGRAM-VARIABLE, the ``ip'' to the
   IP-VARIABLE, and the ``memory'' slot to the MEMORY-VARIABLE,
   executing the BODY forms, permitted access to these definitions,
   and returning the desinent evaluated form.
   ---
   Upon any of the three slot agnominations' lacuna a default value's
   apportionment assumes the respective onus:
     ----------------------------------------------------------------
     Slot             | Argument name             | Default value
     -----------------+---------------------------+------------------
     program          | program-variable          | $program
     ................................................................
     ip               | ip-variable               | $ip
     ................................................................
     memory           | memory-variable           | $memory
     ................................................................
     jump-destination | jump-destination-variable | $jump-destination
     ----------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (with-slots
           ((,program-variable          program)
            (,ip-variable               ip)
            (,memory-variable           memory)
            (,jump-destination-variable jump-destination))
           ,evaluated-interpreter
         (declare (type mineso-program    ,program-variable))
         (declare (ignorable              ,program-variable))
         (declare (type fixnum            ,ip-variable))
         (declare (ignorable              ,ip-variable))
         (declare (type Memory            ,memory-variable))
         (declare (ignorable              ,memory-variable))
         (declare (type (or null integer) ,jump-destination-variable))
         (declare (ignorable              ,jump-destination-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun make-interpreter (program &key (memory (build-default-memory)))
  "Creates and returns a new ``Interpreter'' the wike of which is
   specified by the PROGRAM to execute, optionally employing a bespoke
   MEMORY model."
  (declare (type mineso-program program))
  (declare (type Memory         memory))
  (the Interpreter
    (make-instance 'Interpreter :program program :memory memory)))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's internally managed program has
   concluded, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (with-interpreter (interpreter)
        (>= $ip (length $program)))))))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (if $jump-destination
      (shiftf $ip $jump-destination NIL)
      (incf   $ip)))
  (values))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command located at the INTERPRETER's instruction pointer
   (IP) position."
  (declare (type Interpreter interpreter))
  (the Command
    (with-interpreter (interpreter)
      (aref $program $ip))))

;;; -------------------------------------------------------

(defun update-instruction-pointer-cell (interpreter)
  "Ensures that the cell at the position zero (0) in the INTERPRETER's
   memory comprehends the current instruction pointer (IP) position and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf (cell-at $memory 0) $ip))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-command (interpreter command-type command)
  (:documentation
    "Evaluates the COMMAND, dispatched on its COMMAND-TYPE, in the
     INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defmacro define-command-dispatch
    (command-type (interpreter-variable command-variable)
     &body body)
  "Furnishes a convenient avenue for the definition of an implementation
   of the generic function ``dispatch-command'', nevening the first
   parameter with the INTERPRETER-VARIABLE, the third by the
   COMMAND-VARIABLE, while the second, dispatching input, the
   COMMAND-TYPE, is agnominated automatically, executes the BODY forms,
   and returns no value."
  (let ((command-type-variable (gensym)))
    (declare (type symbol command-type-variable))
    `(defmethod dispatch-command
         ((,interpreter-variable  Interpreter)
          (,command-type-variable (eql ,command-type))
          (,command-variable      Command))
       (declare (type Interpreter  ,interpreter-variable))
       (declare (ignorable         ,interpreter-variable))
       (declare (type command-type ,command-type-variable))
       (declare (ignore            ,command-type-variable))
       (declare (type Command      ,command-variable))
       (declare (ignorable         ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(defun process-command (interpreter command)
  "Evaluates the COMMAND, dispatched on its type, in the INTERPRETER's
   context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (dispatch-command interpreter
    (command-type command)
    command)
  (values))

;;; -------------------------------------------------------

(defun set-cell-directly (interpreter cell-position new-value)
  "Stores the NEW-VALUE in the INTERPRETER memory's cell designated by
   the CELL-POSITION, circumventing any contingent cell rule, and
   concomitantly desisting from a manipulation of the zero-indexed unit,
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type size        cell-position))
  (declare (type integer     new-value))
  (unless (zerop cell-position)
    (with-interpreter (interpreter)
      (setf (cell-at $memory cell-position) new-value)))
  (values))

;;; -------------------------------------------------------

(defun set-cell-regularly (interpreter cell-position new-value)
  "Stores the NEW-VALUE in the INTERPRETER memory's cell designated by
   the CELL-POSITION under observance of a contingently applicable cell
   rule, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type size        cell-position))
  (declare (type integer     new-value))
  (with-interpreter (interpreter)
    (case cell-position
      (0  (setf      $ip new-value))
      (1  (incf      (cell-at $memory cell-position) new-value))
      (2  (decf      (cell-at $memory cell-position) new-value))
      (3  (multiplyf (cell-at $memory cell-position) new-value))
      (4  (dividef   (cell-at $memory cell-position) new-value))
      (6  (write-char (code-char new-value))
          (finish-output)
          (setf (cell-at $memory cell-position) new-value))
      (9  (bit-shift-left  (cell-at $memory cell-position) new-value))
      (10 (bit-shift-right (cell-at $memory cell-position) new-value))
      (11 (format T "~d " new-value)
          (finish-output)
          (setf (cell-at $memory cell-position) new-value))
      (otherwise
        (setf (cell-at $memory cell-position) new-value))))
  (values))

;;; -------------------------------------------------------

(define-command-dispatch :select-cell (interpreter command)
  "Empights the INTERPRETER memory's cell pointer to the position
   communicated by the COMMAND argument and returns no value.
   ---
   This operation implements the Mineso \"-\" command."
  (with-interpreter (interpreter)
    (setf (cell-pointer $memory)
      (command-argument command))))

;;; -------------------------------------------------------

(define-command-dispatch :set-active-cell (interpreter command)
  "Stores the COMMAND argument in the INTERPRETER memory's active cell,
   upon necessity instigating the cell rule for a special active cell,
   and returns no value.
   ---
   This operation implements the Mineso \"<\" command."
  (with-interpreter (interpreter)
    (set-cell-regularly interpreter
      (cell-pointer     $memory)
      (command-argument command))))

;;; -------------------------------------------------------

(define-command-dispatch :set-active-cell-directly (interpreter command)
  "Stores the COMMAND argument in the INTERPRETER memory's active cell
   without applying the cell rule for a special active cell, and returns
   no value.
   ---
   This operation implements the Mineso \"=\" command."
  (with-interpreter (interpreter)
    (set-cell-directly interpreter
      (cell-pointer     $memory)
      (command-argument command))))

;;; -------------------------------------------------------

(define-command-dispatch :copy-active-cell-to (interpreter command)
  "Copies the value of the INTERPRETER memory's active cell to the cell
   located at the position designated by the COMMAND argument, upon
   necessity instigating the cell rule for a special target cell, and
   returns no value.
   ---
   This operation implements the Mineso \">\" command."
  (with-interpreter (interpreter)
    (set-cell-regularly interpreter
      (command-argument command)
      (active-cell      $memory))))

;;; -------------------------------------------------------

(define-command-dispatch :copy-directly-to-active-cell (interpreter
                                                        command)
  "Copies the value of the INTERPRETER memory cell located at the
   position designated by the COMMAND argument to the active cell
   without applying its contingent cell rule, and returns no value.
   ---
   This operation implements the Mineso \"@\" command."
  (with-interpreter (interpreter)
    (set-cell-directly interpreter
      (cell-pointer $memory)
      (cell-at $memory
        (command-argument command)))))

;;; -------------------------------------------------------

(define-command-dispatch :compare-cells (interpreter command)
  "Sets the INTERPRETER memory's active cell value to the numeric
   Boolean value zero (0) or one (1), depending upon the unary or binary
   logical operation communicated by the COMMAND argument, and returns
   no value.
   ---
   This operation implements the Mineso \"?\" command."
  (with-interpreter (interpreter)
    (flet ((get-numeric-truth-value (boolean-truth-value)
            "Returns a numeric truth value for the generalized
             BOOLEAN-TRUTH-VALUE which responds with zero (0) for a
             ``NIL'' (\"false\") input and one (1) for a non-``NIL''
             (\"true\") argument."
            (declare (type T boolean-truth-value))
            (the bit (if boolean-truth-value 1 0)))
           
           (set-active-cell-value (new-value)
            "Stores the NEW-VALUE in the INTERPRETER memory's active
             cell and returns no value."
            (declare (type integer new-value))
            (set-cell-directly interpreter
              (cell-pointer $memory)
              new-value)
            (values)))
      
      (case (command-argument command)
        ;; Equal.
        (0
          (set-active-cell-value
            (get-numeric-truth-value
              (= (active-cell $memory)
                 (cell-at     $memory 5)))))
        ;; Less than.
        (1
          (set-active-cell-value
            (get-numeric-truth-value
              (< (active-cell $memory)
                 (cell-at     $memory 5)))))
        ;; Greater than.
        (2
          (set-active-cell-value
            (get-numeric-truth-value
              (> (active-cell $memory)
                 (cell-at     $memory 5)))))
        ;; Invert.
        (3
          (set-active-cell-value
            (if (zerop (active-cell $memory)) 1 0)))
        (otherwise NIL)))))

;;; -------------------------------------------------------

(define-command-dispatch :jump-forward (interpreter command)
  "If the INTERPRETER memory's active cell does not equal zero (0),
   translates the INTERPRETER's instruction pointer (IP) a number of
   positions equal to the COMMAND argument augmented by one (1) forward
   in the PROGRAM, otherwise abstains from any causatum, in any case
   returning no value.
   ---
   This operation implements the Mineso \"[\" command."
  (with-interpreter (interpreter)
    (unless (zerop (active-cell $memory))
      (setf $jump-destination
        (+ $ip
           (1+ (command-argument command)))))))

;;; -------------------------------------------------------

(define-command-dispatch :jump-back (interpreter command)
  "If the INTERPRETER memory's active cell does not equal zero (0),
   translates the INTERPRETER's instruction pointer (IP) a number of
   positions equal to the COMMAND argument augmented by one (1) back in
   the PROGRAM, otherwise abstains from any causatum, in any case
   returning no value.
   ---
   This operation implements the Mineso \"]\" command."
  (with-interpreter (interpreter)
    (unless (zerop (active-cell $memory))
      (setf $jump-destination
        (- $ip
           (1+ (command-argument command)))))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Executes the Mineso program governed by the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (update-instruction-pointer-cell interpreter)
    (process-command interpreter
      (get-current-command interpreter))
    (advance-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Mineso (code &key (memory (build-default-memory)))
  "Interprets the piece of Mineso source CODE, optionally deploying a
   bespoke program MEMORY, and returns no value."
  (declare (type string code))
  (declare (type Memory memory))
  (interpret-program
    (make-interpreter
      (collect-commands code)
      :memory memory))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Mineso
  "-1
   <111111111111111111111111111111111111111111111111111111111111111111111111
   >111111
   <11111111111111111111111111111
   >111111
   <1111111
   >111111
   >111111
   <111
   >111111
   =11111111111111111111111111111111
   >111111
   <1111111111111111111111111111111111111111111111111111111
   >111111
   <111111111111111111111111
   >111111
   <111
   >111111
   >11
   -11
   @111111
   <111111
   >111111
   <11111111
   >111111
   =111111111111111111111111111111111
   >111111")

;;; -------------------------------------------------------

;; Exponentiation: Prints the power of two cell values.
;; 
;; Memory layout:
;;   subtractionCell    = cell[2]   { special cell }
;;   multiplicationCell = cell[3]   { special cell }
;;   multiplierCell     = cell[5]   { regular cell }
;;   outputCell         = cell[11]  { special cell }
;; 
;; Basic concept:
;;   (1) The multiplicationCell is unconditionally set to one (1).
;;   (2) The regular multiplierCell receives the base A.
;;   (3) The subtractionCell is unconditionally set to the exponent B.
;;   (4) While the subtractionCell is positive:
;;         The multiplicationCell is multiplied by the multiplierCell
;;         The subtractionCell is decremented by one (1).
;;   (5) At the end the program, multiplicationCell value is send to the
;;       outputCell, which prints its numeric content.
;; 
;; Notes:
;;   - In order to modify the base A, replace the argument in the
;;     fourth line
;;       =11             # Receive exponentiation base A
;;     by a new unary value.
;;   - In order to modify the exponent B, replace the argument in the
;;     ninth line
;;       =111            # Store exponent B in the subtraction cell
;;     by a new unary value.
(interpret-Mineso
  "
  -111            # Change to multiplication cell at position three
  =1              # Set multiplication cell to one

  -11111          # Change to fifth cell
  =11             # Receive exponentiation base A

  [11             # Proceed as usual if A does not equal zero
  =1              # Otherwise set fifth cell to one for subsequent
  [11111111111    # skipping to end of program

  -11             # Change to subtraction cell for setting exponent B
  =111            # Store exponent B in the subtraction cell

  [11             # Proceed as usual if B does not equal zero
  =1              # Otherwise set subtraction cell to one for subsequent
  [111111         # skipping to end of program

  -11111          # Change to fifth cell for multiplying with multiplication cell
  >111            # Multiply multiplication cell by A via fifth cell

  -11             # Change to subtraction cell for subsequent deduction and back jump test
  <1              # Decrement B value in its agency as a jump counter
  ]111            # Jump back if necessary for repeated multiplication
  -111            # Switch to multiplication cell which contains the power

  -111            # Change to multiplication cell at position three for pending printing
  >11111111111    # Output multiplication cell with its exponentation result
  ")

;;; -------------------------------------------------------

;; Print down from inclusive five (5) to inclusive zero (0).
(interpret-Mineso "-11=111111<1>11111111111]1 ")

;;; -------------------------------------------------------

;; Generate and print the powers of two from inclusive 2^0 (= 1) to
;; inclusive 2^7 (= 128).
(interpret-Mineso
  "-11111
   =11111111
   
   -11
   @11111
   
   -11111
   =1
   
   -111111111
   @11111
   
   -111111111
   >11111111111
   <1
   
   -11
   <1
   ]1111")
