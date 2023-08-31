;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Linefuck6", invented by the Esolang user "RandomIdiot" and
;; presented on April 26th, 2023, intended as a derivative of Urban
;; Mueller's "brainfuck", diorism of which wones in several aspects,
;; including the curtailment of the entheus' octuple to a more
;; compendious sextuple instruction set, the deployment of linear
;; symbols for their denotation, a starkly constrained tape, and a
;; peculiar character encoding absent from the ASCII standard.
;; 
;; 
;; Concept
;; =======
;; The Linefuck6 programming language is founded upon a sextuple of
;; command symbols, molded into stroke forms, that operate on a vector
;; of ten cells, capaciated to hold integers in the range [0, 62], thus
;; being rendered congruent to a dioristic encoding of 63 representable
;; characters.
;; 
;; == COMMANDS ASSUME LINE FORMS ==
;; The ambitus reserved for the language's donat ostends the most
;; kenspeckle variation, as the command identifiers assume a linear
;; character.
;; 
;; In a diction entalented with enhanced reification, the following
;; characters participate:
;; 
;;   -----------------------------------------------
;;   Character | Name           | Unicode code point
;;   ----------+----------------+-------------------
;;   -         | hyphen-minus   | U+002D
;;   ...............................................
;;   –         | en dash        | U+2013
;;   ...............................................
;;   _         | low line       | U+005F
;;   ...............................................
;;   ~         | tilde          | U+007E
;;   ...............................................
;;   '         | apostrophe     | U+0027
;;   ...............................................
;;   "         | quotation mark | U+0022
;;   -----------------------------------------------
;; 
;; == THE MEMORY: TEN CELLS OF INTEGERS IN THE RANGE [0, 62] ==
;; The program memory assumes the guise of a decimal account of cells,
;; partaking of a vector whose indices start with one (1) and conclude
;; with the subscript ten (10).
;; 
;; Each cell, at its inchoation empight on the default value of zero
;; (0), might store at any instance a value in the integer range
;; [0, 62]. Its amenability to gradual incrementation, but desistence
;; from homologating deductions, is ameliorated by the wrapping nature:
;; If transgressing the upper extremum of 62, the state returns to the
;; minimum of zero (0).
;; 
;; A cell pointer, at the program's commencement referencing the first
;; cell, maintains the currently active instance, the same provides the
;; aefauld entity receptive for indagations and alterations. This
;; pointer itself responds to behests that translate it in a stepwise
;; fashion along the sinistrodextral airt, wrapping around when passing
;; the desinent cell, indexed with ten (10), to return to the first
;; entity at the position one (1).
;; 
;; == LINEFUCK6 OFFERS A KENSPECKLE CHARACTER REPERTOIRE ==
;; Deviating from the ASCII standard, Linefuck6 mandates a character
;; repertoire of itself, in conjunction with a accommodated encoding
;; policy.
;; 
;; This set of symbols embraces the minuscular and majuscular Latin
;; letters, the ten decimal digits, and the space character, the
;; coefficacy of which endows the same with a cardinality of 63 members,
;; encoded in the integer numbers from inclusive zero (0) to inclusive
;; 62 --- intentionally congruent with the cell capacity.
;; 
;; The quadruple division explicated aboon applies to the 63 Linefuck6
;; repertoire members by the following principle:
;; 
;;   ------------------------------------------------
;;   Linefuck6 code range | Symbols | Category
;;   ---------------------+---------+----------------
;;   0–25                 | a–z     | Latin minuscles
;;   ................................................
;;   26–51                | A–Z     | Latin majuscles
;;   ................................................
;;   52–61                | 0–9     | Decimal digits
;;   ................................................
;;   62                   | (space) | Whitespaces
;;   ------------------------------------------------
;; 
;; A listing of explicit eludication and nimiety shall be adduced which
;; equiparates the admissible characters, their Linefuck6 encoding, and
;; the traditional ASCII codes in a palpable manner:
;; 
;;   ---------------------------------------
;;   Character | Linefuck6 code | ASCII code
;;   ----------+----------------+-----------
;;       a     |        0       |     97
;;   .......................................
;;       b     |        1       |     98
;;   .......................................
;;       c     |        2       |     99
;;   .......................................
;;       d     |        3       |    100
;;   .......................................
;;       e     |        4       |    101
;;   .......................................
;;       f     |        5       |    102
;;   .......................................
;;       g     |        6       |    103
;;   .......................................
;;       h     |        7       |    104
;;   .......................................
;;       i     |        8       |    105
;;   .......................................
;;       j     |        9       |    106
;;   .......................................
;;       k     |       10       |    107
;;   .......................................
;;       l     |       11       |    108
;;   .......................................
;;       m     |       12       |    109
;;   .......................................
;;       n     |       13       |    110
;;   .......................................
;;       o     |       14       |    111
;;   .......................................
;;       p     |       15       |    112
;;   .......................................
;;       q     |       16       |    113
;;   .......................................
;;       r     |       17       |    114
;;   .......................................
;;       s     |       18       |    115
;;   .......................................
;;       t     |       19       |    116
;;   .......................................
;;       u     |       20       |    117
;;   .......................................
;;       v     |       21       |    118
;;   .......................................
;;       w     |       22       |    119
;;   .......................................
;;       x     |       23       |    120
;;   .......................................
;;       y     |       24       |    121
;;   .......................................
;;       z     |       25       |    122
;;   .......................................
;;       A     |       26       |     65
;;   .......................................
;;       B     |       27       |     66
;;   .......................................
;;       C     |       28       |     67
;;   .......................................
;;       D     |       29       |     68
;;   .......................................
;;       E     |       30       |     69
;;   .......................................
;;       F     |       31       |     70
;;   .......................................
;;       G     |       32       |     71
;;   .......................................
;;       H     |       33       |     72
;;   .......................................
;;       I     |       34       |     73
;;   .......................................
;;       J     |       35       |     74
;;   .......................................
;;       K     |       36       |     75
;;   .......................................
;;       L     |       37       |     76
;;   .......................................
;;       M     |       38       |     77
;;   .......................................
;;       N     |       39       |     78
;;   .......................................
;;       O     |       40       |     79
;;   .......................................
;;       P     |       41       |     80
;;   .......................................
;;       Q     |       42       |     81
;;   .......................................
;;       R     |       43       |     82
;;   .......................................
;;       S     |       44       |     83
;;   .......................................
;;       T     |       45       |     84
;;   .......................................
;;       U     |       46       |     85
;;   .......................................
;;       V     |       47       |     86
;;   .......................................
;;       W     |       48       |     87
;;   .......................................
;;       X     |       49       |     88
;;   .......................................
;;       Y     |       50       |     89
;;   .......................................
;;       Z     |       51       |     90
;;   .......................................
;;       0     |       52       |     48
;;   .......................................
;;       1     |       53       |     49
;;   .......................................
;;       2     |       54       |     50
;;   .......................................
;;       3     |       55       |     51
;;   .......................................
;;       4     |       56       |     52
;;   .......................................
;;       5     |       57       |     53
;;   .......................................
;;       6     |       58       |     54
;;   .......................................
;;       7     |       59       |     55
;;   .......................................
;;       8     |       60       |     56
;;   .......................................
;;       9     |       61       |     57
;;   .......................................
;;    (space)  |       62       |     32
;;   ---------------------------------------
;; 
;; 
;; Architecture
;; ============
;; Inspired, yet ultimately eloigned from its brainfuck provenance,
;; Linefuck6 employs a tape composed of restricted integer-valued cells,
;; the traditional 30,000, or the modern infinite dation, substituted
;; for a mere decimal account in their tally, operated upon by a cell
;; pointer which, starting with the first cell, indexed by one (1),
;; wraps around in its strictly sinstrodextral traversal from the tenth
;; to the first position if transgressing the desinence's bourne.
;; 
;; == THE MEMORY: A TAPE COMPOSED OF TEN CELLS ==
;; Linefuck6's tape naits a tape whose compency is fixed at ten cells,
;; indexed by consecutive integers starting from one (1) and concluding
;; with ten (10).
;; 
;; == THE CELL POINTER: A SINISTRODEXTRAL TRAVELER THAT WRAPS AROUND ==
;; In the lealty to its brainfuck consanguinity, a cell pointer, or
;; simply norned pointer, applies itself to the current cell's
;; selection.
;; 
;; At the program's inchoation empight on the cell at the index one (1),
;; it may be translated with graduate motion in the dextral airt, but
;; lacks an athwart equivalency.
;; 
;; Upon an attempt to conduct the pointer beyond the desinent, tenth
;; position, the same automatically wraps around to iterum commence at
;; the first, one-indexed, component.
;; 
;; == THE CELLS: STORAGES FOR INTEGER SCALARS IN THE RANGE [0, 62] ==
;; Each cell stores a scalar integer value impounded to obey the
;; particular range [0, 62], which conflates with the encodings for
;; character in Linefuck6.
;; 
;; The cell value may be incremented in graduation, but no deduction
;; acquires admissibility; in lieu of this, the imperative to increase
;; the value beyond the upper march of 62 results in a wrapping around,
;; such that the cell value is rendered to the minimum of zero (0).
;; 
;; == LINEFUCK6 AND BRAINFUCK JUXTAPOSED ==
;; The following tabular illustration shall serve in the juxtaposition
;; of Linefuck 6's and brainfuck's memory model in the most significant
;; aspects:
;; 
;;   ------------------------------------------------------------------
;;                   | Linefuck6     | brainfuck
;;   ----------------+---------------+---------------------------------
;;   Number of cells | 10            | 30,000   (traditional, minimum)
;;                   |               | infinite (modern)
;;   ..................................................................
;;   Cell pointer    | left-to-right | left-to-right or right-to-left
;;   ..................................................................
;;   Cell pointer    | wraps around  | If not on an infinite tape, an
;;   overflow        |               | error should be signaled.
;;   ..................................................................
;;   Cell range      | [0, 62]       | Implementation-dependent, often
;;                   |               | chosen as an unsigned byte in
;;                   |               | the closed interval [0, 255].
;;   ..................................................................
;;   Cell overflow   | wraps around  | If not infinite in its capacity,
;;   behavior        |               | the cell wraps around.
;;   ------------------------------------------------------------------
;; 
;; 
;; Data Types
;; ==========
;; Linefuck6 employs a bivial expression of a type system, imprimis in
;; the integer range [0, 62]; as a parhedral compartment employing a
;; special character repertoire whose encodings, veering from the ASCII
;; regulation, concords with selfsame numeric species, amplecting merely
;; minuscular and majuscular Latin letters, the ten decimal digits, and
;; the space character.
;; 
;; 
;; Syntax
;; ======
;; The sextuple instruction set of Linefuck6 is represented by aefauld
;; symbols, chosen with the purpose of incorporating a design in lines,
;; whereas any non-instruction constituent is homologated, but
;; ultimately ignored and thus ineffectuous.
;; 
;; 
;; Instructions
;; ============
;; Linefuck6's instruction set tallies six members, the same endeavor
;; an approximate equipollence with the octuple brainfuck cleronomy,
;; parvipotent, natheless, in their input and output facilities, yet
;; perpetuated in capacitation to manage the memory, perform rudimentary
;; arithmetics, receive input and issue output, as well as conduct a
;; jump-based control flow.
;; 
;; == OVERVIEW ==
;; Illustrated by a cursory apercu, an essential acquaintance with the
;; sextuple instruction set shall be the following table's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   -       | Increments the current cell value by one (1).
;;           | If the new value exceeds the maximum of 62, it is reset
;;           | to the lower extremum of zero (0).
;;   ..................................................................
;;   –       | Moves the cell pointer one step to the right.
;;           | If the cell pointer transgresses the desinent cell,
;;           | located at the index ten (10), it is translated to the
;;           | first position one (1).
;;   ..................................................................
;;   _       | Prints the character whose character code corresponds to
;;           | the Linefuck6 encoding to the standard output.
;;   ..................................................................
;;   ~       | Queries the standard input for a character conformant to
;;           | the Linefuck6 repertoire and stores its character code
;;           | in the current cell.
;;           | An error of the type "InvalidCharacterError" is signaled
;;           | if the submitted character does not belong to the
;;           | Linefuck6 character repertoire.
;;   ..................................................................
;;   '       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching '"' command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   "       | If the current cell value does not equal zero (0), moves
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "'" command.
;;           | Otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == LINEFUCK6 AND BRAINFUCK: OPERATIONS COMPARED ==
;; A juxtaposition of Linefuck6 and its brainfuck stock-father in terms
;; of operational competences shall be adduced in the following table:
;; 
;;   ------------------------------------------------------------------
;;   Linefuck6 | brainfuck | Apostille
;;   ----------+-----------+-------------------------------------------
;;   -         | +         | Linefuck6 can only increment to the cell
;;             |           | value 62, subsequently wrapping around to
;;             |           | zero (0).
;;   ..................................................................
;;   –         | >         | Linefuck6 deploys ten cells, wrapping
;;             |           | around to the first if passing beyond the
;;             |           | desinent one.
;;   ..................................................................
;;   _         | .         | Linefuck6 can only output a variety of 63
;;             |           | different characters, covering Latin
;;             |           | letters, decimal digits, and the space.
;;   ..................................................................
;;   ~         | ,         | Linefuck6 can distinguish 63 different
;;             |           | character for input.
;;   ..................................................................
;;   '         | [         | -
;;   ..................................................................
;;   "         | ]         | -
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its rather simplistic nature, and the potential of applying
;; the principles of "noscitur a sociis" to the concepts from brainfuck
;; as inspirations aliunde, a few ambiguites obstinately perpetuate
;; their woning in the specification, a selection thereof shall be
;; adhibited further scrutiny.
;; 
;; == HOW SHALL UNRECOGNIZED INPUTS BE HANDLED? ==
;; Linefuck6's restriction of its character repertoire to a subset of
;; ASCII, tallying 63 members and employing a kenspeckle encoding
;; scheme, whence ensues the cells' equiparation in capacity to respect
;; the range [0, 62], imposes a difficulty in the reception of inputs.
;; 
;; It has been chosen that charactes not included in the Linefuck6
;; repertoire, than directed as a response to an input request, shall
;; beget an error of the type "InvalidCharacterError" apprizing about
;; the nature of the infringement.
;; 
;; 
;; Implementation
;; ==============
;; This simple interpreter has been realized in the programming language
;; Common Lisp, operating immediately on the source code in its verbatim
;; string form, destitute of any intermediate transformations.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-29
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Linefuck6]
;;   The Esolang contributors, "Linefuck6", May 12th, 2023
;;   URL: "https://esolangs.org/wiki/Linefuck6"
;;   
;;   [stackoverflow2015q28313558]
;;   The Stackoverflow contributors,
;;     "How to wrap a number into a range?",
;;     February 4th, 2015
;;   URL: "https://stackoverflow.com/questions/28313558/
;;         how-to-wrap-a-number-into-a-range"
;;   Notes:
;;     - Describes the wrapping of values in an arbitrary range.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type predicates.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-p (candidate &optional (element-type T))
  "Determines whether the CANDIDATE represents a list composed of zero
   or more elements, whose members conform to the ELEMENT-TYPE, which
   defaults to the comprehensive ``T'', returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (declare (type T element-type))
  (the boolean
    (not (null
      (and
        (listp candidate)
        (every
          #'(lambda (element)
              (declare (type T element-type))
              (typep element element-type))
          (the list candidate)))))))

;;; -------------------------------------------------------

(defun hash-table-of-p (candidate &optional (key-type T) (value-type T))
  "Determines whether the CANDIDATE represents a hash table of zero or
   more entries, whose keys conform to the KEY-TYPE and whose values to
   the VALUE-TYPE, where both default to the comprehensive ``T'',
   returning on confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (declare (type T key-type))
  (declare (type T value-type))
  (the boolean
    (not (null
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
                 (typep value value-type))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf  (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (list-of-p candidate element-type)))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose keys conform to
   the KEY-TYPE and whose values conforms to the VALUE-TYPE, both
   defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (hash-table-of-p candidate key-type value-type)))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype lf6-integer ()
  "The ``lf6-integer'' type defines the valid range of integers numbers
   in accordance with the Linefuck6 specification as an integral range
   of [0, 62]."
  '(integer 0 62))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump to back
   jump positions, and vice versa, in a piece of Linefuck6 source code,
   its reification expressed in a hash table that mutually maps fixnum
   objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-pointer ()
  "The ``cell-pointer'' type defines the one-based memory cell pointer
   as an integer in the range [1, 10]."
  '(integer 1 10))

;;; -------------------------------------------------------

(deftype cell-vector ()
  "The ``cell-vector'' type defines the program memory as a
   one-dimensional simple array of ten ``lf6-integer'' objects."
  '(simple-array lf6-integer (*)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Character-Error (error)
  ((offending-character
    :initarg       :offending-character
    :initform      (error "Missing offending character.")
    :reader        invalid-character-error-offending-character
    :type          character
    :documentation "The character whose Linefuck6 character code cannot
                    be determined."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Character-Error condition))
      (declare (type destination             stream))
      (format stream "The character \"~c\" cannot be represented by ~
                      the Linefuck6 character repertoire."
        (invalid-character-error-offending-character condition))))
  (:documentation
    "The ``Invalid-Character-Error'' condition serves to signal an
     anomalous situation in which a character has been provided to the
     system which does belong the recognized character repertoire of the
     Linefuck6 programming language."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character table.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 63) +CHARACTER-SET+))

;;; -------------------------------------------------------

(defparameter +CHARACTER-SET+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "
  "Associates the recognized Linefuck6 characters with their numeric
   codes, designated by the zero-based position in this sequence.")

;;; -------------------------------------------------------

(defun get-lf6-character (code)
  "Returns the character answering to the Linefuck6 character CODE."
  (declare (type lf6-integer code))
  (the character
    (schar +CHARACTER-SET+ code)))

;;; -------------------------------------------------------

(defun get-lf6-character-code (character)
  "Returns the Linefuck6 code for the CHARACTER, or signals an error of
   the type ``Invalid-Character-Error'' if the symbol does not
   contribute to the representable repertoire."
  (declare (type character character))
  (the lf6-integer
    (or (position character +CHARACTER-SET+ :test #'char=)
        (error 'Invalid-Character-Error
               :offending-character character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Computes and returns the jump table for the piece of Linefuck6 source
   CODE, connecting its jump forward and back point locations in the
   same."
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      
      if (char= token #\') do
        (push position forward-jump-points)
      else if (char= token #\") do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched back jump point at position ~d." position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun jump-table-get-destination (jump-table current-position)
  "Returns the obverse jump end point affiliated with the
   CURRENT-POSITION in the JUMP-TABLE, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     current-position))
  (the fixnum
    (or (gethash current-position jump-table)
        (error "No end point associated with the position ~d."
          current-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of numeric range operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-value (value-to-be-wrapped minimum maximum)
  "Wraps the VALUE-TO-BE-WRAPPED around in the closed integer range
   [MINIMUM, MAXIMUM] and returns the result."
  (declare (type integer value-to-be-wrapped))
  (declare (type integer minimum))
  (declare (type integer maximum))
  (the integer
    (+ minimum
       (mod (- value-to-be-wrapped minimum)
            maximum))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of cell vector operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cell-vector ()
  "Creates and returns a new cell vector, composed of ten (10) elements
   that conform to the ``lf6-integer'' type."
  (the cell-vector
    (make-array 10
      :element-type    'lf6-integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents the program memory as a decimal
   account of cells, ranged in the interval [0, 62], and operated upon
   by a cell pointer as the currently active cell designator's
   warklume."
  (cells   (make-cell-vector) :type cell-vector)
  (pointer 1                  :type cell-pointer))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the lf6-integer
    (aref (memory-cells memory)
      (1- (memory-pointer memory)))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceded by a wrapping around of the same into the valid range of
   [0, 62], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (aref (memory-cells memory)
          (1- (memory-pointer memory)))
        (mod new-value 63))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell by one, contingently wrapping
   around to the minimum of zero (0) if transcending the upper bourne of
   62, and returns no value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right, contingently
   wrapping around to the first position if transcending beyond the
   desinent cell, and returns no value."
  (declare (type Memory memory))
  (setf (memory-pointer memory)
    (wrap-value
      (1+ (memory-pointer memory)) 1 10))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Linefuck6 (code)
  "Interprets the piece of Linefuck6 source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (build-jump-table code))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\-
          (memory-increment memory))
        
        (#\–
          (memory-move-right memory))
        
        (#\_
          (write-char
            (get-lf6-character
              (memory-current-cell memory))))
        
        (#\~
          (format T "~&>> ")
          (force-output)
          (setf (memory-current-cell memory)
                (get-lf6-character-code
                  (read-char)))
          (clear-input))
        
        (#\'
          (when (zerop (memory-current-cell memory))
            (setf ip
              (jump-table-get-destination jump-table ip))))
        
        (#\"
          (unless (zerop (memory-current-cell memory))
            (setf ip
              (jump-table-get-destination jump-table ip))))
        
        (otherwise
          NIL))
      (incf ip)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Linefuck6
  "
  ---------------------------------_–
  ----_
  -------__
  ---_–
  --------------------------------------------------------------_–
  –––––––
  ---------------_–
  _---_–
  ------------_–
  ---_
  ")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Linefuck6
  "~_")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Linefuck6
  "-'–~_–––––––––\"")
