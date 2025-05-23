;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Unibrain", invented by the Esolang user Robert de Bath and
;; presented on June 28th, 2015, its propriums' commorancy a derivation
;; of Urban Mueller's language "brainfuck", committing to a syntactical
;; cambistry which replaces the octuple instruction identifiers, in the
;; provenance expressed via one-character symbols, by tokens whose
;; tally of incorporated repeated sub-substrings unambiguously maps, as
;; an integer code from the interval [1, 8], to the operative specimens.
;; 
;; 
;; Concept
;; =======
;; Unibrain furnishes a derivation of the brainfuck programming
;; language, in its conception's firmament serving as an encoding of
;; the entheus' octuple instruction set in the repetitions that form a
;; word's structure.
;; 
;; == TOKENS: WORDS SEPARATED BY WHITESPACES ==
;; A Unibrain program's design enumerates a sequence of zero or more
;; tokens, these compositions whose dispansion, counting one or more
;; characters, ceases at a whitespace as a sepiment.
;; 
;; == ONLY ALPHANUMERIC CHARACTERS ARE RETAINED ==
;; A word's conformation as constituting a character sequence of one or
;; more members' participation, segregated from its peers by
;; whitespaces, adhibits a complete tolerance to any other symbol, but
;; affiliates no further entelech to such eloigned from the alphanumeric
;; contingency. Effectively, any non-alphanumeric element is expunged
;; from the token ere its actual perquisition for decoding purposes.
;; 
;; == CHARACTER CASE DOES NOT MATTER ==
;; No potential for discrimination is apportioned a woning in the
;; characters' cases, whence ensues a compatibility betwixt minuscules
;; and majuscular letters.
;; 
;; == REPETITIONS IN TOKENS ENCODE BRAINFUCK INSTRUCTIONS ==
;; A token's designment proceeds from a non-empty sequence of
;; alphanumeric characters, construed with a distinction betwixt the
;; characters' cases, and inwith whose bournes is encoded exactly one
;; brainfuck instructions.
;; 
;; This concealment's haecceity emerges from a pattern's existency,
;; scilicet, a sub-string repetition which covers the token's entirety.
;; Such a recurrent pattern is inquired into an admissible tally of
;; betwixt inclusive one (1) and inclusive eight (8) times, expected to
;; accomplish, upon its seamless concatenation, the token's content.
;; 
;; An ultimity from this numeric account's obtention, the integral
;; identifier is mapped to exactly one brainfuck instruction as
;; stipulated by the following tabulation:
;; 
;;   ----------------------------
;;   Repetition count | brainfuck
;;   -----------------+----------
;;   1                | >
;;   ............................
;;   2                | <
;;   ............................
;;   3                | +
;;   ............................
;;   4                | -
;;   ............................
;;   5                | .
;;   ............................
;;   6                | ,
;;   ............................
;;   7                | [
;;   ............................
;;   8                | ]
;;   ----------------------------
;; 
;; == DISAMBIGUATION: REALIZED BY FAVORING HIGHER REPETITIONS COUNTS ==
;; A small yet significant set of rules derives from the nomothesia
;; concerning the recognition of patterns and their disambiguation in
;; requisite circumstances:
;; 
;;   (1) Only a repetition count from the closed interval [1, 8] may
;;       be imputed. Forbisens with a higher mete of recurrence are not
;;       embraced in the solution.
;;   
;;   (2) If more than one mode of partitioning, conformable with the
;;       range [1, 8], applies to the token's analysis, the greatest
;;       account must be chosen.
;;       
;;       For instance, upon a confrontation with the phrase
;;       
;;         abababab
;;       
;;       three contingencies for the partition generation exist:
;;       
;;         (a) "ab",   repeated four (4) times
;;         (b) "abab", repeated two  (2) times.
;;       
;;       In this case, the alternative (a) constitutes the paravaunt
;;       selection, as four copies supersede two in the quantity.
;; 
;; == THE PARTITIONING PROCESS ==
;; A consectary from the aboon explications, an algorithm for the
;; repetition count's extraction shall not be reckoned as a task of
;; peisant complexity; natheless, a proposal for siccan service shall
;; be adduced in this documentation's tmema
;; "APPENDIX A: STRING PARTITIONING", commorant in the "Appendices"
;; segment.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical application of one's conspectuity, a Unibrain
;; program's conformation is edified upon the firmament of words, their
;; sepiments constituting whitespaces, while any constituent in such a
;; token merely accounts for the membership in the alphanumeric species,
;; ignoring all other characters inwith our without a word's bournes.
;; 
;; == TOKENS: WORDS WHOSE ALPHANUMERIC CONSTITUENTS MATTER ==
;; A token's diorism ensues from a sequence of one or more characters,
;; among which only those from the alphanumeric realm contribute, while
;; other participants' dation shall be entirely neglected.
;; 
;; As a forbisen, the word
;; 
;;   Baa_$:!Baa
;; 
;; is tantamount to a mete of commensurate patration with
;; 
;;   BaaBaa
;; 
;; Compounds whose conformation wists of no membership other than those
;; ignored entities will receive an adhibition of tolerance in a grade
;; paregal to their inefficacy.
;; 
;; The word
;; 
;;   _$:!
;; 
;; as an example, is entalented with no epiphenomenal potential at all.
;; 
;; The case-insensitive nature commorant in the diorism further
;; homologates the equiparation of; for instance,
;; 
;;   Baa
;; 
;; and
;; 
;;   bAA
;; 
;; enjoy, vauncing from the adiaphorous conception of cases, a perfect
;; congruency.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of Unibrain's recipiency does not elude brainfuck's
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
;; Unibrain's instruction set constitutes a reformulation of brainfuck's
;; octuple cleronomy, molded into the tally of repetitions comprising
;; a token's componency.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall furnish a sufficient mete of
;; nortelry concerning the language's operative competences.
;; 
;; Please heed that, given the bourneless mickleness accommodated a
;; woning in the expression of repetitive phrases capacitated to form
;; any of the eight instructions' code, the most abstract notion has
;; been adjudged the most behoovable; in this case, the repetition
;; count occupies the sinistral column, in lieu of an exemplary word.
;; 
;;   ------------------------------------------------------------------
;;   Repetition count | Effect
;;   -----------------+------------------------------------------------
;;   1                | Translates the cell pointer one step to the
;;                    | right.
;;   ..................................................................
;;   2                | Translates the cell pointer one step to the
;;                    | left.
;;   ..................................................................
;;   3                | Increments the current cell value by one (1).
;;                    | If the new state transgresses the upper march
;;                    | of 255, the value wraps around to the minimum
;;                    | of zero (0).
;;   ..................................................................
;;   4                | Decrements the current cell value by one (1).
;;                    | If the new state transgresses the lower march
;;                    | of zero (0), the value wraps around to the
;;                    | maximum of 255.
;;   ..................................................................
;;   5                | Prints the character whose ASCII code
;;                    | corresponds to the current cell value to the
;;                    | standard output conduit.
;;   ..................................................................
;;   6                | Queries the standard input conduit for a
;;                    | character and stores its ASCII code in the
;;                    | current cell.
;;   ..................................................................
;;   7                | If the current cell contains zero (0), moves
;;                    | the instruction pointer (IP) forward to the
;;                    | position immediately succeeding the matching
;;                    | jump end instruction, signified by a token with
;;                    | a repetition count of eight (8); otherwise
;;                    | proceeds as usual.
;;   ..................................................................
;;   8                | If the current cell does not contain zero (0),
;;                    | moves the instruction pointer (IP) back to the
;;                    | position immediately succeeding the matching
;;                    | jump start instruction, signified by a token
;;                    | with a repetition count of seven (7); otherwise
;;                    | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == UNIBRAIN AND BRAINFUCK ==
;; Its status as an encoding of brainfuck serves to entalent Unibrain's
;; instruction set with an equipollence meted with patration in its
;; equiparation with the entheus. Hence, the following juxtaposition
;; of the Unibrain repetition tallies in a token and the ensuing
;; brainfuck epiphenomenon shall be limned:
;; 
;;   ----------------------------
;;   Repetition count | brainfuck
;;   -----------------+----------
;;   1                | >
;;   ............................
;;   2                | <
;;   ............................
;;   3                | +
;;   ............................
;;   4                | -
;;   ............................
;;   5                | .
;;   ............................
;;   6                | ,
;;   ............................
;;   7                | [
;;   ............................
;;   8                | ]
;;   ----------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, its actual execution process
;; experiencing the prevenience of a transformation stage from the
;; source code string into an intermediate instruction vector form.
;; 
;; 
;; Appendices
;; ==========
;; A few topics' participation harbors its woning in the gloam betwixt
;; such importance as to vindicate an admission into the documentation's
;; main body and a paravail mete's supputation, yet peisant anent a
;; mentioning within. These subjects shall be the following appendices'
;; cynosure.
;; 
;; == APPENDIX A: STRING PARTITIONING ==
;; The cynosure of the Unibrain lexical analyzation process appertains
;; to the detection of repetitive sub-strings in a character sequence
;; which in a closed catena's formation replicate the source word.
;; 
;; This dever's attendance is facilitated in the gnarity that at most
;; eight partitions may be administered to the token, governed
;; concomitantly by a minimum of one.
;; 
;; The extracted repetition tally conflates with an encoded brainfuck
;; command's unambiguous identification, this being an integral datum
;; desumed from the range [1, 8].
;; 
;; A possible solution to the predicament of a string forbisen's
;; recognition shall be adduced in the following pseudocode. The telos
;; of this endeavor wones in the function "extractInstructionCode",
;; thilk decodes a token into the numeric identifier.
;; 
;;   { Returns an ordered list of the token's characters grouped into }
;;   { substrings of equal length.                                    }
;;   function splitTokenIntoPartitions (token, numberOfPartitions)
;;     Input:
;;       token:              The word to segregate into a tally of
;;                           NUMBER_OF_PARTITIONS. Its size is expected
;;                           to be divisble by the NUMBER_OF_PARTITIONS
;;                           without rest. Its characters are indexed
;;                           commencing with inclusive zero (0).
;;       numberOfPartitions: The tally of tmemata into which the TOKEN
;;                           shall be divided; where it holds:
;;                             (numberOfPartitions is an integer) AND
;;                             (1 <= numberOfPartitions <= 8)     AND
;;                             ((length(token) modulo
;;                               numberOfPartitions) = 0).
;;     
;;     Output:
;;       partitions:         An ordered list which holds a tally of
;;                           NUMBER_OF_PARTITIONS sub-strings, formed
;;                           by the TOKEN's segregation into tmemata
;;                           of equal extent. Its elements are indexed
;;                           commencing with inclusive zero (0).
;;     
;;     Process:
;;       let partitions    <- empty ordered list
;;       let tokenLength   <- length(token)
;;       let partitionSize <- tokenLength / numberOfPartitions
;;       
;;       for partitionIndex from 0 to (numberOfPartitions - 1) do
;;         let tmemaStartIndex  <- partitionIndex * partitionSize
;;         let tmemaEndIndex    <- tmemaStartIndex + partitionSize
;;         let currentPartition <- token[tmemaStartIndex,
;;                                       tmemaEndIndex]
;;         
;;         append currentPartition to partitions
;;       end repeat
;;       
;;       return partitions
;;   end function
;;   
;;   
;;   { Determines whether all strings in the the ordered list of      }
;;   { partitions are equal, without consideration of the characters' }
;;   { case.                                                          }
;;   function allPartitionsAreEqual (partitions)
;;     Input:
;;       partitions:         An ordered list of string, all equal in
;;                           their length. Its elements are indexed
;;                           commencing with inclusive zero (0).
;;     
;;     Output:
;;       allPartitionsMatch: A Boolean flag which determines whether
;;                           all elements in the PARTITIONS list are
;;                           lexicographically equal, disregarding the
;;                           distinction betwixt minuscules and
;;                           majuscules.
;;     
;;     Process:
;;       let allPartitionsMatch <- true
;;       let numberOfPartitions <- length(partitions)
;;       
;;       for partitionIndex from 0 to (numberOfPartitions - 2) do
;;         let leftPartition  <- partitions[partitionIndex]
;;         let rightPartition <- partitions[partitionIndex + 1]
;;         
;;         if leftPartition != rightPartition then
;;           allPartitionsMatch <- false
;;           terminate for loop
;;         end if
;;       end for
;;       
;;       return allPartitionsMatch
;;   end function
;;   
;;   
;;   { Returns a numeric instruction code for the brainfuck operation }
;;   { represented by the token, this code being an integral number   }
;;   { from the closed interval [1, 8].                               }
;;   function extractInstructionCode (token)
;;     Input:
;;       token:           A word composed of one or more alphanumeric
;;                        characters.
;;     
;;     Output:
;;       instructionCode: A numeric identifier which maps the TOKEN
;;                        to one of the octuple brainfuck commands;
;;                        the value constituting an integer number from
;;                        the closed interval [1, 8]. This datum
;;                        conflates with the tally of repetitions of
;;                        a sub-string in the TOKEN, the same
;;                        completely replicates the TOKEN's content
;;                        without consideration of the character cases.
;;     
;;     Process:
;;       let instructionCode <- nil
;;       let tokenLength     <- length(token)
;;       
;;       for numberOfPartitions from 8 down to 1 do
;;         if (tokenLength modulo numberOfPartitions) = 0
;;           let partitions       <- splitTokenIntoPartitions(token)
;;           let partitionMatches <- allPartitionsAreEqual(partitions)
;;           
;;           if partitionMatches then
;;             instructionCode <- numberOfPartitions
;;             terminate for loop
;;           end if
;;         end if
;;       end repeat
;;       
;;       return instructionCode
;;   end function
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-05-13
;; 
;; Sources:
;;   [esolang2020Unibrain]
;;   The Esolang contributors, "Unibrain", June 20th, 2020
;;   URL: "https://esolangs.org/wiki/Unibrain"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, returns ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun admits-any-type-p (type-specifier)
  "Determines whether the TYPE-SPECIIFER accepts objects of any species
   by constituting the generic sentinel ``*'', returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (resolve-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))

;;; -------------------------------------------------------

(defun object-is-of-type-p (candidate expected-type)
  "Determines whether the CANDIDATE complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type T candidate))
  (declare (type T expected-type))
  (the boolean
    (resolve-boolean-value-of
      (or (admits-any-type-p expected-type)
          (typep             candidate expected-type)))))

;;; -------------------------------------------------------

(defmacro define-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type nevened by the TYPE-NAME, utilizing the
   LAMBDA-LIST for its formal parameters, and appropriating the
   CANDIDATE-NAME for the subject of its docimasy, evaluates the BODY
   forms, and construes the desinent form's primary return value as its
   conclusion, a \"generalized boolean\" value of \"true\" corresponding
   to its acceptance, while \"false\" communicates the rejection.
   ---
   If the first BODY represents a string object, thilk is interpreted as
   the type definition's documentation string, and is reappropriated for
   this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, thilk
   defaults to the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (admits-any-type-p element-type)
      (loop
        for    current-element of-type T in (the list candidate)
        always (typep current-element element-type)))))

;;; -------------------------------------------------------

(define-bespoke-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, for each such component holds the key's lealty to the
   the KEY-TYPE and the value's to the VALUE-TYPE, both resorting to the
   generic sentinel ``*'' in their default state."
  (and
    (hash-table-p candidate)
    (or
      (and (admits-any-type-p key-type)
           (admits-any-type-p value-type))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and (object-is-of-type-p current-key   key-type)
               (object-is-of-type-p current-value value-type))))))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   brainfuck operations."
  '(member
    :move-cell-pointer-right
    :move-cell-pointer-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a parsed representation of
   Unibrain operations as a one-dimensional simple array of
   ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype connection-map ()
  "The ``connection-map'' type defines a bidirectional association
   betwixt the matching jump points in a parsed Unibrain program by
   mediation of their zero-based indices, manifesting in a hash table
   whose keys and values both assume fixnum types."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value whose componency
   enumerates eight accolent bits, thus covering the closed integral
   interval of [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype sparse-byte-vector ()
  "The ``sparse-byte-vector'' type defines an infinite one-dimensional
   sparse array of unsigned bytes, amenable to signed integer indices,
   by adminiculum of a hash table, the integer keys of which correspond
   to the array subscripts, while the octet values accommodate the
   elements."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which enumerates, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype unibrain-token-generator ()
  "The ``unibrain-token-generator'' type defines a function whose dever
   wones in the translation of a brainfuck command's numeric identifier,
   occupying the closed integral interval [1, 8], into an equivalent
   Unibrain token.
   ---
   Proceeding from this diorism, the function must comply to the
   signature
     lambda (command-identifier destination) => ignored-result
   where the COMMAND-IDENTIFIER represents a numeric encoding of one of
   the octuple brainfuck instructions in an integer number desumed from
   the range [1, 8], while the DESTINATION contributes the specification
   of the sink to issue the Unibrain instruction representation on. The
   function result will be ignored; its type is thus encumbered from any
   further stipulations."
  '(function ((integer 1 8) destination) *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of array operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-array ((index-variable element-variable array)
                    &body body)
  "Evaluates the ARRAY and iterates its entire elements, utilizing the
   INDEX-VARIABLE to refer to the currently traversed index and the
   ELEMENT-VARIABLE for the element at that position, evaluates the
   BODY forms, and returns no value.
   ---
   Please heed the homologation of multidimensional arrays for the
   ARRAY parameter, thilk will be processed in row-major order."
  (let ((evaluated-array (gensym)))
    (declare (type symbol evaluated-array))
    `(let ((,evaluated-array ,array))
       (declare (type array ,evaluated-array))
       (loop
         for ,index-variable
           of-type fixnum
           from    0
           below   (array-total-size ,evaluated-array)
         for ,element-variable
           of-type T
           =       (row-major-aref ,evaluated-array ,index-variable)
         do
           (progn
             ,@body))
       (values))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction vector operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction-vector (instructions)
  "Creates and returns a fresh ``instruction-vector'' comprehending the
   elements provided in the INSTRUCTIONS list."
  (declare (type (list-of instruction) instructions))
  (the instruction-vector
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Unibrain program.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing Unibrain program instructions.")
    :type          instruction-vector
    :documentation "An array of parsed Unibrain instructions."))
  (:documentation
    "The ``Program'' class implements a parsed Unibrain program as a
     one-dimensional simple array of ``instruction'' objects."))

;;; -------------------------------------------------------

(defun make-program (instructions)
  "Creates and returns a fresh Unibrain ``Program'' founded upon the
   list of INSTRUCTIONS."
  (declare (type (list-of instruction) instructions))
  (the Program
    (make-instance 'Program :instructions
      (make-instruction-vector instructions))))

;;; -------------------------------------------------------

(defun get-program-size (program)
  "Returns the tally of instructions partaking of this Unibrain
   PROGRAM."
  (declare (type Program program))
  (the fixnum
    (length
      (slot-value program 'instructions))))

;;; -------------------------------------------------------

(defun index-is-valid-for-program-p (program probed-index)
  "Determines whether the PROBED-INDEX constitutes a valid zero-based
   location into the PROGRAM, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  probed-index))
  (the boolean
    (with-slots (instructions) program
      (declare (type instruction-vector instructions))
      (resolve-boolean-value-of
        (array-in-bounds-p instructions probed-index)))))

;;; -------------------------------------------------------

(defun get-instruction-at (program index)
  "Returns the instruction located in the Unibrain PROGRAM at the
   zero-based INDEX."
  (declare (type Program program))
  (declare (type fixnum  index))
  (the instruction
    (with-slots (instructions) program
      (declare (type instruction-vector instructions))
      (aref instructions index))))

;;; -------------------------------------------------------

(defmacro do-program-instructions
    ((index-variable instruction-variable program)
     &body body)
  "Traverses the PROGRAM's instruction sequence in its extant order,
   employing the INDEX-VARIABLE to refer to the contemporaneously
   sojourned position and the INSTRUCTION-VARIABLE as a reference to the
   instruction at the same index, while evaluating the BODY forms, and
   returns no value."
  `(do-array (,index-variable
              ,instruction-variable
              (slot-value ,program 'instructions))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun divisor-is-aliquot-to-p (dividend divisor)
  "Determines whether the DIVISOR constitutes an aliquot part of the
   DIVIDEND, that is, the former divides the latter without any residing
   orrels, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type integer dividend))
  (declare (type integer divisor))
  (the boolean
    (resolve-boolean-value-of
      (zerop
        (mod dividend divisor)))))

;;; -------------------------------------------------------

(defun supputate-next-lower-aliquot (dividend previous-aliquot)
  "Returns for the DIVIDEND the aliquot strictly smaller than the
   PREVIOUS-ALIQUOT, or answers with the value zero (0) upon its
   disrespondency."
  (declare (type integer dividend))
  (declare (type integer previous-aliquot))
  (the (integer 0 *)
    (if (> previous-aliquot 1)
      (loop
        for current-divisor
          of-type (integer 1 *)
          from    (1- previous-aliquot)
          downto  1
        
        when (divisor-is-aliquot-to-p dividend current-divisor) do
          (return current-divisor)
        
        finally
          (return 1))
      0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   inwith whose diorism are incorporated the space, horizontal tab, and
   newline entities, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations for partitioner.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-repeats-at-p (source start extent)
  "Determines whether the tmema commencing at the inclusive START
   position into the SOURCE and encompassing the EXTENT tally of
   subsequent characters, is replicated immediately succeeding the thus
   demarcated parcel, while disregarding the contents' case, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum extent))
  (let* ((end-of-left-moiety  (+ start extent))
         (end-of-right-moiety (+ end-of-left-moiety extent)))
    (declare (type fixnum end-of-left-moiety))
    (declare (type fixnum end-of-right-moiety))
    (the boolean
      (resolve-boolean-value-of
        (string-equal source source
          :start1 start
          :end1   end-of-left-moiety
          :start2 end-of-left-moiety
          :end2   end-of-right-moiety)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of partition calculator.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type string        *partition-source*))
(declaim (type (integer 0 *) *partition-source-size*))
(declaim (type (integer 0 *) *current-partition-count*))
(declaim (type (integer 0 *) *current-partition-size*))

;;; -------------------------------------------------------

(defparameter *partition-source* ""
  "The word whose partitions shall be subjected to supputations.")

(defparameter *partition-source-size* 0
  "The tally of characters comprising the *PARTITION-SOURCE* to
   segregate into its partitions.")

(defparameter *current-partition-count* 0
  "The most recently supputated tally of partitions which, if multiplied
   by the *CURRENT-PARTITION-SIZE*, cover the *PARTITION-SOURCE-SIZE* in
   its entirety.")

(defparameter *current-partition-size* 0
  "The most recently supputated partition size, this constituting an
   aliquot of the *PARTITION-SOURCE-SIZE*.")

;;; -------------------------------------------------------

(defun update-current-partition-size ()
  "Supputates the partition size based upon the
   *CURRENT-PARTITION-COUNT* and its appertaining state and returns no
   value."
  (setf *current-partition-size*
    (if (plusp *current-partition-count*)
      (/ *partition-source-size*
         *current-partition-count*)
      0))
  (values))

;;; -------------------------------------------------------

(defun set-partition-source (new-source)
  "Sets the *PARTITION-SOURCE* to the NEW-SOURCE, resets all
   appertaining state variables, and returns no value."
  (declare (type string new-source))
  (psetf
    *partition-source*      new-source
    *partition-source-size* (length new-source))
  (setf *current-partition-count*
    (supputate-next-lower-aliquot *partition-source-size* 9))
  (update-current-partition-size)
  (values))

;;; -------------------------------------------------------

(defun request-next-partition ()
  "Configures the partitioning to attempt the next lower tally of
   divisions with respect to the contemporaneous
   *CURRENT-PARTITION-COUNT*, if possible, and returns no value."
  (setf *current-partition-count*
    (supputate-next-lower-aliquot
      *partition-source-size*
      *current-partition-count*))
  (update-current-partition-size)
  (values))

;;; -------------------------------------------------------

(defun current-partition-matches-p ()
  "Determines whether the current partition of the source word is
   capacitated to replicate the provenance string in its entirety,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (loop
      for left-partition-start-point
        of-type fixnum
        =       0
        then    left-partition-end-point
      for left-partition-end-point
        of-type fixnum
        =       (+ left-partition-start-point
                   *current-partition-size*)
      
      while
        (< left-partition-end-point
           (length *partition-source*))
      
      always
        (string-repeats-at-p
          *partition-source*
          left-partition-start-point
          *current-partition-size*))))

;;; -------------------------------------------------------

(defun find-matching-partition-count (word)
  "Finds and returns for the WORD the covenable tally of partitions from
   the closed interval [1, 8], with larger values being favored.
   ---
   This operation modifies the partitioning facility's state; as a
   consectary, the resulting number of partitions will not only be
   delivered, but also concomitantly stored in the
   *CURRENT-PARTITION-COUNT*, until another dedicated instruction may
   alter the infrastructure comprehending this global variable."
  (declare (type string word))
  (set-partition-source word)
  (the (integer 1 8)
    (loop
      until
        (current-partition-matches-p)
      do
        (request-next-partition)
      finally
        (return *current-partition-count*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-brainfuck-instruction (number-of-repetitions)
  "Returns for the NUMBER-OF-REPETITIONS governing a conceived word's
   conformation the corresponding instruction, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type (integer 0 *) number-of-repetitions))
  (the instruction
    (case number-of-repetitions
      (1         :move-cell-pointer-right)
      (2         :move-cell-pointer-left)
      (3         :increment)
      (4         :decrement)
      (5         :output)
      (6         :input)
      (7         :jump-forward)
      (8         :jump-back)
      (otherwise
        (error "Cannot decode a repetition count of ~d."
          number-of-repetitions)))))

;;; -------------------------------------------------------

(defun decode-word (word)
  "Returns for the WORD a covenable ``instruction'' representation."
  (declare (type string word))
  (the instruction
    (decode-brainfuck-instruction
      (find-matching-partition-count word))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-non-alphanumeric-characters (source)
  "Creates and returns a fresh string retaining merely the alphanumeric
   characters commorant in the SOURCE."
  (declare (type string source))
  (the string
    (remove-if-not #'alphanumericp source)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents the null string, that is,
   a character sequence composed of exactly zero members, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (resolve-boolean-value-of
      (string= source ""))))

;;; -------------------------------------------------------

(defun locate-start-of-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, detects
   the nearest following word and returns its first character's index;
   otherwise, upon its disrespondency, responds with the SOURCE's
   length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-current-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, detects
   the end of the nearest following word and index immediately suceeding
   its desinent character; otherwise, upon its disrespondency, responds
   with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-bournes-of-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, detects
   the nearest following word and returns two values:
     (1) If a word could be encountered, its first character's index,
         otherwise the SOURCE's length.
     (2) If a word could be encountered, the position immediately
         succeeding its desinent character, otherwise the SOURCE's
         length."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((word-start-position (locate-start-of-next-word source start)))
    (declare (type fixnum word-start-position))
    (the (values fixnum fixnum)
      (values
        word-start-position
        (locate-end-of-current-word source word-start-position)))))

;;; -------------------------------------------------------

(defun extract-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, seeks
   the word and returns two values:
     (1) If a word could be detected, a fresh string comprehending its
         characters; otherwise produces a fresh empty string.
     (2) If a word could be detected, the position immediately
         succeeding its desinent character; otherwise responds with the
         SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (multiple-value-bind (word-start-position word-end-position)
        (locate-bournes-of-next-word source start)
      (declare (type fixnum word-start-position))
      (declare (type fixnum word-end-position))
      (values
        (subseq source word-start-position word-end-position)
        word-end-position))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts from the piece of Unibrain SOURCE code the ensconced
   instructions and returns a ``Program'' representation thereof."
  (declare (type string source))
  (let ((current-position 0)
        (current-word     ""))
    (declare (type fixnum current-position))
    (declare (type string current-word))
    (flet
        ((read-word ()
          "Proceeding from the CURRENT-POSITION into the SOURCE,
           extracts the nearest following word, updates the
           CURRENT-POSITION to the location immediately succeeding the
           same, and returns no value."
          (multiple-value-setq (current-word current-position)
            (extract-next-word source current-position))
          (setf current-word
            (remove-non-alphanumeric-characters current-word))
          (values)))
      (the Program
        (make-program
          (loop
            do
              (read-word)
            
            unless (string-is-empty-p current-word)
              collect (decode-word current-word)
            
            until
              (>= current-position
                  (length source))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          connection-map
    :documentation "Connects a Unibrain program's jump points in a
                    bidirectional fashion by adminiculum of their
                    zero-based positions into the same."))
  (:documentation
    "The ``Jump-Table'' class serves in the castaldy of a parsed
     Unibrain program's jump points, realized in a bilateral fashion by
     mediation of their zero-based indices into the ensconcing
     program."))

;;; -------------------------------------------------------

(defun prepare-pristine-jump-table ()
  "Creates and returns an initially vacant ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (table start-point destination-point)
  "Connects the START-POINT and DESTINATION-POINT in the jump TABLE in a
   bidirectional fashion and returns no value."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     destination-point))
  (with-slots (connections) table
    (declare (type connection-map connections))
    (psetf
      (gethash start-point       connections) destination-point
      (gethash destination-point connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun locate-jump-destination (table start-point)
  "Returns for the START-POINT in the jump TABLE the allied destination
   position; or signals upon its disrespondency an error of an
   unspecified type."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (the fixnum
    (with-slots (connections) table
      (declare (type connection-map connections))
      (or (gethash start-point connections)
          (error "No destination associated with the jump point ~d."
            start-point)))))

;;; -------------------------------------------------------

(defun supputate-jump-table (program)
  "Creates and returns for the Unibrain PROGRAM a fresh jump table which
   ligates its matching jump points in a bilateral fashion by mediation
   of their zero-based positions into the instruction sequence."
  (declare (type program program))
  (let ((jump-table          (prepare-pristine-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (do-program-instructions (current-position
                              current-instruction
                              program)
      (case current-instruction
        (:jump-forward
          (push current-position forward-jump-points))
        (:jump-back
          (if forward-jump-points
            (connect-jump-points jump-table
              (pop forward-jump-points)
              current-position)
            (error "Unmatched back jump point as ~:r instruction"
              current-position)))
        (otherwise
          NIL)))
    (when forward-jump-points
      (error "Unmatched forward jump point~p at position~:p ~{~d~^, ~}."
        (length forward-jump-points)
        forward-jump-points))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          sparse-byte-vector
    :documentation "A sparse vector of unsigned byte-valued cells,
                    manifesting in a hash table.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, which designates the currently
                    active cell by its castaldy of the selected index,
                    or key, in the CELLS table."))
  (:documentation
    "The ``Tape'' class serves in the program memory's establishment,
     conceived in a linear cell arrangement's guise, its componency
     delineated by cells that each mete a scalar unsigned byte's
     capacity, the salvatory being operated upon by a cell pointer
     whose capacitation homologates the currently active unit's
     addressing."))

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Creates and returns a fresh ``Tape'', commorant in its inchoate
   state, and returns no value."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (with-slots (cells pointer) tape
      (declare (type sparse-byte-vector cells))
      (declare (type integer            pointer))
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Transfers the NEW-VALUE, contingently adjusted by overflowing at the
   admissible bournes in order to accommodate the valid byte range of
   [0, 255], into the TAPE's currently selected cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-slots (cells pointer) tape
    (declare (type sparse-byte-vector cells))
    (declare (type integer            pointer))
    (setf (gethash pointer cells)
          (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun translate-cell-pointer-by (tape offset)
  "Relocates the TAPE's cell pointer by the OFFSET relative to its
   contemporaneous position and returns no value."
  (declare (type Tape            tape))
  (declare (type (integer -1 +1) offset))
  (incf (slot-value tape 'pointer) offset)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :type          Program
    :documentation "The parsed Unibrain program to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position into
                    the PROGRAM's instruction sequence.")
   (jump-table
    :type          Jump-Table
    :documentation "Connects the jump points in the PROGRAM via their
                    zero-based indices.")
   (tape
    :initform      (prepare-pristine-tape)
    :type          Tape
    :documentation "The program memory as a bilaterally infinite
                    dispansion of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of accompassing
     actual efficacy to a Unibrain program supplied in the form of an
     instruction sequence."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Builds a connable jump table for the INTERPRETER's Unibrain program,
   stores thilk in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-table)
    (supputate-jump-table
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Unibrain
   PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun entire-program-has-been-processed-p (interpreter)
  "Determines whether the program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type Program program))
      (declare (type fixnum  ip))
      (not (index-is-valid-for-program-p program ip)))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction referenced by the INTERPRETER's instruction
   pointer (IP)."
  (declare (type Interpreter interpreter))
  (the instruction
    (with-slots (program ip) interpreter
      (declare (type Program program))
      (declare (type fixnum  ip))
      (get-instruction-at program ip))))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Program program))
    (declare (type fixnum  ip))
    (when (index-is-valid-for-program-p program ip)
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (instruction interpreter)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((instruction (eql :move-cell-pointer-right))
     (interpreter Interpreter))
  "Ignoring the INSTRUCTION, translates the cell pointer governed by the
   INTERPRETER's tape one step to the right and returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (translate-cell-pointer-by tape 1))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((instruction (eql :move-cell-pointer-left))
     (interpreter Interpreter))
  "Ignoring the INSTRUCTION, translates the cell pointer governed by the
   INTERPRETER's tape one step to the left and returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (translate-cell-pointer-by tape -1))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :increment))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, increments the value of the INTERPRETER
   tape's current cell by one and returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (incf (current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :decrement))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, decrements the value of the INTERPRETER
   tape's current cell by one and returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (decf (current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :output))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, prints the character whose ASCII code
   corresponds to the INTERPRETER tape's current cell to the standard
   output conduit and returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (format *standard-output* "~c"
      (code-char
        (current-cell-value tape))))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :input))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, queries the standard input conduit for a
   character and stores its ASCII code in the INTERPRETER tape's current
   cell."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (format *standard-output* "~&>> ")
    (finish-output *standard-output*)
    (setf (current-cell-value tape)
      (char-code
        (read-char *standard-input* NIL #\Null)))
    (clear-input *standard-input*))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :jump-forward))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, determines whether the INTERPRETER tape's
   current cell contains the value zero (0), on confirmation
   moving the INTERPRETER's instruction pointer (IP) forward to the
   matching jump end point, otherwise accompassing no causatum; and in
   any case returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape jump-table ip) interpreter
    (declare (type Tape       tape))
    (declare (type Jump-Table jump-table))
    (declare (type fixnum     ip))
    (when (zerop (current-cell-value tape))
      (setf ip
        (locate-jump-destination jump-table ip))))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :jump-back))
                                (interpreter Interpreter))
  "Ignoring the INSTRUCTION, determines whether the INTERPRETER tape's
   current cell contains a non-zero value, on confirmation moving the
   INTERPRETER's instruction pointer (IP) backward to the matching jump
   start point, otherwise accompassing no causatum; and in any case
   returns no value."
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (type Interpreter interpreter))
  (with-slots (tape jump-table ip) interpreter
    (declare (type Tape       tape))
    (declare (type Jump-Table jump-table))
    (declare (type fixnum     ip))
    (unless (zerop (current-cell-value tape))
      (setf ip
        (locate-jump-destination jump-table ip))))
  (values))

;;; -------------------------------------------------------

(defun process-current-instruction (interpreter)
  "Evaluates the INTERPRETER's currently pointed instruction and returns
   no value."
  (declare (type Interpreter interpreter))
  (process-instruction
    (get-current-instruction interpreter)
    interpreter)
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Unibrain program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (entire-program-has-been-processed-p interpreter) do
    (process-current-instruction interpreter)
    (advance-instruction-pointer interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Unibrain (code)
  "Interprets the piece of Unibrain source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-instructions code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck code generator.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-instructions-to-brainfuck
    (instructions
     &key (destination *standard-output*))
  "Generates for the Unibrain INSTRUCTIONS an equivalent brainfuck
   program, writes thilk to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise produces and
   returns a fresh string comprehending the output."
  (declare (type program     instructions))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (do-program-instructions (current-position
                                current-instruction
                                instructions)
        (format destination "~c"
          (case current-instruction
            (:move-cell-pointer-right #\>)
            (:move-cell-pointer-left  #\<)
            (:increment               #\+)
            (:decrement               #\-)
            (:output                  #\.)
            (:input                   #\,)
            (:jump-forward            #\[)
            (:jump-back               #\])
            (otherwise
              (error "Invalid instruction: ~a at position ~d."
                current-instruction
                current-position)))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (write-instructions-to-brainfuck
          instructions
          :destination brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Unibrain-to-brainfuck converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-Unibrain-to-brainfuck
    (unibrain-code
     &key (destination *standard-output*))
  "Generates for the piece of UNIBRAIN-CODE an equivalent brainfuck
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise produces a fresh
   string comprehending the output."
  (declare (type string      unibrain-code))
  (declare (type destination destination))
  (the (or null string)
    (write-instructions-to-brainfuck
      (extract-instructions unibrain-code)
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations for brainfuck converter. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-repeatedly (content-to-repeat
                         number-of-repetitions
                         destination)
  "Prints the CONTENT-TO-REPEAT the NUMBER-OF-REPETITIONS times to the
   DESTINATION and returns for a non-``NIL'' DESTINATION the ``NIL''
   value; otherwise, for a ``NIL'' DESTINATION, creates and returns a
   fresh string comprehending the result."
  (declare (type T             content-to-repeat))
  (declare (type (integer 0 *) number-of-repetitions))
  (declare (type destination   destination))
  (the (or null string)
    (format destination "~v@{~a~:*~}"
      number-of-repetitions
      content-to-repeat)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck identifier table.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 8) +BRAINFUCK-COMMAND-TOKENS+))

;;; -------------------------------------------------------

(defparameter +BRAINFUCK-COMMAND-TOKENS+
  "><+-.,[]"
  "Associates the recognized brainfuck command symbols with their
   zero-based positions in the Unibrain encoding table.
   ---
   Please heed that Unibrain assigns to the octuple character set
   desumed from brainfuck as integer number from the closed interval
   [1, 8], while this affiliation shifts the range to [0, 7], ultimately
   necessitating modulations upon a desideration of the tabulation's
   utilization.")

;;; -------------------------------------------------------

(defun locate-brainfuck-command-token (token)
  "Returns the zero-based position of the brainfuck TOKEN in the
   Unibrain encoding, or responds with ``NIL'' upon its absence from the
   recognized membership."
  (the (or null (integer 0 7))
    (position token +BRAINFUCK-COMMAND-TOKENS+ :test #'char=)))

;;; -------------------------------------------------------

(defun brainfuck-command-character-p (candidate)
  "Determines whether the CANDIDATE represents a brainfuck command
   symbol, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-boolean-value-of
      (locate-brainfuck-command-token candidate))))

;;; -------------------------------------------------------

(defun encode-brainfuck-token (token)
  "Returns for the brainfuck command TOKEN the identifying instruction
   code as an integer number in the range [1, 8], or signals an error
   of an unspecified type upon its disrespondency."
  (declare (type character token))
  (the (integer 1 8)
    (1+
      (or (locate-brainfuck-command-token token)
          (error "No brainfuck command token: ~c." token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Unibrain converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type unibrain-token-generator
               +DEFAULT-UNIBRAIN-TOKEN-GENERATOR+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-UNIBRAIN-TOKEN-GENERATOR+
  #'(lambda (instruction-code destination)
      (declare (type (integer 1 8) instruction-code))
      (declare (type destination   destination))
      (print-repeatedly "Developers" instruction-code destination)
      (values))
  "The default translation routine for a brainfuck command symbol's
   transcription into a Unibrain token, thilk replicates the phrase
   \"Developers\" an account of times tantamount to the numeric
   instruction code, while returning no value.")

;;; -------------------------------------------------------

(defun convert-brainfuck-to-Unibrain
    (brainfuck-code
     &key
       (unibrain-token-generator +DEFAULT-UNIBRAIN-TOKEN-GENERATOR+)
       (destination             NIL))
  "Generates for the BRAINFUCK-CODE an equivalent Unibrain program,
   optionally employing the UNIBRAIN-TOKEN-GENERATOR for the tokens'
   production if not desiderating repetitions of the \"Developers\"
   string, and prints the resulting Unibrain code to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, produces a fresh string comprehending the
   result."
  (declare (type string                   brainfuck-code))
  (declare (type unibrain-token-generator unibrain-token-generator))
  (declare (type destination              destination))
  (the (or null string)
    (if destination
      (do-array (current-position current-character brainfuck-code)
        (when (brainfuck-command-character-p current-character)
          (fresh-line destination)
          (funcall unibrain-token-generator
            (encode-brainfuck-token current-character)
            destination)))
      (with-output-to-string (unibrain-code)
        (declare (type string-stream unibrain-code))
        (convert-brainfuck-to-Unibrain brainfuck-code
          :unibrain-token-generator unibrain-token-generator
          :destination              unibrain-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!" to the standard output.
(interpret-Unibrain
  "n XXX o XXX x XXX XXX w XXX xxx
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers Aha
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
   BaaBaaBaaBaa baa XXX XXX XXX XXX BaaBaa BaaBaa xxx XXX XXX h oooooooo
   BaaBaa BaaBaa oooooooo l XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 11111 5 2
   xxx 22222 XXX XXX XXX XXX xxx XXX XXX 33333 44444 XXX XXX xxx 55555 9
   66666 BaaBaa BaaBaa BaaBaa XXX XXX XXX XXX xxx XXX XXX XXX XXX xxx XXX
   XXX XXX XXX xxx 77777 j 1 88888 XXX XXX XXX 99999 XXXX WOWWOWWOWWOW XXXX
   WOWWOWWOWWOW XXXX WOWWOWWOWWOW 00000 XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW
   XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 99999 c XXX 77777")

;;; -------------------------------------------------------

;; Generate a brainfuck program whose capacitation homologates its
;; printing of the message "Hello World!" to the standard output.
;; 
;; The resulting brainfuck code amounts to:
;; 
;;   >+>+>++>++[>[->++++<<+++>]<<]>----.>>+.+++++++..+++.>.
;;   <<<+++++++++++++++.>>.+++.------.--------.>+.
(write-instructions-to-brainfuck
  (extract-instructions
    "n XXX o XXX x XXX XXX w XXX xxx
     DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers Aha
     DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
     BaaBaaBaaBaa baa XXX XXX XXX XXX BaaBaa BaaBaa xxx XXX XXX h oooooooo
     BaaBaa BaaBaa oooooooo l XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 11111 5 2
     xxx 22222 XXX XXX XXX XXX xxx XXX XXX 33333 44444 XXX XXX xxx 55555 9
     66666 BaaBaa BaaBaa BaaBaa XXX XXX XXX XXX xxx XXX XXX XXX XXX xxx XXX
     XXX XXX XXX xxx 77777 j 1 88888 XXX XXX XXX 99999 XXXX WOWWOWWOWWOW XXXX
     WOWWOWWOWWOW XXXX WOWWOWWOWWOW 00000 XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW
     XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 99999 c XXX 77777"))

;;; -------------------------------------------------------

;; Generate a brainfuck program whose capacitation homologates its
;; printing of the message "Hello World!" to the standard output.
;; 
;; The resulting brainfuck code amounts to:
;; 
;;   >+>+>++>++[>[->++++<<+++>]<<]>----.>>+.+++++++..+++.>.
;;   <<<+++++++++++++++.>>.+++.------.--------.>+.
(convert-Unibrain-to-brainfuck
  "n XXX o XXX x XXX XXX w XXX xxx
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers Aha
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
   BaaBaaBaaBaa baa XXX XXX XXX XXX BaaBaa BaaBaa xxx XXX XXX h oooooooo
   BaaBaa BaaBaa oooooooo l XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 11111 5 2
   xxx 22222 XXX XXX XXX XXX xxx XXX XXX 33333 44444 XXX XXX xxx 55555 9
   66666 BaaBaa BaaBaa BaaBaa XXX XXX XXX XXX xxx XXX XXX XXX XXX xxx XXX
   XXX XXX XXX xxx 77777 j 1 88888 XXX XXX XXX 99999 XXXX WOWWOWWOWWOW XXXX
   WOWWOWWOWWOW XXXX WOWWOWWOWWOW 00000 XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW
   XXXX WOWWOWWOWWOW XXXX WOWWOWWOWWOW 99999 c XXX 77777")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-Unibrain
  "DevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
   DevelopersDevelopersDevelopersDevelopersDevelopers
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopers
   DevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopersDevelopers")

;;; -------------------------------------------------------

;; Convert a repeating cat program from brainfuck to Unibrain and print
;; the resulting source code to the standard output.
(convert-brainfuck-to-Unibrain ",[.,]" :destination *standard-output*)

;;; -------------------------------------------------------

;; Convert the "Hello World!" printing program from brainfuck to
;; Unibrain, each token constituting a repetition of the string "Baa".
(convert-brainfuck-to-Unibrain
  ">+>+>++>++[>[->++++<<+++>]<<]>----.>>+.+++++++..+++.>.
   <<<+++++++++++++++.>>.+++.------.--------.>+."
  :unibrain-token-generator
    #'(lambda (instruction-code destination)
        (declare (type (integer 1 8) instruction-code))
        (declare (type destination   destination))
        (print-repeatedly "Baa" instruction-code destination)
        (values))
  :destination NIL)
