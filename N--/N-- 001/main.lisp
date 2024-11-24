;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "N--", invented by the Esolang user "Moon" and presented on
;; May 10th, 2016, the nature of its investment accounting for the
;; kenspeckle indicium appertaining to the manipulation of bit
;; sequences measuring no bournes in their mickleness, their castaldy
;; being assigned to variables, upon thilk a quintuple of
;; single-character instructions operate.
;; 
;; 
;; Concept
;; =======
;; The N-- programming language partakes of a very simplistic nature,
;; its operative components already exhausted by a quintuple membership,
;; operating on a contingently infinite registry of variables, each
;; lending a salvatory to a bit sequence of arbitrary extent, the
;; placeholders nevened by an identifier compact of two minuscular Latin
;; letters.
;; 
;; == N-- OPERATES ON BIT SEQUENCES ==
;; An N-- program operates on ordered bit sequences of an arbitrary
;; dispansion as the sole object of deliberation, permitting modulations
;; in order to submit to a particular telos.
;; 
;; Vouchsafed the paravaunt potency in its effect on a these binary
;; collections, the logical NAND operation's appropriates the language's
;; cynosure.
;; 
;; == NAND: TRUE IF ALSO FALSE ==
;; The NAND operation's diorism founds upon an activation in any case
;; where at least one input assumes a zero (0) state; as a consectary,
;; merely the case of two one-valued (1) ingoing values provokes a
;; zero (0) result. A sequence of NAND-combined Boolean truth values
;; thus produces true if --- and only if --- one or more inputs are
;; false.
;; 
;; The following shall ostend the NAND gate's truth table for two
;; inputs, "A" and "B", resulting in a conjoined "Output":
;; 
;;   ---------------
;;   A | B | Output
;;   --+---+--------
;;   0 | 0 | 1
;;   ...............
;;   1 | 0 | 1
;;   ...............
;;   0 | 1 | 1
;;   ...............
;;   1 | 1 | 0
;;   ---------------
;; 
;; == VARIABLES: THE DATA STORAGE ==
;; The aefauld components of retentive potential, variables lend a
;; commorancy to ordered bit sequences, each a ordained to a single
;; instance's purview; this castaldy's circumference covers the binary
;; data's perquisition and modification, contingently realized with a
;; concomitant higher-level interpretation of the bits, for instance,
;; as an unsigned integer datum.
;; 
;; A variable, if not granted to possede a deviating paraphernalia,
;; always inchoates with a single zero-valued bit.
;; 
;; Any variable's agnomination embraces an exact two-character
;; componency, deviating anenst the concrete syntax in the dichotomy
;; betwixt the innate and bespoke species.
;; 
;; == VARIABLES BIFURCATE INTO SPECIAL AND CUSTOM ENTITIES ==
;; The variables bifurcate in the diorism of authochthonous specimens,
;; siccan enjoy an automatical attendance and fixated purposes;
;; maintaining the compernage of placeholders whose creation registers
;; the developer as its provenance.
;; 
;; A quadruple cardinality enumerates the former, fixed class, always
;; demarcated in their syntactical conformation by a dollar sign, "$",
;; at the sole minuscular Latin letter's prefixion.
;; 
;; Custom variables, on the other hand, are imposed with urgent
;; stringency in their nevening by two Latin minuscules.
;; 
;; == SPECIAL VARIABLES ==
;; N--'s diorism of succedaneous entities lays its amplectation around
;; four such specimens whose devotion is airted towards a particular,
;; fixed purpose.
;; 
;; This select membership's delineation shall be the following
;; tabulation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Special variable | Agency
;;   -----------------+------------------------------------------------
;;   $s               | Contains the tally of characters constituting
;;                    | the currently executed program's source code.
;;                    |------------------------------------------------
;;                    | Modifications adhibited to this variable
;;                    | instigate an undefined behavior.
;;                    |------------------------------------------------
;;                    | At the program's inchoation, this variable
;;                    | comprehends the program's character count
;;                    | encoded in binary form.
;;   ..................................................................
;;   $p               | Represents the program counter (PC) or
;;                    | instruction pointer (IP), which contains at any
;;                    | instant during a program's execution the
;;                    | currently processed character in the source
;;                    | code.
;;                    |------------------------------------------------
;;                    | Please heed the emphasis on the character-based
;;                    | nature of this cursor, as counterdistinguished
;;                    | from an instruction-based one.
;;                    |------------------------------------------------
;;                    | The position enumeration commences with the
;;                    | index zero (0).
;;                    |------------------------------------------------
;;                    | The modulation of this variable capacitates the
;;                    | program control flow's redirection by jumping
;;                    | to arbitrary characters in the code.
;;                    |------------------------------------------------
;;                    | At the program's inchoation, this variable
;;                    | amplects a single zero (0) bit.
;;   ..................................................................
;;   $i               | Represents the input conduit.
;;                    |------------------------------------------------
;;                    | If read from, thilk queries the standard input
;;                    | for a character and stores its ANSI
;;                    | representation's bits in this variable.
;;                    |------------------------------------------------
;;                    | If written to, complies with a generic
;;                    | variable's receptive behavior by admission of
;;                    | its new content.
;;                    |------------------------------------------------
;;                    | At the program's inchoation, this variable
;;                    | amplects eight (8) zero-valued bits.
;;   ..................................................................
;;   $o               | Represents the output conduit.
;;                    |------------------------------------------------
;;                    | If read from, complies with a generic
;;                    | variable's responsive behavior by presenting
;;                    | its current state.
;;                    |------------------------------------------------
;;                    | If written to, thilk prints the character whose
;;                    | ANSI code equals the new state to the standard
;;                    | output.
;;                    |------------------------------------------------
;;                    | At the program's inchoation, this variable
;;                    | amplects eight (8) zero-valued bits.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; The N-- programming language wists of variables as the aefauld
;; structures of its memory architecture's scaffolding.
;; 
;; These identifiers, a quadruple among as special-purpose specimens
;; exempted, always bear the designment of a twain of Latin minuscules,
;; responding with a bit sequence of potentially infinite dispansion.
;; The account of the variables' tally wists only of a confinement by
;; the upper limit naturally imposed on the combination of the
;; admissible character jumelles.
;; 
;; 
;; Data Types
;; ==========
;; The paravaunt species of deliberation in an N-- program is begotten
;; by the bit sequence, an ordered list of one or more bits, its
;; capacity not impounded by any theoretical stipulation.
;; 
;; Ensuing from the various aspects of the very potent bit sequence
;; notion, certain operations and variables apply themselves to
;; particular interpretations thereof; for instance, the "$p"
;; placeholder maintains the current instruction pointer (IP) or
;; program counter (PC) index in the common guise of a binary stream
;; for most perquisition and manipulation purposes, yet exhibits a
;; twifaced haecceity as an unsigned non-negative integer number when
;; administered the faculty of its indexical agency in selecting the
;; contemporaneously active instruction among the program statements.
;; 
;; 
;; Syntax
;; ======
;; Exercising a syntactical conspectuity upon an N-- program, thilk
;; enlists an ordered sequence of zero or more instructions, everichon
;; membership an identifier succeeded by one or more arguments, the
;; entirety being independent of whitespaces' discerning mediation.
;; 
;; == INSTRUCTIONS ==
;; An instruction's diorism enumerates a twifold componency, the
;; operation's single-symbol identification serves as a indicium and
;; parasceve to one or more operand's patration. Sepiments install an
;; object of the developer's deliberation, rather than a requisitum,
;; and may only amplect the whitespace entities, which please consult
;; alow.
;; 
;; == ARGUMENTS ==
;; An instruction's arguments are restricted in a variable name's
;; assumption; thilk entity may either constitute a primordial specimen,
;; a twissel of a parasceuastic dollar sign, "$", and an ensuing
;; lowercase letter's solitude, or a bespoke succedaneum, as is levied
;; against the stipulation of a Latin minuscules' twain.
;; 
;; == WHITESPACES ==
;; The participation of whitespaces betwixt instruction identifiers
;; and arguments constitutes a subject in reception of a paregal mete
;; in tolerance as significance's destitution.
;; 
;; == COMMENTS ==
;; The contemporaneous language standard, at this document's
;; parturition, does not incorporate the capacity for descants in any
;; form.
;; 
;; == GRAMMAR ==
;; The N-- language's donat shall become a more formal treatise's
;; recipient in the adhibition of an Extended Backus-Naur Form (EBNF)
;; diction:
;; 
;;   program           := { command } ;
;;   command           := nandCommand
;;                     |  leftShiftCommand
;;                     |  rightShiftCommand
;;                     |  appendCommand
;;                     |  removeCommand
;;                     |  nullifyCommand
;;                     ;
;;   
;;   nandCommand       := "N" , variable , variable , variable ;
;;   leftShiftCommand  := "L" , variable ;
;;   rightShiftCommand := "R" , variable ;
;;   appendCommand     := "v" , variable ;
;;   removeCommand     := "^" , variable ;
;;   nullifyCommand    := "!" , variable ;
;;   
;;   variable          := customVariable | specialVariable ;
;;   customVariable    := minuscule , minuscule ;
;;   specialVariable   := "$i" | "$o" | "$p" | "$s" ;
;;   
;;   minuscule         := "a" | ... | "z" ;
;; 
;; 
;; Instructions
;; ============
;; Tallying a sextuple membership, N-- operative faculties exclusively
;; to logical manipulations, whence, in the case of its appliance to
;; the special variables, in particular the instruction pointer (IP)
;; salvatory "$p", accomplish the escape from the logical realm into
;; the control flow governance.
;; 
;; == OVERVIEW ==
;; An apercu entalented with compendiousness rather than exhaustion in
;; its presentation shall communicate the requisite mete of nortelry
;; with the language's operative competences.
;; 
;; Please heed that succedaneous sections are limned by a catena of
;; underlying asterisks ("*"), expecting their supersession by actual
;; N-- code in the ultimate code.
;; 
;;   ------------------------------------------------------------------
;;   Command          | Effect
;;   -----------------+------------------------------------------------
;;   N var1 var2 var3 | NAND-combines the first bit stored in the
;;     **** **** **** | variable amenable to the name {var1} with the
;;                    | first bit of the variable {var2} and sets the
;;                    | first bit of the variable {var3} to the result.
;;                    |------------------------------------------------
;;                    | The operands {var1} and {var2} remain unaltered
;;                    | by this instruction, whereas {var3}'s content
;;                    | will be destructively overwritten.
;;   ..................................................................
;;   L var            | Performs a left-shift of the bits stored in the
;;     ***            | variable amenable to the name {var} by a single
;;                    | position, modifying its content in the process.
;;   ..................................................................
;;   R var            | Performs a right-shift of the bits stored in
;;     ***            | the variable amenable to the name {var} by a
;;                    | single position, modifying its content in the
;;                    | process.
;;   ..................................................................
;;   ^ var            | Removes the first bit, that is, thilk occupying
;;     ***            | the least significant position, in the variable
;;                    | amenable to the name {var}, modifying its
;;                    | content in the process.
;;                    |------------------------------------------------
;;                    | If the variable comprehends a single bit, no
;;                    | causatum applies.
;;   ..................................................................
;;   v var            | Appends a zero-valued bit (0) to the rear of
;;     ***            | the variable amenable to the name {var},
;;                    | modifying its content in the process.
;;   ..................................................................
;;   ! var            | "Nullifies" the variable amenable to the name
;;     ***            | {var} by setting its content to an aefauld zero
;;                    | (0) bit.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The original specification, in its circumference encroaching a
;; metewand's humble portion, naturally retains a proportional crepuscle
;; in its lucidity; a corollary thereof, the following sections shall
;; attend to these ambiguous aspects.
;; 
;; == ARE WHITESPACES VOUCHSAFED HOMOLOGATION? ==
;; The curtailed nature applying to the protolog's elucidations and
;; forbisens registers a lacuna concerning the tolerance invested in
;; adscititious content, in particular whitespaces.
;; 
;; It has been adjudged, as an illation elicited from tye syntactical
;; presentations, to impute an interdiction of any content deviating
;; from the instruction and operand identifiers. This imposition is
;; levied, of course, against whitespaces, a diorism whose in whose
;; circumference reside spaces, horizontal tabs, and newline entities.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been realized in the programming
;; language Common Lisp, an N-- program's constituting a simplistic
;; endeavour, inside of whom the executing entity operates directly on
;; the source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-10-23
;; 
;; Sources:
;;   [esolang2023N--]
;;   The Esolang contributors, "N--", May 8th, 2023
;;   URL: "https://esolangs.org/wiki/N--"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype variable-name ()
  "The ``variable-name'' type defines an identifier entalented with
   concinnity as a variable name as a string of two characters'
   dispansion."
  '(string 2))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose conformation
   enumerates zero or more entries, everichon among thsse colocates a
   key of the TYPE-TYPE to a value of the VALUE-TYPE, for both holds the
   comprehensive ``T'' as the default."
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

(deftype list-of (&optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list tallying a cardinality equal to
   the SIZE in elements, each member among these subscribes to the
   ELEMENT-TYPE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (eq size '*)
                (= (length (the list candidate)) size))
            (or (eq element-type '*)
                (every
                  #'(lambda (current-element)
                      (declare (type T current-element))
                      (typep current-element element-type))
                  (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype variable-name ()
  "The ``variable-name'' type defines a variable identifier as a simple
   string, its mete fixed as two characters."
  '(simple-string 2))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which amplects, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral number greater
   than or equal to zero (0), but liberated from any upper extremum's
   coercion, its elements, as a consectary, accounting for occupants of
   the integer range [0, +infinity]."
  '(integer 0 *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' interface accoutres a common foundry for all
   classes whose purpose dedicates thilk to the representation of N--
   instructions, their ensconcement that of the requisite operands'
   enumeration.")

;;; -------------------------------------------------------

(defstruct (NAND-Instruction
  (:include     Instruction)
  (:constructor make-nand-instruction
    (first-input second-input output)))
  "The ``NAND-Instruction'' class serves in the encapsulation of the N--
   instruction \"N\"."
  (first-input  (error "Missing first input.")
                :type      variable-name
                :read-only T)
  (second-input (error "Missing second input.")
                :type      variable-name
                :read-only T)
  (output       (error "Missing output.")
                :type      variable-name
                :read-only T))

;;; -------------------------------------------------------

(defstruct (Left-Shift-Instruction
  (:include     Instruction)
  (:constructor make-left-shift-instruction (operand)))
  "The ``Left-Shift-Instruction'' class serves in the encapsulation of
   the N-- instruction \"L\"."
  (operand (error "Missing operand.")
           :type      variable-name
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Right-Shift-Instruction
  (:include     Instruction)
  (:constructor make-right-shift-instruction (operand)))
  "The ``Right-Shift-Instruction'' serves in the encapsulation of the
   N-- instruction \"R\"."
  (operand (error "Missing operand.")
           :type      variable-name
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Remove-Front-Instruction
  (:include     Instruction)
  (:constructor make-remove-front-instruction (operand)))
  "The ``Remove-Front-Instruction'' class serves in the encapsulation
   of the N-- instruction \"^\"."
  (operand (error "Missing operand.")
           :type      variable-name
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Append-Instruction
  (:include     Instruction)
  (:constructor make-append-instruction (operand)))
  "The ``Append-Instruction'' class serves in the encapsulation of the
   N-- instruction \"v\"."
  (operand (error "Missing operand.")
           :type      variable-name
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Nullify-Instruction
  (:include     Instruction)
  (:constructor make-nullify-instruction (operand)))
  "The ``Nullify-Instruction'' class serves in the encapsulation of the
   N-- instruction \"!\"."
  (operand (error "Missing operand.")
           :type      variable-name
           :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of variable-name 4) +SPECIAL-VARIABLE-NAMES+))
(declaim (type (integer 2 2)             +VARIABLE-NAME-LENGTH+))

;;; -------------------------------------------------------

(defparameter +SPECIAL-VARIABLE-NAMES+
  '("$i" "$o" "$p" "$s")
  "Lists the variable identifiers reserved for special purposes.")

(defparameter +VARIABLE-NAME-LENGTH+ 2
  "Defines the valid tally of characters partaking in a variable name.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\"
   expression, constructing from its provenance a veridical Boolean
   equivalency, and returns for a non-``NIL'' input a ``boolean'' value
   of ``T'', otherwise, for a ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minuscule-p (candidate)
  "Determines whether the CANDIDATE represents a minuscular Latin
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (alpha-char-p candidate)
        (lower-case-p candidate)))))

;;; -------------------------------------------------------

(defun character-twain-p (candidate)
  "Determines whether the CANDIDATE represents a string composed of a
   character twissel, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (= (length candidate) 2))))

;;; ------------------------------------------------------

(defun composed-of-minuscules-only-p (candidate)
  "Determines whether the CANDIDATE constitutes a string composed of
   minuscular Latin letters only, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (every #'minuscule-p candidate))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace entity,
   which in its diorism's perimeter amplects the space, horizontal tab,
   and newline specimens, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate '(#\Linefeed #\Newline #\Return #\Space #\Tab)
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Variable name validation operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun special-variable-name-p (candidate)
  "Determines whether the CANDIDATE subsumes into the particular species
   of special variable designators reserved by the N-- programming
   language, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate +SPECIAL-VARIABLE-NAMES+ :test #'string=))))

;;; ------------------------------------------------------

(defun variable-name-p (candidate)
  "Determines whether the CANDIDATE complies with the stipulations
   levied against a variable identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (and (character-twain-p                 candidate)
         (or (composed-of-minuscules-only-p candidate)
             (special-variable-name-p       candidate)))))

;;; ------------------------------------------------------

(defun validate-variable-name (candidate)
  "Determines whether the CANDIDATE complies with the stipulations
   levied against a variable identifier, returning on confirmation the
   subject of this docimasy in its ipsissima verba constitution;
   otherwise signals an error of an unspecified type."
  (declare (type string candidate))
  (the variable-name
    (or (and (variable-name-p candidate)
             candidate)
        (error "The identifier \"~a\" does not represent a valid ~
                variable name."
          candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the location into
   the SOURCE immediately succeeding the skipped parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-string-of-length (source start number-of-characters)
  "Proceeding from the START position into the SOURCE, reads a sequence
   of at most NUMBER-OF-CHARACTERS' dispansion, and returns two values:
     (1) A string comprehending the characters thus gathered.
     (2) The position into the SOURCE immediately succeeding the
         consumed parcel."
  (let ((end-position (min (+ start number-of-characters)
                           (length source))))
    (declare (type fixnum end-position))
    (the (values string fixnum)
      (values
        (subseq source start end-position)
        end-position))))

;;; -------------------------------------------------------

(defun read-variable-name (source start)
  "Proceeding from the START position into the SOURCE, reads a variable
   identifier and returns two values:
     (1) The variable name as a string of two minuscular characters'
         composition.
     (2) The position into the SOURCE immediately succeeding the
         identified variable name."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (identifier new-position)
      (read-string-of-length source
        (skip-whitespaces source start)
        +VARIABLE-NAME-LENGTH+)
    (declare (type string identifier))
    (declare (type fixnum new-position))
    (the (values variable-name fixnum)
      (values
        (validate-variable-name identifier)
        new-position))))

;;; -------------------------------------------------------

(defun expect-character (source position expected-token)
  "Determines whether the character at the specified POSITION into the
   SOURCE represents the EXPECTED-TOKEN, returning on confirmation the
   location into the SOURCE immediately succeeding the POSITION;
   otherwise signals an error of an unspecified type."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-token))
  (the fixnum
    (cond
      ((not (array-in-bounds-p source position))
        (error "Expected the character \"~c\" at position ~d, ~
                but found the source exhausted."
          expected-token position))
      ((char= (char source position) expected-token)
        (1+ position))
      (T
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-token position (char source position))))))

;;; -------------------------------------------------------

(defun read-nand-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"N\"
   instruction, and returns two values:
     (1) A ``NAND-Instruction'' representation of the consumed \"N\"
         operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"N\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((new-position     start)
        (input-variable-1 NIL)
        (input-variable-2 NIL)
        (output-variable  NIL))
    (declare (type fixnum                  new-position))
    (declare (type (or null variable-name) input-variable-1))
    (declare (type (or null variable-name) input-variable-2))
    (declare (type (or null variable-name) output-variable))
    (setf new-position (expect-character source new-position #\N))
    (multiple-value-setq (input-variable-1 new-position)
      (read-variable-name source new-position))
    (multiple-value-setq (input-variable-2 new-position)
      (read-variable-name source new-position))
    (multiple-value-setq (output-variable new-position)
      (read-variable-name source new-position))
    (the (values NAND-Instruction fixnum)
      (values
        (make-nand-instruction
          input-variable-1
          input-variable-2
          output-variable)
        new-position))))

;;; -------------------------------------------------------

(defun read-left-shift-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"L\"
   instruction, and returns two values:
     (1) A ``Left-Shift-Instruction'' representation of the consumed
         \"L\" operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"L\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (operand new-position)
      (read-variable-name source
        (expect-character source start #\L))
    (declare (type variable-name operand))
    (declare (type fixnum        new-position))
    (the (values Left-Shift-Instruction fixnum)
      (values
        (make-left-shift-instruction operand)
        new-position))))

;;; -------------------------------------------------------

(defun read-right-shift-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"R\"
   instruction, and returns two values:
     (1) A ``Right-Shift-Instruction'' representation of the consumed
         \"R\" operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"R\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (operand new-position)
      (read-variable-name source
        (expect-character source start #\L))
    (declare (type variable-name operand))
    (declare (type fixnum        new-position))
    (the (values Right-Shift-Instruction fixnum)
      (values
        (make-right-shift-instruction operand)
        new-position))))

;;; -------------------------------------------------------

(defun read-remove-front-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"^\"
   instruction, and returns two values:
     (1) A ``Remove-Front-Instruction'' representation of the consumed
         \"^\" operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"^\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (operand new-position)
      (read-variable-name source
        (expect-character source start #\^))
    (declare (type variable-name operand))
    (declare (type fixnum        new-position))
    (the (values Remove-Front-Instruction fixnum)
      (values
        (make-remove-front-instruction operand)
        new-position))))

;;; -------------------------------------------------------

(defun read-append-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"v\"
   instruction, and returns two values:
     (1) An ``Append-Instruction'' representation of the consumed \"v\"
         operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"v\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (operand new-position)
      (read-variable-name source
        (expect-character source start #\v))
    (declare (type variable-name operand))
    (declare (type fixnum        new-position))
    (the (values Append-Instruction fixnum)
      (values
        (make-append-instruction operand)
        new-position))))

;;; -------------------------------------------------------

(defun read-nullify-instruction (source start)
  "Proceeding from START position into the SOURCE, consumes an N-- \"!\"
   instruction, and returns two values:
     (1) A ``Nullify-Instruction'' representation of the consumed \"!\"
         operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the \"!\" operation."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (operand new-position)
      (read-variable-name source
        (expect-character source start #\!))
    (declare (type variable-name operand))
    (declare (type fixnum        new-position))
    (the (values Nullify-Instruction fixnum)
      (values
        (make-nullify-instruction operand)
        new-position))))

;;; -------------------------------------------------------

(defun read-instruction (source start)
  "Proceeding from the START position into the SOURCE, reads an N--
   instruction and returns two values:
     (1) A conable ``Instruction'' representation of the recognized N--
         operation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the consumed operation."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Instruction fixnum)
    (if (array-in-bounds-p source start)
      (let ((current-character (char source start)))
        (declare (type character current-character))
        (case current-character
          (#\N (read-nand-instruction         source start))
          (#\L (read-left-shift-instruction   source start))
          (#\R (read-right-shift-instruction  source start))
          (#\^ (read-remove-front-instruction source start))
          (#\v (read-append-instruction       source start))
          (#\! (read-nullify-instruction      source start))
          (otherwise
            (error "Invalid character \"~c\" at position ~d."
              current-character start))))
      (error "Expected an instruction to commence at position ~d, ~
              but encountered the source exhausted."
        start))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Boolean operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nand-combine-bit (first-bit second-bit)
  "Returns the NAND-combination of the FIRST-BIT and the SECOND-BIT."
  (declare (type bit first-bit))
  (declare (type bit second-bit))
  (the bit
    (if (= first-bit second-bit 1)
      0
      1)))

;;; -------------------------------------------------------

(defun measure-minimum-bit-count (bits)
  "Returns the minimum tally of bits imposing a requisitum for the
   BITS' representation, this being no less than one (1)."
  (declare (type (unsigned-byte *) bits))
  (the (integer 1 *)
    (max 1
      (integer-length bits))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Bit-Set".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Bit-Set
  (:constructor make-zero-bit-set
    (&optional (size 1)
     &aux      (bits 0)))
  (:constructor make-bit-set-with-value
    (bits
     &aux (size (measure-minimum-bit-count bits)))))
  "The ``Bit-Set'' applies itself to the castaldy of a sequence of one
   or more bits, its reification concealed by an ensconcing nature.
   ---
   A ``Bit-Set'''s content obeys a construction proceeding from the most
   significant to the least significant position; in corollary, the
   \"first\" bit is always empight at the highest-valued location (MBS),
   the \"desinent\" one at the lowest (LSB).
   ---
   Given a ``Bit-Set'' composed of N bits,
     b[N-1], b[N-2], ..., b[1], b[0],
   where
     N >= 0,
   the first bit is designated by b[N-1], the last by b[0]."
  (bits 0 :type (unsigned-byte *)   :read-only NIL)
  (size 1 :type (integer       1 *) :read-only NIL))

;;; -------------------------------------------------------

(defun singleton-bit-set-p (bits)
  "Determines whether the sequence of BITS represents a singleton bit
   set, that is, one composed of an aefauld bit only, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Bit-Set bits))
  (the boolean
    (get-boolean-value-of
      (= (bit-set-size bits)
         1))))

;;; -------------------------------------------------------

(defun set-bit-set-to (bits new-value)
  "Stores the integer-encoded bits of the NEW-VALUE in the set of BITS,
   updates the latter's size to the minimum representation of the
   former, and returns no value."
  (declare (type Bit-Set              bits))
  (declare (type non-negative-integer new-value))
  (psetf (bit-set-bits bits) new-value
         (bit-set-size bits) (max 1 (integer-length new-value)))
  (values))

;;; -------------------------------------------------------

(defun valid-bit-index-p (bits probed-index)
  "Determines whether the PROBED-INDEX constitutes a valid position
   into the sequence of BITS, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Bit-Set              bits))
  (declare (type non-negative-integer probed-index))
  (the boolean
    (get-boolean-value-of
      (< probed-index
         (bit-set-size bits)))))

;;; -------------------------------------------------------

(defun bit-at (bits index)
  "Returns the bit stored in the sequence of BITS at the zero-based
   INDEX."
  (declare (type Bit-Set bits))
  (the bit
    (ldb (byte 1 index)
      (bit-set-bits bits))))

;;; -------------------------------------------------------

(defun (setf bit-at) (new-value bits index)
  "Sets the bit in the sequence of BITS at the zero-based INDEX to the
   NEW-VALUE and returns no value."
  (declare (type Bit-Set bits))
  (when (valid-bit-index-p bits index)
    (setf (ldb (byte 1 index) (bit-set-bits bits))
          new-value))
  (values))

;;; -------------------------------------------------------

(defun first-bit-position (bits)
  "Returns the position of the first bit in the sequence of BITS."
  (declare (type Bit-Set bits))
  (declare (ignore       bits))
  (the non-negative-integer 0))

;;; -------------------------------------------------------

(defun first-bit (bits)
  "Returns the value of the first bit in the sequence of BITS."
  (declare (type Bit-Set bits))
  (the bit
    (bit-at bits
      (first-bit-position bits))))

;;; -------------------------------------------------------

(defun (setf first-bit) (new-value bits)
  "Sets the bit at the first position in the sequence of BITS to the
   NEW-VALUE and returns no value."
  (declare (type Bit-Set bits))
  (setf (bit-at bits (first-bit-position bits)) new-value)
  (values))

;;; -------------------------------------------------------

(defun nand-combine-first-bits (left-source right-source destination)
  "Sets the first bit of DESTINATION bit sets's content to the
   NAND-combination of the LEFT-SOURCE's and RIGHT-SOURCE's first bit
   and returns the modified DESTINATION.
   ---
   Neither the LEFT-SOURCE nor the RIGHT-SOURCE are subjected to
   modulations if not resolving to the same object as the DESTINATION."
  (declare (type Bit-Set left-source))
  (declare (type Bit-Set right-source))
  (declare (type Bit-Set destination))
  (setf (first-bit destination)
    (nand-combine-bit
      (first-bit left-source)
      (first-bit right-source)))
  (the Bit-Set destination))

;;; -------------------------------------------------------

(defun shift-bits-left (bits)
  "Performs a bit shift in the left direction on the sequence of BITS
   by a single step and returns no value."
  (declare (type Bit-Set bits))
  (setf (bit-set-bits bits)
    (ash (bit-set-bits bits) 1))
  (incf (bit-set-size bits))
  (values))

;;; -------------------------------------------------------

(defun shift-bits-right (bits)
  "Performs a bit shift in the right direction on the sequence of BITS
   by a single step and returns no value."
  (declare (type Bit-Set bits))
  (setf (bit-set-bits bits)
    (ash (bit-set-bits bits) -1))
  (unless (singleton-bit-set-p bits)
    (decf (bit-set-size bits)))
  (values))

;;; -------------------------------------------------------

(defun append-bit (bits)
  "Appends a zero-valued bit (0) to the tail of the sequence of BITS and
   returns no value."
  (declare (type Bit-Set bits))
  (setf (bit-set-bits bits)
    (ash (bit-set-bits bits) 1))
  (incf (bit-set-size bits))
  (values))

;;; -------------------------------------------------------

(defun remove-first-bit (bits)
  "Removes from the sequence of BITS the first bit and returns no
   value."
  (declare (type Bit-Set bits))
  (cond
    ((singleton-bit-set-p bits)
      (setf (bit-set-bits bits) 0))
    (T
      (decf (bit-set-size bits))
      (setf (bit-set-bits bits)
        (ldb (byte (bit-set-size bits) 1)
          (bit-set-bits bits)))))
  (values))

;;; -------------------------------------------------------

(defun nullify-bit-set (bits)
  "Resets the state of the sequence of BITS to a singleton zero (0) bit
   and returns no value."
  (declare (type Bit-Set bits))
  (psetf
    (bit-set-bits bits) 0
    (bit-set-size bits) 1)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((bits Bit-Set) (stream T))
  (declare (type Bit-Set     bits))
  (declare (type destination stream))
  (format stream "Bit-Set with ~d bit~:p: ~v,'0b"
    (bit-set-size bits)
    (bit-set-size bits)
    (bit-set-bits bits)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of reserved variable names.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type variable-name +INPUT-VARIABLE-NAME+))
(declaim (type variable-name +OUTPUT-VARIABLE-NAME+))
(declaim (type variable-name +IP-VARIABLE-NAME+))
(declaim (type variable-name +SOURCE-LENGTH-VARIABLE-NAME+))

;;; -------------------------------------------------------

(defparameter +INPUT-VARIABLE-NAME+ "$i"
  "The variable identifier reserved for the reception of input.")

(defparameter +OUTPUT-VARIABLE-NAME+ "$o"
  "The variable identifier reserved for the commission of output.")

(defparameter +IP-VARIABLE-NAME+ "$p"
  "The variable identifier reserved for the instruction pointer (IP)
   position.")

(defparameter +SOURCE-LENGTH-VARIABLE-NAME+ "$s"
  "The variable identifier reserved for the N-- source code length.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "NVariable".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (NVariable
  (:constructor make-nvariable (name
                                &optional
                                  (value (make-zero-bit-set)))))
  "The ``NVariable'' class establishes a representation of an N--
   variable, the capacity apportioned to whom metes an aefauld bit set."
  (name  (error "Missing name.")  :type string  :read-only T)
  (value (error "Missing value.") :type Bit-Set :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Variable-Set".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Set ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :accessor      variable-set-entries
    :type          (hash-table-of variable-name NVariable)
    :documentation "Maps the special and the defined variable names to
                    connable representations."))
  (:documentation
    "The ``Variable-Set'' class is apportioned the dever of
     accommodating a set of variables' castaldy, the circumference of
     which admits both fixed memberships, appertaining to the subset of
     special specimens, and bespoke inclusions."))

;;; -------------------------------------------------------

(defun insert-default-variable (variables name default-variable)
  "Creates in the set of VARIABLES a representative of a special
   variable, its existency communicated by the NAME and its affiliated
   DEFAULT-VARIABLE instance, and returns no value."
  (declare (type Variable-Set  variables))
  (declare (type variable-name name))
  (declare (type NVariable     default-variable))
  (setf (gethash name (variable-set-entries variables))
        default-variable)
  (values))

;;; -------------------------------------------------------

(defun make-default-variable-set (code-length)
  "Creates and returns the a ``Variable-Set'' for an N-- program whose
   source code is meted with the CODE-LENGTH tally of characters, the
   set being populated with the default membership in the form of
   reserved variables."
  (declare (type fixnum code-length))
  (let ((variables (make-instance 'Variable-Set)))
    (declare (type Variable-Set variables))
    (insert-default-variable variables +INPUT-VARIABLE-NAME+
      (make-nvariable +INPUT-VARIABLE-NAME+
        (make-zero-bit-set 8)))
    (insert-default-variable variables +OUTPUT-VARIABLE-NAME+
      (make-nvariable +OUTPUT-VARIABLE-NAME+
        (make-zero-bit-set 8)))
    (insert-default-variable variables +IP-VARIABLE-NAME+
      (make-nvariable +IP-VARIABLE-NAME+))
    (insert-default-variable variables +SOURCE-LENGTH-VARIABLE-NAME+
      (make-nvariable +SOURCE-LENGTH-VARIABLE-NAME+
        (make-bit-set-with-value code-length)))
    (the Variable-Set variables)))

;;; -------------------------------------------------------

(defun register-variable (variables name)
  "Creates a fresh ``NVariable'' instance, its agnomination derived from
   the NAME and its incipial value chosen by its default, affiliates
   thilk with the NAME in the set of VARIABLES, and returns the thus
   produced variable object."
  (declare (type Variable-Set  variables))
  (declare (type variable-name name))
  (let ((new-variable (make-nvariable name)))
    (declare (type NVariable new-variable))
    (setf (gethash name (variable-set-entries variables)) new-variable)
    (the NVariable new-variable)))

;;; -------------------------------------------------------

(defun ensure-variable-exists (variables name)
  "Ensures the presence of a variable amenable to the NAME in the set of
   VARIABLES, and returns either the already extant or the newly created
   and registered variable's value."
  (declare (type Variable-Set  variables))
  (declare (type variable-name name))
  (multiple-value-bind (value contains-name-p)
      (gethash name
        (variable-set-entries variables))
    (declare (type (or null NVariable) value))
    (declare (type T                   contains-name-p))
    (unless contains-name-p
      (setf value
        (register-variable variables name)))
    (the NVariable value)))

;;; -------------------------------------------------------

(defun look-up-variable (variables name)
  "Returns the value stored in the set of VARIABLES under the NAME."
  (declare (type Variable-Set  variables))
  (declare (type variable-name name))
  (the NVariable
    (ensure-variable-exists variables name)))

;;; -------------------------------------------------------

(defun list-variable-names (variables)
  "Returns a list of the VARIABLES' identifiers in their natural order."
  (declare (type Variable-Set variables))
  (the (list-of variable-name)
    (loop
      for current-name
        of-type variable-name
        being the hash-keys in (variable-set-entries variables)
      collect current-name)))

;;; -------------------------------------------------------

(defun get-sorted-variable-names (variables)
  "Returns a list of the VARIABLES' identifiers sorted in a
   lexicographic fashion."
  (declare (type Variable-Set variables))
  (the (list-of variable-name)
    (sort
      (list-variable-names variables)
      #'string<)))

;;; -------------------------------------------------------

(defmethod print-object ((variables Variable-Set) (stream T))
  (declare (type Variable-Set variables))
  (declare (type destination  stream))
  (format stream "~&Variable-Set:")
  (dolist (variable-name (get-sorted-variable-names variables))
    (declare (type variable-name variable-name))
    (format stream "~&~2t~s => ~a" variable-name
      (look-up-variable variables variable-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type boolean    +DEFAULT-TRACING-BEHAVIOR+))
(declaim (type (real 0 *) +DEFAULT-POST-TRACING-DELAY+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-TRACING-BEHAVIOR+ T
  "The default program state tracing behavior, which confirms its cyclic
   printing.")

(defparameter +DEFAULT-POST-TRACING-DELAY+ 1
  "The default delay to impose after printing the program state, empight
   on one (1) second.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((source
    :initarg       :source
    :initform      (error "Missing N-- source code.")
    :reader        program-source
    :type          string
    :documentation "The piece of N-- source code to interpret.")
   (ip-manipulated-p
    :initform      NIL
    :accessor      program-ip-manipulated-p
    :type          boolean
    :documentation "A flag which determines whether the program has
                    manipulated the instruction pointer (IP) variable
                    \"$ip\" directly, thus actuating a jumping in the
                    code; as a corollary, the IP is not advanced in an
                    implicit mode to the subsequent operation.")
   (variables
    :type          Variable-Set
    :accessor      program-variables
    :documentation "Maintains a collection of variables.")
   (traces-program-state-p
    :initarg       :traces-program-state-p
    :initform      +DEFAULT-TRACING-BEHAVIOR+
    :accessor      traces-program-state-p
    :type          boolean
    :documentation "A flag which determines whether, as a consequence
                    of each instruction execution cycle, the program
                    state, comprehending its variables, shall be printed
                    to the standard output.")
   (post-tracing-delay-in-seconds
    :initarg       :post-tracing-delay-in-seconds
    :initform      +DEFAULT-POST-TRACING-DELAY+
    :accessor      post-tracing-delay-in-seconds
    :type          (real 0 *)
    :documentation "The number of seconds to abide after printing the
                    program state as an execution cycle's conclusion."))
  (:documentation
    "The ``Interpreter'' class serves in the agency of applying to an
     N-- program actual efficacy."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Prepares the variable set for the INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'variables)
    (make-default-variable-set
      (length
        (slot-value interpreter 'source))))
  (values))

;;; -------------------------------------------------------

(defun get-variable (interpreter name)
  "Returns the variable instance associated with the NAME in the
   INTERPRETER's registry, contingently producing such is absent ere
   its representation."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the NVariable
    (look-up-variable
      (program-variables interpreter)
      name)))

;;; -------------------------------------------------------

(defun get-variable-bits (interpreter name)
  "Returns the value of the variable instance associated with the NAME
   in the INTERPRETER's registry, contingently producing such is absent
   ere its representation."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the Bit-Set
    (nvariable-value
      (get-variable interpreter name))))

;;; -------------------------------------------------------

(defun decoded-variable-value (interpreter name)
  "Returns the integer value represented by ``Bit-Set'' under the
   castaldy of the variable instance associated with the NAME
   in the INTERPRETER's registry, contingently producing such is absent
   ere its representation."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (the non-negative-integer
    (bit-set-bits
      (get-variable-bits interpreter name))))

;;; -------------------------------------------------------

(defun (setf decoded-variable-value) (new-bits interpreter name)
  "Stores the NEW-BITS in the variable amenable to the NAME in the
   INTERPRETER's registry and returns no value."
  (declare (type non-negative-integer new-bits))
  (declare (type Interpreter          interpreter))
  (declare (type string               name))
  (set-bit-set-to
    (get-variable-bits interpreter name)
    new-bits)
  (values))

;;; -------------------------------------------------------

(defun program-ip (interpreter)
  "Returns the position of the instruction pointer (IP) in the
   INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the non-negative-integer
    (decoded-variable-value interpreter +IP-VARIABLE-NAME+)))

;;; -------------------------------------------------------

(defun (setf program-ip) (new-position interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   NEW-POSITION and returns no value."
  (declare (type non-negative-integer new-position))
  (declare (type Interpreter          interpreter))
  (setf (decoded-variable-value interpreter +IP-VARIABLE-NAME+)
        new-position)
  (values))

;;; -------------------------------------------------------

(defun program-size (interpreter)
  "Returns the tally of characters comprising the INTERPRETER's
   program."
  (declare (type Interpreter interpreter))
  (the non-negative-integer
    (decoded-variable-value interpreter +SOURCE-LENGTH-VARIABLE-NAME+)))

;;; -------------------------------------------------------

(defun input-conduit-state (interpreter)
  "Returns the current input conduit state stored in the INTERPRETER's
   respective variable."
  (declare (type Interpreter interpreter))
  (the non-negative-integer
    (decoded-variable-value interpreter +INPUT-VARIABLE-NAME+)))

;;; -------------------------------------------------------

(defun (setf input-conduit-state) (new-state interpreter)
  "Stores the integral NEW-STATE in the input variable, as registered
   at the INTERPRETER, and returns no value."
  (declare (type non-negative-integer new-state))
  (declare (type Interpreter          interpreter))
  (setf (decoded-variable-value interpreter +INPUT-VARIABLE-NAME+)
        new-state)
  (values))

;;; -------------------------------------------------------

(defun output-conduit-state (interpreter)
  "Returns the current output conduit state stored in the INTERPRETER's
   respective variable."
  (declare (type Interpreter interpreter))
  (the non-negative-integer
    (decoded-variable-value interpreter +OUTPUT-VARIABLE-NAME+)))

;;; -------------------------------------------------------

(defun seek-next-instruction (interpreter)
  "Proceeding from the current position into the INTERPRETER's source,
   relocates its instruction pointer (IP) to the nearest command token
   and returns no value."
  (declare (type Interpreter interpreter))
  (setf (program-ip interpreter)
    (skip-whitespaces
      (program-source interpreter)
      (program-ip     interpreter)))
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) in response to
   the contingency of a prevenient jump operation or its absence and
   returns no value."
  (declare (type Interpreter interpreter))
  (if (program-ip-manipulated-p interpreter)
    (setf (program-ip-manipulated-p interpreter) NIL)
    (seek-next-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's program has been executed in its
   entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (program-ip   interpreter)
          (program-size interpreter)))))

;;; -------------------------------------------------------

(defun validate-variable-mutability (interpreter name)
  "Determines whether the variable amenable to the NAME, as registered
   at the INTERPRETER, may be modulated in its content, returning on
   confirmation no value; otherwise an error of an unspecified type is
   signaled."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type string      name))
  (when (string= name +SOURCE-LENGTH-VARIABLE-NAME+)
    (error "A modification of the variable \"~a\" is interdicted."
      name))
  (values))

;;; -------------------------------------------------------

(defun handle-potential-input-variable (interpreter variable-name)
  "Determines whether the VARIABLE-NAME designates the input variable,
   as registered at the INTERPRETER, on confirmation querying the
   standard input for a character and storing its ANSI code in the input
   variable, otherwise accompassing no causatum, in any case returning
   no value."
  (declare (type Interpreter interpreter))
  (declare (type string      variable-name))
  (when (string= variable-name +INPUT-VARIABLE-NAME+)
    (format T "~&>> ")
    (finish-output)
    (setf (input-conduit-state interpreter)
      (char-code
        (read-char NIL NIL #\Null)))
    (clear-input))
  (values))

;;; -------------------------------------------------------

(defun handle-potential-output-variable (interpreter name)
  "Determines whether the NAME designates the output variable, as
   registered at the INTERPRETER, on confirmation printing the character
   whose ANSI code concurs with the variable's state to the standard
   output, otherwise accompasses no consequence, in any case returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (when (string= name +OUTPUT-VARIABLE-NAME+)
    (format T "~c"
      (code-char
        (output-conduit-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defun handle-potential-ip-variable (interpreter name)
  "Contingently configures the INTERPRETER's direct instruction pointer
   (IP) manipulation flag in response to an expected modification
   applied to the variable amenable to the NAME and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (when (string= name "$p")
    (setf (program-ip-manipulated-p interpreter) T))
  (values))

;;; -------------------------------------------------------

(defun handle-variable-after-modification (interpreter name)
  "Determines whether the NAME designates a variable entalented with a
   dedicated purpose, on confirmation executing the appropriate
   epiphenomal action, in any case returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (handle-potential-output-variable interpreter name)
  (handle-potential-ip-variable     interpreter name)
  (values))

;;; -------------------------------------------------------

(defun access-variable (interpreter name)
  "Returns the bit sequence associated with the variable NAME in the
   INTEPRETER, contingently preceded by a particular action's execution
   in the case of a special variable."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (handle-potential-input-variable interpreter name)
  (the Bit-Set
    (get-variable-bits interpreter name)))

;;; -------------------------------------------------------

(defun get-next-instruction (interpreter)
  "Returns the instruction commencing at the current position in the
   INTERPRETER's current source."
  (declare (type Interpreter interpreter))
  (flet ((accept-instruction (consumed-instruction new-position)
          "Updates the INTERPRETER's instruction pointer (IP) to the
           NEW-POSITION and returns the CONSUMED-INSTRUCTION."
          (declare (type Instruction consumed-instruction))
          (declare (type fixnum      new-position))
          (setf (program-ip interpreter) new-position)
          (the Instruction consumed-instruction)))
    (the Instruction
      (multiple-value-call #'accept-instruction
        (read-instruction
          (program-source interpreter)
          (program-ip     interpreter))))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction NAND-Instruction))
  (declare (type Interpreter      interpreter))
  (declare (type NAND-Instruction instruction))
  (validate-variable-mutability interpreter
    (nand-instruction-output instruction))
  (nand-combine-first-bits
    (access-variable interpreter
      (nand-instruction-first-input instruction))
    (access-variable interpreter
      (nand-instruction-second-input instruction))
    (get-variable-bits interpreter
      (nand-instruction-output instruction)))
  (handle-variable-after-modification interpreter
    (nand-instruction-output instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Left-Shift-Instruction))
  (declare (type Interpreter            interpreter))
  (declare (type Left-Shift-Instruction instruction))
  (validate-variable-mutability interpreter
    (left-shift-instruction-operand instruction))
  (shift-bits-left
    (access-variable interpreter
      (left-shift-instruction-operand instruction)))
  (handle-variable-after-modification interpreter
    (left-shift-instruction-operand instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Right-Shift-Instruction))
  (declare (type Interpreter             interpreter))
  (declare (type Right-Shift-Instruction instruction))
  (validate-variable-mutability interpreter
    (right-shift-instruction-operand instruction))
  (shift-bits-right
    (access-variable interpreter
      (right-shift-instruction-operand instruction)))
  (handle-variable-after-modification interpreter
    (right-shift-instruction-operand instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Append-Instruction))
  (declare (type Interpreter        interpreter))
  (declare (type Append-Instruction instruction))
  (validate-variable-mutability interpreter
    (append-instruction-operand instruction))
  (append-bit
    (access-variable interpreter
      (append-instruction-operand instruction)))
  (handle-variable-after-modification interpreter
    (append-instruction-operand instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Remove-Front-Instruction))
  (declare (type Interpreter              interpreter))
  (declare (type Remove-Front-Instruction instruction))
  (validate-variable-mutability interpreter
    (remove-front-instruction-operand instruction))
  (remove-first-bit
    (access-variable interpreter
      (remove-front-instruction-operand instruction)))
  (handle-variable-after-modification interpreter
    (remove-front-instruction-operand instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Nullify-Instruction))
  (declare (type Interpreter         interpreter))
  (declare (type Nullify-Instruction instruction))
  (validate-variable-mutability interpreter
    (nullify-instruction-operand instruction))
  (nullify-bit-set
    (access-variable interpreter
      (nullify-instruction-operand instruction)))
  (handle-variable-after-modification interpreter
    (nullify-instruction-operand instruction))
  (values))

;;; -------------------------------------------------------

(defun print-program-state-if-necessary (interpreter)
  "Prints the variables governed by the INTERPRETER's castaldy to the
   standard output and returns no value."
  (when (traces-program-state-p interpreter)
    (format T "~&~%~a" (program-variables interpreter))
    (sleep (post-tracing-delay-in-seconds interpreter)))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Interprets the N-- program consigned to the INTERPRETER's castaldy
   and returns no value.
   ---
   Immediately previent to the program's conclusion, if actually
   eventuating, the entire variable registry will be printed to the
   standard output."
  (declare (type Interpreter interpreter))
  (seek-next-instruction interpreter)
  (loop until (program-completed-p interpreter) do
    (process-instruction interpreter
      (get-next-instruction interpreter))
    (print-program-state-if-necessary interpreter)
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-N--
    (code
     &key
       (traces-program-state-p        +DEFAULT-TRACING-BEHAVIOR+)
       (post-tracing-delay-in-seconds +DEFAULT-POST-TRACING-DELAY+))
  "Interprets the piece of N-- source CODE, optionally commiting to a
   continuous printing of the program state succeeding each completed
   instruction execution by adminiculum of the TRACES-PROGRAM-STATE-P
   flag's activation, with a delay in seconds meted in the
   POST-TRACING-DELAY-IN-SECONDS configuration, and returns no value."
  (declare (type string     code))
  (declare (type boolean    traces-program-state-p))
  (declare (type (real 0 *) post-tracing-delay-in-seconds))
  (execute-program
    (make-instance 'Interpreter
      :source                        code
      :traces-program-state-p        traces-program-state-p
      :post-tracing-delay-in-seconds post-tracing-delay-in-seconds))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a variable "aa" with the value 0 and another, "ab", storing
;; the bit 1.
(interpret-N-- "vaavabNaaaaab")

;;; -------------------------------------------------------

;; Query two characters from the standard input, NAND-combine their
;; first bits, and print thilk to the standard output via the dedicated
;; output variable "$o".
(interpret-N-- "N$i$i$o")

;;; -------------------------------------------------------

;; Perpetual loop.
(interpret-N-- "!$p")

;;; -------------------------------------------------------

;; Perpetual binary appender.
(interpret-N-- "vaaNaaaaaaLaa!$p")
