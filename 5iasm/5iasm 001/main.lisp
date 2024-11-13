;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "5iasm", invented by the Esolang user "Xyzzy" and presented
;; on January 6th, 2023, its proprium's manifestation reified in a
;; quintuple operative membership's coefficiency in the manipulation of
;; 13 signed integer-valued registers by adminiculum of an syntaxis
;; molded following an assembly language's simulacrum.
;; 
;; 
;; Concept
;; =======
;; The 5iasm programming language operates on 13 signed integer-valued
;; registers, a triad among these nuncupated to particular services,
;; the warklume of its efficacy realized in an assembly-like syntaxis,
;; its competences' perimeter amplecting basic arithmetics, conditional
;; instruction skipping, as well as unconditional jumping based upon
;; line numbers or labels.
;; 
;; == 5IASM: AN ASSEMBLY LANGUAGE WITH FIVE INSTRUCTIONS ==
;; The choice of "5iasm" as the language agnomination does not present
;; an aleatory parturition of the tongue: The nevening refers the
;; product as a "[5] [I]nstruction [AS]se[M]bly language", in this
;; amplectation ostending the operative aspect's tally, as well as the
;; guise's entheus.
;; 
;; == THE MEMORY: TEN GENERAL + THREE SPECIAL REGISTERS ==
;; 5iasm's memory incarnates in its diorism a decimal account of
;; general-purpose registers, complemented a treble of specialized
;; specimens. Common to all 13 exponents, the capacity's mete is
;; fixated by a scalar integer each, wisting of no bournes along both
;; axes.
;; 
;; An explicit and meticulous listing of the registers in conjunction
;; with their purposes and behavioral peculiarities shall be limned by
;; the below tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Register | Purpose     | Role and behavior
;;   ---------+-------------+------------------------------------------
;;   A        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   B        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   C        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   D        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   E        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   F        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   G        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   H        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   X        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   Y        | Common      | If read by the "isz" operation, returns
;;            |             | its current state.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, modulates its state
;;            |             | accordingly.
;;   ..................................................................
;;   I        | Input       | Queries the standard input for a single
;;            |             | character and presents its ASCII code.
;;            |             |------------------------------------------
;;            |             | If read by the "isz" operation, queries
;;            |             | the standard input for a single character
;;            |             | and returns its Unicode code point. If
;;            |             | this conduit is exhausted (EOF), responds
;;            |             | with the default value zero (0).
;;            |             | This behavior only transpire if the
;;            |             | input/output register "N" homologates
;;            |             | such.
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, its state can be adjusted as
;;            |             | liberally as any of the general-purpose
;;            |             | register. If queried by the "isz"
;;            |             | command, however, the stored value is
;;            |             | overwritten by the thus obtained
;;            |             | character code, as elucidated aboon. This
;;            |             | new state can subsequently be modulated
;;            |             | by "inc" and "dec" as usual.
;;   ..................................................................
;;   O        | Output      | Prints the character corresponding to its
;;            |             | value construed as an ASCII code.
;;            |             |------------------------------------------
;;            |             | If read by the "isz" operation, returns
;;            |             | the value one (1).
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, prints the character whose
;;            |             | Unicode code point matches the new state.
;;            |             | A printing only transpire if the
;;            |             | input/output register "N" homologates
;;            |             | such.
;;   ..................................................................
;;   N        | I/O control | Enables or disables the input/output
;;            |             | facilities.
;;            |             |------------------------------------------
;;            |             | If read by the "isz" operation, returns
;;            |             | the value one (1).
;;            |             |------------------------------------------
;;            |             | If modified by the "inc" or "dec"
;;            |             | operation, determines whether the number
;;            |             | three (3) constitutes an aliquot part of
;;            |             | of its new content, on confirmation
;;            |             | enabling input and output, otherwise
;;            |             | disabling the same.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; The 5iasm architecture's diorism ensues from a 13 registers
;; componency, each such entity amenable to a single character
;; identifier, its capacity measured by a scalar signed integer.
;; 
;; 
;; Data Types
;; ==========
;; The 5iasm programming language wists of an aefauld data type only:
;; signed integer numbers of any magnitude, their commonrancy the 13
;; registers.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical conspection, a program in the 5iasm language
;; defines an ordered sequence of lines, everichon among these either
;; an instruction's habitancy, or one bereft of epiphenomenal capacity,
;; each such statement compact of an operation and exactly a single
;; operand.
;; 
;; == WHITESPACES ==
;; Spaces, in their diorism exhausted by the traditional space and the
;; horizontal tab, levy an imposition betwixt tokens and a
;; supererogation at any other spatial designation.
;; 
;; Newlines as sepiments define a requisitum for the segregation of any
;; two accolent instructions.
;; 
;; Blank lines constitute the recipients of tolerance in an mete
;; apportioned with the equipollence of their neglect; their presence,
;; as a corollary, does not contribute to the line numbering scheme.
;; 
;; == COMMENTS ==
;; The inclusion of adversaria in a program proceeds by means of a
;; prefatory semicolon ";", its presence a homologation of any spatial
;; choice, the content of which dispands to the ensconcing line's
;; desinence.
;; 
;; == GRAMMAR ==
;; The 5iasm programming language's donat shall enjoy a more detailed
;; ilk of elucidation by the following Extended Backus-Naur Form (ENBF)
;; exposition's dation.
;; 
;; Please heed the omission of spaces' explicit limning, with a
;; concomitant expectation of their presence betwixt the tokens.
;; 
;;   program         := { innerLine } , [ lastLine ] ;
;;   innerLine       := [ statement ] , [ comment ] , newlines ;
;;   lastLine        := [ statement ] , [ comment ] ;
;;   comment         := ";" , { character - newline } ;
;;   statement       := decCommand
;;                   |  incCommand
;;                   |  iszCommand
;;                   |  jmpCommand
;;                   |  stpCommand
;;                   |  labelDefinition
;;                   ;
;;   decCommand      := "dec" , registerName ;
;;   incCommand      := "inc" , registerName ;
;;   iszCommand      := "isz" , registerName ;
;;   jmpCommand      := "jmp" , { lineNumber | labelName } ;
;;   stpCommand      := "stp" ;
;;   labelDefinition := labelName , ":" ;
;;   
;;   registerName    := "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
;;                   |  "I" | "N" | "O"
;;                   ;
;;   labelName       := labelcharacter , { labelCharacter } , ":" ;
;;   labelCharacter  := character - ( ":" | ";" | whitespace ) ;
;;   lineNumber      := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   whitespace      := newline | space ;
;;   newlines        := newline ;
;;   newline         := "\n" ;
;;   space           := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; A quintuple membership exhausts 5iasm's instruction set, its
;; circumference amplecting basic arithmetics, conditional skipping,
;; and an unconditional goto construct based on line numbers or labels;
;; this five elements of the fundament supplemented by a label
;; definition implement that supplies the firmament for subsequent
;; control flow redirections via names.
;; 
;; == OVERVIEW ==
;; The following apercu shall concern itself with a cursory species of
;; gnarity's dation pertaining to the language's operative features,
;; including in its compass besides the five basic instructions the
;; label definition feature.
;; 
;; Please heed the demarcation of succedaneous segments by adminiculum
;; of underlying asterisks ("*"), intended for their substitution by
;; actual 5iasm code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command         | Effect
;;   ----------------+-------------------------------------------------
;;   inc register    | Increments the value of the register designated
;;       ********    | by {register} by one (1).
;;                   |-------------------------------------------------
;;                   | If {register} designates an invalid register
;;                   | name, an error of the type "InvalidRegisterName"
;;                   | is signaled.
;;   ..................................................................
;;   dec register    | Decrements the value of the register designated
;;       ********    | by {register} by one (1).
;;                   |-------------------------------------------------
;;                   | If {register} designates an invalid register
;;                   | name, an error of the type "InvalidRegisterName"
;;                   | is signaled.
;;   ..................................................................
;;   isz register    | If the value of the register designated by
;;       ********    | {register} equals zero (0), skips the subsequent
;;                   | instruction; otherwise proceeds as usual.
;;                   |-------------------------------------------------
;;                   | If {register} designates an invalid register
;;                   | name, an error of the type "InvalidRegisterName"
;;                   | is signaled.
;;   ..................................................................
;;   jmp destination | Relocates the instruction pointer (IP) to the
;;       *********** | line designated by the {destination}.
;;                   |-------------------------------------------------
;;                   | The {destination} must be either of the twain:
;;                   |   (a) The one-based line number to target.
;;                   |   (b) The name of a label whose declaration line
;;                   |       shall be targeted.
;;                   |-------------------------------------------------
;;                   | If the {destination} designates an invalid line
;;                   | number, an error of the type
;;                   | "InvalidLineNumberError" is signaled.
;;                   |-------------------------------------------------
;;                   | If the {destination} designates a nonexistant
;;                   | label name, an error of the type
;;                   | "UndefinedLabelNameError" is signaled.
;;   ..................................................................
;;   labelName :     | Declares a new label designated by the
;;   *********       | {labelName} and associated with its one-based
;;                   | line number.
;;                   |-------------------------------------------------
;;                   | The {labelName} must be a valid label
;;                   | identifier.
;;                   |-------------------------------------------------
;;                   | If the {labelName} designates an invalid
;;                   | identifier, an error of the type
;;                   | "InvalidLabelNameError" is signaled.
;;                   |-------------------------------------------------
;;                   | If the {labelName} has already been declared
;;                   | prior to this definition, an error of the type
;;                   | "DuplicateLabelNameError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its rather mickle compass of its elucidations and concomitant
;; example's provision, certain aspects of crepuscular conformation
;; remain, a subset extracted thereof shall be the below sections'
;; cynosure.
;; 
;; == WHAT RESPONSE SHALL BE ELICTED BY INVALID LINE NUMBERS? ==
;; In the "jmp" command's capacitation wones a twifold potential for
;; the control flow's redirection; imprimis, a label name's summons,
;; and, as a secondary option, an one-based line number's imposition.
;; The system's response, however, to such indices deviating from the
;; numeric gamut demarcated by the minimum one (1) and the tally of
;; effective lines, does not partake in the protologue's treatise.
;; 
;; It has been adjudged to inflict a program encountering an invalid
;; line number with an error of the type "InvalidLineNumberError", the
;; severity apportioned to whom immediately aborts the execution with a
;; failed state.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's incarnation is realized in the programming
;; language Common Lisp, the source code's reformulation from its
;; original string to an executable variant proceeds by adminiculum of
;; a two-tier process, the incipient producing from the one-dimensional
;; character sequence a collection of lines, the second yielding from
;; these extracted rows an ordered list of representative instruction
;; objects.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-10-31
;; 
;; Sources:
;;   [esolang2023_5iasm]
;;   The Esolang contributors, "5iasm", January 12th, 2023
;;   URL: "https://esolangs.org/wiki/5iasm"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (type-name
                              (candidate-name &rest lambda-list)
                              &body body)
  "Defines a derived type whose agnomination is provided by the
   TYPE-NAME, the stipulations of its formal parameters being
   appropriated in an ipsissima verba mode from the LAMBDA-LIST, and
   which probes, under assignment of the CANDIDATE-NAME for the subject
   of the docimasy's nevening, the same for its covenableness by
   evaluating the BODY forms, the desinent form's primary return value
   supplying the assessment's conclusion, a generalized boolean result
   of \"true\" for its compliance, other \"false\".
   ---
   The first BODY form, if resolving to a string object, will be
   construed as the type's documentation string, and be reappropriated
   for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
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

(deftype instruction-class ()
  "The ``instruction-class'' type enumerates the recognized variation on
   5iasm operations, serving as representations of language mnemonics."
  '(member :inc :dec :jmp :isz :stp))

;;; -------------------------------------------------------

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   T)
                                             (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which complies with the KEY-TYPE, allied
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-custom-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   the same holds the default of the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable 5iasm program as a
   one-dimensional simple array of ``Statement'' instances."
  '(simple-array Statement (*)))

;;; -------------------------------------------------------

(deftype line-number ()
  "The ``line-number'' type defines an index into a 5iasm program's
   lines as a positive integer number greater than or equal to one (1),
   but bourneless along the positive axis, that is, a commorant of the
   range [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype register-bank ()
  "The ``register-bank'' type defines a fixed-sized set of registers,
   amenable to single-character names that respond with a signed integer
   number each, and being represented by a hash table, mapping character
   identifiers to integer obejcts."
  '(hash-table-of character integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 13) +REGISTER-NAMES+))

;;; -------------------------------------------------------

(defparameter +REGISTER-NAMES+ "ABCDEFGHINOXY"
  "Defines the recognized register names embedded in a single string.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition 5iasm-Error (error)
  ()
  (:documentation
    "The ``5iasm-Error'' condition type establishes the firmament shared
     by all error species subsumed into the process of a 5iasm program's
     analyzation, parsing, or interpretation."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Name-Error (5iasm-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending label name.")
    :reader        duplicate-label-name-error-offending-name
    :type          string
    :documentation "The already defined label name."))
  (:documentation
    "The ``Duplicate-Label-Name-Error'' condition type serves in the
     apprizal about the attempt to declare a label by an agnomination
     already reserved for such."))

;;; -------------------------------------------------------

(define-condition Invalid-Label-Name-Error (5iasm-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending label name.")
    :reader        invalid-label-name-error-offending-name
    :type          string
    :documentation "The identifier irreconcilable for an employment as
                    a label name."))
  (:documentation
    "The ``Invalid-Label-Name-Error'' condition type serves in the
     apprizal about the attempt to declare a label by an agnomination
     inadmissible to this purpose, either by being of a forinsecal
     conformation, or by its alliance with a 5iasm language keyword."))

;;; -------------------------------------------------------

(define-condition Invalid-Line-Number-Error (5iasm-Error simple-error)
  ((offending-index
    :initarg       :offending-index
    :initform      (error "Missing offending line number.")
    :reader        invalid-line-number-error-offending-index
    :type          integer
    :documentation "The line number transgressing the 5iasm program's
                    established bournes."))
  (:documentation
    "The ``Invalid-Line-Number-Error'' condition type serves in the
     apprizal about the attempt to navigate a 5iasm program's
     instruction pointer (IP) to a line of uncovenable indexing."))

;;; -------------------------------------------------------

(define-condition Invalid-Register-Name-Error (5iasm-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending register name.")
    :reader        invalid-register-name-error-offending-name
    :type          character
    :documentation "The invalid register name contributing this error's
                    etiology."))
  (:documentation
    "The ``Invalid-Register-Name-Error'' condition type serves in the
     apprizal about an inadmissible register agnomination's trial for
     any species of access."))

;;; -------------------------------------------------------

(define-condition Missing-Label-Name-Error (5iasm-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending label name.")
    :reader        missing-label-name-error-offending-name
    :type          string
    :documentation "The identifier requested but absent for a
                    label-based control flow operation."))
  (:documentation
    "The ``Missing-Label-Name-Error'' condition type serves in the
     apprizal about the attempt to navigate a 5iasm program to an
     inexistent label."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition signaling operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-duplicate-label-name-error (offending-name)
  "Signals an error of the type ``Duplicate-Label-Name-Error'',
   apprizing about the OFFENDING-NAME as its etiology."
  (declare (type string offending-name))
  (error 'Duplicate-Label-Name-Error
    :offending-name   offending-name
    :format-control   "A label with the name ~s has already been ~
                       defined."
    :format-arguments (list offending-name)))

;;; -------------------------------------------------------

(defun signal-invalid-label-name-error (offending-name)
  "Signals an error of the type ``Invalid-Label-Name-Error'', apprizing
   about the OFFENDING-NAME as its etiology."
  (declare (type string offending-name))
  (error 'Invalid-Label-Name-Error
    :offending-name   offending-name
    :format-control   "The name ~s cannot be utilized for a label."
    :format-arguments (list offending-name)))

;;; -------------------------------------------------------

(defun signal-invalid-line-number-error (offending-index)
  "Signals an error of the type ``Invalid-Line-Number-Error'', apprizing
   about the OFFENDING-INDEX as its etiology."
  (declare (type integer offending-index))
  (error 'Invalid-Line-Number-Error
    :offending-index  offending-index
    :format-control   "The index ~d does not designate a valid line ~
                       number into the program."
    :format-arguments (list offending-index)))

;;; -------------------------------------------------------

(defun signal-invalid-register-name-error (offending-name)
  "Signals an error of the type ``Invalid-Register-Name-Error'',
   apprizing about the OFFENDING-NAME as its etiology."
  (declare (type character offending-name))
  (error 'Invalid-Register-Name-Error
    :offending-name   offending-name
    :format-control   "The name \"~c\" does not designate a valid ~
                       register."
    :format-arguments (list offending-name)))

;;; -------------------------------------------------------

(defun signal-missing-label-name-error (offending-name)
  "Signals an error of the type ``Missing-Label-Name-Error'', apprizing
   about the OFFENDING-NAME as its etiology."
  (declare (type string offending-name))
  (error 'Missing-Label-Name-Error
    :offending-name   offending-name
    :format-control   "No label with the name ~s exists."
    :format-arguments (list offending-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump destination classes.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Destination)
  "The ``Jump-Destination'' interface establishes a common firmament
   alow which all \"jmp\" jump destination specifications subsume.")

;;; -------------------------------------------------------

(defstruct (Jump-Label-Destination
  (:include Jump-Destination))
  "The ``Jump-Label-Destination'' class furnishes an encapsulation of a
   \"jmp\"-conformant relocation target in the form of a label name."
  (label (error "Missing jump destination label.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Jump-Line-Destination
  (:include Jump-Destination))
  "The ``Jump-Line-Destination'' class furnishes an encapsulation of a
   \"jmp\"-conformant relocation target in the form of a line number."
  (line (error "Missing jump destination line number.")
        :type      integer
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of statement classes.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Statement)
  "The ``Statement'' interface serves in the provision of a firmament
   for the representation of 5iasm instructions.")

;;; -------------------------------------------------------

(defstruct (Dec-Statement
  (:include Statement))
  "The ``Dec-Statement'' class applies itself to the representation of
   a 5iasm \"dec\" instruction, incorporating in its diorism the
   targeted register's agnomination."
  (register (error "Missing \"dec\" instruction register.")
            :type      character
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Inc-Statement
  (:include Statement))
  "The ``Inc-Statement'' class applies itself to the representation of
   a 5iasm \"inc\" instruction, incorporating in its diorism the
   targeted register's agnomination."
  (register (error "Missing \"inc\" instruction register.")
            :type      character
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Isz-Statement
  (:include Statement))
  "The ``Isz-Statement'' class applies itself to the representation of
   a 5iasm \"isz\" instruction, incorporating in its diorism the
   targeted register's agnomination."
  (register (error "Missing \"isz\" instruction register.")
            :type      character
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Jmp-Statement
  (:include Statement))
  "The ``Jmp-Statement'' class applies itself to the representation of
   a 5iasm \"jmp\" instruction, incorporating in its diorism the
   relocation destination."
  (destination (error "Missing \"jmp\" instruction destination.")
               :type      Jump-Destination
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Definition-Statement
  (:include Statement))
  "The ``Label-Definition-Statement'' class applies itself to the
   representation of a 5iasm label definition instruction, incorporating
   in its diorism the label name."
  (label (error "Missing label definition instruction name.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Statement
  (:include Statement))
  "The ``NOP-Statement'' class applies itself to the encapsulation of
   a no-operation, or NOP, represented in the 5iasm programming language
   by a blank line.")

;;; -------------------------------------------------------

(defstruct (Stp-Statement
  (:include Statement))
  "The ``Stp-Statement'' class applies itself to the encapsulation of
   a 5iasm ``stp'' instruction.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its nature as a \"generalized boolean\",
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aliquot-p (dividend divisor)
  "Determines whether the DIVISOR forms an aliquot of the DIVIDEND, that
   is, divides the latter without yielding a rest, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type integer dividend))
  (declare (type integer divisor))
  (the boolean
    (get-boolean-value-of
      (zerop (rem dividend divisor)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDDIATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a 5iasm keyword
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (not (space-character-p candidate))
        (char/= candidate #\:)
        (char/= candidate #\;)))))

;;; -------------------------------------------------------

(defun register-name-p (candidate)
  "Determines whether the CANDIDATE represents a register identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate +REGISTER-NAMES+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, returns the index
   at which the nearest word terminates."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'identifier-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun extract-word-at (source start)
  "Proceeding from the START position into the SOURCE, locates the
   nearest word and returns two values:
     (1) The word commencing at the START position into the SOURCE as a
         fresh string.
     (2) The position into the SOURCE immediately succeeding the parcel
         under the detected word's occupancy."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (locate-end-of-word source start)))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (subseq source start end)
        end))))

;;; -------------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position immediately
   succeeding the occupied segment."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun trim-spaces-around-string (source)
  "Returns a fresh string based upon the SOURCE with its sinistral and
   dextral edges being purged of contingent spaces and horizontal tabs."
  (declare (type string source))
  (the string
    (string-trim '(#\Space #\Tab) source)))

;;; -------------------------------------------------------

(defun parse-jump-destination (source)
  "Parses the SOURCE as a ``Jump-Destination'' and returns the result."
  (declare (type string source))
  (the Jump-Destination
    (handler-case
      (make-jump-line-destination :line
        (parse-integer source))
      (error ()
        (make-jump-label-destination :label source)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (statements)
  "Creates and returns a fresh ``program'' derived from the list of
   STATEMENTS."
  (declare (type (list-of Statement) statements))
  (the program
    (coerce statements
      '(simple-array Statement (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Line-Scanner".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Line-Scanner
  (:constructor make-blank-line-scanner ()))
  "The ``Line-Scanner'' class serves in the encapsulation of a line of
   string extracted from the input 5iasm program, operating in
   conjunction with a position cursor, and prepared in such a manner as
   to facilitate its interpretation."
  (source "" :type string :read-only NIL)
  (cursor 0  :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun line-scanner-is-exhausted-p (scanner)
  "Determines whether the line SCANNER is exhausted, that is, its
   position cursor has transgressed its dextral bourne, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (the boolean
    (get-boolean-value-of
      (>= (line-scanner-cursor scanner)
          (length (line-scanner-source scanner))))))

;;; -------------------------------------------------------

(defun trim-line (scanner)
  "Curtails the line SCANNER's both ends by the expungement of spaces
   and tabs and returns the modified SCANNER."
  (declare (type Line-Scanner scanner))
  (setf (line-scanner-source scanner)
    (trim-spaces-around-string
      (line-scanner-source scanner)))
  (the Line-Scanner scanner))

;;; -------------------------------------------------------

(defun skip-space-in-current-line (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   skips a sequence of zero or more accolent spaces and returns the
   modified SCANNER."
  (declare (type Line-Scanner scanner))
  (setf (line-scanner-cursor scanner)
    (skip-spaces
      (line-scanner-source scanner)
      (line-scanner-cursor scanner)))
  (the Line-Scanner scanner))

;;; -------------------------------------------------------

(defun set-line-scanner-source (scanner new-source)
  "Changes the line SCANNER's source to the NEW-SOURCE, prunes the
   string's edges from spaces, resets its state, and returns the
   modified SCANNER."
  (declare (type Line-Scanner scanner))
  (declare (type string       new-source))
  (psetf (line-scanner-source scanner) new-source
         (line-scanner-cursor  scanner) 0)
  (trim-line scanner)
  (the Line-Scanner scanner))

;;; -------------------------------------------------------

(defun line-scanner-character-at (scanner index)
  "Returns the character located at the zero-based INDEX into the line
   SCANNER's source."
  (declare (type Line-Scanner scanner))
  (declare (type fixnum       index))
  (the character
    (char (line-scanner-source scanner) index)))

;;; -------------------------------------------------------

(defun character-at-position-matches-p (scanner
                                        index
                                        expected-character)
  "Determines whether the character located at the zero-based INDEX into
   the line SCANNER's source matches the EXPECTED-CHARACTER, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (declare (type fixnum       index))
  (declare (type character    expected-character))
  (the boolean
    (get-boolean-value-of
      (and
        (not (line-scanner-is-exhausted-p scanner))
        (char= (line-scanner-character-at scanner index)
               expected-character)))))

;;; -------------------------------------------------------

(defun current-character (scanner)
  "Returns the character commorant at the current position into the
   line SCANNER's source."
  (declare (type Line-Scanner scanner))
  (the character
    (line-scanner-character-at scanner
      (line-scanner-cursor scanner))))

;;; -------------------------------------------------------

(defun current-character-matches-p (scanner expected-character)
  "Determines whether the character located at the current position into
   the line SCANNER's source matches the EXPECTED-CHARACTER, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (declare (type character    expected-character))
  (the boolean
    (character-at-position-matches-p scanner
      (line-scanner-cursor scanner)
      expected-character)))

;;; -------------------------------------------------------

(defun comment-starts-p (scanner)
  "Determines whether, proceeding from the current position into the
   line SCANNER's source, a comment segment commences, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (the boolean
    (current-character-matches-p scanner #\;)))

;;; -------------------------------------------------------

(defun read-next-word (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   consumes the nearest word and returns a fresh string representation
   thereof."
  (declare (type Line-Scanner scanner))
  (skip-space-in-current-line scanner)
  (multiple-value-bind (next-word new-position)
      (extract-word-at
        (line-scanner-source scanner)
        (line-scanner-cursor  scanner))
    (declare (type string next-word))
    (declare (type fixnum new-position))
    (setf (line-scanner-cursor scanner) new-position)
    (the string next-word)))

;;; -------------------------------------------------------

(defun colon-follows-p (scanner)
  "Determines whether the line SCANNER's current character represents a
   colon (\":\"), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (the boolean
    (and
      (not (line-scanner-is-exhausted-p scanner))
      (char= (current-character scanner) #\:))))

;;; -------------------------------------------------------

(defun expect-colon (scanner)
  "Determines whether the current line SCANNER character constitutes a
   colon (\":\"), on confirmation consuming the same, while returning
   the modified SCANNER; otherwise an error of an unspecified type is
   signaled."
  (declare (type Line-Scanner scanner))
  (cond
    ((line-scanner-is-exhausted-p scanner)
      (error "Expected a colon (\":\") name at position ~d, ~
              but encountered the line exhausted."
        (line-scanner-cursor scanner)))
    ((current-character-matches-p scanner #\:)
      (incf (line-scanner-cursor scanner)))
    (T
      (error "Expected a colon (\":\") at position ~d, but encountered ~
              the character \"~c\"."
        (line-scanner-cursor scanner)
        (current-character   scanner))))
  (the Line-Scanner scanner))

;;; -------------------------------------------------------

(defun expect-end-of-line (scanner)
  "Determines whether, proceeding from the current position into the
   line SCANNER's source, no effective data follows, which imposes an
   interdiction to any source except for spaces and comments, returning
   on confirmation the SCANNER; otherwise signals an error of an
   unspecified type."
  (declare (type Line-Scanner scanner))
  (skip-space-in-current-line scanner)
  (unless (or (line-scanner-is-exhausted-p scanner)
              (comment-starts-p            scanner))
    (error "Expected the end of the source line, but encountered ~
            the character \"~c\" at position ~d."
      (current-character   scanner)
      (line-scanner-cursor scanner)))
  (the Line-Scanner scanner))

;;; -------------------------------------------------------

(defun register-name-follows-p (scanner)
  "Determines whether the line SCANNER's current character represents a
   register name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (the boolean
    (register-name-p
      (current-character scanner))))

;;; -------------------------------------------------------

(defun read-register-name (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   reads a register name and returns the same."
  (declare (type Line-Scanner scanner))
  (the character
    (cond
      ((line-scanner-is-exhausted-p scanner)
        (error "Expected a register name at position ~d, ~
                but encountered the line exhausted."
          (line-scanner-cursor scanner)))
      ((register-name-follows-p scanner)
        (prog1
          (current-character scanner)
          (incf (line-scanner-cursor scanner))))
      (T
        (signal-invalid-register-name-error
          (current-character scanner))))))

;;; -------------------------------------------------------

(defun identifier-follows-p (scanner)
  "Determines whether the line SCANNER's current character represents a
   valid identifier constituent, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Line-Scanner scanner))
  (the boolean
    (and
      (not (line-scanner-is-exhausted-p scanner))
      (identifier-character-p
        (current-character scanner)))))

;;; -------------------------------------------------------

(defun read-jump-destination (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   reads a \"jmp\" instruction operand, this being either a label name
   or a numeric line number, and returns a connable ``Jump-Destination''
   representation of the same."
  (declare (type Line-Scanner scanner))
  (the Jump-Destination
    (parse-jump-destination
      (read-next-word scanner))))

;;; -------------------------------------------------------

(defun read-label-definition (scanner label-name)
  "Proceeding from the current position into the line SCANNER's source,
   having just consumed the LABEL-NAME, reads a label definition
   instruction and returns a ``Label-Definition-Statement''
   representation thereof."
  (declare (type Line-Scanner scanner))
  (declare (type string       label-name))
  (expect-colon       scanner)
  (expect-end-of-line scanner)
  (the Label-Definition-Statement
    (make-label-definition-statement :label label-name)))

;;; -------------------------------------------------------

(defun parse-line (scanner input-line)
  "Evaluates the INPUT-LINE, utilizing the SOURCE-LINE as its ensconcing
   scanner, and returns a ``Statement'' representation of its content."
  (declare (type Line-Scanner scanner))
  (declare (type string       input-line))
  
  (set-line-scanner-source scanner input-line)
  (trim-line               scanner)
  
  (the Statement
    (cond
      ;; Blank line.
      ((line-scanner-is-exhausted-p scanner)
        (make-nop-statement))
      
      ;; Pure comment line.
      ((comment-starts-p scanner)
        (make-nop-statement))
      
      ((identifier-follows-p scanner)
        (let ((next-word (read-next-word scanner)))
          (declare (type string next-word))
          
          (cond
            ((string= next-word "")
              (prog1
                (make-nop-statement)
                (expect-end-of-line scanner)))
            
            ((colon-follows-p scanner)
              (read-label-definition scanner next-word))
            
            ((string= next-word "dec")
              (skip-space-in-current-line scanner)
              (prog1
                (make-dec-statement :register
                  (read-register-name scanner))
                (expect-end-of-line scanner)))
            
            ((string= next-word "inc")
              (skip-space-in-current-line scanner)
              (prog1
                (make-inc-statement :register
                  (read-register-name scanner))
                (expect-end-of-line scanner)))
            
            ((string= next-word "isz")
              (skip-space-in-current-line scanner)
              (prog1
                (make-isz-statement :register
                  (read-register-name scanner))
                (expect-end-of-line scanner)))
            
            ((string= next-word "jmp")
              (skip-space-in-current-line scanner)
              (prog1
                (make-jmp-statement :destination
                  (read-jump-destination scanner))
                (expect-end-of-line scanner)))
            
            ((string= next-word "stp")
              (prog1
                (make-stp-statement)
                (expect-end-of-line scanner)))
            
            (T
              (read-label-definition scanner next-word)))))
      
      (T
        (signal-invalid-label-name-error
          (read-next-word scanner))))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Extracts from the piece of 5iasm SOURCE code the incorporated
   statements and returns these as a vector."
  (declare (type string source))
  (let ((line-scanner (make-blank-line-scanner)))
    (declare (type Line-Scanner line-scanner))
    (with-input-from-string (input-stream source)
      (declare (type string-stream input-stream))
      (make-program
        (loop
          for current-line
            of-type (or null string)
            =       (read-line input-stream NIL NIL)
          while current-line
          append
            (let ((current-statement
                    (parse-line line-scanner current-line)))
              (declare (type Statement current-statement))
              (unless (nop-statement-p current-statement)
                (list current-statement))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :reader        label-table-entries
    :type          (hash-table-of string line-number)
    :documentation "Maps the label names as string to the one-based
                    line numbers of their declarations."))
  (:documentation
    "The ``Label-Table'' class is apportioned the dever of a 5iasm
     program's jump label's castaldy, affiliating with the respective
     names the one-based line numbers of their declarations in the
     code.
     ---
     Each label name is ought to be unique in the fact of its
     declaration."))

;;; -------------------------------------------------------

(defun make-empty-label-table ()
  "Creates and returns an initially vacant ``Label-Table''."
  (the Label-Table
    (make-instance 'Label-Table)))

;;; -------------------------------------------------------

(defun contains-label-with-name (labels name)
  "Determines whether an entry amenable to the NAME exists in the LABELS
   table, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name
          (label-table-entries labels))))))

;;; -------------------------------------------------------

(defun validate-availability-of-label-name (labels name)
  "Determines whether the label NAME may be appropriated in the LABELS
   table, returning on confirmation no value; otherwise an error of the
   type ``Duplicate-Label-Name-Error'' will be signaled."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (when (contains-label-with-name labels name)
    (signal-duplicate-label-name-error name))
  (values))

;;; -------------------------------------------------------

(defun register-label (labels name line-number)
  "Associates the label designated by the NAME with the one-based
   LINE-NUMBER in the LABELS table and returns no value.
   ---
   Upon an entry's presence with the same NAME, an error of the type
   ``Duplicate-Label-Name-Error'' will be signaled."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (declare (type line-number line-number))
  (validate-availability-of-label-name labels name)
  (setf (gethash name (label-table-entries labels)) line-number)
  (values))

;;; -------------------------------------------------------

(defun query-label-location (labels name)
  "Returns the one-based line number associated with the declaration of
   the label amenable to the NAME in the LABELS table, or signals an
   error of the type ``Missing-Label-Name-Error'' upon its absence."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (the line-number
    (or (gethash name (label-table-entries labels))
        (signal-missing-label-name-error name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-labels (program)
  "Generates and returns a fresh ``Label-Table'' composed of the 5iasm
   PROGRAM's label definitions."
  (declare (type program program))
  (let ((labels (make-empty-label-table)))
    (declare (type Label-Table labels))
    (loop
      for current-statement of-type Statement     across program
      and line-number       of-type (integer 1 *) from   1 by 1
      when (label-definition-statement-p current-statement) do
        (register-label labels
          (label-definition-statement-label current-statement)
          line-number))
    (the Label-Table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register bank.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-default-register-bank ()
  "Creates and returns a fresh ``register-bank'' initialized with the
   default states."
  (let ((registers (make-hash-table :test #'eql)))
    (declare (type register-bank registers))
    (loop for register-name of-type character across +REGISTER-NAMES+ do
      (setf (gethash register-name registers) 0))
    (the register-bank registers)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing 5iasm program.")
    :reader        program-statements
    :type          program
    :documentation "The 5iasm program in its diorism as a vector of
                    statements.")
   (labels
    :type          Label-Table
    :reader        program-labels
    :documentation "A mapping of label definitions to their one-based
                    line numbers in the PROGRAM.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          fixnum
    :documentation "The current instruction pointer (IP) position inside
                    of the PROGRAM.")
   (program-halted-p
    :initform      NIL
    :accessor      program-halted-p
    :type          boolean
    :documentation "Determines whether the program has been halted,
                    either naturally by a transgression of its bournes,
                    or in an extrinsic manner via a \"stp\" operation.")
   (registers
    :initform      (make-default-register-bank)
    :reader        program-registers
    :type          register-bank
    :documentation "Maintains the registers, associating the names with
                    signed integer values."))
  (:documentation
    "The ``Interpreter'' class serves in the administration of actual
     efficacy to a 5iasm program supplied as a statement sequence."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Generates the labels for the INTERPRETER's program, stores these in
   the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'labels)
    (collect-labels
      (program-statements interpreter)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the 5iasm
   PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-size (interpreter)
  "Returns the tally of statements partaking in the INTERPRETER's 5iasm
   program."
  (declare (type Interpreter interpreter))
  (the fixnum
    (length
      (program-statements interpreter))))

;;; -------------------------------------------------------

(defun valid-program-index-p (interpreter probed-index)
  "Determines whether the PROBED-INDEX represents a valid zero-based
   position into the INTERPRETER's program, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     probed-index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (program-statements interpreter)
        probed-index))))

;;; -------------------------------------------------------

(defun advance-instruction-pointer-if-possible (interpreter)
  "Attempts to translate the INTERPRETER's instruction pointer (IP)
   forward by one position, in any case returning no value."
  (declare (type Interpreter interpreter))
  (unless (program-halted-p interpreter)
    (incf (program-ip interpreter))
    (setf (program-ip interpreter)
      (min
        (program-ip   interpreter)
        (program-size interpreter))))
  (values))

;;; -------------------------------------------------------

(defun halt-program-if-necessary (interpreter)
  "Determines whether the 5iasm program submitted to the INTERPRETER's
   castaldy shall be halted, on confirmation modulating the respective
   flag, otherwise accompassing no value, and in any case returning no
   value."
  (declare (type Interpreter interpreter))
  (unless (program-halted-p interpreter)
    (setf (program-halted-p interpreter)
      (not (valid-program-index-p interpreter
        (program-ip interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) one position
   forward, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (advance-instruction-pointer-if-possible interpreter)
  (halt-program-if-necessary               interpreter)
  (values))

;;; -------------------------------------------------------

(defun jump-to-line (interpreter target-line-number)
  "Relocates the INTERPRETER's instruction pointer (IP) to the one-based
   TARGET-LINE-NUMBER and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     target-line-number))
  (setf (program-ip interpreter) target-line-number)
  (halt-program-if-necessary interpreter)
  (values))

;;; -------------------------------------------------------

(defun get-current-statement (interpreter)
  "Returns the statement contemporaneously selected by the INTERPRETER's
   instruction pointer (IP)."
  (declare (type Interpreter interpreter))
  (the Statement
    (aref
      (program-statements interpreter)
      (program-ip         interpreter))))

;;; -------------------------------------------------------

(defun register-value (interpreter name)
  "Returns the value of the register associated with the NAME in the
   INTERPRETER.
   ---
   This operation circumvents any contingent epiphenomena commorant in
   the designated register's perquisition aspect."
  (declare (type Interpreter interpreter))
  (declare (type character   name))
  (the integer
    (gethash name
      (program-registers interpreter))))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value interpreter name)
  "Associates the NEW-VALUE with the register amenable to the NAME in
   the INTERPRETER and returns no value.
   ---
   This operation circumvents any contingent epiphenomena commorant in
   the designated register's modification aspect."
  (declare (type Interpreter interpreter))
  (declare (type character   name))
  (setf (gethash name (program-registers interpreter)) new-value)
  (values))

;;; -------------------------------------------------------

(defun io-is-enabled-p (interpreter)
  "Determines whether the input and output facilities of the 5iasm
   program consigned to the INTERPRETER's castaldy are enabled,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (aliquot-p (register-value interpreter #\N) 3)))

;;; -------------------------------------------------------

(defgeneric read-register (interpreter name)
  (:documentation
    "Returns the numeric value stored in the register registered by the
     NAME at the INTERPRETER.")
  
  (:method ((interpreter Interpreter) (name (eql #\I)))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (when (io-is-enabled-p interpreter)
      (format T "~&>> ")
      (finish-output)
      (let ((user-input (char-code (read-char NIL NIL #\Null))))
        (declare (type fixnum user-input))
        (clear-input)
        (setf (register-value interpreter name) user-input)))
    (the integer
      (register-value interpreter name)))
  
  (:method ((interpreter Interpreter) (name (eql #\N)))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type character   name))
    (declare (ignore           name))
    (the integer 1))
  
  (:method ((interpreter Interpreter) (name (eql #\O)))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type character   name))
    (declare (ignore           name))
    (the integer 0))
  
  (:method ((interpreter Interpreter) (name character))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (the integer
      (register-value interpreter name))))

;;; -------------------------------------------------------

(defgeneric write-register (interpreter name new-value)
  (:documentation
    "Stores the NEW-VALUE in the register registered under the NAME at
     the INTERPRETER and returns no value.")
  
  (:method ((interpreter Interpreter)
            (name        (eql #\I))
            (new-value   integer))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (declare (type integer     new-value))
    (setf (register-value interpreter name) new-value)
    (values))
  
  (:method ((interpreter Interpreter)
            (name        (eql #\N))
            (new-value   integer))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (declare (type integer     new-value))
    (setf (register-value interpreter name) new-value)
    (values))
  
  (:method ((interpreter Interpreter)
            (name        (eql #\O))
            (new-value   integer))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (declare (type integer     new-value))
    (setf (register-value interpreter name) new-value)
    (when (io-is-enabled-p interpreter)
      (format T "~c"
        (code-char new-value)))
    (values))
  
  (:method ((interpreter Interpreter)
            (name        character)
            (new-value   integer))
    (declare (type Interpreter interpreter))
    (declare (type character   name))
    (declare (type integer     new-value))
    (setf (register-value interpreter name) new-value)
    (values)))

;;; -------------------------------------------------------

(defgeneric resolve-jump-destination (interpreter destination)
  (:documentation
    "Returns the one-based line of the number amenable to the 5iasm
     \"jmp\" instruction compatible jump DESTINATION utilizing the
     INTERPRETER's configurations.
     ---
     An error of an unspecified type shall be signaled if no resolution,
     or no appropriate solution, is discoverable.")
  
  (:method ((interpreter Interpreter)
            (destination Jump-Label-Destination))
    (declare (type Interpreter            interpreter))
    (declare (type Jump-Label-Destination destination))
    (the integer
      (query-label-location
        (program-labels interpreter)
        (jump-label-destination-label destination))))
  
  (:method ((interpreter Interpreter)
            (destination Jump-Line-Destination))
    (declare (type Interpreter           interpreter))
    (declare (ignore                     interpreter))
    (declare (type Jump-Line-Destination destination))
    (the integer
      (jump-line-destination-line destination))))

;;; -------------------------------------------------------

(defgeneric execute-statement (interpreter statement)
  (:documentation
    "Executes the STATEMENT in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter)
            (statement   Dec-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type Dec-Statement statement))
    (let ((register-name (dec-statement-register statement)))
      (declare (type character register-name))
      (write-register interpreter register-name
        (1- (register-value interpreter register-name))))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (statement   Inc-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type Inc-Statement statement))
    (let ((register-name (inc-statement-register statement)))
      (declare (type character register-name))
      (write-register interpreter register-name
        (1+ (register-value interpreter register-name))))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (statement   Isz-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type Isz-Statement statement))
    (when (zerop (read-register interpreter
                   (isz-statement-register statement)))
      (advance-program interpreter))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (statement   Jmp-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type Jmp-Statement statement))
    (jump-to-line interpreter
      (resolve-jump-destination interpreter
        (jmp-statement-destination statement)))
    (values))
  
  (:method ((interpreter Interpreter)
            (statement   Label-Definition-Statement))
    (declare (type Interpreter                interpreter))
    (declare (type Label-Definition-Statement statement))
    (declare (ignore                          statement))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (statement   Stp-Statement))
    (declare (type Interpreter   interpreter))
    (declare (type Stp-Statement statement))
    (declare (ignore             statement))
    (setf (program-halted-p interpreter) T)
    (advance-program interpreter)
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the 5iasm program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-halted-p interpreter) do
    (execute-statement interpreter
      (get-current-statement interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-5iasm (code)
  "Interprets the piece of 5iasm source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-5iasm
  "; Query input.
   isz I
   
   ; Disable input/output.
   inc N
   
   ; Handle special case of EOF input, which omits copying to the
   ; output register.
   isz I
   jmp copy_input_to_output
   jmp print_input
   
   ; Copy input register value to output register.
   copy_input_to_output:
   inc O
   dec I
   isz I
   jmp copy_input_to_output
   
   ; Adjust for supernumerary output register incrementation.
   dec O
   
   print_input:
   ; Enable input/output.
   dec N
   ; Print input stored in output register.
   inc O")

;;; -------------------------------------------------------

;; Perpetually repeating cat program.
(interpret-5iasm
  "repeat_program:
   
   ; Query input.
   isz I
   
   ; Disable input/output.
   inc N
   
   ; Handle special case of EOF input, which omits copying to the
   ; output register.
   isz I
   jmp copy_input_to_output
   jmp print_input
   
   ; Copy input register value to output register.
   ; The register 'A' applies itself to an adminicular role, employed
   ; for a conditional skipping via 'isz A' during the 'O' register's
   ; resetting to zero (0).
   copy_input_to_output:
   inc O
   inc A
   dec I
   isz I
   jmp copy_input_to_output
   
   ; Adjust for supernumerary output register incrementation.
   dec O
   dec A
   
   print_input:
   ; Enable input/output.
   dec N
   ; Print input stored in output register.
   inc O
   inc A
   
   ; Disable input/output.
   inc N
   
   ; Reset output register to zero.
   reset_output_register:
   dec O
   dec A
   isz A
   jmp reset_output_register
   
   ; Enable input/output.
   dec N
   
   jmp repeat_program")

;;; -------------------------------------------------------

;; Addition.
(interpret-5iasm
  "
  ; Clear C
  clearC:
  isz C
  jmp decC
  jmp clearX
  decC:
  dec C
  jmp clearC
  ; Clear X
  clearX:
  isz X
  jmp decX
  jmp processA
  decX:
  dec X
  jmp clearX
  ; Copy A to X and C
  processA:
  isz A
  jmp inc
  jmp restoreA
  inc:
  dec A
  inc X
  inc C
  jmp processA
  ; Now A is in X and C, but A=0
  ; Now restore A from X
  restoreA:
  isz X
  jmp incA
  jmp processB
  incA:
  inc A
  dec X
  jmp restoreA
  ; Now we copy the value from B to X and C
  ; X is 0, so it becomes a backup copy again.
  ; And in C is the sum afterwards.
  processB:
  isz B
  jmp inc2
  jmp restoreB
  inc2:
  dec B
  inc X
  inc C
  jmp processB
  ; We restore the value in B from X.
  restoreB:
  isz X
  jmp incB
  jmp done
  incB:
  inc B
  dec X
  jmp restoreB
  ; Done.
  done: 
  stp    
  ")
