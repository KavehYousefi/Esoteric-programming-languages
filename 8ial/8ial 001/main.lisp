;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "8ial", invented by the Esolang user "Ractangle" and
;; presented on September 15th, 2024, the kenspeckle haecceity of thilk
;; wones in its octuple instruction set, desumed in a conceptual
;; exercise of the conspectuity in the mimicry of an assembly language,
;; its competences' cynosure being the manipulation of integer objects
;; stored in 16 registers, aided in this endeavour by a twissel of
;; label-based control flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The 8ial programming language simulates an assembly language tallying
;; eight instructions, chosen as warklumes for a register bank's
;; manipulation by adminiculum of integer objects.
;; 
;; == 8IAL: AN [8] [i]NSTRUCTION [a]SSEMBLY [l]ANGUAGE ==
;; The 8ial programming language's agnomination engages in a bewrayment
;; of its operative circumference and its designment's entheus, this
;; being an "8 instruction assembly language".
;; 
;; == THE MEMORY: 16 REGISTERS ==
;; 8ial's data compartment employs the services of 16 registers, each
;; such a salvatory to a single unsigned byte value, occupying the
;; integral range of [0, 255], and, upon any of its bournes' violation,
;; wrapping to the overthwart extremum.
;; 
;; == REGISTERS: NEVENED BY NUMERICS ==
;; A register reference's syntactical indicium manifests in a
;; parasceuastic dollar sign, "$", whence follows an unsigned positive
;; integer identifier, for the same an enumeration principle applies
;; with an indexing from inclusive one (1) through sixteen (16).
;; 
;; 
;; Syntax
;; ======
;; From a conspection's exercise on its syntaxis, the 8ial programming
;; language's conformation limns an ordered sequence of zero or more
;; tokens, their occasions' merist a catena enumerating at least one
;; whitespace.
;; 
;; == PROGRAMS: SEQUENCES OF TOKENS ==
;; The gendrure of a program's consitution emerges from a
;; whitespace-separated token sequence, the ordonnance of which imposes
;; an instruction's definition, in the pevenience of zero through three
;; operands.
;; 
;; == INSTRUCTIONS: IDENTIFERS + ARGUMENTS ==
;; An instruction's inchoation proceeds by means of its identifier, a
;; prefixion to a maximum of three operands' participation.
;; 
;; == OPERANDS: REGISTER NAMES OR INTEGER LITERALS ==
;; The operand term's reification bifurcates into the contingency for
;; register names and integer literals, where the former contribute the
;; paravaunt utility.
;; 
;; == REGISTER NAMES: DESIGNATED POSITIVE INTEGER IDENTIFIERS ==
;; A register name empights an discriminating dollar sign, "$", to the
;; actual identification moeity's prevenience, a positive integer
;; number to whom the tolerance's admission of identifiers from
;; inclusive one (1) to inclusive sixteen (16) delineate the dation.
;; 
;; == INTEGER LITERALS: SIGNED OR UNSIGNED DECIMAL NUMBERS ==
;; Integer literals may assume signed or unsigned decimal numbers
;; endowed with bourneless mickleness along both axes.
;; 
;; == COMMENTS ==
;; The current language rendition's capacitation tholes a carency in
;; explicit commentary introductions; however, any token not engaged in
;; an operative causatum's affiliation constitutes a perfect neglect's
;; subject, whence is begotten the contingency for descants'
;; incarnations.
;; 
;; == GRAMMAR ==
;; The 8ial programming language's donet shall enjoy an augmented ilk
;; of formality's dation by mediation of an Extended Backus-Naur Form
;; (ENBF) description:
;; 
;;   program         := { command } ;
;;   
;;   command         := incCommand
;;                   |  decCommand
;;                   |  outCommand
;;                   |  putCommand
;;                   |  labelDefCommand
;;                   |  jmpCommand
;;                   |  jirCommand
;;                   |  endCommand
;;                   ;
;;   
;;   incCommand      := "INC" , registerName ;
;;   decCommand      := "DEC" , registerName ;
;;   outCommand      := "OUT" , registerName ;
;;   putCommand      := "PUT" , registerName ;
;;   labelDefCommand := ";"   , labelName ;
;;   jmpCommand      := "JMP" , labelName ;
;;   jirCommand      := "JIR" , labelName
;;                            , registerName
;;                            , numericOperand ;
;;   endCommand      := "END" ;
;;   
;;   labelName       := labelCharacter , { labelCharacter } ;
;;   labelCharacter  := digit | letter | "-" | "_" ;
;;   
;;   numericOperand  := signedInteger | registerName ;
;;   registerName    := "$" , unsignedInteger ;
;;   signedInteger   := [ "+" | "-" ] , unsignedInteger ;
;;   unsignedInteger := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   letter          := "a" | ... | "z" | "A" | ... | "Z" ;
;; 
;; 
;; Instructions
;; ============
;; 8ial's operative competences are begotten by an octuple instruction
;; set, the compass of which enumerates warklumes for the register
;; manipulation, numeric input and output conduits, as well as a
;; label-based control flow mechanism.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall serve in the adhibition of a cursory mete
;; of gnarity with the language's competences.
;; 
;; Please heed that underlining of succedaneous segments, intended for
;; their substitution in the ultimate program by valid 8ial code, via
;; a catena of asterisks ("*").
;; 
;; Please also heed that registers lacking an explicit initialization
;; respond with the default value of zero (0).
;; 
;;   ------------------------------------------------------------------
;;   Command                  | Effect
;;   -------------------------+----------------------------------------
;;   INC register             | Increments the value of the {register}
;;       ********             | by an amount of one (1). Upon a
;;                            | transgression of its upper bourne of
;;                            | 255, the {register} state wraps around
;;                            | to the minimum of zero (0).
;;                            |----------------------------------------
;;                            | The {register} must be a register name
;;                            | whose index is desumed from the closed
;;                            | integer interval of [1, 16].
;;   ..................................................................
;;   DEC register             | Decrements the value of the {register}
;;       ********             | by an amount of one (1). Upon a
;;                            | transgression of its lower bourne of
;;                            | zero (0), the {register} state wraps
;;                            | around to the maximum of 255.
;;                            |----------------------------------------
;;                            | The {register} must be a register name
;;                            | whose index is desumed from the closed
;;                            | integer interval of [1, 16].
;;   ..................................................................
;;   PUT register             | Queries the standard input for a signed
;;       ********             | or unsigned integer number and stores
;;                            | the response in the {register}.
;;                            |----------------------------------------
;;                            | The {register} must be a register name
;;                            | whose index is desumed from the closed
;;                            | integer interval of [1, 16].
;;   ..................................................................
;;   OUT register             | Prints the value of the {register} in
;;       ********             | its verbatim numeric form to the
;;                            | standard output conduit.
;;                            |----------------------------------------
;;                            | The {register} must be a register name
;;                            | whose index is desumed from the closed
;;                            | integer interval of [1, 16].
;;   ..................................................................
;;   ;label                   | Declares a label amenable to the
;;    *****                   | {label} name and memorizes an
;;                            | association with its position in
;;                            | the program.
;;                            |----------------------------------------
;;                            | If the {label} name is defined more
;;                            | than once in a program, an error of the
;;                            | type "DuplicateLabelNameError" is
;;                            | signaled.
;;   ..................................................................
;;   JMP label                | Relocates the instruction pointer (IP)
;;       *****                | to the position of the label designated
;;                            | by the {labelName}.
;;                            |----------------------------------------
;;                            | If the {label} cannot be retrieved in
;;                            | the program, an error of the type
;;                            | "UndefinedLabelError" is signaled.
;;   ..................................................................
;;   JIR label register guard | If the value of the {register} equals
;;       ***** ******** ***** | the {guard}, relocates the instruction
;;                            | pointer (IP) to the position of the
;;                            | label designated by the {labelName};
;;                            | otherwise accompasses no causatum.
;;                            |----------------------------------------
;;                            | The {register} must be a register name
;;                            | whose index is desumed from the closed
;;                            | integer interval of [1, 16].
;;                            |----------------------------------------
;;                            | The {guard} must either be a literal
;;                            | integer number or a register name whose
;;                            | index is desumed from the closed
;;                            | integer interval of [1, 16].
;;                            |----------------------------------------
;;                            | If the {label} cannot be retrieved in
;;                            | the program, an error of the type
;;                            | "UndefinedLabelError" is signaled.
;;   ..................................................................
;;   END                      | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's development has been realized in the programming
;; language Common Lisp, the entirety's componency being distributed
;; among several tiers, the incipiency among these obtaining its
;; provision from a lexical analyzation and concomitant parsing applied
;; to the sere tokens, contingently by an coalescence of operands with
;; their affiliated operations, yielding as its product an encapsulation
;; in an "Instruction" class object.
;; 
;; The generation and gathering of these instruction representations in
;; a vector compound establishes the interpreter's requisitum for an
;; ultimate execution.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   Date: 2025-11-23
;; 
;; Sources:
;;   [esolang2025_8ial]
;;   The Esolang contributors, "8ial", November 14th, 2025
;;   URL: "https://esolangs.org/wiki/8ial"
;;   
;;   [esolang2024comment]
;;   The Esolang contributors, "Comment", November 20th, 2024
;;   URL: "https://esolangs.org/wiki/Comment#8ial"
;;   Notes:
;;     - Demonstrates the induction of comments into the 8ial
;;       programming language.
;;     - States that an error inflicting a command transforms thilk
;;       into a comment.
;;     - The statements, at least partially, have been rendered
;;       obsolete by the current 8ial language rendition.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   thilk holds the default of the comprehensive ``T'' species."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which complies with the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the default
   of the comprehensive ``T''."
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

(deftype register-number ()
  "The ``register-number'' type defines a register identifier as an
   integral number greater than or equal to one (1), but without a
   natural bourne towards the upper extremum, thus being a commorant of
   the integer range [1, +infinity]."
  '(integer 1 16))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte datum comprised of eight
   (8) accolent bits, and thus a commorant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype register-array ()
  "The ``register-array'' type defines a random-access bank of sixteen
   (16) unsigned byte-valued registers in the guise of a one-dimensional
   simple array of octets."
  '(simple-array octet (16)))

;;; -------------------------------------------------------

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a one-dimensional simple
   array compact of zero or more ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass of its diorism enumerates, without the claim of exhaustion,
   the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a veridical Boolean tantamount to the OBJECT as construed in
   its aspect as a \"generalized boolean\" entity, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, produces the selfsame ``NIL'' value."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operands.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand)
  "The ``Operand'' interface serves in the furnishment of a common
   firmament for all  representations of 8ial instruction operands.")

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:include Operand))
  "The ``Literal-Operand'' class represents a literal integer number
   specified as an 8ial instruction operand."
  (value (error "Missing literal operand value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Register-Operand
  (:include Operand))
  "The ``Register-Operand'' class represents a numeric register
   identifier specified as an 8ial instruction operand."
  (index (error "Missing register operand identifier.")
         :type      register-number
         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' interface establishes a common foundry for all
   classes pursuing the representation of an 8ial operation invocation,
   this diorism's circumference incorporating the arguments.")

;;; -------------------------------------------------------

(defstruct (INC-Instruction
  (:include Instruction))
  "The ``INC-Instruction'' class serves in the encapsulation of an
   8ial \"INC\" operation, nuncupated to an optated register's
   incrementation by an amount of one (1)."
  (register (error "Missing INC-Instruction register index.")
            :type      Register-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (DEC-Instruction
  (:include Instruction))
  "The ``DEC-Instruction'' class serves in the encapsulation of an
   8ial \"DEC\" operation, nuncupated to an optated register's
   incrementation by an amount of one (1)."
  (register (error "Missing DEC-Instruction register index.")
            :type      Register-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (OUT-Instruction
  (:include Instruction))
  "The ``OUT-Instruction'' class serves in the encapsulation of an 8ial
   \"OUT\" operation, nuncupated to the printing of an optated
   register's value on the standard output."
  (register (error "Missing OUT-Instruction register index.")
            :type      Register-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (PUT-Instruction
  (:include Instruction))
  "The ``PUT-Instruction'' class serves in the encapsulation of an
   8ial \"PUT\" operation, nuncupated to a specified register value's
   reception of numeric data from the standard input."
  (register (error "Missing PUT-Instruction register index.")
            :type      Register-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Definition-Instruction
  (:include Instruction))
  "The ``Label-Definition-Instruction'' class encapsulates the behest
   involving the definition of a label in an 8ial program."
  (label (error "Missing Label-Definition-Instruction label.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (JMP-Instruction
  (:include Instruction))
  "The ``JMP-Instruction'' class serves in the encapsulation of an 8ial
   \"JMP\" operation, nuncupated to the unconditional navigation to an
   optated label."
  (label (error "Missing JMP-Instruction label.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (JIR-Instruction
  (:include Instruction))
  "The ``JIR-Instruction'' class serves in the encapsulation of an
   8ial \"JIR\" operation, nuncupated to the program instruction
   pointer's (IP) conditional relocation to a specific label upon a
   given register value's equiparation with a guard."
  (label     (error "Missing JIR-Instruction label.")
             :type      string
             :read-only T)
  (candidate (error "Missing JIR-Instruction candidate register.")
             :type      Register-Operand
             :read-only T)
  (guard     (error "Missing JIR-Instruction guard.")
             :type      Operand
             :read-only T))

;;; -------------------------------------------------------

(defstruct (END-Instruction
  (:include Instruction))
  "The ``END-Instruction'' class serves in the encapsulation of an 8ial
   \"END\" operation, nuncupated to a program's immediate termination.")

;;; -------------------------------------------------------

(defstruct (NOP-Instruction
  (:include Instruction))
  "The ``NOP-Instruction'' class serves in the encapsulation of a
   ineffectual no-operation, or NOP.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction vector operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction-vector (instructions)
  "Creates and returns a fresh ``instruction-vector'' from the list of
   INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the instruction-vector
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing program instructions.")
    :reader        program-instructions
    :type          instruction-vector
    :documentation "A vector of the program's instructions."))
  (:documentation
    "The ``Program'' class serves in the encapsulation of a parsed
     8ial program's instructions in the guise of a random-access
     sequence."))

;;; -------------------------------------------------------

(defun program-instruction-at (program index)
  "Returns the instruction at the zero-based index into the PROGRAM."
  (declare (type Program program))
  (declare (type fixnum  index))
  (the Instruction
    (aref (program-instructions program) index)))

;;; -------------------------------------------------------

(defun program-size (program)
  "Returns the tally of instructions comprising the PROGRAM."
  (declare (type Program program))
  (the fixnum
    (length (program-instructions program))))

;;; -------------------------------------------------------

(defun valid-program-index-p (program probed-index)
  "Determines whether the PROBED-INDEX represents a valid location into
   the PROGRAM's instruction sequence, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  probed-index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (program-instructions program)
        probed-index))))

;;; -------------------------------------------------------

(defmethod print-object ((program Program) (stream T))
  (declare (type Program     program))
  (declare (type destination stream))
  (format stream "~&Program with ~d instruction~:p"
    (program-size program))
  (loop
    for current-instruction
      of-type Instruction
      across  (program-instructions program)
    and instruction-index
      of-type fixnum
      from    0
      by      1
    do
      (format stream "~&~2tNo. ~d: ~a"
        instruction-index current-instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition 8ial-Error (error)
  ()
  (:documentation
    "The ``8ial-Error'' condition type serves in the apprizal about an
     anomalous situation whose emergency occurred in conjunction with
     an 8ial program's admission, analyzation, or interpretation."))

;;; -------------------------------------------------------

(define-condition Label-Error (8ial-Error)
  ((name
    :initarg       :name
    :initform      (error "Missing label error name.")
    :type          string
    :reader        label-error-name
    :documentation "The label identifier responsible for this error's
                    instigation."))
  (:documentation
    "The ``Label-Error'' condition type serves in the communication of
     an anomalous situation whose etiology is begotten by a mistaken
     handling of a label name in any conceivable mode."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (Label-Error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type destination           stream))
      (format stream "A label with the name ~s has already been ~
                      registered and cannot be redefined."
        (label-error-name condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the
     communication of an anomalous situation whose etiology emerges
     from the attempt to define a label with an identifier already
     employed for such purpose."))

;;; -------------------------------------------------------

(define-condition Undefined-Label-Error (Label-Error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type Undefined-Label-Error condition))
      (declare (type destination           stream))
      (format stream "No label with the name ~s exists."
        (label-error-name condition))))
  (:documentation
    "The ``Undefined-Label-Error'' condition type serves in the
     communication of an anomalous situation whose etiology emerges
     from the attempt to perquire a label with an identifier not
     registered for this purpose."))

;;; -------------------------------------------------------

(define-condition 8ial-Condition (condition)
  ()
  (:documentation
    "The ``8ial-Condition'' condition type furnishes a common foundry
     for all conditions serving to communicate in a neural fashion a
     significant event pertinent to an 8ial program's admission,
     analyzation, or interpretation."))

;;; -------------------------------------------------------

(define-condition Halt-Condition (8ial-Condition)
  ()
  (:documentation
    "The ``Halt-Condition'' condition type serves in the apprizal about
     a behest to immediately terminate an 8ial program in the course of
     its execution."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of user input operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-user-input ()
  "Displays a prompt character sequence on the standard output, queries
   the standard input for a line of string, and returns thilk.
   ---
   For the case of an end-of-file (EOF) encounter, this operation
   signals a ``Halt-Condition''."
  (format T "~&>> ")
  (finish-output)
  (the (or null string)
    (prog1
      (or (read-line NIL NIL NIL)
          (signal 'Halt-Condition))
      (clear-input))))

;;; -------------------------------------------------------

(defun parse-input (user-input)
  "Attempts to parse the USER-INPUT as a signed or unsigned integer
   number, on success returning the received numeric value, otherwise
   issuing an error message to the standard output, ere responding with
   the ``NIL'' sentinel."
  (declare (type string user-input))
  (the (or null integer)
    (handler-case
      (parse-integer user-input)
      (error ()
        (format T "~&The input ~s does not represent an integer ~
                     number. Please commit a valid datum."
          user-input)
        NIL))))

;;; -------------------------------------------------------

(defun query-integer ()
  "Queries the standard input for a signed or unsigned integer number
   until thilk has been committed in a covenable form, finally returning
   the received datum."
  (the integer
    (loop
      for user-input
        of-type (or null string)
        =       (read-user-input)
      for parsed-input
        of-type (or null integer)
        =       (parse-input user-input)
      until parsed-input
      finally
        (return parsed-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register identifier operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-register-number (identifier)
  "Determines whether the numeric IDENTIFIER represents a valid register
   name, which ought to constitute a positive integer number, returning
   on confirmation the IDENTIFIER in its ipsissima verba guise;
   otherwise signals an error of an unspecified type."
  (declare (type integer identifier))
  (the integer
    (if (typep identifier 'register-number)
      identifier
      (error "The value ~d does not represent a valid register ~
              number, as the same must be a positive integer ~
              greater than or equal to one."
        identifier))))

;;; -------------------------------------------------------

(defun string-introduces-register-name-p (source)
  "Determines whether the SOURCE, by adminiculum of a dollar sign
   (\"$\") prefixion, is entalented with the contingency for a register
   reference's designation, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (and
        (plusp (length source))
        (string= source #\$ :start1 0 :end1 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label name operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun label-name-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent admissible
   for a label identifier, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (alphanumericp candidate)
          (find          candidate "_-" :test #'char=)))))

;;; -------------------------------------------------------

(defun valid-label-name-p (candidate)
  "Determines whether the CANDIDATE comprehends exclusively characters
   admissible for a label identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (every #'label-name-character-p candidate))))

;;; -------------------------------------------------------

(defun validate-label-name (name)
  "Determines whether the NAME signified a valid label identifier,
   returning on confirmation the NAME in its verbatim conformation;
   otherwise signals an error of an unspecified type."
  (declare (type string name))
  (the string
    (cond
      ((zerop (length name))
        (error "The empty string cannot be a label name."))
      ((not (valid-label-name-p name))
        (error "The identifier ~s contains symbols not homologated ~
                to appear in a label name."
          name))
      (T
        name))))

;;; -------------------------------------------------------

(defun string-introduces-a-label-definition-p (source)
  "Determines whether the SOURCE, by adminiculum of a semicolon (\";\")
   prefixion, is entalented with the contingency for a label
   definition's designation, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (and
        (plusp (length source))
        (string= source #\; :start1 0 :end1 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent homologated
   to appear in a word, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (whitespace-character-p candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global scanner variables.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type string  *source*))
(declaim (type fixnum  *source-length*))
(declaim (type fixnum  *word-start-index*))
(declaim (type fixnum  *word-end-index*))
(declaim (type string  *current-word*))
(declaim (type boolean *source-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of 8ial source code to analyze.")

(define-symbol-macro *source-length*
  (the fixnum
    (length *source*)))

(defparameter *word-start-index* 0
  "The inclusive zero-based start position of the most recently
   extracted ``*CURRENT-WORD*'' from the ``*SOURCE*''.")

(defparameter *word-end-index* 0
  "The exclusive zero-based end position of the most recently extracted
   ``*CURRENT-WORD*'' from the ``*SOURCE*''.")

(define-symbol-macro *current-word*
  (the string
    (subseq *source* *word-start-index* *word-end-index*)))

(define-symbol-macro *source-is-exhausted-p*
  (the boolean
    (get-boolean-value-of
      (>= *word-start-index*
          *word-end-index*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-scanner-source (new-source)
  "Sets the NEW-SOURCE as the scanner's source, restores the its
   incipial configurations, and returns no value."
  (psetf
    *source*           new-source
    *word-start-index* 0
    *word-end-index*   0)
  (values))

;;; -------------------------------------------------------

(defun locate-start-of-next-word ()
  "Proceeding from the current position into the *SOURCE*, locates the
   index of the nearest following word's first character and returns no
   value."
  (setf *word-start-index*
    (or (position-if #'word-character-p *source*
          :start *word-end-index*)
        *source-length*))
  (values))

;;; -------------------------------------------------------

(defun locate-end-of-word ()
  "Proceeding from the current position into the *SOURCE*, locates the
   index succeeding the desinent character of the nearest following word
   and returns no value."
  (setf *word-end-index*
    (or (position-if-not #'word-character-p *source*
          :start *word-start-index*)
        *source-length*))
  (values))

;;; -------------------------------------------------------

(defun peek-next-word ()
  "Proceeding from the current position into the *SOURCE*, consumes the
   nearest following word, memorizes its bournes, and returns no value."
  (locate-start-of-next-word)
  (locate-end-of-word)
  (values))

;;; -------------------------------------------------------

(defun current-word-represents-register-p ()
  "Determines whether *CURRENT-WORD* represents a register significator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (string-introduces-register-name-p *current-word*)))

;;; -------------------------------------------------------

(defun expect-register-reference ()
  "Determines whether the *CURRENT-WORD* introduces a register
   reference, its incipient character being equal to the \"$\" sign, on
   confirmation returning no value; otherwise an error of an
   unspecified type is signaled."
  (unless (current-word-represents-register-p)
    (error "The token ~s, commencing at position ~d, does not ~
            represent a register reference."
      *current-word* *word-start-index*))
  (values))

;;; -------------------------------------------------------

(defun read-number ()
  "Parses the *CURRENT-WORD* as a signed or unsigned integer literal and
   returns the extracted numeric object."
  (the integer
    (handler-case
      (parse-integer *current-word*)
      (error ()
        (error "The token ~s, starting at the position ~d, does not~
                represent an integral number."
          *current-word* *word-start-index*)))))

;;; -------------------------------------------------------

(defun read-integer-literal ()
  "Parses the *CURRENT-WORD* as a signed or unsigned integer literal and
   returns a fresh ``Literal-Operand'' representation of the extracted
   numeric object."
  (the Literal-Operand
    (make-literal-operand :value
      (read-number))))

;;; -------------------------------------------------------

(defun read-register-reference ()
  "Parses the *CURRENT-WORD* as a register reference and returns a fresh
   ``Register-Operand'' representation of the extracted numeric object."
  (expect-register-reference)
  (incf *word-start-index*)
  (the Register-Operand
    (make-register-operand :index
      (validate-register-number
        (read-number)))))

;;; -------------------------------------------------------

(defun read-numeric-operand ()
  "Attempts to parse the *CURRENT-WORD* as a register reference, on
   success returning a fresh ``Register-Operand'' representation of its
   signified target; otherwise produces a fresh ``Literal-Operand'' of
   the imputed signed or unsigned integer datum extracted from the
   string object."
  (the Operand
    (if (current-word-represents-register-p)
      (read-register-reference)
      (read-integer-literal))))

;;; -------------------------------------------------------

(defun read-label-name ()
  "Interprets the *CURRENT-WORD* as a label name, validates its
   conformation, and returns thilk in its ipsissima verba form."
  (the string
    (validate-label-name *current-word*)))

;;; -------------------------------------------------------

(defun read-next-instruction ()
  "Proceeding from the current position into the *SOURCE*, reads an
   8ial instruction and returns a covenable ``Instruction''
   representation thereof."
  (the Instruction
    (cond
      ((string-introduces-a-label-definition-p *current-word*)
        (incf *word-start-index*)
        (make-label-definition-instruction :label
          (read-label-name)))
      
      ((string= *current-word* "INC")
        (peek-next-word)
        (make-inc-instruction :register
          (read-register-reference)))
      
      ((string= *current-word* "END")
        (peek-next-word)
        (make-end-instruction))
      
      ((string= *current-word* "OUT")
        (peek-next-word)
        (make-out-instruction :register
          (read-register-reference)))
      
      ((string= *current-word* "JMP")
        (peek-next-word)
        (make-jmp-instruction :label
          (read-label-name)))
      
      ((string= *current-word* "PUT")
        (peek-next-word)
        (make-put-instruction :register
          (read-register-reference)))
      
      ((string= *current-word* "JIR")
        (make-jir-instruction
          :label     (progn
                       (peek-next-word)
                       (read-label-name))
          :candidate (progn
                       (peek-next-word)
                       (read-register-reference))
          :guard     (progn
                       (peek-next-word)
                       (read-numeric-operand))))
      
      ((string= *current-word* "DEC")
        (peek-next-word)
        (make-dec-instruction :register
          (read-register-reference)))
      
      (T
        (make-nop-instruction)))))

;;; -------------------------------------------------------

(defun parse-next-instruction ()
  "Requests and parses the next token and returns an ``Instruction''
   representation of its content, if possible; otherwise responds with
   the ``NIL'' value to designate the *SOURCE*'s exhaustion."
  (peek-next-word)
  (the (or null Instruction)
    (unless *source-is-exhausted-p*
      (read-next-instruction))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of 8ial SOURCE code and returns a fresh ``Program''
   encapsulation of its entailed instructions."
  (declare (type string source))
  (set-scanner-source source)
  (the Program
    (make-instance 'Program :instructions
      (make-instruction-vector
        (loop
          for next-instruction
            of-type (or null Instruction)
            =       (parse-next-instruction)
          while next-instruction
            collect next-instruction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :accessor      label-table-entries
    :type          (hash-table-of string fixnum)
    :documentation "Maps the collected label names to their zero-based
                    instruction positions in the underlying 8ial
                    program."))
  (:documentation
    "The ``Label-Table'' class applies itself to the castaldy of labels
     in an 8ial program, affiliating the unique label names with their
     zero-based indices in the program's instruction sequence."))

;;; -------------------------------------------------------

(defun contains-label-p (labels name)
  "Determines whether the LABELS table comprehends a label amenable to
   the NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name
          (label-table-entries labels))))))

;;; -------------------------------------------------------

(defun define-label (labels name position)
  "Defines a new label in the LABELS table by associating the NAME with
   the zero-based POSITION of its occurrency in the underlying 8ial
   program and returns no value.
   ---
   Upon the detection of a label associated with the NAME among the
   extant LABELS, an error of the type ``Duplicate-Label-Error'' is
   signaled."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (declare (type fixnum      position))
  (if (contains-label-p labels name)
    (error 'Duplicate-Label-Error :name name)
    (setf (gethash name (label-table-entries labels)) position))
  (values))

;;; -------------------------------------------------------

(defun look-up-label (labels name)
  "Returns the zero-based position into the underlying 8ial program
   lending a commorancy to the definition of the label amenable to the
   NAME in the LABELS table.
   ---
   Upon a lacuna of a label for the NAME among the extant LABELS, an
   error of the type ``Undefined-Label-Error'' is signaled."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (the fixnum
    (or (gethash name (label-table-entries labels))
        (error 'Undefined-Label-Error :name name))))

;;; -------------------------------------------------------

(defun label-table-size (labels)
  "Returns the tally of entries in the LABELS table."
  (declare (type Label-Table labels))
  (the (integer 0 *)
    (hash-table-count
      (label-table-entries labels))))

;;; -------------------------------------------------------

(defun build-labels (program)
  "Creates and returns a fresh ``Label-Table'' for the 8ial PROGRAM."
  (declare (type Program program))
  (let ((labels (make-instance 'Label-Table)))
    (declare (type Label-Table labels))
    (loop
      for current-instruction
        of-type Instruction
        across  (program-instructions program)
      for current-position
        of-type fixnum
        from    0
        by      1
      when (label-definition-instruction-p current-instruction) do
        (define-label labels
          (label-definition-instruction-label current-instruction)
          current-position))
    (the Label-Table labels)))

;;; -------------------------------------------------------

(defmethod print-object ((labels Label-Table) (stream T))
  (declare (type Label-Table labels))
  (declare (type destination stream))
  (format stream "~&Label-Table with ~d entr~:@p:"
    (label-table-size labels))
  (loop
    for current-name
      of-type string
      being the hash-keys in (label-table-entries labels)
    using
      (hash-value current-position)
    and current-index
      of-type fixnum
      from    0
      by      1
    do
      (format stream "~&~2tNo. ~d: ~s => ~d"
        current-index current-name current-position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :reader        interpreter-program
    :type          Program
    :documentation "The 8ial instruction sequence to execute.")
   (labels
    :reader        interpreter-labels
    :type          Label-Table
    :documentation "Maps the defined label names to their zero-based
                    indices into the PROGRAM.")
   (ip
    :initform      0
    :accessor      interpreter-ip
    :type          fixnum
    :documentation "The current instruction pointer (IP) location.")
   (registers
    :initform      (make-array 16
                     :element-type    'octet
                     :initial-element 0
                     :adjustable      NIL
                     :fill-pointer    NIL)
    :reader        interpreter-registers
    :type          register-array
    :documentation "Maintains the memory's 16 unsigned-byte valued
                    registers as a vector."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     efficacy to an 8ial program furnished in the form of a sequence of
     instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Gathers the labels defined in the INTERPRETER's 8ial program, stores
   the same in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'labels)
    (build-labels
      (interpreter-program interpreter)))
  (values))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the 8ial program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Program program))
    (declare (type fixnum  ip))
    (the boolean
      (not (valid-program-index-p program ip)))))

;;; -------------------------------------------------------

(defun current-instruction (interpreter)
  "Returns the instruction located at the INTERPRETER'S contemporaneous
   instruction pointer (IP) in its program."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Program program))
    (declare (type fixnum  ip))
    (the Instruction
      (program-instruction-at program ip))))

;;; -------------------------------------------------------

(defun register-value (interpreter index)
  "Returns the integral value stored in the INTERPRETER's register
   amenable to the INDEX."
  (declare (type Interpreter     interpreter))
  (declare (type register-number index))
  (with-slots (registers) interpreter
    (declare (type register-array registers))
    (the integer
      (aref registers
        (1- index)))))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value interpreter index)
  "Stores the NEW-VALUE in the INTERPRETER register amenable to the
   INDEX and returns no value.."
  (declare (type integer         new-value))
  (declare (type Interpreter     interpreter))
  (declare (type register-number index))
  (with-slots (registers) interpreter
    (declare (type register-array registers))
    (setf (aref registers (1- index))
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Returns the integral value associated with the OPERAND in the
     INTERPRETER's context.")
  
  (:method ((interpreter Interpreter) (operand Literal-Operand))
    (declare (type Interpreter     interpreter))
    (declare (ignore               interpreter))
    (declare (type Literal-Operand operand))
    (the integer
      (literal-operand-value operand)))
  
  (:method ((interpreter Interpreter) (operand Register-Operand))
    (declare (type Interpreter      interpreter))
    (declare (type Register-Operand operand))
    (the integer
      (register-value interpreter
        (register-operand-index operand)))))

;;; -------------------------------------------------------

(defun operands-are-equal-p (interpreter first-operand second-operand)
  "Determines whether the FIRST-OPERAND and the SECOND-OPERAND, as
   resolved in the INTERPRETER's context, amount to an equal value,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type Operand     first-operand))
  (declare (type Operand     second-operand))
  (the boolean
    (get-boolean-value-of
      (= (resolve-operand interpreter first-operand)
         (resolve-operand interpreter second-operand)))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction INC-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type INC-Instruction instruction))
  (incf
    (register-value interpreter
      (register-operand-index
        (inc-instruction-register instruction))))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction END-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (ignore               interpreter))
  (declare (type END-Instruction instruction))
  (declare (ignore               instruction))
  (signal 'Halt-Condition)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction Label-Definition-Instruction))
  (declare (type Interpreter                  interpreter))
  (declare (ignore                            interpreter))
  (declare (type Label-Definition-Instruction instruction))
  (declare (ignore                            instruction))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction OUT-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type OUT-Instruction instruction))
  (format T "~&~d"
    (resolve-operand interpreter
      (out-instruction-register instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction JMP-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type JMP-Instruction instruction))
  (setf (interpreter-ip interpreter)
    (look-up-label
      (interpreter-labels    interpreter)
      (jmp-instruction-label instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction PUT-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type PUT-Instruction instruction))
  (setf
    (register-value interpreter
      (register-operand-index
        (put-instruction-register instruction)))
    (query-integer))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction JIR-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type JIR-Instruction instruction))
  (when (operands-are-equal-p interpreter
          (jir-instruction-candidate instruction)
          (jir-instruction-guard     instruction))
    (setf (interpreter-ip interpreter)
      (look-up-label
        (interpreter-labels    interpreter)
        (jir-instruction-label instruction))))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction DEC-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type DEC-Instruction instruction))
  (decf
    (register-value interpreter
      (register-operand-index
        (dec-instruction-register instruction))))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction NOP-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (ignore               interpreter))
  (declare (type NOP-Instruction instruction))
  (declare (ignore               instruction))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the 8ial program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (handler-case
    (loop until (program-is-completed-p interpreter) do
      (process-instruction interpreter
        (current-instruction interpreter))
      (incf (interpreter-ip interpreter)))
    (Halt-Condition ()
      NIL))
  (values))

;;; -------------------------------------------------------

(defun interpret-8ial (code)
  "Interprets the piece of 8ial source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-instance 'Interpreter :program
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-8ial "PUT $1 ;d JIR l $1 0 JIR l $1 1 DEC $1 JMP d ;l OUT $1 JIR l $1 1 END")

;;; -------------------------------------------------------

;; Numeric cat program which terminates on a zero (0) input.
(interpret-8ial ";repeat PUT $1 OUT $1 JIR x $1 0 JMP repeat ;x END")
