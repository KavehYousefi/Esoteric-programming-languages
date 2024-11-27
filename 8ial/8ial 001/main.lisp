;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "8ial", invented by the Esolang user "Ractangle" and
;; presented on September 15th, 2024, the kenspeckle haecceity of thilk
;; wones in its octuple instruction set, desumed in a conceptual
;; exercise of the conspectuity in the mimicry of an assembly language,
;; its competences' cynosure being the manipulation of integer objects
;; stored in registers and a stack, aided in this endeavour by a
;; label-based control flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The 8ial programming language simulates an assembly language tallying
;; eight instructions, chosen as warklumes for a register bank, selected
;; register, and stack's manipulation by adminiculum of integer objects.
;; 
;; == 8IAL: AN [8] [i]NSTRUCTION [a]SSEMBLY [l]ANGUAGE ==
;; The 8ial programming language's agnomination engages in a bewrayment
;; of its operative circumference and its designment's entheus, this
;; being an "8 instruction assembly language".
;; 
;; == THE MEMORY: REGISTER BANK, POINTED REGISTER, AND STACK ==
;; 8ial intrines for its data castaldy, thilk always appertains to
;; signed integers of any magnitude, a register bank of theoretically
;; infinite componency, a particular active, or "pointed", register, and
;; a stack with no upper bourne in its mickleness.
;; 
;; == REGISTERS: NEVENED BY NUMERICS ==
;; A register reference's syntactical indicium manifests in a
;; parasceuastic dollar sign, "$", whence follows an unsigned positive
;; integer identifier.
;; 
;; 
;; Syntax
;; ======
;; From a conspection's exercise on its syntaxis, the 8ial programming
;; language's conformation limns an ordered sequence of zero or more
;; lines, everichon among these, if not blank or a lone comment's
;; woning, accommodate an aefauld instruction, thilk's identifier
;; is succeeded by zero through three operands.
;; 
;; == PROGRAMS: LINE OF INSTRUCTIONS ==
;; Every line not administered a blank composition or a commentary
;; dever only, bears a single instruction's causatum.
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
;; number.
;; 
;; == INTEGER LITERALS: SIGNED OR UNSIGNED DECIMAL NUMBERS ==
;; Integer literals may assume signed or unsigned decimal numbers
;; endowed with bourneless mickleness along both axes.
;; 
;; == COMMENTS ==
;; The 8ial programming language homologates the incarnation of comments
;; as constituents of either a line of reserved dedication, or a
;; compernage to the posterior of a prevenient instruction statement.
;; Such a descant's introduction ensues from an ecphoneme's ("!")
;; adminicular agency, whence any content may emerge until the carrying
;; line's desinence.
;; 
;; == GRAMMAR ==
;; The 8ial programming language's donet shall enjoy an augmented ilk
;; of formality's dation by mediation of an Extended Backus-Naur Form
;; (ENBF) description:
;; 
;;   program         := { innerLine } , [ finalLine ] ;
;;   innerLine       := [ command ] , [ comment ] , newlines ;
;;   finalLine       := [ command ] , [ comment ] ;
;;   
;;   comment         := "!" , { character - newline } ;
;;   command         := evrCommand
;;                   |  jirCommand
;;                   |  labelDefinition
;;                   |  outCommand
;;                   |  ptrCommand
;;                   |  pshCommand
;;                   |  putCommand
;;                   ;
;;   
;;   evrCommand      := "EVR" , numericOperand ;
;;   jirCommand      := "JIR" , labelName , registry , numericOperand ;
;;   labelDefinition := ";" , labelName ;
;;   outCommand      := "OUT" ;
;;   ptrCommand      := "PTR" , registryName ;
;;   pshCommand      := "PSH" , registryName ;
;;   putCommand      := "PUT" , registryName ;
;;   
;;   labelName       := labelCharacter , { labelCharacter } ;
;;   labelCharacter  := digit | letter | "-" | "_" ;
;;   
;;   numericOperand  := signedInteger | registryName ;
;;   registryName    := "$" , unsignedInteger ;
;;   signedInteger   := [ "+" | "-" ] , unsignedInteger ;
;;   unsignedInteger := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   letter          := "a" | ... | "z" | "A" | ... | "Z" ;
;;   newlines        := newline , { newline } ;
;;   newline         := "\n" ;
;;   space           := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; 8ial's operative competences are begotten by an octuple instruction
;; set, the compass of which enumerates warklumes for the register and
;; stack manipulation, numeric input and output conduits, as well as a
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
;;   PSH registry             | Pushes the value of the {registry} onto
;;       ********             | the stack.
;;   ..................................................................
;;   PUT registry             | Queries the standard input for a signed
;;       ********             | or unsigned integer number and stores
;;                            | the response in the {registry}.
;;   ..................................................................
;;   OUT                      | Pops the top stack element and prints
;;                            | the same in its verbatim numeric form
;;                            | to the standard output.
;;                            |----------------------------------------
;;                            | If the stack is empty at the instant
;;                            | of this operation's invocation, an
;;                            | error of the type "EmptyStackError" is
;;                            | signaled.
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
;;   JIR label registry guard | If the value of the {registry} equals
;;       ***** ******** ***** | the {guard}, relocates the instruction
;;                            | pointer (IP) to the position of the
;;                            | label designated by the {labelName};
;;                            | otherwise accompasses no causatum.
;;                            |----------------------------------------
;;                            | The {guard} must either be a literal
;;                            | integer number or a registry name.
;;                            |----------------------------------------
;;                            | If the {label} cannot be retrieved in
;;                            | the program, an error of the type
;;                            | "UndefinedLabelError" is signaled.
;;   ..................................................................
;;   PTR registry             | Designates the registry with the name
;;       ********             | {registry} as the currently active
;;                            | one.
;;   ..................................................................
;;   EVR newValue             | Stores the {newValue} in the currently
;;       ********             | selected registry.
;;                            |----------------------------------------
;;                            | The {newValue} must either be a literal
;;                            | integer number or a registry name.
;;                            |----------------------------------------
;;                            | If no registry is currently selected,
;;                            | an error of the type
;;                            | "NoPointedRegistryError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A few inroads of dubiety inflict the 8ial protolog, whence provenance
;; a selected parcel shall be extracted for an express disquisition.
;; 
;; == DO ERRONEOUS INSTRUCTIONS TRANSMOGRIFY TO COMMENTS? ==
;; As an instance of supererogation besides the dedicated comment
;; syntax, the source [esolang2024comment] parlays of commands
;; invalidated in their conformance with the governing stipulations as
;; construed in the sense of a commentary supplement; the language's
;; main document, [esolang2024_8ial], abstains from this claim's
;; replication.
;; 
;; It has been adjudged, with the adminicular lacuna of the paravail
;; command-to-comment transcription in the protolog, and the fact of
;; the crebritude by which the 8ial standard has changed in the
;; preterite chronology, to excise this option, retaining merely the
;; explicit comment species, introduced via an ecphoneme ("!"), as a
;; canonical proprium.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-11-25
;; 
;; Sources:
;;   [esolang2024_8ial]
;;   The Esolang contributors, "8ial", October 5th, 2024
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

(deftype registry-number ()
  "The ``registry-number'' type defines a registry identifier as an
   integral number greater than or equal to one (1), but without a
   natural bourne towards the upper extremum, thus being a commorant of
   the integer range [1, +infinity]."
  '(integer 1 *))

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

(defstruct (Registry-Operand
  (:include Operand))
  "The ``Registry-Operand'' class represents a numeric registry
   identifier specified as an 8ial instruction operand."
  (name (error "Missing registry operand identifier.")
        :type      registry-number
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' interface establishes a common foundry for all
   classes pursuing the representation of an 8ial operation invocation,
   this diorism's circumference incorporating the arguments.")

;;; -------------------------------------------------------

(defstruct (Edit-Instruction
  (:include Instruction))
  "The ``Edit-Instruction'' class serves in the encapsulation of an
   8ial \"EVR\" operation, nuncupated to the selected registry's
   modulation."
  (new-value (error "Missing Edit-Instruction value.")
             :type      Operand
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Input-Instruction
  (:include Instruction))
  "The ``Input-Instruction'' class serves in the encapsulation of an
   8ial \"PUT\" operation, nuncupated to a specified registry value's
   reception of data from the standard input."
  (registry (error "Missing Input-Instruction registry name.")
            :type      Registry-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Jump-Instruction
  (:include Instruction))
  "The ``Jump-Instruction'' class serves in the encapsulation of an
   8ial \"JIR\" operation, nuncupated to the program instruction
   pointer's (IP) conditional relocation to a specific label upon a
   given registry value's equiparation with a guard."
  (label     (error "Missing Jump-Instruction label name.")
             :type      string
             :read-only T)
  (candidate (error "Missing Jump-Instruction candidate registry.")
             :type      Registry-Operand
             :read-only T)
  (guard     (error "Missing Jump-Instruction guard.")
             :type      Operand
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Definition-Instruction
  (:include Instruction))
  "The ``Label-Definition-Instruction'' class encapsulates the behest
   involving the definition of a label in an 8ial program."
  (name (error "Missing Label-Definition-Instruction name.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Instruction
  (:include Instruction))
  "The ``Output-Instruction'' class serves in the encapsulation of an
   8ial \"OUT\" operation, nuncupated to the program stack top element's
   display on the standard output.")

;;; -------------------------------------------------------

(defstruct (Point-Instruction
  (:include Instruction))
  "The ``Point-Instruction'' class serves in the encapsulation of an
   8ial \"PTR\" operation, nuncupated to a specified registry's
   selection as the currently active unit."
  (registry (error "Missing Point-Instruction registry name.")
            :type      Registry-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Push-Instruction
  (:include Instruction))
  "The ``Push-Instruction'' class serves in the encapsulation of an
   8ial \"PSH\" operation, nuncupated to a specified registry value's
   insertion on the program stack."
  (registry (error "Missing Push-Instruction registry name.")
            :type      Registry-Operand
            :read-only T))



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
;; -- Implementation of program stack.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ((elements
    :initform      NIL
    :accessor      stack-elements
    :type          (list-of integer)
    :documentation "A list representing the last in, first out (LIFO)
                    store of integers."))
  (:documentation
    "The ``Stack'' class implements an unbounded stack of signed integer
     numbers."))

;;; -------------------------------------------------------

(defun stack-size (stack)
  "Returns the tally of elements comprising the STACK."
  (declare (type Stack stack))
  (the (integer 0 *)
    (length
      (stack-elements stack))))

;;; -------------------------------------------------------

(defun stack-is-empty-p (stack)
  "Determines whether the STACK is destitute of any elements, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (null
      (stack-elements stack))))

;;; -------------------------------------------------------

(defun push-onto-stack (stack new-element)
  "Pushes the NEW-ELEMENT onto the STACK's top and returns no value."
  (declare (type Stack stack))
  (push new-element
    (stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (stack)
  "Removes and returns the STACK's top element.
   ---
   If the STACK is empty at this operation's invocation, an error of the
   type ``Empty-Stack-Error'' is signaled."
  (declare (type Stack stack))
  (the integer
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error)
      (pop (stack-elements stack)))))

;;; -------------------------------------------------------

(defmethod print-object ((stack Stack) (stream T))
  (declare (type Stack       stack))
  (declare (type destination stream))
  (format stream "~&Stack with ~d element~:p:"
    (stack-size stack))
  (format stream "~{~&~2t~d~}"
    (stack-elements stack)))



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

(define-condition Label-Error (8ial-error)
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

(define-condition No-Pointed-Registry-Error (8ial-Error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type No-Pointed-Registry-Error condition))
      (declare (ignore                         condition))
      (declare (type destination               stream))
      (format stream "No pointed registry has been selected.")))
  (:documentation
    "The ``No-Pointed-Registry-Error'' condition type serves in the
     apprizal about the attempt to query or modify an 8ial program's
     pointed registry without a prevenient selection of such."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (8ial-Error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot peek into or pop from an empty stack.")))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves to apprize about
     an anomalous situation whose etiology emerges from the attempt to
     peek into or pop from an empty stack."))

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
;; -- Implementation of registry identifier operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-registry-number (identifier)
  "Determines whether the numeric IDENTIFIER represents a valid registry
   name, which ought to constitute a positive integer number, returning
   on confirmation the IDENTIFIER in its ipsissima verba guise;
   otherwise signals an error of an unspecified type."
  (declare (type integer identifier))
  (the integer
    (or (and (plusp identifier) identifier)
        (error "The value ~d does not represent a valid registry ~
                number, as the same must be a positive integer ~
                greater than or equal to one."
          identifier))))



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
          (find candidate "_-" :test #'char=)))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent homologated
   to appear in a word, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (not (space-character-p candidate))
        (char/= candidate #\!)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global scanner variables.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum    *current-line-index*))
(declaim (type string    *current-line*))
(declaim (type fixnum    *current-line-length*))
(declaim (type fixnum    *current-column-index*))
(declaim (type character *current-character*))
(declaim (type boolean   *current-line-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *current-line-index* -1
  "The zero-based index of the currently processed line.")

(defparameter *current-line* ""
  "The currently processed line's content as a string.")

(defparameter *current-column-index* 0
  "The zero-based index into the *CURRENT-LINE*.")

;;; -------------------------------------------------------

(define-symbol-macro *current-line-length*
  (the fixnum
    (length *current-line*)))

(define-symbol-macro *current-character*
  (the character
    (if (array-in-bounds-p *current-line* *current-column-index*)
      (char *current-line* *current-column-index*)
      #\Null)))

(define-symbol-macro *current-line-is-exhausted-p*
  (the boolean
    (get-boolean-value-of
      (>= *current-column-index*
          *current-line-length*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-scanner ()
  "Restores the scanner's incipial configurations and returns no value."
  (psetf
    *current-line-index*   -1
    *current-line*         ""
    *current-column-index* 0)
  (values))

;;; -------------------------------------------------------

(defun assign-next-line-index ()
  "Returns the subsequent line index."
  (incf *current-line-index*)
  (the fixnum *current-line-index*))

;;; -------------------------------------------------------

(defun set-current-line (new-line)
  "Configures the scanner in a fashion which accepts the NEW-LINE as
   the currently processed one and returns no value."
  (declare (type string new-line))
  (psetf
    *current-line-index*   (assign-next-line-index)
    *current-line*         new-line
    *current-column-index* 0)
  (values))

;;; -------------------------------------------------------

(defun conclude-current-line ()
  "Relocates the *CURRENT-COLUMN-INDEX* to the *CURRENT-LINE*'s desinent
   position and returns no value."
  (setf *current-column-index* *current-line-length*)
  (values))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   skips a sequence of zero or more accolent spaces and returns no
   value."
  (setf *current-column-index*
    (or (position-if-not #'space-character-p *current-line*
          :start *current-column-index*)
        *current-line-length*))
  (values))

;;; -------------------------------------------------------

(defun comment-starts-p ()
  "Determines whether, proceeding from the *CURRENT-COLUMN-INDEX* into
   the *CURRENT-LINE*, a comment section commences, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (get-boolean-value-of
      (and (not *current-line-is-exhausted-p*)
           (char= *current-character* #\!)))))

;;; -------------------------------------------------------

(defun skip-optional-comment ()
  "Determines whether, proceeding from the *CURRENT-COLUMN-INDEX* into
   the *CURRNET-LINE*, a comment section commences, on confirmation
   relocating the *COLUMN-COLUMN-INDEX* to the *CURRENT-LINE*'s
   desinence, otherwise administering no causatum, and in any case
   returns no value."
  (when (comment-starts-p)
    (conclude-current-line))
  (values))

;;; -------------------------------------------------------

(defun expect-end-of-current-line ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   determines whether the latter is destitute of any effective content,
   admitting exclusively spaces and an optional comment segment, on
   confirmation returning no value; otherwise an error of an unspecified
   type is signaled."
  (skip-spaces)
  (skip-optional-comment)
  (unless *current-line-is-exhausted-p*
    (error "Expected the line number ~d to conclude, but encountered ~
            the character \"~c\" at position ~d."
      *current-line-index*
      *current-character*
      *current-column-index*))
  (values))

;;; -------------------------------------------------------

(defun locate-end-of-word ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   returns the position of the nearest space character."
  (the fixnum
    (or (position-if-not #'word-character-p *current-line*
          :start *current-column-index*)
        *current-line-length*)))

;;; -------------------------------------------------------

(defun peek-next-word ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   peeks the next word, demarcated either by a succeeding space or the
   end of the line, and returns three values:
     (1) A fresh string representing the detected word.
     (2) The start position of this word, which conflates with the
         *CURRENT-COLUMN-INDEX*.
     (3) The position into the *CURRENT-LINE* immediately succeeding the
         detected word.
   ---
   The scanner's state will not be modified by this operation."
  (let ((start-position *current-column-index*)
        (end-position   (locate-end-of-word)))
    (declare (type fixnum start-position))
    (declare (type fixnum end-position))
    (the (values string fixnum fixnum)
      (values
        (subseq *current-line* start-position end-position)
        start-position
        end-position))))

;;; -------------------------------------------------------

(defun registry-reference-follows-p ()
  "Determines whether the character at the *CURRENT-LINE-INDEX* into
   the *CURRENT-LINE* introduces a registry reference, being equal to
   the \"$\" sign, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (the boolean
    (get-boolean-value-of
      (char= *current-character* #\$))))

;;; -------------------------------------------------------

(defun expect-dollar-sign ()
  "Determines whether the character at the *CURRENT-COLUMN-INDEX* into
   the *CURRENT-LINE* introduces a registry reference, being equal to
   the \"$\" sign, on confirmation returning no value; otherwise an
   error of an unspecified type is signaled."
  (unless (registry-reference-follows-p)
    (error "Expected the character \"$\" to introduce a registry ~
            name, but encountered the symbol \"~c\" in column ~d ~
            of line ~d."
      *current-character* *current-column-index* *current-line-index*))
  (values))

;;; -------------------------------------------------------

(defun read-number ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads an signed or unsigned integer number and returns thilk."
  (multiple-value-bind (next-word start-position end-position)
      (peek-next-word)
    (declare (type string next-word))
    (declare (type fixnum start-position))
    (declare (ignore      start-position))
    (declare (type fixnum end-position))
    (the integer
      (prog1
        (handler-case
          (parse-integer next-word)
          (error ()
            (error "The token ~s in column ~d of line ~d does not ~
                    represent an integral number."
              next-word *current-column-index* *current-line-index*)))
        (setf *current-column-index* end-position)))))

;;; -------------------------------------------------------

(defun read-integer-literal ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads a signed or unsigned integer literal and returns a connable
   representation thereof."
  (the Literal-Operand
    (make-literal-operand :value
      (read-number))))

;;; -------------------------------------------------------

(defun read-registry-reference ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads a registry reference and returns a connable representation
   thereof."
  (expect-dollar-sign)
  (incf *current-column-index*)
  (the Registry-Operand
    (make-registry-operand :name
      (validate-registry-number
        (read-number)))))

;;; -------------------------------------------------------

(defun read-numeric-operand ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads either a registry reference or a numeric literal and returns
   a covenable representation thereof."
  (the Operand
    (if (registry-reference-follows-p)
      (read-registry-reference)
      (read-integer-literal))))

;;; -------------------------------------------------------

(defun read-label-name ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads and returns a label name."
  (multiple-value-bind (next-word start-position end-position)
      (peek-next-word)
    (declare (type string next-word))
    (declare (type fixnum start-position))
    (declare (ignore      start-position))
    (declare (type fixnum end-position))
    (the string
      (prog1
        (validate-label-name next-word)
        (setf *current-column-index* end-position)))))

;;; -------------------------------------------------------

(defun read-instruction ()
  "Proceeding from the *CURRENT-COLUMN-INDEX* into the *CURRENT-LINE*,
   reads an 8ial instruction and returns a covenable representation
   thereof."
  (multiple-value-bind (next-word start-position end-position)
      (peek-next-word)
    (declare (type string next-word))
    (declare (type fixnum start-position))
    (declare (type fixnum end-position))
    
    (the Instruction
      (prog1
        (cond
          ((char= *current-character* #\;)
            (incf *current-column-index*)
            (make-label-definition-instruction :name
              (read-label-name)))
          
          ((string= next-word "EVR")
            (setf *current-column-index* end-position)
            (skip-spaces)
            (make-edit-instruction :new-value
              (read-numeric-operand)))
          
          ((string= next-word "OUT")
            (setf *current-column-index* end-position)
            (make-output-instruction))
          
          ((string= next-word "PSH")
            (setf *current-column-index* end-position)
            (skip-spaces)
            (make-push-instruction :registry
              (read-registry-reference)))
          
          ((string= next-word "PTR")
            (setf *current-column-index* end-position)
            (skip-spaces)
            (make-point-instruction :registry
              (read-registry-reference)))
          
          ((string= next-word "PUT")
            (setf *current-column-index* end-position)
            (skip-spaces)
            (make-input-instruction :registry
              (read-registry-reference)))
          
          ((string= next-word "JIR")
            (setf *current-column-index* end-position)
            (skip-spaces)
            (make-jump-instruction
              :label
                (prog1
                  (read-label-name)
                  (skip-spaces))
              :candidate
                (prog1
                  (read-registry-reference)
                  (skip-spaces))
              :guard
                (prog1
                  (read-numeric-operand)
                  (skip-spaces))))
          
          (T
            (error "The identifier ~s, inchoating in the column ~d of ~
                    line number ~d does not introduce an instruction."
              next-word start-position *current-line-index*)))
        
        (expect-end-of-current-line)))))

;;; -------------------------------------------------------

(defun parse-current-line ()
  "Parses the *CURRENT-LINE* and returns an ``Instruction''
   representation of its content, if possible; otherwise responds with
   the ``NIL'' value to designate a no-operation (NOP)."
  (skip-spaces)
  (skip-optional-comment)
  (the (or null Instruction)
    (unless *current-line-is-exhausted-p*
      (read-instruction))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of 8ial SOURCE code and returns a fresh ``Program''
   encapsulation of its entailed instructions."
  (declare (type string source))
  (reset-scanner)
  (flet ((prepare-instruction-for-appendage (instruction)
          "Prepares the INSTRUCTION for a gathering via a ``loop'' macro
           invocation's ``append'' action by wrapping the same in a
           singleton list, if non-``NIL'', otherwise, for a ``NIL''
           INSTRUCTION, returns the ``NIL'' sentinel itself."
          (declare (type (or null Instruction) instruction))
          (the (or null (cons Instruction null))
            (when instruction
              (list instruction)))))
    (with-input-from-string (source-stream source)
      (declare (type string-stream source-stream))
      (the Program
        (make-instance 'Program :instructions
          (make-instruction-vector
            (loop
              for source-line
                of-type (or null string)
                =       (read-line source-stream NIL NIL)
              while source-line append
                (progn
                  (set-current-line source-line)
                  (prepare-instruction-for-appendage
                    (parse-current-line))))))))))



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
     in a 8ial program, affiliating the unique label names with their
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
          (label-definition-instruction-name current-instruction)
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
;; -- Implementation of registry bank.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Registry-Bank ()
  ((entries
    :initform      (make-hash-table :test #'eql)
    :accessor      registry-bank-entries
    :type          (hash-table-of registry-number integer)
    :documentation "Affiliates the numeric registry identifiers with
                    their values."))
  (:documentation
    "The ``Registry-Bank'' class is apportioned the dever of an 8ial
     program registries' castaldy."))

;;; -------------------------------------------------------

(defun ensure-registry-entry (registries name)
  "Ascertains the existency of a registry amenable to the NAME in the
   REGISTIRES bank by either returning its value, if already present, or
   inserting a new zero-valued entry upon its absence, ere this datum
   is produced."
  (declare (type Registry-Bank   registries))
  (declare (type registry-number name))
  (with-slots (entries) registries
    (declare (type (hash-table-of registry-number integer) entries))
    (multiple-value-bind (extant-value contains-name-p)
        (gethash name entries)
      (declare (type (or null integer) extant-value))
      (declare (type T                 contains-name-p))
      (the integer
        (if contains-name-p
          extant-value
          (prog1 0
            (setf (gethash name entries) 0)))))))

;;; -------------------------------------------------------

(defun read-registry (registries name)
  "Returns the integer value stored in the registry amenable to the NAME
   in the REGISTRIES bank.
   ---
   A registry not yet explicitly set responds with the default value of
   zero (0)."
  (declare (type Registry-Bank   registries))
  (declare (type registry-number name))
  (the integer
    (ensure-registry-entry registries name)))

;;; -------------------------------------------------------

(defun write-registry (registries name new-value)
  "Stores the NEW-VALUE in the registry amenable to the NAME in the
   REGISTRIES bank and returns no value."
  (declare (type Registry-Bank   registries))
  (declare (type registry-number name))
  (declare (type integer         new-value))
  (with-slots (entries) registries
    (declare (type (hash-table-of registry-number integer) entries))
    (setf (gethash name entries 0) new-value))
  (values))

;;; -------------------------------------------------------

(defun registry-bank-size (registries)
  "Returns the tally of entries in the REGISTRIES bank."
  (declare (type Registry-Bank registries))
  (the (integer 0 *)
    (hash-table-count
      (registry-bank-entries registries))))

;;; -------------------------------------------------------

(defmethod print-object ((registries Registry-Bank) (stream T))
  (declare (type Registry-Bank registries))
  (declare (type destination   stream))
  (format stream "~&Registry-Bank with ~d entr~:@p:"
    (registry-bank-size registries))
  (loop
    for current-name
      of-type registry-number
      being the hash-keys in (registry-bank-entries registries)
    using
      (hash-value current-value)
    do
      (format stream "~&~2t~d => ~d" current-name current-value)))



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
   (registries
    :initform      (make-instance 'Registry-Bank)
    :reader        interpreter-registries
    :type          Registry-Bank
    :documentation "Maps registry names to their integer values.")
   (stack
    :initform      (make-instance 'Stack)
    :reader        interpreter-stack
    :type          Stack
    :documentation "Maintains an unbounded stack of integer numbers.")
   (pointed-registry
    :initform      NIL
    :accessor      interpreter-pointed-registry
    :type          (or null registry-number)
    :documentation "The currently selected registry's identifier."))
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

(defun registry-value (interpreter name)
  "Returns the integral value stored in the INTERPRETER's registry
   amenable to the NAME."
  (declare (type Interpreter     interpreter))
  (declare (type registry-number name))
  (with-slots (registries) interpreter
    (declare (type Registry-Bank registries))
    (the integer
      (read-registry registries name))))

;;; -------------------------------------------------------

(defun (setf registry-value) (new-value interpreter name)
  "Stores the NEW-VALUE in the INTERPRETER registry amenable to the
   NAME and returns no value.."
  (declare (type integer         new-value))
  (declare (type Interpreter     interpreter))
  (declare (type registry-number name))
  (with-slots (registries) interpreter
    (declare (type Registry-Bank registries))
    (write-registry registries name new-value))
  (values))

;;; -------------------------------------------------------

(defun confirm-pointed-registry-selected (interpreter)
  "Determines whether a pointed registry has been specified in the
   INTERPRETER's context, returning on confirmation no value; otherwise
   an error of the type ``No-Pointed-Registry-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (unless (interpreter-pointed-registry interpreter)
    (error 'No-Pointed-Registry-Error))
  (values))

;;; -------------------------------------------------------

(defun pointed-registry-value (interpreter)
  "Returns the ineger value stored in the INTERPRETER's pointer
   registry.
   ---
   If no pointed registry has yet been chosen, an error of the type
   ``No-Pointed-Registry-Error'' will be signaled."
  (declare (type Interpreter interpreter))
  (confirm-pointed-registry-selected interpreter)
  (the integer
    (registry-value interpreter
      (interpreter-pointed-registry interpreter))))

;;; -------------------------------------------------------

(defun (setf pointed-registry-value) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER's pointed registry and
   returns no value.
   ---
   If no pointed registry has yet been chosen, an error of the type
   ``No-Pointed-Registry-Error'' will be signaled."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (confirm-pointed-registry-selected interpreter)
  (with-slots (pointed-registry) interpreter
    (declare (type (or null registry-number) pointed-registry))
    (setf (registry-value interpreter pointed-registry) new-value))
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
  
  (:method ((interpreter Interpreter) (operand Registry-Operand))
    (declare (type Interpreter      interpreter))
    (declare (type Registry-Operand operand))
    (the integer
      (registry-value interpreter
        (registry-operand-name operand)))))

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
                                (instruction Edit-Instruction))
  (declare (type Interpreter      interpreter))
  (declare (type Edit-Instruction instruction))
  (setf (pointed-registry-value interpreter)
    (resolve-operand interpreter
      (edit-instruction-new-value instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Input-Instruction))
  (declare (type Interpreter       interpreter))
  (declare (type Input-Instruction instruction))
  (setf (registry-value interpreter
          (registry-operand-name
            (input-instruction-registry instruction)))
    (query-integer))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Jump-Instruction))
  (declare (type Interpreter      interpreter))
  (declare (type Jump-Instruction instruction))
  (when (operands-are-equal-p interpreter
          (jump-instruction-candidate instruction)
          (jump-instruction-guard     instruction))
    (setf (interpreter-ip interpreter)
      (look-up-label
        (interpreter-labels     interpreter)
        (jump-instruction-label instruction))))
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
                                (instruction Output-Instruction))
  (declare (type Interpreter        interpreter))
  (declare (type Output-Instruction instruction))
  (declare (ignore                  instruction))
  (format T "~&~d"
    (pop-from-stack
      (interpreter-stack interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Point-Instruction))
  (declare (type Interpreter       interpreter))
  (declare (type Point-Instruction instruction))
  (setf (interpreter-pointed-registry interpreter)
    (registry-operand-name
      (point-instruction-registry instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Push-Instruction))
  (declare (type Interpreter      interpreter))
  (declare (type Push-Instruction instruction))
  (push-onto-stack
    (interpreter-stack interpreter)
    (resolve-operand interpreter
      (push-instruction-registry instruction)))
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

;; Perpetually repeating numeric cat program.
(interpret-8ial
  ";repeat
   PUT $1
   PSH $1
   OUT
   JIR repeat $2 0")

;;; -------------------------------------------------------

;; Repeating cat program which demonstrates the program termination upon
;; the input conduit's exhaustion.
(with-input-from-string (input "")
  (declare (type string-stream input))
  (let ((*standard-input* input))
    (interpret-8ial
      ";repeat
       PUT $1
       PSH $1
       OUT
       JIR repeat $1 0")))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-8ial
  "
  PUT $1
  JIR o $1 1
  PSH $1
  OUT
  JIR e $1 0
  ;o
  PSH $1
  OUT
  JIR o $1 1
  ;e   
  ")

;;; -------------------------------------------------------

;; Select the register "$1", set its value to 100, and print the
;; this content.
(interpret-8ial
  "PTR $1
   EVR 100
   PSH $1
   OUT")
