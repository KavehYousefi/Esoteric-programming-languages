;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Counterlang", invented by the Esolang user "BestCoder" and
;; presented on February 18th, 2024, its haecceity manifesting in the
;; creation and modulation of signed integer-valued counters, operating
;; in champarty with input/output conduits and a line-based goto
;; construct.
;; 
;; 
;; Concept
;; =======
;; The Counterlang programming language's firmament is realized in the
;; perquisition and modification of signed integer-valued counters.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical application of the conspectuity, a Counterlang
;; program's conformation ostends a sequence of zero or more lines,
;; everichon dedicated to zero or one instruction's accommodation.
;; 
;; == PROGRAMS: LINES OF INSTRUCTIONS ==
;; A Counterlang program enumerates zero or more lines, either
;; comprehending an aefauld operation invocation, or producing no
;; effective content, optionally in both cases homologated to conclude
;; with an optional comment.
;; 
;; == INSTRUCTIONS ==
;; An instruction, ensuing from the Counterlang programming language's
;; definition, either constitutes a counter identifier succeeded by
;; a modulation tmema, or an operative sentinel prevenient to the
;; referenced operand.
;; 
;; == OPERANDS: INTEGER LITERALS OR COUNTER NAMES ==
;; An instruction operand's variation bifurcates into a signed decimal
;; literal or a counter name, the latter's conformation limns a sequence
;; of one or more Latin letters.
;; 
;; == WHITESPACES ==
;; The participation of spaces and horizontal tabs constitutes, with the
;; "count" instruction identifier's segregation from the counter name
;; posing as the sole exemption, an optional insertion.
;; 
;; Newline entities, on the other hand, account for an imperative
;; element betwixt instruction lines, and as an interdiction in any
;; other intermede.
;; 
;; == COMMENTS ==
;; A provision for comments partakes of the language by adminiculum of
;; a parasceuastic hash sign, "#", the dispansion of which extends until
;; the end of the contemporaneous line.
;; 
;; == GRAMMAR ==
;; An augmented mete of stringency's dation shall be vouchsafed by the
;; following Extended Backus-Naur Form (EBNF) description:
;; 
;;   program          := { innerLine } , [ lastLine ] ;
;;   innerLine        := lineContent , newlines ;
;;   lastLine         := lineContent ;
;;   lineContent      := padding , [ command ] , [ comment ] ;
;;   comment          := "#" , { character - newline } ;
;;   command          := initCommand
;;                    |  addCommand
;;                    |  resetCommand
;;                    |  printCharCommand
;;                    |  printNumCommand
;;                    |  jumpCommand
;;                    |  haltCommand
;;                    ;
;;   initCommand      := "count" , spacing , counterName ;
;;   addCommand       := counterName , operand ;
;;   resetCommand     := counterName , "-" ;
;;   printCharCommand := "," , operand ;
;;   printNumCommand  := "." , operand ;
;;   jumpCommand      := "!" , operand ;
;;   haltCommand      := ":" ;
;;   
;;   operand          := signedInteger | counterName ;
;;   signedInteger    := [ "+" | "-" ] , unsignedInteger ;
;;   unsignedInteger  := digit , { digit } ;
;;   counterName      := letter , { letter } ;
;;   
;;   padding          := { space } ;
;;   spacing          := space , { space } ;
;;   newlines         := newline , { newline } ;
;;   newline          := "\n" ;
;;   space            := " " ;
;; 
;; 
;; Instructions
;; ============
;; A septuple componency exhausts Counterlang's instruction set, the
;; perimeter of which admitting basic arithmetics, input and output
;; facilities, an unconditional goto mechanism, as well as a termination
;; behest.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be the administration of a
;; fundamental mete of nortelry with the Counterlang language's
;; operative competences.
;; 
;; Please heed the demarcation of succedaneous tmema by adminculum of
;; an amplecting jumelle of braces, "{" and "}", intended for their
;; substitution by actual Counterlang code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   count {name}   | Creates a new counter amenable to the {name}, its
;;                  | incipial value registered as zero (0).
;;                  |--------------------------------------------------
;;                  | {name} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | If a counter answering to the {name} already
;;                  | exists, an error of the type
;;                  | "DuplicateCounterNameError" is signaled.
;;   ..................................................................
;;   {name}{number} | Adds the {number} to the value of the counter
;;                  | amenable to the {name}.
;;                  |--------------------------------------------------
;;                  | {name} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | {number} must be a signed or unsigned integer
;;                  | number.
;;                  |--------------------------------------------------
;;                  | If no counter answering to the {name} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   {name1}{name2} | Adds the value of the counter amenable to the
;;                  | name {name2} to that of the counter designated by
;;                  | the identifier {name1}.
;;                  |--------------------------------------------------
;;                  | {name1} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | {name2} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | If no counter answering to {name1} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;                  |--------------------------------------------------
;;                  | If no counter answering to {name2} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   {name}-        | Resets the counter amenable to the {name} to its
;;                  | inchoate state of zero (0).
;;                  |--------------------------------------------------
;;                  | If no counter answering to the {name} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   .{number}      | Prints the {number} in its verbatim numeric form
;;                  | to the standard output.
;;                  |--------------------------------------------------
;;                  | {number} must be a signed or unsigned integer
;;                  | number.
;;   ..................................................................
;;   .{name}        | Prints the value stored in the counter amenable
;;                  | to the {name} in its verbatim numeric form to the
;;                  | standard output.
;;                  |--------------------------------------------------
;;                  | {name} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | If no counter answering to the {name} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   ,{charCode}    | Prints the character whose ASCII code corresponds
;;                  | to the numeric {charCode} to the standard output.
;;                  |--------------------------------------------------
;;                  | {charCode} must be a non-negative integer number
;;                  | greater than or equal to zero (0).
;;   ..................................................................
;;   ,{name}        | Prints the character whose ASCII code corresponds
;;                  | to the numeric value stored in the counter
;;                  | amenable to the {name} to the standard output.
;;                  |--------------------------------------------------
;;                  | {name} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | If no counter answering to the {name} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   !{lineNumber}  | Relocates the instruction pointer (IP) to the
;;                  | one-indexed line number in the program.
;;                  |--------------------------------------------------
;;                  | {lineNumber} must be a positive integer number
;;                  | greater than or equal to one (1).
;;   ..................................................................
;;   !{name}        | Relocates the instruction pointer (IP) to the
;;                  | one-indexed line number in the program.
;;                  |--------------------------------------------------
;;                  | {name} must be a valid counter identifier.
;;                  |--------------------------------------------------
;;                  | If no counter answering to the {name} exists, an
;;                  | error of the type "UnboundCounterNameError" is
;;                  | signaled.
;;   ..................................................................
;;   :              | Immediately terminates the program. Any
;;                  | succeedent instruction will be ignored.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been provided in Common Lisp,
;; replicating the epiphenomenal Counterlang command lines in dedicated
;; ``Instruction'' object equivalents in order to accompass the
;; requisite causata.
;; 
;; == A BESPOKE FUNCTION SYNTAX ==
;; A didascalic component is accommodated a woning in this project, the
;; entelechy an alternative function definition infrastructure,
;; superimposed upon the autochthonous Common Lisp operator ``defun''
;; as a macro's product, and entalented with the telos of augmented
;; comfort in the declaration of the function return and formal
;; parameter types.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-09-17
;; 
;; Sources:
;;   [esolang2024Counterlang]
;;   The Esolang contributors, "Counterlang", February 25th, 2024
;;   URL: "https://esolangs.org/wiki/Counterlang"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is desumed from the
   TYPE-NAME and whose formal parameters concur with the LAMBDA-LIST,
   the subject of this docimasy being norned by the CANDIDATE-NAME,
   executes the BODY forms with access to both the CANDIDATE-NAME and
   the LAMBDA-LIST, and construes the desinent BODY form's primary
   return value an assessment of the subject's eligiblity, a
   \"generalized boolean\" value of \"true\" serving to confirm a
   positive adjudgment, while a \"false\" response reneges such
   admission.
   ---
   The first BODY form, if resolving to a string object, will be
   interpreted as the type definition's documentation string and, hence,
   be reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            (format NIL "Definition of the derived type ~a." type-name))
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

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies to the ELEMENT-TYPE, the same
   defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among these complies with the KEY-TYPE,
   corresponding with a value conformant to the VALUE-TYPE, for both
   holds the comprehensive ``T'' as a default."
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

(deftype program ()
  "The ``program'' type defines an executable Counterlang program as a
   one-dimensional simple array of ``Instruction'' objects."
  '(simple-array Instruction (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Formal-Parameter".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Formal-Parameter
  (:constructor make-formal-parameter (name &optional (type T))))
  "The ``Formal-Parameter'' class furnishes the diorism of a formal
   parameter, or formal argument, as a contributor to a function
   definition's signature, composed of a mandatory name and an optional
   type, the same, as a consequence of its omission, resorts to the
   comprehensive ``T'' specification."
  (name (error "Missing parameter name.") :type symbol :read-only T)
  (type T                                 :type T      :read-only T))

;;; -------------------------------------------------------

(defun build-parameter-declarations (parameter)
  "Creates and returns the type and ``ignorable'' declarations for the
   formal PARAMETER as a list comprehending as its elements twissel
   these specifications."
  (declare (type Formal-Parameter parameter))
  (the list
    `((declare (type      ,(formal-parameter-type parameter)
                          ,(formal-parameter-name parameter)))
      (declare (ignorable ,(formal-parameter-name parameter))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parameter-List".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parameter-List
  (:constructor make-parameter-list (parameters)))
  "The ``Parameter-List'' class serves in the ensconcement of zero or
   more formal parameters to a function definition."
  (parameters (error "Missing parameters.")
              :type      (list-of Formal-Parameter)
              :read-only T))

;;; -------------------------------------------------------

(defun extract-parameter-names (parameters)
  "Returns a list comprehending the formal PARAMETERS' argument names."
  (declare (type Parameter-List parameters))
  (the (list-of symbol)
    (mapcar #'formal-parameter-name
      (parameter-list-parameters parameters))))

;;; -------------------------------------------------------

(defun build-parameter-list-declarations (parameters)
  "Creates and returns a list of ``declare'' declarations for each
   member of the PARAMETERS list."
  (declare (type Parameter-List parameters))
  (the list
    (mapcan #'build-parameter-declarations
      (parameter-list-parameters parameters))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Function-Head".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Function-Head
  (:constructor make-function-head (name
                                    &optional (result-type :none))))
  "The ``Function-Head'' type applies itself to the encapsulation of a
   function's agnomination and result type."
  (name        (error "Missing function name.")
               :type      (or symbol (list-of symbol))
               :read-only T)
  (result-type (error "Missing function result type.")
               :type      T
               :read-only T))

;;; -------------------------------------------------------

(defun function-head-result-type-is-none-p (head)
  "Determines whether the function HEAD's result type resolves expressly
   to the ``:none'' sentinel, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Function-Head head))
  (the boolean
    (not (null
      (and (keywordp (function-head-result-type head))
           (eq       (function-head-result-type head) :none))))))

;;; -------------------------------------------------------

(defun build-function-result-declaration (head)
  "Creates and returns a result type declaration for the function HEAD,
   this being concinnable for its employment in a ``the'' special form
   invocation."
  (declare (type Function-head head))
  (the T
    (if (function-head-result-type-is-none-p head)
      '(values)
      (function-head-result-type head))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of bespoke function definition operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-formal-parameter (specification)
  "Parses the parameter specification and returns a ``Formal-Parameter''
   representation thereof."
  (declare (type (or symbol list) specification))
  (the Formal-Parameter
    (typecase specification
      (symbol
        (make-formal-parameter specification))
      (list
        (apply #'make-formal-parameter specification))
      (otherwise
        (error "The object ~s cannot be interpreted as a ~
                formal parameter."
          specification)))))

;;; -------------------------------------------------------

(defun parse-formal-parameter-list (specifications)
  "Parses the formal parameters from the SPECIFICATIONS and returns a
   ``Parameter-List'' encapsulation thereof."
  (declare (type list specifications))
  (the Parameter-List
    (make-parameter-list
      (mapcar #'parse-formal-parameter specifications))))

;;; -------------------------------------------------------

(defun parse-function-head (specification)
  "Parses the function name and optional result type communicated via
   the SPECIFICATION and returns a frsh ``Function-Head'' representation
   thereof."
  (declare (type (or symbol list) specification))
  (the Function-Head
    (typecase specification
      (symbol
        (make-function-head specification :none))
      (list
        (apply #'make-function-head specification))
      (otherwise
        (error "Cannot parse the object ~s as a function head."
          specification)))))

;;; -------------------------------------------------------

(defmacro define-function (name-and-result-type (&rest lambda-list)
                           &body body)
  "Define a new function the agnomination of which and its expected
   result types derive from the NAME-AND-RESULT-TYPE complex, deploying
   the LAMBDA-LIST for its formal parameters' diorism, and executing
   the BODY forms, the desinent form's return values constituting the
   output.
   ---
   For the NAME-AND-RESULT, the following stipulations apply:
     ------------------------------------------------------------------
     name-and-result | Interpretation
     ----------------+-------------------------------------------------
     name            | A symbolic specification furnishes the
                     | function's name, while tacitly assuming no
                     | return value, which is tantamount to the
                     | ``:none'' sentinel.
     ..................................................................
     (name)          | A singleton list specification furnishes the
                     | function's name as a symbol, while tacitly
                     | assuming no return value, which is tantamount to
                     | the ``:none'' sentinel.
     ..................................................................
     (name result)   | A two-element list specification furnishes the
                     | function's name as a symbol in its sinistral
                     | compartment, while the return type, being of any
                     | type specifier form, is communicated in the
                     | second moiety.
     ------------------------------------------------------------------
   ---
   The LAMBDA-LIST's compass of homologation admits the following set
   of forbisens:
     ------------------------------------------------------------------
     lambda-list | Interpretation
     ------------+-----------------------------------------------------
     name        | A symbolic specification which specifies an argument
                 | of the comprehensive type ``T''.
     ..................................................................
     (name type) | A list of two elements, the first being the symbolic
                 | argument name, the second its type specifier.
     ------------------------------------------------------------------"
  (let ((head       (parse-function-head         name-and-result-type))
        (parameters (parse-formal-parameter-list lambda-list)))
    (declare (type Function-Head  head))
    (declare (type Parameter-List parameters))
    `(defun ,(function-head-name      head)
            ,(extract-parameter-names parameters)
       ,@(build-parameter-list-declarations parameters)
       ,(or (and (stringp (first body))
                 (> (length body) 1)
                 (pop body))
            "")
       (the ,(build-function-result-declaration head)
         (progn ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Counterlang-Error (error)
  ()
  (:documentation
    "The ``Counterlang-Error'' condition type accommodates a fundament
     common to all conditions whose involvement is tangent to a
     Counterlang program's evaluation."))

;;; -------------------------------------------------------

(define-condition Duplicate-Counter-Name-Error
  (Counterlang-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing counter name.")
    :reader        duplicate-counter-name-error-offending-name
    :type          string
    :documentation "The counter name already registered and subsequently
                    attempted to reuse."))
  (:documentation
    "The ``Duplicate-Counter-Name-Error'' condition type establishes am
     institution responsible for the communication of an anomalous
     situation whose etiology wones in the attempt to define a counter
     of an already extant agnomination."))

;;; -------------------------------------------------------

(define-condition Unbound-Counter-Name-Error
  (Counterlang-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing counter name.")
    :reader        unbound-counter-name-error-offending-name
    :type          string
    :documentation "The identifier not yet associated with a counter,
                    but attempted in a subjection to its perquisition or
                    modulation."))
  (:documentation
    "The ``Unbound-Counter-Name-Error'' condition type establishes an
     institution responsible for the communication of an anomalous
     situation whose etiology wones in the attempt to query or modify a
     counter by an identifier not yet registered."))

;;; -------------------------------------------------------

(define-function signal-duplicate-counter-name-error
    ((offending-name string))
  "Signals an error of the type ``Duplicate-Counter-Name-Error'' which
   communicates the OFFENDING-NAME."
  (error 'Duplicate-Counter-Name-Error
    :offending-name   offending-name
    :format-control   "A counter with the name ~s already exists ~
                       and cannot be initialized anew."
    :format-arguments (list offending-name)))

;;; -------------------------------------------------------

(define-function signal-unbound-counter-name-error
    ((offending-name string))
  "Signals an error of the type ``Unbound-Counter-Name-Error'' which
   communicates the OFFENDING-NAME."
  (error 'Unbound-Counter-Name-Error
    :offending-name   offending-name
    :format-control   "Cannot find a counter with the name ~s."
    :format-arguments (list offending-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (get-boolean-value-of boolean)
                 ((object              T))
  "Returns a Boolean representation of the OBJECT ensuing from its
   construe as a \"generalized boolean\" designator, returning for a
   non-``NIL'' input a ``boolean'' value of ``T'', otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (not (null object)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (space-character-p boolean)
                 ((candidate        character))
  "Determines whether the CANDIDATE represents a space or horizontal
   tab character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (member candidate '(#\Space #\Tab) :test #'char=)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (skip-spaces fixnum)
                 ((source string) (start fixnum))
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position into the
   SOURCE immediately succeeding the skipped tmema."
  (or (position-if-not #'space-character-p source :start start)
      (length source)))

;;; -------------------------------------------------------

(define-function (get-character-at (or null character))
                 ((source string) (position fixnum))
  "Returns the character at the POSITION into the SOURCE, or responds
   with ``NIL'' upon its bournes' transgression."
  (when (array-in-bounds-p source position)
    (char source position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of line tokenizer.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (read-identifier (values keyword string fixnum))
                 ((source string) (start fixnum))
  "Proceeding from the START position into the SOURCE, consumes a
   counter name and returns three values:
     (1) The token type ``:counter-name''.
     (2) The detected counter name as a string.
     (3) The position into SOURCE immediately succeeding the detected
         counter name's parcel."
  (let ((end-position
          (or (position-if-not #'alpha-char-p source :start start)
              (length source))))
    (declare (type fixnum end-position))
    (let ((identifier (subseq source start end-position)))
      (declare (type string identifier))
      (if (string= identifier "count")
        (values :count        identifier end-position)
        (values :counter-name identifier end-position)))))

;;; -------------------------------------------------------

(define-function (read-symbolic-token (values keyword T fixnum))
                 ((source             string)
                  (position           fixnum)
                  (token-type         keyword))
  "Creates a fresh token combining the TOKEN-TYPE with the character at
   the POSITION in the SOURCE and returns two values:
     (1) The TOKEN-TYPE.
     (2) The character at the POSITION into the SOURCE.
     (3) The index immediately succeeding the POSITION."
  (values token-type
    (get-character-at source position)
    (1+ position)))

;;; -------------------------------------------------------

(define-function (read-integer-literal (values keyword integer fixnum))
                 ((source string) (start  fixnum))
  "Proceeding from the START position into the SOURCE, consumes an
   unsigned integer literal and returns three values:
     (1) The token type ``:integer''.
     (2) The consumed unsigned integer number.
     (3) The position into the SOURCE immediately succeeding the tmema
         occupied by the consumed integer number (2)."
  (let ((end-position
          (or (position-if-not #'digit-char-p source :start start)
              (length source))))
    (declare (type fixnum end-position))
    (values :integer
      (parse-integer source :start start :end end-position)
      end-position)))

;;; -------------------------------------------------------

(define-function (read-next-token (values keyword T fixnum))
                 ((source string) (start fixnum))
  "Proceeding from the START position Into the SOURCE, extracts the next
   token and returns two values:
     (1) The detected token's type.
     (2) The detected token's value.
     (3) The position into the SOURCE immediately succeeding the
         token's occupied parcel."
  (let ((current-character (get-character-at source start)))
    (declare (type (or null character) current-character))
    (cond
      ((null current-character)
        (values :end-of-line NIL start))
      ((space-character-p current-character)
        (read-next-token source
          (skip-spaces source start)))
      ((char= current-character #\#)
        (values :end-of-line NIL
          (length source)))
      ((alpha-char-p current-character)
        (read-identifier source start))
      ((digit-char-p current-character)
        (read-integer-literal source start))
      ((char= current-character #\-)
        (read-symbolic-token source start :minus))
      ((char= current-character #\.)
        (read-symbolic-token source start :dot))
      ((char= current-character #\,)
        (read-symbolic-token source start :comma))
      ((char= current-character #\!)
        (read-symbolic-token source start :ecphoneme))
      ((char= current-character #\:)
        (read-symbolic-token source start :colon))
      (T
        (error "Invalid character \"~c\" at position ~d."
          current-character start)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operand classes.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface serves in a common foundry's dation for all
   concrete classes pursuing the representation of Counterlang
   instruction operands.")

;;; -------------------------------------------------------

(defstruct (Integer-Operand
  (:include     Operand)
  (:constructor make-integer-operand (value)))
  "The ``Integer-Operand'' class edifies an encapsulation of an
   instruction operand comprehending an integer literal."
  (value (error "Missing integer operand value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Counter-Name-Operand
  (:include     Operand)
  (:constructor make-counter-name-operand (name)))
  "The ``Counter-Name-Operand'' class produces an encapsulation of an
   instruction operand comprehending a counter name reference."
  (name (error "Missing counter name operand identifier.")
        :type      string
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' interface furnishes a substratum entreparted by
   all classes whose telos involves the modeling of a Counterlang
   operation.")

;;; -------------------------------------------------------

(defstruct (Add-Counters-Instruction
  (:include    Instruction)
  (:constructor make-add-counters-instruction (counter-twain)))
  "The ``Add-Counters-Instruction'' serves in the encapsulation of a
   counter incrementation operation, adhering to the forbisen
   \"{counter}{addend}\"."
  (counter-twain (error "Missing counter twain.")
                 :type      Counter-Name-Operand
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Add-Integer-Instruction
  (:include     Instruction)
  (:constructor make-add-integer-instruction (counter addend)))
  "The ``Add-Integer-Instruction'' serves in the encapsulation of a
   counter incrementation operation, adhering to the forbisen
   \"{counter}{addend}\"."
  (counter (error "Missing augend counter.")
           :type      Counter-Name-Operand
           :read-only T)
  (addend  (error "Missing addend.")
           :type      Integer-Operand
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Define-Counter-Instruction
  (:include     Instruction)
  (:constructor make-define-counter-instruction (name)))
  "The ``Define-Counter-Instruction'' serves in the encapsulation of a
   counter definition operation, adhering to the forbisen
   \"count {name}\"."
  (name (error "Missing counter name.")
        :type      Counter-Name-Operand
        :read-only T))

;;; -------------------------------------------------------

(defstruct (End-Instruction
  (:include     Instruction)
  (:constructor make-end-instruction ()))
  "The ``End-Instruction'' serves in the encapsulation of a program
   termination operation, adhering to the forbisen \":\".")

;;; -------------------------------------------------------

(defstruct (Goto-Instruction
  (:include     Instruction)
  (:constructor make-goto-instruction (destination)))
  "The ``Goto-Instruction'' serves in the encapsulation of a goto
   operation, adhering to the forbisen \"!{lineNumber}\"."
  (destination (error "Missing goto destination.")
               :type      Operand
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Number-Instruction
  (:include     Instruction)
  (:constructor make-print-number-instruction (argument)))
  "The ``Print-Number-Instruction'' serves in the encapsulation of a
   number printing operation, adhering to the forbisen \".{argument}\"."
  (argument (error "Missing print number argument.")
            :type      Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Print-Character-Instruction
  (:include     Instruction)
  (:constructor make-print-character-instruction (argument)))
  "The ``Print-Character-Instruction'' serves in the encapsulation of a
   character printing operation, adhering to the forbisen
   \",{argument}\"."
  (argument (error "Missing print character argument.")
            :type      Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Reset-Instruction
  (:include     Instruction)
  (:constructor make-reset-instruction (counter)))
  "The ``Reset-Instruction'' serves in the encapsulation of a counter
   resetting operation, adhering to the forbisen \"{counter}-\"."
  (counter (error "Missing counter name.")
           :type      Counter-Name-Operand
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Subtract-Instruction
  (:include     Instruction)
  (:constructor make-subtract-instruction (counter subtrahend)))
  "The ``Subtract-Instruction'' serves in the encapsulation of a counter
   decrementation operation, adhering to the forbisen
   \"{counter}-{subtrahend}\"."
  (counter    (error "Missing minuend counter.")
              :type      Counter-Name-Operand
              :read-only T)
  (subtrahend (error "Missing subtrahend.")
              :type      Operand
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (expect-token         (values keyword T fixnum))
                 ((source              string)
                  (start               fixnum)
                  (expected-token-type keyword))
  "Proceeding from the START position into the SOURCE, determines
   whether the subsequent token conforms to the EXPECTED-TYPE-TYPE,
   on confirmation returning three values:
     (1) The successfully attested token's type.
     (2) The successfully attested token's value.
     (3) The position into the SOURCE immediately succeeding this token
   Otherwise an error of an unspecified type is signaled."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (read-next-token source start)
    (declare (type keyword next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (if (eq next-token-type expected-token-type)
      (values next-token-type next-token-value new-position)
      (error "Expected a token of the type ~s at position ~d, ~
              but encountered (~s, ~s)."
        expected-token-type start
        next-token-type next-token-value))))

;;; -------------------------------------------------------

(define-function
    (parse-counter-name (values Counter-Name-Operand fixnum))
    ((source string) (start fixnum))
  "Proceeding from the START position into the SOURCE, parses a counter
   name and returns two values:
     (1) The detected counter name ensconced in a fresh
         ``Counter-Name-Operand'' object.
     (2) The position into the SOURCE immediately succeeding the
         consumed counter name's occupied parcel."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (expect-token source start :counter-name)
    (declare (type keyword next-token-type))
    (declare (ignore       next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (values
      (make-counter-name-operand next-token-value)
      new-position)))

;;; -------------------------------------------------------

(define-function (expect-end-of-line :none)
                 ((source string) (start fixnum))
  "Proceeding from the START position into the SOURCE, determines
   whether the remaining tmema is devoid of any effective content,
   admitting merely the contingency for spacings and a comment, on
   confirmation returning no value; otherwise signals an error of an
   unspecified type."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (read-next-token source start)
    (declare (type keyword next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (declare (ignore       new-position))
    (unless (eq next-token-type :end-of-line)
      (error "Expected an end-of-line token at position ~d, ~
              but encountered (~s, ~s)."
        start next-token-type next-token-value)))
  (values))

;;; -------------------------------------------------------

(define-function (parse-minus-operation Instruction)
                 ((counter-name         Counter-Name-Operand)
                  (source               string)
                  (start                fixnum))
  "Proceeding from the START position into the SOURCE, parses an
   arithmetic operation involving the COUNTER-NAME and comprehending a
   minus sign (\"-\") and returns an ``Instruction'' thereof."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (read-next-token source start)
    (declare (type keyword next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (case next-token-type
      ;; {counter}-{integer}
      (:integer
        (prog1
          (make-subtract-instruction counter-name
            (make-integer-operand next-token-value))
          (expect-end-of-line source new-position)))
      ;; {counter}-{counter}
      (:counter-name
        (prog1
          (make-subtract-instruction counter-name
            (make-counter-name-operand next-token-value))
          (expect-end-of-line source new-position)))
      ;; {counter}-
      (:end-of-line
        (prog1
          (make-reset-instruction counter-name)
          (expect-end-of-line source new-position)))
      (otherwise
        (error "Invalid instruction pattern: \"~a-~a\""
          (counter-name-operand-name counter-name)
          next-token-value)))))

;;; -------------------------------------------------------

(define-function (parse-arithmetic-operation Instruction)
                 ((counter-name              Counter-Name-Operand)
                  (source                    string)
                  (start                     fixnum))
  "Proceeding from the START position into the SOURCE, parses an
   arithmetic operation involving the COUNTER-NAME and returns an
   ``Instruction'' thereof."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (read-next-token source start)
    (declare (type keyword next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (case next-token-type
      ;; {counter}{integer}
      (:integer
        (prog1
          (make-add-integer-instruction counter-name
            (make-integer-operand next-token-value))
          (expect-end-of-line source new-position)))
      ;; {counter}-[...]
      (:minus
        (parse-minus-operation counter-name source new-position))
      ;; {counter}{counter}
      (:end-of-line
        (prog1
          (make-add-counters-instruction counter-name)
          (expect-end-of-line source new-position)))
      (otherwise
        (error "Invalid instruction syntax: \"~a ~a\"."
          (counter-name-operand-name counter-name)
          next-token-value)))))

;;; -------------------------------------------------------

(define-function (parse-operand (values Operand fixnum))
                 ((source string) (start fixnum))
  "Proceeding from the START position into the SOURCE, parses a integer
   literal or a counter name and returns two values:
     (1) The parsed object's ``Operand'' representation.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the extracted operand."
  (multiple-value-bind (next-token-type next-token-value new-position)
      (read-next-token source start)
    (declare (type keyword next-token-type))
    (declare (type T       next-token-value))
    (declare (type fixnum  new-position))
    (case next-token-type
      (:integer
        (values
          (make-integer-operand next-token-value)
          new-position))
      (:counter-name
        (values
          (make-counter-name-operand next-token-value)
          new-position))
      (otherwise
        (error "No operand token: (~s, ~s) at position ~d."
          next-token-type next-token-value start)))))

;;; -------------------------------------------------------

(define-function (parse-line (or null Instruction))
                 ((source    string))
  "Parses the source LINE and returns an ``Instruction'' representation
   thereof; or, upon its vacancy, responds with the ``NIL'' value."
  (let ((current-position 0)
        (token-type       :end-of-line)
        (token-value      NIL)
        (next-position    0))
    (declare (type fixnum current-position))
    (multiple-value-setq (token-type token-value next-position)
      (read-next-token source current-position))
    (case token-type
      (:end-of-line
        NIL)
      ;; count {name}
      (:count
        (let ((counter-name NIL))
          (declare (type (or null Counter-Name-Operand) counter-name))
          (multiple-value-setq (counter-name next-position)
            (parse-counter-name source next-position))
          (prog1
            (make-define-counter-instruction counter-name)
            (expect-end-of-line source next-position))))
      ;; {counter}[...]
      (:counter-name
        (parse-arithmetic-operation
          (make-counter-name-operand token-value)
          source
          next-position))
      ;; .{argument}
      (:dot
        (let ((argument NIL))
          (declare (type (or null Operand) argument))
          (multiple-value-setq (argument next-position)
            (parse-operand source next-position))
          (prog1
            (make-print-number-instruction argument)
            (expect-end-of-line source next-position))))
      ;; ,{argument}
      (:comma
        (let ((argument NIL))
          (declare (type (or null Operand) argument))
          (multiple-value-setq (argument next-position)
            (parse-operand source next-position))
          (prog1
            (make-print-character-instruction argument)
            (expect-end-of-line source next-position))))
      ;; !{destination}
      (:ecphoneme
        (let ((destination NIL))
          (declare (type (or null Operand) destination))
          (multiple-value-setq (destination next-position)
            (parse-operand source next-position))
          (prog1
            (make-goto-instruction destination)
            (expect-end-of-line source next-position))))
      ;; :
      (:colon
        (prog1
          (make-end-instruction)
          (expect-end-of-line source next-position)))
      (otherwise
        (error "Invalid token (~s, ~s) at position ~d."
          token-type token-value current-position)))))

;;; -------------------------------------------------------

(define-function (parse-program program) ((source string))
  "Parses the SOURCE and returns a Counterlang program representation
   of its ensconced instructions."
  (coerce
    (with-input-from-string (source-stream source)
      (declare (type string-stream source-stream))
      (loop
        for current-line
          of-type (or null string)
          =       (read-line source-stream NIL NIL)
        
        while current-line
        
        for parsed-instruction
          of-type (or null Instruction)
          =       (parse-line current-line)
        
        when parsed-instruction
          collect parsed-instruction))
    '(simple-array Instruction (*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of counter table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Counter-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :reader        counter-table-entries
    :type          (hash-table-of name integer)
    :documentation "Maps the defined counter names to their ensconced
                    integer values."))
  (:documentation
    "The ``Counter-Table'' class' onus is delineated by its castaldy of
     an arbitary tally of counters, affiliating the unique identifier
     names with signed integer values."))

;;; -------------------------------------------------------

(define-function (make-empty-counter-table Counter-Table) ()
  "Creates and returns a fresh and initially vacant ``Counter-Table''."
  (make-instance 'Counter-Table))

;;; -------------------------------------------------------

(define-function (counter-table-contains-name-p boolean)
                 ((counters                     Counter-Table)
                  (name                         string))
  "Determines whether the table of COUNTERS table entails an entry
   amenable to the NAME, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (nth-value 1
      (gethash name
        (counter-table-entries counters)))))

;;; -------------------------------------------------------

(define-function (define-counter :none)
                 ((counters      Counter-Table)
                  (name          string))
  "Creates a fresh counter denoted by the NAME and affiliated with the
   default state of zero (0), registers this entry in the COUNTERS
   table, and returns no value.
   ---
   If an entry with such a NAME already participates in the COUNTERS
   table, an error of the type ``Duplicate-Counter-Name-Error"
  (if (counter-table-contains-name-p counters name)
    (signal-duplicate-counter-name-error name)
    (setf (gethash name (counter-table-entries counters)) 0))
  (values))

;;; -------------------------------------------------------

(define-function (get-counter-value integer)
                 ((counters         Counter-Table)
                  (name             string))
  "Returns the integer value maintained by the counter amenable to the
   NAME in the table of COUNTERS, or signals an error of the type
   ``Unbound-Counter-Name-Error'' upon its disrespondency."
  (if (counter-table-contains-name-p counters name)
    (nth-value 0
      (gethash name
        (counter-table-entries counters)))
    (signal-unbound-counter-name-error name)))

;;; -------------------------------------------------------

(define-function (set-counter-value :none)
                 ((counters         Counter-Table)
                  (name             string)
                  (new-value        integer))
  "Associates the NEW-VALUE with the counter amenable to the NAME in the
   COUNTERS table and returns no value."
  (if (counter-table-contains-name-p counters name)
    (setf (gethash name (counter-table-entries counters)) new-value)
    (signal-unbound-counter-name-error name))
  (values))

;;; -------------------------------------------------------

(define-function (get-counter-names (list-of string))
                 ((counters         Counter-Table))
  "Returns a fresh list comprehending the COUNTERS table's registered
   counter names."
  (loop
    for current-counter-name
      of-type string
      being the hash-keys in (counter-table-entries counters)
    collect current-counter-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of counter name splicer.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (string-starts-with-p boolean)
                 ((haystack            string)
                  (prefix              string))
  "Determines whether the HAYSTACK ends with the PREFIX, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (string= haystack prefix
      :start1 0
      :end1   (min (length prefix)
                   (length haystack)))))

;;; -------------------------------------------------------

(define-function (string-ends-with-p boolean)
                 ((haystack          string)
                  (suffix            string))
  "Determines whether the HAYSTACK ends with the SUFFIX, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (string= haystack suffix
      :start1 (max 0
                (- (length haystack)
                   (length suffix))))))

;;; -------------------------------------------------------

(define-function (string-twissel-matches-length-p boolean)
                 ((source                         string)
                  (prefix                         string)
                  (suffix                         string))
  "Determines whether the PREFIX and SUFFIX in conjunction ostend a
   combined length paregal to the SOURCE's character tally, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (= (length source)
       (+ (length prefix)
          (length suffix)))))

;;; -------------------------------------------------------

(define-function (string-contains-twissel-p boolean)
                 ((source                   string)
                  (prefix                   string)
                  (suffix                   string))
  "Determines whether the PREFIX and SUFFIX, conjoined in this exact
   arrangement, manage to replicate the SOURCE with perfect lealty,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (and
    (string-twissel-matches-length-p source prefix suffix)
    (string-starts-with-p            source prefix)
    (string-ends-with-p              source        suffix)))

;;; -------------------------------------------------------

(define-function (find-counter-name-combination (values string string))
                 ((compound-name                string)
                  (counter-names                (list-of string)))
  "Searches in the COUNTER-NAMES for a combination of two not
   necessarily distinct members which are capacitated to replicate the
   COMPOUND-NAME in an ipsissima verba fashion, returning on
   confirmation two values:
     (1) The left  moeity of the detected component string.
     (2) The right moeity of the detected component string.
   Otherwise, upon a connable assemablage's lacuna, signals an error of
   an unspecified type."
  (loop
    named search-loop
    for left-name of-type string in counter-names do
      (loop for right-name of-type string in counter-names do
        (when (string-contains-twissel-p
                compound-name left-name right-name)
          (return-from search-loop
            (values left-name right-name))))
    finally
      (error "No components found which could edify the string ~s."
        compound-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Counterlang program.")
    :type          program
    :documentation "The Counterlang program to execute.")
   (ip
    :initform      0
    :type          integer
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the PROGRAM.")
   (goto-jump-requested-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether a goto instruction has been
                    actuated, which serves to apprize about the next
                    instruction pointer (IP) destination as the already
                    imposed line number.")
   (counters
    :initform      (make-empty-counter-table)
    :type          Counter-Table
    :documentation "A mapping betwixt the counter name strings and their
                    integral values."))
  (:documentation
    "The ``Interpreter'' class is apportioned that onus to accompas
     actual efficacy to a Counterlang program supplied as vector of
     instructions."))

;;; -------------------------------------------------------

(define-function (make-interpreter Interpreter)
                 ((program         Program))
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   Counterlang program's execution."
  (make-instance 'Interpreter :program program))

;;; -------------------------------------------------------

(define-function (program-completed-p boolean)
                 ((interpreter        Interpreter))
  "Determines whether the Counterlang program consigned to the
   INTERPRETER's castaldy has been processed to a state of patration,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (not (array-in-bounds-p program ip))))

;;; -------------------------------------------------------

(define-function (advance-ip   :none)
                 ((interpreter Interpreter))
  "Advances the INTERPRETER's internally managed instruction pointer
   (IP) to the next position in its program and returns no value."
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (when (< ip (length program))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(define-function (go-to-line   :none)
                 ((interpreter Interpreter)
                  (destination integer))
  "Relocates the INTERPRETER's internally managed instruction pointer
   (IP) to the one-based DESTINATION line number and returns no value."
  (with-slots (ip goto-jump-requested-p) interpreter
    (declare (type fixnum  ip))
    (declare (type boolean goto-jump-requested-p))
    (psetf ip                    (1- destination)
           goto-jump-requested-p T))
  (values))

;;; -------------------------------------------------------

(define-function (move-to-next-instruction :none)
                 ((interpreter             Interpreter))
  "Relocates the INTERPRETER's instruction pointer (IP) to the next
   instruction, either advancing linearly, or jumping to a specified
   line, and returns no value."
  (with-slots (goto-jump-requested-p) interpreter
    (declare (type boolean goto-jump-requested-p))
    (if goto-jump-requested-p
      (setf goto-jump-requested-p NIL)
      (advance-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(define-function (end-program  :none)
                 ((interpreter Interpreter))
  "Designates the INTERPRETER's program as terminated and returns no
   value."
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (setf ip (length program)))
  (values))

;;; -------------------------------------------------------

(define-function (get-current-instruction Instruction)
                 ((interpreter            Interpreter))
  "Returns the instruction located at the INTERPRETER's instruction
   pointer (IP) location."
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (aref program ip)))

;;; -------------------------------------------------------

(define-function (make-counter  :none)
                 ((interpreter  Interpreter)
                  (counter-name string))
  "Declares a new counter amenable to the COUNTER-NAME in the
   INTERPRETER, affiliates thilk with the default state of zero (0), and
   returns no value."
  (with-slots (counters) interpreter
    (declare (type Counter-Table counters))
    (define-counter counters counter-name))
  (values))

;;; -------------------------------------------------------

(define-function (counter-value integer)
                 ((interpreter  Interpreter)
                  (counter-name string))
  "Returns the value associated with the COUNTER-NAME in the
   INTERPRETER."
  (with-slots (counters) interpreter
    (declare (type Counter-Table counters))
    (get-counter-value counters counter-name)))

;;; -------------------------------------------------------

(define-function ((setf counter-value) :none)
                 ((new-value           integer)
                  (interpreter         Interpreter)
                  (counter-name        string))
  "Stores the NEW-VALUE in the counter amenable to the COUNTER-NAME in
   the INTERPRETER and returns no value."
  (with-slots (counters) interpreter
    (declare (type Counter-Table counters))
    (set-counter-value counters counter-name new-value))
  (values))

;;; -------------------------------------------------------

(define-function (reset-counter :none)
                 ((interpreter  Interpreter)
                  (counter-name string))
  "Resets the counter amenable to the COUNTER-NAME in the INTERPRETER
   to its inchoate state of zero (0) and returns no value."
  (setf (counter-value interpreter counter-name) 0)
  (values))

;;; -------------------------------------------------------

(define-function (list-counter-names (list-of string))
                 ((interpreter       Interpreter))
  "Returns a fresh list comprehending the names of the counters governed
   by the INTERPRETER's castaldy."
  (with-slots (counters) interpreter
    (declare (type Counter-Table counters))
    (get-counter-names counters)))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Determines the OPERAND's value in the INTERPRETER's context and
     returns thilk.")
  
  (:method ((interpreter Interpreter) (operand Integer-Operand))
    (declare (type Interpreter     interpreter))
    (declare (ignore               interpreter))
    (declare (type Integer-Operand operand))
    (the integer
      (integer-operand-value operand)))
  
  (:method ((interpreter Interpreter) (operand Counter-Name-Operand))
    (declare (type Interpreter          interpreter))
    (declare (type Counter-Name-Operand operand))
    (the integer
      (counter-value interpreter
        (counter-name-operand-name operand)))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Add-Counters-Instruction))
  (declare (type Interpreter              interpreter))
  (declare (type Add-Counters-Instruction instruction))
  (multiple-value-bind (augend-counter-name addend-counter-name)
      (find-counter-name-combination
        (counter-name-operand-name
          (add-counters-instruction-counter-twain instruction))
        (list-counter-names interpreter))
    (declare (type string augend-counter-name))
    (declare (type string addend-counter-name))
    (incf
      (counter-value interpreter augend-counter-name)
      (counter-value interpreter addend-counter-name)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Add-Integer-Instruction))
  (declare (type Interpreter             interpreter))
  (declare (type Add-Integer-Instruction instruction))
  (incf
    (counter-value interpreter
      (counter-name-operand-name
        (add-integer-instruction-counter instruction)))
    (resolve-operand interpreter
      (add-integer-instruction-addend instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction Define-Counter-Instruction))
  (declare (type Interpreter                interpreter))
  (declare (type Define-Counter-Instruction instruction))
  (make-counter interpreter
    (counter-name-operand-name
      (define-counter-instruction-name instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction End-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type End-Instruction instruction))
  (declare (ignore               instruction))
  (end-program interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Goto-Instruction))
  (declare (type Interpreter      interpreter))
  (declare (type Goto-Instruction instruction))
  (go-to-line interpreter
    (resolve-operand interpreter
      (goto-instruction-destination instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction
    ((interpreter Interpreter)
     (instruction Print-Character-Instruction))
  (declare (type Interpreter                 interpreter))
  (declare (type Print-Character-Instruction instruction))
  (format T "~c"
    (code-char
      (resolve-operand interpreter
        (print-character-instruction-argument instruction))))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Print-Number-Instruction))
  (declare (type Interpreter              interpreter))
  (declare (type Print-Number-Instruction instruction))
  (format T "~&~d"
    (resolve-operand interpreter
      (print-number-instruction-argument instruction)))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Reset-Instruction))
  (declare (type Interpreter       interpreter))
  (declare (type Reset-Instruction instruction))
  (reset-counter interpreter
    (counter-name-operand-name
      (reset-instruction-counter instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Subtract-Instruction))
  (declare (type Interpreter          interpreter))
  (declare (type Subtract-Instruction instruction))
  (decf
    (counter-value interpreter
      (counter-name-operand-name
        (subtract-instruction-counter instruction)))
    (resolve-operand interpreter
      (subtract-instruction-subtrahend instruction)))
  (values))

;;; -------------------------------------------------------

(define-function (execute-program :none)
                 ((interpreter    Interpreter))
  "Executes the Counterlang program consigned to the INTERPRETER's
   castaldy and returns no value."
  (loop until (program-completed-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (move-to-next-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(define-function (interpret-Counterlang :none) ((code string))
  "Interprets the piece of Counterlang source CODE and returns no
   value."
  (execute-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Count up from inclusive zero (0) to positive infinity.
(interpret-Counterlang
  "count upCounter
   .upCounter
   upCounter1
   !2")

;;; -------------------------------------------------------

;; Demonstrate the addition of counters.
(interpret-Counterlang
  "count myCounter
   myCounter50
   myCountermyCounter
   .myCounter")

;;; -------------------------------------------------------

;; Demonstrate the addition of counters.
(interpret-Counterlang
  "count firstCounter
   count secondCounter
   firstCounter10
   secondCounter20
   firstCountersecondCounter
   .firstCounter")

;;; -------------------------------------------------------

;; Demonstrate the subtraction of counters.
(interpret-Counterlang
  "count firstCounter
   count secondCounter
   firstCounter10
   secondCounter3
   firstCounter-secondCounter
   .firstCounter")

;;; -------------------------------------------------------

;; Demonstrate several arithmetic operations.
(interpret-Counterlang
  "
  count a #make a a counter initialized as 0
  a1 #add 1 to counter of a
  a-2 #add -2 to counter of a
  a2 #add 2 to counter of a
  aa #add a to counter of a (double)
  aa #double
  aa #double a is 16 now
  a- #reset a to 0
  ")

;;; -------------------------------------------------------

;; Demonstrate the printing facilities.
(interpret-Counterlang
  ".1  #prints 1
   ,65 #prints A")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-Counterlang
  "!2 #loop
   !1 #loop")

;;; -------------------------------------------------------

;; Demonstrate immediate termination.
(interpret-Counterlang
  ": #end
   count thisisnotexecuted
   thisisnotexecuted100
   .thisisnotexecuted")
