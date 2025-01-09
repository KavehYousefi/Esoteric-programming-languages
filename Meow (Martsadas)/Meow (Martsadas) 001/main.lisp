;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Meow", invented by the Esolang user "Martsadas" and
;; presented on September 9th, 2021, its kenspeckle haecceity woning
;; in a tongue whose derivation registers a feline entheus, operating
;; as a concomitant on a twain of signed integer-valued registers,
;; its warklumes enhanced by arithmetic expressions, input and output
;; communication, and a label-based control flow construct.
;; 
;; 
;; Concept
;; =======
;; The Meow programming language ostends the assignment of a diction
;; originating from a feline entheus while operating on a twissel of
;; registers, their agnomination fixed at "a" for the primary specimen,
;; and "b" for its parhedral compernage, attending to a scalar signed
;; integer datum.
;; 
;; == THE MEMORY: TWO INTEGER-VALUED REGISTERS ==
;; The data bailiwick submits to a register twain's castaldy, among
;; which is administered an inbalance of ponderance: The primary
;; register, norned "a", defines the paravaunt specimen's establishment,
;; being rendered the cynosure of most operations; a paravail competence
;; applies to the secondary component, nevend "b".
;; 
;; Disencumbered from the discrepancy's purview appertaining to their
;; ranks, the register twissel's capacity does not wist of any
;; prerogatives and discriminations, both lending a commorancy to an
;; aefauld integer object of any sign and mickleness.
;; 
;; 
;; Syntax
;; ======
;; The Meow programming language's diction is desumed from the subject
;; of felines, this mimicry molded into a sequence of instructions
;; distributed via whitespaces.
;; 
;; == INSTRUCTIONS ==
;; An instruction's conformation imposes the operator identifier as the
;; incipient constituent, segregated from its contingent arguments,
;; their componency amounting to an arity of zero through two members,
;; one or more whitespaces.
;; 
;; == ARGUMENTS ==
;; The segregation of arguments from the inchoating instruction
;; identifier and any contingent subsequent compernage proceeds by means
;; of one or more whitespaces, thus excluding such sepiments from any
;; non-string constituent, which a fortiori relates to label names and
;; arithmetic expressions.
;; 
;; == INTEGER NUMBERS ==
;; The specification of an integer number amplects an optional sign
;; followed by one or more decimal digits.
;; 
;; == STRINGS ==
;; A string literal's diorism ensues from a character sequence of zero
;; or more elements' membership, demarcated by a jumelle of double
;; quotation marks ('"').
;; 
;; == REGISTERS ==
;; The avalable registers' compound partakes of a representation via the
;; one-character identifier, commorant in the set {"a", "b"}.
;; 
;; 
;; Instructions
;; ============
;; An octuple cardinality applies to the Meow programming language's
;; instruction set, the circumference one in attendance of its
;; register's indagation and manipulation, the reception of input and
;; issuance of output, as well as a conditional control flow mechanism
;; whose foundry's edification is begotten by labels.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall edify a foundational mete of
;; nortelry regarding the Meow language's operative competences.
;; 
;; Please heed that succedaneous tmema bear an underline formed by an
;; asterisk ("*") catena, intended for their subsequent substitution by
;; actual Meow code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   Put value      | Stores the {value} in the primary register "a".
;;       *****      |--------------------------------------------------
;;                  | The {value} must be an expression.
;;   ..................................................................
;;   Move           | Swaps the value stored in the primary register
;;                  | "a" with that from the secondary register "b".
;;   ..................................................................
;;   Meow           | Prints the character whose ASCII code equals the
;;                  | the value stored in the primary register "a" to
;;                  | the standard output.
;;   ..................................................................
;;   Purr text      | Prints the {text} to the standard output.
;;        ****      |--------------------------------------------------
;;                  | The {text} must be a string, ensconced in a
;;                  | jumelle of double quotation marks ('"..."').
;;   ..................................................................
;;   Get            | Queries the standard input for a character and
;;                  | stores its ASCII code in the primary register
;;                  | "a".
;;   ..................................................................
;;   Cat labelName  | Defines a new label identified by the
;;       *********  | {labelName}.
;;                  |--------------------------------------------------
;;                  | The {labelName} must be a valid name not yet
;;                  | appropriated for this purpose.
;;                  |--------------------------------------------------
;;                  | If a label amenable to the {labelName} already
;;                  | exists, an error of the type
;;                  | "DuplicateLabelNameError" is signaled.
;;   ..................................................................
;;   Go guard label | If the {guard} expression yields a negative
;;      ***** ***** | result, relocates the instruction pointer (IP) to
;;                  | the definition point of the {label}.
;;                  |--------------------------------------------------
;;                  | The {guard} must be an expression.
;;                  |--------------------------------------------------
;;                  | The {label} must be the name of an extant label.
;;                  |--------------------------------------------------
;;                  | If no label amenable to the {label} name exists,
;;                  | an error of the type "UndefinedLabelError" is
;;                  | signaled.
;;   ..................................................................
;;   Exit           | Immediately halts the program.
;;                  |--------------------------------------------------
;;                  | If the program concludes in an exhaustion of its
;;                  | comprehended instructions without a prevenient
;;                  | "Exit" behest, an error of the type
;;                  | "MissingExitInstructionError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; The involvement of expressions in the Meow programming languages
;; acquires manifold apparitions:
;; 
;;   (1) INTEGER LITERALS
;;       Integer numbers may be inserted into the program in a literal
;;       form as signed or unsigned decimal numbers, disencumbered from
;;       any impositions of its mickleness.
;;   
;;   (2) STRING LITERALS
;;       The specification of string literals proceeds by means of zero
;;       or more characters, their ensconcement reified in a jumelle of
;;       double quotes ('"').
;;   
;;   (3) REGISTER NAMES
;;       The twain of offered integer-valued registers express their
;;       amenability to the character identifiers "a" and "b".
;;   
;;   (4) COMPOUND EXPRESSIONS
;;       Complex expressions are elicited by an edification from two
;;       or more atomar or composite specimens, desumed from the
;;       arithmetic vales, concluding in a response originating from
;;       the same provenance.
;; 
;; == UNARY OPERATORS ==
;; A twissel of unary operations experience their admission to the
;; arithmetic realm:
;; 
;;   ------------------------------------------------------------------
;;   Unary operator | Effect
;;   ---------------+--------------------------------------------------
;;   +              | Positive sign
;;   ..................................................................
;;   -              | Negative sign
;;   ------------------------------------------------------------------
;; 
;; == BINARY OPERATORS ==
;; A quituple membership exhausts the binary operators' dation, the
;; diorisms appertaining exclusively to the arithmetic and lexicographic
;; bailiwicks:
;; 
;;   ------------------------------------------------------------------
;;   Binary operator | Effect
;;   ----------------+-------------------------------------------------
;;   +               | Addition
;;   ..................................................................
;;   -               | Subtraction
;;   ..................................................................
;;   *               | Multiplication
;;   ..................................................................
;;   /               | Integer division; truncates fractional part
;;   ..................................................................
;;   %               | Remainder
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp utilizes a recursive
;; descent parser, extended and complemented by a Pratt parser component
;; for the processing of expressions.
;; 
;; == INTERPRETATION CONSTITUTES A TRIPLE STAGE PROCESS ==
;; The complete interpretation is defined in terms of a graduated
;; processing, separable into several tiers:
;; 
;;   (1) The statement parser's avails itself with the statement
;;       tokenizer's warklumes for the extraction peisant objects from
;;       the Meow source code string in order to replicate the
;;       ensconced oeprations in an ordered list of instructions.
;;   (2) If an expression tmema has been detected, the statement parser
;;       extracts thilk and induces it in the expression tokenizer.
;;   (3) The Pratt parser queries these tokens and assembles a
;;       hierarchical expression representation, contributing the
;;       respective instruction's operand in a covenable format.
;;   (4) The interpreter traverses the instruction sequence and embues
;;       it with effect. If the instruction operand subsumes into the
;;       parsed expression type, the interpreter's parergal dever
;;       amounts to this hierarchical arithmetic composite's evaluation
;;       in the pursuit of an integer value's gendrure.
;; 
;; == EXPRESSIONS ARE ASSEMBLED VIA PRATT PARSING ==
;; The parser combines aspects of recursive descent and Pratt's
;; solution, with the former apprehending the general process, aided by
;; the latter for the assemblage of expressions. The Pratt component's
;; conventions and notions are partially derived from Denis
;; Lantsman [lantsman2018prattparsers].
;; 
;; These two parsers are intertwined in a kenspeckle fashion, the
;; recursive descent specimen attaining a paravaunt significance in the
;; entire process' manuduction. The Pratt parser exerts its duty merely
;; upon the superimposed entity's behest, impounded in this matter to
;; the production of representations from an expression string.
;; 
;; 
;; Appendices
;; ==========
;; Ensuing from the documentation's main subjects, a certain repertoire
;; appertaining to topics harboring significance to a more remote mete
;; remain. This orra material, maugre its paravail status, shall be the
;; following sections' bailiwick.
;; 
;; == APPENDIX A: OPERATOR BINDING POWERS AND ASSOCIATIVITIES ==
;; This implementation's reliance upon the Pratt parser and its innate
;; components, the binding power and associativity, rede a compendious
;; tendance's adhibition upon the subject.
;; 
;; A listing of the available operators, their binding powers, and
;; associativity configurations shall be ostended in the following
;; tabular illustration:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Binding power | Associativity | Apostil
;;   ---------+---------------+---------------+------------------------
;;   (...)    | 0             | none          | Effectively endowed
;;            |               |               | with the highest
;;            |               |               | binding power.
;;   ..................................................................
;;   +        | 130           | left-to-right | 
;;   ..................................................................
;;   -        | 130           | left-to-right | 
;;   ..................................................................
;;   *        | 140           | left-to-right | 
;;   ..................................................................
;;   /        | 140           | left-to-right | 
;;   ..................................................................
;;   %        | 140           | left-to-right | 
;;   ..................................................................
;;   - (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   + (sign) | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-03
;; 
;; Sources:
;;   [cppreference2023c++operprec]
;;   The cppreference contributors, "C++ Operator Precedence",
;;     September 10th, 2023
;;   URL: "https://en.cppreference.com/w/cpp/language/
;;         operator_precedence"
;;   Notes:
;;     - Lists the C++ operators in conjunction with their precedence
;;       and associativity attributes.
;;   
;;   [crockford2007topdownopprec]
;;   Douglas Crockford, "Top Down Operator Precedence", 2007-02-21
;;   URL: "http://crockford.com/javascript/tdop/tdop.html"
;;   Notes:
;;     - Describes a Pratt parser implementing a subset of JavaScript,
;;       yclept "Simplified JavaScript".
;;     - Introduces the concept of "statement denotation" ("std") for
;;       incorporating statements into the Pratt parser's homogeneous
;;       expression system.
;;   
;;   [crockford2010tdopparse.js]
;;   Douglas Crockford, "parse.js", 2010-06-26
;;   URL: "http://crockford.com/javascript/tdop/parse.js"
;;   Notes:
;;     - Presents the source code for a Pratt parser implementing a
;;       subset of JavaScript, yclept "Simplified JavaScript".
;;     - The main page, supplying elucidations, can be found under
;;       -> "https://crockford.com/javascript/tdop/tdop.html".
;;   
;;   [esolang2023MeowMartsadas]
;;   The Esolang contributors, "Meow (Martsadas)", November 21st, 2023
;;   URL: "https://esolangs.org/wiki/Meow_(Martsadas)"
;;   
;;   [grand1997javalangref]
;;   Mark Grand,
;;     "Java Language Reference", 2nd Edition July 1997,
;;     "Chapter 4.14 Order of Operations",
;;     1997
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   Notes:
;;     - Describes and lists the order of operations established in the
;;       Java programming language.
;;   
;;   [kumar2016javaprecassoc]
;;   Krishan Kumar, "Java Operators: Precedence and Associativity", 2016
;;   URL: "https://cs-fundamentals.com/java-programming/
;;         java-operators-precedence-and-associativity"
;;   Notes:
;;     - Lists the operator precedences and associativities specified
;;       for the Java programming language.
;;   
;;   [lantsman2018prattparsers]
;;   Denis Lantsman, "How Desmos uses Pratt Parsers", 2018
;;   URL: "https://engineering.desmos.com/articles/pratt-parser/"
;;   Notes:
;;     - Provides a pellucid explanation of the Pratt parser concept.
;;   
;;   [pratt1973top]
;;   Vaughan R. Pratt, "Top Down Operator Precedence", 1973
;;   URL: "https://daesan.com/wp-content/uploads/2018/05/
;;         top_down_operator_precedence.pdf"
;;   
;;   [pythonswfoundation2023operprec]
;;   Python Software Foundation,
;;     "The Python Language Reference",
;;     section 6.17, "Operator precedence",
;;     September 14th, 2023
;;   URL: "https://docs.python.org/3/reference/
;;         expressions.html#operator-precedence"
;;   Notes:
;;     - Lists the operators in Python in conjunction with their
;;       precedences.
;;   
;;   [sedgewick2022operatorprecedence]
;;   Robert Sedgewick, Kevin Wayne,
;;     "Appendix A: Operator Precedence in Java", 2022
;;   URL: "https://introcs.cs.princeton.edu/java/11precedence/"
;;   Notes:
;;     - Operator precedence in Java.
;;   
;;   [stackoverflow2011q2811319]
;;   The Stack Overflow contributors,
;;     "What is the difference between >>> and >> operators in Java?",
;;     2011
;;   URL: "https://stackoverflow.com/questions/2811319/
;;         difference-between-and"
;;   Notes:
;;     - Describes the ">>>" operator in Java, an unsigned right shift.
;;   
;;   [williams2022onrecursivedescent]
;;   URL: "https://chidiwilliams.com/post/
;;         on-recursive-descent-and-pratt-parsing/"
;;   Notes:
;;     - Introduces a "ParseRule" class and a "Precedence" enumerated
;;       type which in conjunction bundle the Pratt parser concepts.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type a hash table composed of zero or more
   entries, each key among these conforms to the KEY-TYPE and affiliates
   with a value of the VALUE-TYPE, for both holding the default of the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type 'T))
  "The ``list-of'' type defines a list invested with a componency
   enumerating zero or more elements of the ELEMENT-TYPE, for thilk
   holds the comprehensive ``T'' as a default configuration."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype expression-list ()
  "The ``expression-list'' type defines an ordered list of zero or more
   ``Expression'' objects."
  '(list-of Expression))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variation on
   binary operations defined in the arithmetic realm."
  '(member :plus :minus :times :divided :remainder))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variation on
   unary operations defined in the arithmetic realm."
  '(member :plus :minus))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the conceivable membership of
   associativity configurations, this policy ostending its commodity as
   a prioritization mechanism betwixt operators entalented with
   equipolllence in their binding power while striving for a token's
   obtention."
  '(member :none :left :right))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, without the arrogation of its
   exhaustion, the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a \"generalized boolean\" and
   produces a veridicous Boolean equivalency thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT responds with ``NIL''."
  (declare (type T object))
  (the boolean (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Meow-Error (error)
  ()
  (:documentation
    "The ``Meow-Error'' condition type establishes a common foundry
     for all conditions pursuing the communications of anomalous
     situations during a Meow program's execution."))

;;; -------------------------------------------------------

(define-condition Undefined-Label-Error (Meow-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending label name.")
    :reader        undefined-label-error-offending-name
    :type          string
    :documentation "The requested label name whose absence has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Undefined-Label-Error condition))
      (declare (type destination           stream))
      (format stream "No label amenable to the name ~s is defined."
        (undefined-label-error-offending-name condition))))
  (:documentation
    "The ``Undefined-Label-Error'' condition type serves in the
     apprizal about an anomalous circumstance whose etiology ensues
     from the attempt to request a label by an undefined name."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (Meow-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending label name.")
    :reader        duplicate-label-error-offending-name
    :type          string
    :documentation "The label name whose existence during the attempt
                    of a redefinition has instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type destination           stream))
      (format stream "A label amenable to the name ~s has already ~
                      been defined."
        (duplicate-label-error-offending-name condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the
     apprizal about an anomalous circumstance whose etiology ensues
     from the attempt to define a label by a name already assigned to
     this purpose."))

;;; -------------------------------------------------------

(define-condition Missing-Exit-Instruction-Error (Meow-Error)
  ((position
    :initarg       :position
    :initform      (error "Missing position.")
    :reader        missing-exit-instruction-error-position
    :type          fixnum
    :documentation "The zero-based index into the program which
                    presented the desinent instruction ere its
                    conclusion without an \"Exit\" marker."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Exit-Instruction-Error condition))
      (declare (type destination                    stream))
      (format stream "Missing \"Exit\" instruction at position ~d ~
                      of the program."
        (missing-exit-instruction-error-position condition))))
  (:documentation
    "The ``Missing-Exit-Instruction-Error'' condition type serves in
     the communication of an anomalous circumstance whose etiology
     wones in the conclusion of a program without a prevenient \"Exit\"
     instruction as its vaunt-courier."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of Meow source code."
  (type  (error "Missing token type.")
         :type      keyword
         :read-only T)
  (value (error "Missing token value.")
         :type      T
         :read-only T))

;;; -------------------------------------------------------

(defun token-matches-type-p (token expected-type)
  "Determines whether the TOKEN's type matches the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression classes.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Expression)
  "The ``Expression'' interface furnishes a common foundry entreparted
   by all concrete classes pursuing the representation of an arithmetic
   expression in a stringently stipulated guise.")

;;; -------------------------------------------------------

(defstruct (Binary-Expression
  (:include     Expression)
  (:constructor make-binary-expression (operator
                                        left-operand
                                        right-operand)))
  "The ``Binary-Expression'' class models a binary operation, compact of
   the operator and its operand twissel."
  (operator      (error "Missing binary expression operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left binary expression operand.")
                 :type      Expression
                 :read-only T)
  (right-operand (error "Missing right binary expression operand.")
                 :type      Expression
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Group-Expression
  (:include     Expression)
  (:constructor make-group-expression (subexpression)))
  "The ``Group-Expression'' class lays its amplectation around one or
   more expressions as a ligating medium."
  (subexpression (error "Missing group subexpression.")
                 :type      Expression
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Integer-Expression
  (:include     Expression)
  (:constructor make-integer-expression (value)))
  "The ``Integer-Expression'' class applies itself to the encapsulation
   of an unsigned integer literal."
  (value (error "Missing integer expression value.")
         :type      (integer 0 *)
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Register-Expression
  (:include     Expression)
  (:constructor make-register-expression (name)))
  "The ``Register-Expression'' class furnishes an envelope around a
   register name as a participant in an arithmetic expression."
  (name (error "Missing register expression name.")
        :type      character
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Expression
  (:include     Expression)
  (:constructor make-unary-expression (operator operand)))
  "The ``Unary-Expression'' class establishes a unary operation, compact
   of the operator and its aefauld operand."
  (operator (error "Missing unary expression operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing unary expression operand.")
            :type      Expression
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Expression-Tokenizer".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Expression-Tokenizer ()
  ((source
    :initarg       :source
    :initform      (error "Missing expression tokenizer source.")
    :type          string
    :documentation "The expression string to tokenize.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The zero-based index of the currently processed
                    character in the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the zero-based CURRENT-POSITION
                    into the SOURCE.
                    ---
                    A value of ``NIL'' signifies the SOURCE's
                    exhaustion."))
  (:documentation
    "The ``Expression-Tokenizer'' class accoutres a entity imparted the
     dever of an arithmetic expression's destructuring into its
     participating tokens."))

;;; -------------------------------------------------------

(defun update-expression-tokenizer-character (tokenizer)
  "Updates the expression TOKENIZER's current character as a consequence
   of its position cursor's contemporaneous status and returns no
   value."
  (declare (type Expression-Tokenizer tokenizer))
  (with-slots (source position character) tokenizer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokenizer Expression-Tokenizer)
                                       &key)
  "Adjusts the expression TOKENIZER's current character as a consequence
   of its source and position cursor and returns no value."
  (declare (type Expression-Tokenizer tokenizer))
  (update-expression-tokenizer-character tokenizer)
  (values))

;;; -------------------------------------------------------

(defun make-expression-tokenizer (source)
  "Creates and returns a fresh ``Expression-Tokenizer'' nuncupated to
   the SOURCE's lexical analyzation."
  (declare (type string source))
  (the Expression-Tokenizer
    (make-instance 'Expression-Tokenizer :source source)))

;;; -------------------------------------------------------

(defun advance-expression-tokenizer (tokenizer)
  "Advances the expression TOKENIZER's position cursor to the next
   character in its source, if possible, and returns no value."
  (declare (type Expression-Tokenizer tokenizer))
  (with-slots (source position) tokenizer
    (declare (type string source))
    (declare (type fixnum position))
    (setf position
      (min (1+ position)
        (length source))))
  (update-expression-tokenizer-character tokenizer)
  (values))

;;; -------------------------------------------------------

(defun move-expression-tokenizer-to (tokenizer new-position)
  "Relocates the expression TOKENIZER's position cursor to the
   NEW-POSITION in its source and returns no value."
  (declare (type Expression-Tokenizer tokenizer))
  (with-slots (source position) tokenizer
    (declare (type string source))
    (declare (type fixnum position))
    (setf position
      (min new-position
        (length source))))
  (update-expression-tokenizer-character tokenizer)
  (values))

;;; -------------------------------------------------------

(defun read-expression-symbol (tokenizer token-type)
  "Reads an expression symbol represented by an aefauld character and
   categorized by the TOKEN-TYPE utilizing the expression TOKENIZER's
   context and returns a covenable token encapsulation thereof."
  (declare (type Expression-Tokenizer tokenizer))
  (declare (type keyword              token-type))
  (with-slots (character) tokenizer
    (declare (type (or null character) character))
    (the Token
      (prog1
        (make-token token-type character)
        (advance-expression-tokenizer tokenizer)))))

;;; -------------------------------------------------------

(defun read-integer-literal (tokenizer)
  "Reads an integer literal by the expression TOKENIZER's mediation and
   returns an ``:integer'' token representation thereof."
  (declare (type Expression-Tokenizer tokenizer))
  (with-slots (source position) tokenizer
    (declare (type string source))
    (declare (type fixnum position))
    (let ((end-of-number
            (or (position-if-not #'digit-char-p source :start position)
                (length source))))
      (declare (type fixnum end-of-number))
      (the Token
        (prog1
          (make-token :integer
            (parse-integer source
              :start position
              :end   end-of-number))
          (move-expression-tokenizer-to tokenizer end-of-number))))))

;;; -------------------------------------------------------

(defun get-next-expression-token (tokenizer)
  "Returns the subsequent token from the expression TOKENIZER's source.
   ---
   Upon its source's exhaustion, the TOKENIZER responds to any request
   with a fresh end-of-file (EOF) token."
  (declare (type Expression-Tokenizer tokenizer))
  (with-slots (character) tokenizer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((find character "ab" :test #'char=)
          (prog1
            (make-token :register character)
            (advance-expression-tokenizer tokenizer)))
        
        ((char= character #\+)
          (read-expression-symbol tokenizer :plus))
        
        ((char= character #\-)
          (read-expression-symbol tokenizer :minus))
        
        ((char= character #\*)
          (read-expression-symbol tokenizer :times))
        
        ((char= character #\/)
          (read-expression-symbol tokenizer :divided))
        
        ((char= character #\%)
          (read-expression-symbol tokenizer :remainder))
        
        ((digit-char-p character)
          (read-integer-literal tokenizer))
        
        ((char= character #\()
          (read-expression-symbol tokenizer :left-parenthesis))
        
        ((char= character #\))
          (read-expression-symbol tokenizer :right-parenthesis))
        
        (T
          (with-slots (source position) tokenizer
            (declare (type string source))
            (declare (type fixnum position))
            (error "Invalid character \"~c\" at position ~d of the ~
                    expression ~s."
              character position source)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((tokenizer
    :initarg       :tokenizer
    :type          Expression-Tokenizer
    :documentation "The token generation entity.")
   (next-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently acquired token from the TOKENIZER,
                    presented as the \"next\" token."))
  (:documentation
    "The ``Token-Stream'' class provides a capacitating layer upon an
     expression tokenizer, amplifying its competences."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokens Token-Stream) &key)
  "Queries a token from the TOKENS stream's underlying tokenizer, stores
   thilk in the TOKENS, and returns no value."
  (declare (type Token-Stream tokens))
  (with-slots (tokenizer next-token) tokens
    (declare (type Expression-Tokenizer tokenizer))
    (declare (type Token                next-token))
    (setf next-token
      (get-next-expression-token tokenizer)))
  (values))

;;; -------------------------------------------------------

(defun make-token-stream (tokenizer)
  "Creates and returns a fresh ``Token-Stream'' whose token acquisition
   proceeds by mediation of the expression TOKENIZER."
  (declare (type Expression-Tokenizer tokenizer))
  (the Token-Stream
    (make-instance 'Token-Stream :tokenizer tokenizer)))

;;; -------------------------------------------------------

(defun consume-token (tokens)
  "Consumes and returns the TOKENS stream's next token."
  (declare (type Token-Stream tokens))
  (with-slots (tokenizer next-token) tokens
    (declare (type Expression-Tokenizer tokenizer))
    (declare (type token                next-token))
    (the Token
      (prog1 next-token
        (setf next-token
          (get-next-expression-token tokenizer))))))

;;; -------------------------------------------------------

(defun peek-token (tokens)
  "Returns without its consumption the next token from the expression
   TOKENS stream."
  (declare (type Token-Stream tokens))
  (with-slots (next-token) tokens
    (declare (type Token next-token))
    (the Token next-token)))

;;; -------------------------------------------------------

(defun expect-token (tokens expected-type)
  "Determines whether the next token in the TOKENS stream conforms to
   the EXPECTED-TYPE, returning on confirmation the probed object;
   otherwise an error of an unspecified type is signaled."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-type))
  (the Token
    (with-slots (next-token) tokens
      (declare (type Token next-token))
      (if (token-matches-type-p next-token expected-type)
        (consume-token tokens)
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-type next-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Parselet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ((basic-binding-power
    :initarg       :basic-binding-power
    :initform      0
    :reader        parselet-basic-binding-power
    :type          integer
    :documentation "The basic binding power, which governs the fortitude
                    of a nud or led token in its agon for a
                    subexpression, destitute, however, of the
                    ASSOCIATIVITY's influence.")
   (associativity
    :initarg       :associativity
    :initform      :none
    :reader        parselet-associativity
    :type          associativity
    :documentation "The associativity, which furnishes a resolution in
                    the case of two operators endowed with equipollence
                    in their agon for a subexpression's acquisition."))
  (:documentation
    "The ``Parselet'' abstract class hafts a substratum for all classes
     whose telos appertains to the conversion of a \"null denotation\"
     (nud) or \"left denotation\" (led) token into a hierarchical
     union."))

;;; -------------------------------------------------------

(defun parselet-effective-binding-power (parselet)
  "Returns the PARSELET's effective binding power, including in its
   compass both the basic fortitude and the associativity information."
  (declare (type Parselet parselet))
  (with-slots (basic-binding-power associativity) parselet
    (declare (type integer       basic-binding-power))
    (declare (type associativity associativity))
    (the integer
      (case associativity
        ((:none :left) basic-binding-power)
        (:right        (1- basic-binding-power))
        (otherwise     (error "Invalid associativity: ~s."
                         associativity))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Nud-Parselet".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nud-Parselet (Parselet)
  ()
  (:documentation
    "The ``Nud-Parselet'' abstract class furnishes a firmament serving
     as a linchpin for the ligation of all classes intending to provide
     a transformation of a nud token to an expression representation."))

;;; -------------------------------------------------------

(defgeneric apply-nud-parselet (parselet tokens nud-token)
  (:documentation
    "Employs the nud PARSELET in order to convert the NUD-TOKEN into an
     expression equivalent, utilizing the TOKENS stream as an
     adminiculum for the contingency of further tokens' obtention, and
     returns the thus yielded expression model."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Led-Parselet".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Led-Parselet (Parselet)
  ()
  (:documentation
    "The ``Led-Parselet'' abstract class establishes a substratum for
     all classes pursuing the implementation of a \"left denotation\"
     or led parser, transforming such a token in an expression."))

;;; -------------------------------------------------------

(defgeneric apply-led-parselet (parselet
                                tokens
                                led-token
                                left-expression)
  (:documentation
    "Employs the led PARSELET in order to convert the LED-TOKEN into an
     expression equivalent, the LEFT-EXPRESSION contributing the first
     operand, while utilizing the TOKENS stream as an adminiculum for
     the contingency of further tokens' obtention, and returns the thus
     yielded expression model."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Expression-Parser".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Expression-Parser ()
  ((token-provider
    :initarg       :token-provider
    :type          Token-Stream
    :documentation "The provenance of the expression token's
                    purveyance.")
   (nud-parselets
    :initform      (make-hash-table :test #'eq)
    :type          (hash-table-of keyword Nud-Parselet)
    :documentation "Affiliates the recognized nud token types with the
                    parselets dedicated to their transformation into an
                    ``Expression''.")
   (led-parselets
    :initform      (make-hash-table :test #'eq)
    :type          (hash-table-of keyword Led-Parselet)
    :documentation "Affiliates the recognized led token types with the
                    parselets dedicated to their transformation into an
                    ``Expression''."))
  (:documentation
    "The ``Expression-Parser'' class fulfils the necessity of an
     arithmetic expression's assemblage from its tokens, their
     provenance a produce from an ``Expression-Tokenizer'''s nitency."))

;;; -------------------------------------------------------

(defun make-expression-parser (token-provider)
  "Creates and returns a fresh ``Expression-Parser'', the provenance of
   its tokens' acquisitions manifests in the TOKEN-PROVIDER."
  (declare (type Token-Stream token-provider))
  (the Expression-Parser
    (make-instance 'Expression-Parser :token-provider token-provider)))

;;; -------------------------------------------------------

(defun register-nud-parselet (parser token-type parselet)
  "Affiliates the TOKEN-TYPE as a nud signification with the nud
   PARSELET in the expression PARSER and returns no value."
  (declare (type Expression-Parser parser))
  (declare (type keyword           token-type))
  (declare (type Nud-Parselet      parselet))
  (with-slots (nud-parselets) parser
    (declare (type (hash-table-of keyword Nud-Parselet) nud-parselets))
    (setf (gethash token-type nud-parselets) parselet))
  (values))

;;; -------------------------------------------------------

(defun nud-token-p (parser token)
  "Determines whether the TOKEN represents affiliates with a nud role
   as imputed by the PARSER's definitions, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (with-slots (nud-parselets) parser
    (declare (type (hash-table-of keyword Nud-Parselet) nud-parselets))
    (the boolean
      (get-boolean-value-of
        (nth-value 1
          (gethash (token-type token) nud-parselets))))))

;;; -------------------------------------------------------

(defun get-nud-parselet (parser token)
  "Returns the ``Nud-Parselet'' dedicated to the TOKEN's handling as
   specified by the expression PARSER's definitions; or signals an error
   upon its disrespondency."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (with-slots (nud-parselets) parser
    (declare (type (hash-table-of keyword Nud-Parselet) nud-parselets))
    (the Nud-Parselet
      (or (gethash (token-type token) nud-parselets)
          (error "The object ~s does not represent a nud token."
            token)))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (parser token)
  "Returns the effective binding power affiliated with the nud TOKEN in
   the expression PARSER."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (the integer
    (parselet-effective-binding-power
      (get-nud-parselet parser token))))

;;; -------------------------------------------------------

(defun parse-nud-token (parser nud-token)
  "Parses the NUD-TOKEN in the expression PARSER's context and returns
   a connable ``Expression'' representation of the result."
  (declare (type Expression-Parser parser))
  (declare (type Token             nud-token))
  (the Expression
    (apply-nud-parselet
      (get-nud-parselet parser nud-token)
      parser
      nud-token)))

;;; -------------------------------------------------------

(defun register-led-parselet (parser token-type parselet)
  "Affiliates the TOKEN-TYPE as a led signification with the led
   PARSELET in the expression PARSER and returns no value."
  (declare (type Expression-Parser parser))
  (declare (type keyword           token-type))
  (declare (type Led-Parselet      parselet))
  (with-slots (led-parselets) parser
    (declare (type (hash-table-of keyword Led-Parselet) led-parselets))
    (setf (gethash token-type led-parselets) parselet))
  (values))

;;; -------------------------------------------------------

(defun led-token-p (parser token)
  "Determines whether the TOKEN represents affiliates with a led role
   as imputed by the PARSER's definitions, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (with-slots (led-parselets) parser
    (declare (type (hash-table-of keyword Led-Parselet) led-parselets))
    (the boolean
      (get-boolean-value-of
        (nth-value 1
          (gethash (token-type token) led-parselets))))))

;;; -------------------------------------------------------

(defun get-led-parselet (parser token)
  "Returns the ``led-Parselet'' dedicated to the TOKEN's handling as
   specified by the expression PARSER's definitions; or signals an error
   upon its disrespondency."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (with-slots (led-parselets) parser
    (declare (type (hash-table-of keyword Led-Parselet) led-parselets))
    (the led-Parselet
      (or (gethash (token-type token) led-parselets)
          (error "The object ~s does not represent a led token."
            token)))))

;;; -------------------------------------------------------

(defun get-led-binding-power (parser token)
  "Returns the effective binding power affiliated with the led TOKEN in
   the expression PARSER."
  (declare (type Expression-Parser parser))
  (declare (type Token             token))
  (the integer
    (parselet-effective-binding-power
      (get-led-parselet parser token))))

;;; -------------------------------------------------------

(defun parse-led-token (parser led-token left-expression)
  "Parses the LED-TOKEN in the expression PARSER's context, utilizing
   the LEFT-EXPRESSION in its agency as the sinitral operand, and
   returns a connable ``Expression'' representation of the result."
  (declare (type Expression-Parser parser))
  (declare (type Token             led-token))
  (the Expression
    (apply-led-parselet
      (get-led-parselet parser led-token)
      parser
      led-token
      left-expression)))

;;; -------------------------------------------------------

(defun token-can-bind-expression-p (parser
                                    candidate-token
                                    competing-binding-power)
  "Determines whether the CANDIDATE-TOKEN, its properties desumed from
   the expression PARSER's diorisms, is endowed with the competence
   and fortitude to acquire a subsequent token when confronted with an
   agonist represented by the COMPETING-BINDING-POWER, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Expression-Parser parser))
  (declare (type Token             candidate-token))
  (declare (type integer           competing-binding-power))
  (the boolean
    (get-boolean-value-of
      (and (led-token-p parser candidate-token)
           (> (get-led-binding-power parser candidate-token)
              competing-binding-power)))))

;;; -------------------------------------------------------

(defun parse-expression (parser current-binding-power)
  "Parses an expression by employing the expression PARSER, the
   invoking entity being represented by the CURRENT-BINDING-POWER, and
   returns the thus produced ``Expression''."
  (declare (type Expression-Parser parser))
  (declare (type integer           current-binding-power))
  (with-slots (token-provider) parser
    (declare (type Token-Stream token-provider))
    (let ((left-expression
            (parse-nud-token parser
              (consume-token token-provider))))
      (declare (type Expression left-expression))
      (loop
        for next-token
          of-type Token
          =       (peek-token token-provider)
        while
          (token-can-bind-expression-p
            parser next-token current-binding-power)
        do
          (consume-token token-provider)
          (setf left-expression
            (parse-led-token parser next-token left-expression)))
      (the Expression left-expression))))

;;; -------------------------------------------------------

(defun assemble-expression (parser)
  "Parses an expression utilizing the PARSER's warklumes and returns a
   covenable ``Expression'' representation thereof."
  (declare (type Expression-Parser parser))
  (the Expression
    (parse-expression parser 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Group-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Group-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Group-Parselet'' class provides a nud parselet dedicated to
     the transformation of an eligible token sequence into a
     ``Group-Expression''."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Group-Parselet)
                               (parser    Expression-Parser)
                               (nud-token Token))
  (declare (type Group-Parselet    parselet))
  (declare (ignore                 parselet))
  (declare (type Expression-Parser parser))
  (declare (type Token             nud-token))
  (the Group-Expression
    (with-slots (token-provider) parser
      (declare (type Token-Stream token-provider))
      (if (token-matches-type-p nud-token :left-parenthesis)
        (prog1
          (make-group-expression
            (parse-expression parser 0))
          (expect-token token-provider :right-parenthesis))
        (error "Expected a left parenthesis, but encountered ~s."
          nud-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Integer-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Integer-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Integer-Parselet'' class provides a nud parselet dedicated
     to the transformation of an eligible token into an
     ``Integer-Expression''."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Integer-Parselet)
                               (parser    Expression-Parser)
                               (nud-token Token))
  (declare (type Integer-Parselet parselet))
  (declare (ignore                        parselet))
  (declare (type Expression-Parser        parser))
  (declare (ignore                        parser))
  (declare (type Token                    nud-token))
  (the Integer-Expression
    (make-integer-expression
      (token-value nud-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Register-Parselet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Register-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Register-Parselet'' class provides a nud parselet dedicated
     to the transformation of an eligible token into a
     ``Register-Expression''."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Register-Parselet)
                               (parser    Expression-Parser)
                               (nud-token Token))
  (declare (type Register-Parselet parselet))
  (declare (ignore                 parselet))
  (declare (type Expression-Parser parser))
  (declare (ignore                 parser))
  (declare (type Token             nud-token))
  (the Register-Expression
    (make-register-expression
      (token-value nud-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Unary-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Unary-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Unary-Parselet'' class furnishes a nud parselet which handles
     a unary operation, producing in the course a ``Unary-Expression''
     tantamount of the same."))

;;; -------------------------------------------------------

(defmethod apply-nud-parselet ((parselet  Unary-Parselet)
                               (parser    Expression-Parser)
                               (nud-token Token))
  (declare (type Unary-Parselet    parselet))
  (declare (ignore                 parselet))
  (declare (type Expression-Parser parser))
  (declare (type Token             nud-token))
  (the Unary-Expression
    (make-unary-expression
      (token-type nud-token)
      (parse-expression parser
        (get-nud-binding-power parser nud-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Binary-Parselet".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Parselet (Led-Parselet)
  ()
  (:documentation
    "The ``Binary-Parselet'' class accoutres a led parselet nuncupated
     to a binary operation's evaluation, producing from such a
     ``Binary-Expression''."))

;;; -------------------------------------------------------

(defmethod apply-led-parselet ((parselet        Binary-Parselet)
                               (parser          Expression-Parser)
                               (led-token       Token)
                               (left-expression Expression))
  (declare (type Binary-Parselet   parselet))
  (declare (type Expression-Parser parser))
  (declare (type Token             led-token))
  (declare (type Expression        left-expression))
  (the Binary-Expression
    (make-binary-expression
      (token-type led-token)
      left-expression
      (parse-expression parser
        (get-led-binding-power parser led-token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of standard parselet registration operations. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-standard-parselets (parser)
  "Registers the standard nud and led parselets at the expression PARSER
   and returns the modified PARSER."
  (declare (type Expression-Parser parser))
  
  (register-nud-parselet parser :left-parenthesis
    (make-instance 'Group-Parselet))
  
  (register-nud-parselet parser :integer
    (make-instance 'Integer-Parselet))
  
  (register-nud-parselet parser :register
    (make-instance 'Register-Parselet))
  
  (register-nud-parselet parser :plus
    (make-instance 'Unary-Parselet
      :basic-binding-power 170
      :associativity       :right))
  
  (register-nud-parselet parser :minus
    (make-instance 'Unary-Parselet
      :basic-binding-power 170
      :associativity       :right))
  
  (register-led-parselet parser :plus
    (make-instance 'Binary-Parselet
      :basic-binding-power 130
      :associativity       :left))
  
  (register-led-parselet parser :minus
    (make-instance 'Binary-Parselet
      :basic-binding-power 130
      :associativity       :left))
  
  (register-led-parselet parser :times
    (make-instance 'Binary-Parselet
      :basic-binding-power 140
      :associativity       :left))
  
  (register-led-parselet parser :divided
    (make-instance 'Binary-Parselet
      :basic-binding-power 140
      :associativity       :left))
  
  (register-led-parselet parser :remainder
    (make-instance 'Binary-Parselet
      :basic-binding-power 140
      :associativity       :left))
  
  (the Expression-Parser parser))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   the diorism of which includes the newline, carriage return, space,
   and horizontal tab entities, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Newline #\Return #\Space #\Tab)
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-string-p (subject)
  "Determines whether the SUBJECT represents an empty string, that is,
   one tallying exactly zero (0) characters among its membership,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string subject))
  (the boolean
    (get-boolean-value-of
      (zerop (length subject)))))

;;; -------------------------------------------------------

(defun single-word-string-p (subject)
  "Determines whether the SUBJECT represents a single word, that is, a
   catena of characters not fractured by one or more whitespaces,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string subject))
  (the boolean
    (get-boolean-value-of
      (notany #'whitespace-character-p subject))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label identifier operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-label-name-p (candidate)
  "Determines whether the CANDIDATE constitutes a valid label
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (and (not (empty-string-p  candidate))
           (single-word-string-p candidate)))))

;;; -------------------------------------------------------

(defun validate-label-name (candidate)
  "Determines whether the CANDIDATE constitutes a valid label
   identifier, returning on confirmation the probed string; otherwise
   signals an error of an unspecified type."
  (declare (type string candidate))
  (the string
    (or (and (valid-label-name-p candidate)
             candidate)
        (error "The identifier ~s does not represent a valid label ~
                name."
          candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of instruction name table.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of simple-string keyword) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Affiliates the recognized Meow instruction names with representative
   keyword symbols, the so stevened \"opcodes\".")

;;; -------------------------------------------------------

(flet ((register-identifier (name opcode)
        "Associates the identifier NAME with its OPCODE, stores this
         union in the +IDENTIFIERS+ table, and returns no value.
         ---
         Any entry amenable to the NAME will be tacitly superseded by
         the new alliance."
        (declare (type simple-string name))
        (declare (type keyword       opcode))
        (setf (gethash name +IDENTIFIERS+) opcode)
        (values)))
  (register-identifier "Cat"  :cat)
  (register-identifier "Get"  :get)
  (register-identifier "Exit" :exit)
  (register-identifier "Go"   :go)
  (register-identifier "Meow" :meow)
  (register-identifier "Move" :move)
  (register-identifier "Purr" :purr)
  (register-identifier "Put"  :put)
  (values))

;;; -------------------------------------------------------

(defun resolve-instruction-name (name)
  "Returns the opcode affiliated with the instruction NAME, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type string name))
  (the keyword
    (or (gethash name +IDENTIFIERS+)
        (error "The identifier ~s does not designate an ~
                instruction."
          name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Statement-Tokenizer".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Statement-Tokenizer ()
  ((source
    :initarg       :source
    :initform      (error "Missing statement tokenizer source.")
    :type          string
    :documentation "The piece of Meow source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE.
                    ---
                    The ``NIL'' value serves as a sentinel for the
                    SOURCE's exhaustion."))
  (:documentation
    "The ``Statement-Tokenizer'' class applies itself to the discernment
     and extraction of instructions from a piece of Meow source code."))

;;; -------------------------------------------------------

(defun update-statement-tokenizer-character (tokenizer)
  "Configures the statement TOKENIZER's character as a consequence of
   its position cursor and returns no value."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (source position character) tokenizer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokenizer Statement-Tokenizer)
                                       &key)
  "Updates the TOKENIZER's current character in response to its incipial
   position cursor state and returns no value."
  (declare (type Statement-Tokenizer tokenizer))
  (update-statement-tokenizer-character tokenizer)
  (values))

;;; -------------------------------------------------------

(defun make-statement-tokenizer (source)
  "Creates and returns a fresh ``Statement-Tokenizer'' dedicated to the
   piece of Meow SOURCE code's lexical analyzation."
  (declare (type string source))
  (the Statement-Tokenizer
    (make-instance 'Statement-Tokenizer :source source)))

;;; -------------------------------------------------------

(defun move-statement-tokenizer-to (tokenizer new-position)
  "Relocates the statement TOKENIZER's position cursor to the
   NEW-POSITION and returns no value."
  (declare (type Statement-Tokenizer tokenizer))
  (declare (type fixnum              new-position))
  (with-slots (source position) tokenizer
    (declare (type string source))
    (declare (type fixnum position))
    (setf position
      (min new-position
        (length source))))
  (update-statement-tokenizer-character tokenizer)
  (values))

;;; -------------------------------------------------------

(defun advance-statement-tokenizer (tokenizer)
  "Advances the statement TOKENIZER's position cursor to the next
   character in its source, if possible, and returns no value."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (position) tokenizer
    (declare (type fixnum position))
    (move-statement-tokenizer-to tokenizer
      (1+ position)))
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, skips a sequence of zero or more accolent whitespaces and
   returns no value."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (source position character) tokenizer
    (declare (type string source))
    (declare (type fixnum position))
    (move-statement-tokenizer-to tokenizer
      (or (position-if-not #'whitespace-character-p source
            :start position)
          (length source))))
  (values))

;;; -------------------------------------------------------

(defun expect-whitespaces (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, determines whether one or more whitespaces ensue, on
   confirmation skipping thilk, while returning no value; otherwise an
   error of an unspecified type is signaled."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (character position) tokenizer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (cond
      ((null character)
        (error "Expected one or more whitespaces commencing from the
                position ~d, but found the source exhausted."
          position))
      ((not (whitespace-character-p character))
        (error "Expected one or more whitespaces commencing from the
                position ~d, but encountered \"~c\"."
          position character))
      (T
        (skip-whitespaces tokenizer))))
  (values))

;;; -------------------------------------------------------

(defun expect-character (tokenizer expected-character)
  "Determines whether the statement TOKENIZER's current character
   concurs with the EXPECTED-CHARACTER, on confirmation advancing its
   position cursor to the next location in its source, while returning
   no value; otherwise signals an error of an unspecifed type."
  (declare (type Statement-Tokenizer tokenizer))
  (declare (type character           expected-character))
  (with-slots (character position) tokenizer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (cond
      ((null character)
        (error "Expected the character \"~c\" at position ~d, ~
                but found the source exhausted."
          expected-character position))
      ((char/= character expected-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-character position character))
      (T
        (advance-statement-tokenizer tokenizer))))
  (values))

;;; -------------------------------------------------------

(defun locate-end-of-word (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, locates the nearest word's desinence and returns the index
   immediately succeeding the same."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (source position character) tokenizer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the fixnum
      (or (position-if #'whitespace-character-p source :start position)
          (length source)))))

;;; -------------------------------------------------------

(defun read-next-word (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, reads the next word and returns a fresh string representation
   thereof."
  (declare (type Statement-Tokenizer tokenizer))
  (let ((end-of-word (locate-end-of-word tokenizer)))
    (declare (type fixnum end-of-word))
    (with-slots (source position character) tokenizer
      (declare (type string source))
      (declare (type fixnum position))
      (the string
        (prog1
          (subseq source position end-of-word)
          (move-statement-tokenizer-to tokenizer end-of-word))))))

;;; -------------------------------------------------------

(defun read-instruction-name (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, reads an instruction name and returns an identifying
   representation thereof."
  (declare (type Statement-Tokenizer tokenizer))
  (the keyword
    (resolve-instruction-name
      (read-next-word tokenizer))))

;;; -------------------------------------------------------

(defun read-string-literal (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, consumes a string literal and returns a covenable
   encapsulation thereof."
  (declare (type Statement-Tokenizer tokenizer))
  (expect-character tokenizer #\")
  (with-slots (character position) tokenizer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (with-output-to-string (content)
      (declare (type string-stream content))
      (loop do
        (case character
          ((NIL)
            (error "Unterminated string literal at position ~d."
              position))
          (#\"
            (advance-statement-tokenizer tokenizer)
            (loop-finish))
          (otherwise
            (write-char                  character content)
            (advance-statement-tokenizer tokenizer)))))))

;;; -------------------------------------------------------

(defun read-expression (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, consumes an arithmetic expression and returns an
   ``Expression'' representation thereof."
  (declare (type Statement-Tokenizer tokenizer))
  (the Expression
    (assemble-expression
      (register-standard-parselets
        (make-expression-parser
          (make-token-stream
            (make-expression-tokenizer
              (read-next-word tokenizer))))))))

;;; -------------------------------------------------------

(defun read-label-name (tokenizer)
  "Proceeding from the current position into the statement TOKENIZER's
   source, consumes a label name and returns a fresh string
   representation thereof."
  (declare (type Statement-Tokenizer tokenizer))
  (the string
    (validate-label-name
      (read-next-word tokenizer))))

;;; -------------------------------------------------------

(defun program-source-exhausted-p (tokenizer)
  "Determines whether the statement TOKENIZER has processed its source
   in its entirety, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Statement-Tokenizer tokenizer))
  (with-slots (character) tokenizer
    (declare (type (or null character) character))
    (the boolean
      (null character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction)
  "The ``Instruction'' interface furnishes a firmament partaken in by
   all concrete classes nuncupated to the representation of a Meow
   statement.")

;;; -------------------------------------------------------

(defstruct (Cat-Instruction
  (:include Instruction))
  "The ``Cat-Instruction'' class establishes a representation of the
   Meow \"Cat\" statement."
  (label (error "Missing label.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Exit-Instruction
  (:include Instruction))
  "The ``Exit-Instruction'' class establishes a representation of the
   Meow \"Exit\" statement.")

;;; -------------------------------------------------------

(defstruct (Get-Instruction
  (:include Instruction))
  "The ``Get-Instruction'' class establishes a representation of the
   Meow \"Get\" statement.")

;;; -------------------------------------------------------

(defstruct (Go-Instruction
  (:include Instruction))
  "The ``Go-Instruction'' class establishes a representation of the
   Meow \"Go\" statement."
  (guard (error "Missing guard.")
         :type      Expression
         :read-only T)
  (label (error "Missing label.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Meow-Instruction
  (:include Instruction))
  "The ``Meow-Instruction'' class establishes a representation of the
   Meow \"Meow\" statement.")

;;; -------------------------------------------------------

(defstruct (Move-Instruction
  (:include Instruction))
  "The ``Move-Instruction'' class establishes a representation of the
   Meow \"Move\" statement.")

;;; -------------------------------------------------------

(defstruct (Purr-Instruction
  (:include Instruction))
  "The ``Purr-Instruction'' class establishes a representation of the
   Meow \"Purr\" statement."
  (message (error "Missing message.")
           :type      string
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Put-Instruction
  (:include Instruction))
  "The ``Put-Instruction'' class establishes a representation of the
   Meow \"Put\" statement."
  (expression (error "Missing expression.")
              :type      Expression
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing program instructions.")
    :reader        program-instructions
    :type          (simple-array Instruction (*))
    :documentation "A random-access ordered sequence of the
                    instructions comprising the program."))
  (:documentation
    "The ``Program'' class applies itself an ordered sequence of zero or
     more Meow instructions' castaldy."))

;;; -------------------------------------------------------

(defun make-program (instructions)
  "Creates and returns a fresh ``Program'' dedicated to the Meow
   INSTRUCTIONS' castaldy."
  (declare (type (list-of Instruction) instructions))
  (the Program
    (make-instance 'Program :instructions
      (coerce instructions
        '(simple-array Instruction (*))))))

;;; -------------------------------------------------------

(defun valid-program-index-p (program index)
  "Returns the instruction located at the zero-based INDEX into the
   PROGRAM."
  (declare (type Program program))
  (declare (type fixnum  index))
  (with-slots (instructions) program
    (declare (type (simple-array Instruction (*)) instructions))
      (the boolean
        (get-boolean-value-of
          (array-in-bounds-p instructions index)))))

;;; -------------------------------------------------------

(defun program-instruction-at (program index)
  "Returns the instruction located at the zero-based INDEX into the
   PROGRAM."
  (declare (type Program program))
  (declare (type fixnum  index))
  (with-slots (instructions) program
    (declare (type (simple-array Instruction (*)) instructions))
    (the Instruction (aref instructions index))))

;;; -------------------------------------------------------

(defun program-size (program)
  "Returns the tally of instructions comprising the PROGRAM."
  (declare (type Program program))
  (with-slots (instructions) program
    (declare (type (simple-array Instruction (*)) instructions))
    (the fixnum (length instructions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program-Parser".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-Parser ()
  ((tokenizer
    :initarg       :tokenizer
    :initform      (error "Missing statement tokenizer for parser.")
    :type          Statement-Tokenizer
    :documentation "The statement tokenizer whose service capacitate
                    the conversion of a piece of Meow source code into
                    a sequence of instructions."))
  (:documentation
    "The ``Program-Parser'' is apportioned the onus of actuating an
     assemblage of a Meow instructions into an ordered sequence from the
     source code's original string form."))

;;; -------------------------------------------------------

(defun make-program-parser (tokenizer)
  "Creates and returns a fresh ``Program-Parser'' whose adit to the
   underlying Meow source code is consigned to the statement TOKENIZER's
   castaldy."
  (declare (type Statement-Tokenizer tokenizer))
  (the Program-Parser
    (make-instance 'Program-Parser :tokenizer tokenizer)))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses an aefauld Meow statement utilizing the PARSER's warklumes and
   returns an ``Instruction'' representation thereof."
  (declare (type Program-Parser parser))
  (with-slots (tokenizer) parser
    (declare (type Statement-Tokenizer tokenizer))
    (skip-whitespaces tokenizer)
    (the Instruction
      (let ((current-opcode (read-instruction-name tokenizer)))
        (case current-opcode
          (:cat
            (expect-whitespaces tokenizer)
            (make-cat-instruction :label
              (read-label-name tokenizer)))
          (:exit
            (make-exit-instruction))
          (:get
            (make-get-instruction))
          (:go
            (expect-whitespaces tokenizer)
            (make-go-instruction
              :guard (read-expression tokenizer)
              :label
                (progn
                  (expect-whitespaces tokenizer)
                  (read-label-name    tokenizer))))
          (:meow
            (make-meow-instruction))
          (:move
            (make-move-instruction))
          (:purr
            (expect-whitespaces tokenizer)
            (make-purr-instruction :message
              (read-string-literal tokenizer)))
          (:put
            (expect-whitespaces tokenizer)
            (make-put-instruction :expression
              (read-expression tokenizer)))
          (otherwise
            (error "Unrecognized opcode: ~s." current-opcode)))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a Meow instruction sequence utilizing the PARSER and returns
   a fresh ``Program'' representation comprehending the same."
  (declare (type Program-Parser parser))
  (with-slots (tokenizer) parser
    (declare (type Statement-Tokenizer tokenizer))
    (the Program
      (make-program
        (loop
          initially
            (skip-whitespaces tokenizer)
          until (program-source-exhausted-p tokenizer) collect
            (prog1
              (parse-statement parser)
              (skip-whitespaces tokenizer)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Label-Table".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((entries
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of string fixnum)
    :documentation "Maps a Meow program's label names to the zero-based
                    indices of their definitions."))
  (:documentation
    "The ``Label-Table'' class serves in the edification of a mapping
     betwixt a Meow program's label names and their allied zero-based
     indices into the ensconcing instruction sequence."))

;;; -------------------------------------------------------

(defun make-empty-label-table ()
  "Creates and returns a fresh ``Label-Table'' whose initial state
   amounts to a perfect vacancy."
  (the Label-Table
    (make-instance 'Label-Table)))

;;; -------------------------------------------------------

(defun contains-label-p (labels name)
  "Determines whether the LABELS table entails definition amenable to
   the NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (with-slots (entries) labels
    (declare (type (hash-table-of string fixnum) entries))
    (the boolean
      (get-boolean-value-of
        (nth-value 1
          (gethash name entries))))))

;;; -------------------------------------------------------

(defun define-label (labels name position)
  "Allies the label amenable to the NAME with the zero-based POSITION
   into its comprehending program, stores thilk in the LABELS table,
   and returns no value."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (declare (type fixnum      position))
  (if (contains-label-p labels name)
    (error 'Duplicate-Label-Error :offending-name name)
    (with-slots (entries) labels
      (declare (type (hash-table-of string fixnum) entries))
      (setf (gethash name entries) position)))
  (values))

;;; -------------------------------------------------------

(defun locate-label (labels name)
  "Returns the zero-based position affiliated with the label NAME in
   the LABELS table."
  (declare (type Label-Table labels))
  (declare (type string      name))
  (with-slots (entries) labels
    (declare (type (hash-table-of string fixnum) entries))
    (the fixnum
      (if (contains-label-p labels name)
        (gethash name entries)
        (error 'Undefined-Label-Error :offending-name name)))))

;;; -------------------------------------------------------

(defun build-label-table (program)
  "Creates and returns a fresh ``Label-Table'' the name-position
   diorisms of which are desumed from the Meow PROGRAM."
  (declare (type Program program))
  (let ((labels (make-empty-label-table)))
    (declare (type Label-Table labels))
    (loop
      for current-instruction
        of-type Instruction
        across  (program-instructions program)
      and instruction-index
        of-type fixnum
        from    0
        by      1
      when (cat-instruction-p current-instruction) do
        (define-label labels
          (cat-instruction-label current-instruction)
          instruction-index))
    (the Label-Table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary and binary operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-unary-operator-handler (operator)
  "Returns a functional object capacitated with the competence to
   represent the unary OPERATOR."
  (declare (type unary-operator operator))
  (the function
    (case operator
      (:plus  #'+)
      (:minus #'-)
      (otherwise
        (error "Unrecognized unary operator: ~s." operator)))))

;;; -------------------------------------------------------

(defun get-binary-operator-handler (operator)
  "Returns a functional object capacitated with the competence to
   represent the binary OPERATOR.
   ---
   Please heed that certain functions, such as ``round'' and ``rem'',
   return two or more values, in which case an elision of the
   supererogatory components, usually accomplished via the ``nth-value''
   operation, might impose a sensible option."
  (declare (type binary-operator operator))
  (the function
    (nth-value 0
      (case operator
        (:plus      #'+)
        (:minus     #'-)
        (:times     #'*)
        (:divided   #'truncate)
        (:remainder #'rem)
        (otherwise
          (error "Unrecognized binary operator: ~s." operator))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-byte-from-standard-input ()
  "Reads a character from the standard input and returns its ASCII code,
   or, upon the conduit's exhaustion, responds with the sentinel -1.
   ---
   In any case, whether an obtention's patration or failure, the input
   conduit will be purged."
  (let ((input-character (read-char NIL NIL NIL)))
    (declare (type (or null character) input-character))
    (the fixnum
      (prog1
        (or (and input-character
                 (char-code input-character))
            -1)
        (clear-input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Meow program.")
    :type          Program
    :documentation "The parsed Meow program's instructions.")
   (labels
    :initform      (make-empty-label-table)
    :type          Label-Table
    :documentation "Associates the PROGRAM's label names with their
                    zero-based indices into the PROGRAM.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The zero-based current instruction pointer (IP)
                    position.")
   (exit-requested-p
    :initform      NIL
    :type          boolean
    :documentation "Memorizes whether a Meow \"Exit\" instruction has
                    been issued, which constitutes a requisitum for the
                    program's successful conclusion.")
   (register-a
    :initform      0
    :type          integer
    :documentation "The value stored in the primary register \"a\".")
   (register-b
    :initform      0
    :type          integer
    :documentation "The value stored in the secondary register \"b\"."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     actual efficacy to a Meow program supplied in the static form of
     its instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Collects the labels partaking of the INTERPRETER's internally managed
   program, stores the same in the INTERPRETER's label table, and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program labels) interpreter
    (declare (type Program     program))
    (declare (type Label-Table labels))
    (setf labels (build-label-table program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Meow
   PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction located at the INTERPRETER's instruction
   pointer (IP) index."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Program program))
    (declare (type fixnum ip))
    (the Instruction (program-instruction-at program ip))))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the Meow program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
    (with-slots (program ip) interpreter
      (declare (type Program program))
      (declare (type fixnum ip))
      (the boolean
        (not (valid-program-index-p program ip)))))

;;; -------------------------------------------------------

(defun program-terminated-p (interpreter)
  "Determines whether the Meow program consigned to the INTERPRETER's
   castaldy has halted, either as an ultimity begotten by its complete
   instruction list's procession, or as a consequence of an explicit
   \"Exit\" behest, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (exit-requested-p ip) interpreter
    (declare (type boolean exit-requested-p))
    (the boolean
      (get-boolean-value-of
        (or (program-exhausted-p interpreter)
            exit-requested-p)))))

;;; -------------------------------------------------------

(defun request-program-exit (interpreter)
  "Marks the INTERPRETER's explicit \"Exit\" instruction encounter flag
   as true and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (exit-requested-p) interpreter
    (declare (type boolean exit-requested-p))
    (setf exit-requested-p T))
  (values))

;;; -------------------------------------------------------

(defun ascertain-correct-program-termination (interpreter)
  "Determines whether the INTERPRETER's program is exhausted and whether
   this status transpires coextensive with an explicit \"Exit\" behest,
   on confirmation returning no value; otherwise an error of the type
   ``Missing-Exit-Instruction-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (with-slots (exit-requested-p ip) interpreter
    (declare (type boolean exit-requested-p))
    (declare (type fixnum  ip))
    (when (and (program-exhausted-p interpreter)
               (not exit-requested-p))
      (error 'Missing-Exit-Instruction-Error :position ip)))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in the underlying program and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type Program program))
    (declare (type fixnum  ip))
    (setf ip
      (min (1+ ip)
        (program-size program))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-label (interpreter name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the label
   amenable to the NAME in the program consigned to its castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (with-slots (labels ip) interpreter
    (declare (type Label-Table labels))
    (declare (type fixnum      ip))
    (setf ip (locate-label labels name)))
  (values))

;;; -------------------------------------------------------

(defun register-value (interpreter name)
  "Returns the value stored in the INTERPRETER register amenable to
   the NAME."
  (declare (type Interpreter interpreter))
  (declare (type character   name))
  (with-slots (register-a register-b) interpreter
    (declare (type integer register-a))
    (declare (ignorable    register-a))
    (declare (type integer register-b))
    (declare (ignorable    register-b))
    (the integer
      (case name
        (#\a register-a)
        (#\b register-b)
        (otherwise (error "Invalid register name: \"~c\"." name))))))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value interpreter name)
  "Stores the value in the INTERPRETER register amenable to the NAME
   and returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (declare (type character   name))
  (with-slots (register-a register-b) interpreter
    (declare (type integer register-a))
    (declare (ignorable    register-a))
    (declare (type integer register-b))
    (declare (ignorable    register-b))
    (the integer
      (case name
        (#\a (setf register-a new-value))
        (#\b (setf register-b new-value))
        (otherwise (error "Invalid register name: \"~c\"." name))))))

;;; -------------------------------------------------------

(defgeneric evaluate-expression (interpreter expression)
  (:documentation
    "Evaluates the arithmetic EXPRESSION in the INTERPRETER's context
     and returns a result conable for this combination.")
  
  (:method ((interpreter Interpreter)
            (expression  Binary-Expression))
    (declare (type Interpreter       interpreter))
    (declare (type Binary-Expression expression))
    (the integer
      (funcall
        (get-binary-operator-handler
          (binary-expression-operator expression))
        (evaluate-expression interpreter
          (binary-expression-left-operand expression))
        (evaluate-expression interpreter
          (binary-expression-right-operand expression)))))
  
  (:method ((interpreter Interpreter)
            (expression  Group-Expression))
    (declare (type Interpreter      interpreter))
    (declare (type Group-Expression expression))
    (the integer
      (evaluate-expression interpreter
        (group-expression-subexpression expression))))
  
  (:method ((interpreter Interpreter)
            (expression  Integer-Expression))
    (declare (type Interpreter        interpreter))
    (declare (ignore                  interpreter))
    (declare (type Integer-Expression expression))
    (the (integer 0 *)
      (integer-expression-value expression)))
  
  (:method ((interpreter Interpreter)
            (expression  Register-Expression))
    (declare (type Interpreter         interpreter))
    (declare (type Register-Expression expression))
    (the integer
      (register-value interpreter
        (register-expression-name expression))))
  
  (:method ((interpreter Interpreter)
            (expression  Unary-Expression))
    (declare (type Interpreter      interpreter))
    (declare (type Unary-Expression expression))
    (the integer
      (funcall
        (get-unary-operator-handler
          (unary-expression-operator expression))
        (evaluate-expression interpreter
          (unary-expression-operand expression))))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (instruction Cat-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (ignore               interpreter))
    (declare (type Cat-Instruction instruction))
    (declare (ignore               instruction))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Exit-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Exit-Instruction instruction))
    (declare (ignore                instruction))
    (request-program-exit interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Get-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Get-Instruction instruction))
    (declare (ignore               instruction))
    (format T "~&>> ")
    (finish-output)
    (setf (register-value interpreter #\a)
      (read-byte-from-standard-input))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Go-Instruction))
    (declare (type Interpreter    interpreter))
    (declare (type Go-Instruction instruction))
    (when (minusp
            (evaluate-expression interpreter
              (go-instruction-guard instruction)))
      (jump-to-label interpreter
        (go-instruction-label instruction)))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Meow-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Meow-Instruction instruction))
    (declare (ignore                instruction))
    (format T "~c"
      (code-char
        (register-value interpreter #\a)))
    (finish-output)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Move-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Move-Instruction instruction))
    (declare (ignore                instruction))
    (rotatef
      (register-value interpreter #\a)
      (register-value interpreter #\b))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Purr-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (ignore                interpreter))
    (declare (type Purr-Instruction instruction))
    (format T "~a"
      (purr-instruction-message instruction))
    (finish-output)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction Put-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Put-Instruction instruction))
    (setf (register-value interpreter #\a)
      (evaluate-expression interpreter
        (put-instruction-expression instruction)))
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Meow program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop
    until (program-terminated-p interpreter) do
      (process-instruction interpreter
        (get-current-instruction interpreter))
      (advance-to-next-instruction interpreter)
    finally
      (ascertain-correct-program-termination interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Meow (code)
  "Interprets the piece of Meow source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program
        (make-program-parser
          (make-statement-tokenizer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates upon the standard input
;; conduit's exhaustion.
(interpret-Meow
  "Cat loop
   Get
   Meow
   Go a exit
   Go -1 loop
   Cat exit Exit")

;;; -------------------------------------------------------

;; Print the message "Hello, World".
(interpret-Meow "Purr \"Hello, World\" Exit")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Meow
  "Get
   Move
   Put b-48
   Cat loop
   Move Meow Move
   Go -a loop
   Exit")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-Meow
"
Put 99
Cat loop

Move
Put 48+(b/10) Meow
Put 48+(b%10) Meow

Purr \" bottles of beer on the wall,
\"

Put 48+(b/10) Meow
Put 48+(b%10) Meow

Purr \" bottles of beer.
Take 1 down,
Pass it around,
\"

Move
Put a-1
Move

Put 48+(b/10) Meow
Put 48+(b%10) Meow

Move

Purr \" bottles of beer!

\"

Go -(a-2) loop

Purr \"2 bottles of beer on the wall,
2 bottles of beer.
Take 1 down,
Pass it around,
1 bottle of beer on the wall, 

1 bottle of beer on the wall,
1 bottle of beer.
Take 1 down,
Pass it around,
0 bottles of beer on the wall,
\"

Exit
")

;;; -------------------------------------------------------

;; "FizzBuzz" program.
(interpret-Meow
"
Put 0
Cat loop
Put a+1
Go (a%15)-1 fizzbuzz
Go (a%3)-1 fizz
Go (a%5)-1 buzz
Go -1 num
Exit

Cat fizzbuzz
Purr \"FizzBuzz
\" Go a-999 loop Exit

Cat fizz
Purr \"Fizz
\" Go a-999 loop Exit

Cat buzz
Purr \"Buzz
\" Go a-999 loop Exit

Cat num
Move

Put 48+(b/100%10) Meow
Put 48+(b/10%10)  Meow
Put 48+(b%10)     Meow
Purr \"
\"

Move
Go a-999 loop Exit
")

;;; -------------------------------------------------------

;; Factorial which admits an input from the closed interval [0, 9].
(interpret-Meow
"
Get Meow
Purr \"! = \"
Put a-48
Move Put 1 Move

Cat loop
Move Put a*b Move
Put a-1
Go -(a-1) loop


Put 48+(b/100000%10) Meow
Put 48+(b/10000%10)  Meow
Put 48+(b/1000%10)   Meow
Put 48+(b/100%10)    Meow
Put 48+(b/10%10)     Meow
Put 48+(b%10)        Meow

Exit
")
