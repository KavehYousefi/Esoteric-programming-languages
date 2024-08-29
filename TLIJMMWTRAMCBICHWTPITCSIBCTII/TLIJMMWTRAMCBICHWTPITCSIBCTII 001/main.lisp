;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TLIJMMWTRAMCBICHWTPITCSIBCTII", invented by the Esolang
;; user "ChuckEsoteric08" and presented on April 23rd, 2023, the diorism
;; of which wones in a septuple of instructions dedicated to a set of
;; 20 registers' perquisition and modulations, operating on non-negative
;; integer objects.
;; 
;; 
;; Concept
;; =======
;; The TLIJMMWTRAMCBICHWTPITCSIBCTII programming language's foundry is
;; realized in a set of 20 registers, non-negative in their capacity,
;; and initialized to a state of zero (0) at the program's inchoation,
;; upon which arithmetic instructions, jump-based goto constructs, and
;; input/output facilities operate.
;; 
;; == TLIJMMWTRAMCBICHWTPITCSIBCTII: BEWRAYING ITS DEVELOPMENT ==
;; The language's agnomination, "TLIJMMWTRAMCBICHWTPITCSIBCTII",
;; constitutes an abbreviated form of the phrase "This Language Is Just
;; Minsky Machine With Twenty Registers And More Commands, But I Choose
;; Hard Way To Prove It Turing Complete So I Wrote Bitwise Cyclic Tag
;; Interpreter In It", with a stark mete of tenability bewraying the
;; modus of its development, as conflating its protolog's aefauld
;; example program.
;; 
;; == THE MEMORY: TWENTY NON-NEGATIVE INTEGER-VALUED REGISTERS ==
;; The data castaldy constitutes an onus assigned to 20 registers,
;; each such serving as a scalar integer number's salvatory, the
;; circumference of this value commencing with the minimum of zero (0),
;; and extending towards the positive infinity, the lower extremum
;; concomitantly imposing the default state.
;; 
;; 
;; Architecture
;; ============
;; The TLIJMMWTRAMCBICHWTPITCSIBCTII programming language enumerates a
;; tally of 20 registers, its amenability to access requests capacitated
;; via zero-based indices, and its capacity measured with a scalar
;; non-negative integer number greater than zero, but without any
;; imposition along the positive axis. Every cell commences in a state
;; of zero (0).
;; 
;; 
;; Data Types
;; ==========
;; An aefauld data type's participation registers in the context of the
;; language, this comprises by non-negative integer numbers, their
;; dispansion's lower extremum measured with the inclusive zero (0)
;; members, while no maximum's purview applies to this realm.
;; 
;; 
;; Syntax
;; ======
;; Exercising a syntactical species of conspectuity, a
;; TLIJMMWTRAMCBICHWTPITCSIBCTII program establishes a composition of
;; zero or more instructions, everichon among these drawing its
;; sustenance from one through three operands, the realm of which
;; appertains to register identifiers and non-negative integers.
;; 
;; == INSTRUCTIONS: ABBREVIATED BEHESTS ==
;; A command identifier embraces betwixt either a triad or tetrad of
;; characters, serving as curtailed equivalencies of the respective
;; operative purposes.
;; 
;; Ensuing from this dioristic component, an argument list, its operands
;; ensconced in a jumelle of parenthesis, "(" and ")".
;; 
;; == OPERANDS: REGISTER INDICES OR NON-NEGATIVE INTEGERS ==
;; An instruction argument or operand may assume one of a twissel of
;; contingencies, scilicet:
;; 
;;   (a) A register identifier, desumed from the integral range of
;;       [0, 19], furnishing the amenability of the 20 data storages.
;;   
;;   (b) A non-negative integer number, its commorance the dispansion
;;       of values greater than or equal to zero (0), with no upper
;;       bourne's imposition.
;; 
;; In circumstances imposing a multitude of arguments, the parameter
;; list's synartesis ensues from a conjunction by commas.
;; 
;; == WHITESPACES ==
;; Whitespaces, a diorism whose perimeter ensconces the space,
;; horizontal tab, and newline entity, enjoys its tolerance in the
;; interstices betwixt any tokens.
;; 
;; == COMMENTS ==
;; No provision for comments participates in the current language
;; rendition.
;; 
;; == GRAMMAR ==
;; A treatise on the language's donat shall be the following Extended
;; Backus-Naur Form (ENBF) description's cynosure:
;; 
;;   program            := { command | whitespaces } ;
;;   command            := copy | dec | div | inc | inp | mul | out ;
;;   
;;   copy               := "copy" , regPairArgs ;
;;   dec                := "dec"  , regIntJumpArgs ;
;;   div                := "div"  , regIntJumpArgs ;
;;   inc                := "inc"  , regIntArgs ;
;;   inp                := "inp"  , regArg ;
;;   mul                := "mul"  , regIntArgs ;
;;   out                := "out"  , regArg ;
;;   
;;   regArg             := "(" , registerId , ")" ;
;;   regPairArgs        := "(" , registerId , "," , registerId , ")" ;
;;   regIntArgs         := "(" , registerId
;;                      ,        ","
;;                      ,        nonNegativeInteger , ")"
;;                      ;
;;   regIntJumpArgs     := "(" , registerId
;;                      ,        ","
;;                      ,        nonNegativeInteger
;;                      ,        ","
;;                      ,        nonNegativeInteger , ")"
;;                      ;
;;   
;;   registerId         := digit , [ digit ] ;
;;   nonNegativeInteger := digit , { digit } ;
;;   
;;   whitespaces        := { whitespace } ;
;;   whitespace         := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A septuple membership is enumerated in the
;; TLIJMMWTRAMCBICHWTPITCSIBCTII programming language's operative
;; faculties, subsuming into its services basic arithmetics, two
;; jump-based conditional control flow conductors, as well as numeric
;; input and output conduits.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall be a compendious mete of gnarity's parcery
;; anent the language's instruction set.
;; 
;; Please heed that succedaneous segments are underlined via an asterisk
;; ("*") and intended for their substitution by actual
;; TLIJMMWTRAMCBICHWTPITCSIBCTII code fragments in the program's
;; ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   inc(r,a)   | Increments the register amenable to the index {r}
;;       * *    | by the amount {a}.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | {a} must specify a non-negative integer number.
;;   ..................................................................
;;   dec(r,a,t) | If the value stored in the register amenable to the
;;       * * *  | index {r} is greater than the value {a}, decrements
;;              | the register's value by {a}; otherwise relocates the
;;              | instruction pointer (IP) to the instruction at the
;;              | zero-based position {t}.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | {a} must specify a non-negative integer number.
;;              |------------------------------------------------------
;;              | {t} must specify a non-negative integer number.
;;   ..................................................................
;;   mul(r,f)   | Multiplies the register amenable to the index {r}
;;       * *    | by the factor {f}.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | {f} must specify a non-negative integer number.
;;   ..................................................................
;;   dec(r,d,t) | If the value {d} constitutes an aliquot of the value
;;       * * *  | stored in the register amenable to the index {r},
;;              | divides the register's value by {d}; otherwise
;;              | relocates the instruction pointer (IP) to the
;;              | instruction at the zero-based position {t}.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | {d} must specify a non-negative integer number.
;;              |------------------------------------------------------
;;              | {t} must specify a non-negative integer number.
;;   ..................................................................
;;   inp(r)     | Queries the standard input for a non-negative integer
;;       *      | number and stores the same in the register amenable
;;              | to the index {r}.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | If the standard input response does not constitute a
;;              | valid integer number, an undefined behavior ensues.
;;   ..................................................................
;;   out(r)     | Prints the value stored in the register amenable to
;;       *      | the index {r} in its verbatim numeric form to the
;;              | standard output.
;;              |------------------------------------------------------
;;              | {r} must specify a valid register identifier.
;;   ..................................................................
;;   copy(s,d)  | Copies the value stored in the register amenable to
;;        * *   | the index {s} to the register with the index {d}.
;;              |------------------------------------------------------
;;              | {s} must specify a valid register identifier.
;;              |------------------------------------------------------
;;              | {d} must specify a valid register identifier.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Despite the protolog's lucid explications, a few elements of
;; ambiguous assessment remain, a subset thereof shall be communicated
;; in the following sections.
;; 
;; == ARE NEGATIVE INTEGER NUMBERS HOMOLOGATED? ==
;; The language standard does not state in express terms whether
;; arguments and user inputs may be transmitted as unsigned integers
;; or extended to their signed counterparts.
;; 
;; Extrapolated from the specification's sole forbisen, a bitwise cyclic
;; tag (BCT) interpreter, and the appertaining impositions, it has been
;; conjectured that merely non-negative integer numbers enjoy their
;; admission into the program.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, its realization a merist that produces several tiers of
;; operative departments, commencing in the
;; TLIJMMWTRAMCBICHWTPITCSIBCTII source string's tokenization via a
;; lexer, these parcels' assemblage into instruction objects by a
;; parser's adminiculum, and the resulting command sequence's evaluation
;; through the actual interpreter class.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-08-27
;; 
;; Sources:
;;   [esolang2023TLIJMMWTRAMCBICHWTPITCSIBCTII]
;;   The Esolang contributors, "TLIJMMWTRAMCBICHWTPITCSIBCTII",
;;     April 23rd, 2023
;;   URL: "https://esolangs.org/wiki/TLIJMMWTRAMCBICHWTPITCSIBCTII"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass of which embraces, without the pursuit of its exhaustion, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype register-id ()
  "The ``register-id'' type defines a register identifier or index as
   an integral number in the closed interval [0, 19], thus covering the
   span of 20 available, zero-indexed registers in the
   TLIJMMWTRAMCBICHWTPITCSIBCTII program memory."
  '(integer 0 19))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral number greater
   than or equal to zero, but disencumbered from an upper extremum,
   thus being an occupant of the range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable
   TLIJMMWTRAMCBICHWTPITCSIBCTII program as a one-dimensional simple
   array of ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type *))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, its
   default chosen as the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (every
                #'(lambda (current-element)
                    (declare (type T current-element))
                    (typep current-element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\"
   truth value, returning for a non-``NIL'' input a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of validating operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-register-id (candidate)
  "Determines whether the CANDIDATE number represents a valid register
   identifier, returning on confirmation the CANDIDATE itself; otherwise
   signals an error of an unspecified type."
  (declare (type integer candidate))
  (the register-id
    (if (typep candidate 'register-id)
      candidate
      (error "The number ~d does not represent a valid register ~
              identifier."
        candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted during the lexical evaluation of a piece of
   TLIJMMWTRAMCBICHWTPITCSIBCTII source code, carrying in its diorism
   a twissel of a categorizing type and a detailing value."
  (type  (error "Missing token type.")
         :type      keyword
         :read-only T)
  (value (error "Missing token value.")
         :type      T
         :read-only T))

;;; -------------------------------------------------------

(defun token-of-type-p (token expected-type)
  "Determines whether the TOKEN complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



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
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-identifier (identifier)
  "Parses the TLIJMMWTRAMCBICHWTPITCSIBCTII identifier and returns a
   token representation thereof."
  (declare (type string identifier))
  (flet ((probe-identifier (expected-identifier token-type)
          "Determines whether the IDENTIFIER is tantamount to the
           EXPECTED-IDENTIFIER, on confirmation returning a fresh token
           which combines the TOKEN-TYPE with the IDENTIFIER; otherwise
           responds with ``NIL''."
          (declare (type simple-string expected-identifier))
          (declare (type keyword       token-type))
          (the (or null Token)
            (when (string= identifier expected-identifier)
              (make-token token-type expected-identifier)))))
    (the Token
      (or (probe-identifier "inc"  :inc)
          (probe-identifier "dec"  :dec)
          (probe-identifier "mul"  :mul)
          (probe-identifier "div"  :div)
          (probe-identifier "inp"  :inp)
          (probe-identifier "out"  :out)
          (probe-identifier "copy" :copy)
          (error "Unrecognized identifier: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class accommodates a lexical analyzer, the dever of
   which amounts to the extraction of significant objects from a piece
   of TLIJMMWTRAMCBICHWTPITCSIBCTII source code and their furnishment
   as ``Token'' objects."
  (source    (error "Missing lexer source.")
             :type      string
             :read-only T)
  (position  (error "Missing lexer position.")
             :type      fixnum
             :read-only NIL)
  (character (error "Missing lexer character.")
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char (lexer-source lexer)
        (incf (lexer-position lexer)))))
  (values))

;;; -------------------------------------------------------

(defun write-lexer-character (lexer destination)
  "Writes the LEXER's current character to the DESTINATION, advances its
   position cursor to the next position in its source, if possible, and
   returns no value."
  (declare (type Lexer       lexer))
  (declare (type destination destination))
  (write-char (lexer-character lexer) destination)
  (advance-to-next-character lexer)
  (values))

;;; -------------------------------------------------------

(defun current-character-matches-p (lexer predicate)
  "Determines whether the LEXER's current character satisfies the
   PREIDCATE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Lexer                    lexer))
  (declare (type (function (character) *) predicate))
  (the boolean
    (get-boolean-value-of
      (when (lexer-character lexer)
        (funcall predicate
          (lexer-character lexer))))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop
            while (current-character-matches-p lexer #'digit-char-p)
            do    (write-lexer-character lexer digits)))))))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Creates and returns a fresh token combining the TOKEN-TYPE with the
   LEXER's current character as the affiliated value, while
   concomitantly advancing the LEXER's position cursor to the next
   character in its source, if possible."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (make-token token-type
      (prog1
        (lexer-character           lexer)
        (advance-to-next-character lexer)))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (parse-identifier
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop
          while (current-character-matches-p lexer #'alpha-char-p)
          do    (write-lexer-character lexer identifier))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (loop
    while (current-character-matches-p lexer #'whitespace-character-p)
    do    (advance-to-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhasution, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-eof-token))
      
      ((current-character-matches-p lexer #'whitespace-character-p)
        (skip-whitespaces lexer)
        (get-next-token   lexer))
      
      ((current-character-matches-p lexer #'digit-char-p)
        (read-number lexer))
      
      ((current-character-matches-p lexer #'alpha-char-p)
        (read-identifier lexer))
      
      ((char= (lexer-character lexer) #\()
        (read-symbol lexer :left-parenthesis))
      
      ((char= (lexer-character lexer) #\))
        (read-symbol lexer :right-parenthesis))
      
      ((char= (lexer-character lexer) #\,)
        (read-symbol lexer :comma))
      
      (T
        (error "Invalid symbol \"~c\" at position ~d."
          (lexer-character lexer)
          (lexer-position  lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:copier NIL))
  "The ``Instruction'' abstract class accoutres a common substratum for
   all classes pursuing the representation of
   TLIJMMWTRAMCBICHWTPITCSIBCTII instructions, itself restricted to the
   castaldy of a register as the ligating object of deliberation for
   all operations."
  (register (error "Missing instruction register.")
            :type      register-id
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Inc-Instruction
  (:include Instruction)
  (:constructor make-inc-instruction (register amount)))
  "The ``Inc-Instruction'' class serves in the encapsulation of the
   \"inc\" operation, compact of the manipulated register and the
   accruement amount."
  (amount (error "Missing incrementation around.")
          :type      non-negative-integer
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Dec-Instruction
  (:include Instruction)
  (:constructor make-dec-instruction (register amount target)))
  "The ``Dec-Instruction'' class serves in the encapsulation of the
   \"dec\" operation, compact of the perquired and contingently
   manipulated register, the deduction amount, which serves in a
   twifaced agency as the stimulation guard, and a jump target index."
  (amount (error "Missing decrementation around.")
          :type      non-negative-integer
          :read-only T)
  (target (error "Missing decrementation target.")
          :type      non-negative-integer
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Mul-Instruction
  (:include Instruction)
  (:constructor make-mul-instruction (register factor)))
  "The ``Mul-Instruction'' class serves in the encapsulation of the
   \"mul\" operation, compact of the modified register and the
   multiplication factor, engaged therein as the multiplier."
  (factor (error "Missing multiplication factor.")
          :type      non-negative-integer
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Div-Instruction
  (:include Instruction)
  (:constructor make-div-instruction (register divisor target)))
  "The ``Div-Instruction'' class serves in the encapsulation of the
   \"div\" operation, compact of the modified register, a divisor for
   the contingent division stage, and a jump target."
  (divisor (error "Missing division divisor.")
           :type      non-negative-integer
           :read-only T)
  (target  (error "Missing division target.")
           :type      non-negative-integer
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Inp-Instruction
  (:include Instruction)
  (:constructor make-inp-instruction (register)))
  "The ``Inp-Instruction'' class serves in the encapsulation of the
   \"inp\" operation, specified merely by the receiving register.")

;;; -------------------------------------------------------

(defstruct (Out-Instruction
  (:include Instruction)
  (:constructor make-out-instruction (register)))
  "The ``Out-Instruction'' class serves in the encapsulation of the
   \"out\" operation, specified merely by the receiving register.")

;;; -------------------------------------------------------

(defstruct (Copy-Instruction
  (:include Instruction)
  (:constructor make-copy-instruction (register destination)))
  "The ``Copy-Instruction'' class serves in the encapsulation of the
   \"copy\" operation, compact of the source and the destination
   register."
  (destination (error "Missing copy destination.")
               :type      register-id
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' from the list of
   INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser
    (lexer
     &aux (current-token (get-next-token lexer)))))
  "The ``Parser'' class is apportioned the dever of assembling from a
   stream of tokens a sequence of ``Instruction''s representing a
   TLIJMMWTRAMCBICHWTPITCSIBCTII program's operative expression."
  (lexer         (error "Missing lexer.")
                 :type      Lexer
                 :read-only T)
  (current-token (error "Missing current token.")
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defun current-token-of-type-p (parser expected-token-type)
  "Determines whether the PARSER's current token complies to the
   EXPECTED-TOKEN-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the boolean
    (token-of-type-p
      (parser-current-token parser)
      expected-token-type)))

;;; -------------------------------------------------------

(defun consume-current-token (parser)
  "Returns the PARSER's current token, while concomitantly querying the
   next one from the underlying lexer and storing the same in its lieu
   in the PARSER."
  (declare (type Parser parser))
  (the Token
    (prog1
      (parser-current-token parser)
      (setf (parser-current-token parser)
        (get-next-token
          (parser-lexer parser))))))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token complies with the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying the next one from the underlying lexer
   and storing the same in the PARSER; otherwise signals an error of an
   unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
    (if (current-token-of-type-p parser expected-token-type)
      (consume-current-token parser)
      (error "The token ~s is not of the expected type ~s."
        (parser-current-token parser)
        expected-token-type))))

;;; -------------------------------------------------------

(defun parse-register-id (parser)
  "Parses a register identifier in the PARSER's context and returns a
   numeric representation thereof."
  (declare (type Parser parser))
  (the register-id
    (validate-register-id
      (token-value
        (expect-token parser :number)))))

;;; -------------------------------------------------------

(defun parse-number (parser)
  "Parses a non-negative integer number in the PARSER's context and
   returns a numeric representation thereof."
  (declare (type Parser parser))
  (the non-negative-integer
    (token-value
      (expect-token parser :number))))

;;; -------------------------------------------------------

(defun parse-inc-instruction (parser)
  "Parses an \"inc\" instruction in the PARSER's context returns an
   ``Inc-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :inc)
  (expect-token parser :left-parenthesis)
  (the Inc-Instruction
    (make-inc-instruction
      (prog1
        (parse-register-id parser)
        (expect-token      parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-dec-instruction (parser)
  "Parses a \"dec\" instruction in the PARSER's context returns a
   ``Dec-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :dec)
  (expect-token parser :left-parenthesis)
  (the Dec-Instruction
    (make-dec-instruction
      (prog1
        (parse-register-id parser)
        (expect-token      parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-mul-instruction (parser)
  "Parses a \"mul\" instruction in the PARSER's context returns an
   ``Mul-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :mul)
  (expect-token parser :left-parenthesis)
  (the Mul-Instruction
    (make-mul-instruction
      (prog1
        (parse-register-id parser)
        (expect-token      parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-div-instruction (parser)
  "Parses a \"div\" instruction in the PARSER's context returns a
   ``Div-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :div)
  (expect-token parser :left-parenthesis)
  (the Div-Instruction
    (make-div-instruction
      (prog1
        (parse-register-id parser)
        (expect-token      parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :comma))
      (prog1
        (parse-number parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-inp-instruction (parser)
  "Parses an \"inp\" instruction in the PARSER's context returns an
   ``Inp-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :inp)
  (expect-token parser :left-parenthesis)
  (the Inp-Instruction
    (make-inp-instruction
      (prog1
        (parse-register-id parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-out-instruction (parser)
  "Parses an \"out\" instruction in the PARSER's context returns an
   ``Out-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :out)
  (expect-token parser :left-parenthesis)
  (the Out-Instruction
    (make-out-instruction
      (prog1
        (parse-register-id parser)
        (expect-token parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-copy-instruction (parser)
  "Parses a \"copy\" instruction in the PARSER's context returns a
   ``Copy-Instruction'' representation thereof."
  (declare (type Parser parser))
  (expect-token parser :copy)
  (expect-token parser :left-parenthesis)
  (the Copy-Instruction
    (make-copy-instruction
      (prog1
        (parse-register-id parser)
        (expect-token      parser :comma))
      (prog1
        (parse-register-id parser)
        (expect-token      parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Parses an instruction in the PARSER's context and returns a connable
   representation thereof."
  (declare (type Parser parser))
  (the Instruction
    (case (token-type (parser-current-token parser))
      (:inc
        (parse-inc-instruction parser))
      (:dec
        (parse-dec-instruction parser))
      (:mul
        (parse-mul-instruction parser))
      (:div
        (parse-div-instruction parser))
      (:inp
        (parse-inp-instruction parser))
      (:out
        (parse-out-instruction parser))
      (:copy
        (parse-copy-instruction parser))
      (otherwise
        (error "No instruction token: ~s."
          (parser-current-token parser))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a sequence of instructions in the PARSER's context and returns
   a ``program'' representation thereof."
  (declare (type Parser parser))
  (the program
    (make-program
      (loop until (current-token-of-type-p parser :eof) collect
        (parse-instruction parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-register-bank ()
  "Creates and returns a fresh vector of 20 registers, everichon among
   these a non-negative integer datum's conditory, amenable to a
   zero-based index."
  (the (simple-array non-negative-integer (20))
    (make-array 20
      :element-type    'non-negative-integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-modify-macro multiply (multiplier)
  *
  "Destructively modifies a place via its multiplication by the
   MULTIPLIER and returns the product.")

;;; -------------------------------------------------------

(define-modify-macro divide (divisor)
  round
  "Destructively modifies a place via its division by the DIVSIOR and
   returns the quotient.")

;;; -------------------------------------------------------

(defun aliquot-p (dividend divisor)
  "Determines whether the DIVISOR constitutes an aliquot of the
   DIVIDEND, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type non-negative-integer dividend))
  (declare (type non-negative-integer divisor))
  (the boolean
    (get-boolean-value-of
      (zerop
        (rem dividend divisor)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun receive-input ()
  "Queries the standard input for a line and returns two values:
     (1) The received string line in its ipsissima verba form.
     (2) The result of parsing the received string line, which either
         produces a signed integer number or the ``NIL'' sentinel upon
         its evaluation's failure."
  (let ((raw-input (read-line NIL NIL "0")))
    (declare (type string raw-input))
    (clear-input)
    (the (values string (or null integer))
      (values raw-input
        (ignore-errors
          (parse-integer raw-input))))))

;;; -------------------------------------------------------

(defun query-for-integer ()
  "Queries the standard input for a non-negative integer number until
   such has been delivered and returns the same."
  (the non-negative-integer
    (loop do
      (format T "~&>> ")
      (finish-output)
      (multiple-value-bind (raw-input parsed-input)
          (receive-input)
        (declare (type string            raw-input))
        (declare (type (or null integer) parsed-input))
        (cond
          ((null parsed-input)
            (format T "~&~s is not a non-negative integer number."
              raw-input))
          ((not (typep parsed-input 'non-negative-integer))
            (format T "~&~s is not a non-negative integer number."
              parsed-input))
          (T
            (return parsed-input)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (program)))
  "The ``Interpreter'' class applies itself to the accompassing of
   efficacy to a TLIJMMWTRAMCBICHWTPITCSIBCTII program supplied in the
   form of instructions."
  (program   (error "Missing program for the interpreter.")
             :type      program
             :read-only T)
  (ip        0
             :type      non-negative-integer
             :read-only NIL)
  (registers (make-register-bank)
             :type      (simple-array non-negative-integer (20))
             :read-only T))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the program maintained by the INTERPRETER is
   exhausted, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (not
        (array-in-bounds-p (interpreter-program interpreter)
          (interpreter-ip interpreter))))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the currently active instruction in the INTERPRETER's
   program."
  (declare (type Interpreter interpreter))
  (the Instruction
    (aref (interpreter-program interpreter)
      (interpreter-ip interpreter))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the subsequent
   position in its program and returns no value."
  (declare (type Interpreter interpreter))
  (unless (program-exhausted-p interpreter)
    (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun jump-to (interpreter new-position)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   zero-based NEW-POSITION and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter) new-position)
  (values))

;;; -------------------------------------------------------

(defun register-value (interpreter register-id)
  "Returns the non-negative integer number stored in the INTERPRETER
   register amenable to the REGISTER-ID."
  (declare (type Interpreter interpreter))
  (declare (type register-id register-id))
  (the non-negative-integer
    (aref (interpreter-registers interpreter) register-id)))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value interpreter register-id)
  "Stores the NEW-VALUE in the INTERPRETER register amenable to the
   REGISTER-ID and returns no value."
  (declare (type non-negative-integer new-value))
  (declare (type Interpreter          interpreter))
  (declare (type register-id          register-id))
  (setf (aref (interpreter-registers interpreter) register-id)
        new-value)
  (values))

;;; -------------------------------------------------------

(defun instruction-register-value (interpreter instruction)
  "Returns the non-negative integer number stored in the INTERPRETER
   register amenable to the register identifier maintained by the
   INSTRUCTION."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (the non-negative-integer
    (register-value interpreter
      (instruction-register instruction))))

;;; -------------------------------------------------------

(defun (setf instruction-register-value) (new-value
                                          interpreter
                                          instruction)
  "Stores the NEW-VALUE in the INTERPRETER register amenable to the
   register identifier maintained by the INSTRUCTION and returns no
   value."
  (declare (type non-negative-integer new-value))
  (declare (type Interpreter          interpreter))
  (declare (type Instruction          instruction))
  (setf (register-value interpreter
          (instruction-register instruction))
        new-value)
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter) (instruction Inc-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Inc-Instruction instruction))
    (incf (instruction-register-value interpreter instruction)
      (inc-instruction-amount instruction))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction Dec-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Dec-Instruction instruction))
    (cond
      ((> (instruction-register-value interpreter instruction)
          (dec-instruction-amount     instruction))
        (decf (instruction-register-value interpreter instruction)
          (dec-instruction-amount instruction))
        (advance-program interpreter))
      (T
        (setf (interpreter-ip interpreter)
          (dec-instruction-target instruction))))
    (values))
  
  (:method ((interpreter Interpreter) (instruction Mul-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Mul-Instruction instruction))
    (multiply (instruction-register-value interpreter instruction)
      (mul-instruction-factor instruction))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction Div-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Div-Instruction instruction))
    (cond
      ((aliquot-p
          (instruction-register-value interpreter instruction)
          (div-instruction-divisor    instruction))
        (divide (instruction-register-value interpreter instruction)
          (div-instruction-divisor instruction))
        (advance-program interpreter))
      (T
        (jump-to interpreter
          (div-instruction-target instruction))))
    (values))
  
  (:method ((interpreter Interpreter) (instruction Inp-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Inp-Instruction instruction))
    (setf (instruction-register-value interpreter instruction)
      (query-for-integer))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction Out-Instruction))
    (declare (type Interpreter     interpreter))
    (declare (type Out-Instruction instruction))
    (format T "~&~d"
      (instruction-register-value interpreter instruction))
    (advance-program interpreter)
    (values))
  
  (:method ((interpreter Interpreter) (instruction Copy-Instruction))
    (declare (type Interpreter      interpreter))
    (declare (type Copy-Instruction instruction))
    (setf (register-value interpreter
            (copy-instruction-destination instruction))
      (instruction-register-value interpreter instruction))
    (advance-program interpreter)
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-exhausted-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-TLIJMMWTRAMCBICHWTPITCSIBCTII (code)
  "Interprets the piece of TLIJMMWTRAMCBICHWTPITCSIBCTII source code
   and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time numeric cat program.
(interpret-TLIJMMWTRAMCBICHWTPITCSIBCTII
  "inp(0)
   out(0)")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-TLIJMMWTRAMCBICHWTPITCSIBCTII
  "inp(0)
   out(0)
   dec(1,0,0)")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-TLIJMMWTRAMCBICHWTPITCSIBCTII
  "inp(0)
   dec(0,0,4)
   out(0)
   dec(0,1,2)
   out(0)")

;;; -------------------------------------------------------

;; Bitwise cyclic tag (BCT) interpreter.
(interpret-TLIJMMWTRAMCBICHWTPITCSIBCTII
  "
  inc(5,1)
  inp(0)
  copy(0,1)
  inp(2)
  div(0,10,7)
  mul(0,10)
  dec(4,1,8)
  inc(3,1)
  out(2)
  dec(0,2,17)
  inc(0,2)
  div(0,10,21)
  div(2,10,14)
  dec(4,1,8)
  dec(2,1,63)
  div(2,10,63)
  dec(4,1,8)
  dec(2,2,63)
  inc(2,2)
  copy(1,0)
  dec(4,1,8)
  dec(0,2,60)
  inc(0,1)
  div(0,10,63)
  div(0,10,37)
  dec(2,2,35)
  inc(2,2)
  div(2,10,30)
  mul(5,10)
  dec(4,1,25)
  dec(2,1,63)
  div(2,10,63)
  mul(5,10)
  inc(5,1)
  dec(4,1,25)
  mul(2,10)
  dec(4,1,50)
  dec(0,1,63)
  dec(2,2,48)
  inc(2,2)
  div(2,10,43)
  mul(5,10)
  dec(4,1,25)
  dec(2,1,63)
  div(2,10,63)
  mul(5,10)
  inc(5,1)
  dec(4,1,38)
  mul(2,10)
  inc(2,1)
  dec(5,2,8)
  inc(5,2)
  div(5,10,55)
  mul(2,10)
  dec(4,1,50)
  dec(5,1,63)
  div(5,10,63)
  mul(2,10)
  inc(2,1)
  dec(4,1,50)
  dec(3,1,25)
  inc(3,1)
  dec(4,1,38)
  ")
