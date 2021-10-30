;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the inchoate esoteric
;; programming language "TEPCS", invented by the Esolang user "Cortex".
;; 
;; Concept
;; =======
;; TEPCS, with the name being an acronym for its most significant
;; characters, tilde, exclamation point, caret, and semicolon, belongs
;; to the family of esoteric programming languages. Its original
;; specification being marked as incomplete and in explicit terms
;; abandoned by its creator, the Esolang user "Cortex", the following
;; limns a rendition with modest application of extrapolation.
;; 
;; 
;; Architecture
;; ============
;; The TEPCS programming language does not incorporate a concept for
;; a particular data structure, as opposed to stack-based, tape-bound or
;; other models established upon a particular storage foundation. Merely
;; the stewardship of the variable maintenance reckons as an imposition,
;; a scantiness that does not inflict an expensitve oncost, demanding a
;; simple associative relationship betwixt name-value pairs.
;; 
;; 
;; Syntax
;; ======
;; The kenspeckle trait molded into TEPCS constitutes the reliance on
;; four special symbols: tilde ("~"), exclamation mark ("!"),
;; caret ("^"), and semicolon (";"). These ordained as instruction
;; constituents, all other characters may, of course, be incorporated
;; into the arguments, or "inputs", which include numbers, strings, and
;; variable names, permissive also as comment text.
;; 
;; == INSTRUCTIONS ==
;; The tilde "~" introduces a command, with its presence acting in the
;; agency of a designation, and the tally of consecutive occurrences
;; denominating the actual type. Each command starts with one to five
;; instances of this symbol in immediate adjacency, followed by zero or
;; more arguments, and terminated by an ecphoneme ("!"). Factually,
;; command names exist in the form of tilde sequences only, not
;; descriptive names as in most other programming languages.
;; 
;; Prior to a thorough disquisition, the following apercu shall project
;; the roles of these four fundamental tokens:
;; 
;;   Character | Effect
;;   ----------+-------------------------------------------------------
;;    ~        | Introduces a command, with the tally of consecutive
;;             | tildes tantamount to a command name.
;;   ..................................................................
;;    !        | Terminates a command.
;;   ..................................................................
;;    ^        | Introduces and separates inputs.
;;   ..................................................................
;;    ;        | Braces a variable name in order to distinguish it from
;;             | a literal string in contexts permissive to both kinds
;;             | of input.
;; 
;; Instructions dependent upon arguments, called "inputs" in TEPCS,
;; signal their expectency by preceding each such data item by a single
;; caret ("^"). Such provisions may be realized as numeric objects,
;; strings, or variables.
;; 
;; Commands are eventually terminated by statement of a single ecphoneme
;; ("!"). Note that, upon perusal of a piece of code, two or more such
;; symbols may flock in succession, usually at the desinence of a print
;; instruction, with the final instance being the actual terminal.
;; 
;; == NUMBERS ==
;; Literal numbers are distinguished from strings by their composition
;; of digits only, permitting at most one fraction separator in the
;; form of a dot ("."), and optionally a prefix sign from plus ("+")
;; and minus ("-"). The presence of the separating dot indicates a
;; numeric constant of the floating-point type, whereas its omission
;; defaults to an integer.
;; 
;; == STRINGS ==
;; String literals are constructed from a sequence of one or more
;; characters, the first of which must be a letter or an underscore.
;; Thus introduced, the following characters are not inflicted with
;; restrictions beside the eschwing of the command constituents "~",
;; "!", and "^". Strings are not demarcated especially, as is the case
;; in most programming language involving quotation marks.
;; 
;; == VARIABLES ==
;; Variables are stated in the program by mediation of their name, which
;; conforms to the same rules as string literals, which see. This
;; verisimilitude enforces some method of disambiguition where both
;; types of values are tolerated: This is achieved by enclosing the
;; variable name in one semicolon on each laterality, yielding:
;;   ;variableName;
;; 
;; The bipartite nature of TEPCS in construe of and distinguishment
;; betwixt string literals and variable names homologates an equivalency
;; in their content, that is, variables may be agnominated as liberally
;; as strings, tolerating, among others, whitespaces and hyphens.
;; 
;; In the general case a variable must be demarcated by semicolons if
;; and only if the context admits string values, in which circumstances
;; the discrimination between an identifier and a literal character
;; sequence loses its power of disambiguation. Concretely, the following
;; situations prescribe a special marking of variables:
;; 
;;   (a) The variable arguments of the print command "~^".
;;   (b) The value (second argument) of a variable declaration "~~^".
;;   (c) The repetition count induced into the loop end command
;;       "~~~~~^e^...!" --- albeit its evaluation, destined to be
;;       discarded, does not bear any practical utility.
;; 
;; This peculiarity in notation, as much as an encumbrance in regards to
;; convolution --- and albeit not an explicit enunciation in the original
;; specification ---, contributes to the language the ability to describe
;; any argument (input) in terms of both a literal and a variable
;; portion, a trait occupied and driven to perfection in the Tcl
;; programming language. In TEPCS, even variable declarations and the
;; choice between a loop start or end statement, being plain arguments,
;; can avail placeholders.
;; 
;; == EBNF ==
;; The grammar of TEPCS can be expressed in the following Extended
;; Backus-Naur Form (EBNF) diagram:
;; 
;;   program             := programStartCommand , { command } ;
;;   command             := printCommand
;;                       |  varDeclareCommand
;;                       |  programStartCommand
;;                       |  arithmeticCommand
;;                       |  loopCommand ;
;;   
;;   printCommand        := "~^" , { expression } , "!" ;
;;   varDeclareCommand   := "~~^" , stringExpression , "^" , expression , "!" ;
;;   programStartCommand := "~~~!" ;
;;   arithmeticCommand   := "~~~~^" , numericExpression , "^" , string , "^" , operator , "!" ;
;;   loopCommand         := loopStartCommand , { command } , loopEndCommand ;
;;   loopStartCommand    := "~~~~~^s^" , numericExpression , "!" ;
;;   loopEndCommand      := "~~~~~^e^" , expression , "!" ;
;;   comment             := ";^!" , { character } , "\n" ;
;;   
;;   expression          := number  | string | identifiedVariable ;
;;   stringExpression    := string  | identifier ;
;;   numericExpression   := number  | identifier ;
;;   
;;   operator            := "+" | "-" | "*" | "/" ;
;;   identifiedVariable  := ";" , identifier , ";" ;
;;   string              := character , { character } ;
;;   number              := [ "+" | "-" ] , digit , { digit } 
;;                       |  [ "+" | "-" ] , digit , "." , { digit } ;
;;   identifier          := character | character , { character } ;
;;   
;;   character           := "A" | ... | "Z" | "a" | ... | "z" | "0" | ... | "9" | ... ;
;;   digit               := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; TEPCS instructions are recognized by the presence of one or more
;; tildes in immediate adjacency, the tally of which determines the
;; command type, acting in the agency of its identifier or name. The
;; language enumerates among the output-only species, that is, data may
;; be printed but not induced into a program by mediation of the user.
;; If relating of "inputs" please bear in mind that this terminology is
;; an idiom of TEPCS's and a homonym of "argument" or "parameter" among
;; typical languages in currency.
;; 
;; == OVERVIEW ==
;; The following tabular exposition limns the pith of TEPCS's
;; instruction set:
;; 
;;   Instruction  | Description
;;   -------------+----------------------------------------------------
;;    ~^x!        | Prints the sequence of zero or more arguments to
;;                | the standard output, with each item being a number,
;;                | string, or a variable, the latter case of which
;;                | requires a demarcation by semicolons in order to
;;                | distinguish it from a string literal. A linebreak is
;;                | always appended automatically.
;;   ..................................................................
;;    ~~^x^y!     | Declares or assigns the variable "x" with the value
;;                | "y". "x" must be a string literal or a variable,
;;                | in that case marked by semicolons. "y" can be any
;;                | value, including a variable name, again
;;                | necessitating a demarcation. If "x" describes an
;;                | already extant variable, its value is overwritten.
;;   ..................................................................
;;    ~~~!        | Starts the program. This instruction must precede
;;                | any other command, permitted only a prolog of
;;                | comments. No program may contain this instruction
;;                | more than once.
;;   ..................................................................
;;    ~~~~^x^y^z! | Performs an arithmetic binary operation supplying
;;                | the left operand "x" and the right operand "y" to
;;                | the operator "z". "x" and "y" might be a number
;;                | literal or a variable, requiring no demarcation.
;;                | "z" must be a string literal or a demarcated
;;                | variable; in any case only the following values may
;;                | be stated or produced:
;;                |   "+" --- addition
;;                |   "-" --- subtraction
;;                |   "*" --- multiplication
;;                |   "/" --- division.
;;   ..................................................................
;;    ~~~~~^x^y!  | Defines the start or end of a loop which shall be
;;                | repeated "y" times. "x" must be a string literal
;;                | or a demarcated variable, resolving to either "s"
;;                | or "e", the former instantiates a loop start, the
;;                | latter an end. "y" must be a number literal or a
;;                | non-demarcated variable, resolving to an integer
;;                | value >= 0. All statements between a start-end pair
;;                | define the loop body.
;; 
;; == START PROGRAM ==
;; The "~~~!" instruction, a mandatory statement, instigates the
;; commencement of a TEPCS program. This instruction must be present as
;; the first in the source code, preceded only, if at all, by comments.
;; Its duplicate occurrence is as little tolerated as its absence, with
;; both violations causing a "DuplicateProgramStartError" error.
;; 
;; == PRINT ==
;; The "~!" command accepts zero or more inputs and prints them to the
;; standard output in the order of their encounter. Variables must be
;; demarced using semicolons and are substituted by their value. A
;; terminating linebreak is appended to the output, even if no arguments
;; have been presented.
;; 
;; == VARIABLE DECLARATION ==
;; Variables are declared using "~~!", associating with the chosen
;; identifier, either a string or a demarcated variable whose value must
;; be a valid identifier string itself, a scalar object of the numeric
;; or string type. If a variable shall assume another variable's
;; content, the data source must be enclosed in semicolons. Any variable
;; must be declared before it can be queried or modified. Already extant
;; variables may be overwritten by a declaration, discrepancies in the
;; types are ignored.
;; 
;; == BINARY OPERATION ==
;; Number literals and variables are subjected to arithmetic operations
;; using the "~~~~!" command. Being exclusively binary operations, both
;; the left and right operand must resolve to a numeric object,
;; requiring in the case of a variable on either side no special marking
;; with semicolons. If the left operand constitutes a variable, its
;; value is destructively modified, otherwise, at the current rendition
;; of the language, no effect takes place, as literals possess the
;; characteristic of constancy; future versions might, however,
;; introduce true expressions which are amenable to returning a value.
;; 
;; The operand must be provided in the form of a string or a variable,
;; the ambiguity being an incentive to the marking of variables with
;; semicolons. In any case, the operator obtained by this process must
;; concur with one of the following four:
;; 
;;   Character | Operator
;;   --------------------------
;;    +        | addition
;;   ..........................
;;    -        | subtraction
;;   ..........................
;;    *        | multiplication
;;   ..........................
;;    /        | division
;; 
;; Note that a division by zero, partaking of the veridical nature of
;; the mathematical definition, results in an error of the type
;; "ArithmeticError".
;; 
;; == ITERATION ==
;; An iteration construct is stated using the "~~~~~!" command,
;; receiving from its two arguments intelligence about start or stop and
;; number of repetitions. The first input, a string or a variable
;; resolving to a string, and in the latter case mandatorily ensconced
;; with semicolons, specifies whether a loop body shall commence or
;; terminate. If producing the string "s" a loop head is assumed, if
;; yielding "e" the current loop is marked as closed; any other value
;; is proscribed and will cause an "InvalidInputError".
;; 
;; The number of iterations is conveyed as an integer literal or a
;; variable responding with such. The tally must be a non-negative value
;; greater than or equal to zero, the minimum inciting an omission
;; of the loop body statements. Any non-conforming argument will inflict
;; an "InvalidInputError".
;; 
;; Each loop start ("~~~~~^s^...!") must be matched with a loop
;; terminator ("~~~~~^e^...!"), between the jumelle a sequence of zero
;; or more instructions and comments are permitted to reside, processed
;; in the order of the encounter. Nested loops are supported.
;; 
;; 
;; Comments
;; ========
;; The incorporation of comments in the source code is exercised by a
;; preceding with the character sequence ";^!". The original
;; specification ascribes its effect to the supersession of an input;
;; a literal adoption would, however, constrict the utility of remarks
;; to infrequent applications. Therefore, comments conforming to this
;; document are permitted a statement at any position in the program,
;; extending to the end of the line.
;; 
;; 
;; Exceptions
;; ==========
;; An extension of the TEPCS programming language, several exceptions
;; have been introduced as warklooms for signaling invalid processes and
;; states during the execution of a program. Additional errors may
;; occur, but inhabit a region of sufficient generality as to avoid
;; conflation with the nature of the TEPCS language. These types
;; include especially lexing and parsing anomalies. The connate signals
;; account for these:
;; 
;;   ArithmeticError
;;     Is signaled if an arithmetic operation, usually in the course of
;;     the "~~~~!" instruction, cannot be completed because of a
;;     mathematical violation. The current TEPCS version bears the
;;     potential to react in such a way to any of these four cases:
;;       (1) One or both operators do not resolve to a numeric object.
;;       (2) The operand cannot be verified as a recognized member.
;;       (3) A division by a divisor of zero has been attempted.
;;       (4) The operation result reaches beyond the limits of
;;           representable numbers. This situation belongs to the
;;           implementation-dependent reactions, as TEPCS does not
;;           impose any stipulations regarding the magnitude of such
;;           objects.
;;  
;;  DuplicateProgramStartError
;;    Is thrown if a program start instruction ("~~~!") has been
;;    encountered after the program has already been started, that is,
;;    such an instruction has been processed before.
;;  
;;  InvalidInputError
;;    Signals than an input (argument) does not produce a value
;;    appropriate for the expected use. The violation might be founded
;;    upon an incompatible type, for instance a string as a binary
;;    operand, or an invalid value, as a loop control not equal to "s"
;;    or "e".
;;  
;;  MissingProgramStartError
;;    Is thrown if an instruction is encountered before the program
;;    start command ("~~~!").
;;  
;;  UndeclaredVariableError
;;    Signals that a variable name, not yet been declared, has been
;;    queried for reading or writing purposes.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Occupying a state of inchoation, the details of TEPCS's behavior
;; are inflicted with several ambiguities and lacunae, the most
;; conspicuous of which are enumerated in this location, conjoined with
;; the currently assigned practices for their rectification.
;; 
;; == DOES OUTPUT INVOLVE IMPLICIT LINEBREAKS? ==
;; It is not stated whether the printed content is preceded and/or
;; succeeded by one or more linebreaks. The examples, in particular the
;; "99 bottles of beer" program, strongly suggest the case for a single
;; newline as a suffix to each output operation. This stance is
;; implemented in the interpreter.
;; 
;; == IS VARIABLE DECLARATION A PREREQUISITE TO VARIABLE USE? ==
;; TEPCS incorporates an instruction for the declaration of variables.
;; Being not as evolved as a statically typed language, TEPCS might or
;; might not follow the philosophy of permitting access to undeclared
;; variables for reading and writing, assuming some default value ---
;; a caract not alien to dynamically typed and scripting languages. In
;; the face of the examples using declarations for variables prior to
;; their usage, the implementation assumes this step an obligation.
;; 
;; == MAY VARIABLES BE DECLARED MULTIPLE TIMES? ==
;; The exercise of a variable declaration instantiates the only moment
;; permissive for the value, be it a string, number, or another
;; variable, to be freely chosen, as the sole alternative means, the
;; arithmetic operations, extend a very restricted set, targeting
;; exclusively numeric objects. If the declaration would be valid for
;; the same variable multiple times, its assessment for reuse were to
;; be valorized strongly. On the other hand, the boundaries betwixt
;; declaration and assignment, on one side, and strongly typed variables
;; on the other will be blurred. The personal choice has been to not
;; constrict declarations in any kind.
;; 
;; == ARE VARIABLES SCOPED? ==
;; The modestly elaborated TEPCS ecosystem does not intrude into many
;; territories relevant to the inquest about scopes; the sole exception
;; resides in the iteration construct. The overall simplicity observed
;; in the language does not warrant an otherwise imputation than a
;; global scope. This postulation has been realized in the
;; implementation.
;; 
;; == MAY COMMENTS APPEAR ANYWHERE? ==
;; Comments, in stringent adherence to the original specification, may
;; occur exclusively in lieu of inputs (arguments), restricting their
;; utility, while definitely describing a terminating position. The
;; benefits of a universal construe prevail in usefulness as well as
;; acquaintance, thus the implementation allows a comment to be
;; introduced at any location in the source, but extending to the end
;; of the line, thus either occupying a horizontal expansion of its own
;; or succeeding a completed instruction.
;; 
;; 
;; Implementation
;; ==============
;; In its intrinsics, the TEPCS programming language is not founded upon
;; a particular architecture, alleviating the design of the
;; implementation in this way. Storage facilities are merely
;; conceptualized in variables. Their simplicity, being disencumbered
;; from the requirements appertaining to scope and strong typification,
;; may be expressed in a concrete data structure by a mapping from
;; string variable names to objects of the number or string type.
;; 
;; Actual investments must be issued into the topic of the iteration
;; construct. The necessity of a loop start command's union with a
;; terminating opposite, even in the case of no repetitions that
;; enforces an omission of the interim content, manifests in a dedicated
;; ``TEPCS-Loop'' class and a stack managing objects of this ilk.
;; Encountering an iteration starter, a new ``TEPCS-Loop'' is created
;; and pushed unto the stack. The matching process betwixt a
;; commencement and terminator is relayed to the arrangement of this
;; collection, as the end instruction queries the topmost element,
;; either returning to the loop body's start position for repeated
;; execution or advancing the code while concomitantly removing the
;; recently indagated ``TEPCS-Loop'' from the stack top. By relocating
;; the instruction pointer to the loop body instead of reprocessing the
;; instruction head itself, redundancy in object creations, in
;; conjunction with the resulting corruption of the start-end matching,
;; is obviated.
;; 
;; In pseudocode nomenclature, the following actions are practised:
;;   
;;   if is loop start then
;;     let newLoopObject <- makeTEPCSLoop (currentPosition)
;;     push newLoopObject into loopStack
;;   else if is loop end then
;;     let currentLoopObject <- get topmost element from loopStack
;;     if currentLoopObject.repetitions > 0 then
;;       move instruction pointer to currentLoopObject.startOfBody
;;     else
;;       pop topmost element from loopStack
;;       advance instruction pointer beyond this loop end command
;;     end if
;;   else
;;     { Process the command. }
;;   end if
;; 
;; Some mete of adversary inhabits the zero repetitions case: Statements
;; must be skipped, juxtaposing contingent nested loops in order to
;; discriminate the currently active one's terminator from inner
;; pairs, until the matching loop end has been discovered. The
;; possibility of comments and the necessity of consuming inputs
;; requires a processing of interim contents, however, without imparting
;; them effective power. Each such visited instruction checks whether
;; the active loop's skipping flag is set, and upon affirmation abstains
;; from actually manifesting its action. A local variable, ``level'',
;; increases with each loop start encountered and decreases when met
;; with a terminator. The first loop end occurrence coinciding with a
;; level value of zero is designated the active iteration's match,
;; concluding the skipping process and advancing beyond the end
;; instruction.
;; 
;; Considering the variegation of error categories, please consider that
;; this implementation resorts to a Procrustean approach involving a
;; single, not directly specified type, benefiting from simplicity while
;; violating its fidelity to the extended language standard. Future
;; renditions shall adjust to conformance as a higher priority.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-29
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/TEPCS"
;;   -> "https://esolangs.org/wiki/User:Cortex"
;;       o User page of the member "Cortex" containing the proclamation
;;         of his abandonment.
;;   -> "https://www.php.net/manual/en/language.variables.variable.php"
;;       o Describes variable variables, that is, the possibility in PHP
;;         to access a variable by a name provided by another variable.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack (&optional (element-type T))
  "The ``stack'' type defines a last-in-first-out (LIFO) data structure
   in the form of a list of zero or more elements, each all of which
   must conform to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T'' type."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, all keys of which must conform to the KEY-TYPE and the
   associated values to the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being   the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant object obtained by
   analyzing a piece of source code."
  (type  NIL :type (or null keyword))
  (value NIL :type T))

;;; -------------------------------------------------------

(defmacro with-token ((type-variable value-variable) token-expression
                      &body body)
  "Evaluates the TOKEN-EXPRESSION to a ``Token'', binds its type to
   the TYPE-VARIABLE and its value to the VALUE-VARIABLE, executes the
   BODY forms, and returns the last evaluated form."
  (let ((evaluated-token (gensym)))
    (declare (type symbol evaluated-token))
    `(let ((,evaluated-token ,token-expression))
       (declare (type Token ,evaluated-token))
       (symbol-macrolet
           ((,type-variable  (token-type  ,evaluated-token))
            (,value-variable (token-value ,evaluated-token)))
         ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      NIL
    :type          (or null string)
    :documentation "")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation ""))
  (:documentation
    "The ``Lexer'' class analyzes a source string in order to retrieve
     significant tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source start-position position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (when (and source (plusp (length source)))
      (setf character      (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which operates on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (cond
      ((< position (1- (length source)))
        (incf position)
        (setf character (char source position)))
      (T
        (setf character NIL))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-peek (lexer)
  "Returns the next character from the LEXER, or ``NIL'' if the code
   would be exhausted."
  (declare (type Lexer lexer))
  (with-slots (source position) lexer
    (declare (type string source))
    (declare (type fixnum position))
    (the (or null character)
      (when (< position (1- (length source)))
        (char source (1+ position))))))

;;; -------------------------------------------------------

(defun lexer-follows (lexer expected-character)
  "Checks whether the next character obtained from the LEXER equals the
   expected CHARACTER and returns a ``boolean'' value of ``T'' on
   affirmation or ``NIL'' on mismatch."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (position character) lexer
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (let ((next-character (lexer-peek lexer)))
      (declare (type (or null character) next-character))
      (or (and (null next-character)
               (null expected-character))
          (and next-character
               expected-character
               (char= next-character expected-character))))))

;;; -------------------------------------------------------

(defun lexer-eat (lexer expected-character)
  "Checks whether the current character obtained from the LEXER equals
   the EXPECTED-CHARACTER, on affirmation advancing to the next
   character and returning the modified LEXER, and signaling an error
   on mismatch."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (position character) lexer
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (if (char= character expected-character)
      (lexer-advance lexer)
      (error "Expected ~s but encountered ~s at position ~d."
        expected-character character position)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Starting at the LEXER's current position, reads a string and returns
   a new ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (position character) lexer
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the Token
      (make-token :string
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop do
            (cond
              ((null character)
                (error "Unterminated string at position ~d." position))
              ((char= character #\Newline)
                (loop-finish))
              ((char= character #\^)
                (loop-finish))
              ((char= character #\\)
                (lexer-advance lexer)
                (write-char character identifier))
              ((and (char= character #\!)
                    (lexer-follows lexer #\!))
                (write-char character identifier)
                (lexer-advance lexer))
              ((and (char= character #\!)
                    (not (lexer-follows lexer #\!)))
                (loop-finish))
              ((char= character #\;)
                (loop-finish))
              (T
                (write-char character identifier)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current LEXER position, reads a variable name
   enclosed in semicolons and returns a ``Token'' representation
   thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (lexer-eat lexer #\;)
    (let ((var-name (token-value (lexer-read-string lexer))))
      (declare (type string var-name))
      (lexer-eat lexer #\;)
      (the Token (make-token :identifier var-name)))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current LEXER position, reads a number and return
   a ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((digits (make-string-output-stream)))
      (declare (type string-stream digits))
      
      (flet ((read-sign ()
              (when (find character "+-" :test #'char=)
                (write-char character digits)
                (lexer-advance lexer))
              (values))
             
             (read-integer-part ()
              (loop while (and character (digit-char-p character)) do
                (write-char character digits)
                (lexer-advance lexer))
              (values))
             
             (read-decimal-part ()
              (when (char= character #\.)
                (write-char character digits)
                (lexer-advance lexer)
                (loop while (and character (digit-char-p character)) do
                  (write-char character digits)
                  (lexer-advance lexer)))
              (values)))
        
        (read-sign)
        (read-integer-part)
        (read-decimal-part)
        
        (the Token
          (prog1
            (make-token :number
              (read-from-string (get-output-stream-string digits)))
            (close digits)))))))
          

;;; -------------------------------------------------------

(defun lexer-read-input (lexer)
  "Starting at the current LEXER position, reads an input (argument) and
   returns a ``Token'' representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (cond
      ((null character)
        (error "Unexpected end of file while reading an input at ~
                position ~d."
          (slot-value lexer 'position)))
      ((or (digit-char-p character)
           (and (find character "+-" :test #'char=)
                (digit-char-p (lexer-peek lexer))))
        (lexer-read-number lexer))
      ((char= character #\;)
        (lexer-read-identifier lexer))
      (T
        (lexer-read-string lexer)))))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current LEXER position, expects the content to be
   comment, skips it, relocates the LEXER either to end of the current
   line or the end of the source, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop do
      (cond
        ((null character)
          (loop-finish))
        ((char= character #\Newline)
          (loop-finish)
          (lexer-advance lexer))
        (T
          (lexer-advance lexer)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-command (lexer)
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (command)
        (declare (type string-stream command))
        (loop while (and character (char= character #\~)) do
          (write-char character command)
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-move-to (lexer new-position)
  "Moves the LEXER to the NEW-POSITION, updates its state, and returns
   the modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf position new-position)
    (if (< position (1- (length source)))
      (setf character (char source position))
      (setf character NIL))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct TEPCS-Loop
  "The ``TEPCS-Loop'' encapsulates the information necessary for
   driving an iteration construct in this language."
  (body-start    NIL :type fixnum)
  (tail-end      NIL :type (or null fixnum))
  (repetitions   0   :type (integer -1 *))
  (scans-for-end NIL :type boolean))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-TEPCS (code)
  "Interprets the TEPCS CODE and reutrns no value."
  (declare (type string code))
  (let ((lexer              (make-lexer code))
        (variables          (make-hash-table :test #'equal))
        (active-loops       NIL)
        (is-program-started NIL)
        ;; The current nesting level while skipping nested loops.
        (level              0))
    (declare (type Lexer                    lexer))
    (declare (type (hash-table-of string T) variables))
    (declare (type (stack TEPCS-Loop)       active-loops))
    (declare (type boolean                  is-program-started))
    
    (declare (type fixnum                   level))
    
    (with-slots (character position) lexer
      (declare (type (or null character) character))
      (declare (type fixnum              position))
      
      (labels
          ((start-program ()
             "Marks the program as started, signaling an error if
              it has already commenced, otherwise returning no
              value."
             (if is-program-started
               (error "The program is already started, as another ~
                       program start instruction has been ~
                       encountered at position ~d."
                 position)
               (setf is-program-started T))
             (values))
           
           (check-if-program-is-started ()
             "Checks whether the program has already started,
              signaling an error if this cannot be affirmed,
              otherwise returning no value."
             (unless is-program-started
               (error "The program has not been started yet, while ~
                       an instruction has been encountered at ~
                       position ~d."
                 position))
             (values))
           
           (is-scanning ()
             "Checks whether the current loop, if one exists, is
              searching for its end command ('~~~~~^e^...!'), returning
              a generalized Boolean result."
             (and (first active-loops)
                  (tepcs-loop-scans-for-end (first active-loops))))
           
           (eat-numeric-expression (token)
             "Attempts to interpret the TOKEN as a number and returns
              the numeric value if possible, otherwise signaling an
              error."
             (declare (type Token token))
             (with-token (type value) token
               (case type
                 (:number     value)
                 (:string     (gethash value variables))
                 (:identifier (gethash value variables))
                 (otherwise   (error "Invalid numeric expression: ~s."
                                     token)))))
           
           (eat-string-expression (token)
             "Attempts to interpret the TOKEN as a string and returns
              the character sequence if possible, otherwise signaling an
              error."
             (declare (type Token token))
             (with-token (type value) token
               (case type
                 (:string     value)
                 (:identifier (gethash value variables))
                 (otherwise   (error "Invalid string expression: ~s."
                                     token))))))
        
        (loop do
          (cond
            ((null character)
              (loop-finish))
            
            ((char= character #\Space)
              (loop while (and character (char= character #\Space)) do
                (lexer-advance lexer)))
            
            ((char= character #\~)
              (let ((command (lexer-read-command lexer)))
                (declare (type string command))
                
                (cond
                  ;; Print.
                  ((string= command "~")
                    (lexer-eat lexer #\^)
                    (check-if-program-is-started)
                    (let ((output ""))
                      (declare (type string output))
                      (setf output
                        (with-output-to-string (characters)
                          (loop
                            while
                              (and character (char/= character #\!))
                            do
                              (with-token (token-type token-value)
                                  (lexer-read-input lexer)
                                (case token-type
                                  (:number
                                    (format characters "~a" token-value))
                                  (:string
                                    (format characters "~a" token-value))
                                  (:identifier
                                    (format characters "~a"
                                      (gethash token-value variables)))
                                  (T
                                    (error "Invalid print token: (~s,~s)."
                                      token-type token-value))))
                            finally
                              (terpri characters))))
                      (unless (is-scanning)
                        (format T "~a" output)))
                    (lexer-eat lexer #\!))
                  
                  ;; Variable declaration.
                  ((string= command "~~")
                    (lexer-eat lexer #\^)
                    (check-if-program-is-started)
                    (let ((var-name
                            (eat-string-expression
                              (lexer-read-input lexer))))
                      (declare (type string var-name))
                      
                      ;; Read variable value.
                      (lexer-eat lexer #\^)
                      (let ((token (lexer-read-input lexer)))
                        (declare (type Token token))
                        (unless (is-scanning)
                          (with-token (type value) token
                            (case type
                              (:number
                                (setf (gethash var-name variables) value))
                              (:string
                                (setf (gethash var-name variables) value))
                              (:identifier
                                (setf (gethash var-name variables)
                                      (gethash value    variables)))
                              (otherwise
                                (error "Invalid value for variable ~
                                        declaration: (~s,~s)."
                                  type value)))))))
                    (lexer-eat lexer #\!))
                  
                  ;; Program start.
                  ((string= command "~~~")
                    (lexer-eat lexer #\!)
                    (start-program))
                  
                  ;; Binary operation.
                  ((string= command "~~~~")
                    (lexer-eat lexer #\^)
                    (check-if-program-is-started)
                    
                    (let ((var-name
                            (eat-string-expression
                              (lexer-read-input lexer))))
                      (declare (type string var-name))
                      
                      ;; Read right operand.
                      (lexer-eat lexer #\^)
                      (let ((right-operand
                              (eat-numeric-expression
                                (lexer-read-input lexer))))
                        (declare (type number right-operand))
                        
                        ;; Read operator.
                        (lexer-eat lexer #\^)
                        (let ((operator
                                (eat-string-expression
                                  (lexer-read-input lexer))))
                          (declare (type string operator))
                          
                          (unless (is-scanning)
                            (cond
                              ((string= operator "+")
                                (incf (gethash var-name variables)
                                      right-operand))
                              ((string= operator "-")
                                (decf (gethash var-name variables)
                                      right-operand))
                              ((string= operator "*")
                                (let ((var-value (gethash var-name variables)))
                                  (declare (type number var-value))
                                  (setf (gethash var-name variables)
                                        (* var-value right-operand))))
                              ((string= operator "/")
                                (let ((var-value (gethash var-name variables)))
                                  (declare (type number var-value))
                                  (setf (gethash var-name variables)
                                        (/ var-value right-operand))))
                              (T
                                (error "Invalid operator: ~s." operator)))))))
                    (lexer-eat lexer #\!))
                  
                  ;; Loop.
                  ((string= command "~~~~~")
                    (lexer-eat lexer #\^)
                    (check-if-program-is-started)
                    
                    (let ((control-type
                            (eat-string-expression
                              (lexer-read-input lexer))))
                      (declare (type string control-type))
                      
                      (cond
                        ;; Start loop.
                        ((string= control-type "s")
                          (lexer-eat lexer #\^)
                          
                          (let ((repetitions
                                  (eat-numeric-expression
                                    (lexer-read-input lexer))))  
                            (declare (type (integer -1 *) repetitions))
                            (lexer-eat lexer #\!)
                            
                            (cond
                              ((is-scanning)
                                (incf level))
                              (T
                                (let ((this-loop
                                        (make-tepcs-loop
                                          :body-start    position
                                          :repetitions   repetitions
                                          :scans-for-end (if (plusp repetitions) NIL T))))
                                  (declare (type TEPCS-Loop this-loop))
                                  
                                  ;; Zero repetitions?
                                  ;; => Start skipping succeeding commands.
                                  (when (<= repetitions 0)
                                    (setf level 0)
                                    (setf (tepcs-loop-scans-for-end this-loop) T))
                                  
                                  (push this-loop active-loops))))))
                        
                        ;; End loop.
                        ((string= control-type "e")
                          (lexer-eat lexer #\^)
                          
                          (let ((input (lexer-read-input lexer)))
                            (declare (type   Token input))
                            (declare (ignore input))
                            
                            (lexer-eat lexer #\!)
                            
                            (cond
                              ((and (is-scanning) (zerop level))
                                (unless active-loops
                                  (error "No loop start found at position ~d."
                                    position))
                                (let ((this-loop (first active-loops)))
                                  (declare (type (or null TEPCS-Loop) this-loop))
                                  (unless this-loop
                                    (error "Expected a TEPC-Loop object, ~
                                            but found ``NIL''."))
                                  (setf (tepcs-loop-scans-for-end this-loop) NIL))
                                (setf level 0)
                              )
                              ((and (is-scanning) (/= level 0))
                                (decf level))
                              (T
                                (unless active-loops
                                  (error "No loop start found at position ~d."
                                    position))
                                (let ((this-loop (first active-loops)))
                                  (declare (type (or null TEPCS-Loop) this-loop))
                                  (unless this-loop
                                    (error "Expected a TEPC-Loop object, ~
                                            but found ``NIL''."))
                                  (decf (tepcs-loop-repetitions this-loop))
                                  ;(setf (tepcs-loop-scans-for-end this-loop) NIL)
                                  (cond
                                    ((plusp (tepcs-loop-repetitions this-loop))
                                      (lexer-move-to lexer
                                        (tepcs-loop-body-start this-loop)))
                                    (T
                                      (pop active-loops))))))))
                        
                        ;; Invalid loop control type (first input).
                        (T
                          (error "Invalid loop control type ~s at position ~d."
                            control-type (1- position))))))
                  
                  (T
                    (error "Cannot process a command ~s." command)))))
            
            ;; Comment (";^!").
            ((char= character #\;)
              (lexer-eat lexer #\;)
              (lexer-eat lexer #\^)
              (lexer-eat lexer #\!)
              (lexer-skip-comment lexer))
            
            ((char= character #\Newline)
              (lexer-advance lexer)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prints "Hello, World!".
;; 
(interpret-TEPCS
  "~~~!
   ~^Hello, World!!")

;;; -------------------------------------------------------

;; Demonstrates the declaration and printing of variables.
;; 
(interpret-TEPCS
  "~~~!
   ~~^property^new!
   ~^Hello, ;property; World!!")

;;; -------------------------------------------------------

;; Demonstrates the declaration of variables and their use in
;; arithmetic operations.
;; 
(interpret-TEPCS
  "~~~!
   ~~^result^100!
   ~~~~^result^5^-!
   ~^100 - 5 = ;result;!")

;;; -------------------------------------------------------

;; Prints five times the text "Hello, World!"
;; 
(interpret-TEPCS
  "~~~!
   ~~~~~^s^5!
   ~^Hello, World!!
   ~~~~~^e^5!")

;;; -------------------------------------------------------

;; The "99 bottles of beer" example program.
;; 
(interpret-TEPCS
  "~~~!
   ~~^bot^99!
   ~~~~~^s^98!
   ~^;bot; bottles of beer on the wall, ;bot; bottles of beer!
   ~^Take one down, pass it around,!
   ~~~~^bot^1^-!
   ~^;bot; bottles of beer on the wall!
   ~~~~~^e^0!
   ~^1 bottle of beer on the wall, 1 bottle of beer!
   ~^Take one down, pass it around, no bottles of beer on the wall!")

;;; -------------------------------------------------------

;; Demonstrates the declaration of a variable with the value of
;; another one.
;; 
(interpret-TEPCS
  "~~~!
   ~~^rabbit^Booboo!
   ~~^bird^;rabbit;!
   ~^;rabbit; and ;bird;!")

;;; -------------------------------------------------------

;; Iterates the number of times equal to the value of the variable
;; "iterations" and prints upon each repetition the current value of
;; the "iterations" variable.
;; 
(interpret-TEPCS
  "~~~!
   ~~^iterations^5!
   ~~~~~^s^iterations!
   ~^;iterations;!
   ~~~~^iterations^1^-!
   ~~~~~^e^iterations!")

;;; -------------------------------------------------------

;; Multiplication table.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^maximumMultiplicand^10!
   ~~^maximumMultiplier^10!
   
   ~~^multiplicand^;maximumMultiplicand;!
   
   ~~~~~^s^maximumMultiplicand!
   
   ~~^multiplier^;maximumMultiplier;!
   ~~~~~^s^maximumMultiplier!
   
   ~~^product^;multiplicand;!
   ~~~~^product^multiplier^*!
   
   ~^;multiplicand; * ;multiplier; = ;product;!
   
   ~~~~^multiplier^1^-!
   ~~~~~^e^multiplier!
   
   ~~~~^multiplicand^1^-!
  
   ~~~~~^e^multiplicand!")

;;; -------------------------------------------------------

;; Factorial.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^n^5!
   ~~^factorial^1!
   
   ~~~~~^s^;n;!
   ~~~~^factorial^n^*!
   ~~~~^n^1^-!
   ~~~~~^e^;n;!
   
   ~^result = ;factorial;!")

;;; -------------------------------------------------------

;; Factorial with additional variable for more informative output.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^n^5!
   ~~^i^;n;!
   ~~^factorial^1!
   
   ~~~~~^s^;n;!
   ~~~~^factorial^i^*!
   ~~~~^i^1^-!
   ~~~~~^e^;n;!
   
   ~^factorial(;n;) = ;factorial;!")

;;; -------------------------------------------------------

;; Demonstrates a variable declaration availing itself with a variable
;; for providing the placeholder name. In this case, we define a
;; variable "variableName" whose value "bunny" will be used to declare
;; a second variable named "bunny" with an integer value of 100.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^variableName^bunny!
   ~~^;variableName;^100!
   
   ~^;variableName; = ;bunny;!")

;;; -------------------------------------------------------

;; Tests a loop of zero repetitions, which designates the same as to
;; be omitted.
;; 
(interpret-TEPCS
  "~~~!
   
   ~^A loop of zero repetitions commences...!
   
   ~~~~~^s^0!
   ~^This text should not appear.!
   ~~~~~^e^0!
   
   ~^The loop should have been omitted.!")

;;; -------------------------------------------------------

;; Tests a loop of zero repetitions in conjunction with a nested, which
;; with both designated to be omitted.
;; 
(interpret-TEPCS
  "~~~!
   
   ~^A loop of zero repetitions commences...!
   
   ~~~~~^s^0!
   ~^This text should not appear.!
   
   ~~~~~^s^100!
   ~^Another loop to skip.!
   ~~~~~^e^100!
   
   ~~~~~^e^0!
   
   ~^The loop should have been omitted.!")

;;; -------------------------------------------------------

;; Demonstrates the use of negative numeric arguments.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^myNumber^-5!
   ~~~~^myNumber^2^+!
   
   ~^My number is ;myNumber;!")

;;; -------------------------------------------------------

;; Demonstrates the use of floating-point numeric arguments.
;; 
(interpret-TEPCS
  "~~~!
   
   ~~^myNumber^-5.2!
   ~~~~^myNumber^2^+!
   
   ~^My number is ;myNumber;!")
