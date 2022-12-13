;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainfault", invented by the Esolang user "Beniolenio" in
;; the year 2021 as an extension of Urban Mueller's "brainfuck" concept,
;; elevated in its potentials by various output alternatives, iteration
;; constructs, and subroutines.
;; 
;; 
;; Concept
;; =======
;; The brainfault programming language's encheson for existence resides
;; in an engagement of sophisticated emoluments whose perimeter
;; comprehends several new methods of iteration, output formats, and,
;; as its chief conspicuous invention, the introduction of subroutines.
;; 
;; == SUBROUTINES ==
;; brainfault's most potent polymechany comprehends the introduction of
;; subroutines, uniquely identified code blocks utile for reuse, but
;; destitute of the capability to accept parameters.
;; 
;; A subroutine definition commences with an amperstand "$" marker,
;; followed by the identifier, and seguing into a sequence of zero or
;; more commands ensconced in braces ("{" and "}"). The lack of an
;; argument list intimates the absence of parameters as a feature.
;; 
;; The subroutine's invocation is realized by stating its name ensconced
;; in one asterisk per side.
;; 
;; Subroutines comply to a principle known as "hoisting", and most
;; commonly associated with the scripting language JavaScript: During
;; the interpretation process, preceding any other evaluation, their
;; definitions are relocated to the top of the data structure modeling
;; the program, usually the abstract syntax tree (AST). This action
;; homologates their invocation to precede the position of their
;; implementation in the source code.


;; == MISCELLANEOUS ITERATION CONSTRUCTS ==
;; Anenst the iteration facility department, another augmentation of
;; brainfault's shows. The original brainfuck language comprehends a
;; single loop variant, based upon the jumping betwixt two endpoints,
;; indagating the current cell content as the warklume for the concrete
;; action. By receiving a cell value of zero (0), the demarcated jump or
;; loop section will be skipped; otherwise a potentially infinite
;; reiteration applies to the embraced statements.
;; 
;; brainfault attends to a second species, which involves the input
;; queue's content. Entering upon its vacancy, the input loop continues
;; while the state still involves the presence of at least one element.
;; A failure in the queue's character provision incites the iteration's
;; cessation.
;; 
;; == CONDITIONALS ==
;; Besides the iterative facility, brainfault provides two variants of
;; conditional constructs.
;; 
;; The first manifestation executes its statements if the current cell
;; value equals a specified value. The second species applies if the
;; values do not concur. Both reifications accept an arbitrary tally of
;; instructions for the case of their execution.
;; 
;; == OUTPUT FORMATS ==
;; brainfuck's constriction to the current cell value's ASCII character
;; equivalent in output only is superseded by brainfault in enduing the
;; facilities with two further options: a printing as decimal integer
;; as an eight-bit binary.
;; 
;; 
;; Architecture
;; ============
;; brainfault's architectural department is delineated by a twifaced
;; appropriation from its provenance, in the deployment of a linearly
;; arranged memory compact of byte-valued cells, and amenable to a
;; mobile pointer; however, the tally of its components extends along
;; both directions into the infinite space.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF BYTES ==
;; brainfault represents a participant in the veridical nature of its
;; geniture, applying itself to the autochthonous construe postulated by
;; the original brainfuck concept in the subject regarding the program
;; memory's design as a sequence of byte-valued cells, occupying the
;; unsigned integer range [0, 255], and wrapping around if exceeding
;; 255 to 0, while, on the other post of its gamut, reacting to a
;; reduction from 0 with a reversion to 255.
;; 
;; A discriminating characteristic of this tape-like memory, its
;; components may assume any subscript and magnitude, that is, an
;; infinite account of cells along both lateralities occupies the
;; retentive structure.
;; 
;; == THE CELL POINTER: THE ACTIVE CELL'S MARKER ==
;; All operations appertaining to the memory access are dispatched to a
;; particular cell, selected by the agency of a special marker: the
;; cell pointer. This entity, initially empight at the first cell,
;; designates at any instant of the program the currently active unit.
;; Instructions may translate this pointer in graduation along both
;; lateralities.
;; 
;; 
;; Data Types
;; ==========
;; Subscribing to the same type system as the incipient brainfuck
;; specification, the legatee wists of a twofold distinguishment among
;; objects into bytes and characters.
;; 
;; == BYTES: THE MEMORY'S CURRENCY ==
;; All program data is consigned to the representative potency of bytes,
;; compact of eight bits, and occupants of the unsigned integer range
;; [0, 255].
;; 
;; The respondency to all arithmetic and inquisitive operations, the
;; former already exhausted by gradual augmentation and reduction, the
;; later a myriad's tale, including all iterative and conditional
;; facilities, serves as an affedavit to this numeric type. As an
;; exercise of supererogation when weighted against its sire's
;; capabilities, two third of the printing commands relate to the
;; verbatim or alternatively formatted octet value output.
;; 
;; Its coefficiency during the interaction with the surrounding system
;; and user supplies the sole relegation to the subordinate character
;; class.
;; 
;; == CHARACTERS: THE INTERACTIONS' CURRENCY ==
;; The character class govern a puisne bailiwick in juxtaposition to the
;; byte's puissance, its engagement restricted to particular
;; circumstances tangent with a brainfault program's interactions,
;; entailing input and output.
;; 
;; During the inquisition for an input, the character received by the
;; user's or system's induction relays to the current cell by a
;; transcription of itself into the yielded character code and the
;; same's storage.
;; 
;; Traveling an athwart airt, upon committing a textual output behest,
;; the current cell's octet datum is translated into the respective
;; character object and transmitted to the standard output.
;; 
;; 
;; Syntax
;; ======
;; brainfault's augmented potence requires a defrayal by a more
;; intricate exposition regarding the syntactical department when
;; equiparated to its brainfuck parentage, admitting compound structures
;; that might entail operative hierarchies.
;; 
;; == INSTRUCTIONS ==
;; The syntaxis appertaining to instructions arrives in a variety
;; concinnous with the confluence of its heritage and the particular
;; designs incorporated into its own novelties.
;; 
;; A preponderance among this set adheres to the brainfuck conformity
;; in employing single characters as command specifiers, embracing also
;; some of the adscititiously introduced brainfault members.
;; 
;; Subroutine designators may resolve to such frugal nomenclature, or
;; alternatively propagate a non-empty sequence compact of majuscular
;; and minuscular Latin letters, as well as underscores ("_").
;; 
;; Compound forms exist as a third linguistic element in the syntax
;; realm, imposing a more complex construction; enumerated in the same
;; reside the conditional blocks depending on equality or inequality, as
;; well as the subroutine definitions.
;; 
;; == COMMENTS ==
;; The brainfault programming language offers provisions for comments,
;; demarcated via one hash sign ("#") at each laterality:
;; 
;;   #commentText#
;; 
;; Their installment may transpire at any location in the code in the
;; interstices betwixt tokens.
;; 
;; == WHITESPACES ==
;; Whitespaces may intrude liberally into the interstices betwixt tokens
;; as optional sepiments, disencumbered from a coercion in use.
;; 
;; == GRAMMAR ==
;; The language's donat may be replicated in the following Extended
;; Backus-Naur Form (EBNF) formulation:
;; 
;;   program              := { command | comment } ;
;;   commands             := { command } ;
;;   command              := increment
;;                        |  decrement
;;                        |  moveLeft
;;                        |  moveRight
;;                        |  outputCharacter
;;                        |  outputInteger
;;                        |  outputBinary
;;                        |  input
;;                        |  loopIfNotZero
;;                        |  inputLoop
;;                        |  conditional
;;                        |  subroutineDefinition
;;                        |  subroutineCall
;;                        ;
;;   increment            := "+" ;
;;   decrement            := "-" ;
;;   moveLeft             := "<" ;
;;   moveRight            := "<" ;
;;   outputCharacter      := "." ;
;;   outputInteger        := ":" ;
;;   outputBinary         := "?" ;
;;   input                := "," ;
;;   loopIfNotZero        := "[" , commands , "]" ;
;;   inputLoop            := "/" , commands , "|" ;
;;   conditional          := if | ifNot ;
;;   if                   := "!"  , integer , "(" , commands ")" ;
;;   ifNot                := "!~" , integer , "(" , commands ")" ;
;;   subroutineDefinition := "$" , identifier , "{" , commands , "}" ;
;;   subroutineCall       := "*" , identifier , "*" ;
;;   
;;   comment              := "#" , { character } , "#" ;
;;   
;;   integer              := digit , { digit } ;
;;   identifier           := { letter | underscore } ;
;;   digit                := "0" | "1" | "2" | "3" | "4"
;;                        |  "5" | "6" | "7" | "8" | "9"
;;                        ;
;;   letter               := "a" | ... | "z" | "A" | ... | "Z" ;
;;   underscore           := "_" ;
;; 
;; 
;; Instructions
;; ============
;; brainfault contributes to the augmentation of its heritage's
;; instruction set in manifold ways, including the introduction of
;; 
;;   - output in decimal and binary form, in addition to the traditional
;;     character printing
;;   - an input-queue-based loop
;;   - two conditional statements affected by the current cell's
;;     equality or inequality with a numeric guard
;;   - niladic subroutines.
;; 
;; == OVERVIEW ==
;; The faculties commorant in brainfault shall now be a cursory
;; description's subject. Please remain tentive about the installment of
;; carets below some commands; these represent the variable portions.
;; For instance, the entry
;; 
;;   !~integer(commands)
;;     ^^^^^^^ ^^^^^^^^
;; 
;; emphasizes that the "integer" and "commands" act as placeholders,
;; intended to be substituted with actual values; whereas the
;; surrounding elements, concretely
;; 
;;   !~       (        )
;; 
;; shall be preserved verbatim in a program. This kenspeckle approach to
;; formatting hopefully obviates misconstrues considering the various
;; reserved parts and the variable fragments.
;; 
;;   ------------------------------------------------------------------
;;   Command                    | Effect
;;   ---------------------------+--------------------------------------
;;   >                          | Moves the cell pointer oen cell to
;;                              | the right.
;;   ..................................................................
;;   <                          | Moves the cell pointer one cell to
;;                              | the left.
;;   ..................................................................
;;   +                          | Increments the current cell value by
;;                              | one.
;;                              | If the cell value exceeds the upper
;;                              | bound of 255 by this operation, it is
;;                              | wrapped around to zero (0).
;;   ..................................................................
;;   -                          | Decrements the current cell value by
;;                              | one.
;;                              | If the cell value descends below the
;;                              | lower bound of zero (0), it is set to
;;                              | to 255.
;;   ..................................................................
;;   .                          | Outputs the ASCII character
;;                              | associated with the current cell
;;                              | value.
;;   ..................................................................
;;   :                          | Outputs the current cell value as an
;;                              | integer number.
;;   ..................................................................
;;   ?                          | Outputs the current cell value as a
;;                              | binary number.
;;                              | Upon necessity it is padded with
;;                              | zero-bits on the most significant
;;                              | places to accommodate for eight
;;                              | positions.
;;   ..................................................................
;;   ,                          | Queries the user for an input
;;                              | character and stores its ASCII code
;;                              | in the current cell.
;;   ..................................................................
;;   [commands]                 | Executes the COMMANDS as long as the
;;    ^^^^^^^^                  | current cell value does not equal
;;                              | zero (0).
;;   ..................................................................
;;   /commands|                 | Executes the COMMANDS as long as the
;;    ^^^^^^^^                  | input queue is empty.
;;   ..................................................................
;;   !integer(commands)         | Executes the COMMANDS once if the
;;    ^^^^^^^ ^^^^^^^^          | current cell value equals the
;;                              | INTEGER; otherwise skips the body.
;;   ..................................................................
;;   !~integer(commands)        | Executes the COMMANDS once if the
;;     ^^^^^^^ ^^^^^^^^         | current cell value does not equal the
;;                              | INTEGER; otherwise skips the body.
;;   ..................................................................
;;   $subroutine_name{commands} | Creates a subroutine with the name
;;    ^^^^^^^^^^^^^^^ ^^^^^^^^  | SUBROUTINE_NAME, defined by a body
;;                              | composed of the COMMANDS.
;;   ..................................................................
;;   *subroutine_name*          | Invokes the subroutine identified by
;;    ^^^^^^^^^^^^^^^           | the SUBROUTINE_NAME.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; An act of amazement, the mickleness commorant in brainfault's
;; operative circumference does not amount to equiparation in the
;; account of ambiguities, with no fardels' detection remained to
;; register.
;; 
;; 
;; Implementation
;; ==============
;; The implementation in Common Lisp follows a tripartite forbisen,
;; compact of the token generation by a lexer, these constituents'
;; assemblage into an abstract syntax tree (AST) by a parser's effort,
;; and the AST's ultimate evaluation using an interpreter.
;; 
;; == THE INTERPRETATION PROCESS AS A TRIPARTITE ENDEAVOR ==
;; The complete interpretation procedure constitutes an arrangement by
;; three tiers' coefficiency:
;; 
;;   (1) A lexical analyzer, also known as "lexer" or "scanner",
;;       participates in the foundational compartment of the parasceve,
;;       extracting from a piece of brainfault source code, supplied in
;;       string form, a sequence of tokens.
;;   
;;   (2) These tokens themselves are dispatched to the parser, its onus
;;       the assemblage of an abstract syntax tree (AST) withal. This
;;       hierarchical structure serves in capturing the various language
;;       constructs in a node form.
;;   
;;   (3) In a coarse, and rather simplified treatise, the AST is
;;       consigned to the interpreter's workings, its wike the nodes'
;;       visitation and application of effect.
;;         With more details in explication, a preliminary operation
;;       partakes in the intermede betwixt the actual interpreter's and
;;       the parser's exercises, responsible for the hoisting of the
;;       subroutines, whence originates a table registering such a code
;;       block's name with its nodes, and prepared for the interpreter's
;;       usage during the program execution stage.
;; 
;; == SUBROUTINE HOISTING: A PRE-INTERPRETER VISIT ==
;; In order to "hoist" the subroutines, that is, register them for a
;; contingent application in the program preceding their actual
;; definition, the abstract syntax tree (AST) is being visited by a
;; specialized entity which returns a subroutine table for the
;; interpreter.
;; 
;; The interpreter's bailiwick is characterized by a particular
;; employment of the visitor design pattern in the context of the
;; abstract syntax tree's traversal. Commencing at the root node, the
;; subtrees and leaves are sojourned in a preorder traversal mode for
;; the sake of executing the program statements.
;; 
;; A peculiarity of brainfault, subroutines are being "hoisted", that
;; is, their name and definition are gathered ere the program's
;; execution, homologating their invocation in a location preceding the
;; subroutine's actual implementation.
;; 
;; In order to accomplish this state of gnarity, the interpretation step
;; must be prepared using a subroutine search and registration process.
;; This enterprise is allotted to the ``Subroutine-Hoister'' class, a
;; dedicated entity implementing the ``Visitor'' interface and
;; traversing the AST in an indagation of subroutine definitions ---
;; ignoring any other node type, including the subroutine invocations.
;; Each such detected definition is persisted in a hash table,
;; associating with its name the ``Subroutine-Definition-Node'' node
;; itself, thus especially encapsulating the body statements, themselves
;; each a ``Node'' object.
;; 
;; Upon the ``Interpreter'' class' processing of the AST, an encounter
;; with a ``Subroutine-Definition-Node'' retains no effect; however, the
;; ``Subroutine-Call-Node'' sojourns incite a lookup of the subroutine's
;; body commands via the hoister's table, employing the comprehended
;; name as a key, and continuing by redirecting the traversal along
;; these child nodes.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-05
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Brainfault"
;;   -> "https://www.oreilly.com/library/view/you-dont-know/9781449335571/ch04.html"
;;       o Treatise on the subject of hoisting in the programming
;;         language JavaScript.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized brainfault
   instruction variants, admitting as an adscititious member the program
   as a vinculum to the implementation's abstract syntax tree (AST)
   nodes."
  '(member
    :move-left
    :move-right
    :increment
    :decrement
    :output-character
    :output-integer
    :output-binary
    :input
    :loop-if-not-zero
    :execute-if-equals
    :loop-if-no-input
    :define-subroutine
    :call-subroutine
    :program))

;;; -------------------------------------------------------

(deftype jump-predicate ()
  "The ``jump-predicate'' defines the variants of predicates employed in
   the conditional facility \"!int(commands)\" and \"!~int(commands)\"."
  '(member :equal :not-equal))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects, steadable in particular for the usance in an abstract syntax
   tree (AST)."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an integer value in the byte range
   [0, 255], compatible with a tape cell's storage type."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse association of signed
   integer cell indices to octet cell values, manifesting in the form of
   a hash table which maps the former type of keys to the values
   conforming to the latter type."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype subroutine-table ()
  "The ``subroutine-table'' defines a mapping of subroutine names to
   abstract syntax tree nodes of the ``Subroutine-Definition-Node''
   class, represented by a hash table."
  '(hash-table-of string Subroutine-Definition-Node))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant portion obtained by the
   analyzation of a piece of brainfault source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subroutine-character-p (character)
  "Checks whether the CHARACTER represents a constituent of a subroutine
   name, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alpha-char-p character)
          (char=        character #\_))))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents A whitespace, returning on
   confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of brainfault source code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class provides a lexical analyzer, tasked with the
     detection of significant constituents in a piece of brainfault
     source code and the generation of tokens therefrom."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'' and
   ``character'' to eponymous symbol macros for general access, executes
   the BODY forms, and returns the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' intended to analyze the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next position in its source, if possible, and
   returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-subroutine-definition (lexer)
  "Starting at the current position into the LEXER's source, reads a
   subroutine name used in a declaration and returns a token
   representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    ;; Skip introducing "$".
    (lexer-advance lexer)
    (the Token
      (make-token :define-subroutine
        (with-output-to-string (name)
          (declare (type string-stream name))
          (loop
            while (and character (subroutine-character-p character))
            do
              (write-char character name)
              (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-subroutine-call (lexer)
  "Starting at the current position into the LEXER's source, reads a
   subroutine name used in an invocation and returns a token
   representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    ;; Skip introducing "*".
    (lexer-advance lexer)
    (the Token
      (make-token :call-subroutine
        (with-output-to-string (name)
          (declare (type string-stream name))
          (loop
            while (and character (subroutine-character-p character))
            do
              (write-char character name)
              (lexer-advance lexer)
            finally
              (if (and character (char= character #\*))
                (lexer-advance lexer)
                (error "Unterminated subroutine call: Expected '*', ~
                        but encountered '~a' at position ~d."
                  character position))))))))

;;; -------------------------------------------------------

(defun lexer-read-character (lexer token-type)
  "Starting at the LEXER's current position in its source, reads a
   single character and returns a new token representation associating
   the TOKEN-TYPE with the consumed character object."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-lexer (lexer)
    (the Token
      (make-token token-type
        (prog1 character
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun read-unsigned-integer (lexer)
  "Reads from the LEXER's source a sequence of one or more decimal
   digits and returns the parsed unsigned integer value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the (integer 0 *)
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (cond
            ((null character)
              (error "Expected a decimal digit, but encountered an ~
                      end-of-file (EOF) at position ~d."
                position))
            ((digit-char-p character)
              (loop while (and character (digit-char-p character)) do
                (write-char character digits)
                (lexer-advance lexer)))
            (T
              (error "Expected a decimal digit, but encountered ~s at ~
                      position ~d."
                character position))))))))

;;; -------------------------------------------------------

(defun read-integer (lexer)
  "Reads from the LEXER's source a potentially signed integer and
   returns its parsed value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the integer
      (cond
        ((null character)
          (error "Expected a sign or a decimal digit, but ~
                  encountered an end-of-file (EOF) at position ~d."
            position))
        ((digit-char-p character)
          (read-unsigned-integer lexer))
        ((char= character #\+)
          (lexer-advance lexer)
          (read-unsigned-integer lexer))
        ((char= character #\-)
          (lexer-advance lexer)
          (- (read-unsigned-integer lexer)))
        (T
          (error "Expected a sign or a decimal digit, but ~
                  encountered ~s at position ~d."
            character position))))))

;;; -------------------------------------------------------

(defun lexer-read-comparison (lexer)
  "Starting at the current position into the LEXER's source, reads an
   equality or non-equality predicate, that is, either \"!~\ or \"!\",
   and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    ;; Skip introducing "!".
    (lexer-advance lexer)
    (the Token
      (case character
        (#\~
          (lexer-advance lexer)
          (make-token :not-equal (read-integer lexer)))
        (otherwise
          (make-token :equal (read-integer lexer)))))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current position in the LEXER's source, skips a
   series of zero or more adjacent whitespaces and returns the modified
   LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (lexer-advance lexer)
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current position into the LEXER's source, skips a
   comment section and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    ;; Skip introducing "#".
    (lexer-advance lexer)
    (loop do
      (case character
        ((NIL)
          (error "Unterminated comment at position ~d." position))
        (#\#
          (lexer-advance lexer)
          (loop-finish))
        (otherwise
          (lexer-advance lexer)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each request with
   a fresh token of the end-of-file type ``:eof'' in conjunction with
   the ``NIL''."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((char= character #\#)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\>)
          (lexer-read-character lexer :right-chevron))
        
        ((char= character #\<)
          (lexer-read-character lexer :left-chevron))
        
        ((char= character #\+)
          (lexer-read-character lexer :plus))
        
        ((char= character #\-)
          (lexer-read-character lexer :minus))
        
        ((char= character #\.)
          (lexer-read-character lexer :dot))
        
        ((char= character #\:)
          (lexer-read-character lexer :colon))
        
        ((char= character #\?)
          (lexer-read-character lexer :question-mark))
        
        ((char= character #\,)
          (lexer-read-character lexer :comma))
        
        ((char= character #\[)
          (lexer-read-character lexer :left-bracket))
        
        ((char= character #\])
          (lexer-read-character lexer :right-bracket))
        
        ((char= character #\/)
          (lexer-read-character lexer :slash))
        
        ((char= character #\|)
          (lexer-read-character lexer :pipe))
        
        ((char= character #\!)
          (lexer-read-comparison lexer))
        
        ((char= character #\$)
          (lexer-read-subroutine-definition lexer))
        
        ((char= character #\*)
          (lexer-read-subroutine-call lexer))
        
        ((char= character #\()
          (lexer-read-character lexer :left-parenthesis))
        
        ((char= character #\))
          (lexer-read-character lexer :right-parenthesis))
        
        ((char= character #\{)
          (lexer-read-character lexer :left-brace))
        
        ((char= character #\})
          (lexer-read-character lexer :right-brace))
        
        (T
          (error "Unexpected character ~s at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of abstract class "Node".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "Missing node type.")
    :type          command-type
    :documentation "The type of this node."))
  (:documentation
    "The abstract class ``Node'' serves to establish a foundation for
     components in an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the NODE type."
  (declare (type Node node))
  (the command-type (slot-value node 'type)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Simple-Instruction-Node".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Simple-Instruction-Node (Node)
  ()
  (:documentation
    "The ``Simple-Instruction-Node'' encapsulates an operation without
     reliance on an argument in a node."))

;;; -------------------------------------------------------

(defun make-simple-instruction-node (type)
  "Creates and returns a ``Simple-Instruction-Node'' which represents
   the command TYPE."
  (declare (type command-type type))
  (the Simple-Instruction-Node
    (make-instance 'Simple-Instruction-Node :type type)))

;;; -------------------------------------------------------

(defmethod print-object ((node Simple-Instruction-Node) stream)
  (declare (type Simple-Instruction-Node node))
  (declare (type destination             stream))
  (format stream "Simple-Instruction-Node(~s)"
    (slot-value node 'type)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Loop-If-Not-Zero-Node".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Loop-If-Not-Zero-Node (Node)
  ((commands
    :initarg       :commands
    :initform      (error "Missing conditional instruction commands.")
    :type          node-list
    :documentation "The instruction constituting the loop body."))
  (:default-initargs
    :type :loop-if-not-zero)
  (:documentation
    "The ``Loop-If-Not-Zero-Node'' represents the jump or iteration
     facility which repeats continuously until the current cell assumes
     a value of zero.
     ---
     This class ultimately models the brainfault language construct
       [ commands ]"))

;;; -------------------------------------------------------

(defun make-loop-if-not-zero-node (commands)
  "Creates and returns a new ``Loop-If-Not-Zero-Node'' whose body is
   comprised by the COMMANDS."
  (declare (type node-list commands))
  (the Loop-If-Not-Zero-Node
    (make-instance 'Loop-If-Not-Zero-Node :commands commands)))

;;; -------------------------------------------------------

(defun loop-if-not-zero-commands (node)
  "Returns the zero-test loop NODE commands."
  (declare (type Loop-If-Not-Zero-Node node))
  (the node-list
    (slot-value node 'commands)))

;;; -------------------------------------------------------

(defmethod print-object ((node Loop-If-Not-Zero-Node) stream)
  (declare (type Loop-If-Not-Zero-Node node))
  (declare (type destination           stream))
  (format stream "Loop-If-Not-Zero-Node(type=~s, commands=~s)"
    (slot-value node 'type)
    (slot-value node 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Input-Loop-Node".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Loop-Node (Node)
  ((commands
    :initarg       :commands
    :initform      (error "Missing conditional instruction commands.")
    :type          node-list
    :documentation "The commads ensconced in this input loop."))
  (:default-initargs
    :type :loop-if-no-input)
  (:documentation
    "The ``Input-Loop-Node'' represents an iterative facility which
     commences its operations if the input conduit is empty, and
     repeats while it still contains elements.
     ---
     This class ultimately models the brainfault language construct
       / commands |"))

;;; -------------------------------------------------------

(defun make-input-loop-node (commands)
  "Creates and returns a new ``Input-Loop-Node'' whose body is comprised
   of the COMMANDS."
  (declare (type node-list commands))
  (the Input-Loop-Node
    (make-instance 'Input-Loop-Node :commands commands)))

;;; -------------------------------------------------------

(defun input-loop-node-commands (node)
  "Returns the input loop NODE body."
  (declare (type Input-Loop-Node node))
  (the node-list
    (slot-value node 'commands)))

;;; -------------------------------------------------------

(defmethod print-object ((node Input-Loop-Node) stream)
  (declare (type Input-Loop-Node node))
  (declare (type destination     stream))
  (format stream "Input-Loop-Node(type=~s, commands=~s)"
    (slot-value node 'type)
    (slot-value node 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Execute-If-Equals-Node".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Execute-If-Equals-Node (Node)
  ((predicate
    :initarg       :predicate
    :initform      (error "Missing conditional instruction predicate.")
    :type          jump-predicate
    :documentation "Determines whether the COMMANDS shall be executed if
                    the GUARD value equals or differs from the current
                    cell value.")
   (guard
    :initarg       :guard
    :initform      (error "Missing conditional instruction guard.")
    :type          integer
    :documentation "The integer to compare the current cell value
                    against.")
   (commands
    :initarg       :commands
    :initform      (error "Missing conditional instruction commands.")
    :type          node-list
    :documentation "A sequence of zero or more commands to execute if
                    the PREDICATE is fulfilled on the GUARD and the
                    current cell value."))
  (:default-initargs
    :type :execute-if-equals)
  (:documentation
    "The ``Execute-If-Equals-Node'' represents the conditional facility
     based upon the equality or inequality of the current memory cell
     to a specified value, known as the guard.
     ---
     This class ultimately models the brainfault language constructs
       !integer(commands)
       !~integer(commands)"))

;;; -------------------------------------------------------

(defun make-execute-if-equals-node (predicate guard commands)
  "Creates and returns a new ``Execute-If-Equals-Node'' whose body,
   defined by the COMMANDS, is executed upon a confirmation of the
   PREDICATE when applied to the GUARD value."
  (declare (type jump-predicate predicate))
  (declare (type integer        guard))
  (declare (type node-list      commands))
  (the Execute-If-Equals-Node
    (make-instance 'Execute-If-Equals-Node
      :predicate predicate
      :guard     guard
      :commands  commands)))

;;; -------------------------------------------------------

(defun execute-if-equals-node-predicate (node)
  "Returns the predicate associated with this conditional execution
   NODE."
  (declare (type Execute-If-Equals-Node node))
  (the jump-predicate
    (slot-value node 'predicate)))

;;; -------------------------------------------------------

(defun execute-if-equals-node-guard (node)
  "Returns the guard associated with this conditional execution NODE."
  (declare (type Execute-If-Equals-Node node))
  (the integer
    (slot-value node 'guard)))

;;; -------------------------------------------------------

(defun execute-if-equals-node-commands (node)
  "Returns the commands associated with this conditional execution
   NODE."
  (declare (type Execute-If-Equals-Node node))
  (the node-list
    (slot-value node 'commands)))

;;; -------------------------------------------------------

(defmethod print-object ((node Execute-If-Equals-Node) stream)
  (declare (type Execute-If-Equals-Node node))
  (declare (type destination         stream))
  (format stream "Execute-If-Equals-Node(type=~s, ~
                                         predicate=~s, ~
                                         guard=~s, ~
                                         commands=~s)"
    (slot-value node 'type)
    (slot-value node 'predicate)
    (slot-value node 'guard)
    (slot-value node 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Subroutine-Definition-Node".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Subroutine-Definition-Node (Node)
  ((name
    :initarg       :name
    :initform      (error "Missing subroutine definition name.")
    :type          string
    :documentation "The name of the subroutine to define.")
   (commands
    :initarg       :commands
    :initform      (error "Missing subroutine definition commands.")
    :type          node-list
    :documentation "A list of zero or more body statements."))
  (:default-initargs
    :type :define-subroutine)
  (:documentation
    "The ``Subroutine-Definition-Node'' represents the declaration and
     implementation of a subroutine.
     ---
     This class ultimately models the brainfault language construct
       $subroutine_name{commands}"))

;;; -------------------------------------------------------

(defun make-subroutine-definition-node (name commands)
  "Creates and returns a new ``Subroutine-Definition-Node'' which
   associates the subroutine NAME with the list of COMMANDS that
   constitute its body."
  (declare (type string    name))
  (declare (type node-list commands))
  (the Subroutine-Definition-Node
    (make-instance 'Subroutine-Definition-Node
      :name     name
      :commands commands)))

;;; -------------------------------------------------------

(defun subroutine-definition-node-name (node)
  "Returns the subroutine name declared in this subroutine definition
   NODE."
  (the string
    (slot-value node 'name)))

;;; -------------------------------------------------------

(defun subroutine-definition-node-commands (node)
  "Returns the subroutine body contained in this subroutine definition
   NODE."
  (the node-list
    (slot-value node 'commands)))

;;; -------------------------------------------------------

(defmethod print-object ((node Subroutine-Definition-Node) stream)
  (declare (type Subroutine-Definition-Node node))
  (declare (type destination                stream))
  (format stream "Subroutine-Definition-Node(type=~s, ~
                                             name=~s, ~
                                             commands=~s)"
    (slot-value node 'type)
    (slot-value node 'name)
    (slot-value node 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Subroutine-Call-Node".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Subroutine-Call-Node (Node)
  ((name
    :initarg       :name
    :initform      (error "Missing subroutine call name.")
    :type          string
    :documentation "The name of the subroutine to invoke."))
  (:default-initargs
    :type :call-subroutine)
  (:documentation
    "The ``Subroutine-Call-Node'' represents the invocation of a
     subroutine.
     ---
     This class ultimately models the brainfault language construct
       *subroutine_name*"))

;;; -------------------------------------------------------

(defun make-subroutine-call-node (name)
  "Creates and returns a new ``Subroutine-Call-Node'' which refers to
   the subroutine of the specified NAME."
  (declare (type string name))
  (the Subroutine-Call-Node
    (make-instance 'Subroutine-Call-Node :name name)))

;;; -------------------------------------------------------

(defun subroutine-call-node-name (node)
  "Returns the subroutine name referenced by the subroutine call NODE."
  (declare (type Subroutine-Call-Node node))
  (the string
    (slot-value node 'name)))

;;; -------------------------------------------------------

(defmethod print-object ((node Subroutine-Call-Node) stream)
  (declare (type Subroutine-Call-Node node))
  (declare (type destination          stream))
  (format stream "Subroutine-Call-Node(type=~s, name=~s)"
    (slot-value node 'type)
    (slot-value node 'name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of class "Program-Node".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-Node (Node)
  ((commands
    :initarg       :commands
    :initform      (error "Missing program node commands.")
    :type          node-list
    :documentation "A list of zero or more commands which compromise the
                    program body."))
  (:default-initargs
    :type :program)
  (:documentation
    "The ``Program-Node'' represents the root node of an abstract syntax
     tree (AST) yielded during the parsing process.
     ---
     Please note that this ilk of node that not represent any
     autochthonous element of a brainfault program."))

;;; -------------------------------------------------------

(defun make-program-node (commands)
  "Creates and returns a new ``Program'' node containing the COMMANDS."
  (declare (type node-list commands))
  (the Program-Node
    (make-instance 'Program-Node :commands commands)))

;;; -------------------------------------------------------

(defun program-node-commands (node)
  "Returns a list containing the program NODE commands."
  (declare (type Program-Node node))
  (the node-list
    (slot-value node 'commands)))

;;; -------------------------------------------------------

(defmethod print-object ((node Program-Node) stream)
  (declare (type Program-Node node))
  (declare (type destination  stream))
  (format stream "Program-Node(type=~s, commands=~s)"
    (slot-value node 'type)
    (slot-value node 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The token purveying entity.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class furnishes a unit for the assemblage of an
     abstract syntax tree (AST) from the tokens supplied by a lexer."))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) node-list) parser-parse-commands))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous symbol macros for general access, executes the BODY
   forms, and returns the last evaluated form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (with-slots (lexer current-token) ,evaluated-parser
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-parser (parser)
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which assembles its abstract
   syntax tree (AST) from the LEXER's supplied tokens."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the indagated token
   while loading the next one from the PARSER's internal lexer; on
   failure an error of an unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-parser (parser)
    (the Token
      (if (token-type-p current-token expected-token-type)
        (prog1 current-token
          (setf current-token (lexer-get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-command (parser)
  "Parses a command using the PARSER and returns a node representation
   thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Node
      (case (token-type current-token)
        (:plus
          (parser-eat parser :plus)
          (make-simple-instruction-node :increment))
        
        (:minus
          (parser-eat parser :minus)
          (make-simple-instruction-node :decrement))
        
        (:left-chevron
          (parser-eat parser :left-chevron)
          (make-simple-instruction-node :move-left))
        
        (:right-chevron
          (parser-eat parser :right-chevron)
          (make-simple-instruction-node :move-right))
        
        (:dot
          (parser-eat parser :dot)
          (make-simple-instruction-node :output-character))
        
        (:colon
          (parser-eat parser :colon)
          (make-simple-instruction-node :output-integer))
        
        (:question-mark
          (parser-eat parser :question-mark)
          (make-simple-instruction-node :output-binary))
        
        (:comma
          (parser-eat parser :comma)
          (make-simple-instruction-node :input))
        
        (:left-bracket
          (parser-eat parser :left-bracket)
          (let ((statements (parser-parse-commands parser)))
            (declare (type node-list statements))
            (parser-eat parser :right-bracket)
            (make-loop-if-not-zero-node statements)))
        
        (:right-bracket
          (error "Unmatched ']' command."))
        
        (:slash
          (parser-eat parser :slash)
          (let ((statements (parser-parse-commands parser)))
            (declare (type node-list statements))
            (parser-eat parser :pipe)
            (make-input-loop-node statements)))
        
        (:pipe
          (error "Unmatched '|' command."))
        
        ((:not-equal :equal)
          (let ((predicate (token-type  current-token))
                (guard     (token-value current-token)))
            (declare (type jump-predicate predicate))
            (declare (type integer        guard))
            (parser-eat parser (token-type current-token))
            (parser-eat parser :left-parenthesis)
            (let ((statements (parser-parse-commands parser)))
              (declare (type node-list statements))
              (parser-eat parser :right-parenthesis)
              (make-execute-if-equals-node predicate guard statements))))
        
        (:define-subroutine
          (let ((name
                  (token-value
                    (parser-eat parser :define-subroutine))))
            (declare (type string name))
            (parser-eat parser :left-brace)
            (let ((commands (parser-parse-commands parser)))
              (declare (type node-list commands))
              (parser-eat parser :right-brace)
              (make-subroutine-definition-node name commands))))
        
        (:call-subroutine
          (let ((name
                  (token-value
                    (parser-eat parser :call-subroutine))))
            (declare (type string name))
            (make-subroutine-call-node name)))
        
        (otherwise
          (error "Unrecognized command token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun end-of-block-token-p (token)
  "Checks whether the TOKEN represents the end of a block, such as a
   iteration facility or a conditional instruction, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token)
        '(:eof :right-parenthesis :right-bracket :right-brace :pipe)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun parser-parse-commands (parser)
  "Parses a sequence of zero or more commands utilizing the PARSER and
   returns a list of the thus collected nodes."
  (declare (type Parser parser))
  (the node-list
    (with-parser (parser)
      (loop
        until   (end-of-block-token-p current-token)
        collect (parser-parse-command parser)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the program represented by the PARSER's internally managed
   LEXER's tokens and returns an abstract syntax tree (AST) in the form
   of a ``Program-Node'' as its root."
  (declare (type Parser parser))
  (the Node
    (make-program-node
      (prog1
        (parser-parse-commands parser)
        (parser-eat            parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Visitor".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Visitor ()
  ()
  (:documentation
    "The ``Visitor'' interface serves to establish a foundation for
     entities intended to traverse an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defgeneric visitor-dispatch-node (visitor node-type node)
  (:documentation
    "Processes the NODE, designated by the NODE-TYPE, using the VISITOR
     and returns no value."))

;;; -------------------------------------------------------

(defun visitor-visit-node (visitor node)
  "Processes the NODE using the VISITOR and returns no value.
   ---
   Maugre its agnomination signifying its primary role in the VISITOR's
   traversal process, this function merely serves as facade, or hub,
   for the ``visitor-dispatch-node'' generic function's implementations,
   which furnish the actual effectivity."
  (declare (type Visitor visitor))
  (declare (type Node    node))
  (visitor-dispatch-node visitor (node-type node) node)
  (values))

;;; -------------------------------------------------------

(defmacro define-visitor-method ((visitor-class node-type) &body body)
  "Defines an implementation of the ``visitor-dispatch-node'' generic
   function by binding to the symbol ``visitor'' the evaluated
   VISITOR-CLASS, to the symbol ``node-type'' the evaluated NODE-TYPE,
   and to ``node'' the ``Node'' class, upon which treble the method
   dispatches, and evaluates the BODY statements, finally returning the
   last evaluated form's results.
   ---
   Please note that the following three symbols are fixated, beyond the
   potency of reconfiguration:
   
     -------------------------------------------------------------
     Bound symbol | Value
     -------------+-----------------------------------------------
     visitor      | Evaluated value of the argument VISITOR-CLASS.
     .............................................................
     node-type    | Evaluated value of the argument NODE-TYPE.
     .............................................................
     node         | Class name ``Node''.
     -------------------------------------------------------------"
  `(defmethod visitor-dispatch-node ((visitor   ,visitor-class)
                                     (node-type (eql ,node-type))
                                     (node      Node))
     (declare (type ,visitor-class visitor))
     (declare (type command-type   ,node-type))
     (declare (type Node           node))
     (declare (ignorable           visitor))
     (declare (ignorable           node-type))
     (declare (ignorable           node))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Subroutine-Hoister".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Subroutine-Hoister (Visitor)
  ((subroutines
    :initarg       :subroutines
    :initform      (make-hash-table :test #'equal)
    :type          subroutine-table
    :documentation "Maintains the detected subroutines by mapping their
                    names to the respective abstract syntax tree nodes
                    of the type ``Subroutine-Definition-Node''."))
  (:documentation
    "The ``Subroutine-Hoister'' class establishes an agent responsible
     for the discovery and registry of subroutine declarations.
     ---
     The brainfault programming language explicitly commands the
     \"hoisting\" of subroutines, that is, their relocation to the top
     of the node hierarchy in order to homologate access to their
     services even prior to their definition in the program. As a
     consectary, a subroutine invocation may refer to such a piece of
     code ere it is declared. In order to realize such a behavior in an
     intepreter, these code blocks must be detected, registered, and
     made accessible in a step preceding the node evaluations. It
     constitutes this class' onus to apply itself to this objective.
     ---
     The ``Subroutine-Hoister'' traverses the induced abstract syntax
     tree (AST) and stores each subroutine declaration node, being
     always of the subclass ``Subroutine-Definition-Node'', in an
     associative data structure of the hash table type, connecting a
     subroutine name with its declaring node. Duplicate definitions'
     presence is encountered with tolerance, with the desinent
     occurrence superseding all predecessors."))

;;; -------------------------------------------------------

(defun make-subroutine-hoister ()
  "Creates and returns a new ``Subroutine-Hoister''."
  (the Subroutine-Hoister (make-instance 'Subroutine-Hoister)))

;;; -------------------------------------------------------

(defmethod visitor-dispatch-node ((hoister   Subroutine-Hoister)
                                  (node-type (eql :program))
                                  (node      Node))
  (declare (type Subroutine-Hoister hoister))
  (declare (type command-type       node-type))
  (declare (type Node               node))
  (declare (ignore                  node-type))
  (dolist (statement (program-node-commands node))
    (declare (type Node statement))
    (visitor-visit-node hoister statement))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-dispatch-node ((hoister   Subroutine-Hoister)
                                  (node-type (eql :define-subroutine))
                                  (node      Node))
  (declare (type Subroutine-Hoister hoister))
  (declare (type command-type       node-type))
  (declare (type Node               node))
  (declare (ignore                  node-type))
  (let ((subroutine-name     (subroutine-definition-node-name     node))
        (subroutine-commands (subroutine-definition-node-commands node)))
    (declare (type string    subroutine-name))
    (declare (type node-list subroutine-commands))
    
    (with-slots (subroutines) hoister
      (declare (type subroutine-table subroutines))
      (setf (gethash subroutine-name subroutines) node))
    
    (dolist (command subroutine-commands)
      (declare (type Node command))
      (visitor-visit-node hoister command)))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Subroutine-Hoister :loop-if-not-zero)
  (dolist (command (slot-value node 'commands))
    (declare (type Node command))
    (visitor-visit-node visitor command))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Subroutine-Hoister :loop-if-no-input)
  (dolist (command (input-loop-node-commands node))
    (declare (type Node command))
    (visitor-visit-node visitor command))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Subroutine-Hoister :execute-if-equals)
  (dolist (command (slot-value node 'commands))
    (declare (type Node command))
    (visitor-visit-node visitor command))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-dispatch-node ((hoister   Subroutine-Hoister)
                                  (node-type T)
                                  (node      Node))
  (declare (type Subroutine-Hoister hoister))
  (declare (type command-type       node-type))
  (declare (type Node               node))
  (declare (ignore                  hoister))
  (declare (ignore                  node-type))
  (declare (ignore                  node))
  (values))

;;; -------------------------------------------------------

(defun subroutine-hoister-collect-subroutines (hoister tree)
  "Applies the subroutine HOISTER to the abstract syntax TREE, stores
   the detected subroutine definitions in a hash table, and returns the
   same.
   ---
   Constituting a persisting member of the ``Subroutine-Hoister'' class
   instance, the thus populated table may be queried at any time by
   referencing the HOISTER's respective accessor functions."
  (declare (type Subroutine-Hoister hoister))
  (declare (type Node               tree))
  (visitor-visit-node hoister tree)
  (the subroutine-table (slot-value hoister 'subroutines)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-table
    :documentation "The tape as a sparse vector, implemented in terms of
                    a hash table which maps the cell indices to the
                    cell values.
                    ---
                    By responding with the default value of zero for
                    absent cells, their participation can yet be
                    simulated in a space-efficient manner.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, storing the index (key) of the
                    currently selected cell in the CELLS hash table."))
  (:documentation
    "The ``Tape'' class provides a bilaterally infinite extent of
     byte-valued cells, amenable to a pointer which designates at any
     instant a single entity as the current one."))

;;; -------------------------------------------------------

(defun make-tape ()
  "Creates and returns a new ``Tape''."
  (the Tape (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun tape-current-cell (tape)
  "Returns the value of the TAPE's current cell."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (declare (type cell-table cells))
    (declare (type integer    pointer))
    (the octet (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf tape-current-cell) (new-value tape)
  "Stores the NEW-VALUE into the TAPE's current cell and returns the
   modified TAPE.
   ---
   Upon a transgression of the cell's byte range [0, 255] the NEW-VALUE
   will be adjusted by wrapping in order to fit it into the constraints."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-slots (cells pointer) tape
    (declare (type cell-table cells))
    (declare (type integer    pointer))
    (setf (gethash pointer cells 0)
      (cond
        ((minusp new-value) (- 256 (mod new-value 255)))
        ((> new-value 255)         (mod new-value 255))
        (T                              new-value))))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-decrement (tape)
  "Decrements the value stored in the TAPE's current cell by one and
   returns the modified TAPE.
   ---
   Upon a transgression of the cell's lower bound in the byte range
   [0, 255] the cell value will be adjusted by wrapping."
  (declare (type Tape tape))
  (decf (tape-current-cell tape))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-increment (tape)
  "Increments the value stored in the TAPE's current cell by one and
   returns the modified TAPE.
   ---
   Upon a transgression of the cell's upper bound in the byte range
   [0, 255] the cell value will be adjusted by wrapping."
  (declare (type Tape tape))
  (incf (tape-current-cell tape))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Moves the TAPE's cell pointer one cell to the left and returns the
   modified TAPE."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Moves the TAPE's cell pointer one cell to the right and returns the
   modified TAPE."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Input-Queue".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Queue ()
  ((current-input
    :initarg       :current-input
    :initform      NIL
    :type          (or null character)
    :documentation "The most recently obtained character.")
   (exhausted-p
    :initarg       :exhausted-p
    :initform      T
    :type          boolean
    :documentation "Determines whether the user has ceased committing
                    input by supplying the newline character."))
  (:documentation
    "The ``Input-Queue'' represents an abstract supply of input
     characters, with the potential of exhaustion."))

;;; -------------------------------------------------------

(defun make-input-queue ()
  "Creates and returns a new ``Input-Queue''."
  (the Input-Queue (make-instance 'Input-Queue)))

;;; -------------------------------------------------------

(defun input-queue-exhausted-p (input-queue)
  (declare (type Input-Queue input-queue))
  (the boolean (slot-value input-queue 'exhausted-p)))

;;; -------------------------------------------------------

(defun input-queue-query (input-queue)
  "Inquires the INPUT-QUEUE for an input character, returning the
   received object or, if the INPUT-QUEUE is exhausted, responding with
   the ``NIL'' value."
  (declare (type Input-Queue input-queue))
  (format T "~&Please input a character: ")
  (with-slots (current-input exhausted-p) input-queue
    (declare (type (or null character) current-input))
    (declare (type boolean             exhausted-p))
    (setf current-input (read-char))
    (clear-input)
    (setf exhausted-p
      (not (null
        (or (null current-input)
            (char= current-input #\Newline)))))
    (the (or null character) current-input)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of predicate tester.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric test-equality (predicate left-operand right-operand)
  (:method ((predicate     (eql :equal))
            (left-operand  integer)
            (right-operand integer))
    (declare (type jump-predicate predicate))
    (declare (type integer        left-operand))
    (declare (type integer        right-operand))
    (the boolean
      (= left-operand right-operand)))
  
  (:method ((predicate     (eql :not-equal))
            (left-operand  integer)
            (right-operand integer))
    (declare (type jump-predicate predicate))
    (declare (type integer        left-operand))
    (declare (type integer        right-operand))
    (the boolean
      (/= left-operand right-operand)))
  
  (:method ((predicate     T)
            (left-operand  T)
            (right-operand T))
    (declare (type T predicate))
    (declare (type T left-operand))
    (declare (type T right-operand))
    (error "Invalid conditional predicate ~s applied to ~s and ~s."
      predicate left-operand right-operand))
  
  (:documentation
    "Checks whether the LEFT-OPERAND and RIGHT-OPERAND satisfy the
     PREDICATE, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter (Visitor)
  ((subroutines
    :initarg       :subroutines
    :initform      (make-hash-table :test #'equal)
    :type          subroutine-table
    :documentation "A mapping of the declared subroutine names to their
                    node representation.")
   (tape
    :initarg       :tape
    :initform      (make-tape)
    :type          Tape
    :documentation "The tape providing the program memory.")
   
   (input-queue
    :initarg       :input-queue
    :initform      (make-input-queue)
    :type          Input-Queue
    :documentation "The input characters supply."))
  (:documentation
    "The ``Interpreter'' applies itself to the evaluation of an abstract
     syntax tree in order to embue it with actual effect."))

;;; -------------------------------------------------------

(defun make-interpreter ()
  "Creates and returns a new ``Interpreter''."
  (the Interpreter (make-instance 'Interpreter)))

;;; -------------------------------------------------------

(defun interpreter-execute-subroutine (interpreter subroutine-name)
  "Invokes the subroutine register under the SUBROUTINE-NAME at the
   INTERPRETER and returns no value.
   ---
   An error of an unspecified type is signaled if the SUBROUTINE-NAME
   cannot be detected."
  (declare (type Interpreter interpreter))
  (declare (type string      subroutine-name))
  (let ((subroutine
          (gethash subroutine-name
            (slot-value interpreter 'subroutines))))
    (declare (type (or null Subroutine-Definition-Node) subroutine))
    (if subroutine
      (let ((body (subroutine-definition-node-commands subroutine)))
        (declare (type node-list body))
        (dolist (command body)
          (declare (type Node command))
          (visitor-visit-node interpreter command)))
      (error "Attempt to call undefined subroutine ~s."
        subroutine-name)))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :program)
  (dolist (command (slot-value node 'commands))
    (declare (type Node command))
    (visitor-visit-node visitor command))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :decrement)
  (tape-decrement (slot-value visitor 'tape))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :increment)
  (tape-increment (slot-value visitor 'tape))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :move-left)
  (tape-move-left (slot-value visitor 'tape))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :move-right)
  (tape-move-right (slot-value visitor 'tape))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :output-character)
  (format T "~c"
    (code-char
      (tape-current-cell
        (slot-value visitor 'tape))))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :output-integer)
  (format T "~d"
    (tape-current-cell
      (slot-value visitor 'tape)))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :output-binary)
  (format T "~8,'0b"
    (tape-current-cell
      (slot-value visitor 'tape)))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :input)
  (setf (tape-current-cell (slot-value visitor 'tape))
    (char-code
      (input-queue-query
        (slot-value visitor 'input-queue))))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :execute-if-equals)
  (let ((predicate             (execute-if-equals-node-predicate node))
        (guard                 (execute-if-equals-node-guard     node))
        (predicate-satisfied-p NIL))
    (declare (type jump-predicate predicate))
    (declare (type integer        guard))
    (declare (type boolean        predicate-satisfied-p))
    (setf predicate-satisfied-p
      (test-equality predicate
        (tape-current-cell (slot-value visitor 'tape)) guard))
    (when predicate-satisfied-p
      (dolist (command (execute-if-equals-node-commands node))
        (declare (type Node command))
        (visitor-visit-node visitor command))))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :loop-if-not-zero)
  (with-slots (tape) visitor
    (declare (type Tape tape))
    (loop until (zerop (tape-current-cell tape)) do
      (dolist (command (loop-if-not-zero-commands node))
        (declare (type Node command))
        (visitor-visit-node visitor command))))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :loop-if-no-input)
  (with-slots (input-queue) visitor
    ;; Input queue is empty?
    ;; => Repeatedly execute the body until the input queue is empty.
    (when (input-queue-exhausted-p input-queue)
      (loop do
        (dolist (command (input-loop-node-commands node))
          (declare (type Node command))
          (visitor-visit-node visitor command))
        (when (input-queue-exhausted-p input-queue)
          (loop-finish)))))
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :define-subroutine)
  (values))

;;; -------------------------------------------------------

(define-visitor-method (Interpreter :call-subroutine)
  (let ((subroutine-name (subroutine-call-node-name node)))
    (declare (type string subroutine-name))
    (interpreter-execute-subroutine visitor subroutine-name))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter tree)
  "Interprets the abstract syntax TREE using the INTERPRETER and returns
   the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type Node        tree))
  (visitor-visit-node interpreter tree)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-brainfault (code)
  "Interprets the piece of brainfault source CODE and returns no value."
  (let ((tree               (parser-parse
                              (make-parser
                                (make-lexer code))))
        (subroutine-hoister (make-subroutine-hoister))
        (interpreter        (make-interpreter)))
    (declare (type Node               tree))
    (declare (type Subroutine-Hoister subroutine-hoister))
    (declare (type Interpreter        interpreter))
    ;; Hoist and collect the subroutines.
    (setf (slot-value interpreter 'subroutines)
      (subroutine-hoister-collect-subroutines subroutine-hoister tree))
    ;; Traverse the abstract syntax tree (AST).
    (interpreter-interpret interpreter tree)
    (values)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine:
;;   ,
;;   !48(.)
;;   !49([.])
;; 
(interpret-brainfault
  ",
   !48(.)
   !49([.])")

;;; -------------------------------------------------------

;; An alternative truth-machine implementation using subroutines.
(interpret-brainfault
  "
  $queryForInput{,}
  $convertToInteger{------------------------------------------------}
  $outputInteger{:}

  $executeTruthMachine
  {
    *queryForInput*
    *convertToInteger*
    *outputInteger*
    [
     *outputInteger*
    ]
  }
  
  *executeTruthMachine*
  ")

;;; -------------------------------------------------------

;; A recursive variant of the truth-machine.
(interpret-brainfault
  "$printRecursively
   {
     .
     !49(*printRecursively*)
   }
   
   ,
   *printRecursively*")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a lack of input.
(interpret-brainfault "/ , . |")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a lack of input,
;; implemented by employing multiple subroutine definitions.
(interpret-brainfault
  "$queryInput{,}
   $printInput{.}
   $executeOneCatStep{*queryInput* *printInput*}
   $executeCatProgram{/ *executeOneCatStep* |}
   *executeCatProgram*")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a lack of input, and prints
;; the user input character's ASCII codes.
(interpret-brainfault "/ , : |")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a lack of input, and prints
;; the user input character's ASCII codes in binary form.
(interpret-brainfault "/ , ? |")

;;; -------------------------------------------------------

;; Print "Hello" (ASCII codes: 72, 101, 108, 108, 111) by adminiculum of
;; subroutines.
(interpret-brainfault
  "
  $incrementByOne{+}
  $incrementByTen{++++++++++}
  $incrementByFifty
  {
    *incrementByTen*
    *incrementByTen*
    *incrementByTen*
    *incrementByTen*
    *incrementByTen*
  }
  $decrementByOne{-}
  $printLetter{.}
  
  # Print the letter 'H' (ASCII code: 72). #
  *incrementByFifty*
  *incrementByTen*
  *incrementByTen*
  *incrementByOne*
  *incrementByOne*
  *printLetter*
  
  # Print the letter 'e' (ASCII code: 101). #
  *decrementByOne*
  *incrementByTen*
  *incrementByTen*
  *incrementByTen*
  *printLetter*
  
  # Print the letter 'l' (ASCII code: 108) twice. #
  *incrementByTen*
  *decrementByOne*
  *decrementByOne*
  *decrementByOne*
  *printLetter*
  *printLetter*
  
  # Print the letter 'o' (ASCII code: 111). #
  *incrementByOne*
  *incrementByOne*
  *incrementByOne*
  *printLetter*
  ")

;;; -------------------------------------------------------

;; Set the first cell to three (3) and check for equality to that
;; number; upon confirmation print it as a decimal value, otherwise in
;; binary form.
;; Please note that the latter case can never transpire.
(interpret-brainfault
  "*check_if_three*
   $check_if_three{+++ !3(:) !~3(?)}")

;;; -------------------------------------------------------

;; Example "encode.fault".
;; Output the character immediately following the user input character.
(interpret-brainfault
  "
  $add_twenty{++++++++++++++++++++}
  $add_hundred{*add_twenty**add_twenty**add_twenty**add_twenty**add_twenty*}
  /# while there is input in the queue #
    , # accept one byte of input #
    >[-]< # reset the flag #
    !~32(!~90(!~122(+.>+<))) # if the input is not a space, a capital Z, nor a lowercase z, add one to its ascii value, output it, and set the flag to one #
    !32(>!0(<.>)<) # if the input is a space and the flag is zero, output the space #
    !90(>!0(<[-]*add_twenty**add_twenty**add_twenty*+++++.>)<) # if the input is a capital Z and the flag is zero, ourput a capital A #
    !122(>!0(<*add_hundred*---.>)<) # if the input is a lowercase z and the flag is zero, output a lowercase a #
  |
  ")

;;; -------------------------------------------------------

;; Example "decode.fault".
;; Output the character immediately preceding the user input character.
(interpret-brainfault
  "
  $add_twenty{++++++++++++++++++++}
  $add_hundred{*add_twenty**add_twenty**add_twenty**add_twenty**add_twenty*}
  /
  ,>[-]<!~32(!~97(!~65(-.>+<)))!32(>!0(<.>)<)!97(>!0(<[-]*add_hundred**add_twenty*++.>)<)!65(>!0(<*add_hundred*----------.>)<)
  |
  ")

;;; -------------------------------------------------------

;; Example "div_two.fault".
;; Starting with the user input character's ASCII code, continously
;; output bisected value, rounded down (floor operation), until it
;; reaches zero (0).
(interpret-brainfault
  "$div_two{!~0([!1(-)!~0(-->+<)]>[-<+>]<)}$ten{++++++++++}*ten*>,:<.>[*div_two*:<.>][-]<. # continuously (floor) divide the input by two and output it while it is nonzero #")
