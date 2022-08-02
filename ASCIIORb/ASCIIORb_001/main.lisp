;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ASCIIORb", invented by the Esolang user "Threesodas".
;; 
;; Concept
;; =======
;; The esoteric programming language ASCIIORb's foundation is
;; circumscribed by three stacks whose manipulations and interoperations
;; realize a program's workings.
;; 
;; == THREE STACKS OPERATE IN COEFFICIENCY ==
;; An ASCIIORb program's operations proceed by means of stacks. In
;; dependency upon a particular telos, three such storage entities are
;; defined:
;;   
;;   - Action Stack (identifier: "STAQ"): Automatically memorizes
;;     already executed instructions for their later repetition.
;;   
;;   - Numerical Stack (identifier: "STAK"): Collects integer objects
;;     for their transfer into the Queue stack in order to convert them
;;     into characters.
;;   
;;   - Queue Stack (identifier: "STAC"): Collects integer objects, which
;;     can be printed in their respective ASCII character form.
;; 
;; ASCIIORb's rather modest capabilities offer a paravaunt generation
;; and display of an ASCII character-based message. The application of
;; which can be traced by the following principles:
;;   
;;   (1) Populate the Numerical stack (STAK) with integer values.
;;   (2) Either copy or move the integer data from the Numerical stack
;;       to the Queue stack (STAC).
;;       - In order to copy a single datum, use the command "PEK".
;;       - In order to copy the accumulated complete stack value, use
;;         the command "PEKAL".
;;       - In order to move a single datum, use the command "PUSH".
;;       - In order to move the accumulated complete stack value, use
;;         the command "PUSHAL".
;;       - In order to remove a single datum, use the command "DROP".
;;       - In order to clear the whole stack, use the command "DROPAL".
;;   (3) As an adminiculum for a facilitated operation one may use the
;;       Action stack (STAQ), repeating already executed statements a
;;       desiderated number of times.
;;       - This is accomplished by the command "REP".
;;   (4) Print the Queue stack content as characters and simultaneously
;;       terminate the program.
;;       - This service constitutes the bailiwick of the command "FIN".
;; 
;; == STATEMENTS PROCEED LINE-WISE ==
;; All operations occupy a line of their own, permitting the insertion
;; of data, their relocation, repetitions of commands, and display of
;; a stack's content.
;; 
;; 
;; Architecture
;; ============
;; The ASCIIORb's architectural department reifies as a tripartite
;; construct, separated into three stacks as its components, each
;; aligning with a dedicated purpose.
;; 
;; == THREE STACKS DESIGN THE MEMORY ==
;; A program in this language embraces a triple of distinct stacks,
;; known as the Action, Numerical, and Queue stack. A short apercu shall
;; educate about the haecceities, ere further perquisitions augment the
;; comprehension:
;;   
;;   Stack     | ID   | Purpose
;;   ----------+------+------------------------------------------------
;;   Action    | STAQ | Stores each processed instruction for latter
;;             |      | referral and repetition.
;;             |      | Any other stack operation is proscribed.
;;   ..................................................................
;;   Numerical | STAK | Permits the insertion and removal of integers
;;             |      | for later transfers into the Queue stack.
;;   ..................................................................
;;   Queue     | STAC | Permits the insertion and removal of integers
;;             |      | for later printing as ASCII characters.
;; 
;; == ACTION STACK: AN INSTRUCTION MEMORY FOR REPETITIONS ==
;; The Action Stack, identified by the code "STAQ", does not homologate
;; the liberal adjustments of its two peers, desisting from a
;; participation in nearly any command apart from the recall operation
;; "REP". Its purpose resides in the memorization and repetition of
;; already executed instructions. Each command, succeeding its
;; execution, is automatically inserted at the top of the Action stack,
;; unambiguously amenable to a zero-based index. This "stack" thus
;; rather assumes the guise of a dynamic array, the population of which
;; proceeds conceptually by insertions to its front, in lieu of the
;; tail, while physically the overthwart airt retains its dominance.
;; 
;; == NUMERICAL STACK: AN INTERMEDIATE INTEGER STORAGE ==
;; Immersed completely into the integer realm, this stack, designated
;; with the code "STAK", serves as an intermediate repository, steadable
;; in the collection of numerical objects for transmission into the
;; Queue stack, terminating in subsequent character output. The
;; traditional stack nature is in this exemplary much less compromised
;; than in the Action companion.
;; 
;; == QUEUE STACK: A CHARACTER OUTPUT FACILITY ==
;; Identified by the code "STAC", the Queue stack partakes of a nature
;; very much akin to the Numerical counterpart. Its telos, however,
;; comprises the reception of integer data, preferrable from the peer,
;; in order to generate ASCII character codes for their ultimate
;; conveyance to the standard output. Perfectly equal to its Numerical
;; kindred, the foundational data structure's attributes are not
;; violated, as opposed to the Action stack.
;; 
;; == STACK CHARACTERISTICS AND REQUIREMENTS ==
;; The characteristics of each stack, and whence issues, the
;; contemplations about its implementation must relate to its
;; responsibilities. The following requirements' impositions apply to
;; the respective members:
;;   
;;   Operation     | Action | Numerical | Queue
;;   --------------+--------+-----------+------
;;   Peek   top    | yes(a) | yes       | yes
;;   Peek   all    | yes(b) | yes       | yes
;;   Pop    top    | no     | yes       | yes
;;   Pop    all    | no     | yes       | yes
;;   Print  all    | yes    | yes       | yes
;;   Push   one    | yes(c) | yes       | yes
;;   Push   many   | no     | yes       | yes
;;   Random access | yes    | no        | no
;;   Remove one    | no     | yes       | yes
;;   Remove all    | no     | yes       | yes
;;   
;;   | Footnotes:
;;   |   (a) The "peek top" operation constitutes a special case of the
;;   |       random access by an integer index.
;;   |   (b) The "peek all" operation constitutes a special case of the
;;   |       random access by an integer index.
;;   |   (c) The "push one" operation is applied automatically, and may
;;   |       not be administered by the programmer's influence.
;; 
;; In particular the Action stack's requisite concerning a random access
;; to its elements by means of an integer index collides starkly with
;; the purist construe of the stack data type's haecceity. Please also
;; note that at most locations where the Action stack conforms to its
;; accompanying twain, to be exact, the peeking operations and the
;; pushing of an item to the top, this constitutes either a specialized
;; and thus incompatible definition in the juxtaposing context, or a
;; concomitant of the index-based inquisition. Namely, integers cannot
;; be pushed nor peeked, forecause this storage's castaldy embraces past
;; instructions instead of a numeric inventory. Merely the content
;; printing does not deviate from one's expectancies.
;; 
;; 
;; Data Types
;; ==========
;; The ASCIIORb language involves a variety of data types, their direct
;; participation establishes a dependency upon the concrete member.
;; 
;; == INTEGER ==
;; The sole numeric entity known to the language entails non-negative
;; integer values in the range [0, +infinity]. An integration of the
;; same may either manifest in the "INC" and "INQ" commands, or become
;; patent as the arguments to the "REP" operation.
;; 
;; == CHARACTER ==
;; The ASCII character set's incorporation proceeds by indirect means,
;; as a collateral of the Queue stack's construe of its own content
;; during a finalizing output operation, where its integer elements
;; experience a transliteration into this textual realm. No further
;; possibility exists for the character data statement.
;; 
;; == STACK ==
;; A treble confluence of the stack data type, furcating into the
;; Action, Numerical, and Queue variants, justifies this last-in
;; first-out (LIFO) collection's enumeration in the context of the type
;; system. Despite the subsumption into a single tier, the members'
;; intrinsics vary strongly, for which please perquire the
;; "Architecture" section.
;; 
;; 
;; Syntax
;; ======
;; ASCIIORb's syntactical delineation betokens a conspicuously
;; homogeneous design, composed of instructions only, depending on the
;; arity either supplied in prefix or infix notation.
;; 
;; == INSTRUCTIONS ==
;; A program's only effectual constituents encompass instructions, each
;; such a commorant of a line of its own, and defined by a single
;; operator in conjunction with one or two operands, any two tokens
;; segregated by at least one space.
;; 
;; Two patterns participate in the contingency of an instruction's
;; statement, bifurcating the instruction set into an equinumerant tally
;; of tiers:
;;   
;;   - UNARY OPERATIONS:
;;     Operations of this tier prepend the command type to the singular
;;     argument, ultimately exposing the structure
;;       <COMMAND-TYPE> <OPERAND>
;;     The <OPERAND> always assumes the form of a stack. A quintuple
;;     membership exhausts this principle:
;;       DROP   <STACK>
;;       DROPAL <STACK>
;;       FIN    <STACK>
;;       INC    <STACK>
;;       INQ    <STACK>
;;   
;;   - BINARY OPERATIONS:
;;     Operations of the binary ilk appropriate an infix shape, with a
;;     left operand on the prefix and a right operand on the postfix
;;     laterality of the embedded command type. The weftage thus
;;     produces the following abstraction:
;;       <LEFT-OPERAND> <COMMAND-TYPE> <RIGHT-OPERAND>
;;     Four of the five participants in this form reside in the
;;     expectancy of stacks on both argument places; an orra member
;;     substitutes the same with integers. The former category
;;     enumerates:
;;       <SOURCE-STACK> PEK    <TARGET-STACK>
;;       <SOURCE-STACK> PEKAL  <TARGET-STACK>
;;       <SOURCE-STACK> PUSH   <TARGET-STACK>
;;       <SOURCE-STACK> PUSHAL <TARGET-STACK>
;;     Our rara avis ascribes to the forbisen
;;       <INTEGER>      REP    <INTEGER>
;; 
;; == NUMBERS ==
;; The exclusive incorporation of numeric values proceeds by means of
;; unsigned non-negative integer literals, being a composition of one or
;; more decimal digits.
;; 
;; == STACKS ==
;; The three stacks comprising the available source and sink for
;; instruction effects are exhausted by their identifying designators:
;; "STAC" being Queue, "STAK" the Numerical, and "STAQ" the Action
;; specimen.
;; 
;; == WHITESPACES ==
;; In dependence upon their location, spaces and linebreaks bear the
;; potential of significance.
;; 
;; Each two instructions are separated by at least one newline
;; character. Supernumerary occurrences partake of tolerance,
;; concomitantly desisting from a contribution to the overall
;; instruction repository.
;; 
;; Each two tokens, designated by instices betwixt operators and
;; operands, require at least one sepiment in the form of a space or tab
;; character, while homologating nimiety in their distribution.
;; Instruction lines tolerate and neglect at both lateralities
;; superfluous spaces.
;; 
;; == COMMENTS ==
;; The ASCIIORb programming language embraces no concept of comments
;; yet.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) description of the
;; language's syntax may be propounded:
;;   
;;   program          := { innerLine } , finalLine ;
;;   innerLine        := line , linebreak ;
;;   finalLine        := line ;
;;   line             := [ spaces ] , [ command ] , [ spaces ] ;
;;   command          := unaryCommand | binaryCommand ;
;;   binaryCommand    := ( stack   , spaces , "PEK"    , spaces , stack   )
;;                    |  ( stack   , spaces , "PEKAL"  , spaces , stack   )
;;                    |  ( stack   , spaces , "PUSH"   , spaces , stack   )
;;                    |  ( stack   , spaces , "PUSHAL" , spaces , stack   )
;;                    |  ( integer , spaces , "REP"    , spaces , integer )
;;                    ;
;;   unaryCommand     := prefixIdentifier , spaces , stack ;
;;   prefixIdentifier := "DROP"
;;                    |  "DROPAL"
;;                    |  "FIN"
;;                    |  "INC"
;;                    |  "INQ"
;;                    ;
;;   stack            := "STAC" | "STAK" | "STAQ" ;
;;   integer          := digit , { digit } ;
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9" ;
;;   linebreak        := "\n" ;
;;   spaces           := space , { space } ;
;;   space            := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; ASCIIORb's instruction set enumerates ten members, each one of which
;; relies on either one or two arguments, with the former case
;; conditioning a prefix syntax, while the latter exhibits an infix
;; design.
;; 
;; == OVERVIEW ==
;; The following table shall introduce the ten available commands in a
;; cursory yet informative manner.
;;   
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   DROP    | Removes the topmost element from the <stack>.
;;           | Syntax:
;;           |   DROP <stack>
;;   ..................................................................
;;   DROPAL  | Removes all elements from the <stack>.
;;           | Syntax:
;;           |   DROPAL <stack>
;;   ..................................................................
;;   FIN     | Prints the content of the <stack> and terminates the
;;           | program.
;;           | Syntax:
;;           |   FIN <stack>
;;   ..................................................................
;;   INC     | Pushes the value one (1) unto the <stack>.
;;           | Syntax:
;;           |   INC <stack>
;;   ..................................................................
;;   INQ     | Pushes the value ten (10) unto the <stack>.
;;           | Syntax:
;;           |   INQ <stack>
;;   ..................................................................
;;   PEK     | Copies the topmost element from the <sourceStack>,
;;           | without removing it, to the <targetStack>.
;;           | Syntax:
;;           |   <sourceStack> PEK <targetStack>
;;   ..................................................................
;;   PEKAL   | Copies all elements from the <sourceStack>, without
;;           | removing them, in the order of their occurrence unto the
;;           | <targetStack>.
;;           | During the copying process, all elements are accumulated
;;           | into their sum, the same is finally pushed as a single
;;           | element unto the target.
;;           | Syntax:
;;           |   <sourceStack> PEKAL <targetStack>
;;   ..................................................................
;;   PUSH    | Removes the topmost element from the <sourceStack> and
;;           | pushes it unto the <targetStack>.
;;           | Syntax:
;;           |   <sourceStack> PUSH <targetStack>
;;   ..................................................................
;;   PUSHAL  | Removes all elements from the <sourceStack> and pushes
;;           | these in their given order unto the <targetStack>.
;;           | During the transfer process, all elements are accumulated
;;           | into their sum, the same is finally pushed as a single
;;           | element unto the target.
;;           | Syntax:
;;           |   <sourceStack> PUSHAL <targetStack>
;;   ..................................................................
;;   REP     | Repeats the instruction at the <actionIndex>-th position
;;           | in the Action stack a <repetitions> tally of times.
;;           | The <repetitions> must be a non-negative integer value.
;;           | The <actionIndex> must be a non-negative integer value,
;;           | with the first processed instruction enumerated with the
;;           | index zero (0).
;;           | Syntax:
;;           |   <repetitions> REP <actionIndex>
;; 
;; 
;; Implementation
;; ==============
;; This implementation of the ASCIIORb programming language emphasizes
;; simplicity as its paravaunt criterion.
;; 
;; The installation of effect into a given program proceeds by means of
;; three stages:
;;   
;;   (1) LEXICAL ANALYZATION:
;;       The ASCIIORb source string is converted into a stream of
;;       tokens.
;;   (2) PARSING:
;;       The tokens are assembled into a vector of instructions.
;;   (3) INTERPRETATION:
;;       The instruction vector is evaluated and processed.
;; 
;; == EACH STACK TYPE MANIFESTS IN A CLASS OF ITS OWN ==
;; Several contemplations goaded the meditation into the complexity
;; inherent in the various stack types' nature:
;;   
;;   (1) METHOD DISPATCHMENT:
;;       Class definitions enable the dispatchment of generic functions
;;       on their arguments.
;;       Common Lisp may dispatch on classes and object identities, but
;;       not on mere types. The variety of stack operations defined in
;;       ASCIIORb when in conflation with the three storage variants
;;       apportions a rather high degree of complexity, in the common
;;       case responding to several conditional execution paths. The
;;       Common Lisp generic functions, declared by "defgeneric" and
;;       implemented using "defmethod", permits the condition's lucid
;;       reformulation by functions whose selection accords to the
;;       language's innate dispatchment mechanism.
;;       
;;   (2) STACK IMPLEMENTATION DETAILS:
;;       The requirements imposition upon the Action stack on one hand
;;       and the Numerical and Queue Stack on the other hand helms a
;;       furcation in the implementations, which ultimately reflects in
;;       the onus encumbering the representative classes.
;;       As stated in the "Architecture" section, which please see,
;;       especially the Action stack's disrespondency to several focal
;;       stack operations, for instance the insertion of numbers or
;;       their transfer from one storage to another, serves to alienate
;;       this entity's nature from the Numerical and Queue company.
;;       Even the latter twain's consanguinity does not condition a
;;       patration in congruency, as a "FIN" operation's printing of the
;;       Numerical content would produce a number display, whereas the
;;       Queue shall construe its elements as ASCII character codes for
;;       a textual output.
;;       Whenever discrepancies in behavior render a lack in concinnity,
;;       distinct classes, supplying deviating reactions to requests,
;;       accommodate relief.
;; 
;; A readily available conclusion thus redes the establishment of three
;; classes, each a correspondence to the respective stack concept in the
;; ASCIIORb language.
;; 
;; == THE STACKS ESTABLISH A CLASS HIERARCHY ==
;; Conditioned by their intended deployments, each of the three stacks
;; exhibits a particular deportment; conditioned by the modes of
;; respondency, any of these manifests in a class of its own.
;;   
;;   Stack     | ID   | Class name
;;   ----------+------+----------------
;;   Action    | STAQ | Action-Stack
;;   Numerical | STAK | Numerical-Stack
;;   Queue     | STAC | Queue-Stack
;; 
;; Despite their ligation into an unambiguous correspondence, several
;; auxiliary entities claim a participation in the actual
;; implementation, interspersed either in the form of an interface or an
;; abstract base class. The complete enumeration of these constituents
;; thus founds the following:
;;   
;;   Class/Interface | Description
;;   ----------------+-------------------------------------------------
;;   Stack           | The interface for all stack subtypes.
;;   ..................................................................
;;   Abstract-Stack  | An abstract superclass which conjoins the
;;                   | intrinsics of the Numerical stack "STAK" and the
;;                   | Queue stack "STAC".
;;   ..................................................................
;;   Numerical-Stack | Represents the Numerical stack "STAK".
;;   ..................................................................
;;   Queue-Stack     | Represents the Queue stack "STAC".
;;   ..................................................................
;;   Action-Stack    | Represents the Action stack "STAQ".
;; 
;; As by the tabular exposition's adminicle gaining conspicuity, the
;; storage components' discrepancies have been molded into a rather
;; convolute class hierarchy, the same shall be following UML class
;; diagram's material:
;; 
;;                +---------------+
;;                | <<interface>> |
;;                |     Stack     |
;;                +---------------+
;;                        ^
;;                        |
;;            +-----------+-----------+
;;            |                       |
;;   +--------+-------+       +-------+------+
;;   | <<abstract>>   |       | Action-Stack |
;;   | Abstract-Stack |       +--------------+
;;   +----------------+
;;            ^
;;            |
;;            +-----------------------+
;;            |                       |
;;   +--------+--------+      +-------+-----+
;;   | Numerical-Stack |      | Queue-Stack |
;;   +-----------------+      +-------------+
;; 
;; == INSTRUCTIONS CONTAIN A TYPE AND OPERANDS ==
;; Instructions are represented by an eponymous "Instruction" class,
;; the diorism of which encompasses, beside a categorizing type
;; identifier, a list of zero or more operands, each of which may
;; constitute a non-negative integer value or a stack designation,
;; agnominated by the dedicated declaration of the "operand-type".
;; 
;; "Instruction" objects present the currency by which the Action stack
;; administers remembrance to the consumed operations.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its detailed exposition, the original ASCIIORb specification
;; displays some mete of wite regarding the omission of meticulously
;; delineated facets. A few ones peisant in their nature shall be
;; enumerated alow, without the claim of the topic's exhaustion.
;; 
;; == WHEN SHALL ACTIONS BE INSERTED INTO THE ACTION STACK? ==
;; The exact point of admission of a processed instruction into the
;; Action stack remains an underspecified item.
;; 
;; Theoretically, no provisions mandate the insertion of an action
;; succeeding its evaluation. This means that the "REP" command could
;; reference the index of its own invocation. Given the forbisen
;;   
;;   INC STAK
;;   5 REP 1
;; 
;; the action "5 REP 1" would assume the index 1. If not yet registered
;; in the Action stack, the request would either be requitted with an
;; error or with neglect; on the other hand, if homologated, an infinite
;; recursion would be incurred, rendering the program with meager lucre.
;; 
;; Based upon the otioseness of incorporating the potential of this
;; infinite self-reference, the inclusion of an instruction as an item
;; to the Action stack has been adjudged as per force following its
;; successful termination. A "REP" command's index, as a consectary, is
;; not defined, and thus unavailable, at the time of its own execution.
;; 
;; == DO "INC" AND "INQ" OPERATE ON THE NUMERICAL STACK ONLY? ==
;; Regarding the operations "INC" and "INQ", intended to insert an
;; integer value of one (1) or ten (10) respectively unto a specified
;; stack, the detailed elucidation states the claim that its amenability
;; resolves exclusively to the Numerical stack; concomitantly, the
;; preceding apercu eschews such a constraint. It is conjectured that
;; this property shall avail in a sharper demarcation betwixt the
;; purposes of the Numerical and the Queue storages, the former of which
;; poses merely as a provisional repository whence the latter is
;; nurtured.
;; 
;; It has been chosen in this implementation to homologate both stacks
;; to access the "INC" and "INQ" operations' services.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-07-27
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/ASCIIORb"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
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
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
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
   entailing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype stack-type ()
  "The ``stack-type'' type enumerates the recognized stack identifiers."
  '(member :STAC :STAK :STAQ))

;;; -------------------------------------------------------

(deftype unsigned-integer ()
  "The ``unsigned-integer'' type defines an non-negative integer value
   of unbounded magnitude."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype operand-type ()
  "The ``operand-type'' defines the value types permissive for an
   instruction operand's assumption."
  '(or unsigned-integer stack-type))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant portion of an analyzed
   piece of ASCIIORb source code."
  (type (error "Missing token type.") :type keyword)
  (value NIL                          :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(declaim (type (hash-table-of string keyword) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Associates with the ASCIIORb language keywords the respective
   tokens.")

;;; -------------------------------------------------------

(flet ((add-identifier (name token-type)
        "Creates a new token whose type and value both resolves to the
         TOKEN-TYPE, associates the NAME with this new entity in the
         global +IDENTIFIERS+ hash table, and returns no value."
        (declare (type string  name))
        (declare (type keyword token-type))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type token-type))
        (values)))
  (add-identifier "DROP"   :DROP)
  (add-identifier "DROPAL" :DROPAL)
  (add-identifier "FIN"    :FIN)
  (add-identifier "INC"    :INC)
  (add-identifier "INQ"    :INQ)
  (add-identifier "PEK"    :PEK)
  (add-identifier "PEKAL"  :PEKAL)
  (add-identifier "PUSH"   :PUSH)
  (add-identifier "PUSHAL" :PUSHAL)
  (add-identifier "REP"    :REP)
  (add-identifier "STAC"   :STAC)
  (add-identifier "STAK"   :STAK)
  (add-identifier "STAQ"   :STAQ)
  (values))

;;; -------------------------------------------------------

(defun get-token-for-identifier (identifier)
  "Returns the token associated with the IDENTIFIER, or signals an error
   of an unspecified type if no correspondence exists."
  (declare (type string identifier))
  (or (gethash identifier +IDENTIFIERS+)
      (error "Unrecognized identifier: ~s." identifier)))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      ""
    :type          string
    :documentation "The current line of the ASCIIORb source to
                    tokenize.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The position into the current SOURCE line.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The characte at the POSITION into the current
                    SOURCE line.
                    ---
                    If resolving to the ``NIL'' value, the SOURCE has
                    been exhausted."))
  (:documentation
    "The ``Lexer'' class provides an lexical analyzer for the separation
     of a piece of ASCIIORb source code into its tokens."))

;;; -------------------------------------------------------

(defun make-lexer ()
  "Creates and returns a new ``Lexer'' which initially operates on an
   empty source string."
  (the Lexer
    (make-instance 'Lexer)))

;;; -------------------------------------------------------

(defun lexer-set-source (lexer new-source)
  "Sets the LEXER's source to the NEW-SOURCE, updates its internal
   state, and returns the modified LEXER."
  (declare (type Lexer  lexer))
  (declare (type string new-source))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf source   new-source)
    (setf position 0)
    (setf character
      (when (array-in-bounds-p source position)
        (aref source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character in its source, if possible,
   updates its internal state, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (aref source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-newlines (lexer)
  "Starting at the current position in the LEXER, reads zero or more
   subsequent newline characters and creates and returns a token
   representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :eol
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop
            while
              (and character (char= character #\Newline))
            do
              (write-char character content)
              (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-spaces (lexer)
  "Starting at the current position in the LEXER, reads zero or more
   subsequent space characters and creates and returns a token
   representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :spaces
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop
            while
              (and character (space-character-p character))
            do
              (write-char character content)
              (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position in the LEXER, reads an unsigned
   integer number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, reads and returns an
   identifier token.
   ---
   An error of an unspecified type is signaled if the consumed
   identifier name cannot be associated with any recognized token."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (get-token-for-identifier
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (and character (alpha-char-p character)) do
            (write-char character identifier)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each query with
   a new end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((char= character #\Newline)
          (lexer-read-newlines lexer))
        
        ((space-character-p character)
          (lexer-read-spaces lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &rest operands)))
  "The ``Instruction'' class bundles the information relating to a
   statement in an ASCIIORb program, involving both the command type and
   its parameters."
  (type
    (error "Missing instruction type.")
    :type keyword)
  (operands
    NIL
    :type (list-of operand-type)))

;;; -------------------------------------------------------

(defun instruction-operand-at (instruction operand-index)
  "Returns the INSTRUCTION's operand located at the OPERAND-INDEX."
  (declare (type Instruction instruction))
  (declare (type fixnum      operand-index))
  (the operand-type
    (elt (instruction-operands instruction) operand-index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack-token-p (token)
  "Checks whether the TOKEN represents a stack, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token) '(:STAC :STAK :STAQ) :test #'eq)))))

;;; -------------------------------------------------------

(defun binary-operator-token-p (token)
  "Checks whether the TOKEN represents a binary operator name, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token)
        '(:PEK :PEKAL :PUSH :PUSHAL) :test #'eq)))))

;;; -------------------------------------------------------

(defun end-of-line-token-p (token)
  "Checks whether the TOKEN represents the end of a line or the file,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token) '(:eol :eof) :test #'eq)))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts from the piece of ASCIIORb CODE the incorporated
   instructions and returns these in a one-dimensional simple array."
  (declare (type string code))
  
  (let ((instructions NIL)
        (lexer        (make-lexer))
        (token        (make-token :eof NIL)))
    (declare (type (list-of Instruction) instructions))
    (declare (type Lexer                 lexer))
    (declare (type Token                 token))
    
    (labels
        ((advance ()
          "Queries the next token from the LEXER, updates the current
           TOKEN, and returns no value."
          (setf token (lexer-get-next-token lexer))
          (values))
         
         (skip-spaces ()
          "Skips a sequence of zero or more space tokens, sets the
           current TOKEN to the first non-space token, and returns no
           value."
          (loop while (token-type-p token :spaces) do
            (advance))
          (values))
         
         (expect (expected-token-type)
          "Checks whether the current TOKEN conforms to the
           EXPECTED-TOKEN-TYPE, on confirmation returning no value,
           otherwise signaling an error of an unspecified type."
          (declare (type keyword expected-token-type))
          (unless (token-type-p token expected-token-type)
            (error "Expected a token of the type ~s, but encountered ~s."
              expected-token-type token))
          (values))
         
         (expect-stack ()
          "Checks whether the current TOKEN represents a stack name,
           on confirmation returning no value, otherwise signaling an
           error of an unspecified type."
          (unless (stack-token-p token)
            (error "Unexpected token ~s instead of a stack." token))
          (values))
         
         (expect-binary-operator ()
          "Checks whether the current TOKEN represents the name of a
           binary command, returning no value, otherwise signaling an
           error of an unspecified type."
          (unless (binary-operator-token-p token)
            (error "Expected a binary operator token, but encountered ~s."
              token))
          (values))
         
         (eat-end-of-line ()
          "Expecting this to be the portion following a completed
           instruction on a line, skips zero or more successive space
           tokens, and checks whether the end of the line or the file
           has been reached, on confirmation querying the next token
           from the LEXER, updating the current TOKEN, and returning no
           value; otherwise signals an error of an unspecified type."
          (skip-spaces)
          (if (end-of-line-token-p token)
            (advance)
            (error "Expected end of line or file, but encountered ~s."
              token))
          (values))
         
         (eat (expected-token-type)
          "Checks whether the current TOKEN conforms to the
           EXPECTED-TOKEN-TYPE, on confirmation querying the next token
           from the LEXER and stores it in the current TOKEN, finally
           returning no value; otherwise signals an error of an
           unspecified type."
          (declare (type keyword expected-token-type))
          (if (token-type-p token expected-token-type)
            (advance)
            (error "Expected a token of the type ~s, but encountered ~s."
              expected-token-type token))
          (values))
         
         (collect-instruction (instruction)
          "Adds the INSTRUCTION to the head of the INSTRUCTIONS list and
           returns no value."
          (declare (type Instruction instruction))
          (push instruction instructions)
          (values)))
      
      (with-input-from-string (input code)
        (declare (type string-stream input))
        (loop
          for   line of-type (or null string) = (read-line input NIL)
          while line
          do
            ;; Update the LEXER to operate on the current LINE.
            (lexer-set-source lexer line)
            ;; Initialize the current TOKEN to the LINE's first token.
            (advance)
            ;; Skips contingent leading spaces at the LINE start.
            (skip-spaces)
            
            (case (token-type token)
              ;; End of the current LINE.
              ((:eol :eof)
                (advance))
              
              ;; Unary instruction.
              ((:INC :INQ :PUSH :PUSHAL :DROP :DROPAL :FIN)
                (let ((operator NIL)
                      (stack    NIL))
                  (declare (type (or null keyword)    operator))
                  (declare (type (or null stack-type) stack))
                  
                  (setf operator (token-value token))
                  (advance)
                  (eat :spaces)
                  (expect-stack)
                  (setf stack (token-value token))
                  (advance)
                  (eat-end-of-line)
                  
                  (collect-instruction
                    (make-instruction operator stack))))
              
              ;; Binary instruction operating on two stacks.
              ((:STAC :STAK :STAQ)
                (let ((source-stack NIL)
                      (operator     NIL)
                      (target-stack NIL))
                  (declare (type (or null stack-type) source-stack))
                  (declare (type (or null keyword)    operator))
                  (declare (type (or null stack-type) target-stack))
                  
                  (setf source-stack (token-value token))
                  (advance)
                  (eat :spaces)
                  (expect-binary-operator)
                  (setf operator (token-value token))
                  (advance)
                  (eat :spaces)
                  (expect-stack)
                  (setf target-stack (token-value token))
                  (advance)
                  (eat-end-of-line)
                  
                  (collect-instruction
                    (make-instruction
                      operator source-stack target-stack))))
              
              ;; Binary instruction which operates on two integers.
              (:number
                (let ((repetitions  0)
                      (action-index 0))
                  (declare (type unsigned-integer repetitions))
                  (declare (type unsigned-integer action-index))
                  
                  (setf repetitions (token-value token))
                  (advance)
                  (eat :spaces)
                  (eat :REP)
                  (eat :spaces)
                  (expect :number)
                  (setf action-index (token-value token))
                  (advance)
                  (eat-end-of-line)
                  
                  (collect-instruction
                    (make-instruction :REP repetitions action-index))))
              
              ;; Invalid first token of the LINE.
              (otherwise
                (error "Invalid instruction token: ~s." token))))))
    
    (the (simple-array Instruction (*))
      (coerce (nreverse instructions)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Stack".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ()
  (:documentation
    "The ``Stack'' interface defines the common base for all stack
     sub-classes"))

;;; -------------------------------------------------------

(defgeneric stack-inc (stack)
  (:documentation
    "Pushes the integer value one (1) unto the STACK and returns no
     value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-inq (stack)
  (:documentation
    "Pushes the integer value ten (10) unto the STACK and returns no
     value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-drop (stack)
  (:documentation
    "Removes the topmost element from the STACK without returning it and
     returns no value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-dropal (stack)
  (:documentation
    "Removes all elements from the STACK and returns no value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-push (target-stack source-stack)
  (:documentation
    "Pops the top element from the SOURCE-STACK and pushes it unto the
     TARGET-STACK, returning no value.
     ---
     If this operation is not valid for either the TARGET-STACK or the
     SOURCE-STACK, an error of the type ``Unsupported-Operation-Error''
     is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-pushal (target-stack source-stack)
  (:documentation
    "Pops all elements from the SOURCE-STACK and pushes these unto the
     TARGET-STACK, returning no value.
     ---
     If this operation is not valid for either the TARGET-STACK or the
     SOURCE-STACK, an error of the type ``Unsupported-Operation-Error''
     is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-pek (target-stack source-stack)
  (:documentation
    "Retrieves without returning the top element from the SOURCE-STACK
     and pushes it unto the TARGET-STACK, returning no value.
     ---
     If this operation is not valid for either the TARGET-STACK or the
     SOURCE-STACK, an error of the type ``Unsupported-Operation-Error''
     is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-pekal (target-stack source-stack)
  (:documentation
    "Retrieves without returning all elements from the SOURCE-STACK and
     pushes these unto the TARGET-STACK, returning no value.
     ---
     If this operation is not valid for either the TARGET-STACK or the
     SOURCE-STACK, an error of the type ``Unsupported-Operation-Error''
     is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-rep (stack action-index)
  (:documentation
    "Returns the instruction stored at the ACTION-INDEX in the STACK.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-fin (stack)
  (:documentation
    "Prints the STACK's content to the standard output and returns no
     value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric stack-store-action (stack new-action)
  (:documentation
    "Pushes the NEW-ACTION unto the top of the STACK and returns no
     value.
     ---
     If this operation is not valid for the STACK, an error of the type
     ``Unsupported-Operation-Error'' is signaled."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Unsupported-Operation-Error".   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Unsupported-Operation-Error (error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        unsupported-operation-error-offended-stack
    :type          Stack
    :documentation "The stack lacking an amenability to the operation
                    represented by the OFFENDING-OPERATION.")
   (offending-operation
    :initarg       :offending-operation
    :initform      (error "Missing offending operation.")
    :reader        unsupported-operation-error-offending-operation
    :type          string
    :documentation "A name chosen to represent the operation whose
                    disrespondency with the OFFENDED-STACK instigated
                    this troublesome condition."))
  (:report
    (lambda (condition stream)
      (declare (type Unsupported-Operation-Error condition))
      (declare (type destination                 stream))
      (format stream "The stack ~s does not support the ~a operation."
        (unsupported-operation-error-offended-stack      condition)
        (unsupported-operation-error-offending-operation condition))))
  (:documentation
    "The ``Unsupported-Operation-Error'' class serves in the
     notification about an operation attempted on a stack incompatible
     with its request."))

;;; -------------------------------------------------------

(defun signal-unsupported-operation (offended-stack offending-operation)
  "Signals an ``Unsupported-Operation-Error'' which relates the
   OFFENDING-OPERATION with the OFFENDED-STACK."
  (declare (type Stack  offended-stack))
  (declare (type string offending-operation))
  (error 'Unsupported-Operation-Error
    :offended-stack      offended-stack
    :offending-operation offending-operation))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Action-Stack".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Action-Stack (Stack)
  ((actions
    :initarg       :actions
    :initform      (make-array 0
                     :element-type    'Instruction
                     :initial-element (make-instruction :dummy)
                     :adjustable      T
                     :fill-pointer    0)
    :type          (vector Instruction *)
    :documentation "A dynamic vector of instructions, which, albeit its
                    appendage to the tail, shall serve as a stack."))
  (:documentation
    "The ``Action-Stack'' class realizes the action stack, the nature of
     which deviates to some mete from the basic capabilities of the
     traditional stack ADT by an abstinence from the removal operation
     (\"pop\"), as well as the inclusion of an adscititous facility to
     homologate random access to the elements under its castaldy."))

;;; -------------------------------------------------------

(defun make-action-stack ()
  "Creates and returns a new empty ``Action-Stack''."
  (the Action-Stack
    (make-instance 'Action-Stack)))

;;; -------------------------------------------------------

(defmethod stack-inc ((stack Action-Stack))
  (declare (type Action-Stack stack))
  (signal-unsupported-operation stack "INC"))

;;; -------------------------------------------------------

(defmethod stack-inq ((stack Action-Stack))
  (declare (type Action-Stack stack))
  (signal-unsupported-operation stack "INQ"))

;;; -------------------------------------------------------

(defmethod stack-drop ((stack Action-Stack))
  (declare (type Action-Stack stack))
  (signal-unsupported-operation stack "DROP"))

;;; -------------------------------------------------------

(defmethod stack-dropal ((stack Action-Stack))
  (declare (type Action-Stack stack))
  (signal-unsupported-operation stack "DROPAL"))

;;; -------------------------------------------------------

(defmethod stack-push ((target-stack Action-Stack)
                       (source-stack Stack))
  (declare (type Action-Stack target-stack))
  (declare (type Stack        source-stack))
  (signal-unsupported-operation target-stack "PUSH"))

;;; -------------------------------------------------------

(defmethod stack-push ((target-stack Stack)
                       (source-stack Action-Stack))
  (declare (type Stack        target-stack))
  (declare (type Action-Stack source-stack))
  (signal-unsupported-operation target-stack "PUSH"))

;;; -------------------------------------------------------

(defmethod stack-pushal ((target-stack Action-Stack)
                         (source-stack Stack))
  (declare (type Action-Stack target-stack))
  (declare (type Stack        source-stack))
  (signal-unsupported-operation target-stack "PUSHAL"))

;;; -------------------------------------------------------

(defmethod stack-pushal ((target-stack Stack)
                         (source-stack Action-Stack))
  (declare (type Stack        target-stack))
  (declare (type Action-Stack source-stack))
  (signal-unsupported-operation target-stack "PUSHAL"))

;;; -------------------------------------------------------

(defmethod stack-pek ((target-stack Action-Stack)
                      (source-stack Stack))
  (declare (type Action-Stack target-stack))
  (declare (type Stack        source-stack))
  (signal-unsupported-operation target-stack "PEK"))

;;; -------------------------------------------------------

(defmethod stack-pek ((target-stack Stack)
                      (source-stack Action-Stack))
  (declare (type Stack        target-stack))
  (declare (type Action-Stack source-stack))
  (signal-unsupported-operation target-stack "PEK"))

;;; -------------------------------------------------------

(defmethod stack-pekal ((target-stack Action-Stack)
                        (source-stack Stack))
  (declare (type Action-Stack target-stack))
  (declare (type Stack        source-stack))
  (signal-unsupported-operation target-stack "PEKAL"))

;;; -------------------------------------------------------

(defmethod stack-pekal ((target-stack Stack)
                        (source-stack Action-Stack))
  (declare (type Stack        target-stack))
  (declare (type Action-Stack source-stack))
  (signal-unsupported-operation target-stack "PEKAL"))

;;; -------------------------------------------------------

(defmethod stack-rep ((stack        Action-Stack)
                      (action-index integer))
  (declare (type Action-Stack     stack))
  (declare (type unsigned-integer action-index))
  (with-slots (actions) stack
    (declare (type (vector Instruction *) actions))
    (the Instruction
      (if (array-in-bounds-p actions action-index)
        (aref actions action-index)
        (error "The index ~d designates an invalid position in the ~
                action stack ~s."
          action-index stack)))))

;;; -------------------------------------------------------

(defmethod stack-fin ((stack Action-Stack))
  (declare (type Action-Stack stack))
  (with-slots (actions) stack
    (declare (type (vector Instruction *) actions))
    (loop
      for action-index
        downfrom (1- (length actions))
        to       0
      for action
        of-type Instruction
        =       (aref actions action-index)
      do
        (case (instruction-type action)
          ((:DROP :DROPAL :FIN :INC :INQ)
            (format T "~&~a ~a"
              (instruction-type       action)
              (instruction-operand-at action 0)))
          ((:PEK :PEKAL :PUSH :PUSHAL :REP)
            (format T "~&~a ~a ~a"
              (instruction-operand-at action 0)
              (instruction-type       action)
              (instruction-operand-at action 1)))
          (otherwise
            (error "Invalid instruction to STACK-FIN: ~s." action)))))
  (values))

;;; -------------------------------------------------------

(defmethod stack-store-action ((stack      Action-Stack)
                               (new-action Instruction))
  (declare (type Action-Stack stack))
  (declare (type Instruction  new-action))
  (vector-push-extend new-action
    (slot-value stack 'actions))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Abstract-Stack".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Abstract-Stack (Stack)
  ((elements
    :initarg       :elements
    :initform      NIL
    :type          (list-of unsigned-integer)
    :documentation "A list used in the agency of a stack, storing
                    non-negative integer objects."))
  (:documentation
    "The ``Abstract-Stack'' class provides a common base for the
     numerical and queue stacks, both of which participate verbatim in
     the traditional stack ADT haecceity."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Numerical-Stack".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Numerical-Stack (Abstract-Stack)
  ()
  (:documentation
    "The ``Numerical-Stack'' represents the Numerical stack, identified
     by the name \"STAK\"."))

;;; -------------------------------------------------------

(defun make-numerical-stack ()
  "Creates and returns an empty ``Numerical-Stack''."
  (the Numerical-Stack
    (make-instance 'Numerical-Stack)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Queue-Stack".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue-Stack (Abstract-Stack)
  ()
  (:documentation
    "The ``Queue-Stack'' represents the Queue stack, identified by the
     name \"STAC\"."))

;;; -------------------------------------------------------

(defun make-queue-stack ()
  "Creates and returns an empty ``Queue-Stack''."
  (the Queue-Stack
    (make-instance 'Queue-Stack)))

;;; -------------------------------------------------------

(defmethod stack-inc ((stack Abstract-Stack))
  (declare (type Abstract-Stack stack))
  (push 1 (slot-value stack 'elements))
  (values))

;;; -------------------------------------------------------

(defmethod stack-inq ((stack Abstract-Stack))
  (declare (type Abstract-Stack stack))
  (push 10 (slot-value stack 'elements))
  (values))

;;; -------------------------------------------------------

(defmethod stack-push ((target-stack Abstract-Stack)
                       (source-stack Abstract-Stack))
  (declare (type Abstract-Stack target-stack))
  (declare (type Abstract-Stack source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (push (pop source-elements) target-elements)))
  (values))

;;; -------------------------------------------------------

(defmethod stack-pushal ((target-stack Numerical-Stack)
                         (source-stack Abstract-Stack))
  (declare (type Numerical-Stack target-stack))
  (declare (type Abstract-Stack  source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (loop while source-elements do
        (push (pop source-elements) target-elements))))
  (values))

;;; -------------------------------------------------------

(defmethod stack-pushal ((target-stack Queue-Stack)
                         (source-stack Abstract-Stack))
  (declare (type Queue-Stack    target-stack))
  (declare (type Abstract-Stack source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (loop
        while   source-elements
        sum     (pop source-elements)
        into    accumulated-value
        finally (push accumulated-value target-elements))))
  (values))

;;; -------------------------------------------------------

(defmethod stack-pek ((target-stack Abstract-Stack)
                      (source-stack Abstract-Stack))
  (declare (type Abstract-Stack target-stack))
  (declare (type Abstract-Stack source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (push (first source-elements) target-elements)))
  (values))

;;; -------------------------------------------------------

(defmethod stack-pekal ((target-stack Numerical-Stack)
                        (source-stack Abstract-Stack))
  (declare (type Numerical-Stack target-stack))
  (declare (type Abstract-Stack  source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (dolist (source-element source-elements)
        (declare (type unsigned-integer source-element))
        (push source-element target-elements))))
  (values))

;;; -------------------------------------------------------

(defmethod stack-pekal ((target-stack Queue-Stack)
                        (source-stack Abstract-Stack))
  (declare (type Queue-Stack    target-stack))
  (declare (type Abstract-Stack source-stack))
  (with-slots ((target-elements elements)) target-stack
    (declare (type (list-of unsigned-integer) target-elements))
    (with-slots ((source-elements elements)) source-stack
      (declare (type (list-of unsigned-integer) source-elements))
      (loop
        for     source-element of-type unsigned-integer in source-elements
        sum     source-element
        into    accumulated-value
        finally (push accumulated-value target-elements))))
  (values))

;;; -------------------------------------------------------

(defmethod stack-drop ((stack Abstract-Stack))
  (declare (type Abstract-Stack stack))
  (with-slots (elements) stack
    (declare (type (list-of unsigned-integer) elements))
    (if elements
      (pop elements)
      (error "Cannot pop from an empty stack.")))
  (values))

;;; -------------------------------------------------------

(defmethod stack-dropal ((stack Abstract-Stack))
  (declare (type Abstract-Stack stack))
  (setf (slot-value stack 'elements) NIL)
  (values))

;;; -------------------------------------------------------

(defmethod stack-rep ((stack        Abstract-Stack)
                      (action-index integer))
  (declare (type Action-Stack     stack))
  (declare (type unsigned-integer action-index))
  (declare (ignore                action-index))
  (signal-unsupported-operation stack "REP"))

;;; -------------------------------------------------------

(defmethod stack-fin ((stack Numerical-Stack))
  (declare (type Numerical-Stack stack))
  (format T "~{~a~^, ~}" (slot-value stack 'elements))
  (values))

;;; -------------------------------------------------------

(defmethod stack-fin ((stack Queue-Stack))
  (declare (type Queue-Stack stack))
  (dolist (element (slot-value stack 'elements))
    (declare (type unsigned-integer element))
    (format T "~c" (code-char element)))
  (values))

;;; -------------------------------------------------------

(defmethod stack-store-action ((stack      Abstract-Stack)
                               (new-action Instruction))
  (declare (type Action-Stack stack))
  (declare (type Instruction  new-action))
  (declare (ignore            new-action))
  (signal-unsupported-operation stack "STORE ACTION"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-stack-table ()
  "Creates and returns a hash table which maps each ``stack-type'' to a
   fresh instance of the appropriate ``Stack'' subclass, thus permitting
   an access to the latter by the former's adminiculum."
  (let ((stack-table (make-hash-table :test #'eq)))
    (declare (type (hash-table-of stack-type Stack) stack-table))
    (setf (gethash :STAC stack-table) (make-queue-stack))
    (setf (gethash :STAK stack-table) (make-numerical-stack))
    (setf (gethash :STAQ stack-table) (make-action-stack))
    (the (hash-table-of stack-type Stack) stack-table)))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          (vector Instruction *)
    :documentation "The instructions to process.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer index into the INSTRUCTIONS
                    vector.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null Instruction)
    :documentation "The instruction at the current instruction pointer
                    position IP in the INSTRUCTIONS vector.")
   (stacks
    :initarg       :stacks
    :initform      (build-stack-table)
    :type          (hash-table-of stack-type Stack)
    :documentation "Associates the recognized stack identifiers with the
                    actual ``Stack'' instances in currency."))
  (:documentation
    "The ``Interpreter'' class applies itself to the processing of an
     instruction sequence."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' which operates on the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-stack (interpreter stack-type)
  "Returns the ``Stack'' instance associated with the STACK-TYPE in the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type stack-type  stack-type))
  (the Stack
    (gethash stack-type
      (slot-value interpreter 'stacks))))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Moves the INTERPRETER's internally managed instruction to the next
   position in its instruction vector, if possible, updates the
   currently active instruction, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions (1+ ip))
        (aref instructions (incf ip)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric dispatch-instruction (interpreter
                                  instruction-type
                                  instruction)
  (:documentation
    "Processes the INSTRUCTION of the INSTRUCTION-TYPE using the
     INTERPRETER and returns no value."))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Processes the INSTRUCTION using the INTERPRETER by invoking the
   respective ``dispatch-instruction'' function and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (dispatch-instruction interpreter
    (instruction-type instruction) instruction)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :DROP))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (stack-drop
    (interpreter-stack interpreter
      (instruction-operand-at instruction 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :DROPAL))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (stack-dropal
    (interpreter-stack interpreter
      (instruction-operand-at instruction 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :FIN))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (stack-fin
    (interpreter-stack interpreter
      (instruction-operand-at instruction 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :INC))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (stack-inc
    (interpreter-stack interpreter
      (instruction-operand-at instruction 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :INQ))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (stack-inq
    (interpreter-stack interpreter
      (instruction-operand-at instruction 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :PEK))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (destructuring-bind (source-stack target-stack)
      (instruction-operands instruction)
    (declare (type stack-type source-stack))
    (declare (type stack-type target-stack))
    (stack-pek
      (interpreter-stack interpreter target-stack)
      (interpreter-stack interpreter source-stack)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :PEKAL))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (destructuring-bind (source-stack target-stack)
      (instruction-operands instruction)
    (declare (type stack-type source-stack))
    (declare (type stack-type target-stack))
    (stack-pekal
      (interpreter-stack interpreter target-stack)
      (interpreter-stack interpreter source-stack)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :PUSH))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (destructuring-bind (source-stack target-stack)
      (instruction-operands instruction)
    (declare (type stack-type source-stack))
    (declare (type stack-type target-stack))
    (stack-push
      (interpreter-stack interpreter target-stack)
      (interpreter-stack interpreter source-stack)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :PUSHAL))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (declare (ignore           instruction-type))
  (destructuring-bind (source-stack target-stack)
      (instruction-operands instruction)
    (declare (type stack-type source-stack))
    (declare (type stack-type target-stack))
    (stack-pushal
      (interpreter-stack interpreter target-stack)
      (interpreter-stack interpreter source-stack)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-instruction ((interpreter      Interpreter)
                                 (instruction-type (eql :REP))
                                 (instruction      Instruction))
  (declare (type Interpreter interpreter))
  (declare (type keyword     instruction-type))
  (declare (type Instruction instruction))
  (destructuring-bind (repetitions action-index)
      (instruction-operands instruction)
    (declare (type unsigned-integer repetitions))
    (declare (type unsigned-integer action-index))
    (let ((instruction-to-repeat
            (stack-rep
              (interpreter-stack interpreter :STAQ)
              action-index)))
      (declare (type Instruction instruction-to-repeat))
      (loop repeat repetitions do
        (process-instruction interpreter instruction-to-repeat))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the INTERPRETER's instructions and returns the modified
   INTEPRETER."
  (declare (type Interpreter interpreter))
  
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type (or null Instruction)  current-instruction))
    (loop while current-instruction do
      (let ((action current-instruction))
        (declare (type Instruction action))
        (process-instruction interpreter current-instruction)
        (stack-store-action
          (interpreter-stack interpreter :STAQ) action))
      (interpreter-advance-ip interpreter)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-ASCIIORb (code)
  "Interprets the piece of ASCIIORb CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (extract-instructions code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ASCIIORb text generator.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-repetition-counts (character)
  "Determines the number of occurrences of the quantity ten (10) and
   one (10) requisite to replicate the CHARACTER's ASCII code and
   returns two values:
     (1) a non-negative integer equal to the number of occurrences of
         the quantity ten (10) in the CHARACTER's ASCII code
     (2) a non-negative integer equal to the number of occurrences of
         the quantity one (1) in the CHARACTER's ASCII code after
         being reduced by the occurrences of ten (10).
   ---
   Effectively, the following equality holds:
     
     (char-code CHARACTER)
     == (multiple-value-bind (tally-of-tens tally-of-ones)
            (get-repetition-counts CHARACTER)
          (+ (* tally-of-tens 10)
             (* tally-of-ones  1)))"
  (declare (type character character))
  (the (values (integer 0 *) (integer 0 *))
    (floor (char-code character) 10)))

;;; -------------------------------------------------------

(defun generate-text-program (text &key (destination NIL))
  "Generates an ASCIIORb program capable of printing the TEXT to the
   standard output and writes its source code to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   producing and returning a new string containing the result."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for character-index
          of-type fixnum
          from    (1- (length text))
          downto  0
        for current-character
          of-type character
          =       (char text character-index)
        do
          (multiple-value-bind (tally-of-tens tally-of-ones)
              (get-repetition-counts current-character)
            (declare (type (integer 0 *) tally-of-tens))
            (declare (type (integer 0 *) tally-of-ones))
            
            (loop repeat tally-of-tens do
              (format destination "~&INQ STAK"))
            
            (loop repeat tally-of-ones do
              (format destination "~&INC STAK"))
            
            (format destination "~&STAK PUSHAL STAC"))
        finally
          (format destination "~&FIN STAC"))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program text :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-ASCIIORb
  "INQ STAK
   9 REP 0
   STAK PUSHAL STAC
   FIN STAC")

;;; -------------------------------------------------------

;; Generate the character "e" by computing
;;   1 + 10 + (9 * 10) = 101 => ASCII code for "e".
(interpret-ASCIIORb
  "INC STAK
   INQ STAK
   9 REP 1
   STAK PUSHAL STAC
   FIN STAC")

;;; -------------------------------------------------------

;; Create an output of ten times the character associated with the
;; ASCII code one (1).
(interpret-ASCIIORb
  "INC STAK
   STAK PEK STAC
   9 REP 1
   FIN STAC")

;;; -------------------------------------------------------

;; Create the output "BA" by peeking all items of the Numerical Stack
;; twice.
(interpret-ASCIIORb
  "INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INC STAK
   INC STAK
   INC STAK
   INC STAK
   INC STAK
   STAK PEKAL STAC
   INC STAK
   STAK PEKAL STAC
   FIN STAC")

;;; -------------------------------------------------------

;; Create the output "ABA" by peeking all items of the Numerical Stack
;; thrice, utilizing the "DROP" command in the desinent step.
(interpret-ASCIIORb
  "INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INQ STAK
   INC STAK
   INC STAK
   INC STAK
   INC STAK
   INC STAK
   STAK PEKAL STAC
   INC STAK
   STAK PEKAL STAC
   DROP STAK
   STAK PEKAL STAC
   FIN STAC")

;;; -------------------------------------------------------

;; Generate an ASCIIORb program capable of reproducing the text
;; "Hello, World!" and print it to the standard output:
(generate-text-program "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Generate an ASCIIORb program capable of reproducing the text
;; "Hello, World!" and execute it using the interpreter.
(interpret-ASCIIORb
  (generate-text-program "Hello, World!"))
