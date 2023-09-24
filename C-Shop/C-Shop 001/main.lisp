;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "C-Shop", invented by the Esolang user "None1" and presented
;; on July 15th, 2023, the foundry of which constitutes the simulation
;; of a shopping activity as a medium to express basic arithmetics as
;; well as input and output facilities.
;; 
;; 
;; Concept
;; =======
;; The C-Shop programming language's kenspeckle attribute wones in its
;; simulation of commerce inside of a shop, the products of which are
;; subsumed into a twofold species, the "big C" and the "small c",
;; distinguished not only by their identification, but also by their
;; integer price, the same is permitted to change either to a literal or
;; a user input derived value.
;; 
;; == PROGRAMS SIMULATE A SHOPPING BOUT ==
;; Programs in the C-Shop programming language limn the picture of a
;; trade in two produces, the "big C" and the "small c", the prices of
;; which obey an amenability to modulations at any instant, being
;; desumed from the integer space.
;; 
;; A tally of zero or more clients might purchase a non-negative amount
;; of this twissel, accruing the respective bill for themselves.
;; 
;; The occasion of payment by a person resolves to the bill value's
;; transliteration into the character whose ASCII code answers to the
;; numeric value.
;; 
;; The enterprise's operations may be enumerated following an abstract
;; conspectuity by an established sequence of actions:
;; 
;;   (1) Welcome the client.
;;   (2) Set the price of a big C.
;;   (3) Set the price of a small c.
;;   (4) Change the price of a big C.
;;   (5) Change the price of a small c.
;;   (6) Buy a certain quantity of big C's and small c's.
;;   (7) Pay for the purchased big C's and small c's.
;; 
;; Please note that, in our concrete model, the inchoate treble assumes
;; a mandatory role, required exactly once and in this specific
;; arrangement, however, permitting the steps (2) and (3) on other
;; occasions as well, while the stages (4) through (7) are subject to a
;; very liberal homologation, the same permits any tally and ordonnance
;; to govern their usance.
;; 
;; == CLIENTS REPRESENT VARIABLES ==
;; All data castaldy proceeds by means of clients, each agnominated as a
;; "person" and uniquely identified by a positive integer, the purchases
;; of which, maintained in their "bill", determines the variable
;; name-value twissels.
;; 
;; In a more abstract diction, as counterdistinguished from C-Shop's
;; ludibund illustrations, a variable identifier answers to an integer
;; value greater than zero, while the affiliated value does not impose
;; any peisant constrictions beside it siclike conformance to the
;; integer species, admitting thus positive as well as negative amounts.
;; 
;; While a client's purchase translates into a variable state's
;; incrementation, the actual payment action, accompassed by a dedicated
;; instruction, issues the output of the character whose ASCII code
;; responds to the bill's vallidom.
;; 
;; 
;; Architecture
;; ============
;; The C-Shop program memory is realized as a registry of variables,
;; amenable by a positive integer name, and endowed with a scalar
;; integer's capacity, the latter might assume any sign and magnitude.
;; 
;; The requisitums' disrespondency to a specific arrangement's
;; regulations redes an efficient unsorted solution, such as a hash
;; table, to engage in this wike.
;; 
;; 
;; Data Types
;; ==========
;; C-Shop's type system bifurcates into a twain of components: imprimis,
;; and more peisant, the signed integer species of unbounded magnitude;
;; further, in a parhedral perspective, the ASCII character repertoire.
;; 
;; == INTEGERS: NATIVES OF THE MEMORY ==
;; Unbounded integers of both permissible signs constitute the program
;; memory's residents, whence issues their paravaunt echolon.
;; 
;; == CHARACTERS: CURRENCY OF THE INTERFACE ==
;; The paravail significance allotted to characters, restricted to the
;; ASCII repertoire, locates them at the junction of the input/output
;; interface, responsible for the reception of character input for
;; transliteration into the corresponding ASCII code and storage in the
;; program memory, as well as the athwart airt, which converts a memory
;; cell's integral datum into the affiliated textual entity.
;; 
;; 
;; Syntax
;; ======
;; C-Shop, in its syntactical apercu, subscribes to a mimicry of natural
;; English language, the single instruction of which, each woning on a
;; line of its own, expressing a sentential coherence in order to
;; communicate the ultimate purpose.
;; 
;; == INSTRUCTIONS ==
;; A C-Shop program is delineated as a sequence of zero or more
;; instructions, preceded by a preamble that comprehends a welcome
;; addressing and the initial price configurations, each such component
;; a commorant of its private line.
;; 
;; Every instruction mimics a sentence invested with validity according
;; to the donat of the English tongue, the words of which,
;; dinstinguished into mainly alphabet keyword and integer arguments,
;; are distinguished by one or more spaces' adminiculum.
;; 
;; == SPACES ==
;; Spaces and tabs, required for each token twain's demarcation,
;; experience a liberal homologation in distribution across the words'
;; intermedes, but not inside of such composites themselves.
;; 
;; == NEWLINES ==
;; Each two instruction lines ought to be segregated by one or more
;; newline sepiments. Blank lines as optional participants are
;; tolerated.
;; 
;; == COMMENTS ==
;; No provision for comments exists in the current language iteration.
;; 
;; == GRAMMAR ==
;; A formal description of the language's donat in the Extended
;; Backus-Naur Form (EBNF) shall be adduced below:
;; 
;;   program         := emptyLines
;;                   ,  welcome
;;                   ,  newlines , setBigCPrice
;;                   ,  newlines , setSmallCPrice
;;                   ,  commandList
;;                   ,  emptyLines
;;                   ;
;;   commandList     := { newlines , [ command ] } ;
;;   command         := setBigCPrice
;;                   |  setSmallCPrice
;;                   |  askBigCPrice
;;                   |  askSmallCPrice
;;                   |  buyCommand
;;                   |  payCommand
;;                   ;
;;   welcome         := "Welcome to C-Shop" ;
;;   setBigCPrice    := "The big C costs $"   , signedInteger ;
;;   setSmallCPrice  := "The small c costs $" , signedInteger ;
;;   askBigCPrice    := "Ask price of the big C" ;
;;   askSmallCPrice  := "Ask price of the small c" ;
;;   buyCommand      := "Person "
;;                   ,  unsignedInteger
;;                   ,  " buys "
;;                   ,  unsignedInteger
;;                   ,  " big C's and "
;;                   ,  unsignedInteger
;;                   ,  " small c's"
;;                   ;
;;   payCommand      := "Person " ,
;;                   ,  unsignedInteger
;;                   ,  " will pay for his order"
;;                   ;
;;   
;;   emptyLines      := { newline } ;
;;   newlines        := newline , { newline } ;
;;   newline         := "\n" ;
;;   signedInteger   := [ "+" | "-" ] , unsignedInteger ,
;;   unsignedInteger := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;; 
;; 
;; Instructions
;; ============
;; C-Shop's instructional roster amplects a cardinality of septuple
;; extent, whose perimeter tallies a program start sentinel, price
;; setting, basic arithmetics, as well as input and output facilities.
;; 
;; == OVERVIEW ==
;; A cursory presentation's adminiculum shall administer the substratum
;; for C-Shop's capactities' recognition, ere further elaborations will
;; enrich this acquaintance:
;; 
;;   ------------------------------------------------------------------
;;   Command                                       | Effect
;;   ----------------------------------------------+-------------------
;;   Welcome to C-Shop                             | Start program
;;   ..................................................................
;;   The big C costs $NBC                          | Set big C price
;;                    ***                          | 
;;   ..................................................................
;;   The small c costs $NSC                        | Set small c price
;;                      ***                        | 
;;   ..................................................................
;;   Ask price of the big C                        | Input big C price
;;   ..................................................................
;;   Ask price of the small c                      | Input small c
;;                                                 | price
;;   ..................................................................
;;   Person PID buys NBC big C's and NSC small c's | Increment bill
;;          ***      ***             ***           | 
;;   ..................................................................
;;   Person PID will pay for his order             | Output bill
;;          ***                                    | 
;;   ------------------------------------------------------------------
;; 
;; == INSTRUCTION DETAILS ==
;; An augmented gnarity regarding the operations will now be furnished:
;; 
;;   +====================================+
;;   |== START PROGRAM (WELCOME CLIENT) ==|
;;   +====================================+============================
;;   | Command | Welcome to C-Shop
;;   |---------+-------------------------------------------------------
;;   | Effect  | Starts the program.
;;   |         |-------------------------------------------------------
;;   |         | This must be the first non-blank line in the code.
;;   +=================================================================
;;   
;;   +===============================================+
;;   |== SET BIG C PRICE (SET PRICE FOR ONE BIG C) ==|
;;   +===============================================+=================
;;   | Command | The big C costs $NBC
;;   |         |                  ***
;;   |---------+-------------------------------------------------------
;;   | Effect  | Sets the price for one unit of the big C to {NBC}.
;;   |         |-------------------------------------------------------
;;   |         | This must be the second non-blank line in the code.
;;   |         |-------------------------------------------------------
;;   |         | {NBC} must be an integer.
;;   +=================================================================
;;   
;;   +===================================================+
;;   |== SET SMALL C PRICE (SET PRICE FOR ONE SMALL C) ==|
;;   +===================================================+=============
;;   | Command | The small c costs $NSC
;;   |         |                    ***
;;   |---------+-------------------------------------------------------
;;   | Effect  | Sets the price for one unit of the small c to {NSC}.
;;   |         |-------------------------------------------------------
;;   |         | This must be the third non-blank line in the code.
;;   |         |-------------------------------------------------------
;;   |         | {NSC} must be an integer.
;;   +=================================================================
;;   
;;   +=======================================================+
;;   |== QUERY FOR BIG C INPUT (CHANGE PRICE OF ONE BIG C) ==|
;;   +=======================================================+=========
;;   | Command | Ask price of the big C
;;   |---------+-------------------------------------------------------
;;   | Effect  | Queries the standard input for an ASCII character and
;;   |         | sets the price of one unit of the big C to its
;;   |         | character code.
;;   +=================================================================
;;   
;;   +===========================================================+
;;   |== QUERY FOR SMALL C INPUT (CHANGE PRICE OF ONE SMALL C) ==|
;;   +===========================================================+=====
;;   | Command | Ask price of the small c
;;   |---------+-------------------------------------------------------
;;   | Effect  | Queries the standard input for an ASCII character and
;;   |         | sets the price of one unit of the small c to its
;;   |         | character code.
;;   +=================================================================
;;   
;;   +=====================================+
;;   |== INCREMENT (ADD TO BILL, OR BUY) ==|
;;   +=====================================+===========================
;;   | Command | Person PID buys NBC big C's and NSC small c's
;;   |         |        ***      ***             ***
;;   |---------+-------------------------------------------------------
;;   | Effect  | Adds to the bill of the person identified by {PID}
;;   |         | the sum of {NBC} times the currently defined cost of
;;   |         | one unit of the big C and {NSC} times the current
;;   |         | price of one unit of the small c, that is:
;;   |         |   bills[PID] <- bills[PID]
;;   |         |                 + NBC * current_price_per_big_C
;;   |         |                 + NSC * current_price_per_small_c
;;   |         |-------------------------------------------------------
;;   |         | {PID} must be a positive integer.
;;   |         |-------------------------------------------------------
;;   |         | {NBC} must be a non-negative integer.
;;   |         |-------------------------------------------------------
;;   |         | {NSC} must be a non-negative integer.
;;   +=================================================================
;;   
;;   +=================================+
;;   |== OUTPUT (PRINT BILL AND PAY) ==|
;;   +=================================+===============================
;;   | Command | Person PID will pay for his order
;;   |         |        ***
;;   |---------+-------------------------------------------------------
;;   | Effect  | Prints to the standard output the character whose
;;   |         | ASCII code corresponds to the value of the bill of the
;;   |         | person identified by {PID}, which means:
;;   |         |   let asciiCode        <- bills[PID]
;;   |         |   let characterForCode <- character for asciiCode
;;   |         |   print characterForCode
;;   |         |-------------------------------------------------------
;;   |         | {PID} must be a positive integer.
;;   +=================================================================
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The C-Shop language's simple nature and the lucid presentation of its
;; protolog afford a savetive against the preponderance among those
;; severe ambiguities potent to haunt it.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes from tokens.
;; 
;; == PARSERS AND COMBINATORS ARE FUNCTIONS ==
;; In eath diction, the parser combinator approach constructs a complete
;; parser entity from a sequence of interoperating smaller parsers,
;; their coefficiency enabled through combinators.
;; 
;; Both parsers and combinators are, in their pristine diorism,
;; represented by functions, accepting a source to parse and returning
;; in the case of a successful application a composition apprehending at
;; least
;; 
;;   - The remaining portion of the source, curtailed by the consumed
;;     items.
;;     If, for instance, the source represents a string, the first
;;     characters matching the parsing predicate will be removed; for
;;     tokens in lieu of this direct input, the residue following the
;;     accepted token objects are delivered.
;;   - An object designating the parser's or combinator's contribution
;;     to the encompassing whole, that is, usually an AST node.
;; 
;; A failure in the parser's or combinator's operations usually
;; concludes either with a communicative flag or an error signaling.
;; 
;; Conforming to an augmentation in formality, the following signature
;; may be proffered for parsers and combinators:
;; 
;;   function (source : any) -> (newSource : any, output : any)
;; 
;; == PARSERS AND COMBINATORS ARE INTERWOVEN IN SERIES ==
;; Considering the successful case, the modified parser or combinator
;; source is utilized as an input to the subsequent parser/combinator,
;; chaining these into a series of processors that, in concluding in an
;; ultimately empty source, build the output structure, for instance,
;; the abstract syntax tree.
;; 
;; == PARSERS EQUAL COMBINATORS ==
;; The discrepancy betwixt parsers and combinators constitutes a rather
;; puisne question of terminology for most objectives, as both partake
;; of a functional commonality. Parsers are usually "stand-alone"
;; components, responsible for the actual modification of the source,
;; whereas combinators ligate zero or more parsers, or other
;; combinators, in order to accompass a result.
;; 
;; If we have, as an example, a parser "characterOf", defined as
;; 
;;   function characterOf (expectedCharacter : character)
;;     let characterParser <- function (source : string)
;;       if source[0] = expectedCharacter then
;;         return (source.substring (1, source.length),
;;                 makeNode(NODE_TYPE_CHARACTER, source[0])
;;       else
;;         return null
;;       end if
;;     end function
;;     
;;     return characterParser
;;   end function
;; 
;; the requisitum involved in parsing more than one character coerces us
;; to discover a chaining of mandatorily matching "characterOf"
;; invocations. To this end, we define the following combinator:
;; 
;;   function allMatch (parsers : parserFunction[0..*])
;;     let allCombinator <- function (source : string)
;;       let newSource <- source
;;       let nodes     <- empty node list
;;       for every parser currentParser in parsers do
;;         let parserResult <- currentParser(source)
;;         
;;         if parserResult is null then
;;           return null
;;         else
;;           newSource <- parserResult[0]
;;           append parserResult[1] to nodes
;;         end if
;;       end for
;;       
;;       return (newSource, nodes)
;;     end function
;;     
;;     return allCombinator
;;   end function
;; 
;; An exemplary invocation of the combinator "allMatch" with several
;; instances of the "characterOf" parser could involve:
;; 
;;   parse (allMatch (characterOf ('h'),
;;                    characterOf ('e'),
;;                    characterOf ('l'),
;;                    characterOf ('l'),
;;                    characterOf ('o')),
;;          "hello")
;; 
;; == A PARSER COMBINATOR IN AN OBJECT-ORIENTED CONTEXT ==
;; The principal and onomastic substrate derives from Jeffrey Massung's
;; "parse" package for Common Lisp, which please see under
;; [massung2020parse]. A diverging aspect is apportioned its commorancy
;; in the object-oriented variation, substituting the functional notions
;; in order to emphasize the coefficacy partaken of by the several
;; components.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-15
;; 
;; Sources:
;;   [devanla2021minimalparsecomb]
;;   Guru Devanla, "Minimal Parser Combinator in Python",
;;                 26th October 2021
;;   URL: "https://gdevanla.github.io/posts/
;;         write-a-parser-combinator-in-python.html"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in Python.
;;   
;;   [elouafi2018gentleintroparscomb]
;;   Yassine Elouafi, "A gentle introduction to parser combinators",
;;                    2018
;;   URL: "https://dev.to/yelouafi/
;;         a-gentle-introduction-to-parser-combinators-21a0"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [elouafi2021introparsercomb]
;;   Yassine Elouafi, "introduction-to-parser-combinators.md",
;;                    June 28, 2021 
;;   URL: "https://gist.github.com/yelouafi/
;;         556e5159e869952335e01f6b473c4ec1"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [esolang2023C-Shop]
;;   The Esolang contributors, "C-Shop", July 19th, 2023
;;   URL: "https://esolangs.org/wiki/C-Shop"
;;   
;;   [goodrich214datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;;   
;;   [massung2020parse]
;;   Jeffrey Massung, "The PARSE Package", 2020
;;   URL: "https://github.com/massung/parse"
;;   Notes:
;;     - GitHub repository of the "parse" package, a Common Lisp library
;;       for token parsing which employs parser combinators.
;;   
;;   [mulligan2023unlocking]
;;   Rory Mulligan, "Unlocking the Power of Parser Combinators: A
;;                   Beginner's Guide", February 9, 2023
;;   URL: "https://www.sitepen.com/blog/
;;         unlocking-the-power-of-parser-combinators-a-beginners-guide"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which lays its amplectation across functions such as
   ``format'' and ``write-char'', to name select forbisens."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype token-queue ()
  "The ``token-queue-type'' defines a first-in first-out (FIFO)
   structure for tokens as an adjustable vector of ``Token'' instances
   with no restriction imposed upon its cardinality."
  '(vector Token *))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype product ()
  "The ``product'' type enumerates the available products, that is,
   variants of \"C\" (or \"c\")."
  '(member :big-C :small-c))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number greater than
   zero, but unbounded towards its upper bourne, that is, a commorant of
   the integral range [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero, but unbounded towards its upper bourne, that
   is, a commorant of the integral range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype c-shop-program ()
  "The ``c-shop-program'' type defines an executable C-Shop program as a
   list composed of zero or more ``Instruction'' members."
  '(list-of Instruction))

;;; -------------------------------------------------------

(deftype c-shop-program-state ()
  "The ``c-shop-program-state'' type enumerates the recognized states
   along which a C-Shop program traverses."
  '(member
    :expect-welcome
    :expect-big-C-price
    :expect-small-c-price
    :standard))

;;; -------------------------------------------------------

(deftype c-shop-program-states ()
  "The ``c-shop-program-states'' type defines an ordered sequence of
   program states which dictate the progress of a C-Shop program."
  '(list-of c-shop-program-state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Boolean operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value (object)
  "Returns a ``boolean'' tantamount for the OBJECT, delivering for a
   non-``NIL'' input the sentinel ``T'', otherwise ``NIL''."
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      (error "Missing token type.")
    :reader        token-type
    :type          keyword
    :documentation "The species appertaining to the token.")
   (value
    :initarg       :value
    :initform      (error "Missing token value.")
    :reader        token-value
    :type          T
    :documentation "The datum carried by the token."))
  (:documentation
    "The ``Token'' class is apportioned the wike of a significant
     object's representation, the same originates from the lexical
     analyzation process' investments."))

;;; -------------------------------------------------------

(defun make-token (type value)
  "Creates and returns a new ``Token'' categorized by its TYPE and
   further detailed by the VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the Token
    (make-instance 'Token :type type :value value)))

;;; -------------------------------------------------------

(defun token-type-p (candidate expected-type)
  "Determines whether the CANDIDATE token conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (get-boolean-value
      (eq (token-type candidate) expected-type))))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) stream)
  (declare (type Token       token))
  (declare (type destination stream))
  (format stream "(Token ~s ~s)"
    (token-type  token)
    (token-value token)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character probing operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a space or horizontal
   tab character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (member candidate '(#\Space #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun word-boundary-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a word boundary, that
   is, either the ``NIL'' value, representing the end of the respective
   source, or a whitespace, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type (or null character) candidate))
  (the boolean
    (or (null candidate)
        (whitespace-character-p candidate))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign, that
   is, either a plus (\"+\") or minus (\"-\"), returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (get-boolean-value
      (find candidate "+-" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :reader        lexer-source
    :type          string
    :documentation "The piece of C-Shop source code to analyze.")
   (position
    :initform      0
    :accessor      lexer-position
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :accessor      lexer-character
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class applies itself to the onus of evaluating a
     piece of C-Shop source code in order to extract its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p
            (lexer-source   lexer)
            (lexer-position lexer))
      (char (lexer-source   lexer)
        (lexer-position lexer))))
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which analyzes the SOURCE."
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Returns the LEXER's current character, while concomitantly advanceing
   the LEXER's position cursor to the next character in its source, if
   possible."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (lexer-character lexer)
      (setf (lexer-character lexer)
        (when (array-in-bounds-p
                (lexer-source lexer)
                (1+ (lexer-position lexer)))
          (char (lexer-source lexer)
            (incf (lexer-position lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-raw-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single word, demarcated by spaces or the source's exhaustion, and
   returns a string representation thereof."
  (declare (type Lexer lexer))
  (with-output-to-string (word)
    (declare (type string-stream word))
    (loop
      until (word-boundary-character-p
              (lexer-character lexer))
      do    (write-char (lexer-advance lexer) word))))

;;; -------------------------------------------------------

(defun lexer-read-raw-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single integer number, demarcated by spaces or the source's
   exhaustion, and returns an integer object representation thereof."
  (declare (type Lexer lexer))
  (the (integer 0 *)
    (parse-integer
      (lexer-read-raw-word lexer))))

;;; -------------------------------------------------------

(defun lexer-read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single word, demarcated by spaces or the source's exhaustion, and
   returns a ``:word'' token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :word
      (lexer-read-raw-word lexer))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single integer number, demarcated by spaces or the source's
   exhaustion, and returns a ``:number'' token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (lexer-read-raw-number lexer))))

;;; -------------------------------------------------------

(defun lexer-read-price (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   priced number, introduced via dollar sign (\"$\"), demarcated by
   spaces or the source's exhaustion, and returns a ``:price'' token
   representation thereof."
  (declare (type Lexer lexer))
  ;; Skip introducing dollar sign ("$").
  (lexer-advance lexer)
  (the Token
    (make-token :price
      (lexer-read-raw-number lexer))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent spaces and returns no value."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character   lexer)
               (space-character-p (lexer-character lexer)))
    do (lexer-advance lexer))
  (values))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to every request
   with a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      (cond
        ;; End of file.
        ((null character)
          (make-token :eof NIL))
        
        ;; Skipped spaces or tabs.
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ;; Newline.
        ((char= character #\Newline)
          (make-token :newline
            (lexer-advance lexer)))
        
        ;; Price.
        ((char= character #\$)
          (lexer-read-price lexer))
        
        ;; Unsigned integer.
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ;; Signed integer.
        ((sign-character-p character)
          (lexer-read-number lexer))
        
        ;; Any other word.
        (T
          (lexer-read-word lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse state.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parse-State ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :reader        parse-state-lexer
    :type          Lexer
    :documentation "The lexer assigned with the responsibiilty of the
                    token provision, the same are gradually introduced
                    into the TOKENS queue.")
   (tokens
    :initarg       :tokens
    :initform      (error "Missing token queue.")
    :reader        parse-state-tokens
    :type          token-queue
    :documentation "The token queue whose elements shall be parsed.")
   (cursor
    :initarg       :cursor
    :initform      0
    :reader        parse-state-cursor
    :type          fixnum
    :documentation "The position into the TOKENS queue designating the
                    token processed by this parse state."))
  (:documentation
    "The ``Parse-State'' class serves in the encapsulation of all
     information requisite for a parser's attempted application,
     maintaining the shared token queue as a compernage to its currently
     probed token's position, or \"cursor\", into the same."))

;;; -------------------------------------------------------

(defun make-initial-parse-state (lexer)
  "Creates and returns a new ``Parse-State'' whose source is realized in
   the queue of TOKENS."
  (declare (type lexer))
  (the Parse-State
    (make-instance 'Parse-State
      :lexer  lexer
      :tokens (make-array 0
                :element-type    'Token
                :initial-element (make-token :eof NIL)
                :adjustable      T
                :fill-pointer    0)
      :cursor 0)))

;;; -------------------------------------------------------

(defun parse-state-advance (template)
  "Creates and returns a new ``Parse-State'' whose source is shared with
   the TEMPLATE state, while the new cursor points to the next location
   in the same."
  (declare (type Parse-State template))
  (the Parse-State
    (make-instance 'Parse-State
      :lexer  (parse-state-lexer  template)
      :tokens (parse-state-tokens template)
      :cursor (1+ (parse-state-cursor template)))))

;;; -------------------------------------------------------

(defun parse-state-element (state)
  "Returns the current token in the parse STATE's source, referenced
   by its cursor."
  (declare (type Parse-State state))
  ;; Ensure that the STATE's position cursor points to a token in its
  ;; token vector by repeatedly querying the next token from its lexer
  ;; and appending the same to the token vector until its sufficiency in
  ;; size.
  (loop
    while
      (>= (parse-state-cursor state)
          (fill-pointer (parse-state-tokens state)))
    do
      (vector-push-extend
        (lexer-get-next-token
          (parse-state-lexer state))
        (parse-state-tokens state)))
  (the Token
    (aref (parse-state-tokens state)
      (parse-state-cursor state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse result.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parse-Result ()
  ((succeeded-p
    :initarg       :succeeded-p
    :initform      (error "Missing success flag.")
    :reader        parse-result-succeeded-p
    :type          boolean
    :documentation "A flag which determines whether the attempted
                    ``Parser'' has succeeded.")
   (state
    :initarg       :state
    :initform      (error "Missing parse state.")
    :reader        parse-result-state
    :type          Parse-State
    :documentation "Encapsulates the advancement accompassed by the
                    executing ``Parser'' with respect to the consumed
                    C-Shop source code.")
   (output
    :initarg       :output
    :initform      (error "Missing output.")
    :reader        parse-result-output
    :type          T
    :documentation "The contribution of the executing ``Parser'' to the
                    C-Shop program pursuing an assemblage."))
  (:documentation
    "The ``Parse-Result'' class serves to envelope a ``Parser'''s
     response to the behest involving its application on a
     ``Parse-State'', amplecting in its nucleus three pieces of
     information:
       (1) A Boolean success or failure flag.
       (2) The ``Parse-State'' produced by this parsing attempt, which
           might either equal the input or constitute an advancement.
       (3) The responsible ``Parser'''s actual output, that is, its
           contribution to the C-Shop program's parsed form; the same
           might be any value, in particular ``NIL'', upon a failed
           endeavor."))

;;; -------------------------------------------------------

(defun make-parse-result (succeeded-p state output)
  "Creates and returns a new ``Parse-Result'', delineated by a success
   flag SUCCEEDED-P, the parse STATE defining its advancement, and an
   OUTPUT that represents the responsible ``Parser'''s contribution to
   the complete parsing output."
  (declare (type boolean     succeeded-p))
  (declare (type Parse-State state))
  (declare (type T           output))
  (the Parse-Result
    (make-instance 'Parse-Result
      :succeeded-p succeeded-p
      :state       state
      :output      output)))

;;; -------------------------------------------------------

(defmethod print-object ((result Parse-Result) stream)
  (declare (type Parse-Result result))
  (declare (type destination  stream))
  (format stream "(Parse-Result succeeded-p=~a state=~a output=~a)"
    (parse-result-succeeded-p result)
    (parse-result-state       result)
    (parse-result-output      result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Instruction ()
  ()
  (:documentation
    "The ``Instruction'' interface accoutres the substratum for all
     classes appropriated to the task of modeling C-Shop
     instructions."))

;;; -------------------------------------------------------

(defclass Welcome-Instruction (Instruction)
  ()
  (:documentation
    "The ``Welcome-Instruction'' class presents an instruction mandated
     to commence a C-Shop program."))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Welcome-Instruction) stream)
  (declare (type Welcome-Instruction instruction))
  (declare (ignore                   instruction))
  (declare (type destination         stream))
  (format stream "(Welcome-Instruction)"))

;;; -------------------------------------------------------

(defclass Set-Price-Instruction (Instruction)
  ((product
    :initarg       :product
    :initform      (error "Missing product.")
    :reader        set-price-instruction-product
    :type          product
    :documentation "The product whose PRICE shall be specified.")
   (price
    :initarg       :price
    :initform      (error "Missing price.")
    :reader        set-price-instruction-price
    :type          integer
    :documentation "The price per one unit of the PRODUCT."))
  (:documentation
    "The ``Set-Price-Instruction'' class represents an instruction
     accommodated for the specification of a big C or small c's
     costage."))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Set-Price-Instruction) stream)
  (declare (type Set-Price-Instruction instruction))
  (declare (type destination           stream))
  (format stream "(Set-Price-Instruction product=~d price=~d)"
    (slot-value instruction 'product)
    (slot-value instruction 'price)))

;;; -------------------------------------------------------

(defclass Ask-Price-Instruction (Instruction)
  ((product
    :initarg       :product
    :initform      (error "Missing product.")
    :reader        ask-price-instruction-product
    :type          product
    :documentation "The product whose new price shall be queried."))
  (:documentation
    "The ``Ask-Price-Instruction'' class furnishes an instruction whose
     competence entails the standard input conduit's querying for the
     price of one unit of a product."))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Ask-Price-Instruction) stream)
  (declare (type Ask-Price-Instruction instruction))
  (declare (type destination           stream))
  (format stream "(Ask-Price-Instruction price=~d)"
    (slot-value instruction 'product)))

;;; -------------------------------------------------------

(defclass Pay-Bill-Instruction (Instruction)
  ((person-id
    :initarg       :person-id
    :initform      (error "Missing person identification.")
    :reader        pay-bill-instruction-person-id
    :type          positive-integer
    :documentation "The identification of the person whose bill shall be
                    displayed on the standard output."))
  (:documentation
    "The ``Pay-Bill-Instruction'' class relates to the wike of producing
     a print of a person's bill on the standard output conduit."))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Pay-Bill-Instruction) stream)
  (declare (type Pay-Bill-Instruction instruction))
  (declare (type destination          stream))
  (format stream "(Pay-Bill-Instruction person-id=~d)"
    (slot-value instruction 'person-id)))

;;; -------------------------------------------------------

(defclass Purchase-Instruction (Instruction)
  ((person-id
    :initarg       :person-id
    :initform      (error "Missing person identification.")
    :reader        purchase-instruction-person-id
    :type          positive-integer
    :documentation "The identification of the person responsible for
                    this purchase.")
   (amount-of-big-Cs
    :initarg       :amount-of-big-Cs
    :initform      (error "Missing amount of big C's.")
    :reader        purchase-instruction-amount-of-big-Cs
    :type          non-negative-integer
    :documentation "The quantity of big C's purchased by the person.")
   (amount-of-small-cs
    :initarg       :amount-of-small-cs
    :initform      (error "Missing amount of small c's.")
    :reader        purchase-instruction-amount-of-small-cs
    :type          non-negative-integer
    :documentation "The quantity of small c's purchased by the
                    person."))
  (:documentation
    "The ``Purchase-Instruction'' class reenacts the purchase of a
     specified number of products by a person."))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Purchase-Instruction) stream)
  (declare (type Purchase-Instruction instruction))
  (declare (type destination          stream))
  (format stream "(Purchase-Instruction person-id=~d ~
                                        amount-of-big-Cs=~d ~
                                        amount-of-small-cs=~d)"
    (slot-value instruction 'person-id)
    (slot-value instruction 'amount-of-big-Cs)
    (slot-value instruction 'amount-of-small-cs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer probing operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-positive-integer (number)
  "Determines whether the NUMBER represents a positive integer,
   returning on confirmation the verbatim input, otherwise signaling an
   error of an unspecified type."
  (declare (type integer number))
  (the positive-integer
    (if (plusp number)
      number
      (error "The value ~d does not represent a positive integer."
        number))))

;;; -------------------------------------------------------

(defun ensure-non-negative-integer (number)
  "Determines whether the NUMBER represents a non-negative integer,
   returning on confirmation the verbatim input, otherwise signaling an
   error of an unspecified type."
  (the non-negative-integer
    (if (minusp number)
      (error "The value ~d does not represent a non-negative integer."
        number)
      number)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((processor
    :initarg       :processor
    :initform      (error "Missing processor.")
    :reader        parser-processor
    :type          (function (Parse-State) Parse-Result)
    :documentation "The function responsible for the actual parsing, the
                    same entails an input ``Parse-State'''s evaluation
                    in order to produce a ``Parse-Result''."))
  (:documentation
    "The ``Parser'' class furnishes an entity responsible for parsing
     a ``Parse-State'''s element in order to respond with a
     ``Parse-Result'' which comprehends its success' assessment, the
     induced or a new ``Parse-State'', and a contingent output that may
     contribute to the assembled C-Shop program."))

;;; -------------------------------------------------------

(defun make-parser (processor)
  "Creates and returns a new ``Parser'' whose operations are conducted
   by the PROCESSOR function."
  (declare (type (function (Parse-State) Parse-Result) processor))
  (the Parser
    (make-instance 'Parser :processor processor)))

;;; -------------------------------------------------------

(defun parser-parse (parser state)
  "Applies the PARSER with the parse STATE and returns a
   ``Parse-Result'' apprizing about the attempt's success or failure."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall
      (parser-processor parser)
      state)))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Creates and returns a new ``Parser'' in a convenient way by defining
   an anonymous processor function, the same accepts as its sole input
   the ``Parse-State'', nevened by the STATE-VARIABLE, and utilizes the
   BODY forms as its implementation, returning the desinent form'
   results, which are expected to produce as the primary value a
   ``Parse-Result'' instance."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun probe-token (predicate)
  "Returns a new ``Parser'' which succeeds if the PREDICATE, applied to
   the input parse state's token, returns a non-``NIL'' response, on
   confirmation returning in its parse result the probed token."
  (declare (type (function (Token) *) predicate))
  (the Parser
    (build-parser (state)
      (let ((probed-token (parse-state-element state)))
        (declare (type Token probed-token))
        (the Parse-Result
          (if (funcall predicate probed-token)
            (make-parse-result T
              (parse-state-advance state)
              probed-token)
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if all of the input PARSERS,
   in their exact order of specification, match, on confirmation
   returning the desinent parser result's output."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for parser
            of-type Parser
            in      parsers
          
          for result
            of-type Parse-Result
            =       (parser-parse parser new-state)
          
          ;; Returning a failed parse result with the input STATE, in
          ;; lieu of the current RESULT, constitutes a very important
          ;; fact.
          unless (parse-result-succeeded-p result) do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return
              (make-parse-result T new-state
                (parse-result-output result))))))))

;;; -------------------------------------------------------

(defun sequence-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if all of its input PARSERS,
   in their exact order of specification, match, on confirmation
   returning in its parse result a list of the collected outputs
   according to the encountered ordonnance."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for parser
            of-type Parser
            in      parsers
          
          for result
            of-type Parse-Result
            =       (parser-parse parser new-state)
          
          if (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          ;; Returning a failed parse result with the input STATE, in
          ;; lieu of the current RESULT, constitutes a very important
          ;; fact.
          else do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun any-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if any of its input PARSERS,
   probed in the exact order of specification, match, on confirmation
   returning the first successful parser's result.
   ---
   If a parser matches, the endeavor of docimacy immediately ceases, and
   no further contender is scrutinized."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for parser
            of-type Parser
            in      parsers
          
          for result
            of-type Parse-Result
            =       (parser-parse parser new-state)
          
          when (parse-result-succeeded-p result) do
            (return result)
          
          finally
            (return
              (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun many-of (parser)
  "Returns a new ``Parser'' which always succeeds, matching the input
   PARSER zero or more times, and returning in its parse result's output
   the collected PARSER outputs in their correct order."
  (declare (type Parser parser))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for result
            of-type Parse-Result
            =       (parser-parse parser new-state)
          
          while (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun return-value (output)
  "Returns a new ``Parser'' which always succeeds, returning in its
   parse result's output the OUTPUT datum."
  (declare (type T output))
  (the Parser
    (build-parser (state)
      (make-parse-result T state output))))

;;; -------------------------------------------------------

(defun bind (antecedent consequent-generator)
  "Returns a new ``Parser'' which furnishes a monadic bind, and which
   succeeds if the following to stages are satisfied:
     (1) The ANTECEDENT parser matches.
     (2) The output of the matching ANTECEDENT parser's result, when
         delivered to the CONSEQUENT-GENERATOR, generates a new
         ``Parser'', the \"consequent parser\", which itself matches the
         ANTECEDENT result's state, returning on confirmation the
         consequent parser's output in the ensconcing ``bind'' parser."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) consequent-generator))
  (the Parser
    (build-parser (state)
      (let ((antecedent-result (parser-parse antecedent state)))
        (declare (type Parse-Result antecedent-result))
        (the Parse-Result
          (if (parse-result-succeeded-p antecedent-result)
            (let ((consequent-parser
                    (funcall consequent-generator
                      (parse-result-output antecedent-result))))
              (declare (type Parser consequent-parser))
              (parser-parse consequent-parser
                (parse-result-state antecedent-result)))
            antecedent-result))))))

;;; -------------------------------------------------------

(defmacro bindlet ((antecedent-output-variable antecedent) &body body)
  "Returns a new ``Parser'' which succeeds if the ANTECEDENT parser
   matches, binding its parse result's output to the
   ANTECEDENT-OUTPUT-VARIABLE, executing the BODY forms, while expecting
   the desinent form to produce a parser."
  `(the Parser
     (bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun word-of (expected-word)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token conforms to the ``:word'' type and concomitantly the token's
   value equals the EXPECTED-WORD, on confirmation comprehending in its
   parse result the probed token."
  (declare (type string expected-word))
  (the Parser
    (probe-token
      #'(lambda (probed-token)
          (declare (type Token probed-token))
          (and
            (token-type-p probed-token :word)
            (string=
              (token-value probed-token)
              expected-word))))))

;;; -------------------------------------------------------

(defun phrase-of (&rest words)
  "Returns a ``Parser'' which succeeds if all WORDS, in the specified
   order, are encountered in immediate succession, on confirmation
   returning in its parse result the matching tokens according to the
   given ordonnance."
  (declare (type (list-of string) words))
  (the Parser
    (apply #'sequence-of
      (mapcar #'word-of words))))

;;; -------------------------------------------------------

(defun of-type (expected-token-type)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token conforms to the EXPECTED-TOKEN-TYPE, on confirmation returning
   in its parse result the probed token."
  (declare (type keyword expected-token-type))
  (the Parser
    (probe-token
      #'(lambda (probed-token)
          (declare (type Token probed-token))
          (token-type-p probed-token expected-token-type)))))

;;; -------------------------------------------------------

(defun price ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token constitutes a ``:price'', on confirmation returning in its
   parse result the probed token's integer value."
  (the Parser
    (bindlet (price (of-type :price))
      (return-value
        (token-value price)))))

;;; -------------------------------------------------------

(defun integer-value ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token constitutes a ``:number'', on confirmation returning in its
   parse result the probed token's integer value."
  (the Parser
    (bindlet (integer-token (of-type :number))
      (declare (type Token integer-token))
      (return-value
        (token-value integer-token)))))

;;; -------------------------------------------------------

(defun newline ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token constitutes a newline, on confirmation returning in its parse
   result the probed token."
  (the Parser
    (of-type :newline)))

;;; -------------------------------------------------------

(defun end-of-program ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   token conforms to the end-of-file (``:eof'') species, on confirmation
   returning in its parse result the probed token."
  (the Parser
    (of-type :eof)))

;;; -------------------------------------------------------

(defun end-of-line ()
  "Returns a new ``Parser'' which succeeds if a statement's coda, that
   is, either a sequence of one or more accolent newlines, or the end of
   the source, follows, on confirmation returning in its parse result
   the matching tokens."
  (the Parser
    (any-of
      (chain-of
        (newline)
        (many-of
          (newline)))
      (end-of-program))))

;;; -------------------------------------------------------

(defun newlines ()
  "Returns a new ``Parser'' which succeeds if a sequence of one or more
   accolent newlines follows, on confirmation returning in its parse
   result the gathered tokens."
  (the Parser
    (chain-of
      (newline)
      (many-of
        (newline)))))

;;; -------------------------------------------------------

(defun empty-lines ()
  "Returns a new ``Parser'' which always succeeds, matching zero or more
   accolent newlines, and returning in its parse result's outputs the
   matching tokens."
  (the Parser
    (many-of
      (newlines))))

;;; -------------------------------------------------------

(defun welcome ()
  "Returns a new ``Parser'' which succeeds if the \"Welcome to C-Shop\"
   command tokens follow, on confirmation returning in its parse
   result's output a ``Welcome-Instruction'' node representation."
  (the Parser
    (chain-of
      (phrase-of "Welcome" "to" "C-Shop")
      (return-value
        (make-instance 'Welcome-Instruction)))))

;;; -------------------------------------------------------

(defun price-for-big-C ()
  "Returns a new ``Parser'' which succeeds if the \"The big C
   costs ...\" command tokens follow, on confirmation returning in its
   parse result's output a ``Set-Price-Instruction'' node
   representation."
  (the Parser
    (chain-of
      (phrase-of "The" "big" "C" "costs")
      (bindlet (price (price))
        (declare (type integer price))
        (return-value
          (make-instance 'Set-Price-Instruction
            :product :big-C
            :price   price))))))

;;; -------------------------------------------------------

(defun price-for-small-C ()
  "Returns a new ``Parser'' which succeeds if the \"The small c costs\"
   command tokens follow, on confirmation returning in its parse
   result's output a ``Set-Price-Instruction'' node representation."
  (the Parser
    (chain-of
      (phrase-of "The" "small" "c" "costs")
      (bindlet (price (price))
        (declare (type integer price))
        (return-value
          (make-instance 'Set-Price-Instruction
            :product :small-c
            :price   price))))))

;;; -------------------------------------------------------

(defun ask-price ()
  "Returns a new ``Parser'' which succeeds if the \"Ask price of the
   big C/small c\" command tokens follow, on confirmation returning in
   its parse result's output a ``Ask-Price-Instruction'' node
   representation."
  (the Parser
    (chain-of
      (phrase-of "Ask" "price" "of" "the")
      (any-of
        (chain-of
          (phrase-of "big" "C")
          (return-value
            (make-instance 'Ask-Price-Instruction
              :product :big-C)))
        (chain-of
          (phrase-of "small" "c")
          (return-value
            (make-instance 'Ask-Price-Instruction
              :product :small-c)))))))

;;; -------------------------------------------------------

(defun purchase ()
  "Returns a new ``Parser'' which succeeds if the \"Person ... buys ...
   big C's and ... small c's\" command tokens follow, on confirmation
   returning in its parse result's output a ``Purchase-Instruction''
   node representation."
  (the Parser
    (chain-of
      (word-of "Person")
      (bindlet (person-id (integer-value))
        (declare (type integer person-id))
        (chain-of
          (word-of "buys")
          (bindlet (amount-of-big-Cs (integer-value))
            (declare (type integer amount-of-big-Cs))
            (chain-of
              (phrase-of "big" "C's" "and")
              (bindlet (amount-of-small-cs (integer-value))
                (declare (type integer amount-of-small-cs))
                (chain-of
                  (phrase-of "small" "c's")
                  (return-value
                    (make-instance 'Purchase-Instruction
                      :person-id
                        (ensure-positive-integer person-id)
                      :amount-of-big-Cs
                        (ensure-non-negative-integer amount-of-big-Cs)
                      :amount-of-small-cs
                        (ensure-non-negative-integer
                          amount-of-small-cs))))))))))))

;;; -------------------------------------------------------

(defun pay ()
  "Returns a new ``Parser'' which succeeds if the \"Person ... will pay
   for his order\" command tokens follow, on confirmation returning in
   its parse result's output a ``Pay-Bill-Instruction'' node
   representation."
  (the Parser
    (chain-of
      (word-of "Person")
      (bindlet (person-id (integer-value))
        (declare (type integer person-id))
        (chain-of
          (phrase-of "will" "pay" "for" "his" "order")
          (return-value
            (make-instance 'Pay-Bill-Instruction
              :person-id (ensure-positive-integer person-id))))))))

;;; -------------------------------------------------------

(defun command ()
  "Returns a new ``Parser'' which succeeds if a C-Shop command could be
   detected, on confirmation returning in its parse result's output the
   operation's node representation."
  (the Parser
    (any-of
      (price-for-big-C)
      (price-for-small-c)
      (ask-price)
      (purchase)
      (pay))))

;;; -------------------------------------------------------

(defun commands ()
  "Returns a new ``Parser'' which always succeeds, parsing zero or more
   C-Shop commands, and returning in its parse result's output a list
   containing the parsed commands' node representations in their correct
   order."
  (the Parser
    (many-of
      (chain-of
        (newlines)
        (command)))))

;;; -------------------------------------------------------

(defun program ()
  "Returns a new ``Parser'' which succeeds a valid C-Shop program
   follows, on confirmation returning in its parse result's output a
   list of the retrieved instructions."
  (the Parser
    (chain-of
      (empty-lines)
      (bindlet (welcome (welcome))
        (chain-of
          (newlines)
          (bindlet (initial-price-for-big-C (price-for-big-c))
            (chain-of
              (newlines)
              (bindlet (initial-price-for-small-c (price-for-small-c))
                (bindlet (commands (commands))
                  (chain-of
                    (empty-lines)
                    (end-of-program)
                    (return-value
                      (append
                        (list welcome
                              initial-price-for-big-c
                              initial-price-for-small-C)
                        commands))))))))))))

;;; -------------------------------------------------------

(defun parse-state (parser initial-state)
  "Parses the INITIAL-STATE utilizing the PARSER and returns its parse
   result."
  (declare (type Parser      parser))
  (declare (type Parse-State initial-state))
  (the Parse-Result
    (parser-parse parser initial-state)))

;;; -------------------------------------------------------

(defun parse-C-shop-program (code)
  "Parses the piece of C-Shop source CODE utilizing and returns the
   resulting ``c-shop-program'', if successful, otherwise signaling an
   error of an unspecified type."
  (declare (type string code))
  (let ((result
          (parse-state
            (program)
            (make-initial-parse-state
              (make-lexer code)))))
    (declare (type Parse-Result result))
    (the c-shop-program
      (if (parse-result-succeeded-p result)
        (parse-result-output result)
        (error "Could not parse the piece of C-Shop source code.")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        interpreter-program
    :type          c-shop-program
    :documentation "The C-Shop program to interpreter, provided as a
                    list of instructions.")
   (program-state
    :initform      (list :expect-welcome
                         :expect-big-C-price
                         :expect-small-c-price
                         :standard)
    :accessor      interpreter-program-states
    :type          c-shop-program-states
    :documentation "Defines the instructions which must be passed by the
                    interpreter in order to validate the PROGRAM's
                    conformance to the standard.")
   (big-C-price
    :initform      0
    :type          integer
    :documentation "The current price specified for one unit of a big
                    C.")
   (small-c-price
    :initform      0
    :type          integer
    :documentation "The current price specified for one unit of a small
                    c.")
   (customers
    :initform      (make-hash-table :test #'eql)
    :accessor      interpreter-customers
    :type          (hash-table-of positive-integer integer)
    :documentation "Maintains an unordered collection of customers, each
                    registered with a unique positive integer
                    identifier, and associated with its selected
                    goods."))
  (:documentation
    "The ``Interpreter'' class is apportioned the wike of processing a
     C-Shop program in order to accompass actual effect to the same."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' whose responsibility is
   indebted to the C-Shop PROGRAM's evaluation."
  (declare (type c-shop-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun interpreter-current-program-state (interpreter)
  "Returns the INTERPRETER's current program state."
  (declare (type Interpreter interpreter))
  (the (or null c-shop-program-state)
    (first (interpreter-program-states interpreter))))

;;; -------------------------------------------------------

(defun interpreter-advance-program-state (interpreter)
  "Transitions the INTERPRETER into the subsequent program state and
   returns no value."
  (declare (type Interpreter interpreter))
  (pop (interpreter-program-states interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-price-of (interpreter product)
  "Returns the current price of the PRODUCT as registered at the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type product     product))
  (the integer
    (case product
      (:big-C    (slot-value interpreter 'big-C-price))
      (:small-c  (slot-value interpreter 'small-c-price))
      (otherwise (error "Unrecognized product: ~s." product)))))

;;; -------------------------------------------------------

(defun (setf interpreter-price-of) (new-price interpreter product)
  "Sets the PRODUCT's price to the NEW-PRICE in the INTEPRETER and
   returns no value."
  (declare (type integer     new-price))
  (declare (type Interpreter interpreter))
  (declare (type product     product))
  (case product
    (:big-C    (setf (slot-value interpreter 'big-C-price)   new-price))
    (:small-c  (setf (slot-value interpreter 'small-c-price) new-price))
    (otherwise (error "Unrecognized product: ~s." product)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-purchase (interpreter
                             person-id
                             amount-of-big-Cs
                             amount-of-small-cs)
  "Increases the bill of the customer registered with the PERSON-ID at
   the INTERPRETER by an amount tantamount to the price per big C times
   the AMOUNT-OF-BIG-CS and the price per small c times the
   AMOUNT-OF-SMALL-C, and returns no value.
   ---
   If no person with the PERSON-ID can be retrieved, a new entry for the
   same with a zero-valued bill is furnished."
  (declare (type Interpreter          interpreter))
  (declare (type positive-integer     person-id))
  (declare (type non-negative-integer amount-of-big-Cs))
  (declare (type non-negative-integer amount-of-small-cs))
  (incf
    (gethash person-id (interpreter-customers interpreter) 0)
    (+ (* amount-of-big-Cs
          (interpreter-price-of interpreter :big-C))
       (* amount-of-small-cs
          (interpreter-price-of interpreter :small-c))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-bill-for (interpreter person-id)
  "Returns the total costage of the bill accumulated by the customer
   registered with PERSON-ID at the INTERPRETER, or signals an error of
   an unspecified type if none such person exists."
  (declare (type Interpreter      interpreter))
  (declare (type positive-integer person-id))
  (the integer
    (or (gethash person-id (interpreter-customers interpreter))
        (error "No person with the ID ~d registered for paying."
          person-id))))

;;; -------------------------------------------------------

(defun interpreter-pay (interpreter person-id)
  "Returns the character whose ASCII code corresponds to the value of
   the bill accumulated by the customer registered with PERSON-ID at the
   INTERPRETER, or signals an error of an unspecified type if none such
   person exists."
  (declare (type Interpreter      interpreter))
  (declare (type positive-integer person-id))
  (the character
    (code-char
      (interpreter-bill-for interpreter person-id))))

;;; -------------------------------------------------------

(defgeneric interpreter-process-instruction (interpreter instruction)
  (:documentation
    "Interprets the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Welcome-Instruction))
  (declare (type Interpreter         interpreter))
  (declare (type Welcome-Instruction instruction))
  (case (interpreter-current-program-state interpreter)
    ((NIL)
      (error "No program state available."))
    (:expect-welcome
      (interpreter-advance-program-state interpreter))
    (otherwise
      (error "The \"Welcome to C-Shop\" instruction is invalid at ~
              this point of the program, as the current state ~
              constitutes ~s."
        (interpreter-current-program-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Set-Price-Instruction))
  (declare (type Interpreter           interpreter))
  (declare (type Set-Price-Instruction instruction))
  (case (interpreter-current-program-state interpreter)
    ((NIL)
      (error "No program state available."))
    (:expect-welcome
      (error "You must welcome the customer ere you can specify the ~
              big C's price."))
    (:expect-big-C-price
      (case (set-price-instruction-product instruction)
        (:big-C
          (setf (interpreter-price-of interpreter :big-C)
            (set-price-instruction-price instruction)))
        (:small-c
          (error "You must set the price of a big C ere you can ~
                  specify that of a small c."))
        (otherwise
          (error "Invalid product: ~s."
            (set-price-instruction-product instruction))))
      (interpreter-advance-program-state interpreter))
    (:expect-small-c-price
      (case (set-price-instruction-product instruction)
        (:big-C
          (error "You must set the price of a small c ere you can ~
                  progress."))
        (:small-c
          (setf (interpreter-price-of interpreter :small-c)
            (set-price-instruction-price instruction)))
        (otherwise
          (error "Invalid product: ~s."
            (set-price-instruction-product instruction))))
      (interpreter-advance-program-state interpreter))
    (:standard
      (setf (interpreter-price-of interpreter
              (set-price-instruction-product instruction))
        (set-price-instruction-price instruction)))
    (otherwise
      (error "The \"Welcome to C-Shop\" instruction is invalid at ~
              this point of the program, as the current state ~
              constitutes ~s."
        (interpreter-current-program-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Ask-Price-Instruction))
  (declare (type Interpreter           interpreter))
  (declare (type Ask-Price-Instruction instruction))
  (case (interpreter-current-program-state interpreter)
    (:standard
      (format T "~&>> ")
      (finish-output)
      (setf (interpreter-price-of interpreter
              (ask-price-instruction-product instruction))
        (char-code
          (read-char)))
      (clear-input))
    (otherwise
      (error "Cannot ask price while in the state ~s."
        (interpreter-current-program-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Purchase-Instruction))
  (declare (type Interpreter          interpreter))
  (declare (type Purchase-Instruction instruction))
  (case (interpreter-current-program-state interpreter)
    (:standard
      (interpreter-purchase interpreter
        (purchase-instruction-person-id          instruction)
        (purchase-instruction-amount-of-big-cs   instruction)
        (purchase-instruction-amount-of-small-cs instruction)))
    (otherwise
      (error "Cannot purchase while in the state ~s."
        (interpreter-current-program-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Pay-Bill-Instruction))
  (declare (type Interpreter          interpreter))
  (declare (type Pay-Bill-Instruction instruction))
  (case (interpreter-current-program-state interpreter)
    (:standard
      (write-char
        (interpreter-pay interpreter
          (pay-bill-instruction-person-id instruction))))
    (otherwise
      (error "Cannot pay while in the state ~s."
        (interpreter-current-program-state interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Evaluates the C-Shop program stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (dolist (instruction (interpreter-program interpreter))
    (declare (type Instruction instruction))
    (interpreter-process-instruction interpreter instruction))
  (values))

;;; -------------------------------------------------------

(defun interpret-C-Shop (code)
  "Interprets the piece of C-Shop source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-C-Shop-program code))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A" by adminiculum of its ASCII code 65, replicated
;; via:
;;   ascii_code = (2 * price_for_big_C) + (1 * price_per_small_c)
;;              = (2 * 30)              + (1 * 5)
;;              = 60                    + 5
;;              = 65.
(interpret-C-Shop
  "Welcome to C-Shop
   The big C costs $30
   The small c costs $5
   Person 1 buys 2 big C's and 1 small c's
   Person 1 will pay for his order")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-C-Shop
  "Welcome to C-Shop
   The big C costs $0
   The small c costs $0
   Ask price of the small c
   Person 1 buys 0 big C's and 1 small c's
   Person 1 will pay for his order")

;;; -------------------------------------------------------

;; Print "Hello World!".
(interpret-C-Shop
  "Welcome to C-Shop
   The big C costs $16
   The small c costs $1
   Person 1 buys 4 big C's and 8 small c's
   Person 1 will pay for his order
   Person 2 buys 6 big C's and 5 small c's
   Person 2 will pay for his order
   Person 3 buys 6 big C's and 12 small c's
   Person 3 will pay for his order
   Person 3 will pay for his order
   Person 4 buys 6 big C's and 15 small c's
   Person 4 will pay for his order
   Person 5 buys 2 big C's and 0 small c's
   Person 5 will pay for his order
   Person 6 buys 5 big C's and 7 small c's
   Person 6 will pay for his order
   Person 4 will pay for his order
   Person 7 buys 7 big C's and 2 small c's
   Person 7 will pay for his order
   Person 3 will pay for his order
   Person 8 buys 6 big C's and 4 small c's
   Person 8 will pay for his order
   Person 9 buys 2 big C's and 1 small c's
   Person 9 will pay for his order")
