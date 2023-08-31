;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Glassman", invented by the Esolang user "Cinnamony" and
;; presented on June 22nd, 2023, the contrivance woning inwith realized
;; in the acquisition of the lyrics from the song "Mister Glassman",
;; by Scotty Sire, in order to limn its instructions, the foundational
;; characteristics of which are appropriated from Urban Mueller's
;; language "brainfuck".
;; 
;; 
;; Concept
;; =======
;; The Glassman programming language is based upon a mimicry of the
;; "Mister Glassman" song's lyrics, their application accompasses the
;; operation of a system derived from Urban Mueller's brainfuck.
;; 
;; == LYRICS ENCODE COMMANDS ==
;; The programs' syntaxis ensues from the lyrics of the song "Mister
;; Glassman", performed by the YouTube personality Scotty Sire and
;; released in the year 2018, the assorted lines of which encode the
;; language's facilities.
;; 
;; == MEMORY IS REALIZED IN BYTE-VALUED CELLS ==
;; A dation from its brainfuck entheus, programs in this language
;; operate on a bilaterally infinite expanse of cells, each an aefauld
;; unsigned byte's salvatory. A cell pointer selects the current
;; instance, the same is apportioned the sole amenability to requests
;; and modifications.
;; 
;; 
;; Architecture
;; ============
;; An aspirant of its stock-father's architectural concoctions,
;; Glassman's acquisition extracts the infinite tape of byte-valued
;; cells with its cell pointer in a verbatim ilk of lealty.
;; 
;; 
;; Data Types
;; ==========
;; A more resourceful aspect commorant in Glassman is ostended in its
;; type system, the circumference of which augments the twain of
;; paravaunt octet and parhedral ASCII character by a string literals
;; that may be expressed by a dedicated command's mediation.
;; 
;; 
;; Syntax
;; ======
;; Regarding its syntactical diorism, Glassman ostends programs as
;; lines of commands.
;; 
;; == INSTRUCTIONS ==
;; A Glassman program is expressed in a sequence of zero or more lines,
;; each such either blank or an aefauld command's, ensconced betwixt
;; two imperative markers, the script preable and its coda.
;; 
;; == WHITESPACES ==
;; A thing of mandatory implication betwixt its keyword tokens, however,
;; not constrained to a maximum in these intermedes, spaces at any other
;; location are considered with perfect tolerance.
;; 
;; Newlines, following a similar notion, ought to appear betwixt two
;; lines of commands, and may contribute empty or blank specimens,
;; mandated by their presence, but not regulated concerning their
;; tally.
;; 
;; == COMMENTS ==
;; No provision for comments exist in the current language rendition.
;; 
;; == GRAMMAR ==
;; A formal definition of Glassman's donat shall now be adhibited:
;; 
;;   program            := scriptPreamble
;;                      ,  newlines
;;                      ,  commandList
;;                      ,  newlines
;;                      ,  scriptCoda
;;                      ,  [ newlines ]
;;                      ;
;;   commandList        := [ newlines ]
;;                      ,  [ command , { newlines , command } ]
;;                      ;
;;   command            := printStringCommand
;;                      |  printCellCommand
;;                      |  inputCommand
;;                      |  moveRightCommand
;;                      |  moveLeftCommand
;;                      |  incrementCommand
;;                      |  decrementCommand
;;                      |  loopCommand
;;                      ;
;;   scriptPreamble     := "Let's paint a picture" ;
;;   scriptCoda         := "Woppity wop" ;
;;   printStringCommand := "They're calling her"
;;                      ,  [ space , { character - newline } ]
;;                      ;
;;   printCellCommand   := "In front of fans rappin' fast" ;
;;   inputCommand       := "So nervously he looks around" ;
;;   moveRightCommand   := "A few years later when now it's prom season" ;
;;   moveLeftCommand    := "You're in class" ;
;;   incrementCommand   := "And he's always in his head thinking too far ahead" ;
;;   decrementCommand   := "Don't be shy kid, make your own fate" ;
;;   loopCommand        := loopStartCommand , newlines
;;                      ,  commandList
;;                      ,  [ newlines ]
;;                      ,  loopEndCommand
;;                      ;
;;   loopStartCommand   := "Even if you think you're bullet proof" ;
;;   loopEndCommand     := "I am glass and so are you" ;
;;   newlines           := newline , { newline } ;
;;   newline            := "\n" ;
;;   space              := " " ;
;; 
;; 
;; Instructions
;; ============
;; The Glassman programming language's instruction set enumerates a
;; cardinality of eleven participants, embracing in its perimeter the
;; program commencement and termination markers, input and output
;; facilities, memory manipulation, as well as jump-based control flow
;; operations.
;; 
;; == BRAINFUCK: A GENEROUS DONOR ==
;; A peisant subset from this entirety has been desumed from brainfuck
;; namely, ultimately augmented by the start and end designators'
;; routines and advenient potence in string display.
;; 
;; == OVERVIEW ==
;; The following apercu shall adhibit a foundational piece of gnarity
;; concerning the language's operational competences, endowed, however,
;; with merely a cursory and tentative vista, intended to be amplified
;; by the next section's elucidations.
;; 
;;   ------------------------------------------------------------------
;;   Command                                            | Effect
;;   ---------------------------------------------------+--------------
;;   Let's paint a picture                              | Start script
;;   ..................................................................
;;   So nervously he looks around                       | Input in cell
;;   ..................................................................
;;   In front of fans rappin' fast                      | Output cell
;;   ..................................................................
;;   They're calling her [...]                          | Output text
;;   ..................................................................
;;   A few years later when now it's prom season        | Move left
;;   ..................................................................
;;   You're in class                                    | Move right
;;   ..................................................................
;;   And he's always in his head thinking too far ahead | Increment
;;   ..................................................................
;;   Don't be shy kid, make your own fate               | Decrement
;;   ..................................................................
;;   Even if you think you're bullet proof              | Jump forward
;;   ..................................................................
;;   I am glass and so are you                          | Jump back
;;   ..................................................................
;;   Woppity wop                                        | End script
;;   ------------------------------------------------------------------
;; 
;; == COMMAND DETAILS ==
;; A more meticulous listing will now entalent the reader with superior
;; sophistication in acquaintance regarding the operations.
;; 
;; Please heed that placeholder sections are underlined with a series of
;; asterisks ("*"), intended for substitution by valid Glassman code.
;; 
;;   +==================+
;;   |== START SCRIPT ==|
;;   +==================+==============================================
;;   | Command | Let's paint a picture
;;   |---------+-------------------------------------------------------
;;   | Effect  | Starts the script.
;;   +=================================================================
;;   
;;   +=====================+
;;   |== QUERY FOR INPUT ==|
;;   +=====================+===========================================
;;   | Command | So nervously he looks around
;;   |---------+-------------------------------------------------------
;;   | Effect  | Queries the standard input for an ASCII character and
;;   |         | stores its character code in the current cell.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +==================+
;;   |== ISSUE OUTPUT ==|
;;   +==================+==============================================
;;   | Command | In front of fans rappin' fast
;;   |---------+-------------------------------------------------------
;;   | Effect  | Prints the character whose ASCII code corresponds to
;;   |         | the current cell value to the standard output.
;;   +=================================================================
;;   
;;   +=================+
;;   |== OUTPUT TEXT ==|
;;   +=================+===============================================
;;   | Command | They're calling her text
;;   |         |                     ****
;;   |---------+-------------------------------------------------------
;;   | Effect  | Prints the {text} to the standard output.
;;   |         | The text must be a sequence of zero or more
;;   |         | characters, extending to the end of the current line.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +=============================+
;;   |== MOVE CELL POINTER RIGHT ==|
;;   +=============================+===================================
;;   | Command | A few years later when now it's prom season
;;   |---------+-------------------------------------------------------
;;   | Effect  | Moves the memory's cell pointer one step to the right.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +============================+
;;   |== MOVE CELL POINTER LEFT ==|
;;   +============================+====================================
;;   | Command | You're in class
;;   |---------+-------------------------------------------------------
;;   | Effect  | Moves the memory's cell pointer one step to the left.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +====================+
;;   |== INCREMENT CELL ==|
;;   +====================+============================================
;;   | Command | And he's always in his head thinking too far ahead
;;   |---------+-------------------------------------------------------
;;   | Effect  | Increments the current cell by one.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +====================+
;;   |== DECREMENT CELL ==|
;;   +====================+============================================
;;   | Command | Don't be shy kid, make your own fate
;;   |---------+-------------------------------------------------------
;;   | Effect  | Decrements the current cell by one.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +==================+
;;   |== JUMP FORWARD ==|
;;   +==================+==============================================
;;   | Command | Even if you think you're bullet proof
;;   |---------+-------------------------------------------------------
;;   | Effect  | If the current cell value equals zero (0), moves the
;;   |         | instruction pointer (IP) forward to the position
;;   |         | immediately succeeding the matching back jump command
;;   |         | "I am glass and so are you". Otherwise proceeds as
;;   |         | usual.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +===============+
;;   |== JUMP BACK ==|
;;   +===============+=================================================
;;   | Command | I am glass and so are you
;;   |---------+-------------------------------------------------------
;;   | Effect  | If the current cell value does not equal zero (0),
;;   |         | moves the instruction pointer (IP) back to the
;;   |         | position immediately succeeding the matching forward
;;   |         | jump command "Even if you think you're bullet proof".
;;   |         | Otherwise proceeds as usual.
;;   |         |-------------------------------------------------------
;;   |         | This command constitutes a verbatim appropriation from
;;   |         | brainfuck.
;;   +=================================================================
;;   
;;   +================+
;;   |== END SCRIPT ==|
;;   +================+================================================
;;   | Command | Woppity wop
;;   |---------+-------------------------------------------------------
;;   | Effect  | Ends the script.
;;   +=================================================================
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its brainfuck cleronomy, a legacy that tacitly imparts some
;; mete of gnarity about certain behaviors and their expectancy, a
;; few inroads of incertitude plague the Glassman protolog, whence a
;; subset shall extricated for further perquisition.
;; 
;; == DO COMMANDS REQUIRE NEWLINE SEPARATORS? ==
;; Whereas all examples accommodate one line's commorancy per command,
;; a definite statement about this ordonnance's imperative or optional
;; nature acquires no exposition.
;; 
;; It has been adjudged that a single line's allotment constitutes a
;; fact more peisant than an endeictic warkloom; a consectary from the
;; Glassman programming language's foundation, a song which, like most
;; musical compositions, derives its concinnity from the segregation
;; into lines of lyrics, every instruction must be such a horizontal
;; abode's recepient. Blank lines, of course, may participate in any
;; conceivable mete.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes from characters.
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
;; Date:   2023-08-26
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
;;   [esolang2023Glassman]
;;   The Esolang contributors, "Glassman", June 24th, 2023
;;   URL: "https://esolangs.org/wiki/Glassman"
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, the keys of which conform to the KEY-TYPE and associate
   with values of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype parser-processor ()
  "The ``parser-processor'' type defines a function responsible for the
   parsing of parse state in order to respond with a parse result,
   implemented as a function from a ``Parse-State'' input to a
   ``Parse-Result'' output."
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser''s."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   ``Node'' objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, and thus a commorant of the integer range [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ()
  (:documentation
    "The ``Node'' interface accommodates a common foundry for all
     classes intent on representing abstract syntax tree (AST) nodes."))

;;; -------------------------------------------------------

(defclass Program-Node (Node)
  ((statements
    :initarg       :statements
    :initform      NIL
    :reader        program-node-statements
    :type          node-list
    :documentation "The program's body."))
  (:documentation
    "The ``Program-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node providing the entire program's
     elements."))

;;; -------------------------------------------------------

(defclass Input-Node (Node)
  ()
  (:documentation
    "The ``Input-Node'' class encapsulates the definition of an abstract
     syntax tree (AST) node capacitating the request for an ASCII
     character input."))

;;; -------------------------------------------------------

(defclass Print-Cell-Node (Node)
  ()
  (:documentation
    "The ``Print-Cell-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the output of the
     current memory cell as an ASCII character."))

;;; -------------------------------------------------------

(defclass Print-String-Node (Node)
  ((text
    :initarg       :text
    :initform      (error "Missing text.")
    :reader        print-string-node-text
    :type          string
    :documentation "The string to print."))
  (:documentation
    "The ``Print-String-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the output of a
     specified string literal."))

;;; -------------------------------------------------------

(defclass Move-Right-Node (Node)
  ()
  (:documentation
    "The ``Move-Right-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the translation of the
     memory cell pointer to the right by a single step."))

;;; -------------------------------------------------------

(defclass Move-Left-Node (Node)
  ()
  (:documentation
    "The ``Move-Left-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the translation of the
     memory cell pointer to the left by a single step."))

;;; -------------------------------------------------------

(defclass Increment-Node (Node)
  ()
  (:documentation
    "The ``Increment-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the incrementation of
     the current memory cell by a magnitude of one."))

;;; -------------------------------------------------------

(defclass Decrement-Node (Node)
  ()
  (:documentation
    "The ``Decrement-Node'' class encapsulates the definition of an
     abstract syntax tree (AST) node capacitating the decrementation of
     the current memory cell by a magnitude of one."))

;;; -------------------------------------------------------

(defclass Loop-Node (Node)
  ((statements
    :initarg       :statements
    :initform      NIL
    :reader        loop-node-statements
    :type          node-list
    :documentation ""))
  (:documentation
    "The ``Loop-Node'' class encapsulates the definition of an abstract
     syntax tree (AST) node capacitating the repeated execution of a
     series of statements in dependence upon the current memory cell
     value's inequality to zero (0)."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-parse-state (source position)))
  "The ``Parse-State'' class serves in the encapsulation of the parsing
   effort's progress."
  (source   (error "Missing source.") :type string)
  (position 0                         :type fixnum))

;;; -------------------------------------------------------

(defun parse-state-current-element (state)
  "Returns the character referenced by the parse STATE, or ``NIL'' if
   the underlying source is exhausted."
  (declare (type Parse-State state))
  (the (or null character)
    (when (array-in-bounds-p
            (parse-state-source   state)
            (parse-state-position state))
      (char
        (parse-state-source   state)
        (parse-state-position state)))))

;;; -------------------------------------------------------

(defun parse-state-advance (state)
  "Creates and returns a new ``Parse-State'' which shares the input
   STATE's source, but advances the position cursor to the next location
   in the same."
  (declare (type Parse-State state))
  (the Parse-State
    (make-parse-state
      (parse-state-source state)
      (1+ (parse-state-position state)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result
                  (succeeded-p
                   &optional (state NIL) (output NIL))))
  "The ``Parse-Result'' class encapsulates a ``Parser'' invocation's
   response to the request of evaluating a ``Parse-State'',
   comprehending a success flag that determines whether the parser has
   matched, the ``Parse-State'' produced by the parser's aaplication,
   and the parser's actual output, that is, its contribution to the
   ultimately produced abstract syntax tree (AST) representation of the
   processed Glassman source code."
  (succeeded-p (error "Missing success flag.")
                   :type boolean)
  (state       NIL :type (or null Parse-State))
  (output      NIL :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class serves as a representative of a parser or a
   combinator, invested with the duty to process a ``Parse-State'' in
   order to respond with a ``Parse-Result'', the former provides the
   Glassman source code's currently probed character, or ``NIL'' if the
   same is exhausted, the latter determines whether the ``Parser'' has
   succeeded, a ``Parse-State'' that either equals the input or defines
   one advanced to the next source position, and an output which
   constitutes its contribution to the assembled abstract syntax tree
   (AST)."
  (processor (error "Missing processor.") :type parser-processor))

;;; -------------------------------------------------------

(defun parser-parse (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   representative of the matching or failure."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall
      (parser-processor parser)
      state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of foundational parser operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun probe-character (predicate)
  "Returns a ``Parser'' which succeeds if the PREDICATE, applied to its
   ``Parse-State'' character, the same might be ``NIL'', matches,
   returning on confirmation in its ``Parse-Result'' the probed
   character."
  (declare (type (function ((or null character)) *) predicate))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (let ((probed-character
                    (parse-state-current-element state)))
              (declare (type (or null character) probed-character))
              (the Parse-Result
                (if (funcall predicate probed-character)
                  (make-parse-result T
                    (parse-state-advance state)
                    probed-character)
                  (make-parse-result NIL state probed-character)))))))))

;;; -------------------------------------------------------

(defmacro define-character-parser ((character-variable) &body body)
  "Creates a new ``Parser'' instance whose processor function expects
   a potentially ``NIL''-valued character norned by the
   CHARACTER-VARIABLE, the function body being composed of the BODY
   forms, the desinent form's result is returned, and, if not ``NIL'',
   is construed as having matched, returning in this case a
   ``Parse-Result'' comprehending the probed character."
  `(the Parser
     (probe-character
       #'(lambda (,character-variable)
           (declare (type (or null character) ,character-variable))
           (declare (ignorable                ,character-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun .character (expected-character)
  "Returns a ``Parser'' which succeeds if its probed ``Parse-State''
   contains the EXPECTED-CHARACTER, on success comprehending in its
   ``Parse-Result'' the matched character."
  (declare (type character expected-character))
  (the Parser
    (define-character-parser (probed-character)
      (and probed-character
           (char= probed-character expected-character)))))

;;; -------------------------------------------------------

(defun .member (expected-characters)
  "Returns a ``Parser'' which succeeds if its probed ``Parse-State''
   contains any of the EXPECTED-CHARACTERS, on success comprehending in
   its ``Parse-Result'' the matched character."
  (declare (type (or (list-of character) string) expected-characters))
  (the Parser
    (define-character-parser (probed-character)
      (and probed-character
           (find probed-character expected-characters
                 :test #'char=)))))

;;; -------------------------------------------------------

(defun .space ()
  "Returns a ``Parser'' which succeeds if its probed ``Parser-State''
   contains a space or tab character, on success comprehending in its
   ``Parse-Result'' the matched character."
  (the Parser
    (.member '(#\Space #\Tab))))

;;; -------------------------------------------------------

(defun .newline ()
  "Returns a ``Parser'' which succeeds if its probed ``Parser-State''
   contains a newline character, on success comprehending in its
   ``Parse-Result'' the matched character."
  (the Parser
    (.character #\Newline)))

;;; -------------------------------------------------------

(defun .any-character ()
  "Returns a ``Parser'' which succeeds if its probed ``Parse-State''
   contains a non-newline character, on success comprehending in its
   ``Parse-Result'' the matched character."
  (the Parser
    (define-character-parser (probed-character)
      (and probed-character
           (char/= probed-character #\Newline)))))

;;; -------------------------------------------------------

(defun .eof ()
  "Returns a ``Parser'' which succeeds if its probed ``Parse-State''
   contains the ``NIL'' character, thus signifying the underlying
   source string's exhaustion, on success comprehending in its
   ``Parse-Result'' the received ``NIL'' value."
  (the Parser
    (define-character-parser (probed-character)
      (null probed-character))))

;;; -------------------------------------------------------

(defun .bind (antecedent parser-generator)
  "Returns a ``Parser'' which implements the a monadic binding, that is,
   it succeeds if the ANTECEDENT matches, and, supplying its result's
   output to the PARSER-GENERATOR for receiving a consequent parser,
   this second parser also matches, returning on confirmation in its
   ``Parse-Result'' the consequent parser's output."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) parser-generator))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((antecedent-result (parser-parse antecedent state)))
            (declare (type Parse-Result antecedent-result))
            (the Parse-Result
              (if (parse-result-succeeded-p antecedent-result)
                (let ((consequent-parser
                        (funcall parser-generator
                          (parse-result-output antecedent-result))))
                  (declare (type Parser consequent-parser))
                  (parser-parse consequent-parser
                    (parse-result-state antecedent-result)))
                antecedent-result)))))))

;;; -------------------------------------------------------

(defmacro .let ((antecedent-output-variable antecedent)
                &body body)
  "Returns a ``Parser'' which implements a monadic binding, submitting
   the output of the ANTECEDENT parser, if the same satisfies its
   ``Parse-State'''s character, as the parameter norned by the
   ANTECEDENT-OUTPUT-VARIABLE to an anonymous function whose forms are
   defined by the BODY, the last among these required to return a
   ``Parser'' whose ``Parse-Result'' is relayed."
  `(the Parser
     (.bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun .return (output)
  "Returns a ``Parser'' which always succeeds, returning in its
   ``Parse-Result'' the OUTPUT."
  (declare (type T output))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (make-parse-result T state output))))))

;;; -------------------------------------------------------

(defun .chain (&rest parsers)
  "Returns a ``Parser'' which succeeds if all of the specified PARSERS,
   in this exact order, match, returning on confirmation in its
   ``Parse-Result'' the desinent parser's output."
  (declare (type parser-list parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state current-result)
              
              for current-parser
                of-type Parser
                in      parsers
              
              for current-result
                of-type Parse-Result
                =       (parser-parse current-parser new-state)
              
              unless (parse-result-succeeded-p current-result) do
                (return
                  (make-parse-result NIL
                    (parse-result-state  current-result)
                    (parse-result-output current-result)))
              
              finally
                (return current-result)))))))

;;; -------------------------------------------------------

(defun .all (&rest parsers)
  "Returns a ``Parser'' which succeeds if all the specified PARSERS, in
   this exact order, match, returning on confirmation in its
   ``Parse-Result'' their compiled outputs in their matching order."
  (declare (type parser-list parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state current-result)
              
              for current-parser
                of-type Parser
                in      parsers
              
              for current-result
                of-type Parse-Result
                =       (parser-parse current-parser new-state)
              
              if (parse-result-succeeded-p current-result)
                collect (parse-result-output current-result)
                into    outputs
              else do
                (return current-result)
              end
              
              finally
                (return
                  (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun .many (parser)
  "Returns a ``Parser'' which always succeeds by applying the specified
   PARSER until it fails to match, finally returning an ordered list of
   the PARSER's successful match outputs."
  (declare (type Parser parser))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for new-state
                of-type Parse-State
                =       state
                then    (parse-result-state current-result)
              
              for current-result
                of-type Parse-Result
                =       (parser-parse parser new-state)
              
              if (parse-result-succeeded-p current-result)
                collect (parse-result-output current-result)
                into    outputs
              else do
                (loop-finish)
              end
              
              finally
                (return
                  (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun .many-1 (parser)
  "Returns a ``Parser'' which succeeds if the specified PARSER matches
   one or more times, returning on confirmation in its ``Parse-Result''
   an ordered list of the successful matches' outputs."
  (declare (type Parser parser))
  (the Parser
    (.let (first-output parser)
      (declare (type T first-output))
      (.let (more-outputs (.many parser))
        (declare (type list more-outputs))
        (.return
          (cons first-output more-outputs))))))

;;; -------------------------------------------------------

(defun .or (&rest choices)
  "Returns a ``Parser'' which succeeds if any of its CHOICES, probed in
   this exact order, match, returning on confirmation the first matching
   parser's ``Parse-Result''."
  (declare (type parser-list choices))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for current-parser
                of-type Parser
                in      choices
              
              for current-result
                of-type Parse-Result
                =       (parser-parse current-parser state)
              
              when (parse-result-succeeded-p current-result) do
                (return current-result)
              
              finally
                (return
                  (make-parse-result NIL state
                    (parse-state-current-element state)))))))))

;;; -------------------------------------------------------

(defun .all-separated-by (parsers separator)
  "Returns a ``Parser'' which succeeds if all of its PARSERS, in this
   exact order match, with each twain separated by teh SEPARATOR,
   returning on confirmation in its ``Parse-Result'' an ordered list
   comprehending the compiled parser outputs."
  (declare (type parser-list parsers))
  (declare (type Parser      separator))
  (the Parser
    (.let (first-output (first parsers))
      (declare (type T first-output))
      (.let (further-outputs
              (apply #'.all
                (mapcar
                  #'(lambda (parser)
                      (declare (type Parser parser))
                      (the Parser
                        (.chain separator parser)))
                  (rest parsers))))
        (declare (type list further-outputs))
        (.return
          (cons first-output further-outputs))))))

;;; -------------------------------------------------------

(defun .between (open-guard close-guard body)
  "Returns a new ``Parser'' which succeeds if the BODY parser is
   surrounding by the OPEN-GUARD to its left and the CLOSE-GUARD to its
   right, returning on confirmation in the ``Parse-Result'' the BODY
   parser's output."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser body))
  (the Parser
    (.chain open-guard
      (.let (output body)
        (declare (type T output))
        (.chain close-guard
          (.return output))))))

;;; -------------------------------------------------------

(defun .word (expected-word)
  "Returns a ``Parser'' which matches if the characters of the
   EXPECTED-WORD, in their specified order, match, returning on
   confirmation in its ``Parse-Result'' the EXPECTED-WORD itself."
  (declare (type string expected-word))
  (the Parser
    (.chain
      (apply #'.all
        (map 'list #'.character expected-word))
      (.return expected-word))))

;;; -------------------------------------------------------

(defun .spaces ()
  "Returns a ``Parser'' which succeeds if one or more spaces follow in
   succession, returning on confirmation in its ``Parse-Result'' the
   gathered space characters."
  (the Parser
    (.many-1
      (.space))))

;;; -------------------------------------------------------

(defun .skip-empty-lines ()
  "Returns a ``Parser'' which always succeeds, skipping zero or more
   newlines and spaces in succession, returning in its ``Parse-Result''
   an ordered list of the the skipped characters."
  (the Parser
    (.many
      (.or
        (.newline)
        (.space)))))

;;; -------------------------------------------------------

(defun .end-of-line ()
  "Returns a ``Parser'' which succeeds if a newline character or the
   ``NIL'' value, in the latter case signifying the end of the source,
   follows, returning on confirmation in its ``Parse-Result'' the
   matching character."
  (the Parser
    (.or
      (.newline)
      (.eof))))

;;; -------------------------------------------------------

(defun .sentence (&rest words)
  "Returns a ``Parser'' which matches if all of the strings specified by
   the WORDS match in this exact order, each twain separated by one or
   more spaces, returning on confirmation in its ``Parse-Result'' all
   the affirmed tokens from the WORDS."
  (declare (type (list-of string) words))
  (the Parser
    (funcall #'.all-separated-by
      (mapcar #'.word words)
      (.spaces))))

;;; -------------------------------------------------------

(defun .line (parser)
  "Returns a ``Parser'' which succeeds if the PARSER, preceded by zero
   or more optional newline and space characters, matches, and followed
   by a mandatory newline or the end of the source, returning on
   confirmation in its ``Parse-Result'' the PARSER's output."
  (declare (type Parser parser))
  (the Parser
    (.between
      (.skip-empty-lines)
      (.end-of-line)
      parser)))

;;; -------------------------------------------------------

(defun parse (parser initial-state)
  "Applies the PARSER to the INITIAL-STATE and returns its
   ``Parse-Result''."
  (declare (type Parser      parser))
  (declare (type Parse-State initial-state))
  (the Parse-Result
    (parser-parse parser initial-state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of accommodated parsers.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () Parser) parse-statements))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Returns a new ``Parser'' whose processor function expects a
   ``Parse-State'' nevened by the STATE-VARIABLE, and whose body is
   composed of the BODY forms, each of such mandated to be a ``Parser''
   itself, the chained composition of which is applied to the
   STATE-VARIABLE, returning the respective ``Parse-Result''."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           (the Parse-Result
             (parser-parse (.chain ,@body) ,state-variable))))))

;;; -------------------------------------------------------

(defun parse-start-script ()
  "Returns a ``Parser'' which succeeds if a line comprehending the
   program start marker \"Let's paint a picture\" follows, returning on
   confirmation in its ``Parse-Result'' the affirming tokens."
  (build-parser (state)
    (.line (.sentence "Let's" "paint" "a" "picture"))))

;;; -------------------------------------------------------

(defun parse-end-script ()
  "Returns a ``Parser'' which succeeds if a line comprehending the
   program end marker \"Woppity wop\" follows, returning on confirmation
   in its ``Parse-Result'' the affirming tokens."
  (build-parser (state)
    (.line (.sentence "Woppity" "wop"))))

;;; -------------------------------------------------------

(defun parse-input ()
  "Returns a ``Parser'' which succeeds if an input command follows,
   returning on confirmation in its ``Parse-Result'' a fresh
   ``Input-Node''."
  (build-parser (state)
    (.chain
      (.line (.sentence "So" "nervously" "he" "looks" "around"))
      (.return
        (make-instance 'Input-Node)))))

;;; -------------------------------------------------------

(defun parse-cell-output ()
  "Returns a ``Parser'' which succeeds if a cell output command follows,
   returning on confirmation in its ``Parse-Result'' a fresh
   ``Cell-Output-Node''."
  (build-parser (state)
    (.chain
      (.line (.sentence "In" "front" "of" "fans" "rappin'" "fast"))
      (.return
        (make-instance 'Print-Cell-Node)))))

;;; -------------------------------------------------------

(defun parse-string-output ()
  "Returns a ``Parser'' which succeeds if a string output command
   follows, returning on confirmation in its ``Parse-Result'' a fresh
   ``Print-String-Node''."
  (build-parser (state)
    (.line
      (.chain
        (.sentence "They're" "calling" "her")
        (.let (text
                (.or
                  (.chain
                    (.spaces)
                    (.let (characters (.many (.any-character)))
                      (declare (type list characters))
                      (.return
                        (coerce characters 'string))))
                 (.return "")))
          (declare (type string text))
          (.return
            (make-instance 'Print-String-Node :text text)))))))

;;; -------------------------------------------------------

(defun parse-move-right ()
  "Returns a ``Parser'' which succeeds if a cell pointer dextral
   translation command follows, returning on confirmation in its
   ``Parse-Result'' a fresh ``Move-Right-Node''."
  (the Parser
    (build-parser (state)
      (.chain
        (.line
          (.sentence "You're" "in" "class"))
        (.return
          (make-instance 'Move-Right-Node))))))

;;; -------------------------------------------------------

(defun parse-move-left ()
  "Returns a ``Parser'' which succeeds if a cell pointer sinistral
   translation command follows, returning on confirmation in its
   ``Parse-Result'' a fresh ``Move-Left-Node''."
  (the Parser
    (build-parser (state)
      (.chain
        (.line
          (.sentence "A" "few" "years" "later" "when" "now" "it's"
                     "prom" "season"))
        (.return
          (make-instance 'Move-Left-Node))))))

;;; -------------------------------------------------------

(defun parse-increment ()
  "Returns a ``Parser'' which succeeds if an incrementation command
   follows, returning on confirmation in its ``Parse-Result'' a fresh
   ``Increment-Node''."
  (the Parser
    (build-parser (state)
      (.chain
        (.line (.sentence "And" "he's" "always" "in" "his" "head"
                          "thinking" "too" "far" "ahead"))
        (.return
          (make-instance 'Increment-Node))))))

;;; -------------------------------------------------------

(defun parse-decrement ()
  "Returns a ``Parser'' which succeeds if a decrementation command
   follows, returning on confirmation in its ``Parse-Result'' a fresh
   ``Decrement-Node''."
  (the Parser
    (build-parser (state)
      (.chain
        (.line (.sentence "Don't" "be" "shy" "kid," "make" "your"
                          "own" "fate"))
        (.return
          (make-instance 'Decrement-Node))))))

;;; -------------------------------------------------------

(defun parse-loop ()
  "Returns a ``Parser'' which succeeds if a loop command follows,
   returning on confirmation in its ``Parse-Result'' a fresh
   ``Loop-Node''."
  (the Parser
    (build-parser (state)
      (.between
        (.line
          (.sentence "Even" "if" "you" "think" "you're" "bullet" "proof"))
        (.line
          (.sentence "I" "am" "glass" "and" "so" "are" "you"))
        (.let (body (parse-statements))
          (.return
            (make-instance 'Loop-Node :statements body)))))))

;;; -------------------------------------------------------

(defun parse-statement ()
  "Returns a ``Parser'' which succeeds if any command follows, returning
   on confirmation in its ``Parse-Result'' a ``Node'' representation of
   the parsed command."
  (the Parser
    (build-parser (state)
      (.or
        (parse-input)
        (parse-cell-output)
        (parse-string-output)
        (parse-move-left)
        (parse-move-right)
        (parse-increment)
        (parse-decrement)
        (parse-loop)))))

;;; -------------------------------------------------------

(defun parse-statements ()
  "Returns a ``Parser'' which succeeds if zero or more commands follow,
   returning on confirmation in its ``Parse-Result'' an ordered list of
   their ``Node'' representations."
  (the Parser
    (build-parser (state)
      (.many
        (parse-statement)))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Returns a ``Parser'' which succeeds if a valid Glassman program
   follows, returning on confirmation in its ``Parse-Result'' a fresh
   ``Program-Node'' representation thereof."
  (the Parser
    (build-parser (state)
      (.skip-empty-lines)
      (parse-start-script)
      (.let (statements (parse-statements))
        (declare (type (list-of T) statements))
        (.chain
          (parse-end-script)
          (.skip-empty-lines)
          (.eof)
          (.return
            (make-instance 'Program-Node :statements statements)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      memory-cells
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector represented by a hash table whose
                    keys maintain the cell indices, affiliating these
                    with the octets as the cell contents.")
   (pointer
    :initform      0
    :accessor      memory-pointer
    :type          integer
    :documentation "Designates the current cell by maintaining its index
                    (key) in the CELLS hash table."))
  (:documentation
    "The ``Memory'' class models the program memory as a bilaterally
     infinite tape of byte-valued cells, the current instance among
     these amenable to the cell pointer's reference."))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the octet
    (gethash (memory-pointer memory)
      (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceded by a wrapping of the same into the valid byte range
   [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory)
          (memory-cells memory) 0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one, upon transgression
   of the maximum wrapping around to the lower march, and returns no
   value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell value by one, upon transgression
   of the minimum wrapping around to the upper march, and returns no
   value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing tree.")
    :type          Node
    :documentation "The abstract syntax tree (AST) representation of the
                    Glassman program.")
   (memory
    :initform      (make-instance 'Memory)
    :accessor      interpreter-memory
    :type          Memory
    :documentation "The program memory."))
  (:documentation
    "The ``Interpreter'' class applies itself to the wike of
     accompassing veridical causata to a Glassman program committed in
     the form of an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defgeneric interpreter-visit-node (interpreter node)
  (:documentation
    "Processes the NODE in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (dolist (statement (program-node-statements node))
    (declare (type Node statement))
    (interpreter-visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Input-Node))
  (declare (type Interpreter interpreter))
  (declare (type Input-Node  node))
  (declare (ignore           node))
  (format T "~&>> ")
  (force-output)
  (setf (memory-current-cell
          (interpreter-memory interpreter))
        (char-code
          (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Print-Cell-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Print-Cell-Node node))
  (declare (ignore               node))
  (write-char
    (code-char
      (memory-current-cell
        (interpreter-memory interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Print-String-Node))
  (declare (type Interpreter       interpreter))
  (declare (type Print-String-Node node))
  (format T "~a"
    (print-string-node-text node))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Loop-Node))
  (declare (type Interpreter interpreter))
  (declare (type Loop-Node   node))
  (loop
    until
      (zerop (memory-current-cell (interpreter-memory interpreter)))
    do
      (dolist (statement (loop-node-statements node))
        (declare (type Node statement))
        (interpreter-visit-node interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Increment-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Increment-Node node))
  (memory-increment
    (interpreter-memory interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Decrement-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Decrement-Node node))
  (memory-decrement
    (interpreter-memory interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Move-Right-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Move-Right-Node node))
  (memory-move-right
    (interpreter-memory interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Move-Left-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Move-Left-Node node))
  (memory-move-left
    (interpreter-memory interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) representation of the
   parsed Glassman program in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (slot-value interpreter 'tree))
  (values))

;;; -------------------------------------------------------

(defun interpret-Glassman (code)
  "Interprets the piece of Glassman source CODE and returns no value."
  (declare (type string code))
  (let ((parse-result
          (parse
            (parse-program)
            (make-parse-state code 0))))
  (declare (type Parse-Result parse-result))
  (if (parse-result-succeeded-p parse-result)
    (interpreter-interpret
      (make-instance 'Interpreter :tree
        (parse-result-output parse-result)))
    (error "Parser failed.")))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-Glassman
  "Let's paint a picture
   They're calling her Hello, world!
   Woppity wop")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a
;; "null character" input.
(interpret-Glassman
  "
  Let's paint a picture
  And he's always in his head thinking too far ahead
  Even if you think you're bullet proof
      So nervously he looks around
      In front of fans rappin' fast
  I am glass and so are you
  Woppity wop
  ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The following pseudocode applies:
;; 
;;   memory[0] < input
;;   
;;   memory[1] <- 8
;;   
;;   { Decrement memory[0] (= input) by 48 to 0 or 1. }
;;   while memory[1] != 0 do
;;     memory[2] <- 6
;;     
;;     while memory[2] != 0 do
;;       memory[0] <- memory[0] - 1
;;     end while
;;     
;;     memory[1] <- memory[1] - 1
;;   end while
;;   
;;   while memory[0] != 0 do
;;     print "1"
;;   end while
;;   
;;   print "0"
(interpret-Glassman
  "
  Let's paint a picture
  
  So nervously he looks around
  
  A few years later when now it's prom season
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  And he's always in his head thinking too far ahead
  
  Even if you think you're bullet proof
    A few years later when now it's prom season
    And he's always in his head thinking too far ahead
    And he's always in his head thinking too far ahead
    And he's always in his head thinking too far ahead
    And he's always in his head thinking too far ahead
    And he's always in his head thinking too far ahead
    And he's always in his head thinking too far ahead
    
    Even if you think you're bullet proof
      You're in class
      You're in class
      Don't be shy kid, make your own fate
      A few years later when now it's prom season
      A few years later when now it's prom season
      Don't be shy kid, make your own fate
    I am glass and so are you
    
    You're in class
    Don't be shy kid, make your own fate
  I am glass and so are you
  
  You're in class
  
  Even if you think you're bullet proof
    They're calling her 1
  I am glass and so are you
  
  They're calling her 0
  
  Woppity wop
  ")
