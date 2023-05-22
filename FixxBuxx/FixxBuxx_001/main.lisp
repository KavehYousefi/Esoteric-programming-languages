;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "FixxBuxx", presented by the Esolang user "Username1234" in
;; the year 2022, and intended as an accommodable manifestation of the
;; popular "FizzBuzz" program.
;; 
;; 
;; Concept
;; =======
;; The FixxBuxx programming language provides a configurable instance of
;; the well-known FizzBuzz counting program, enabling user inputs,
;; modifications of the counter range, output conditions, messages on a
;; per-counter basis, as well as conditional execution in accordance
;; with the same liberty.
;; 
;; == Fizz buzz: A COUNTING GAME ==
;; The conceptual provenance of the FixxBuxx programming language
;; ultimately retreats to the "Fizz buzz" counting game, a sportive
;; warklume for the administration of nortelry anenst the arithmetic
;; division concept.
;; 
;; The students are required to count upwards from the number one (1),
;; responding in accord with the following principles in dependence upon
;; the current counter value:
;; 
;;   (1) If the counter constitutes an aliquot of 15, the text
;;       "fizz buzz" must be uttered.
;;   (2) If the counter constitutes an aliquot of 3, the text "fizz"
;;       must be uttered.
;;   (3) If the counter constitutes an aliquot of 5, the text "buzz"
;;       must be uttered.
;;   (4) In any other case, the counter value itself must be stated.
;; 
;; A failure to answer correctly or promptly inflicts the violating
;; participant with disqualification.
;; 
;; == FizzBuzz: Fizz buzz FOR COMPUTERS ==
;; Constructed upon the instructional nature of the Fizz buzz counting
;; game, the "FizzBuzz" program is appropriated especially for the
;; programmatic ambit, either serving as a medium for disports, or
;; committed to an epideictic wike, assigned to proving a language's
;; facilities.
;; 
;; This latter engagement may comprehend, among others, the following
;; criteria for a programming language:
;; 
;;   (a) The iteration construct, invested in particular with an
;;       inherent, or endowed by adscititious efforts with an advenient,
;;       counting mechanism.
;;   (b) Arithmetic operations, either directly capacitating the
;;       division and remainder calculations or indirectly allowing for
;;       such faculties' mimicry.
;;   (c) Conditionals, contributing the requisites for a distinguishment
;;       betwixt the various printing cases.
;;   (d) The output conduit, whose amenability to integer as well as
;;       string displays must be avered.
;; 
;; By default, the loop counter iterates from inclusive 1 to inclusive
;; 100, with increments of one; albeit other bournes and graduations do
;; not elude the participants' liberal establishments.
;; 
;; == FixxBuxx: AN ACCOMMODABLE FizzBuzz PROGRAM ==
;; The FixxBuxx programming language enhances a FizzBuzz program with
;; a multitude of adjustable elements.
;; 
;; Initialized with the standards of its cleronomy, counting from 1 to
;; 100, and printing the respective messages as a variable of its
;; remainders, a set of configurations are imported:
;; 
;;   (a) The iteration counter range can be specified, including any
;;       signed integer start value, potentially extending into an
;;       infinite upper march.
;;   (b) The aliquots for the "Fizz", "Buzz", and "FizzBuzz" may be
;;       adjusted. While the former two result from a direct statement,
;;       the desinent one is supputated as their least common multiple
;;       (LCM).
;;   (c) For each of the three standard texts, "Fizz", "Buzz", and
;;       "FizzBuzz", custom messages may be supplied.
;;   (d) With each specific counter value a dedicated text can be
;;       associated, superseding all other potential outputs.
;;   (e) With each specific counter value a sequence of zero or more
;;       commands may be affiliated, being executed immediately after
;;       the text output.
;;   (f) All integer specifiers are capacitated their substitution by
;;       user inputs.
;; 
;; 
;; Architecture
;; ============
;; Its simplicity does not legate any particular architecture to the
;; language's memory model, thus participating in a
;; counterdistinguishment from the elements of its versatile
;; configuration, a composition embracing mappings from counter states
;; to text strings and events, as well as strings as surrogates for the
;; traditional "Fizz", "Buzz", and "FizzBuzz" messages.
;; 
;; Merely two components' eligibility merits a mentioning in the
;; inquisitions into the configurations' stewardship: the aforementioned
;; mapping of counter values to output message and a second mapping of
;; the same indicators to instruction sequences.
;; 
;; Both associative instruments relate to the implementor's personal
;; deliberations, mandating, however, a unique correspondence from the
;; signed integer key to the value, which in the former case assumes a
;; string form, and the in the latter the internal instruction model,
;; most commonly as a hierarchy of abstract syntax tree (AST) nodes.
;; 
;; 
;; Data Types
;; ==========
;; The type system lent commorancy in FixxBuxx is compact of two
;; distinct members: signed integers of any magnitude, entailing as a
;; particular case the positive infinity, and strings with no constraint
;; regarding their length.
;; 
;; 
;; Syntax
;; ======
;; A rather complex rule governs the FixxBuxx syntaxis, with
;; peculiarities and particular components, including the provision of
;; arguments and the conditional significance of spaces.
;; 
;; == INSTRUCTIONS ==
;; Each non-empty line is either dedicated to a single instruction, an
;; annotation, or a conjunction of the former preceding the latter,
;; concluding with a linebreak at the desinence or the end of the
;; program itself.
;; 
;; == WHITESPACES ==
;; Whitespaces, bifurcating into the newlines and space species, the
;; latter of which amplects both the traditional space and the
;; horizontal tab, engage in affiliation with diverging agencies.
;; 
;; Newlines are only homologated as terminating entities for an
;; instruction line or a vacant expanse. Their employment in the
;; interstices inflicts the program with an error.
;; 
;; Spaces are administered tolerance in most cases, except for the
;; embedded arguments of the "Fi...Bu..." operation and those occasions
;; where strings condition a particular expectancy.
;; 
;; == COMMENTS ==
;; Comments are admitted to the language by adminiculum of the dedicated
;; annotation syntax, instigated via the closing parenthesis ")" and
;; always extending to the end of the current line. Their occasion may
;; constitute a successor to a preceding command, as well as an
;; otherwise vacant line's commorancy.
;; 
;; == GRAMMAR ==
;; A formulation of the language's donet in the Extended Backus-Naur
;; Form (EBNF) answers to the following expression:
;; 
;;   program            := statementList ;
;;   statementList      := [ line ] , { linebreak , [ line ] } ;
;;   line               := optionalSpaces
;;                      ,  [ statement ] ,
;;                      ,  optionalSpaces
;;                      ,  optionalAnnotation
;;                      ;
;;   statement          := FiBuCommand
;;                      |  FizzCommand
;;                      |  BuzzCommand
;;                      |  FizzBuzzCommand
;;                      |  rangeCommand
;;                      |  whenCommand
;;                      |  ifCommand
;;                      ;
;;   FiBuCommand        := "Fi" , finiteIntegerExpr
;;                      ,  "Bu" , finiteIntegerExpr ;
;;   FizzCommand        := "Fizz"     , arrow , string ;
;;   BuzzCommand        := "Buzz"     , arrow , string ;
;;   FizzBuzzCommand    := "FizzBuzz" , arrow , string ;
;;   rangeCommand       := "range"
;;                      ,  openParenthesis
;;                      ,  finiteIntegerExpr
;;                      ,  separator
;;                      ,  integerExpression
;;                      ,  closeParenthesis
;;                      ;
;;   whenCommand        := "when"
;;                      ,  openParenthesis
;;                      ,  finiteIntegerExpr
;;                      ,  closeParenthesis
;;                      ;
;;   ifCommand          := "if"
;;                      ,  openParenthesis
;;                      ,  finiteIntegerExpr
;;                      ,  closeParenthesis
;;                      ,  optionalSpaces
;;                      ,  "{"
;;                      ,  statementList
;;                      ,  "}"
;;                      ;
;;   
;;   arrow              := optionalSpaces , "->" , space ;
;;   separator          := optionalSpaces , "," , optionalSpaces ;
;;   openParenthesis    := "(" , optionalSpaces ;
;;   closeParenthesis   := optionalSpaces , ")" ;
;;   optionalAnnotation := [ annotation ];
;;   annotation         := ")" , { nonTerminator } ;
;;   string             := { character | escapedCharacter } ;
;;   escapedCharacter   := "\" , character ;
;;   nonTerminator      := character - ( ")" | "}" | EOF | linebreak ) ;
;;   integerExpression  := finiteIntegerExpression | "inf" ;
;;   finiteIntegerExpr  := integer | "in" ;
;;   integer            := [ "+" | "-" ] , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   linebreak          := "\n" ;
;;   optionalSpaces     := { space };
;;   space              := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; FixxBuxx's instruction set tallies a heptad of members, each either
;; directly involved in the printing operation or an indirect
;; participant by altering the counter range or injecting conditional
;; segments.
;; 
;; == OVERVIEW ==
;; A foundational nortelry shall be adhibited by the following apercu's
;; adminiculum.
;; 
;; Please note that variable portions are emphasized by an underline
;; compact of asterisks ("*"), describing the instruction parameters.
;; Any other content, encompassing spaces, ought to be construed in its
;; verbatim form.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   Fidiv1Budiv2      | Sets the Fizz-divisor, by default conditioning
;;     ****  ****      | an output of "Fizz", to {div1}, and the
;;                     | Buzz-divisor, traditionally affiliated with
;;                     | "Buzz", to {div2}. The FizzBuzz-divisor will
;;                     | be automatically calculated as
;;                     |   div3 = lcm(div1, div2)
;;                     | where lcm(x, y) constitutes the least common
;;                     | multiple of x and y.
;;                     | {div1} must be an integer.
;;                     | {div2} must be an integer.
;;   ..................................................................
;;   Fizz -> text      | If the iteration counter is an aliquot of the
;;           ****      | Fizz-divisor, traditionally associated with an
;;                     | output of "Fizz", the {text} will instead be
;;                     | printed.
;;                     | {text} must be a string.
;;   ..................................................................
;;   Buzz -> text      | If the iteration counter is an aliquot of the
;;           ****      | Buzz-divisor, traditionally associated with an
;;                     | output of "Buzz", the {text} will instead be
;;                     | printed.
;;                     | {text} must be a string.
;;   ..................................................................
;;   FizzBuzz -> text  | If the iteration counter is an aliquot of the
;;               ****  | FizzBuzz-divisor, traditionally associated
;;                     | with an output of "FizzBuzz", the {text} will
;;                     | instead be printed.
;;                     | {text} must be a string.
;;   ..................................................................
;;   range(min, max)   | Sets the iteration counter's inclusive start
;;         ***  ***    | value to {min} and its inclusive end value to.
;;                     | {max}.
;;                     | {min} must be an integer.
;;                     | {max} must be an integer or infinity.
;;   ..................................................................
;;   when(counter)text | If the iteration counter assumes the value
;;        ******* **** | {counter}, the {text} shall be issued.
;;                     | {counter} must be an integer.
;;                     | {text} must be a string.
;;                     | If in contest with a "Fizz", "Buzz", or
;;                     | "FizzBuzz" output, this text apprehends
;;                     | priority.
;;   ..................................................................
;;   if(counter){code} | If the iteration counter assumes the value
;;      *******  ****  | {counter}, the {code} shall be executed.
;;                     | {counter} must be an integer.
;;                     | {code} must be a sequence of zero or mores
;;                     | statements.
;;                     | The {code} execution succeeds the printing.
;;   ------------------------------------------------------------------
;; 
;; == SPECIAL VALUES: "INF" FOR INFINITY, "IN" FOR INPUT ==
;; In addition to the literal decimals, two particular instruments
;; participate as alternatives, endowing the language with a
;; capacitation for infinite repetitions and input reception: the
;; constant "inf" and the function "in".
;; 
;; The establishment of the "inf" constant, whose admission is exhausted
;; already by the second argument to the "range" operation, homologates
;; an infinitely repeating program loop.
;; 
;; The "in" function, acceptable in any circumstance involving an
;; integer literal or the infinity sentinel "inf", serves to query the
;; user for a signed integer in decimal format, whose value is
;; subsequently incorporated in lieu of the invocation.
;; 
;; == OUTPUT PRINCIPLES ==
;; Ensuing from the intricate relationships betwixt the configurations,
;; their influence unto a single program iteration's output is inflicted
;; with a high mete of perplexity.
;; 
;; A detailed treatise on the expected print result during such a cycle,
;; in reliance upon the loop's counter and the committed configuration,
;; shall be limned:
;; 
;;   (1) If the counter associates with a particular text, specified via
;;       the "when" command, the corresponding text is printed.
;;   (2) If the counter is an aliquot of the Fizz-divisor:
;;       (2.a) If an accommodated Fizz-text is defined, specified via
;;             the "Fizz" command, the corresponding text is printed.
;;       (2.b) Otherwise, the default message "Fizz" is printed.
;;   (3) If the counter is an aliquot of the Buzz-divisor:
;;       (3.a) If an accommodated Buzz-text is defined, specified via
;;             the "Buzz" command, the corresponding text is printed.
;;       (3.b) Otherwise, the default message "Buzz" is printed.
;;   (4) If the counter is an aliquot of the FizzBuzz-divisor:
;;       (4.a) If an accommodated FizzBuzz-text is defined, specified
;;             via the "FizzBuzz" command, the corresponding text is
;;             printed.
;;       (4.b) Otherwise, the default message "FizzBuzz" is printed.
;;   (5) In any other case, the counter is printed verbatim.
;; 
;; == ACCOMMODABLE PROGRAM LOOP ==
;; A FixxBuxx program's entirety executes in a contingently infinitely
;; repeating iteration, amenable to several adaptations.
;; 
;; A more explicit treatise shall now apply itself to the details,
;; empighted as a piece of pseudocode, whose essential variables ought
;; to be limned in an incipient effort.
;; 
;; The following definitions in the ultimate exuction are begotten from
;; the involved instructions. Please note the "lcm" function, defined as
;; 
;;   lcm(x, y)
;; 
;; which returns the least common multiple (LCM) of the integer inputs x
;; and y.
;; 
;;   ------------------------------------------------------------------
;;   Variable               | Source of definition          | Default
;;   -----------------------+-------------------------------+----------
;;   counterStart           | range(start, ...)             | 1
;;   ..................................................................
;;   counterEnd             | range(...,   end)             | 100
;;   ..................................................................
;;   fizzDivisor            | FifizzDivisorBu...            | 3
;;   ..................................................................
;;   buzzDivisor            | Fi...........BubuzzDivisor    | 5
;;   ..................................................................
;;   fizzBuzzDivisor        | lcm(fizzDivisor, buzzDivisor) | 15
;;   ..................................................................
;;   fizzText               | Fizz -> fizzText              | Fizz
;;   ..................................................................
;;   buzzText               | Buzz -> buzzText              | Buzz
;;   ..................................................................
;;   fizzBuzzText           | FizzBuzz -> fizzBuzzText      | FizzBuzz
;;   ..................................................................
;;   whenTable(counter)     | when(counter)text             | -
;;   ..................................................................
;;   ifTable(counter){code} | if(counter){code}             | -
;;   ------------------------------------------------------------------
;; 
;; Proceeding from the above definitions, the following principle holds
;; for the FixxBuxx program loop:
;; 
;;   for counter from {counterStart} to {counterEnd} do
;;     if whenTable(counter) is defined then
;;       print the string for whenTable(counter)
;;     else if counter mod {fizzbuzzDivisor} then
;;       print {fizzbuzzText}
;;     else if counter mod {fizzDivisor}
;;       print {fizzText}
;;     else if counter mode {buzzDivisor}
;;       print {buzzText}
;;     else
;;       print counter
;;     end if
;;     
;;     print linebreak
;;     
;;     if ifTable(counter) is defined then
;;       execute the code associated with ifTable(counter)
;;     end if
;;   end for
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The FixxBuxx protolog, maugre its enjoyment of elucidations and
;; examples, is inflicted with a few ambiguities, a subset of which
;; shall be the following sections' cynosure.
;; 
;; == HOW ARE WHITESPACES ADDRESSED? ==
;; The specification's preponderance employs whitespaces --- especially
;; non-linebreak spaces --- in an irregular fashion, and abstains from
;; their instalment in the interstices betwixt command or argument
;; tokens and parenthesis or braces. The pattern first adduced in the
;; commands sections propagates with unperturbed stringency into the
;; examples, with the "range" operation's parameter list contributing
;; the sole divergence by omitting the space succeeding the comma
;; betwixt its argument twain in an example. Hence, the question arises
;; whether the distribution of spaces in concord with the instructional
;; syntax imposes an obligation.
;; 
;; It has been adjudged that spaces, embracing in their diorism the
;; space and the horizontal tab, may be employed liberally in all cases
;; immune to ambivalencies. The perplexing aspects merely arise in the
;; strings' vicinage, where any content is invested with significance;
;; as a corollary, in the following context the space characters are
;; apportioned enhanced scrutiny:
;; 
;;   (1) In the command "Fi...Bu...", no spaces are homologated.
;;   (2) In the commands "Fizz", "Buzz", and "FizzBuzz", a single space
;;       must succeed the arrow "->". Any content following this segment
;;       is construed verbatim as a string.
;;       No space constitutes a requisitum betwixt the command
;;       identifier and the arrow, but may be present as an option.
;;   (2) In the command "when", the closing parenthesis ")" is succeeded
;;       by literal content; that is, any character, including spaces,
;;       coalesces into a coherent string. The ")" should therefore not
;;       be separated from the string argument by such a sepiment, least
;;       it registers the intention as a string prefix.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes directly from a source string's characters.
;; 
;; == LEXER AND PARSER COALESCE ==
;; The realization of the complete FixxBuxx code processing bifurcates
;; into a twain --- rather than a treble --- of stages, coalescing the
;; lexical analyzation, usually a lexer's bailiwick, with the parsing
;; step, with immediacy succeeded by the actual interpretation.
;; 
;; The confluence's encheson originates from the kenspeckle syntax
;; appertaining to FixxBuxx, and the imposition of an experimental
;; character that ensues therefrom.
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
;; Date:   2023-05-22
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
;;   [esolang2022FizzBuzz]
;;   The Esolang contributors, "FizzBuzz", 2022
;;   URL: "https://esolangs.org/wiki/FizzBuzz"
;;   Notes:
;;     - Describes the "FizzBuzz" program.
;;   
;;   [esolang2022FixxBuxx]
;;   The Esolang contributors, "FixxBuxx", 2022
;;   URL: "https://esolangs.org/wiki/FixxBuxx"
;;   Notes:
;;     - Specification of the "FixxBuxx" programming language.
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
;;   [wikipedia2023fizzbuzz]
;;   The Wikipedia contributors, "Fizz buzz", 2023
;;   URL: "https://en.wikipedia.org/wiki/Fizz_buzz"
;;   Notes:
;;     - Describes the counting game "Fizz buzz".
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
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype parselet ()
  "The ``parselet'' type defines the actual operative unit of a parser
   or combinator as a function which accepts a ``Parse-State'' and
   returns a ``Parse-Result''."
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered sequence of abstract syntax
   tree (AST) subtrees as a simple list of ``Node'' objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype fb-integer ()
  "The ``fb-integer'' type defines the interpretation of the integral
   number type in FixxBuxx as either a signed integer of any magnitude,
   or the special ``:infinity'' keyword symbol which designates the
   positive infinity."
  '(or integer (eql :infinity)))

;;; -------------------------------------------------------

(deftype output-mode ()
  "The ``output-mode'' type enumerates the possible printing variants
   which a FizzBuzz program might, with dependence upon the current
   iteration counter's state, issue.
   ---
   The following correspondences hold with respect to the canonical
   FizzBuzz variant:
   
     ------------------------------------------------------------------
     Counter state | FizzBuzz output        | ``output-mode'' symbol
     --------------+------------------------+--------------------------
     Aliquot of  3 | \"Fizz\"               | :fizz
     ..................................................................
     Aliquot of  5 | \"Buzz\"               | :buzz
     ..................................................................
     Aliquot of 15 | \"FizzBuzz\"           | :fizzbuzz
     ..................................................................
     Otherwise     | Verbatim counter state | :counter
     ------------------------------------------------------------------"
  '(member :fizz :buzz :fizzbuzz :counter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-parse-state (source &optional (cursor 0)))
  (:copier      NIL))
  "The ``Parse-State'' class encapsulates the information necessitated
   for advancing the parsing process, embracing the FixxBuxx source code
   in its string form and a reference to the currently indagated
   position in the same."
  (source (error "Missing source.") :type string)
  (cursor 0                         :type fixnum))

;;; -------------------------------------------------------

(defun parse-state-current-character (state)
  "Returns the character in the parse STATE's source under its position
   cursor, or ``NIL'' if the same has exceeded the boundaries."
  (declare (type Parse-State state))
  (the (or null character)
    (when (array-in-bounds-p (parse-state-source state)
            (parse-state-cursor state))
      (char
        (parse-state-source state)
        (parse-state-cursor state)))))

;;; -------------------------------------------------------

(defun parse-state-advance (state)
  "Moves the parse STATE's position cursor to the next location in its
   source and returns the modified STATE."
  (declare (type Parse-State state))
  (incf (parse-state-cursor state))
  (the Parse-State state))

;;; -------------------------------------------------------

(defun parse-state-copy (template)
  "Returns a copy of the TEMPLATE parse state, retaining a reference to
   its source, but cloning its cursor."
  (declare (type Parse-State template))
  (the Parse-State
    (make-parse-state
      (parse-state-source template)
      (parse-state-cursor template))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-successful-parse-result
    (state data &aux (succeeded-p T)))
  (:constructor make-failed-parse-result
    (&optional (state NIL) (data NIL) &aux (succeeded-p NIL))))
  "The ``Parse-Result'' class serves to encapsulate the response of a
   parser's or combinator's application unto a parse state,
   communicating whether the request has been successful, the parse
   state being probed in the process, and an optional piece of data
   acting as the actual parser output, for instance an abstract syntax
   tree (AST) node, especially in the case of success."
  (state       NIL :type (or null Parse-State))
  (data        NIL :type T)
  (succeeded-p NIL :type boolean))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (parselet)))
  "The ``Parser'' class provides a parser or combinator, employing an
   internal callback function, the \"parselet\", for the processing of
   the current state and the response entailing the resulting data item
   in conjunction with a new state and a success flag."
  (parselet (error "Missing parselet.") :type parselet))

;;; -------------------------------------------------------

(defun parser-parse (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   representing its response attempt at processing its input."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall (parser-parselet parser) state)))

;;; -------------------------------------------------------

(defun accept-state (predicate)
  "Returns a new parser which succeeds if the PREDICATE, probing an
   input parse state, is fulfilled, returning a parse result which in
   the positive case comprehends the input state's character and a new
   parse state derived by advancing the input, otherwise responding with
   a failed state."
  (declare (type (function (Parse-State) *) predicate))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (if (funcall predicate state)
              (make-successful-parse-result
                (parse-state-advance (parse-state-copy state))
                (parse-state-current-character state))
              (make-failed-parse-result state NIL)))))))

;;; -------------------------------------------------------

(defun accept-character (predicate)
  "Returns a new parser which succeeds if the PREDICATE, probing the
   input parse state's current character, or ``NIL'' if having
   progressed beyond its source, is fulfilled, returning a parse result
   which in the positive case comprehends the input state's character
   and a new parse state derived by advancing the input, otherwise
   responding with a failed state."
  (declare (type (function ((or null character)) *) predicate))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (if (funcall predicate (parse-state-current-character state))
              (make-successful-parse-result
                (parse-state-advance (parse-state-copy state))
                (parse-state-current-character state))
              (make-failed-parse-result state NIL)))))))

;;; -------------------------------------------------------

(defun .character (expected-character)
  "Returns a new parser which succeeds if the current character equals
   the EXPECTED-CHARACTER, respecting the case during the
   juxtaposition."
  (declare (type character expected-character))
  (the Parser
    (accept-state
      #'(lambda (state)
          (declare (type Parse-State state))
          (the boolean
            (not (null
              (and (parse-state-current-character state)
                   (char= (parse-state-current-character state)
                          expected-character)))))))))

;;; -------------------------------------------------------

(defun .digit (&optional (radix 10))
  "Returns a ew parser which succeeds if the current character can be
   interpreted as a digit in the RADIX, defaulting to the base-10
   decimal system, in that case encapsulating the character in its
   result."
  (declare (type (integer 2 36) radix))
  (the Parser
    (accept-character
      #'(lambda (candidate)
          (declare (type (or null character) candidate))
          (the boolean
            (not (null
              (and candidate
                   (digit-char-p candidate radix)))))))))

;;; -------------------------------------------------------

(defun .space ()
  "Returns a new parser which succeeds if the current character
   represents a space or horizontal tab, in that case encapsulating the
   consumed space in its result."
  (the Parser
    (accept-character
      #'(lambda (character)
          (declare (type (or null character) character))
          (the boolean
            (not (null
              (and character
                   (member character
                     '(#\Space #\Tab) :test #'char=)))))))))

;;; -------------------------------------------------------

(defun .newline ()
  "Returns a new parser which succeeds if the current character
   represents a linebreak, in that case encapsulating the consumed
   newline in its result."
  (the Parser
    (accept-character
      #'(lambda (character)
          (declare (type (or null character) character))
          (the boolean
            (not (null
              (and character
                   (member character
                     '(#\Newline) :test #'char=)))))))))

;;; -------------------------------------------------------

(defun .any-character ()
  "Returns a parser which succeeds for any non-``NIL'' character, in
   that encapsulating the consumed character in its result."
  (the Parser
    (accept-character
      #'(lambda (character)
          (declare (type (or null character) character))
          (the boolean
            (not (null character)))))))

;;; -------------------------------------------------------

(defun .eof ()
  "Returns a parser which succeeds upon the source's exhaustion, that
   is, if the position cursor violates the source's bournes, in that
   case encapsulating the ``NIL'' value in its result."
  (the Parser
    (accept-character
      #'(lambda (candidate)
          (declare (type (or null character) candidate))
          (the boolean
            (null candidate))))))

;;; -------------------------------------------------------

(defun .or (&rest alternatives)
  "Returns a new parser which succeeds if at least one of its
   ALTERNATIVES matches, probed in the specified order, and returns in
   its result the first matching parser's data item."
  (declare (type (list-of Parser) alternatives))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for current-alternative
                of-type Parser
                in      alternatives
              for result
                of-type Parse-Result
                =       (parser-parse current-alternative state)
              ;; The CURRENT-ALTERNATIVE matches?
              ;; => Returns a successful result with its state and data.
              ;; => Terminate the loops immediately, as none of the
              ;;    subsequent ALTERNATIVES are of interest.
              do
                (when (parse-result-succeeded-p result)
                  (return
                    (make-successful-parse-result
                      (parse-result-state result)
                      (parse-result-data  result))))
              ;; No matching alternative found?
              ;; => Returns a failed result.
              finally
                (return
                  (make-failed-parse-result state))))))))

;;; -------------------------------------------------------

(defun .bind (predicate parser-generator)
  "Returns a new parser which realizes a monadic binding, testing the
   PREDICATE parser for a match, and, if succeding, invoking the
   PARSER-GENERATOR function with the PREDICATE result's data item,
   obtaining a second parser that is applied to the PREDICATE result's
   parse state, on its affirmation returning a successful parse result
   comprehending the generated parser's data item."
  (declare (type Parser                predicate))
  (declare (type (function (*) Parser) parser-generator))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((predicate-result (parser-parse predicate state)))
            (declare (type Parse-Result predicate-result))
            (the Parse-Result
              (if (parse-result-succeeded-p predicate-result)
                (parser-parse
                  (funcall parser-generator
                    (parse-result-data predicate-result))
                  (parse-result-state predicate-result))
                (make-failed-parse-result
                  (parse-result-state predicate-result)
                  (parse-result-data  predicate-result)))))))))

;;; -------------------------------------------------------

(defmacro .let ((action-input-variable predicate)
                &body body)
  "Returns a new parser which succeeds if PREDICATE parser matches, in
   this case binding the PREDICATE result's data item to the
   ACTION-INPUT-VARIABLE and evaluating the BODY forms, expecting the
   desinent form to produce a parser."
  `(.bind
     ,predicate
     #'(lambda (,action-input-variable)
         (declare (type T    ,action-input-variable))
         (declare (ignorable ,action-input-variable))
         ,@body)))

;;; -------------------------------------------------------

(defmacro .let* ((first-binding &rest additional-bindings)
                 &body body)
  "Creates a new parser by attempting to match all parsers in the
   bindings composed of the FIRST-BINDING and the ADDITIONAL-BINDINGS,
   in this order, evaluates the BODY forms, and expects the desinent
   form to produce a parser."
  (if (null additional-bindings)
    `(.let ,first-binding
       ,@body)
    `(.let ,first-binding
       (.let* ,additional-bindings
         ,@body))))

;;; -------------------------------------------------------

(defun .return (value)
  "Returns a new parser which always succeeds, returning in its result
   the VALUE, but consuming no characters from the parse state."
  (declare (type T value))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (make-successful-parse-result state value))))))

;;; -------------------------------------------------------

(defun .optional (parser &optional (default NIL))
  "Returns a new parser which always succeeds, returning for a success
   input PARSER its result, otherwise responding with a new successful
   parse result comprehending the DEFAULT value in its data
   compartment."
  (declare (type Parser parser))
  (declare (type T      default))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((result (parser-parse parser state)))
            (declare (type Parse-Result result))
            (the Parse-Result
              (if (parse-result-succeeded-p result)
                result
                (make-successful-parse-result state default))))))))

;;; -------------------------------------------------------

(defun .one-or-more (filter)
  "Returns a new parser which succeeds if one or more occurrences of the
   FILTER parser match in immediate succession."
  (declare (type Parser filter))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((first-result (parser-parse filter state)))
            (declare (type Parse-Result first-result))
            (the Parse-Result
              (if (parse-result-succeeded-p first-result)
                (let ((new-state  (parse-result-state first-result))
                      (data-items NIL))
                  (declare (type Parse-State new-state))
                  (declare (type (list-of T) data-items))
                  (push (parse-result-data first-result) data-items)
                  (loop
                    for current-result
                      of-type Parse-Result
                      =       (parser-parse filter new-state)
                    while (parse-result-succeeded-p current-result)
                    do
                      (setf new-state
                        (parse-result-state current-result))
                      (push (parse-result-data current-result)
                        data-items)
                    finally
                      (return
                        (make-successful-parse-result new-state
                          (nreverse data-items)))))
                first-result)))))))

;;; -------------------------------------------------------

(defun .zero-or-more (filter)
  "Returns a new parser which succeeds if zero or more occurrences of
   the FILTER parser match in immediate succession."
  (declare (type Parser filter))
  (the Parser
    (.optional
      (.one-or-more filter)
      NIL)))

;;; -------------------------------------------------------

(defun .many-until (parser end)
  "Returns a new parser which succeeds if zero or more occurrences of
   the specified PARSER match in immediate succession, followed by an
   instance of the END parser that is probed but not consumed."
  (declare (type Parser parser))
  (declare (type Parser end))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (let ((new-state  state)
                  (data-items NIL))
              (declare (type Parse-State state))
              (declare (type (list-of T) data-items))
              (loop
                for end-result
                  of-type Parse-Result
                  =       (parser-parse end new-state)
                until (parse-result-succeeded-p end-result)
                do
                  (let ((parser-result
                          (parser-parse parser new-state)))
                    (declare (type Parse-Result parser-result))
                    (cond
                      ((parse-result-succeeded-p parser-result)
                        (push (parse-result-data parser-result)
                              data-items)
                        (setf new-state
                          (parse-result-state parser-result)))
                      (T
                        (return
                          (make-failed-parse-result new-state
                            (parse-result-data parser-result))))))
                finally
                  (return
                    (make-successful-parse-result new-state
                      (nreverse data-items))))))))))

;;; -------------------------------------------------------

(defun .chain (&rest parsers)
  "Returns a new parser which succeeds if and only if all of its
   PARSERS, in the specified order, succeeds, only the desinent
   parser's result."
  (declare (type (list-of Parser) parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (cond
              ((null parsers)
                (make-successful-parse-result state NIL))
              (T
                (let ((new-state state))
                  (declare (type Parse-State new-state))
                  (loop
                    for current-parser
                      of-type Parser
                      in      parsers
                    for current-result
                      of-type Parse-Result
                      =       (parser-parse current-parser new-state)
                    do
                      (if (parse-result-succeeded-p current-result)
                        (setf new-state
                          (parse-result-state current-result))
                        (return current-result))
                    finally
                      (return current-result))))))))))

;;; -------------------------------------------------------

(defun .between (open-guard close-guard parser)
  "Returns a new parser which succeeds if and only if the OPEN-GUARD
   parser, the intermediate PARSER, and the CLOSE-GUARD parser, in this
   order, succeed, returning only the PARSER's result."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser parser))
  (the Parser
    (.chain open-guard
      (.let (parser-output parser)
        (.chain close-guard
          (.return parser-output))))))

;;; -------------------------------------------------------

(defun .before (captured-parser suffix)
  "Returns a new parser which succeeds if the CAPTURED-PARSER and the
   SUFFIX parser match in this order, returning in its result the
   CAPTURED-PARSER result's data item while omitting the SUFFIX'."
  (declare (type Parser captured-parser))
  (declare (type Parser suffix))
  (the Parser
    (.let (parser-output captured-parser)
      (.chain suffix
        (.return parser-output)))))

;;; -------------------------------------------------------

(defun .one-or-more-separated-by (parser separator)
  "Returns a new parser which succeeds if one or more instances of the
   PARSER exist, with each twain accompanied by exactly one SEPARATOR
   instance in the intermede."
  (declare (type Parser parser))
  (declare (type Parser separator))
  (the Parser
    (.let (first-data-item parser)
      (declare (type T first-data-item))
      (.let (subsequent-data-items
              (.zero-or-more
                (.chain separator parser)))
        (declare (type (list-of T) subsequent-data-items))
        (.return (cons first-data-item subsequent-data-items))))))

;;; -------------------------------------------------------

(defun .zero-or-more-separated-by (parser separator)
  "Returns a new parser which succeeds if zero or more instances of the
   input PARSER follow, each twain being connected by one instance of
   the SEPARATOR, entailing in the successful result a list of the
   PARSER outputs in their correct order."
  (declare (type Parser parser))
  (declare (type Parser separator))
  (the Parser
    (.optional
      (.one-or-more-separated-by parser separator)
      NIL)))

;;; -------------------------------------------------------

(defun .all-separated-by (separator &rest parsers)
  "Returns a new parser which succeeds if all PARSERS match, with each
   twain of these connected by the SEPARATOR, upon its application
   returning in its result a list comprehending all the collected data
   items of the PARSERS in their specified order.
   ---
   An empty PARSERS input always succeeds, returning the ``NIL'' value."
  (declare (type Parser           separator))
  (declare (type (list-of Parser) parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (if parsers
              (let ((data-items   NIL)
                    (first-result (parser-parse (first parsers) state)))
                (declare (type (list-of T)  data-items))
                (declare (type Parse-Result first-result))
                (if (parse-result-succeeded-p first-result)
                  (let ((new-state (parse-result-state first-result)))
                    (declare (type Parse-State new-state))
                    (push (parse-result-data first-result) data-items)
                    (loop
                      for consequent-parser
                        of-type Parser
                        in      (rest parsers)
                      for consequent-result
                        of-type Parse-Result
                        =       (parser-parse
                                  (.chain separator consequent-parser)
                                  new-state)
                      do
                        (cond
                          ((parse-result-succeeded-p consequent-result)
                            (push (parse-result-data consequent-result)
                                  data-items)
                            (setf new-state
                              (parse-result-state consequent-result)))
                          (T
                            (return
                              (make-failed-parse-result new-state NIL))))
                      finally
                        (return
                          (make-successful-parse-result new-state
                            (nreverse data-items)))))
                  ;; First parser has already failed.
                  first-result))
              ;; No parsers?
              ;; => Always succeed.
              (make-successful-parse-result state NIL)))))))

;;; -------------------------------------------------------

(defun .skip-one (parser)
  "Returns a new parser which succeeds if the input PARSER matches,
   returning the ``NIL'' value in the result."
  (declare (type Parser parser))
  (the Parser
    (.chain parser
      (.return NIL))))

;;; -------------------------------------------------------

(defun .skip-one-or-more (parser)
  "Returns a new parser which succeeds if the input PARSER matches one
   or more times, returning the ``NIL'' in the result."
  (declare (type Parser parser))
  (the Parser
    (.chain
      (.one-or-more parser)
      (.return NIL))))

;;; -------------------------------------------------------

(defun .skip-zero-or-more (parser)
  "Returns a new parser which succeeds if the input PARSER matches zero
   or more times, returning the ``NIL'' in the result."
  (declare (type Parser parser))
  (the Parser
    (.optional
      (.skip-one-or-more parser)
      NIL)))

;;; -------------------------------------------------------

(defmacro define-niladic-parser-function (name &body parsers)
  "Defines a function of no arguments which upon its invocation returns
   a parser obtained by chaining the specified PARSERS, with the
   desinent member's result constituting the new parser's own result."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    `(defun ,name ()
       (the Parser
         (make-parser
           #'(lambda (,state-variable)
               (declare (type Parse-State ,state-variable))
               (declare (ignorable        ,state-variable))
               (the Parse-Result
                 (parser-parse (.chain ,@parsers)
                   ,state-variable))))))))

;;; -------------------------------------------------------

(defun declaration-p (form)
  "Determines whether the FORM represents a declaration, that is, a
   list introduced by the symbol ``declare'', returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T form))
  (the boolean
    (not (null
      (and form
           (listp   form)
           (symbolp (first form))
           (eq      (first form) 'declare))))))

;;; -------------------------------------------------------

(defun extract-form-sections (forms)
  "Extracts from the FORMS its pertinent sections and returns three
   values as their representations:
     (1) a documentation string, or ``NIL'' if no retrievable
     (2) a list of zero or more declarations
     (3) a list comprehending the remaining FORMS elements not excised
         through the steps producing the values for (1) and (2)."
  (declare (type list forms))
  (let ((remaining-forms (copy-list forms))
        (documentation   NIL)
        (declarations    NIL)
        (body            NIL))
    (declare (type list             remaining-forms))
    (declare (type (or null string) documentation))
    (declare (type list             declarations))
    (declare (type list             body))
    
    ;; Check for a documentation string as the first FORMS element.
    (let ((first-form (first forms)))
      (when (and first-form (stringp first-form))
        (setf documentation first-form)
        (pop remaining-forms)))
    
    ;; Checks for zero or more adjacent ``(declare ...)'' forms.
    (loop do
      (let ((form (first remaining-forms)))
        (if (declaration-p form)
          (push (pop remaining-forms) declarations)
          (loop-finish))))
    
    ;; Retain the remaining forms as the uncategorized body.
    (setf body remaining-forms)
    
    (the (values (or null string) list list)
      (values documentation (nreverse declarations) body))))

;;; -------------------------------------------------------

(defmacro define-parser-function (name (&rest parameters)
                                  &body body)
  "Establishes a function dependent upon the formal parameters and
   agnominated by the specified NAME which, when invoked, creates a new
   parser composed of the chained PARSERS, that is, the thus produced
   parser only succeeds if all of its constituents in this order match,
   otherwise failing."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    (multiple-value-bind (documentation declarations parsers)
        (extract-form-sections body)
      (declare (type (or null string) documentation))
      (declare (type list             declarations))
      (declare (type list             parsers))
      `(defun ,name (,@parameters)
         ,(or documentation "")
         ,@declarations
         (the Parser
           (make-parser
             #'(lambda (,state-variable)
                 (declare (type Parse-State ,state-variable))
                 (the Parse-Result
                   (parser-parse
                     (.chain ,@parsers) ,state-variable)))))))))

;;; -------------------------------------------------------

(defun parse (parser source)
  "Orders the PARSER to process the SOURCE, on success returning the
   parser's result, otherwise signaling an error of an unspecified
   type."
  (declare (type Parser parser))
  (declare (type string source))
  (let ((initial-state (make-parse-state source 0)))
    (declare (type Parse-State initial-state))
    (let ((result (parser-parse parser initial-state)))
      (declare (type Parse-Result result))
      (if (parse-result-succeeded-p result)
        (parse-result-data result)
        (error "~&PARSER FAILED with state ~s."
          (parse-result-state result))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of AST nodes.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "Missing node type.")
    :reader        node-type
    :type          keyword
    :documentation "The categorizing type of this node, the castaldy of
                    which constitutes an example of supererogation in
                    this project, and a hint to the general approaches
                    perpetuated in most implementations."))
  (:documentation
    "The ``Node'' interface provides a common base for all abstract
     syntax tree (AST) nodes, each a representative of a particular
     FixxBuxx language facility."))

;;; -------------------------------------------------------

(defclass Literal-Node (Node)
  ((value-type
    :initarg       :value-type
    :initform      (error "Missing value type.")
    :reader        literal-node-value-type
    :type          (member :integer :string)
    :documentation "The data type associated with the literal.")
   (value
    :initarg       :value
    :initform      (error "Missing value.")
    :reader        literal-node-value
    :type          (or integer string)
    :documentation "The value of the literal."))
  (:default-initargs
    :type :literal)
  (:documentation
    "The ``Literal-Node'' class serves to encapsulate a literal object,
     that is, either a signed decimal integer or a string."))

;;; -------------------------------------------------------

(defclass Infinity-Node (Node)
  ()
  (:default-initargs
    :type :infinity)
  (:documentation
    "The ``Infinity-Node'' class represents the positive infinity,
     answering to the FixxBuxx constant \"Inf\"."))

;;; -------------------------------------------------------

(defclass Input-Node (Node)
  ()
  (:default-initargs
    :type :input)
  (:documentation
    "The ``Input-Node'' class represents an input command, corresponding
     to the ``FixxBuxx'' instruction \"in\"."))

;;; -------------------------------------------------------

(defclass Divisors-Node (Node)
  ((fizz-divisor
    :initarg       :fizz-divisor
    :initform      (error "Missing Fizz divisor.")
    :reader        divisors-node-fizz-divisor
    :type          Node
    :documentation "The new aliquot required for generating a \"Fizz\"
                    or its substitute text.
                    ---
                    The standard FizzBuzz program employs the integer
                    three (3).")
   (buzz-divisor
    :initarg       :buzz-divisor
    :initform      (error "Missing Buzz divisor.")
    :reader        divisors-node-buzz-divisor
    :type          Node
    :documentation "The new aliquot required for generating a \"Buzz\"
                    or its substitute text.
                    ---
                    The standard FizzBuzz program employs the integer
                    three (5)."))
  (:default-initargs
    :type :divisors)
  (:documentation
    "The ``Divisors-Node'' class serves to encapsulate the custom
     divisors which, when ascertained as aliquots for the program loop's
     counter, either generate the \"Fizz\"-message, \"Buzz\"-message, or
     \"FizzBuzz\"-message."))

;;; -------------------------------------------------------

(defclass Text-Replacement-Node (Node)
  ((target
    :initarg       :target
    :initform      (error "Missing replacement target.")
    :reader        text-replacement-node-target
    :type          (member :fizz :buzz :fizzbuzz)
    :documentation "Determines whether this node defines a substitution
                    of the standard text \"Fizz\", \"Buzz\", or
                    \"FizzBuzz\".")
   (substitute
    :initarg       :substitute
    :initform      (error "Missing replacement substitute.")
    :reader        text-replacement-node-substitute
    :type          Literal-Node
    :documentation "The text to print instead of \"Fizz\", \"Buzz\", or
                    \"FizzBuzz\"."))
  (:default-initargs
    :type :text-replacement)
  (:documentation
    "The ``Text-Replacement-Node'' class serves to encapsulates the
     replacement texts for any of the treble standard messages \"Fizz\",
     \"Buzz\", and \"FizzBuzz\", thus providing a representation of the
     FixxBuxx \"Fi...Bu...\" command."))

;;; -------------------------------------------------------

(defclass Range-Node (Node)
  ((start
    :initarg       :start
    :initform      (error "Missing start.")
    :reader        range-node-start
    :type          Node
    :documentation "The start value value to assumbe by the counter.")
   (end
    :initarg       :end
    :initform      (error "Missing end.")
    :reader        range-node-end
    :type          Node
    :documentation "The end value to assume by the counter."))
  (:default-initargs
    :type :range)
  (:documentation
    "The ``Range-Node'' class serves to encapsulate the information
     requisite to replicate a FixxBuxx \"range(...)\" command, compact
     of a counter start and end indicator."))

;;; -------------------------------------------------------

(defclass When-Node (Node)
  ((counter
    :initarg       :counter
    :initform      (error "Missing counter.")
    :reader        when-node-counter
    :type          Node
    :documentation "The \"when\" predicate or antecedent, that is, the
                    counter value to replace by the SUBSTITUTE text.")
   (substitute
    :initarg       :substitute
    :initform      (error "Missing substitute.")
    :reader        when-node-substitute
    :type          Node
    :documentation "The \"when\" action, that is, the string to print in
                    lieu of the superseded counter (see ANTECEDENT)."))
  (:default-initargs
    :type :when)
  (:documentation
    "The ``When-Node'' class represents an abstract syntax tree (AST)
     node serving in encapsulating a FixxBuxx \"when\" command
     invocation.
     ---
     The \"when\" commands permits the redefinition of the output issued
     if the iteration should print the counter verbatim, that is,
     neither a \"Fizz\", \"Buzz\", nor a \"FizzBuzz\" shall be
     printed."))

;;; -------------------------------------------------------

(defclass Block-Node (Node)
  ((statements
    :initarg       :statements
    :initform      NIL
    :reader        block-node-statements
    :type          node-list
    :documentation "The statements comprising the block."))
  (:default-initargs
    :type :block)
  (:documentation
    "The ``Block-Node'' class ensconces a sequence of zero or more
     commands, themselves modeled by nodes."))

;;; -------------------------------------------------------

(defclass If-Node (Node)
  ((counter
    :initarg       :counter
    :initform      (error "Missing counter.")
    :reader        if-node-counter
    :type          Node
    :documentation "The \"when\" predicate or antecedent, that is, the
                    counter value whose occasion shall instigate the
                    STATEMENTS.")
   (statements
    :initarg       :statements
    :initform      (error "Missing statement block.")
    :reader        if-node-statements
    :type          Block-Node
    :documentation "The statements to be executed if the COUNTER value
                    is reached in the FixxBuxx program loop."))
  (:default-initargs
    :type :if)
  (:documentation
    "The ``If-Node'' class represents an abstract syntax tree (AST) node
     serving in encapsulating a FixxBuxx \"if\" command invocation.
     ---
     The \"if\" commands permits the association an arbitrary series of
     instructions with a specific counter value, executing the same
     immediately after the respective output operation, thus offering a
     conditional execution facility."))

;;; -------------------------------------------------------

(defclass No-Op-Node (Node)
  ()
  (:default-initargs
    :type :no-op)
  (:documentation
    "The ``No-Op-Node'' class represents a null operation, most commonly
     generated in order to cover a FixxBuxx annotation command
     \")...\"."))

;;; -------------------------------------------------------

(defclass Program-Node (Node)
  ((statements
    :initarg       :statements
    :initform      (error "Missing program statements.")
    :reader        program-node-statements
    :type          node-list
    :documentation "The program statements."))
  (:default-initargs
    :type :program)
  (:documentation
    "The ``Program-Node'' class serves as the root node of an abstract
     syntax tree (AST)."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of parsers and combinators.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (string) Parser) identifier-parser))
(declaim (ftype (function ()       Parser) statement-list-parser))

;;; -------------------------------------------------------

(define-niladic-parser-function integer-literal-parser
  (.let (digits
          (.or
            ;; Unsigned integer.
            (.one-or-more (.digit 10))
            ;; Signed integer.
            (.let* ((sign
                      (.or (.character #\+)
                           (.character #\-)))
                    (decimal-digits
                      (.one-or-more (.digit 10))))
              (.return
                (push sign decimal-digits)))))
    (.return
      (make-instance 'Literal-Node
        :value-type :integer
        :value      (parse-integer (coerce digits 'string))))))

;;; -------------------------------------------------------

(define-niladic-parser-function infinity-parser
  (.chain
    (identifier-parser "inf")
    (.return
      (make-instance 'Infinity-Node))))

;;; -------------------------------------------------------

(define-niladic-parser-function input-parser
  (.chain
    (identifier-parser "in")
    (.return
      (make-instance 'Input-Node))))

;;; -------------------------------------------------------

(define-niladic-parser-function finite-integer-parser
  (.or
    (integer-literal-parser)
    (input-parser)))

;;; -------------------------------------------------------

(define-niladic-parser-function integer-expression-parser
  (.or
    (infinity-parser)
    (finite-integer-parser)))

;;; -------------------------------------------------------

(define-parser-function identifier-parser (identifier)
  (declare (type string identifier))
  (apply #'.chain
    (append (map 'list #'.character identifier)
      (list (.return identifier)))))

;;; -------------------------------------------------------

;; FiBuCommand := "Fi" , finiteIntegerExpr , "Bu" , finiteIntegerExpr ;
(define-niladic-parser-function fibu-parser
  (.let* ((fizz-divisor
            (.chain
              (identifier-parser "Fi")
              (finite-integer-parser)))
          (buzz-divisor
            (.chain
              (identifier-parser "Bu")
              (finite-integer-parser))))
    (declare (type Node fizz-divisor))
    (declare (type Node buzz-divisor))
    (.return
      (make-instance 'Divisors-Node
        :fizz-divisor fizz-divisor
        :buzz-divisor buzz-divisor))))

;;; -------------------------------------------------------

(defun escaped-character-parser ()
  "Returns a new parser which succeeds if a backlash ('\\'), immediately
   succeeded by any character, follows, embracing in its result the
   ensuing character."
  (.chain
    (.character #\\)
    (.any-character)))

;;; -------------------------------------------------------

(define-niladic-parser-function string-parser
  (.let (characters
          (.many-until
            (.or
              (escaped-character-parser)
              (.any-character))
            (.or
              (.eof)
              (.newline)
              (.character #\))
              (.character #\}))))
    (declare (type (list-of character) characters))
    (.return
      (make-instance 'Literal-Node
        :value-type :string
        :value      (coerce characters 'string)))))

;;; -------------------------------------------------------

;; arrow := optionalSpaces , "->" , space ;
(define-niladic-parser-function arrow-parser
  (.between
    (.skip-zero-or-more (.space))
    (.skip-one          (.space))
    (identifier-parser "->")))

;;; -------------------------------------------------------

;; FizzCommand     := "Fizz"     , arrow , string ;
;; BuzzCommand     := "Buzz"     , arrow , string ;
;; FizzBuzzCommand := "FizzBuzz" , arrow , string ;
(define-niladic-parser-function text-replacement-parser
  (.let (target
          (.or
            (.chain (identifier-parser "FizzBuzz")
              (.return :fizzbuzz))
            (.chain (identifier-parser "Fizz")
              (.return :fizz))
            (.chain (identifier-parser "Buzz")
              (.return :buzz))))
    (declare (type (member :fizzbuzz :fizz :buzz) target))
    (.chain
      (arrow-parser)
      (.let (replacement (string-parser))
        (declare (type Literal-Node replacement))
        (.return
          (make-instance 'Text-Replacement-Node
            :target     target
            :substitute replacement))))))

;;; -------------------------------------------------------

(defun space-surrounding-parser (parser)
  "Returns a parser which succeeds if the input PARSER matches,
   contingently surrounded by zero or more adjacent spaces, and responds
   with the PARSER's parse result."
  (declare (type Parser parser))
  (the Parser
    (.between
      (.skip-zero-or-more (.space))
      (.skip-zero-or-more (.space))
      parser)))

;;; -------------------------------------------------------

(defun argument-list-parser (&rest arguments)
  "Returns a parser which succeeds if all the ARGUMENTS, themselves
   parsers, are encountered in this order, each separated by a single,
   contingently space-surrounded, comma (\",\"), the whole expression
   ought to be ensconced in parenthesis \"(\"...\")\", with spaces
   homologated in the interior."
  (declare (type (list-of Parser) arguments))
  (the Parser
    (.between
      (space-surrounding-parser
        (.character #\())
      (.chain
        (.skip-zero-or-more (.space))
        (.character #\)))
      (apply
        #'.all-separated-by
        (space-surrounding-parser
          (.character #\,))
        arguments))))

;;; -------------------------------------------------------

;; rangeCommand := "range"
;;              ,  optionalSpaces
;;              ,  "("
;;              ,  optionalSpaces
;;              ,  finiteIntegerExpr
;;              ,  optionalSpaces
;;              ,  ","
;;              ,  optionalSpaces
;;              ,  integerExpression
;;              ,  optionalSpaces
;;              , ")" ;
(define-niladic-parser-function range-parser
  (.let (bounds
          (.chain
            (identifier-parser "range")
            (argument-list-parser
              (finite-integer-parser)
              (integer-expression-parser))))
    (declare (type node-list bounds))
    (destructuring-bind (start end) bounds
      (declare (type Node start))
      (declare (type Node end))
      (.return
        (make-instance 'Range-Node
          :start start
          :end   end)))))

;;; -------------------------------------------------------

(define-niladic-parser-function when-parser
  (.chain
    (identifier-parser "when")
    (.let (counter (argument-list-parser (finite-integer-parser)))
      (declare (type node-list counter))
      (.let (substitute (string-parser))
        (declare (type Node substitute))
        (.return
          (make-instance 'When-Node
            :counter    (first counter)
            :substitute substitute))))))

;;; -------------------------------------------------------

;; "{" , statementList , "}" ;
(define-niladic-parser-function code-block-parser
  (.chain
    (.skip-zero-or-more (.space))
    (.between
      (.character #\{)
      (.character #\})
      (statement-list-parser))))

;;; -------------------------------------------------------

(define-niladic-parser-function if-parser
  (.chain
    (identifier-parser "if")
    (.let* ((counter
              (argument-list-parser
                (finite-integer-parser)))
            (statements
              (code-block-parser)))
      (declare (type node-list counter))
      (declare (type node-list statements))
      (.return
        (make-instance 'If-Node
          :counter    (first counter)
          :statements (make-instance 'Block-Node
                        :statements statements))))))

;;; -------------------------------------------------------

(define-niladic-parser-function annotation-parser
  (.character #\))
  (.chain
    (.many-until
      (.any-character)
      (.or
        (.newline)
        (.eof)))
    (.return
      (make-instance 'No-Op-Node))))

;;; -------------------------------------------------------

(define-niladic-parser-function statement-parser
  (space-surrounding-parser
    (.or
      (fibu-parser)
      (text-replacement-parser)
      (range-parser)
      (when-parser)
      (if-parser)
      (annotation-parser))))

;;; -------------------------------------------------------

(define-niladic-parser-function line-parser
  (space-surrounding-parser
    (.before
      (statement-parser)
      (.optional
        (annotation-parser)))))

;;; -------------------------------------------------------

(define-niladic-parser-function optional-whitespace-skipper
  (.skip-zero-or-more
    (.or
      (.space)
      (.newline))))

;;; -------------------------------------------------------

(define-niladic-parser-function statement-list-parser
  (.chain
    (optional-whitespace-skipper)
    (.zero-or-more-separated-by
      (line-parser)
      (.one-or-more (.newline)))))

;;; -------------------------------------------------------

;; Parse the end of the program, succeeding the statements by a series
;; of optional spaces and linebreaks.
(define-niladic-parser-function coda-parser
  (.chain
    (optional-whitespace-skipper)
    (.eof)))

;;; -------------------------------------------------------

(define-niladic-parser-function program-parser
  (.let (statements (statement-list-parser))
    (declare (type node-list statements))
    (.chain
      (coda-parser)
      (.return
        (make-instance 'Program-Node
          :statements statements)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of range operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric counter-in-range-p (counter start end)
  (:method ((counter integer)
            (start   integer)
            (end     integer))
    (declare (type integer counter))
    (declare (type integer start))
    (declare (type integer end))
    (the boolean
      (not (null
        (<= start counter end)))))
  
  (:method ((counter integer)
            (start   integer)
            (end     (eql :infinity)))
    (declare (type integer         counter))
    (declare (type integer         start))
    (declare (type (eql :infinity) end))
    (declare (ignore               end))
    (the boolean
      (not (null
        (>= counter start)))))
  
  (:method ((counter integer)
            (start   (eql :infinity))
            (end     integer))
    (declare (type integer         counter))
    (declare (type (eql :infinity) start))
    (declare (ignore               start))
    (declare (type integer         end))
    (the boolean
      (not (null
        (<= counter end)))))
  
  (:method ((counter integer)
            (start   (eql :infinity))
            (end     (eql :infinity)))
    (declare (type integer         counter))
    (declare (ignore               counter))
    (declare (type (eql :infinity) start))
    (declare (ignore               start))
    (declare (type (eql :infinity) end))
    (declare (ignore               end))
    (the boolean T))
  
  (:documentation
    "Determines whether the COUNTER resides inside of the closed
     interval determined by the inclusive START and inclusive END posts,
     returning on confirmation a ``boolean'' value of ``T'', otherwise
     ``NIL''."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aliquot-p (dividend divisor)
  "Determines whether the DIVISOR constitutes an aliquot of the
   DIVIDEND, which means that the former divides the latter without
   leaving a remainder, and returns on confirmations a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type integer dividend))
  (declare (type integer divisor))
  (the boolean
    (not (null
      (zerop (rem dividend divisor))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing abstract syntax tree (AST).")
    :reader        interpreter-tree
    :type          Node
    :documentation "The abstract syntax tree (AST) to evaluate.")
   
   ;; "Fizz", "Buzz", and "FizzBuzz" output replacements.
   (fizz-text
    :initarg       :fizz-text
    :initform      "Fizz"
    :accessor      interpreter-fizz-text
    :type          string
    :documentation "The text to print instead of the standard
                    \"Fizz\".")
   (buzz-text
    :initarg       :buzz-text
    :initform      "Buzz"
    :accessor      interpreter-buzz-text
    :type          string
    :documentation "The text to print instead of the standard
                    \"Buzz\".")
   (fizzbuzz-text
    :initarg       :fizzbuzz-text
    :initform      "FizzBuzz"
    :accessor      interpreter-fizzbuzz-text
    :type          string
    :documentation "The text to print instead of the standard
                    \"FizzBuzz\".")
   
   ;; "Fizz", "Buzz", and "FizzBuzz" loop counter aliquots.
   (fizz-divisor
    :initarg       :fizz-divisor
    :initform      3
    :accessor      interpreter-fizz-divisor
    :type          integer
    :documentation "The divisor which, if an aliquot of the current
                    counter value, produces the text \"Fizz\" or its
                    equivalent, for the latter please see the slot
                    FIZZ-TEXT.")
   (buzz-divisor
    :initarg       :buzz-divisor
    :initform      5
    :accessor      interpreter-buzz-divisor
    :type          integer
    :documentation "The divisor which, if an aliquot of the current
                    counter value, produces the text \"Buzz\" or its
                    equivalent, for the latter please see the slot
                    BUZZ-TEXT.")
   (fizzbuzz-divisor
    :initarg       :fizzbuzz-divisor
    :initform      15
    :accessor      interpreter-fizzbuzz-divisor
    :type          integer
    :documentation "The divisor which, if an aliquot of the current
                    counter value, produces the text \"FizzBuzz\" or its
                    equivalent, for the latter please see the slot
                    FIZZBUZZ-TEXT.")
   
   ;; Loop counter range.
   (counter-start
    :initarg       :counter-start
    :initform      1
    :accessor      interpreter-counter-start
    :type          integer
    :documentation "The iteration counter's inclusive start value.")
   (counter-end
    :initarg       :counter-end
    :initform      100
    :accessor      interpreter-counter-end
    :type          fb-integer
    :documentation "The iteration counter's inclusive end value.")
   
   ;; Specific counter-to-output mappings.
   (counter-outputs
    :initarg       :counter-outputs
    :initform      (make-hash-table :test #'eql)
    :accessor      interpreter-counter-outputs
    :type          (hash-table-of integer string)
    :documentation "A mapping of specific counter values to the
                    replacement which shall be printed in lieu of the
                    verbatim counter state.")
   
   ;; Events associated with counter states.
   (counter-events
    :initarg       :counter-events
    :initform      (make-hash-table :test #'eql)
    :accessor      interpreter-counter-events
    :type          (hash-table-of integer Block-Node)
    :documentation "A mapping of specific counter values to statements
                    that should be executed after printing the same."))
  (:documentation
    "The ``Interpreter'' class provides an entity responsible for the
     evaluation of a FixxBuxx program's abstract syntax tree (AST)
     model."))

;;; -------------------------------------------------------

(defun interpreter-set-divisors (interpreter fizz-divisor buzz-divisor)
  "Sets the INTERPRETER's FIZZ-DIVISOR and BUZZ-DIVISOR, as an
   epiphenomenal concomitant also determining and storing the FizzBuzz
   divisor, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     fizz-divisor))
  (declare (type integer     buzz-divisor))
  (setf (interpreter-fizz-divisor interpreter) fizz-divisor)
  (setf (interpreter-buzz-divisor interpreter) buzz-divisor)
  (setf (interpreter-fizzbuzz-divisor interpreter)
    (lcm fizz-divisor buzz-divisor))
  (values))

;;; -------------------------------------------------------

(defun interpreter-counter-text (interpreter counter)
  "Returns an object utile for printing the COUNTER state in concord
   with the INTERPRETER's configuration."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the (or integer string)
    (gethash counter
      (interpreter-counter-outputs interpreter) counter)))

;;; -------------------------------------------------------

(defun interpreter-has-counter-text-p (interpreter counter)
  "Determines whether the INTERPRETER possesses a substitute text in
   association with the COUNTER state, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the boolean
    (not (null
      (nth-value 1
        (gethash counter
          (interpreter-counter-outputs interpreter)))))))

;;; -------------------------------------------------------

(defun interpreter-set-counter-text (interpreter counter text)
  "Registers at the INTERPRETER the TEXT to output if the FixxBuzz
   program loop counter assumes the COUNTER state, and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (declare (type string      text))
  (setf (gethash counter (interpreter-counter-outputs interpreter))
        text)
  (values))

;;; -------------------------------------------------------

(defun interpreter-output-mode (interpreter counter)
  "Returns the output mode for the COUNTER's state in relation to the
   INTERPRETER's configuration."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the output-mode
    (cond
      ((interpreter-has-counter-text-p interpreter counter)
        :counter)
      ((aliquot-p counter (interpreter-fizzbuzz-divisor interpreter))
        :fizzbuzz)
      ((aliquot-p counter (interpreter-fizz-divisor interpreter))
        :fizz)
      ((aliquot-p counter (interpreter-buzz-divisor interpreter))
        :buzz)
      (T
        :counter))))

;;; -------------------------------------------------------

(defun interpreter-set-counter-event (interpreter counter statements)
  "Associates with the COUNTER state in the INTERPRETER the STATEMENTS
   which shall be executed upon the FixxBuxx program loop's assumption
   of the same COUNTER value, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (declare (type Block-Node  statements))
  (setf (gethash counter (interpreter-counter-events interpreter))
        statements)
  (values))

;;; -------------------------------------------------------

(defun interpreter-has-counter-event-p (interpreter counter)
  "Determines whether an event is registered at the INTERPRETER to be
   executed when assuming the COUNTER state, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the boolean
    (not (null
      (nth-value 1
        (gethash counter
          (interpreter-counter-events interpreter)))))))

;;; -------------------------------------------------------

(defun interpreter-get-counter-event (interpreter counter)
  "Returns the ``Block-Node'' representing the event to be executed upon
   the COUNTER state's assumption in the FixxBuxx program loop, or
   responds with the ``NIL'' value if none such is registered at the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the (or null Block-Node)
    (nth-value 0
      (gethash counter
        (interpreter-counter-events interpreter)))))

;;; -------------------------------------------------------

(defun interpreter-shall-proceed-p (interpreter counter)
  "Determines whether the program shall engage in at least another
   iteration based upon the COUNTER's state and the INTERPRETER's
   configuration, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     counter))
  (the boolean
    (counter-in-range-p counter
      (interpreter-counter-start interpreter)
      (interpreter-counter-end   interpreter))))

;;; -------------------------------------------------------

(defgeneric interpreter-visit-node (interpreter node)
  (:documentation
    "Visits and evaluates the NODE utilizing the INTERPRETER and returns
     no value."))

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
                                   (node        Divisors-Node))
  (declare (type Interpreter   interpreter))
  (declare (type Divisors-Node node))
  (interpreter-set-divisors interpreter
    (interpreter-visit-node interpreter
      (divisors-node-fizz-divisor node))
    (interpreter-visit-node interpreter
      (divisors-node-buzz-divisor node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Literal-Node))
  (declare (type Interpreter  interpreter))
  (declare (ignore            interpreter))
  (declare (type Literal-Node node))
  (the (or string integer)
    (literal-node-value node)))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Infinity-Node))
  (declare (type Interpreter   interpreter))
  (declare (ignore             interpreter))
  (declare (type Infinity-Node node))
  (declare (ignore             node))
  (the fb-integer :infinity))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Input-Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Input-Node  node))
  (declare (ignore           node))
  (format T "~&>> ")
  (the integer
    (prog1
      (parse-integer
        (read-line))
      (clear-input))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        No-Op-Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type No-Op-Node  node))
  (declare (ignore           node))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Range-Node))
  (declare (type Interpreter interpreter))
  (declare (type Range-Node  node))
  (setf (interpreter-counter-start interpreter)
    (interpreter-visit-node interpreter
      (range-node-start node)))
  (setf (interpreter-counter-end interpreter)
    (interpreter-visit-node interpreter
      (range-node-end node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Text-Replacement-Node))
  (declare (type Interpreter           interpreter))
  (declare (type Text-Replacement-Node node))
  (case (text-replacement-node-target node)
    (:fizz
      (setf (interpreter-fizz-text interpreter)
        (interpreter-visit-node interpreter
          (text-replacement-node-substitute node))))
    (:buzz
      (setf (interpreter-buzz-text interpreter)
        (interpreter-visit-node interpreter
          (text-replacement-node-substitute node))))
    (:fizzbuzz
      (setf (interpreter-fizzbuzz-text interpreter)
        (interpreter-visit-node interpreter
          (text-replacement-node-substitute node))))
    (otherwise
      (error "Invalid text replacement target: ~s."
        (text-replacement-node-target node))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        When-Node))
  (declare (type Interpreter interpreter))
  (declare (type When-Node   node))
  (let ((counter
          (interpreter-visit-node interpreter
            (when-node-counter node)))
        (text
          (interpreter-visit-node interpreter
            (when-node-substitute node))))
    (declare (type integer counter))
    (declare (type string  text))
    (interpreter-set-counter-text interpreter counter text))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Block-Node))
  (declare (type Interpreter interpreter))
  (declare (type Block-Node  node))
  (dolist (statement (block-node-statements node))
    (declare (type Node statement))
    (interpreter-visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        If-Node))
  (declare (type Interpreter interpreter))
  (declare (type If-Node     node))
  (let ((counter
          (interpreter-visit-node interpreter
            (if-node-counter node)))
        (statements
          (if-node-statements node)))
    (declare (type integer    counter))
    (declare (type Block-Node statements))
    (interpreter-set-counter-event interpreter counter statements))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) representation of the
   FixxBuxx program maintained by the INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  
  ;; Evaluate the abstract syntax tree (AST).
  (interpreter-visit-node interpreter
    (interpreter-tree interpreter))
  
  ;; Execute the counting iteration.
  (loop
    for counter
      of-type integer
      from    (interpreter-counter-start interpreter)
      by      1
    while
      (interpreter-shall-proceed-p interpreter counter)
    do
      (let ((output-mode (interpreter-output-mode interpreter counter)))
        (declare (type output-mode output-mode))
        (format T "~&~a"
          (case output-mode
            (:fizzbuzz
              (interpreter-fizzbuzz-text interpreter))
            (:fizz
              (interpreter-fizz-text interpreter))
            (:buzz
              (interpreter-buzz-text interpreter))
            (:counter
              (interpreter-counter-text interpreter counter))
            (otherwise
              (error "Invalid output mode ~s for counter value ~d."
                output-mode counter)))))
      ;; If an event is associated with the COUNTER, execute its
      ;; statements.
      (when (interpreter-has-counter-event-p interpreter counter)
        (interpreter-visit-node interpreter
          (interpreter-get-counter-event interpreter counter))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-FixxBuxx (code)
  "Interprets the piece of FixxBuxx source CODE and returns no value."
  (interpreter-interpret
    (make-instance 'Interpreter :tree
      (parse
        (program-parser)
        code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!  ".
(interpret-FixxBuxx
  "
  range(1,1)            ) The range is from 1 to 1
  when(1)Hello, World!  ) When you need to print 1, print \"Hello, World!\" instead.
  Fi1Bu1                ) Multiples of 1(Only 1), print \"FizzBuzz\". But you need to print \"Hello, World!\" because of the \"when\" command.
  ")

;;; -------------------------------------------------------

;; Print the default FizzBuzz program from 1 to 100.
(interpret-FixxBuxx
  "Fi3Bu5                ) Multiples of 3 are replaced with \"Fizz\", and multiples of 5 are replaced with \"Buzz\". Multiples of lcm(3,5) = 15 are replaced with \"FizzBuzz\".")

;;; -------------------------------------------------------

;; Truth-machine
(interpret-FixxBuxx
  "range(in,1)           ) If input is 0, the range is from 0 to 0. If input is 1, the range is from 1 to 1
   Fizz -> 1             ) Change \"Fizz\" into \"1\".
   Buzz -> 1             ) Change \"Buzz\" into \"1\".
   FizzBuzz -> 1         ) Change \"FizzBuzz\" into \"1\".
   when(0)0              ) When you need to output 0, output 0.
   if(0){range(0,0)}     ) If the first output is 0 (the input is 0), the range is 0 to 0.
   if(1){range(1,inf)}   ) If the first output is 1 (the input is 1), the range is 1 to infinity.
   Fi1Bu1                ) Multiples of lcm(1,1) = 1, print \"FizzBuzz\", but it's replaced by \"1\".")

;;; -------------------------------------------------------

;; Employ several facilities, including conditional execution, to print
;; a textual form of the counter values 1 to 10.
(interpret-FixxBuxx
  "range(1, 10)
   when(1)one
   if(1){when(2)two}
   if(2){when(3)three
     when(4)four
     when(5)five}
   if(5){
     when(6)six
     when(7)seven
     when(8)eight}
   Fi9Bu10
   Fizz -> nine
   Buzz -> ten")
