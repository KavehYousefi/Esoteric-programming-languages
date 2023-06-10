;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "wepmlrio", presented by the Esolang user "Esowiki201529A"
;; in the year 2015, and intended as a syntactical reformulation of
;; Urban Mueller's "brainfuck" programming language, with the eight
;; instruction tokens substituted by letters from the string "wepmlrio",
;; pursuing an eath transmission via Morse code.
;; 
;; 
;; Concept
;; =======
;; The wepmlrio programming language is founded upon brainfuck,
;; appropriating its entire concepts verbatim, while merely employing a
;; different set of characters for the command tokens' representation,
;; the selection of the same relays to the requirements of Morse code
;; communication.
;; 
;; The command identifier octuple, accompanied by the Morse codes, shall
;; be adduced:
;; 
;;   --------------------------
;;   Command token | Morse code
;;   --------------+-----------
;;   w             | .--
;;   e             | .
;;   p             | .--.
;;   m             | --
;;   l             | .-..
;;   r             | .-.
;;   I             | ..
;;   o             | ---
;;   --------------------------
;; 
;; 
;; Architecture
;; ============
;; wepmlrio subscribes to the native tenets of its brainfuck ancestor,
;; maintaining a linear sequence of unsigned-byte-valued cells,
;; admitting the integer range of [0, 255], however, not necessitated to
;; accommodate the fixed 30,000 in tally, nor constrained to
;; non-negative indices.
;; 
;; 
;; Instructions
;; ============
;; wepmlrio's cleronomy apportions to it the exact eight instructions
;; commorant in its inspiration, brainfuck; expressed simply in a more
;; elaborate guise.
;; 
;; == OVERVIEW ==
;; An apercu endowed with compendiousness shall educate about the octet
;; of instructions furnished to the language.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   r       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   l       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   p       | Increments the current cell value by one.
;;           | If transcending the upper bound of 255, the value is
;;           | wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   m       | Decrements the current cell value by one.
;;           | If transcending the lower bound of zero (0), the value
;;           | is wrapped around to the maximum of 255.
;;   ..................................................................
;;   I       | Queries the user for an ASCII character and stores its
;;           | ASCII code in the current cell.
;;   ..................................................................
;;   o       | Prints to the standard output the character
;;           | corresponding to the current cell value when construed
;;           | an ASCII code.
;;   ..................................................................
;;   w       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   e       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK-EQUIVALENCY ==
;; The fact of its direct equivalency with brainfuck permits an
;; unambiguous juxtaposition regarding wepmlrio's and its stock-father's
;; command tokens:
;; 
;;   ---------------------------------------
;;   wepmlrio command | brainfuck equivalent
;;   -----------------+---------------------
;;   w                | [
;;   .......................................
;;   e                | ]
;;   .......................................
;;   p                | +
;;   .......................................
;;   m                | -
;;   .......................................
;;   l                | <
;;   .......................................
;;   r                | >
;;   .......................................
;;   I                | ,
;;   .......................................
;;   o                | .
;;   ---------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; wepmlrio's perfect congruency with brainfuck ascertains its
;; disencumbrance from most ambiguities. A single instance, however, may
;; be levied to disquisition.
;; 
;; == WHICH CONCRETE DESIGN APPLIES TO THE MEMORY? ==
;; Merely the cell value model, ramose in the variations appertaining to
;; brainfuck's construe in modern contexts, tharfs a more explicit
;; treatise. Its has been adjudged, and ascertained by its aefauld
;; "Hello World" example, that each cell maintains an unsigned octet in
;; the range [0, 255], wrapping its value around to the opposite
;; extremum if transgressing one of its marches. The ensconcing tape
;; is unbounded along both lateralities.
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp exhibits a dioristic element in
;; the champarty of parser combinators and macros, the former assigned
;; to the assemblage of an immediately executable Lisp program in the
;; form of its S-expressions, the latter capacitated with the actual
;; evaluation of the thus generated code by the Lisp interpreter itself.
;; 
;; == LEXING AND PARSING ARE REALIZED IN THE PARSER COMBINATORS ==
;; The deployment of parser combinators expands across both the wikes of
;; lexical analyzation and parsing --- a choice justly reckoned as
;; ponderous for the facile nature of brainfuck in the current wepmlrio
;; guise; however, also an exercise vindicated by its epideictic
;; vallidom.
;; 
;; A parser either returns a command object, or a list thereof for the
;; topmost specimen, which represents the entire program's instruction
;; sequence. Non-command tokens produce the ``NIL'' value, a substitute
;; datum that always eludes capture.
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
;; == PARSERS AND COMBINATORS PROVIDE AN S-EXPRESSIONS FACTORY ==
;; Proceeding from the general nature of a parser's output, the project
;; at hand adhibits the offered competences to directly assemble a piece
;; of Common Lisp code, which is subsequently evaluated by the Lisp
;; interpreter itself.
;; 
;; In lieu of an intermediate representation, which most likely assumes
;; the mold of an abstract syntax tree (AST) or a simple vector of
;; instructions, the parsers transform the input string into
;; S-expressions, amenable to immediate execution when inducted into a
;; macro's context.
;; 
;; A conceived solution in concord with the traditional concept of a
;; custom interpreter succeeding the lexical analyzation and parsing
;; stages is illustrated below:
;; 
;;   +-----------------------------+
;;   | wepmlrio source code string |
;;   +-----------------------------+
;;                  | 
;;                  | Lexical analyzation and parsing
;;                  | 
;;                  V
;;   +-----------------------------+
;;   | Intermediate representation |
;;   | (AST or instruction vector) |
;;   +-----------------------------+
;;                  | 
;;                  | Interpretation (by custom interpreter)
;;                  | 
;;                  V
;;   +-----------------------------+
;;   | Executed program            |
;;   +-----------------------------+
;; 
;; The bartery of the intermediate representation (stage 2) for direct
;; S-expressions permits the obviation of many efforts accompanying the
;; instructions' evaluation, as the Lisp interpreter itself operates on
;; the few facilities requisite for its acquainted duties.
;; 
;;   +-----------------------------+
;;   | wepmlrio source code string |
;;   +-----------------------------+
;;                  | 
;;                  | Lexical analyzation and parsing
;;                  | 
;;                  V
;;   +-----------------------------+
;;   | Common Lisp S-expressions   |
;;   +-----------------------------+
;;                  | 
;;                  | Interpretation (by Lisp interpreter itself)
;;                  | 
;;                  V
;;   +-----------------------------+
;;   | Executed program            |
;;   +-----------------------------+
;; 
;; == HOMOICONICITY: CODE = DATA = CODE = ... ==
;; Homoiconicity as a language characteristic refers to the concomitant
;; disposition of a data structure for both data storage purposes and
;; source code representation. In the case of Lisp, the "LISt Processing
;; Language", the linked list partakes of this twifaced nature.
;; 
;; An example shall aide in the principle's illustration: Given the
;; list of three elements "+", "1", and "2", which would be rendered in
;; visual terms as
;; 
;;   (+ 1 2)
;; 
;; a derivation of code from data shall be extended. The graphical mode
;; presented aboon, as already stated, actual refers to a list:
;; 
;;   (list '+ 1 2)
;; 
;; or, as the quoting mechanism homologates an apostrophe ("'") to
;; abbreviate the "quote" special operator, the following, less common,
;; equivalency presides:
;; 
;;   (list (quote +) 1 2)
;; 
;; Alternatively, in this case, the whole expression may be quoted:
;; 
;;   (quote (+ 1 2))
;; 
;; This produces a literal --- an object not intended for modification.
;; Iterum, it holds for the curtailed design:
;; 
;;   '(+ 1 2)
;; 
;; If we now elide the preceding single quote --- or the ensconcing
;; "quote" special operator in the former code fragement ---, the form
;; 
;;   (+ 1 2)
;; 
;; is yielded. The absence of a quoting mark incites in the Lisp
;; interpreter the desire to evaluate this desinent form, producing an
;; addition and ultimately yielding the value 3 (= 1 + 2). The first
;; element, "+", is interpreted as an operation identifier, with any
;; subsequent items contributing its arguments:
;; 
;;    +------ Invoke the operation with the name "+" (addition) ...
;;    |
;;    | +------ ... with the number "1" as the first argument ...
;;    | |
;;    | | +------ ... and the number "2" as the second argument.
;;    | | |
;;    V V V
;;   (+ 1 2)
;; 
;; In very simple terms: An expression without quotation is evaluated as
;; code; a piece of code with quotation is rendered as data. In
;; corollary, code and data "look alike".
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
;;   [esolang2022wepmlrio]
;;   The Esolang contributors, "wepmlrio", 2022
;;   URL: "https://esolangs.org/wiki/Wepmlrio"
;;   
;;   [esolang2023trivialbfsub]
;;   The Esolang contributors, "Trivial brainfuck substitution", 2023
;;   URL: "https://esolangs.org/wiki/Trivial_brainfuck_substitution"
;;   Notes:
;;     - Describes the family of trivial brainfuck substitutions.
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

(deftype parser ()
  "The ``parser'' type defines a parser or combinator as a function
   which, upon invocation with a parse state, responds with a parse
   result."
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype parser-function ()
  "The ``parser-function'' type defines a parser or combinator in accord
   with the ``parser'' type, however, reduced to the general
   specification of a ``function'' in order to be eligible as a type
   specifier in all cases, especially exhibiting compatibility with the
   ``the'' special form and the ``of-type'' option in a ``loop''
   construct."
  'function)

;;; -------------------------------------------------------

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
              for element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype s-expression ()
  "The ``s-expression'' type defines a Common Lisp program as an
   S-expression."
  'T)

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight adjacent
   bits, thus being a commorant of the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   unsigned byte-valued cells, amenable to signed integer subscripts,
   and realized in the form of a hash table that associates integer keys
   with ``octet'' values."
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
                (and (typep key   'integer)
                     (typep value 'octet))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-parse-state (source position))
  (:copier      NIL))
  "The ``Parse-State'' represents a piece of wepmlrio source code and a
   pointer into its currently indagated character, forming the input
   into a parser."
  (source   (error "Missing source.") :type string)
  (position 0                         :type fixnum))

;;; -------------------------------------------------------

(defun parse-state-advance (parse-state)
  "Returns a copy of the PARSE-STATE sharing the same source, but having
   its position cursor advanced to the next location."
  (declare (type Parse-State parse-state))
  (the Parse-State
    (make-parse-state
      (parse-state-source parse-state)
      (1+ (parse-state-position parse-state)))))

;;; -------------------------------------------------------

(defun parse-state-character (parse-state)
  "Returns the character at the current position into the PARSE-STATE's
   source, or ``NIL'' if the same is exhausted."
  (declare (type Parse-state parse-state))
  (the (or null character)
    (when (array-in-bounds-p (parse-state-source parse-state)
            (parse-state-position parse-state))
      (char (parse-state-source parse-state)
        (parse-state-position parse-state)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p
                                   &optional (state  NIL)
                                             (output NIL)))
  (:copier      NIL))
  "The ``Parse-Result'' class serves to encapsulate the response of a
   parser when applied to a particular ``Parse-State'', entailing a
   success flag to determine its matching, the state produced by the
   administration process, and the parser's output, which shall
   contribute to the overall assemblage --- in this project, usually a
   piece of Common Lisp code."
  (succeeded-p NIL :type boolean)
  (state       NIL :type (or null Parse-State))
  (output      NIL :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parsers and combinators.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () parser-function) parse-statements))

;;; -------------------------------------------------------

(defun probe-character (predicate)
  "Returns a new parser which succeeds if the PREDICATE, expecting a
   character or the ``NIL'' value, matches by responding with a
   non-``NIL'' value, on confirmation embracing in its result the probed
   character."
  (declare (type (function ((or null character)) *) predicate))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (if (funcall predicate (parse-state-character current-state))
            (make-parse-result T
              (parse-state-advance   current-state)
              (parse-state-character current-state))
            (make-parse-result NIL current-state NIL))))))

;;; -------------------------------------------------------

(defun parse-character (expected-character)
  "Returns a new parser which succeeds if its input state's current
   element equals the EXPECTED-CHARACTER, on confirmation embracing in
   its result the probed state element."
  (declare (type character expected-character))
  (the parser-function
    (probe-character
      #'(lambda (state-character)
          (declare (type (or null character) state-character))
          (the boolean
            (not (null
              (and state-character
                   (char= state-character expected-character)))))))))

;;; -------------------------------------------------------

(defun parse-comment ()
  "Returns a new parser which succeeds if its input state's current
   element constitutes a commentary character, that is, not a wepmlrio
   command token, on confirmation embracing in its result the probed
   state element."
  (the parser-function
    (probe-character
      #'(lambda (state-character)
          (declare (type (or null character) state-character))
          (the boolean
            (not (null
              (and state-character
                   (not (find state-character "wepmlrIo"
                          :test #'char=))))))))))

;;; -------------------------------------------------------

(defun parse-eof ()
  "Returns a new parser which succeeds if its state's source is
   exhausted, on confirmation embracing in its result the ``NIL''
   value."
  (the parser-function
    (probe-character
      #'(lambda (state-character)
          (declare (type (or null character) state-character))
          (the boolean
            (null state-character))))))

;;; -------------------------------------------------------

(defun parse-bind (antecedent parser-generator)
  "Returns a new parser which implements a monadic binding by applying
   the ANTECEDENT parser to its input state, upon its matching
   requesting a consquent parser from the PARSER-GENERATOR function,
   which is subsequently applied to the ANTECEDENT's resulting parse
   state, and, if the consequent parser also matches, embracing in its
   result the output thus produced."
  (declare (type parser                         antecedent))
  (declare (type (function (*) parser-function) parser-generator))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (let ((antecedent-result (funcall antecedent current-state)))
          (declare (type Parse-Result antecedent-result))
          (the Parse-Result
            (if (parse-result-succeeded-p antecedent-result)
              (funcall
                ;; Request the consequent parser from the
                ;; PARSER-GENERATOR.
                (funcall parser-generator
                  (parse-result-output antecedent-result))
                (parse-result-state antecedent-result))
              (make-parse-result NIL
                (parse-result-state antecedent-result) NIL)))))))

;;; -------------------------------------------------------

(defmacro parse-let ((output-variable antecedent) &body body)
  "Applies the ANTECEDENT parser, binds its output, if it has matched,
   to the OUTPUT-VARIABLE, evaluates the BODY forms with access to this
   datum, and returns the desinent evaluated form's result, expected to
   resolve to a parser."
  `(parse-bind ,antecedent
     #'(lambda (,output-variable)
         (declare (ignorable ,output-variable))
         ,@body)))

;;; -------------------------------------------------------

(defun parse-chain (&rest parsers)
  "Returns a new parser which succeeds if all of its input PARSERS, in
   the specified order, match, on confirmation embracing in its result a
   list of the collected outputs in accord with the PARSERS."
  (declare (type (list-of parser-function) parsers))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (if parsers
            (loop
              for new-state
                of-type Parse-State
                =       current-state
                then    (parse-result-state result)
              for parser
                of-type parser-function
                in      parsers
              for result
                of-type Parse-Result
                =       (funcall parser new-state)
              when (not (parse-result-succeeded-p result)) do
                (return result)
              finally
                (return result))
            (make-parse-result T current-state
              (parse-state-character current-state)))))))

;;; -------------------------------------------------------

(defun parse-return (output)
  "Returns a new parser which always succeeds, embracing in its result
   the OUTPUT."
  (declare (type T output))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (make-parse-result T current-state output)))))

;;; -------------------------------------------------------

(defun parse-choices (&rest choices)
  "Returns a new parser which succeeds if any of its CHOICES match, on
   confirmation embracing in its result the first eligibile choice's
   output."
  (declare (type (list-of parser-function) choices))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (if choices
            (loop
              for new-state
                of-type Parse-State
                =       current-state
                then    (parse-result-state result)
              for parser
                of-type parser-function
                in      choices
              for result
                of-type Parse-Result
                =       (funcall parser new-state)
              when (parse-result-succeeded-p result) do
                (return result)
              finally
                (return
                  (make-parse-result NIL current-state NIL)))
            (make-parse-result T current-state
              (parse-state-character current-state)))))))

;;; -------------------------------------------------------

(defun parse-between (open-guard close-guard body)
  "Returns a new parser which succeeds if the OPEN-GUARD, the BODY, and
   the CLOSE-GUARD parsers match in this order, on confirmation
   embracing in its result the BODY parser's output."
  (declare (type parser open-guard))
  (declare (type parser close-guard))
  (declare (type parser body))
  (the parser-function
    (parse-chain open-guard
      (parse-let (body-output body)
        (parse-chain close-guard
          (parse-return body-output))))))

;;; -------------------------------------------------------

(defun parse-command ()
  "Returns a new parser which matches if its input state's element
   represents a wepmlrio command token, on confirmation embracing in its
   result a representative piece of Common Lisp code."
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (funcall
            
            (parse-choices
              (parse-between
                (parse-character #\w)
                (parse-character #\e)
                (parse-let (statements (parse-statements))
                  (parse-return
                    `(loop until (zerop (current-cell)) do
                       ,@(or statements
                             '('()))))))
              
              (parse-chain (parse-character #\p)
                (parse-return
                  '(increment-current-cell)))
              
              (parse-chain (parse-character #\m)
                (parse-return
                  '(decrement-current-cell)))
              
              (parse-chain (parse-character #\l)
                (parse-return
                  '(move-cell-pointer-left)))
              
              (parse-chain (parse-character #\r)
                (parse-return
                  '(move-cell-pointer-right)))
              
              (parse-chain (parse-character #\I)
                (parse-return
                  '(query-user-input)))
              
              (parse-chain (parse-character #\o)
                (parse-return
                  '(output-current-cell))))
            
            current-state)))))

;;; -------------------------------------------------------

(defun parse-many (parser)
  "Returns a new parser which succeeds if the specified PARSER matches
   zero or more times in immediate succession, embracing in its result
   a list of the input PARSER's outputs in the order of encounter."
  (declare (type parser parser))
  (the parser-function
    #'(lambda (current-state)
        (declare (type Parse-State current-state))
        (the Parse-Result
          (loop
            for new-state
              of-type Parse-State
              =       current-state
              then    (parse-result-state result)
            for result
              of-type Parse-Result
              =       (funcall parser new-state)
            while (parse-result-succeeded-p result)
              collect (parse-result-output result)
              into    outputs
            finally
              (return
                (make-parse-result T
                  (parse-result-state result) outputs)))))))

;;; -------------------------------------------------------

(defun parse-skip-many (parser)
  "Returns a new parser which always succeeds by skipping zero or more
   consecutive instances of the input PARSER and embracing in its result
   the ``NIL'' value."
  (declare (type parser parser))
  (the parser-function
    (parse-chain (parse-many parser)
      (parse-return NIL))))

;;; -------------------------------------------------------

(defun parse-statements ()
  "Returns a new parser which always succeeds, embracing in its result
   a list of Common Lisp code fragment matching the extracted wepmlrio
   instructions."
  (the parser-function
    (parse-many
      (parse-chain
        (parse-skip-many
          (parse-comment))
        (parse-command)))))

;;; -------------------------------------------------------

(defun parse-program (parser initial-parse-state)
  "Applies the PARSER to the INITIAL-PARSE-STATE in order to parse the
   program represented by the latter's source and returns the respective
   parse result."
  (declare (type parser      parser))
  (declare (type Parse-State initial-parse-state))
  (the Parse-Result
    (funcall parser initial-parse-state)))

;;; -------------------------------------------------------

(defun parse-code (code)
  "Parses the piece of wepmlrio source CODE, on success returning a
   Common Lisp program capable of replicating its effects, otherwise
   responding with an error of an unspecified type."
  (declare (type string code))
  (let ((result (parse-program (parse-statements) (make-parse-state code 0))))
    (declare (type Parse-Result result))
    (the s-expression
      (if (parse-result-succeeded-p result)
        (parse-result-output result)
        (error "Parsing failed with the result ~s." result)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro interpret-wepmlrio (code)
  "Interprets the piece of wepmlrio source CODE and returns no value."
  `(let ((memory       (make-hash-table :test #'eql))
         (cell-pointer 0))
     (declare (type memory  memory))
     (declare (type integer cell-pointer))
     
     (labels
         ((current-cell ()
           "Returns the current MEMORY cell's value."
           (the octet
             (gethash cell-pointer memory 0)))
         
          ((setf current-cell) (new-value)
           "Sets the current MEMORY cell's value to the NEW-VALUE,
            contingently wrapping it around to ascertain the unsigned
            byte range of [0, 255], and returns no value."
           (declare (type integer new-value))
           (setf (gethash cell-pointer memory 0)
                 (mod new-value 256))
           (values))
          
          (move-cell-pointer-right ()
           "Moves the MEMORY's CELL-POINTER one step to the right and
            returns no value."
           (incf cell-pointer)
           (values))
          
          (move-cell-pointer-left ()
           "Moves the MEMORY's CELL-POINTER one step to the left and
            returns no value."
           (decf cell-pointer)
           (values))
          
          (increment-current-cell ()
           "Increments the current MEMORY cell's value by one and
            returns no value."
           (incf (current-cell))
           (values))
          
          (decrement-current-cell ()
           "Decrements the current MEMORY cell's value by one and
            returns no value."
           (decf (current-cell))
           (values))
          
          (query-user-input ()
           "Queries the user for an input character, stores its ASCII
            code in the current MEMORY cell, and returns no value."
           (format T "~&>> ")
           (setf (current-cell)
                 (char-code (read-char)))
           (clear-input)
           (values))
          
          (output-current-cell ()
           "Prints to the standard output the character associated with
            the current MEMORY cell's value if construed as an ASCII
            code and returns no value."
           (write-char (code-char (current-cell)))
           (values)))
       
       ,@(parse-code code))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-wepmlrio "pwmmrmwrrprmmmmmllelmmlmmmermorrrporroopppworellllopppommmmmmollmorrrrpo")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a null character
;; input.
(interpret-wepmlrio "IowIoe")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-wepmlrio "Iowmmrpwrrelwoelle")
