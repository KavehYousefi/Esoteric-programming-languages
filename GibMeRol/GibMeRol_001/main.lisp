;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "GibMeRol", presented by the Esolang user "Milkman" in the
;; year 2022, and intended as a syntactical reformulation of Urban
;; Mueller's "brainfuck" programming language, with the eight
;; instruction tokens substituted by letters from the string "GibMeRol".
;; 
;; 
;; Concept
;; =======
;; The GibMeRol programming language is founded upon brainfuck,
;; approprating its entire concepts verbatim, while merely employing a
;; different set of characters for the command tokens' representation.
;; 
;; 
;; Architecture
;; ============
;; GibMeRol subscribes to the native tenets of its brainfuck ancestor,
;; maintaining a linear sequence of unsigned-byte-valued cells,
;; admitting the integer range of [0, 255], however, not necessitated to
;; accommodate the fixed 30,000 in tally, nor constrained to
;; non-negative indices.
;; 
;; 
;; Instructions
;; ============
;; GibMeRol's cleronomy apportions to it the exact eight instructions
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
;;   G       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   i       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   b       | Increments the current cell value by one.
;;           | If transcending the upper bound of 255, the value is
;;           | wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   M       | Decrements the current cell value by one.
;;           | If transcending the lower bound of zero (0), the value
;;           | is wrapped around to the maximum of 255.
;;   ..................................................................
;;   e       | Queries the user for an ASCII character and stores its
;;           | ASCII code in the current cell.
;;   ..................................................................
;;   R       | Prints to the standard output the character
;;           | corresponding to the current cell value when construed
;;           | an ASCII code.
;;   ..................................................................
;;   o       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   l       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK-EQUIVALENCY ==
;; The fact of its direct equivalency with brainfuck permits an
;; unambiguous juxtaposition regarding GibMeRol's and its stock-father's
;; command tokens:
;; 
;;   ---------------------------------------
;;   GibMeRol command | brainfuck equivalent
;;   -----------------+---------------------
;;   G                | >
;;   .......................................
;;   i                | <
;;   .......................................
;;   b                | +
;;   .......................................
;;   M                | -
;;   .......................................
;;   e                | ,
;;   .......................................
;;   R                | .
;;   .......................................
;;   o                | [
;;   .......................................
;;   l                | ]
;;   ---------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; GibMeRol's perfect congruency with brainfuck ascertains its
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
;; This implementation in Common Lisp ordains parser combinators to both
;; the wikes of lexical analyzation and parsing --- a choice justly
;; reckoned as ponderous for the facile nature of brainfuck in the
;; current GibMeRol guise; however, also an exercise vindicated by its
;; epideictic vallidom.
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
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-17
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
;;   [esolang2022GibMeRol]
;;   The Esolang contributors, "GibMeRol", 2022
;;   URL: "https://esolangs.org/wiki/GibMeRol"
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

(deftype tuple-of (&rest element-types)
  "The ``tuple-of'' type defines a tuple as a list equal to the
   ELEMENT-TYPES' cardinality, which each i-th list element conforming
   to the i-th ELEMENT-TYPES' member."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (= (length (the list candidate))
               (length element-types))
            (every
              #'(lambda (expected-element-type current-element)
                  (declare (type T expected-element-type))
                  (declare (type T current-element))
                  (typep current-element expected-element-type))
              element-types
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype parse-state ()
  "The ``parse-state'' type defines the advancement of the parsing
   process in terms of a tuple compact of the GibMeRol source code to
   analyze and the current position into the same."
  '(tuple-of string fixnum))

;;; -------------------------------------------------------

(deftype success-flag ()
  "The ``success-flag'' type enumerates the modes of a parse result,
   concretely, whether the respective parser has matched."
  '(member :successful :failed))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized GibMeRol commands."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype parse-result ()
  "The ``parse-result'' type defines the result of a parser's
   application to a particular state as a three-element tuple compact of
   a ``success-flag'', determining whether the parser has matched, the
   new state produced by the parser during its administration, and a
   datum which represents the parser's actual result or output --- in
   this project, either a ``command'' object, a list of such, or the
   ``NIL'' value for a skipped non-command character, while in more
   complex applications an abstract syntax tree (AST) node would be
   produced."
  '(tuple-of success-flag parse-state T))

;;; -------------------------------------------------------

(deftype parser ()
  "The ``parser'' type defines a parser or combinator as a function that
   accepts the current ``parse-state'' and returns a ``parse-result'' an
   encapsulation of the its success flag, the new parse state produced
   by the consumption of the processed character, and the output."
  '(function (parse-state) parse-result))

;;; -------------------------------------------------------

(deftype gibmerol-program ()
  "The ``gibmerol-program'' type defines an execute model of a GibMeRol
   program as a vector of zero or more commands."
  '(vector command *))

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
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from forward jump locations
   in a GibMeRol instruction sequence to the matching back jump
   positions, and vice versa, implemented as a hash table affiliating
   fixnum keys to values of the same type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines a unsigned byte compact of eight adjacent
   bits, thus covering the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   contingently infinite extent, associating arbitrary integer keys to
   octet values in order to simulate a byte array."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse state.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-parse-state (source position)
  "Creates and returns a new ``parse-state'' compact of the GibMeRol
   SOURCE code and the current position into the same."
  (declare (type string source))
  (declare (type fixnum position))
  (the parse-state
    (list source position)))

;;; -------------------------------------------------------

(defun parse-state-element (state)
  "Returns the source character at the current position in the STATE, or
   ``NIL'' if the cursor has transcended the code bounds."
  (declare (type parse-state state))
  (the (or null character)
    (destructuring-bind (source position) state
      (declare (type string source))
      (declare (type fixnum position))
      (when (array-in-bounds-p source position)
        (char source position)))))

;;; -------------------------------------------------------

(defun parse-state-advance (state)
  "Creates a copy of the STATE which shares it source, but contain a
   position cursor having advanced the next location in the source.
   ---
   The original STATE will not be modified."
  (declare (type parse-state state))
  (destructuring-bind (source position) state
    (declare (type string source))
    (declare (type fixnum position))
    (the parse-state
      (make-parse-state source (1+ position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse result.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-parse-result (success-flag state &optional (output NIL))
  "Creates and returns a new parse result for a parser whose successful
   matching is determined by the SUCCESS-FLAG, in any case comprehending
   the probed STATE during the parsing attempt, and an optional OUTPUT
   that represents the parsing result."
  (declare (type success-flag success-flag))
  (declare (type parse-state  state))
  (declare (type T            output))
  (the parse-result
    (list success-flag state output)))

;;; -------------------------------------------------------

(defun parse-result-succeeded-p (result)
  "Determines whether the parse RESULT represents a success, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type parse-result result))
  (the boolean
    (not (null
      (eq (first result) :successful)))))

;;; -------------------------------------------------------

(defun parse-result-state (result)
  "Returns the ``parse-state'' induced into the parse RESULT during its
   parsing attempt."
  (declare (type parse-result result))
  (the parse-state
    (second result)))

;;; -------------------------------------------------------

(defun parse-result-output (result)
  "Returns the output of the parser having generated this parse RESULT,
   which might constitute the ``NIL'' value upon its failure."
  (declare (type parse-result result))
  (the T
    (third result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parsers and combinators.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parser-parse (parser state)
  "Invokes the PARSER on the parse STATE and returns its
   ``parse-result''."
  (declare (type parser      parser))
  (declare (type parse-state state))
  (the parse-result
    (funcall parser state)))

;;; -------------------------------------------------------

(defun accept-element (predicate)
  "Returns a new parser which succeeds if the PREDICATE, applied to the
   parse state's current character, which may be ``NIL'', returns a
   generalized true value, on confirmation returning in its result the
   tested character or the ``NIL'' object."
  (declare (type (function ((or null character)) *) predicate))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (the parse-result
          (if (funcall predicate (parse-state-element state))
            (make-parse-result :successful
              (parse-state-advance state)
              (parse-state-element state))
            (make-parse-result :failed state))))))

;;; -------------------------------------------------------

(defun .character (expected-character)
  "Returns a new parser which succeeds if the parse state's current
   character equals the EXPECTED-CHARACTER."
  (declare (type character expected-character))
  (the function
    (accept-element
      #'(lambda (current-element)
          (declare (type (or null character) current-element))
          (and current-element
               (char= current-element expected-character))))))

;;; -------------------------------------------------------

(defun .comment ()
  "Returns a new parser which succeeds if the parse state's current
   character represents a non-command token."
  (the function
    (accept-element
      #'(lambda (current-element)
          (declare (type (or null character) current-element))
          (and current-element
               (not (find current-element "GibMeRol" :test #'char=)))))))

;;; -------------------------------------------------------

(defun .eof ()
  "Returns a new parser which succeeds if the parse state's current
   character equals ``NIL'', that is, the GibMeRol souce is exhausted."
  (the function
    (accept-element
      #'(lambda (current-element)
          (declare (type (or null character) current-element))
          (null current-element)))))

;;; -------------------------------------------------------

(defun .return (value)
  "Returns a new parser which always succeeds, returning in its result
   the VALUE."
  (declare (type T value))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (the parse-result
          (make-parse-result :successful state value)))))

;;; -------------------------------------------------------

(defun .chain (&rest parsers)
  "Returns a parser which succeeds if all of its input PARSERS, in this
   exact order, match, on confirmation returning in its result the
   desinent parser's output, while ignoring the preceding ones."
  (declare (type (list-of function) parsers))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (let ((new-state state))
          (declare (type parse-state new-state))
          (the parse-result
            (loop
              for parser
                of-type function
                in      parsers
              for result
                of-type parse-result
                =       (parser-parse parser new-state)
              if (parse-result-succeeded-p result) do
                (setf new-state (parse-result-state result))
              else do
                (return
                  (make-parse-result :failed new-state))
              finally
                (return result)))))))

;;; -------------------------------------------------------

(defun .or (&rest choices)
  "Returns a new parser which succeeds if at least one of the CHOICES
   matches, on confirmation returning in its result first successful
   parser's output."
  (declare (type (list-of function) choices))
  (the function
    #'(lambda (state)
        (declare (type parse-state))
        (the parse-result
          (loop
            for choice
              of-type function
              in      choices
            for result
              of-type parse-result
              =       (parser-parse choice state)
            when (parse-result-succeeded-p result) do
              (return result)
            finally
              (return
                (make-parse-result :failed state)))))))

;;; -------------------------------------------------------

(defun .skip-zero-or-more-times (parser)
  "Returns a new parser which always succeeds by skipping zero or more
   consecutive occurrences of the PARSER, returning in its result the
   ``NIL'' value."
  (declare (type parser parser))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (let ((new-state state))
          (declare (type parse-state new-state))
          (loop
            for result
              of-type parse-result
              =       (parser-parse parser new-state)
            if (parse-result-succeeded-p result) do
              (setf new-state (parse-result-state result))
            else do
              (return
                (make-parse-result :successful new-state NIL)))))))

;;; -------------------------------------------------------

(defun .zero-or-more-times (parser)
  "Returns a new parser which succeeds if the parser matches zero or
   more times in immediate succession, on confirmation returning in its
   result a list comprehending the parser's outputs, conforming to the
   order of encounter."
  (declare (type parser parser))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (let ((outputs   NIL)
              (new-state state))
          (declare (type list        outputs))
          (declare (type parse-state new-state))
          (the parse-result
            (loop
              for result
                of-type parse-result
                =       (parser-parse parser new-state)
              if (parse-result-succeeded-p result) do
                (push (parse-result-output result) outputs)
                (setf new-state (parse-result-state result))
              else do
                (loop-finish)
              finally
                (return
                  (make-parse-result :successful new-state
                    (nreverse outputs)))))))))

;;; -------------------------------------------------------

(defun .before (main-parser suffix)
  "Returns a new parser which succeeds if both the MAIN-PARSER and the
   SUFFIX-PARSER match in this exact order, upon confirmation returning
   merely the MAIN-PARSER's output in the result."
  (declare (type parser main-parser))
  (declare (type parser suffix))
  (the function
    #'(lambda (state)
        (declare (type parse-state state))
        (let ((main-result (parser-parse main-parser state)))
          (declare (type parse-result main-result))
          (the parse-result
            (if (parse-result-succeeded-p main-result)
              (let ((suffix-result
                      (parser-parse suffix
                        (parse-result-state main-result))))
                (if (parse-result-succeeded-p suffix-result)
                  (make-parse-result :successful
                    (parse-result-state  suffix-result)
                    (parse-result-output main-result))
                  suffix-result))
              main-result))))))

;;; -------------------------------------------------------

(defun .command ()
  "Returns a new parser which matches if its input state points to a
   GibMeRol command identifier, upon confirmation returning the
   representative ``command'' object."
  (the function
    (.or
      (.chain
        (.character #\G)
        (.return :move-right))
      (.chain
        (.character #\i)
        (.return :move-left))
      (.chain
        (.character #\b)
        (.return :increment))
      (.chain
        (.character #\M)
        (.return :decrement))
      (.chain
        (.character #\e)
        (.return :input))
      (.chain
        (.character #\R)
        (.return :output))
      (.chain
        (.character #\o)
        (.return :jump-forward))
      (.chain
        (.character #\l)
        (.return :jump-back)))))

;;; -------------------------------------------------------

(defun parse (parser initial-state)
  "Invokes the PARSER on the INITIAL-STATE and, upon success, returns
   the assembled sequence of GibMeRol instructions; upon failure, an
   error of an unspecified type is signaled."
  (declare (type parser      parser))
  (declare (type parse-state initial-state))
  (let ((result (parser-parse parser initial-state)))
    (declare (type parse-result result))
    (the T
      (if (parse-result-succeeded-p result)
        (parse-result-output result)
        (error "The parsing failed with the result ~s." result)))))

;;; -------------------------------------------------------

(defun parse-code (code)
  "Parses the piece of GibMeRol source CODE and returns the encompassed
   instructions as a one-dimensional simple array of commands."
  (declare (type string code))
  (the gibmerol-program
    (coerce
      (parse
        (.before
          (.zero-or-more-times
            (.chain
              (.skip-zero-or-more-times
                (.comment))
              (.command)))
          (.chain
            (.skip-zero-or-more-times
              (.comment))
            (.eof)))
        (make-parse-state code 0))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Calculates and returns for the GibMeRol INSTRUCTIONS a jump table
   which associates with each forward jump command's location in the
   same the respective back jump position, and vice versa."
  (declare (type gibmerol-program instructions))
  (let ((jump-table             (make-hash-table :test #'eql))
        (jump-start-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-start-positions))
    (loop for ip of-type fixnum from 0 below (length instructions) do
      (case (aref instructions ip)
        (:jump-forward
          (push ip jump-start-positions))
        (:jump-back
          (if jump-start-positions
            (let ((jump-start-position (pop jump-start-positions))
                  (jump-end-position   ip))
              (declare (type fixnum jump-start-position))
              (declare (type fixnum jump-end-position))
              (setf (gethash jump-start-position jump-table)
                    jump-end-position)
              (setf (gethash jump-end-position jump-table)
                    jump-start-position))
            (error "Unmatched back jump at position ~d." ip)))
        (otherwise
          NIL)))
    
    (when jump-start-positions
      (error "Unmatched forward jumps at positions ~{~d~^, ~}."
        jump-start-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the GibMeRol INSTRUCTIONS and returns no value."
  (declare (type gibmerol-program instructions))
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (memory              (make-hash-table :test #'eql))
          (pointer             0))
      (declare (type fixnum     ip))
      (declare (type jump-table jump-table))
      (declare (type memory     memory))
      (declare (type integer    pointer))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction in
             the INSTRUCTIONS vector, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Relocates the instruction pointer IP to the opposite jump
             boundary established by the JUMP-TABLE, updates the
             CURRENT-INSTRUCTION, and returns no value.
             ---
             The absence of a JUMP-TABLE entry amenable to the current
             position instigates an error of an unspecified type."
            (setf ip
              (or (gethash ip jump-table)
                  (error "No jump point associated with the IP ~d."
                    ip)))
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the MEMORY's current cell value."
            (the octet
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the MEMORY's current cell,
             contingently following the value's wrapping into the
             unsigned byte range [0, 255], and returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0)
                  (mod new-value 256))
            (values)))
        
        (loop while current-instruction do
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:move-right
              (incf pointer))
            
            (:move-left
              (decf pointer))
            
            (:increment
              (incf (current-cell)))
            
            (:decrement
              (decf (current-cell)))
            
            (:input
              (format T "~&>> ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            (:output
              (write-char
                (code-char
                  (current-cell))))
            
            (:jump-forward
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (:jump-back
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (otherwise
              (error "Unrecognized instruction ~s at position ~d."
                current-instruction ip)))
          
          (advance)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-GibMeRol (code)
  "Interprets the piece of GibMeRol source CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (parse-code code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-GibMeRol-converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-GibMeRol-code-for-brainfuck (bf-code)
  "Generates for the brainfuck code BF-CODE the equivalent GibMeRol
   program and returns a fresh string comprehending the result."
  (declare (type string bf-code))
  (the string
    (with-output-to-string (gibmerol-code)
      (declare (type string-stream gibmerol-code))
      (loop for bf-token of-type character across bf-code do
        (write-char
          (case bf-token
            (#\>       #\G)
            (#\<       #\i)
            (#\+       #\b)
            (#\-       #\M)
            (#\,       #\e)
            (#\.       #\R)
            (#\[       #\o)
            (#\]       #\l)
            (otherwise bf-token))
          gibmerol-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-GibMeRol
  "bbbbbbbboGbbbboGbbGbbbGbbbGbiiiiMlGbGbGMGGboiliMlGGRGMMMRbbbbbbbRRbbbRGGRiMRiRbbbRMMMMMMRMMMMMMMMRGGbRGbbR")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a null character
;; input.
(interpret-GibMeRol "eRoeRl")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-GibMeRol "eRoMMGboGGlioRliil")
