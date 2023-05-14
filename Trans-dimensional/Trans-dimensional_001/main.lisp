;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Trans-dimensional", presented by the Esolang user
;; "Dpleshkov" in the year 2017, and founded upon the manipulation of an
;; arbitrary tally of dimensions for the storage and retrieval of
;; numbers or strings.
;; 
;; 
;; Concept
;; =======
;; Programs in the Trans-dimensional programming language apply
;; themselves to the castaldy of zero or more dimensions, nevened by
;; unique identifiers during their creation, whose capacitation permits
;; their inclusion of signed integer locations, any of these a salvatory
;; to a scalar object from the set of integers, floating-point numbers,
;; and strings.
;; 
;; The language's entirety is rather curtailed in its potentials,
;; offering besides the creation, maintenance and printing of objects no
;; further commodities.
;; 
;; 
;; Architecture
;; ============
;; Trans-dimensional programs operate on a potentially infinite tally of
;; dimensions, each amenable to a unique identifier and endowed with an
;; equinumerant liberality in its positions.
;; 
;; These locations inside of a dimension respond to a signed integer
;; designator with unbounded mickleness, storing at the respective cell
;; either an integer, a floating-point number, or a string.
;; 
;; 
;; Data Types
;; ==========
;; A rather generously distributed variety of data types participates in
;; the language, comprehending unbounded signed integers, floating-point
;; numbers, and strings.
;; 
;; 
;; Syntax
;; ======
;; The Trans-dimensional programming language's syntactical department
;; utilizes statements, a subset of which employs subscripts, all
;; terminated with a semicolon.
;; 
;; == INSTRUCTIONS ==
;; Every command's introduction commences with a keyword, the procedure
;; to its succession enveloping one or more arguments, and concluding
;; with a semicolon in the desinent location.
;; 
;; Whitespaces, whose diorism embraces spaces, tabs, as well as
;; newlines, define a mandatory imposition in the tokens' intermedes,
;; exempting the semicolon's posterior portion.
;; 
;; == SUBSCRIPTS ==
;; The "get" and "put" commands request memory objects by adminiculum of
;; subscripts, compact of a dimension name and the integer-valued
;; position specifier, the compound being ensconced in a pair of
;; brackets, "[" and "]", with a single colon ":" occupying the
;; intermede.
;; 
;; == WHITESPACES ==
;; An obligation betwixt commands and their arguments, whitespaces,
;; enumerated by spaces, horizontal tabs, and linebreaks, apply in the
;; agency of requisites. At any other location, their presence and
;; quantity accompasses no significance.
;; 
;; == COMMENTS ==
;; The current language iteration remains marked by a destitution
;; concerning comments' supplementation.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) specification applies
;; to the language's donat:
;; 
;;   program       := { separator , statement , separator } ;
;;   statement     := command , ";" ;
;;   command       := createCommand
;;                 |  getCommand
;;                 |  putCommand
;;                 |  outputCommand
;;                 ;
;;   createCommand := "dimension" , name ;
;;   getCommand    := "get" , subscript ;
;;   putCommand    := "put" , subscript , "->" , expression ;
;;   outputCommand := "output" , expression ;
;;   subscript     := "[" , name , ":" , expression , "]" ;
;;   expression    := number | string | getCommand ;
;;   name          := nameChar , { nameChar } ;
;;   nameChar      := "a" ... "z" | "A" ... "Z" | digit | "_" | "-" ;
;;   string        := '"' , { character } , '"' ;
;;   number        := integer | float ;
;;   float         := [ "+" | "-" ] , digits , "." , digits ;
;;   integer       := [ "+" | "-" ] , digits ;
;;   digits        := digit , { digit } ;
;;   digit         := "0" | "1" | "2" | "3" | "4"
;;                 |  "5" | "6" | "7" | "8" | "9"
;;                 ;
;;   separator     := { whitespace } ;
;;   whitespace    := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A quadruple of commands assembles Trans-dimensional's instruction
;; set, assigned the onus of the basic tasks involving an arbitrary
;; account of dimensions' maintenance, and enhanced by an output
;; conduit.
;; 
;; == OVERVIEW ==
;; The quadruple instruction set shall now be a cursory apercu's
;; material. Please note that variable portions, designating parameters,
;; are underlined with carets ("^") as an adminiculum for distinguishing
;; from the language keywords.
;; 
;;   ------------------------------------------------------------------
;;   Command                   | Effect
;;   --------------------------+---------------------------------------
;;   dimension name            | Creates a new dimension designated by
;;             ^^^^            | the {name}.
;;                             | {name} must be a valid identifier.
;;   ..................................................................
;;   get [name : pos]          | Returns the value at the position
;;        ^^^^   ^^^           | {pos} in the dimension designated by
;;                             | the {name}.
;;                             | {name} must be a valid identifier.
;;                             | {pos} must be a signed integer.
;;   ..................................................................
;;   put [name : pos] -> value | Stores the {value} at the position
;;        ^^^^   ^^^     ^^^^^ | {pos} in the dimension designated by
;;                             | the {name}.
;;                             | {name} must be a valid identifier.
;;                             | {pos} must be a signed integer.
;;                             | {value} may be a number or a string.
;;   ..................................................................
;;   output value              | Prints to the standard output the
;;          ^^^^^              | {value}.
;;                             | {value} may be a number or a string.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The language's original specification, being rather compendious in
;; its extent, permits several ambivalences' inroad, a select of the
;; same shall be elucidated in the following.
;; 
;; == WHAT RAMIFICATIONS ENSUE FROM UNSPECIFIED ADDRESSES? ==
;; The command reference accommodated by the protolog states an
;; operation's execution in the case of a dimension's existence as well
;; as its requested position's presence, but abstains from further
;; deliberations concerning any of the predicate twain's want of
;; fulfilment.
;; 
;; Three scenarios rede their tenability:
;; 
;;   (a) An error is signaled.
;;   (b) A default value is returned.
;;   (c) The entire statement involved in the incomplete request is
;;       excalated.
;; 
;; The agon has been decided in the third option's (c) favor: The
;; complete operation whose dependency ligates it to the unserviceable
;; objective retains no effect.
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
;; Date:   2023-05-03
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
;;   [esolang2017transdimensional]
;;   The Esolang contributors, "Trans-dimensional - Esolang", 2017
;;   URL: "https://esolangs.org/wiki/Trans-dimensional"
;;   Notes:
;;     - Original specification of the Trans-dimensional esoteric
;;       programming language.
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
  "The ``list-of'' type defines a list of zero or more elemens, each
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

(deftype token-list ()
  "The ``token-list'' type defines a list of zero or more tokens."
  '(list-of Token))

;;; -------------------------------------------------------

(deftype parselet ()
  "The ``parselet'' type defines a function which accepts a
   ``Parse-State'' and returns a ``Parse-Result''.
   ---
   This functional object contributes the actual operative component of
   a ``Parser'' instance, its responsibility the discrimination betwixt
   parse states deemed responsible and such interdicted. In a conceptual
   perspective, the \"parser\" or \"combinator\" notion itself can be
   reduced to this function, without emcumbered by an object-oriented
   ensconcement as an inherent requisitum; from a vista desiderating a
   higher abstraction level, the dichotomy postulates its sensibility."
  '(function (Parse-State) Parse-Result))

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

(deftype tdobject ()
  "The ``tdobject'' type defines the objects admissible to the
   maintenance by the memory, which ultimately refers to their
   acceptance in a dimension's position."
  '(or integer float string))

;;; -------------------------------------------------------

(deftype tdobject-map ()
  "The ``tdobject-map'' type defines an association of positions in a
   dimension to the objects stored at the same locations, realized as a
   hash table of integer keys as positions that are affiliated with
   ``tdobject''s."
  '(hash-table-of integer tdobject))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines an association of a dimension identifier
   to the respective dimensions, implemented as a hash table mapping
   ``tdobject'' keys to values of the type ``Dimension''."
  '(hash-table-of tdobject Dimension))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates the data requisite for capturing a
   significant portion of Trans-dimensional source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (tested-token expected-type)
  "Determines whether the TESTED-TOKEN conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   tested-token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type tested-token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Queue".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Queue
  (:constructor make-token-queue (&rest elements)))
  "The ``Token-Queue'' implements a first-in-first-out (FIFO) storage of
   tokens."
  (elements NIL :type token-list))

;;; -------------------------------------------------------

(defun token-queue-enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the end of the QUEUE and returns no
   value."
  (declare (type Token-Queue queue))
  (declare (type Token       new-element))
  (setf (token-queue-elements queue)
        (append (token-queue-elements queue)
                (list new-element)))
  (values))

;;; -------------------------------------------------------

(defun token-queue-dequeue (queue)
  "Removes and returns the token at the front of the QUEUE, or returns
   a fresh ``:eof'' token upon its vacancy."
  (declare (type Token-Queue queue))
  (the Token
    (or (pop (token-queue-elements queue))
        (make-token :eof NIL))))

;;; -------------------------------------------------------

(defun token-queue-peek (queue)
  "Returns without removing the token at the front of the QUEUE, or
   returns a fresh ``:eof'' token upon its vacancy."
  (declare (type Token-Queue queue))
  (the Token
    (or (first (token-queue-elements queue))
        (make-token :eof NIL))))

;;; -------------------------------------------------------

(defun token-queue-copy (queue)
  "Creates and returns a new ``Token-Queue'' by copying the elements
   from the extant QUEUE.
   ---
   Please note that the QUEUE tokens themselves are not cloned, that is,
   they are shared betwixt the two token queues."
  (declare (type Token-Queue queue))
  (the Token-Queue
    (apply #'make-token-queue
      (token-queue-elements queue))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-parse-state (&optional
                                   (tokens (make-token-queue))))
  (:copier      NIL))
  "The ``Parse-State'' class encapsulates the information serving to
   delineate the parsing process' state, reflecting its progress.
   ---
   Parser combinators accept a parse state and responds with an output
   resolving to a ``Parse-Result'', either comprehending the induced
   state or, in most cases, a fresh one derived by copying the input."
  (tokens (make-token-queue) :type Token-Queue))

;;; -------------------------------------------------------

(defun parse-state-current-token (state)
  "Returns the parse STATE's current token without removing it."
  (declare (type Parse-State state))
  (the Token
    (token-queue-peek
      (parse-state-tokens state))))

;;; -------------------------------------------------------

(defun parse-state-load-next-token (state)
  "Queries the next token from the parse STATE's token queue, removes
   and returns the same."
  (declare (type Parse-State state))
  (the Token
    (token-queue-dequeue
      (parse-state-tokens state))))

;;; -------------------------------------------------------

(defun parse-state-copy (template)
  "Creates and returns a copy of the TEMPLATE parse state, comprehending
   also a copy of its ``Token-Queue''."
  (declare (type Parse-State template))
  (the Parse-State
    (make-parse-state
      (token-queue-copy
        (parse-state-tokens template)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor
    make-successful-parse-result (data state &aux (succeeded-p T)))
  (:constructor
    make-failed-parse-result (&optional (state NIL))))
  "The ``Parse-Result'' class encapsulates the data requisite for the
   definition of a parser's response to a parsing instruction.
   ---
   In the context of parser combinators, a parser or combinator accepts
   a ``Parse-State'' and responds with a ``Parse-Result'', the latter of
   which reports to the invoking instance the success or failure of the
   parser/combinator's efforts, carrying a new or the original parse
   state, and contingently enriched by the actual production of the
   parser/combinator, usually an abstract syntax tree (AST) node."
  (data        NIL :type T)
  (state       NIL :type (or null Parse-State))
  (succeeded-p NIL :type boolean))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (parselet)))
  "The ``Parser'' class represents a parser or parser combinator by
   encapsulating its ``parselet'', a function that, provided with a
   ``Parse-State'', responds with a ``Parse-Result''."
  (parselet (error "Missing parselet.") :type parselet))

;;; -------------------------------------------------------

(defun parser-parse (parser state)
  "Invokes the PARSER with the parse STATE and returns a
   ``Parse-Result'' detailing the consequence."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall
      (parser-parselet parser) state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of basic parsers and parser combinators.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun .type (expected-type)
  "Creates and returns a new parser which succeeds if the state's
   current token conforms to the EXPECTED-TYPE."
  (declare (type keyword expected-type))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State))
          (let ((current-token (parse-state-current-token state)))
            (declare (type Token current-token))
            (the Parse-Result
              (if (token-type-p current-token expected-type)
                (let ((new-parse-state (parse-state-copy state)))
                  (declare (type Parse-State new-parse-state))
                  (make-successful-parse-result
                    (parse-state-load-next-token new-parse-state)
                    new-parse-state))
                (make-failed-parse-result state))))))))

;;; -------------------------------------------------------

(defun .or (&rest choices)
  "Creates and returns a new parser which succeeds if any of its
   CHOICES, themselves parsers, succeeds, the candidates being probed in
   the order of the specification."
  (declare (type (list-of Parser) choices))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (loop
              for choice
                of-type Parser
                in      choices
              for result
                of-type Parse-Result
                =       (parser-parse choice state)
              when (parse-result-succeeded-p result)
                do (return result)
              finally
                (return
                  (make-failed-parse-result state))))))))

;;; -------------------------------------------------------

(defun .return (value)
  "Creates and returns a parser which always succeeds, returning the
   VALUE without consuming any tokens from the supplied state."
  (declare (type T value))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (the Parse-Result
            (make-successful-parse-result value state))))))

;;; -------------------------------------------------------

(defun .bind (predicate action)
  "Creates and returns a parser representative of a monadic binding, by
   applying the PREDICATE parser whose result's data, if successful, is
   handed to the ACTION, a callback that produces a consequent parser,
   the result of which, when invoked with the PREDICATE result's state,
   constitutes the actual parsing result.
   ---
   A pseudocode representation amounts to:
   
     predicateResult <- predicate.parse (state)
     
     if predicateResult.succeeded then
       consequentParser <- call action (predicateResult.data)
       return consequentParser.parse (predicateResult.state)
     else
       return failureParseResult
     end if"
  (declare (type Parser                predicate))
  (declare (type (function (*) Parser) action))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((result (parser-parse predicate state)))
            (declare (type Parse-Result result))
            (the Parse-Result
              (if (parse-result-succeeded-p result)
                (let ((consequent-parser
                        (funcall action
                          (parse-result-data result))))
                  (declare (type Parser consequent-parser))
                  (parser-parse consequent-parser
                    (parse-result-state result)))
                (make-failed-parse-result state))))))))

;;; -------------------------------------------------------

(defmacro .let ((action-parameter-variable parser) &body body)
  "Evaluates the PARSER to a ``Parser'' object, binds its result to a
   local variable designated by the ACTION-PARAMETER-VARIABLE, executes
   the BODY forms, and returns the last evaluated form's result,
   expecting it to resolve to a ``Parser'' instance.
   ---
   Conceptually this macro establishes a wrapper around the ``.bind''
   function, in particular abbreviating the parser producing callback
   ``action'' parameter by an automatic derivation from the BODY forms,
   the action function's own parameter being substituted by a local
   variable hecht ``action-parameter-variable'' for access by the BODY
   forms."
  `(.bind
     ,parser
     #'(lambda (,action-parameter-variable)
         (declare (ignorable ,action-parameter-variable))
         ,@body)))

;;; -------------------------------------------------------

(defun .chain (first-parser &rest further-parsers)
  "Creates and returns a new parser which succeeds if all of the input
   parsers, composed of the FIRST-PARSER and the optional
   FURTHER-PARSERS, match in the specified order, returning upon its
   application the last parser's data in a successful result."
  (declare (type Parser           first-parser))
  (declare (type (list-of Parser) further-parsers))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((new-state state))
            (declare (type Parse-State new-state))
            (the Parse-Result
              (loop
                for current-parser
                  of-type Parser
                  in      (append (list first-parser) further-parsers)
                for current-result
                  of-type Parse-Result
                  =       (parser-parse current-parser new-state)
                do
                  (cond
                    ((parse-result-succeeded-p current-result)
                      (setf new-state
                        (parse-result-state current-result)))
                    (T
                      (return current-result)))
                finally
                  (return current-result))))))))

;;; -------------------------------------------------------

(defun .optional (predicate default)
  "Creates and returns a new parser which attempts to match the
   PREDICATE parser, upon success returning its result; otherwise
   producing a successful result entailing the DEFAULT value without
   consuming a token."
  (declare (type Parser predicate))
  (declare (type T      default))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((predicate-result (parser-parse predicate state)))
            (declare (type Parse-Result predicate-result))
            (the Parse-Result
              (if (parse-result-succeeded-p predicate-result)
                predicate-result
                (parser-parse (.return default) state))))))))

;;; -------------------------------------------------------

(defun .one-or-more (filter)
  "Creates and returns a new parser which parses one or more tokens that
   satisfy the FILTER parser, succeeding if at the FILTER matches at
   least once, and returning a list containing the FILTER's result data
   items."
  (declare (type Parser filter))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state))
          (let ((first-result (parser-parse filter state)))
            (declare (type Parse-Result first-result))
            (the Parse-Result
              (cond
                ((parse-result-succeeded-p first-result)
                  (let ((data-items NIL)
                        (new-state  (parse-result-state first-result)))
                    (declare (type (list-of T) data-items))
                    (declare (type Parse-State new-state))
                    (push (parse-result-data first-result) data-items)
                    (loop
                      for result
                        of-type Parse-Result
                        =       (parser-parse filter new-state)
                      while
                        (parse-result-succeeded-p result)
                      do
                        (setf new-state (parse-result-state result))
                        (push (parse-result-data result) data-items)
                      finally
                        (return
                          (make-successful-parse-result
                            (nreverse data-items) new-state)))))
                (T
                  first-result))))))))

;;; -------------------------------------------------------

(defun .zero-or-more (filter)
  "Creates and returns an always succeeding parser which collects zero
   or more tokens that statisfy the FILTER parser and returns these in
   a list matching the encounters' order."
  (declare (type Parser filter))
  (the Parser
    (.optional
      (.one-or-more filter)
      NIL)))

;;; -------------------------------------------------------

(defun .between (open-guard close-guard parser)
  "Creates and returns a new parser which succeeds if the OPEN-GUARD
   matches, followed by the PARSER, and concluded with the CLOSE-GUARD,
   finally returning the PARSER's result."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser parser))
  (the Parser
    (.chain open-guard
      (.let (parser-output parser)
        (.chain close-guard
          (.return parser-output))))))

;;; -------------------------------------------------------

(defun .before (parser closure)
  "Creates and returns a parser which only succeeds if the specified
   PARSER and CLOSURE match in this exact order, returning only the
   PARSER's result, while the CLOSURE's is discarded.
   ---
   This parser combinator is beneficial in cases where a term's value
   shall be returned after a concluding suffix is ascertained, for
   instance by terminating a statement with a semicolon (\";\"); in such
   circumstances the statement's parsed output shall be returned, but
   the semicolon's presence ought to be assayed first, without this
   terminator providing any useful result itself.
   ---
   Example --- Checks whether a statement is concluded by a semicolon:
     (.before
       (parse-statement)
       (.type :semicolon))"
  (declare (type Parser parser))
  (declare (type Parser closure))
  (the Parser
    (.chain
      (.let (parser-output parser)
        (.chain closure
          (.return parser-output))))))

;;; -------------------------------------------------------

(defun .fail (datum &rest arguments)
  "Creates and returns a parser which always fails, signaling in the
   process an error composed of the DATUM and the ARGUMENTS, as conforms
   to the ``error'' function signature."
  (declare (type T           datum))
  (declare (type (list-of T) arguments))
  (the Parser
    (make-parser
      #'(lambda (state)
          (declare (type Parse-State state)
                   (ignore           state))
          (apply #'error datum arguments)))))

;;; -------------------------------------------------------

(defmacro define-parser (name &body parsers)
  "Establishes a niladic function of the specified NAME which, when
   invoked, creates a new parser composed of the chained PARSERS, that
   is, the thus produced parser only succeeds if all of its constituents
   in this order match, otherwise failing."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    `(defun ,name ()
       (the Parser
         (make-parser
           #'(lambda (,state-variable)
               (declare (type Parse-State ,state-variable))
               (the Parse-Result
                (parser-parse
                  (.chain ,@parsers) ,state-variable))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse function.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-token-queue (parser tokens)
  "Assembles an abstract syntax tree (AST) from the token-queue TOKENS
   and returns the product."
  (declare (type Parser      parser))
  (declare (type Token-Queue tokens))
  (let ((initial-state (make-parse-state tokens)))
    (declare (type Parse-State initial-state))
    (let ((result (parser-parse parser initial-state)))
      (declare (type Parse-Result result))
      (the T
        (if (parse-result-succeeded-p result)
          result
          (error "Parser failed with the result ~s." result))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent for an
   identifier name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (find candidate "-_" :test #'char=))))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign,
   that is, either a plus (\"+\") or minus (\"-\"), returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defstruct (Lexer
  (:constructor
    make-lexer (source
                &aux (position 0)
                     (character
                       (when (array-in-bounds-p source position)
                         (char source position))))))
  "The ``Lexer'' class represents an entity whose capacitation refers to
   the extraction of tokens from a piece of Trans-dimensional source
   code."
  (source    (error "Missing lexer source.") :type string)
  (position  0                               :type fixnum)
  (character NIL                             :type (or null character)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next location in its source,
   if possible, updates the LEXER's current state, and returns no
   value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char (lexer-source lexer)
        (incf (lexer-position lexer)))))
  (values))

;;; -------------------------------------------------------

(defun lexer-peek (lexer)
  "Returns the character at the location immediately succeeding the
   LEXER's current position in its source, or ``NIL'' if the index
   transgresses the boundaries."
  (declare (type Lexer lexer))
  (the (or null character)
    (when (array-in-bounds-p (lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char (lexer-source lexer)
        (1+ (lexer-position lexer))))))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token associated with the IDENTIFIER, either delivering a
   Trans-dimensional keyword representation or, if none can matched,
   responding with a generic ``:identifier'' token."
  (declare (type string identifier))
  (the Token
    (cond
      ((string= identifier "dimension")
        (make-token :dimension identifier))
      ((string= identifier "get")
        (make-token :get identifier))
      ((string= identifier "put")
        (make-token :put identifier))
      ((string= identifier "output")
        (make-token :output identifier))
      (T
        (make-token :identifier identifier)))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Commencing at the current position into the LEXER's source, reads an
   identifier name and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier-token
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop
          while (and (lexer-character lexer)
                     (identifier-character-p (lexer-character lexer)))
          do
            (write-char (lexer-character lexer) identifier)
            (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Commencing at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent whitespaces, and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character lexer)
               (whitespace-character-p (lexer-character lexer)))
    do
      (lexer-advance lexer))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Consumes the character at the LEXER's current position into its
   source and returns a new token associating the TOKEN-TYPE with the
   character object as its value."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (prog1
      (make-token token-type (lexer-character lexer))
      (lexer-advance lexer))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Commencing at the current position into the LEXER' source, reads a
   string demarcated by double quotes ('\"') and returns a ``:string''
   token representation thereof."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop do
          (case (lexer-character lexer)
            ((NIL)
              (error "Unterminated string literal."))
            (#\"
              (lexer-advance lexer)
              (loop-finish))
            (otherwise
              (write-char (lexer-character lexer) content)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Commencing at the current position into the LEXER's source, reads an
   integer or floating-point number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (let ((token-type :integer))
    (declare (type (member :integer :float) token-type))
    
    (with-open-stream (digits (make-string-output-stream))
      (declare (type string-stream digits))
      
      (labels
          ((write-optional-sign ()
            "Determines whether the LEXER's current character represents
             an arithmetic sign, on confirmation writing the same to the
             DIGITS stream ere advancing the LEXER; in any case
             returning no value."
            (when (and (lexer-character lexer)
                       (sign-character-p (lexer-character lexer)))
              (write-char (lexer-character lexer) digits)
              (lexer-advance lexer))
            (values))
           
           (expect-digit ()
            "Determines whether the LEXER's current character represents
             a decimal digit, on confirmation returning no value,
             otherwise signaling an error of an unspecified type."
            (unless (and (lexer-character lexer)
                         (digit-char-p (lexer-character lexer)))
              (error "Expected a decimal digit, but encountered ~s ~
                      at position ~d."
                (lexer-character lexer)
                (lexer-position  lexer)))
            (values))
           
           (write-one-or-more-digits ()
            "Commencing at the LEXER's current position, writes a
             sequence of one or more accolent decimal digits to the
             DIGITS stream and returns no value.
             ---
             If no digits at all can be detected, an error of an
             unspecified type is signaled."
            (expect-digit)
            (loop
              while (and (lexer-character lexer)
                         (digit-char-p (lexer-character lexer)))
              do
                (write-char (lexer-character lexer) digits)
                (lexer-advance lexer))
            (values))
           
           (probe-for-decimal-separator ()
            "Determines whether the LEXER's current character represents
             a decimal separator, that is, the dot \".\", on
             confirmation updating the TOKEN-TYPE to ``:float'' and
             advancing the position cursor, ere returning no value,
             otherwise simply resorting to this desinent step."
            (when (and (lexer-character lexer)
                       (char= (lexer-character lexer) #\.))
              (setf token-type :float)
              (write-char (lexer-character lexer) digits)
              (lexer-advance lexer))
            (values)))
        
        (write-optional-sign)
        (write-one-or-more-digits)
        (probe-for-decimal-separator)
        
        ;; Decimal point found?
        ;; => Read fractional digits.
        (when (eq token-type :float)
          (write-one-or-more-digits))
        
        (the Token
          (make-token token-type
            (case token-type
              (:integer
                (parse-integer
                  (get-output-stream-string digits)))
              (:float
                (read-from-string
                  (get-output-stream-string digits)))
              (otherwise
                (error "Invalid token type: ~s." token-type)))))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to every query with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-token :eof NIL))
      
      ((whitespace-character-p (lexer-character lexer))
        (lexer-skip-whitespaces lexer)
        (lexer-get-next-token   lexer))
      
      ((alpha-char-p (lexer-character lexer))
        (lexer-read-identifier lexer))
      
      ((or (digit-char-p (lexer-character lexer))
           (char= (lexer-character lexer) #\+))
        (lexer-read-number lexer))
      
      ((char= (lexer-character lexer) #\-)
        (case (lexer-peek lexer)
          ;; Standalone hyphens are not permitted.
          ((NIL)
            (error "Invalid standalone symbol \"-\" at position ~d."
              (lexer-position lexer)))
          ;; Arrow ("->") follows?
          (#\>
            ;; Skip hyphen ("-").
            (lexer-advance lexer)
            ;; Skip greater-than sign (">").
            (lexer-advance lexer)
            (make-token :arrow "->"))
          ;; Digit follows? => Read number.
          (otherwise
            (lexer-read-number lexer))))
      
      ((char= (lexer-character lexer) #\")
        (lexer-read-string lexer))
      
      ((char= (lexer-character lexer) #\;)
        (lexer-read-symbol lexer :semicolon))
      
      ((char= (lexer-character lexer) #\:)
        (lexer-read-symbol lexer :colon))
      
      ((char= (lexer-character lexer) #\[)
        (lexer-read-symbol lexer :left-bracket))
      
      ((char= (lexer-character lexer) #\])
        (lexer-read-symbol lexer :right-bracket))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          (lexer-character lexer) (lexer-position lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor make-node (type &rest attributes)))
  "The ``Node'' class represents an abstract syntax tree (AST) node by
   encapsulating the requisite information for a Trans-dimensional
   language facility's delineation."
  (type       (error "Missing node type.") :type keyword)
  (attributes NIL                          :type (list-of T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of parsers and combinators.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () Parser) expression-parser))

;;; -------------------------------------------------------

(define-parser identifier-parser
  (.or
    (.let (name (.type :identifier))
      (.return
        (make-node :string
          (token-value name))))
    (.let (integer (.type :integer))
      (.return
        (make-node :integer
          (token-value integer))))
    (.fail
      "No valid identifier name.")))

;;; -------------------------------------------------------

;; createCommand := "dimension" , name , ";" ;
(define-parser dimension-parser
  (.chain (.type :dimension)
    (.let (name (identifier-parser))
      (declare (type Node name))
      (.return
        (make-node :dimension name)))))

;;; -------------------------------------------------------

(define-parser integer-parser
  (.let (integer-token (.type :integer))
    (.return
      (make-node :integer
        (token-value integer-token)))))

;;; -------------------------------------------------------

(define-parser float-parser
  (.let (integer-token (.type :float))
    (.return
      (make-node :float
        (token-value integer-token)))))

;;; -------------------------------------------------------

;; string := '"' , { character } , '"" ;
(define-parser string-parser
  (.let (string-token (.type :string))
    (.return
      (make-node :string
        (token-value string-token)))))

;;; -------------------------------------------------------

;; subscript := "[" , name , ":" , expression , "]" ;
(define-parser subscript-parser
  (.between
    (.type :left-bracket)
    (.type :right-bracket)
    (.let (identifier (identifier-parser))
      (declare (type Node identifier))
      (.chain
        (.type :colon)
        (.let (index (expression-parser))
          (declare (type Node index))
          (.return
            (make-node :subscript identifier index)))))))

;;; -------------------------------------------------------

;; getCommand := "get" , subscript ;
(define-parser get-parser
  (.chain
    (.type :get)
    (.let (subscript (subscript-parser))
      (declare (type Node subscript))
      (.return
        (make-node :get subscript)))))

;;; -------------------------------------------------------

;; expression := number | string | getCommand ;
(define-parser expression-parser
  (.or (get-parser)
       (integer-parser)
       (float-parser)
       (string-parser)))

;;; -------------------------------------------------------

;; putCommand := "put" , subscript , "->" , expression ;
(define-parser put-parser
  (.chain
    (.type :put)
    (.let (subscript (subscript-parser))
      (declare (type Node subscript))
      (.chain
        (.type :arrow)
        (.let (value (expression-parser))
          (declare (type Node value))
          (.return
            (make-node :put subscript value)))))))

;;; -------------------------------------------------------

;; outputCommand := "output" , expression ;
(define-parser output-parser
  (.chain
    (.type :output)
    (.let (expression (expression-parser))
      (declare (type Node expression))
      (.return
        (make-node :output expression)))))

;;; -------------------------------------------------------

(define-parser command-parser
  (.before
    (.or (dimension-parser)
         (get-parser)
         (put-parser)
         (output-parser))
    (.type :semicolon)))

;;; -------------------------------------------------------

(define-parser program-parser
  (.let (statements
          (.before
            (.zero-or-more
              (command-parser))
            (.type :eof)))
    (.return
      (make-node :program statements))))
    
;;; -------------------------------------------------------

(defun collect-tokens-into-queue (lexer)
  "Collects the LEXER's token into a ``Token-Queue'' and returns the
   same."
  (declare (type Lexer lexer))
  (the Token-Queue
    (loop
      for     token of-type Token = (lexer-get-next-token lexer)
      until   (token-type-p token :eof)
      collect token into tokens
      finally (return (apply #'make-token-queue tokens)))))

;;; -------------------------------------------------------

(defun parse-lexer-tokens (lexer)
  "Assembles an abstract syntax tree (AST) from the LEXER's tokens and
   returns the product."
  (declare (type Lexer lexer))
  (let ((result
          (parse-token-queue
            (program-parser)
            (collect-tokens-into-queue lexer))))
    (declare (type Parse-Result result))
    (the Node
      (if (parse-result-succeeded-p result)
        (parse-result-data result)
        (error "Parser failed: ~s." result)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Dimension".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Dimension
  (:constructor make-dimension))
  "The ``Dimension'' class models a dimension as a theoretically
   infinite extent, each integer-keyed location of which stores an
   aefauld datum."
  (elements (make-hash-table :test #'eql) :type tdobject-map))

;;; -------------------------------------------------------

(defun dimension-element-at (dimension position)
  "Returns the element at the DIMENSION's POSITION, or ``NIL'' if none
   such object is explicitly specified."
  (declare (type Dimension dimension))
  (declare (type integer   position))
  (the (or null tdobject)
    (gethash position (dimension-elements dimension))))

;;; -------------------------------------------------------

(defun (setf dimension-element-at) (new-element dimension position)
  "Stores the NEW-ELEMENT in the DIMENSION at the specified POSITION and
   returns no value."
  (declare (type tdobject  new-element))
  (declare (type Dimension dimension))
  (declare (type integer   position))
  (setf (gethash position (dimension-elements dimension)) new-element)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (dimension index)))
  "The ``Location'' class specifies a position in the memory, also known
   as a \"subscript\", compact of a referenced dimension identification
   and the position along the same."
  (dimension (error "Missing dimension.") :type tdobject)
  (index     (error "Missing index.")     :type integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns a new empty memory."
  (the memory
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun memory-contains-dimension-p (memory name)
  "Determines whether the MEMORY maintains a dimension amenable to the
   NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type memory   memory))
  (declare (type tdobject name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name memory))))))

;;; -------------------------------------------------------

(defun memory-ensure-dimension (memory name)
  "Ascertains the presence of a dimension nevened by the NAME in the
   MEMORY through the creation of the same in the case of its absence,
   and returns either the newly generated dimension or the already
   present instance."
  (declare (type memory   memory))
  (declare (type tdobject name))
  (multiple-value-bind (dimension contains-dimension-p)
      (gethash name memory)
    (declare (type (or null Dimension) dimension))
    (declare (type T                   contains-dimension-p))
    (unless contains-dimension-p
      (setf dimension             (make-dimension))
      (setf (gethash name memory) dimension))
    (the Dimension dimension)))

;;; -------------------------------------------------------

(defun memory-create-dimension (memory name)
  "Creates a new dimension of the NAME in the MEMORY, if not already
   extant, and returns no value."
  (declare (type memory   memory))
  (declare (type tdobject name))
  (memory-ensure-dimension memory name)
  (values))

;;; -------------------------------------------------------

(defun memory-element-at (memory location)
  "Returns the object in the MEMORY specified by the LOCATION."
  (declare (type memory   memory))
  (declare (type Location location))
  (multiple-value-bind (dimension contains-dimension-p)
      (gethash (location-dimension location) memory)
    (declare (type (or null Dimension) dimension))
    (declare (type T                   contains-dimension-p))
    (the (or null tdobject)
      (when contains-dimension-p
        (dimension-element-at dimension
          (location-index location))))))

;;; -------------------------------------------------------

(defun (setf memory-element-at) (new-element memory location)
  "Stores the NEW-ELEMENT in the MEMORY at the LOCATION, contingently
   superseding an already extant datum, and returns no value."
  (declare (type tdobject new-element))
  (declare (type memory   memory))
  (declare (type Location location))
  (setf
    (dimension-element-at
      (memory-ensure-dimension memory
        (location-dimension location))
      (location-index location))
    new-element)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class applies itself to the evaluation of an
   abstract syntax tree (AST) in order to accompass its effects."
  (tree   (error "Missing interpreter AST.") :type Node)
  (memory (make-hash-table :test #'equal)    :type memory))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE, dispatched on the NODE-TYPE, employing the
     INTERPRETER, and returns a value appropriate for this purpose."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Visits the NODE employing the INTERPRETER, internally dispatching on
   the NODE type by invocation of the most eligible
   ``interpreter-dispatch-node'' generic function implementation, and
   returns a value appropriate for this purpose."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (interpreter-dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :program))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (dolist (statement (first (node-attributes node)))
    (declare (type Node statement))
    (interpreter-visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :string))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the string
    (first (node-attributes node))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :integer))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the integer
    (first (node-attributes node))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :float))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the real
    (first (node-attributes node))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :subscript))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (the Location
    (make-location
      (interpreter-visit-node interpreter
        (first  (node-attributes node)))
      (interpreter-visit-node interpreter
        (second (node-attributes node))))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :dimension))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((dimension-name
          (interpreter-visit-node interpreter
            (first (node-attributes node)))))
    (declare (type tdobject dimension-name))
    (memory-create-dimension
      (interpreter-memory interpreter) dimension-name))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :get))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((location
          (interpreter-visit-node interpreter
            (first (node-attributes node)))))
    (declare (type Location location))
    (the (or null tdobject)
      (memory-element-at
        (interpreter-memory interpreter)
        location))))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :put))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (destructuring-bind (subscript-node value-node)
      (node-attributes node)
    (declare (type Node subscript-node))
    (declare (type Node value-node))
    (let ((new-value (interpreter-visit-node interpreter value-node)))
      (declare (type (or null tdobject) new-value))
      (when new-value
        (setf
          (memory-element-at
            (interpreter-memory interpreter)
            (interpreter-visit-node interpreter subscript-node))
          new-value))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-dispatch-node ((interpreter Interpreter)
                                      (node-type   (eql :output))
                                      (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type Node        node))
  (let ((value
          (interpreter-visit-node interpreter
            (first (node-attributes node)))))
    (declare (type (or null tdobject) value))
    (when value
    (format T "~&~a" value)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the Trans-dimensional program maintained by the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Trans-dimensional (code)
  "Interprets the piece of Trans-dimensional source CODE and returns no
   value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parse-lexer-tokens
        (make-lexer code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Trans-dimensional
  "dimension X;
   put [X:1] -> \"Hello, World!\";
   output get [X:1];")

;;; -------------------------------------------------------

(interpret-Trans-dimensional
  "dimension A;
   put [A:1] -> -0.5;
   output get [A:1];")

;;; -------------------------------------------------------

(interpret-Trans-dimensional
  "dimension X;
   dimension Y;
   put [X : 1] -> \"X1\";
   put [Y : 2] -> get [X : 1];
   output get [Y : 2];")

;;; -------------------------------------------------------

(interpret-Trans-dimensional
  "dimension 100;
   put [100:1] -> -8;
   output get [100:1];")

;;; -------------------------------------------------------

(interpret-Trans-dimensional
  "dimension X;
   output get [X:1];")
