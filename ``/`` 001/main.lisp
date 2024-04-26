;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "``", invented by the Esolang user "Xyzzy" and presented on
;; February 19th, 2023, the proprium of its diorism relates to its
;; instructions' molding by weftages that align via the backtick symbol
;; "`", also norned "grave" or "grave accent", in order to attend to the
;; telos of two registers and an infinite integer array's perquisition
;; and manipulation.
;; 
;; 
;; Concept
;; =======
;; The `` programming language's kenspeckle design wones in the mode of
;; its instructions' expression, bedighting the same in concord with an
;; ordonnance the derivation of whom constitutes a dependency upon one
;; or more backtick ("`") characters and an occasion unsigned integer
;; operand.
;; 
;; == THE BACKTICK: A INDICIUM OF DISTINGUISHMENT ==
;; The conspicable element of any `` program, the "`" character in its
;; tally, spatiality, and intercourse with decimal digits applies itself
;; to the latreutical dever of the several operations' distinguishment.
;; 
;; == THE REGISTER TWISSEL: TWO SALVATORIES FOR INTEGER SCALARS ==
;; The paravaunt salvatory nuncupated to the castaldy of data is
;; realized in the twain of registers, the agnomination for whom issues
;; from the reserve of "a" and "b", in their capacity governed by such
;; equiparation's purview as to admit an aefault signed integer's
;; accommodation.
;; 
;; Maugre the modesty invested in their componency's tally, the
;; registers, in particular the specimen "a", enjoy the excellent role
;; in the data management, permitting by the intercourse with both the
;; their internal gremial exchange and the cell array's transfer, as
;; as well the communication competences along the input and output
;; conduits, the premier warklume's attribution.
;; 
;; == THE CELL ARRAY: AN INFINITE EXPANSION OF INTEGER NUMBERS ==
;; An equipoise to its mickleness, the infinite cell array partakes of
;; a parhedral agency only, homologating signed integer subscripts'
;; affiliations with values desumed from the selfsame commorancy.
;; 
;; 
;; Syntax
;; ======
;; The syntactical aspect of its presentation is governed in `` by the
;; backtick ("`") symbol and its daimen interplay with arguments, the
;; former's distribution entalented with the potential of the
;; operations' identification.
;; 
;; Each twissel of accolent instructions' intermede ought to be attended
;; to by at least one semicolon (";"), with whitespaces an implement of
;; tolerable nature betwixt tokens --- not inwith, however ---, and
;; administered an equipollence of insignificance.
;; 
;; 
;; Instructions
;; ============
;; The `` programming language wist of an ennead's membership anenst its
;; instructions, in a few cases reliant upon an unsigned integer
;; argument, and invested with competences whose perimeter's
;; amplectation exhausts the manipulation and commerce of the registers
;; and the cell tape, conditional instruction omission, an unconditional
;; jump facility, as well as input and output communication.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's adhibition shall be the following
;; apercu's dation.
;; 
;; Please heed the demarcation of succedaneous segments by an underline
;; compact of asterisks ("*"), such levy the requisite for their
;; supersession by actual `` code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ------------------------------------------------------------------
;;   `0      | Restores the register "a" to its default value of
;;           | zero (0)>
;;   ..................................................................
;;   `n      | If the value {n} constitutes an positive integer number
;;    *      | greater than or equal to one (1), increments the
;;           | register "a" by that amount; otherwise, if {n} is
;;           | omitted, decrement the register "a" by one (1).
;;           |---------------------------------------------------------
;;           | {n} must be a positive integer number greater than or
;;           | equal to one (1), or must be entirely omitted.
;;   ..................................................................
;;   `0`     | Copies the value of the register "a" to register "b",
;;           | while abstaining from the former register's
;;           | modification.
;;   ..................................................................
;;   ``0     | Sets the cell pointer to the value stored in the
;;           | register "a", and subsequently stores the value under
;;           | the new cell pointer position in the register "a".
;;           |---------------------------------------------------------
;;           | In a pseudocode diction it holds:
;;           |   cellPointer <- register(a)
;;           |   register(a) <- cells[cellPointer]
;;   ..................................................................
;;   ``1     | Sets the cell pointer to the value stored in the
;;           | register "b", and subsequently copies the value of the
;;           | register "b" into the cell under the new cell pointer
;;           | position.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction it holds:
;;           |   cellPointer        <- register(b)
;;           |   cells[cellPointer] <- register(b)
;;   ..................................................................
;;   `1`     | If the register "a" contains a value of zero (0), skips
;;           | the subsequent command; otherwise proceeds as usual.
;;   ..................................................................
;;   `-n     | Relocates the instruction pointer (IP) to the command
;;     *     | amenable to the position {n}.
;;           |---------------------------------------------------------
;;           | The command indices commence with one (1).
;;           |---------------------------------------------------------
;;           | {n} must be a positive integer number greater than or
;;           | equal to one (1).
;;   ..................................................................
;;   ``      | Prints the character whose Unicode code point
;;           | corresponds to the value of the register "a" to the
;;           | standard output.
;;   ..................................................................
;;   ```     | Queries the standard input for a Unicode character and
;;           | tranfers its code point into the register "a".
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of a vector of commands
;; from a sequence of tokens.
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
;; Date:   2024-04-21
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
;;   [englishclub2024floors]
;;   The EnglishClub contributors, "Floors of a House", 2024
;;   URL: "https://www.englishclub.com/vocabulary/floors-house.php"
;;   Notes:
;;     - Visualizes the conceptual design of a house's floors.
;;     - Emphasizes the discrepancies in American and British English.
;;       o In particular, the floor at land level is stevened the
;;         "ground floor" in British English, while concomitantly the
;;         American English terminology assigns "first floor" to it.
;;       o The "Thief, Police and the Building" programming language
;;         complies with the British nomenclature.
;;   
;;   [esolang2023``]
;;   The Esolang contributors, "``", June 27th, 2023
;;   URL: "https://esolangs.org/wiki/%60%60"
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
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-derived-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type norned by the TYPE-NAME, its formal parameters
   being appropriated ipsissima verba from the LAMBDA-LIST, and which
   probes the quesited object by the agnomination provided through the
   CANDIDATE-VARIABLE symbol, evaluating the BODY forms, and construing
   the desinent form's primary return value as a
   \"generalized boolean\", siccan ought to respond with a \"true\"
   sentinel for the candidate's eligiblity, otherwise with \"false\".
   ---
   The first BODY form, upon its establishment of a string object, is
   subjected to an interpretation as the derived type's documentation
   string, and, as a corollary, reappropriated for this sole purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(defun accepts-any-object-p (object)
  "Determines whether the OBJECT represents the generic type sentinel
   ``*'', the same homologates any object's participation, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null
      (and (symbolp object)
           (eq      object '*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype register-name ()
  "The ``register-name'' type enumerates the recognized register names."
  '(member :register-a :register-b))

;;; -------------------------------------------------------

(define-derived-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the generic sentinel
   ``*''."
  (and
    (listp candidate)
    (or
      (accepts-any-object-p element-type)
      (loop
        for    element of-type T in (the list candidate)
        always (typep element element-type)))))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list compact of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable `` program as a
   one-dimensional simple array of ``Command'' objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(define-derived-type hash-table-of (candidate
                                    &optional (key-type   '*)
                                              (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, the keys of which comply to the KEY-TYPE, the values to
   the VALUE-TYPE, both defaulting to the generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and
          (or (accepts-any-object-p key)
              (typep                key key-type))
          (or (accepts-any-object-p value)
              (typep                value value-type))))))

;;; -------------------------------------------------------

(deftype cell-vector ()
  "The ``cell-vector'' type defines a sparse vector of integer-valued
   cells, amenable to signed integer indices, and realized by
   adminiculum of a hash table, the keys of which correspond to the
   subscripts, while the values answer to the cell values."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   enclosing in its diorism the space, horizontal tab, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface serves in a common foundry's furnishment
   for the purpose of encapsulating a `` operation.")

;;; -------------------------------------------------------

(defstruct (Reset-Command
  (:include Command))
  "The ``Reset-Command'' class is endowed with that dever to represent
   a request for the \"a\" register default state's restoration,
   expressed in the `` programming language by the forbisen \"`0\".")

;;; -------------------------------------------------------

(defstruct (Increment-Command
  (:include Command))
  "The ``Increment-Command'' class is endowed with that dever to
   represent a request for the \"a\" register incrementation or
   decrementation, expressed in the `` programming language by the
   forbisen \"`{n}\" or \"`\"."
  (amount (error "Missing amount.")
          :type      integer
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Copy-Register-Command
  (:include Command))
  "The ``Copy-Register-Command'' class is endowed with that dever to
   represent a request for the \"a\" register content's copying to the
   \"b\" register, expressed in the `` programming language by the
   forbisen \"`0`\".")

;;; -------------------------------------------------------

(defstruct (Set-Cell-Pointer-Command
  (:include Command))
  "The ``Set-Pointer-Command'' class is endowed with that dever to
   represent a request for the cell pointer's commerce with one of the
   registers, expressed in the `` programming language by the forbisens
   \"``0\" and \"``1\"."
  (register (error "Missing register.")
            :type      register-name
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Skip-Command
  (:include Command))
  "The ``Skip-Command'' class is endowed with that dever to represent a
   request for the subsequent command's omission upon the \"a\"
   register value's equality to zero (0), expressed in the ``
   programming language by the forbisen \"`1`\".")

;;; -------------------------------------------------------

(defstruct (Jump-Command
  (:include Command))
  "The ``Jump-Command'' class is endowed with that dever to represent a
   request for instruction pointer's (IP) relocation, expressed in the
   `` programming language by the forbisens \"`-{n}\"."
  (destination (error "Missing line number.")
               :type      integer
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Command
  (:include Command))
  "The ``Output-Command'' class is endowed with that dever to represent
   a request for the \"a\" register content's printing in the form of
   its affiliated Unicode character, expressed in the `` programming
   language by the forbisen \"``\".")

;;; -------------------------------------------------------

(defstruct (Input-Command
  (:include Command))
  "The ``Input-Command'' class is endowed with that dever to represent a
   request for the \"a\" register's reception of a user input character,
   expressed in the `` programming language by the forbisen \"```\".")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of `` source code."
  (type  (error "Missing token type.")
         :type      keyword
         :read-only T)
  (value (error "Missing token value.")
         :type      T
         :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Lexer) (values)) advance-to-next-token))
(declaim (ftype (function (Lexer) (values)) skip-whitespaces))
(declaim (ftype (function (Lexer) (values)) update-lexer-character))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for the lexer.")
    :type          string
    :documentation "The piece of `` source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class serves in the extraction of significant
     objects from a piece of `` source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Sets the LEXER's internally managed character to the first one in
   its source and returns no value."
  (declare (type Lexer lexer))
  (update-lexer-character lexer)
  (skip-whitespaces       lexer)
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' nuncupated to the `` SOURCE
   code's analyzation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun current-character-equals-p (lexer expected-character)
  "Determines whether the LEXER's current character equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   An exhausted lexer, such maintains a ``NIL''-valued current
   character, always responds with ``NIL'' without the
   EXPECTED-CHARACTER's prior interrogation."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the boolean
      (when character
        (not (null
          (char= character expected-character)))))))

;;; -------------------------------------------------------

(defun current-character-satisfies-p (lexer predicate)
  "Determines whether the LEXER's current character satisfies the
   PREDICATE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   An exhausted lexer, such maintains a ``NIL''-valued current
   character, always responds with ``NIL'' without the PREDICATE's prior
   interrogation."
  (declare (type Lexer                    lexer))
  (declare (type (function (character) *) predicate))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the boolean
      (when character
        (not (null
          (funcall predicate character)))))))

;;; -------------------------------------------------------

(defun update-lexer-character (lexer)
  "Updates the LEXER's internally managed current character and
   returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position) lexer
    (declare (type string source))
    (declare (type fixnum position))
    (when (array-in-bounds-p source position)
      (incf position)))
  (update-lexer-character lexer)
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces, and returns no
   value."
  (declare (type Lexer lexer))
  (loop
    while (current-character-satisfies-p lexer #'whitespace-character-p)
    do    (advance-to-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-token (lexer)
  "Relocates the LEXER's position cursor to the next token's
   incipiency and returns no value."
  (declare (type Lexer lexer))
  (advance-to-next-character lexer)
  (skip-whitespaces          lexer)
  (values))

;;; -------------------------------------------------------

(defun count-backticks (lexer)
  "Proceeding from the current position into the LEXER's source,
   tallies the number of accolent backtick (\"`\") and returns this
   account."
  (declare (type Lexer lexer))
  (the (integer 0 *)
    (loop
      while (current-character-equals-p lexer #\`)
      do    (advance-to-next-character  lexer)
      count 1)))

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
          (with-slots (character) lexer
            (declare (type (or null character) character))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (advance-to-next-character lexer))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (the Token
      (case character
        ((NIL)
          (make-eof-token))
        ((#\Newline #\Space #\Tab)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        (#\`
          (let ((number-of-backticks (count-backticks lexer)))
            (declare (type (integer 0 *) number-of-backticks))
            (case number-of-backticks
              (1 (make-token :single-backtick 1))
              (2 (make-token :double-backtick 2))
              (3 (make-token :triple-backtick 3))
              (otherwise
                (error "Invalid number of backticks: ~d."
                  number-of-backticks)))))
        (#\-
          (prog1
            (make-token :hyphen character)
            (advance-to-next-token lexer)))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (read-number lexer))
        (#\;
          (prog1
            (make-token :semicolon character)
            (advance-to-next-token lexer)))
        (otherwise
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-State".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-initial-parse-state (lexer))
  (:constructor advance-parse-state
    (current-state
     &aux (lexer  (parse-state-lexer  current-state))
          (tokens (parse-state-tokens current-state))
          (cursor (1+ (parse-state-cursor current-state))))))
  "The ``Parse-State'' class serves in the encapsulation of the parsing
   process' advancement via its castaldy of the already consumed tokens
   from the underlying lexer, both shared among all instances, and a
   ``Parse-State'' object's personal index into this collection."
  (lexer  (error "Missing lexer for parse state.")
          :type      Lexer
          :read-only T)
  (tokens (make-array 0
            :element-type    'Token
            :initial-element (make-eof-token)
            :adjustable      T
            :fill-pointer    0)
          :type      (vector Token *)
          :read-only T)
  (cursor 0
          :type      fixnum
          :read-only NIL))

;;; -------------------------------------------------------

(defun load-token-if-necessary (parse-state)
  "Determines whether the PARSE-STATE's cursor into the shared token
   buffer constitutes a valid designator inwith the currently demarcated
   bournes, upon its refutation querying the next token from the
   underlying lexer and appending the same to the buffer, in any case
   returning no value."
  (declare (type Parse-State parse-state))
  (when (>= (parse-state-cursor parse-state)
            (fill-pointer (parse-state-tokens parse-state)))
    (vector-push-extend
      (get-next-token
        (parse-state-lexer parse-state))
      (parse-state-tokens parse-state)))
  (values))

;;; -------------------------------------------------------

(defun get-current-token (parse-state)
  "Returns the token affiliated with the PARSE-STATE."
  (declare (type Parse-State parse-state))
  (load-token-if-necessary parse-state)
  (the Token
    (aref
      (parse-state-tokens parse-state)
      (parse-state-cursor parse-state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parse-Result".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class encapsulates a ``Parser'''s response to a
   parse request, committed via a ``Parse-State''."
  (succeeded-p (error "Missing success/failure flag.")
               :type      boolean
               :read-only T)
  (state       (error "Missing result state.")
               :type      Parse-State
               :read-only T)
  (output      (error "Missing result output.")
               :type      T
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class serves in the implementation of a parser or
   combinator, satisfying its dever by a callback function's adminicle."
  (processor (error "Missing parser processor.")
             :type      (function (Parse-State) Parse-Result)
             :read-only T))

;;; -------------------------------------------------------

(defun apply-parser (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   encapsulating its response."
  (the Parse-Result
    (funcall
      (parser-processor parser)
      state)))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Accommodates a convenience service for the creation of a fresh
   ``Parser'' instance, the processor callback function of which is
   defined in an implicit fashion, admitting as its aefauld input a
   ``Parse-State'' nevened via the STATE-VARIABLE, and employing for its
   implementations the BODY forms, the desinent one of which is expected
   to return a ``Parse-Result'' that encapsulates the thus produced
   parser's response to an invocation request."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))

;;; -------------------------------------------------------

(defmacro define-parser (&rest parsers)
  "Returns a fresh ``Parser'' as a ``chain-of'' composition of input
   PARSERS, hence returning the desinent one's parse result."
  (let ((state-variable (gensym)))
    (declare (type symbol state-variable))
    `(the Parser
       (build-parser (,state-variable)
         (the Parse-Result
           (apply-parser
             (chain-of ,@parsers)
             ,state-variable))))))

;;; -------------------------------------------------------

(defun satisfies-token-predicate (predicate)
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   satisfies the PREDICATE, returning on confirmation in its parse
   result the probed token."
  (declare (type (function (Token) *) predicate))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (let ((probed-token (get-current-token state)))
          (declare (type Token probed-token))
          (if (funcall predicate probed-token)
            (make-parse-result T
              (advance-parse-state state)
              probed-token)
            (make-parse-result NIL state probed-token)))))))

;;; -------------------------------------------------------

(defmacro token-which-satisfies ((token-variable) &body body)
  "Returns a fresh ``Parser'' which succeeds if its input state's token
   satisfies an imposed requirement, accomplishing this by binding the
   probed token to the TOKEN-VARIABLE name and evaluating the BODY
   forms, the desinent form determining whether the predicate is
   satisfied, a non-``NIL'' value communicating it affirmation, ``NIL''
   the refutation, the parser on confirmation returning in its parse
   result the probed token."
  `(the Parser
     (satisfies-token-predicate
       #'(lambda (,token-variable)
           (declare (type Token ,token-variable))
           (declare (ignorable  ,token-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun token-of-type (expected-token-type)
  "Returns a fresh ``Parser'' which succeeds if its input parse state's
   token type matches the EXPECTED-TOKEN-TYPE, returning upon
   confirmation in its parse result the probed token."
  (declare (type keyword expected-token-type))
  (the Parser
    (token-which-satisfies (probed-token)
      (token-type-p probed-token expected-token-type))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a fresh ``Parser'' which succeeds if all of its input
   PARSERS, in the exact order of their specification, match, returning
   on confirmation the desinent parser's result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       state
            then    (parse-result-state current-result)
          
          for current-parser
            of-type Parser
            in      parsers
          
          for current-result
            of-type Parse-Result
            =       (apply-parser current-parser current-state)
          
          unless (parse-result-succeeded-p current-result) do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return current-result))))))

;;; -------------------------------------------------------

(defun any-of (&rest parsers)
  "Returns a fresh ``Parser'' which succeeds if any of its input
   PARSERS, probed in the exact order of their specification, matches,
   returning on confirmation the first eligible parser's result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-parser
            of-type Parser
            in      parsers
          
          for current-result
            of-type Parse-Result
            =       (apply-parser current-parser input-state)
          
          when (parse-result-succeeded-p current-result) do
            (return current-result)
          
          finally
            (return
              (make-parse-result NIL input-state NIL)))))))

;;; -------------------------------------------------------

(defun separated-by (subject sepiment)
  "Returns a fresh ``Parser'' which always succeeds, matching zero or
   more instances of the SUBJECT, each twain segregated by the SEPIMENT,
   returning in its parse result a list of the gathered SUBJECT outputs
   in their encountered order."
  (declare (type Parser subject))
  (declare (type Parser sepiment))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       input-state
            then    (parse-result-state current-result)
          
          for current-result
            of-type Parse-Result
            =       (apply-parser subject current-state)
            then    (apply-parser
                      (chain-of sepiment subject)
                      current-state)
          
          while (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          
          finally
            (return
              (make-parse-result T current-state outputs)))))))

;;; -------------------------------------------------------

(defun zero-or-more-times (parser)
  "Returns a fresh ``Parser'' which always succeeds, matching the input
   PARSER a tally of zero or more times, and returning in its parse
   result a list of the PARSER's outputs in their encountered order."
  (declare (type Parser parser))
  (the Parser
    (build-parser (input-state)
      (the Parse-Result
        (loop
          for current-state
            of-type Parse-State
            =       input-state
            then    (parse-result-state current-result)
          
          for current-result
            of-type Parse-Result
            =       (apply-parser parser current-state)
          
          while (parse-result-succeeded-p current-result)
            collect (parse-result-output current-result)
            into    outputs
          
          finally
            (return
              (make-parse-result T current-state outputs)))))))

;;; -------------------------------------------------------

(defun output-of (output)
  "Returns a fresh ``Parser'' which always succeeds, returning in its
   parse result the OUTPUT."
  (declare (type T output))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (make-parse-result T state output)))))

;;; -------------------------------------------------------

(defun monadic-bind (antecedent consequent-generator)
  "Returns a fresh ``Parser'' which answers to the diorism of a monadic
   binding, expecting the ANTECEDENT to match and, upon this
   prerequisite's confirmation, invokes the CONSEQUENT-GENERATOR with
   the ANTECEDENT parse result's output, its produce another parser's
   obtention which is applied to the ANTECEDENT result's state, finally
   returning this consequent parser's parse result."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) consequent-generator))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (let ((antecedent-result (apply-parser antecedent state)))
          (declare (type Parse-Result antecedent-result))
          (if (parse-result-succeeded-p antecedent-result)
            (let ((consequent-result
                    (apply-parser
                      (funcall consequent-generator
                        (parse-result-output antecedent-result))
                      (parse-result-state antecedent-result))))
              (declare (type Parse-Result consequent-result))
              (if (parse-result-succeeded-p consequent-result)
                consequent-result
                (make-parse-result NIL state NIL)))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defmacro bind-parser ((antecedent-output-variable antecedent)
                       &body body)
  "Returns a fresh ``Parser'' which succeeds if, in an inchoate stage,
   the ANTECEDENT matches, on confirmation binding its result's output
   to the ANTECEDENT-OUTPUT-VARIABLE, evaluates the BODY forms,
   expecting the desinent form's primary value respond with a parser,
   the result of which is ultimately communicated."
  `(the Parser
     (monadic-bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun between (open-guard close-guard subject)
  "Returns a fresh ``Parser'' which succeeds if its OPEN-GUARD, SUBJECT,
   and CLOSE-GUARD match in this exact order, returning on confirmation
   in it parse result the SUBJECT's output."
  (declare (type Parser open-guard))
  (declare (type Parser close-guard))
  (declare (type Parser subject))
  (the Parser
    (define-parser
      open-guard
      (bind-parser (subject-output subject)
        (declare (type T subject-output))
        (chain-of close-guard
          (output-of subject-output))))))

;;; -------------------------------------------------------

(defun number-of (expected-number)
  "Returns a fresh ``Parser'' which succeeds if its input state token
   represents an integral number equal to the EXPECTED-NUMBER, returning
   on confirmation in its parse result the probed token."
  (declare (type integer expected-number))
  (the Parser
    (token-which-satisfies (probed-token)
      (and
        (token-type-p probed-token :number)
        (= (token-value probed-token) expected-number)))))

;;; -------------------------------------------------------

(defun number-of-zero ()
  "Returns a fresh ``Parser'' which succeeds if its input state token
   represents an integral value of zero (0), returning on confirmation
   in its parse result the probed token."
  (the Parser
    (number-of 0)))

;;; -------------------------------------------------------

(defun number-of-one ()
  "Returns a fresh ``Parser'' which succeeds if its input state token
   represents an integral value of one (1), returning on confirmation
   in its parse result the probed token."
  (the Parser
    (number-of 1)))

;;; -------------------------------------------------------

(defun positive-number ()
  "Returns a fresh ``Parser'' which succeeds if its input state token
   represents a positive integer value, returning on confirmation in
   its parse result the probed token."
  (the Parser
    (token-which-satisfies (probed-token)
      (and
        (token-type-p probed-token :number)
        (plusp (token-value probed-token))))))

;;; -------------------------------------------------------

(defun parse-reset-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` reset command
   follows, returning on confirmation a ``Reset-Command'' representation
   thereof."
  (the Parser
    (define-parser
      (token-of-type :single-backtick)
      (number-of-zero)
      (output-of
        (make-reset-command)))))

;;; -------------------------------------------------------

(defun parse-increment-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` increment or
   decrement command follows, returning on confirmation a
   ``Increment-Command'' representation thereof."
  (the Parser
    (define-parser
      (token-of-type :single-backtick)
      (any-of
        (bind-parser (amount (positive-number))
          (declare (type Token amount))
          (output-of
            (make-increment-command :amount
              (token-value amount))))
        (output-of
          (make-increment-command :amount -1))))))

;;; -------------------------------------------------------

(defun parse-copy-register-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` register copy
   command follows, returning on confirmation a
   ``Copy-Register-Command'' representation thereof."
  (the Parser
    (define-parser
      (token-of-type :single-backtick)
      (number-of-zero)
      (token-of-type :single-backtick)
      (output-of
        (make-copy-register-command)))))

;;; -------------------------------------------------------

(defun parse-skip-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` skip command
   follows, returning on confirmation a ``Skip-Command'' representation
   thereof."
  (the Parser
    (define-parser
      (token-of-type :single-backtick)
      (number-of-one)
      (token-of-type :single-backtick)
      (output-of
        (make-skip-command)))))

;;; -------------------------------------------------------

(defun parse-jump-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` jump command
   follows, returning on confirmation a ``Jump-Command'' representation
   thereof."
  (the Parser
    (define-parser
      (token-of-type :single-backtick)
      (token-of-type :hyphen)
      (bind-parser (destination (positive-number))
        (declare (type Token destination))
        (output-of
          (make-jump-command :destination
            (token-value destination)))))))

;;; -------------------------------------------------------

(defun parse-set-cell-pointer-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` cell pointer
   setting command follows, returning on confirmation a
   ``Set-Cell-Pointer-Command'' representation thereof."
  (the Parser
    (define-parser
      (token-of-type :double-backtick)
      (any-of
        (chain-of
          (number-of 0)
          (output-of
            (make-set-cell-pointer-command :register :register-a)))
        (chain-of
          (number-of 1)
          (output-of
            (make-set-cell-pointer-command :register :register-b)))))))

;;; -------------------------------------------------------

(defun parse-output-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` output command
   follows, returning on confirmation an ``Output-Command''
   representation thereof."
  (the Parser
    (define-parser
      (token-of-type :double-backtick)
      (output-of
        (make-output-command)))))

;;; -------------------------------------------------------

(defun parse-input-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` input command
   follows, returning on confirmation an ``Input-Command''
   representation thereof."
  (the Parser
    (define-parser
      (token-of-type :triple-backtick)
      (output-of
        (make-input-command)))))

;;; -------------------------------------------------------

(defun parse-command ()
  "Returns a fresh ``Parser'' which succeeds if a `` command can be
   detected, returning on confirmation in its parse result a ``Command''
   representation thereof."
  (the Parser
    (define-parser
      (any-of
        (parse-copy-register-command)
        (parse-skip-command)
        (parse-jump-command)
        (parse-reset-command)
        (parse-increment-command)
        (parse-input-command)
        (parse-output-command)
        (parse-set-cell-pointer-command)))))

;;; -------------------------------------------------------

(defun semicolon ()
  "Returns a fresh ``Parser'' which succeeds if the `` command
   separator, a semicolon (\";\"), follows, returning on confirmation in
   its parse result the ensconcing token."
  (the Parser
    (define-parser
      (token-of-type :semicolon))))

;;; -------------------------------------------------------

(defun command-separator ()
  "Returns a fresh ``Parser'' which succeeds if the `` command
   separator, a semicolon (\";\"), follows a tally of one or more times,
   returning on confirmation in its parse result the ``T'' value."
  (the Parser
    (define-parser
      (semicolon)
      (zero-or-more-times (semicolon))
      (output-of T))))

;;; -------------------------------------------------------

(defun parse-commands ()
  "Returns a fresh ``Parser'' which succeeds if zero or more ``
   commands, each twissel segregated by one or more semicolons, follow,
   returning on confirmation in its parse result a one-dimensional
   simple array of ``Command'' object representations."
  (the Parser
    (define-parser
      (between
        (zero-or-more-times
          (command-separator))
        (chain-of
          (zero-or-more-times
            (command-separator))
          (token-of-type :eof))
        (bind-parser
            (commands
              (separated-by
                (parse-command)
                (command-separator)))
          (declare (type (list-of Command) commands))
          (output-of
            (coerce commands
              '(simple-array Command (*)))))))))

;;; -------------------------------------------------------

(defun parse-program (lexer)
  "Parses the `` program whose constituents are communicated by
   mediation of the LEXER's tokens, assembles and returns it."
  (declare (type Lexer lexer))
  (let ((parse-result
          (apply-parser
            (parse-commands)
            (make-initial-parse-state lexer))))
    (declare (type Parse-Result parse-result))
    (the program
      (if (parse-result-succeeded-p parse-result)
        (parse-result-output parse-result)
        (error "Error during the parsing of the program.")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing `` program.")
    :type          program
    :documentation "The `` program to interpret.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP), designating the
                    zero-based index into the PROGRAM vector of the
                    currently evaluated command.")
   (jump-point
    :initform      NIL
    :type          (or null fixnum)
    :documentation "The optional zero-based index of the command to
                    move the instruction pointer (IP) to, if the most
                    recently processed command elicits a \"jump\"
                    action.
                    ---
                    A value of ``NIL'' acts as a sentinel in order to
                    communicate the usual instruction pointer advance
                    behavior.")
   (register-a
    :initform      0
    :accessor      register-a
    :type          integer
    :documentation "The integral value stored in the register \"a\".")
   (register-b
    :initform      0
    :accessor      register-b
    :type          integer
    :documentation "The integral value stored in the register \"a\".")
   (cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-vector
    :documentation "A sparse vector of integer-valued cells.")
   (cell-pointer
    :initform      0
    :accessor      cell-pointer
    :type          integer
    :documentation "A cursor entrusted with the current cell's selection
                    among the CELLS."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of imbuing a ``
     program with actual effect."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Returns a fresh ``Interpreter'' nuncupated to the `` PROGRAM's
   evaluation."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the `` program consigned to the INTERPRETER's
   castaldy is completed, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (the boolean
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the INTERPRETER's currently processed command."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (the Command
      (aref program ip))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer to the next command,
   contingently employing the \"jump\" destination for such telos, and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip jump-point) interpreter
    (declare (type fixnum           ip))
    (declare (type (or null fixnum) jump-point))
    (if jump-point
      (shiftf ip jump-point NIL)
      (incf   ip)))
  (values))

;;; -------------------------------------------------------

(defun jump-to (interpreter command-number)
  "Memorizes the one-based COMMAND-NUMBER as the zero-based jump
   destination for the subsequent INTERPRETER operation cycle's
   instruction pointer (IP) and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     command-number))
  (with-slots (jump-point) interpreter
    (declare (type (or null fixnum) jump-point))
    (setf jump-point
      (1- command-number)))
  (values))

;;; -------------------------------------------------------

(defun current-cell (interpreter)
  "Returns the integral value stored in the INTERPRETER's current memory
   cell."
  (declare (type Interpreter interpreter))
  (with-slots (cells cell-pointer) interpreter
    (declare (type cell-vector cells))
    (declare (type integer     cell-pointer))
    (the integer
      (gethash cell-pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER's current memory cell and
   returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (with-slots (cells cell-pointer) interpreter
    (declare (type cell-vector cells))
    (declare (type integer     cell-pointer))
    (setf (gethash cell-pointer cells 0) new-value))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter)
            (command     Reset-Command))
    (declare (type Interpreter   interpreter))
    (declare (type Reset-Command command))
    (declare (ignorable          command))
    (setf (register-a interpreter) 0)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Increment-Command))
    (declare (type Interpreter       interpreter))
    (declare (type Increment-Command command))
    (let ((amount (increment-command-amount command)))
      (declare (type integer amount))
      (cond
        ((plusp amount)
          (incf (register-a interpreter) amount))
        ((minusp amount)
          (decf (register-a interpreter)))
        (T
          (error "Invalid increment amount: ~d." amount))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Copy-Register-Command))
    (declare (type Interpreter           interpreter))
    (declare (type Copy-Register-Command command))
    (declare (ignorable                  command))
    (setf (register-b interpreter)
      (register-a interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Set-Cell-Pointer-Command))
    (declare (type Interpreter              interpreter))
    (declare (type Set-Cell-Pointer-Command command))
    (case (set-cell-pointer-command-register command)
      (:register-a
        (setf (cell-pointer interpreter)
          (register-a interpreter))
        (setf (register-a interpreter)
          (current-cell interpreter)))
      (:register-b
        (setf (cell-pointer interpreter)
          (register-b interpreter))
        (setf (current-cell interpreter)
          (register-b interpreter)))
      (otherwise
        (error "Invalid register: ~s."
          (set-cell-pointer-command-register command))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Skip-Command))
    (declare (type Interpreter  interpreter))
    (declare (type Skip-Command command))
    (declare (ignore            command))
    (when (zerop (register-a interpreter))
      (advance-program interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Jump-Command))
    (declare (type Interpreter  interpreter))
    (declare (type Jump-Command command))
    (jump-to interpreter
      (jump-command-destination command))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Output-Command))
    (declare (type Interpreter    interpreter))
    (declare (type Output-Command command))
    (declare (ignore              command))
    (format T "~c"
      (code-char
        (register-a interpreter)))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Input-Command))
    (declare (type Interpreter    interpreter))
    (declare (type Input-Command command))
    (declare (ignore              command))
    (format T "~&>> ")
    (finish-output)
    (setf (register-a interpreter)
      (char-code
        (read-char)))
    (clear-input)
    (values)))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interpret the `` program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-|``| (code)
  "Interprets the piece of `` source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-lexer code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-|``| ";;;```;``;`-1;;;")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-|``|
  "
  ```;
  `;`;`;`;`;`;`;`;`;`;
  `;`;`;`;`;`;`;`;`;`;
  `;`;`;`;`;`;`;`;`;`;
  `;`;`;`;`;`;`;`;`;`;
  `;`;`;`;`;`;`;`;
  `1`;
  `-53;
  `-57;
  `0;`49;``;`-55;
  `0;`48;``
  ")
