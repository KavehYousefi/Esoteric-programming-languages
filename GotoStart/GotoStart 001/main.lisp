;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "GotoStart" and its variant "Infinite GotoStart", both
;; invented by the Esolang user "ChuckEsoteric08" and presented on
;; April 30th, 2023, the kenspeckle proprium of which resides in
;; capability of repeating a program at any instant from its incipiency,
;; while operating a contingently infinite set of indexed integer
;; registers.
;; 
;; 
;; Concept
;; =======
;; The GotoStart family of languages operates on a repertoire of
;; indexed registers, each capable of a scalar integer's storage,
;; permitting as a conspicable element of the operative haecceity the
;; control flow's relocation to the program start.
;; 
;; == BASIC GOTOSTART: JUMP ANYWHERE ==
;; The basic GotoStart specimen includes in its homologation the
;; return statement's occurrency at any point of the program.
;; 
;; == INFINITE GOTOSTART: JUMP AT CONCLUSION ==
;; The Infinite GotoStart variation, as an indicium in the compernage of
;; the remaining diorisms' equality with the basic language, tolerates,
;; and imposes as an imperative, the restart behest merely at the
;; program's conclusion, whereas at a position deviating therefrom
;; such action is inflicted with interdiction.
;; 
;; == THE MEMORY: INTEGER SCALAR REGISTERS ==
;; The language operates on a theoretically infinite contingency of
;; registers, amenable no non-negative integer indices, and entalented
;; with the capacity for signed integer numbers of any mickleness.
;; 
;; 
;; Instructions
;; ============
;; A sextuple cardinality enumerates GotoStart's operational roster,
;; its bailiwick's entelech a commorancy to basic arithmetics on the
;; registers, a conditional execution facility, a restart instrument,
;; and an output conduit.
;; 
;; == OVERVIEW ==
;; An apercu anent the GotoStart programming language's competences
;; shall be limned in the following tabulation.
;; 
;; Please heed that succedaneous segments are emphasized via a catena
;; of asterisks ("*") in an underlining form, and intended for their
;; supersession by actual GotoStart code in the program's ultimatity.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   +(i)              | Increments the register amenable by the index
;;     *               | {i} by one (1).
;;                     |-----------------------------------------------
;;                     | {i} must be a non-negative register index.
;;   ..................................................................
;;   -(i)              | If the register amenable to the index {i}
;;     *               | contains a value greater than zero (0),
;;                     | decrements it by one (1).
;;                     |-----------------------------------------------
;;                     | {i} must be a non-negative register index.
;;   ..................................................................
;;   =(i:n)            | Sets the state stored in the register amenable
;;     * *             | to the index {i} to the value {n}.
;;                     |-----------------------------------------------
;;                     | {i} must be a non-negative register index.
;;                     |-----------------------------------------------
;;                     | {n} must be a signed or unsigned integer
;;                     | literal.
;;   ..................................................................
;;   .(i)              | Prints the value stored in the register
;;     *               | amenable to the index {i} in its verbatim
;;                     | numeric form to the standard output.
;;                     |-----------------------------------------------
;;                     | {i} must be a non-negative register index.
;;   ..................................................................
;;   ?(i=n>statements) | If the register amenable to the index {i}
;;     * * **********  | equals the value {n}, executes the
;;                     | {statements}.
;;                     |-----------------------------------------------
;;                     | {i} must be a non-negative register index.
;;                     |-----------------------------------------------
;;                     | {n} must be a signed or unsigned integer
;;                     | literal.
;;                     |-----------------------------------------------
;;                     | {statements} must be an ordered list of zero
;;                     | or more statements.
;;   ..................................................................
;;   ^                 | Immediately relocates the instruction pointer
;;                     | (IP) to the start of the program.
;;                     |-----------------------------------------------
;;                     | In the Infinite GotoStart variant, this
;;                     | command may and must only occur as the last
;;                     | statement of a program.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-29
;; 
;; Sources:
;;   [esolang2023GotoStart]
;;   The Esolang contributors, "GotoStart", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/GotoStart"
;;   
;;   [esolang2024GotoStartTuring]
;;   The Esolang contributors, "GotoStart Turing-completness proof",
;;     January 4th, 2024
;;   URL: "https://esolangs.org/wiki/GotoStart_Turing-completness_proof"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (every
                #'(lambda (element)
                    (declare (type T element))
                    (typep element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype property-list-of (&optional (indicator-type '*)
                                     (value-type     '*))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, each key, or indicator, of which
   complies with the INDICATOR-TYPE and associates with a value of the
   VALUE-TYPE, both defaulting to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and
                  (or (eq    indicator-type '*)
                      (typep indicator indicator-type))
                  (or (eq    value-type '*)
                      (typep value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type   '*)
                                  (value-type '*))
  "The ``hash-table-of'' type defines a hash table the componency of
   which enumerates zero or more entries, each among these composed of
   a key compliant with the KEY-TYPE and a value adhering to the
   VALUE-TYPE, both governed by the default IN the generic sentinel
   ``*''."
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
                (and
                  (or (eq    key-type '*)
                      (typep key key-type))
                  (or (eq    value-type '*)
                      (typep value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype attribute-list ()
  "The ``attribute-list'' type defines a collection of abstract syntax
   tree (AST) node attributes, realized as a property list, or plist,
   the keys of which assume keywords, while the values comply to any
   type."
  '(property-list-of keyword *))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   ``AST-Node'' instances."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype register-set ()
  "The ``register-set'' type defines a sparse vector of registers,
   amenable to non-negative integer indices, and realized as a hash
   table, the keys of which contribute the subscripts, while the values
   assume the integral register states."
  '(hash-table-of (integer 0 *) integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class encapsulates an object of significance extracted
   during the lexical analyzation of a piece of GotoStart source code,
   and completed in its diorism by the categorizing type in champarty
   with the detailing value."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents an arithmetic sign
   symbol, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, the diorism
   of which intrines the space, horizontal tab, and newline characters,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun character-at-satisfies-p (source position predicate)
  "Determines whether the character located at the POSITION position
   into the SOURCE exists and complies with the PREDICATE's expectation,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string                   source))
  (declare (type fixnum                   position))
  (declare (type (function (character) *) predicate))
  (the boolean
    (not (null
      (and
        (array-in-bounds-p source position)
        (funcall predicate
          (char source position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class establishes an entity the capacitation of which
   permits the detection and extraction of significant objects in the
   form of tokens from a piece of GotoStart source code."
  (source    (error "Missing lexer source.")
             :type      string
             :read-only NIL)
  (position  (error "Missing lexer position.")
             :type      fixnum
             :read-only NIL)
  (character (error "Missing lexer character.")
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p
            (lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char
        (lexer-source lexer)
        (incf (lexer-position lexer)))))
  (values))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Creates and returns a new token of the TOKEN-TYPE, its value being
   derived from the LEXER's current character, prevenient to its
   advancement to the next position."
  (declare (type Lexer lexer))
  (the Token
    (make-token token-type
      (prog1
        (lexer-character lexer)
        (advance-lexer   lexer)))))

;;; -------------------------------------------------------

(defun introduces-number-p (lexer)
  "Determines whether, proceeding from the start position into the
   LEXER's source, a signed or unsigned integer number is commenced,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (not (null
      (or
        (and
          (character-at-satisfies-p
            (lexer-source   lexer)
            (lexer-position lexer)
            #'sign-character-p)
          (character-at-satisfies-p
            (lexer-source lexer)
            (1+ (lexer-position lexer))
            #'digit-char-p))
        (character-at-satisfies-p
          (lexer-source   lexer)
          (lexer-position lexer)
          #'digit-char-p))))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a signed or unsigned integer number and returns a
   ``:number'' token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (when (and (lexer-character lexer)
                     (sign-character-p (lexer-character lexer)))
            (write-char (lexer-character lexer) digits)
            (advance-lexer lexer))
          (loop
            while
              (and (lexer-character lexer)
                   (digit-char-p (lexer-character lexer)))
            do
              (write-char (lexer-character lexer) digits)
              (advance-lexer lexer)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent whitespaces and returns the
   contingently modified LEXER."
  (declare (type Lexer lexer))
  (symbol-macrolet
      ((whitespace-follows-p
        (not (null
          (and (lexer-character lexer)
               (whitespace-character-p
                 (lexer-character lexer)))))))
    (declare (type boolean whitespace-follows-p))
    (loop while whitespace-follows-p do
      (advance-lexer lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER to any request with a fresh
   end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-eof-token))
      
      ((whitespace-character-p (lexer-character lexer))
        (get-next-token
          (skip-whitespaces lexer)))
      
      ((introduces-number-p lexer)
        (read-number lexer))
      
      ((char= (lexer-character lexer) #\+)
        (read-symbol lexer :plus))
      
      ((char= (lexer-character lexer) #\-)
        (read-symbol lexer :minus))
      
      ((char= (lexer-character lexer) #\=)
        (read-symbol lexer :equal))
      
      ((char= (lexer-character lexer) #\.)
        (read-symbol lexer :dot))
      
      ((char= (lexer-character lexer) #\?)
        (read-symbol lexer :eroteme))
      
      ((char= (lexer-character lexer) #\:)
        (read-symbol lexer :colon))
      
      ((char= (lexer-character lexer) #\>)
        (read-symbol lexer :right-angular-bracket))
      
      ((char= (lexer-character lexer) #\^)
        (read-symbol lexer :caret))
      
      ((char= (lexer-character lexer) #\()
        (read-symbol lexer :left-parenthesis))
      
      ((char= (lexer-character lexer) #\))
        (read-symbol lexer :right-parenthesis))
      
      (T
        (error "Unexpected character \"~c\" at position ~d."
          (lexer-character lexer)
          (lexer-position  lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) node.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node
  (:constructor make-ast-node (kind &rest attributes)))
  "The ``AST-Node'' class represents an abstract syntax tree (AST) node
   in a generic fashion, realized as a commodity which specifies a
   Procrustean treatment of the subject via its identifying kind and a
   property list governing its attributes."
  (kind       (error "Missing node kind.")
              :type      keyword
              :read-only T)
  (attributes NIL
              :type      attribute-list
              :read-only T))

;;; -------------------------------------------------------

(defun get-ast-node-attribute (node attribute-name)
  "Returns the value affiliated with the ATTRIBUTE-NAME in the NODE, or
   responds with ``NIL'' upon its lacuna."
  (declare (type AST-Node node))
  (declare (type keyword  attribute-name))
  (the T
    (getf (ast-node-attributes node) attribute-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) AST-Node) parse-statement-list))

;;; -------------------------------------------------------

(defstruct (Parser
  (:constructor make-parser
    (lexer
     &aux (current-token (get-next-token lexer)))))
  "The ``Parser'' class furnishes an entity entalented with the dever of
   assembling an abstract syntax tree (AST) from a series of tokens."
  (lexer         (error "Missing lexer.")
                 :type      Lexer
                 :read-only T)
  (current-token (error "Missing current token.")
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the probed token,
   while concomitantly substituting the same in the PARSER by the
   internally managed lexer's next token; otherwise signals an error of
   an unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
    (if (eq (token-type (parser-current-token parser))
            expected-token-type)
      (prog1
        (parser-current-token parser)
        (setf (parser-current-token parser)
          (get-next-token
            (parser-lexer parser))))
      (error "Expected a token of the type ~s, but encountered ~a."
        expected-token-type
        (parser-current-token parser)))))

;;; -------------------------------------------------------

(defun parse-parenthesized-register-index (parser)
  "Parses a register identifier ensconced in a jumelle of parentheses
   by the PARSER's adminiculum and returns the thus amplected register
   index."
  (declare (type Parser parser))
  (expect-token parser :left-parenthesis)
  (the integer
    (prog1
      (token-value
        (expect-token parser :number))
      (expect-token parser :right-parenthesis))))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses a single statement from the PARSER's tokens and returns an
   ``AST-Node'' representation thereof, or, upon its failure in such a
   recognition, returns the ``NIL'' value."
  (declare (type Parser parser))
  (the (or null AST-Node)
    (case (token-type (parser-current-token parser))
      (:plus
        (expect-token parser :plus)
        (make-ast-node :increment :register
          (parse-parenthesized-register-index parser)))
      
      (:minus
        (expect-token parser :minus)
        (make-ast-node :decrement :register
          (parse-parenthesized-register-index parser)))
      
      (:equal
        (expect-token parser :equal)
        (expect-token parser :left-parenthesis)
        (prog1
          (make-ast-node :set
            :register
              (prog1
                (token-value
                  (expect-token parser :number))
                (expect-token parser :colon))
            :value
              (token-value
                (expect-token parser :number)))
          (expect-token parser :right-parenthesis)))
      
      (:dot
        (expect-token parser :dot)
        (make-ast-node :output :register
          (parse-parenthesized-register-index parser)))
      
      (:eroteme
        (expect-token parser :eroteme)
        (expect-token parser :left-parenthesis)
        (prog1
          (make-ast-node :execute-if
            :subject
              (prog1
                (token-value
                  (expect-token parser :number))
                (expect-token parser :equal))
            :guard
              (prog1
                (token-value
                  (expect-token parser :number))
                (expect-token parser :right-angular-bracket))
            :statements
              (parse-statement-list parser))
          (expect-token parser :right-parenthesis)))
      
      (:caret
        (expect-token parser :caret)
        (make-ast-node :go-to-start))
      
      (otherwise
        NIL))))

;;; -------------------------------------------------------

(defun parse-statement-list (parser)
  "Parses a sequence of zero or more GotoStart statements and returns a
   ``:block'' node representation thereof."
  (declare (type Parser parser))
  (the AST-Node
    (make-ast-node :block :statements
      (loop
        for statement
          of-type (or null AST-Node)
          =       (parse-statement parser)
        while statement
        collect statement))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Assembles a GotoStart program from the PARSER's tokens and returns
   a ``:program'' node representation thereof."
  (declare (type Parser parser))
  (the AST-Node
    (make-ast-node :program :statements
      (parse-statement-list parser))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of node visitor.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Visitor
  "The ``Visitor'' interface establishes a substratum for all entities
   intent on the traversal of a GotoStart program defined in terms of
   an abstract syntax tree (AST).")

;;; -------------------------------------------------------

(defgeneric dispatch-node (visitor node-kind node)
  (:documentation
    "Processes the NODE, identified by the NODE-KIND, in the VISITOR's
     context and returns no value."))

;;; -------------------------------------------------------

(defun visit-node (visitor node)
  "Processes the NODE in the VISITOR's context and returns no value."
  (declare (type Visitor  visitor))
  (declare (type AST-Node node))
  (dispatch-node visitor (ast-node-kind node) node)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of AST node perquisition operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-last-node (nodes)
  "Returns the desinent node from the NODES list, or ``NIL'' upon its
   vacancy."
  (declare (type node-list nodes))
  (the (or null AST-Node)
    (first (last nodes))))

;;; -------------------------------------------------------

(defun get-program-statements (program-node)
  "Returns a list of the PROGRAM-NODE's statements, each member itself
   an ``AST-Node'' instance."
  (declare (type AST-Node program-node))
  (the node-list
    (get-ast-node-attribute
      (get-ast-node-attribute program-node :statements)
      :statements)))

;;; -------------------------------------------------------

(defun program-empty-p (program-node)
  "Determines whether the PROGRAM-NODE's statement list is empty,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type AST-Node program-node))
  (the boolean
    (null (get-program-statements program-node))))

;;; -------------------------------------------------------

(defun program-terminates-in-go-to-command-p (program-node)
  "Determines whether the PROGRAM-NODE's statement list entails as its
   desinent node a \"go to start\" (\"^\") instruction, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type AST-Node program-node))
  (the boolean
    (eq (ast-node-kind
          (get-last-node
            (get-program-statements program-node)))
        :go-to-start)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of GotoStart validator.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Goto-Validator
  (:include     Visitor)
  (:constructor make-goto-validator
    (tree
     &aux (last-node
            (get-last-node
              (get-program-statements tree))))))
  "The ``Goto-Validator'' class furnishes a node visitor accommodated
   for the validation of the Infinite GotoStart language variant's
   validation regarding its conformation."
  (tree      (error "Missing program tree.")
             :type      AST-Node
             :read-only T)
  (last-node (error "Missing last program node.")
             :type      AST-Node
             :read-only T))

;;; -------------------------------------------------------

(defmethod dispatch-node ((validator Goto-Validator)
                          (node-kind (eql :program))
                          (node      AST-Node))
  (declare (type Goto-Validator validator))
  (declare (type keyword        node-kind))
  (declare (ignore              node-kind))
  (declare (type AST-Node       node))
  (visit-node validator
    (get-ast-node-attribute node :statements))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((validator Goto-Validator)
                          (node-kind (eql :block))
                          (node      AST-Node))
  (declare (type Goto-Validator validator))
  (declare (type keyword        node-kind))
  (declare (ignore              node-kind))
  (declare (type AST-Node       node))
  (dolist (statement (get-ast-node-attribute node :statements))
    (declare (type AST-Node statement))
    (visit-node validator statement))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((validator Goto-Validator)
                          (node-kind (eql :execute-if))
                          (node      AST-Node))
  (declare (type Goto-Validator validator))
  (declare (type keyword        node-kind))
  (declare (ignore              node-kind))
  (declare (type AST-Node       node))
  (visit-node validator
    (get-ast-node-attribute node :statements))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((validator Goto-Validator)
                          (node-kind (eql :go-to-start))
                          (node      AST-Node))
  (declare (type Goto-Validator validator))
  (declare (type keyword        node-kind))
  (declare (ignore              node-kind))
  (declare (type AST-Node       node))
  (unless (eq node (goto-validator-last-node validator))
    (error "An Infinite GotoStart program does not tolerate a ~
            \"^\" instruction at any position except for the ~
            desinent one."))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((validator Goto-Validator)
                          (node-kind T)
                          (node      AST-Node))
  (declare (type Goto-Validator validator))
  (declare (ignore              validator))
  (declare (type keyword        node-kind))
  (declare (ignore              node-kind))
  (declare (type AST-Node       node))
  (declare (ignore              node))
  (values))

;;; -------------------------------------------------------

(defun validate-Infinite-GotoStart-program (validator)
  "Determines whether the abstract syntax tree (AST) governed by the
   VALIDATOR constitutes an admissive Infinite GotoStart program,
   returning on confirmation the VALIDATOR's program tree; otherwise
   signals an error of an unspecified type."
  (declare (type Goto-Validator validator))
  (cond
    ((program-empty-p (goto-validator-tree validator))
      (error "An Infinite GotoStart program may not be empty."))
    ((not (program-terminates-in-go-to-command-p
            (goto-validator-tree validator)))
      (error "An Infinite GotoStart program must terminate in a ~
              \"^\" statement, but concludes with the node ~a."
        (get-last-node
          (get-program-statements
            (goto-validator-tree validator)))))
    (T
      (visit-node validator
        (goto-validator-tree validator))))
  (the AST-Node
    (goto-validator-tree validator)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Go-To-Start-Condition (condition)
  ()
  (:documentation
    "The ``Go-To-Start-Condition'' condition type serves to signal the
     behest involving a return to a GotoStart program's incipiency,
     signified by the \"^\" instruction."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of register operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-register-index (index)
  "Determines whether the INDEX represents a valid identifier for a
   GotoStart register, returning on confirmation the same; otherwise
   signals an error of an unspecified type."
  (declare (type integer index))
  (the (integer 0 *)
    (if (minusp index)
      (error "The index ~d is not a valid non-negative register index."
        index)
      index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:include     Visitor)
  (:constructor make-GotoStart-interpreter (tree))
  (:constructor make-Infinite-GotoStart-interpreter
    (tree
     &aux (tree
            (validate-Infinite-GotoStart-program
              (make-goto-validator tree))))))
  "The ``Interpreter'' class serves to accompass a GotoStart program's
   efficacy, furnishes in the guise of an abstract syntax tree (AST)."
  (tree            (error "Missing program tree.")
                   :type      AST-Node
                   :read-only T)
  (registers       (make-hash-table :test #'eql)
                   :type      register-set
                   :read-only T)
  (shall-restart-p NIL
                   :type boolean
                   :read-only NIL))

;;; -------------------------------------------------------

(defun register-value (interpreter index)
  "Returns the value maintained by the register amenable to the INDEX,
   governed by the INTERPRETER's castaldy."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (the integer
    (gethash
      (validate-register-index index)
      (interpreter-registers   interpreter)
      0)))

;;; -------------------------------------------------------

(defun (setf register-value) (new-value interpreter index)
  "Stores the NEW-VALUE in the register amenable to the INDEX, governed
   by the INTERPRETER's castaldy, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (setf
    (gethash
      (validate-register-index index)
      (interpreter-registers   interpreter)
      0)
    new-value)
  (values))

;;; -------------------------------------------------------

(defun increment-register (interpreter index)
  "Increments the register amenable to the INDEX, governed by the
   INTERPRETER's castaldy, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (incf (register-value interpreter index))
  (values))

;;; -------------------------------------------------------

(defun decrement-register (interpreter index)
  "Decrements the register amenable to the INDEX, governed by the
   INTERPRETER's castaldy, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (when (plusp (register-value interpreter index))
    (decf (register-value interpreter index)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :program))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (visit-node interpreter
    (get-ast-node-attribute node :statements))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :block))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (dolist (statement (get-ast-node-attribute node :statements))
    (declare (type AST-Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :increment))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (increment-register interpreter
    (get-ast-node-attribute node :register))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :decrement))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (decrement-register interpreter
    (get-ast-node-attribute node :register))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :set))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (setf
    (register-value interpreter
      (get-ast-node-attribute node :register))
    (get-ast-node-attribute node :value))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :output))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (format T "~d "
    (register-value interpreter
      (get-ast-node-attribute node :register)))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :execute-if))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (when (= (register-value interpreter
             (get-ast-node-attribute node :subject))
           (get-ast-node-attribute node :guard))
    (visit-node interpreter
      (get-ast-node-attribute node :statements)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-kind   (eql :go-to-start))
                          (node        AST-Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-kind))
  (declare (ignore           node-kind))
  (declare (type AST-Node    node))
  (declare (ignore           node))
  (signal 'Go-To-Start-Condition)
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the GotoStart program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop do
    (handler-case
      (progn
        (setf (interpreter-shall-restart-p interpreter) NIL)
        (visit-node interpreter
          (interpreter-tree interpreter)))
      (Go-To-Start-Condition ()
        (setf (interpreter-shall-restart-p interpreter) T)))
    while
      (interpreter-shall-restart-p interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-GotoStart (code)
  "Interprets the piece of GotoStart source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-GotoStart-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Infinite-GotoStart (code)
  "Interprets the piece of Infinite GotoStart source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (make-Infinite-GotoStart-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the register 0 to the value 1 and print its state.
(interpret-GotoStart
  "+(0)
   .(0)")

;;; -------------------------------------------------------

;; Perpetually count from one (1) up to inclusive three (3), printing
;; each state, and repeat the process.
(interpret-GotoStart
  "?(0=0>+(0).(0))
   ?(0=1>+(0).(0))
   ?(0=2>+(0).(0))
   ?(0=3>=(0:0))
   ^")

;;; -------------------------------------------------------

;; Increment one register, while decrementing the other, and enter an
;; infinite loop.
(interpret-GotoStart
  "?(0=0>+(1)=(0:3)^)
   ?(0=1>+(1)=(0:4)^)
   ?(0=2>+(2)=(0:2)^)
   ?(0=3>+(1)=(0:1)^)
   ?(0=4>?(1=0>=(0:1)^)-(1)=(0:6)^)
   ?(0=5>+(3)=(0:4)^)
   ?(0=6>-(1)=(0:6)^)")

;;; -------------------------------------------------------

;; Infinitely count up from inclusive one (1).
(interpret-Infinite-GotoStart
  "+(0)
   .(0)
   ^")

;;; -------------------------------------------------------

;; Increment one register, while decrementing the other, and enter an
;; infinite loop.
(interpret-Infinite-GotoStart
  "?(0=0>+(1)=(0:3))
   ?(0=1>+(1)=(0:4))
   ?(0=2>+(2)=(0:2))
   ?(0=3>+(1)=(0:1))
   ?(0=4>?(1=0>=(0:1))?(0=4>-(1)=(0:6)))
   ?(0=5>+(3)=(0:4))
   ?(0=6>-(1)=(0:6))^")
