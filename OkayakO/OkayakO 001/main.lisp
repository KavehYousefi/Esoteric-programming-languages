;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "OkayakO", invented by the Esolang user "ChuckEsoteric08"
;; and presented on May 3rd, 2022, the kenspeckle element of which
;; resides in a twain of properties, one being its object-oriented
;; nature, specifying the ubiquitous imposition of the statements'
;; ensconcement in objects, the second a twifaced contingency commorant
;; in its instructions by adminiculum of a binary program state that
;; may assume at any instant a value from one (1) or two (2).
;; 
;; 
;; Concept
;; =======
;; The OkayakO programming language's efficacy is founded upon the
;; definition of objects by mediation of their numeric identifiers and
;; a list of their body statements, with a subsequent invocation using
;; the handle in order to manipulate a signed integer-valued
;; accumulator.
;; 
;; == THE PROGRAM STATE: A BINARY INSTRUCTION CONTINGENCY ==
;; The concrete effects' allotment answers to a twissel dependency, one
;; being, as a wont of most languages, the despecifications engulfed in
;; the nuncupated tokens; the second, indicial in this specimen,
;; ensuing from the furnishment of the binary-valued program state.
;; 
;; This option, admissive to modulations by a strict interface, may
;; assume at any instant in the program's course one of the two values
;; 1 (one) or 2 (two), with the former the inchoation's acquisition.
;; 
;; The act of its value's inversion is norned a "switch"; fhe following
;; nomothesia applies to the program state and this switching
;; respondency:
;; 
;;   --------------------------------------
;;   Current state | New state after switch
;;   --------------+-----------------------
;;   1             | 2
;;   ......................................
;;   2             | 1
;;   --------------------------------------
;; 
;; == THE ACCUMULATOR: A SCALAR INTEGER SALVATORY ==
;; The aefauld conditory entalented with the program's data castaldy
;; is realized in the accumulator, an integral register whose capacity
;; homologates signed integer numbers of any mickleness' inclusion.
;; 
;; == PROGRAMS ARE LISTS OF OBJECT DEFINITIONS ==
;; An OkayakO program's componency enumerates on its highest echolon
;; a sequence of object definition, everichon among these either
;; distributed along a maximum of two lines, the incipient serving in
;; its identifier's adduction, the subsequent, if present at all,
;; furnishing the diorism of its body statements.
;; 
;; Supplied in an illustration's mold, a single object definition limns
;; the imposition
;; 
;;   objectID
;;   bodyStatements
;; 
;; The "objectID" establishes an unambiguous object identification in
;; the form of a signed or unsigned integer number, its registration a
;; requisitum to its future invocation. An already established
;; identifier supersedes any prevenient object registration entry
;; stevened by the same handle.
;; 
;; The "bodyStatements", an optional supplement, if present, ought to
;; occupy a line of its own, comprehending an ordered list of zero or
;; more instructions.
;; 
;; == PROGRAMS START WITH THE FIRST OBJECT ==
;; The OkayakO program's progression ensues from the first stated
;; object's activation, whence ensues a catena of invocations required
;; to be actuated by the program via the "`" operation's employment.
;; This instrument's lacuna constitutes the cheason for the code's
;; immediate termination following this entry object's exercise.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical perspective, an OkayakO program's designment
;; produces a sequence of zero or more lines, each such either dedicated
;; to an object identifier's expression or its body statements'
;; enumeration, with the imperative identifier always preceding the
;; optional command listing.
;; 
;; == GRAMMAR ==
;; An amplification in the language formulation's stringency shall be
;; administered by the following Extended Backus-Naur Form (EBNF)
;; treatise:
;; 
;;   program                     := [ newlines ]
;;                               ,  objectDefinition
;;                               ,  { subsequentObjectDefinitions }
;;                               ,  [ newlines ]
;;                               ;
;;   
;;   subsequentObjectDefinitions := newlines , objectDefinition ;
;;   objectDefinition            := objectId , ":"
;;                               , newlines
;;                               , commands
;;                               ;
;;   commands                    := { command } ;
;;   command                     := "@" | "!" | "#" | "`" | '"' ;
;;   
;;   objectId                    := [ signum ] , integer ;
;;   signum                      := "+" | "-" ;
;;   integer                     := digit , { digit } ;
;;   digit                       := "0" | "1" | "2" | "3" | "4"
;;                               |  "5" | "6" | "7" | "8" | "9"
;;                               ;
;;   newlines                    := newline , { newline } ;
;;   newline                     := "\n" :
;; 
;; 
;; Instructions
;; ============
;; OkayakO's instruction set tallis a quintuple membership, for a triad
;; among this account exists a bifurcation of the causatum in dependency
;; of the current program state.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be the available instructions'
;; enumeration and elucidation, ostending the incorporation of the
;; twifaced approach that the binary program state introduces:
;; 
;;   ------------------------------------------------------------------
;;   Command | State 1                    | State 2
;;   --------+----------------------------+----------------------------
;;   @       | Toggles the program state. | Toggles the program state.
;;   ..................................................................
;;   !       | Resets the accumulator to  | Increments the accumulator
;;           | its default value of zero  | by one (1).
;;           | (0).                       | 
;;   ..................................................................
;;   #       | Changes the program state  | Decrements the accumulator
;;           | to the variant 2.          | by one.
;;   ..................................................................
;;   `       | Executes the statements    | Changes the program state
;;           | comprising the body of the | to the variant 1.
;;           | object whose identifier    | 
;;           | equals the accumulator's   | 
;;           | value. If no object with   | 
;;           | such number exists, no     | 
;;           | effect applies.            | 
;;   ..................................................................
;;   "       | Outputs the accumulator in | Outputs the accumulator in
;;           | its verbatim numeric form. | its verbatim numeric form.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-13
;; 
;; Sources:
;;   [esolang2023Okayako]
;;   The Esolang contributors, "Okayako", Feburary 11th, 2023
;;   URL: "https://esolangs.org/wiki/Okayako"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among which assumes the KEY-TYPE and answers
   to a value of the VALUE-TYPE, both defaulting to the generic sentinel
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
                  (or (eq key-type '*)
                      (typep key key-type))
                  (or (eq value-type '*)
                      (typep value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for the
   same holds the default of the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (loop
                for    element of-type T in (the list candidate)
                always (typep element element-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variation on OkayakO
   operations."
  '(member
    :switch-state
    :reset-or-increment
    :change-to-second-state-or-decrement
    :execute-or-change-to-first-state
    :output))

;;; -------------------------------------------------------

(deftype okayako-program ()
  "The ``okayako-program'' type defines an executable OkayakO program as
   an ordered list comprehending zero or more ``OkayakO-Object''
   definitions."
  '(list-of OkayakO-Object))

;;; -------------------------------------------------------

(deftype object-registry ()
  "The ``object-registry'' type defines a collection of OkayakO object
   definitions amenable to their integral identifiers, and realized in
   a hash table, the keys of which maintain the numeric handles, while
   the values amplect the ``OkayakO-Object'' instances themselves."
  '(hash-table-of integer OkayakO-Object))

;;; -------------------------------------------------------

(deftype program-state ()
  "The ``program-state'' type defines the OkayakO program state, the
   same influences the operations' interpretation, as an integer number
   from the set {1, 2}."
  '(integer 1 2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean truth value interpretation of the OBJECT, producing
   for a non-``NIL'' input the ``boolean'' ``T'' constant, otherwise
   responding, for a ``NIL'' OBJECT, with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of OkayakO source code during the
   lexical analyzation stage."
  (type  (value "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal tab
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\+)
          (char= candidate #\-)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for lexer.")
    :type          string
    :documentation "The piece of OkayakO source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The position into the SOURCE of the currently
                    selected character.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class furnishes a lexical analyzer, or lexer, the
     onus of which constitutes the extraction of tokens from a piece of
     OkayakO source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Stores the first character of the LEXER's source in the same and
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

(defun make-lexer (source)
  "Creates and returns a fresh ``Lexer'' dedicated to the OkayakO SOURCE
   code's evaluation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the LEXER's current character, while concomitantly advances
   its LEXER's position cursor to the next character in its source, if
   possible, and returns no value."
  (declare (type Lexer lexer))
  (the (or null character)
    (with-slots (source position character) lexer
      (declare (type string              source))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source
              (incf position))))))))

;;; -------------------------------------------------------

(defun skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent spaces and returns the
   contingently modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (advance-lexer lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun read-newlines (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a sequence of zero or more newline characters and returns a
   ``:newline'' token representation of the consumed content."
  (declare (type Lexer lexer))
  (the Token
    (make-token :newline
      (with-slots (character) lexer
        (declare (type (or null character) character))
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop while (and character (char= character #\Newline)) do
            (write-char (advance-lexer lexer) content)))))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an unsigned integer number and returns a ``:number'' token
   representation thereof."
  (declare (type Lexer lexer))
    (the Token
    (make-token :number
      (with-slots (character) lexer
        (declare (type (or null character) character))
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop
              initially
                (when (and character (sign-character-p character))
                  (write-char (advance-lexer lexer) digits))
              while (and character (digit-char-p character)) do
                (write-char (advance-lexer lexer) digits))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its underlying source's depletion, the LEXER responds to any
   request with a fresh ``:eof'' (end-of-file) token."
  (declare (type Lexer lexer))
  (the Token
    (with-slots (character position) lexer
      (declare (type (or null character) character))
      (declare (type fixnum              position))
      (cond
        ((null character)
          (make-eof-token))
        ((space-character-p character)
          (get-next-token
            (skip-spaces lexer)))
        ((char= character #\Newline)
          (read-newlines lexer))
        ((char= character #\@)
          (make-token :at-sign
            (advance-lexer lexer)))
        ((char= character #\!)
          (make-token :ecphoneme
            (advance-lexer lexer)))
        ((char= character #\#)
          (make-token :hash-sign
            (advance-lexer lexer)))
        ((char= character #\`)
          (make-token :grave-accent
            (advance-lexer lexer)))
        ((char= character #\")
          (make-token :double-quote
            (advance-lexer lexer)))
        ((char= character #\:)
          (make-token :colon
            (advance-lexer lexer)))
        ((or (sign-character-p character)
             (digit-char-p     character))
          (read-number lexer))
        (T
          (error "Unexpected character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "OkayakO-Object".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (OkayakO-Object)
  "The ``OkayakO-Object'' class serves in the representation of an
   OkayakO object definition, embracing in its diorism the identifying
   integral value and a contingently empty list of its ensconced
   statements."
  (identifier (error "Missing object identifier.")
              :type      integer
              :read-only T)
  (statements (error "Missing object body.")
              :type      (list-of command)
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The provenance for the token obtention.")
   (current-token
    :initform      (make-eof-token)
    :type          Token
    :documentation "The token most recently acquired from the LEXER."))
  (:documentation
    "The ``Parser'' class applies itself to the assemblage of an OkayakO
     program from a sequence of tokens provided by a lexer."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries the incipient token from the PARSER's lexer, stores it in the
   PARSER, and returns no value."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token
      (get-next-token lexer)))
  (values))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a fresh ``Parser'' whose tokens are provided by
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the probed token,
   while concomitantly querying the next token from the underlying lexer
   and storing the same in the PARSER; otherwise signals an error of an
   unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (if (token-is-of-type-p current-token expected-token-type)
      (prog1 current-token
        (setf current-token
          (get-next-token lexer)))
      (error "Expected a token of the type ~a, but encountered ~a."
        expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun skip-newline-tokens (parser)
  "Skips a sequence of zero or more accolent newline tokens maintained
   by the PARSER and returns no value."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-is-of-type-p current-token :newline) do
      (eat-token parser :newline)))
  (values))

;;; -------------------------------------------------------

(defun parse-object-body (parser)
  "Parses a sequence of zero or more statement which comprise an OkayakO
   object's body utilizing the PARSER's tokens and returns a conable
   representation thereof."
  (declare (type Parser parser))
  (the (list-of command)
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (when (token-is-of-type-p current-token :newline)
        (eat-token parser :newline)
        (flet ((collect-command (command)
                "Returns a singleton list comprehending the COMMAND as
                 its aefauld member, while concomitantly querying and
                 storing the next token from its underlying lexer."
                (declare (type command command))
                (prog1
                  (list command)
                  (eat-token parser
                    (token-type current-token)))))
          (loop append
            (case (token-type current-token)
              (:at-sign
                (collect-command :switch-state))
              (:ecphoneme
                (collect-command :reset-or-increment))
              (:hash-sign
                (collect-command :change-to-second-state-or-decrement))
              (:grave-accent
                (collect-command :execute-or-change-to-first-state))
              (:double-quote
                (collect-command :output))
              (otherwise
                (loop-finish)))))))))

;;; -------------------------------------------------------

(defun parse-object (parser)
  "Parses an OkayakO object definition employing the PARSER's tokens
   and returns a conable representation thereof."
  (declare (type Parser parser))
  (the OkayakO-Object
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (let ((object-number
              (token-value
                (eat-token parser :number))))
        (declare (type integer object-number))
        (eat-token parser :colon)
        (prog1
          (make-okayako-object
            :identifier object-number
            :statements (parse-object-body parser))
          (skip-newline-tokens parser))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses an OkayakO program utilizing the PARSER and returns a list of
   its object definitions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the okayako-program
      (loop
        initially
          (skip-newline-tokens parser)
        until
          (token-is-of-type-p current-token :eof)
        collect
          (parse-object parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of object registry.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-object-registry ()
  "Creates and returns a fresh ``object-registry''."
  (the object-registry
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun register-object (registry new-member)
  "Includes the NEW-MEMBER in the object REGISTRY, contingently
   expelling a prevenient association with the candidate's identifier,
   and returns no value."
  (declare (type object-registry registry))
  (declare (type OkayakO-Object  new-member))
  (setf (gethash (okayako-object-identifier new-member) registry)
    new-member)
  (values))

;;; -------------------------------------------------------

(defun contains-object-id-p (registry probed-id)
  "Determines whether the object REGISTRY entails a member amenable to
   the PROBED-ID, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type object-registry registry))
  (declare (type integer         probed-id))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash probed-id registry)))))

;;; -------------------------------------------------------

(defun get-object-by-id (registry object-id)
  "Returns the OkayakO object amenable to the OBJECT-ID in the object
   REGISTRY, or, upon its disrespondency, signals an error of an
   unspecified type."
  (declare (type object-registry registry))
  (declare (type integer         object-id))
  (the OkayakO-Object
    (or (gethash object-id registry)
        (error "No object with the identifier ~d can be detected."
          object-id))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of object collector operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-objects (program)
  "Collects the object definitions partaking of the OkayakO PROGRAM and
   returns an ``object-registry'' encapsulation thereof."
  (declare (type okayako-program program))
  (the object-registry
    (let ((objects (make-object-registry)))
      (declare (type object-registry objects))
      (dolist (current-object program objects)
        (declare (type OkayakO-Object current-object))
        (register-object objects current-object)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Interpreter OkayakO-Object) (values))
                execute-object))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing OkayakO program for the ~
                           interpreter.")
    :reader        get-program
    :type          okayako-program
    :documentation "The OkayakO program, a composition of zero or more
                    object definition, to evaluate.")
   (objects
    :initarg       :objects
    :initform      (make-object-registry)
    :reader        get-objects
    :type          object-registry
    :documentation "Maintains a mapping betwixt the object identifiers
                    and the represented OkayakO objects.")
   (program-state
    :initform      1
    :accessor      program-state
    :type          program-state
    :documentation "The program state which determines several
                    operations' causata.")
   (accumulator
    :initform      0
    :accessor      accumulator
    :type          integer
    :documentation "The scalar integer register."))
  (:documentation
    "The ``Interpreter'' class is assignment the dever of accompass
     actual efficacy to a parsed OkayakO program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' nuncupated to the OkayakO
   PROGRAM's evaluation."
  (declare (type okayako-program))
  (the Interpreter
    (make-instance 'Interpreter
      :program program
      :objects (collect-objects program))))

;;; -------------------------------------------------------

(defun can-execute-object-p (interpreter object-id)
  "Determines whether the INTERPRETER's capacitation incorporates the
   execution of the object amenable to the OBJECT-ID, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     object-id))
  (the boolean
    (contains-object-id-p
      (get-objects interpreter)
      object-id)))

;;; -------------------------------------------------------

(defun switch-state (interpreter)
  "Switches the INTERPRETER's maintained state and returns no value."
  (declare (type Interpreter interpreter))
  (setf (program-state interpreter)
    (case (program-state interpreter)
      (1 2)
      (2 1)
      (otherwise
        (error "Invalid program state: ~d."
          (program-state interpreter)))))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the OkayakO COMMAND in the INTERPRETER's context and
     returns no value.")
  
  (:method ((interpreter Interpreter)
            (command     (eql :switch-state)))
    (declare (type Interpreter interpreter))
    (declare (type command     command))
    (declare (ignore           command))
    (switch-state interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     (eql :reset-or-increment)))
    (declare (type Interpreter interpreter))
    (declare (type command     command))
    (declare (ignore           command))
    (case (program-state interpreter)
      (1 (setf (accumulator interpreter) 0))
      (2 (incf (accumulator interpreter) 1))
      (otherwise
        (error "Invalid program state: ~d."
          (program-state interpreter))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     (eql :change-to-second-state-or-decrement)))
    (declare (type Interpreter interpreter))
    (declare (type command     command))
    (declare (ignore           command))
    (case (program-state interpreter)
      (1 (setf (program-state interpreter) 2))
      (2 (decf (accumulator   interpreter) 1))
      (otherwise
        (error "Invalid program state: ~d."
          (program-state interpreter))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     (eql :execute-or-change-to-first-state)))
    (declare (type Interpreter interpreter))
    (declare (type command     command))
    (declare (ignore           command))
    (case (program-state interpreter)
      (1 (when (can-execute-object-p interpreter
                 (accumulator interpreter))
           (execute-object interpreter
             (get-object-by-id
               (get-objects interpreter)
               (accumulator interpreter)))))
      (2 (setf (program-state interpreter) 1))
      (otherwise
        (error "Invalid program state: ~d."
          (program-state interpreter))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     (eql :output)))
    (declare (type Interpreter interpreter))
    (declare (type command     command))
    (declare (ignore           command))
    (format T "~d "
      (accumulator interpreter))
    (values)))

;;; -------------------------------------------------------

(defun execute-object (interpreter object)
  "Executes the OKayakO OBJECT's statements in the INTERPRETER's context
   and returns no value."
  (declare (type Interpreter    interpreter))
  (declare (type Okayako-Object object))
  (dolist (command (okayako-object-statements object))
    (declare (type command command))
    (process-command interpreter command))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the OkayakO program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (when (get-program interpreter)
    (execute-object interpreter
      (first
        (get-program interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-OkayakO (code)
  "Interprets the piece of OkayakO source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "1, 2, 3, 4".
(interpret-OkayakO
  "0:
   @!\"@`
   1:
   @!\"@`
   2:
   @!\"@`
   3:
   @!\"")

;;; -------------------------------------------------------

;; Count down from four (4) to one (1).
(interpret-OkayakO
  "
  0:
  @!!!!\"@`

  1:

  2:
  @#\"@`

  3:
  @#\"@`

  4:
  @#\"@`
  ")
