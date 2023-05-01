;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the Tablebase lexer, parser, and interpreter,
;; responsible for the analyzation, loading, and execution of a piece of
;; Tablebase source code.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :tablebase.interpreter
  (:use
    :cl
    :tablebase.calculator)
  (:export
    #:interpret-Tablebase
    #:load-Tablebase-file))

;;; -------------------------------------------------------

(in-package :tablebase.interpreter)



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
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
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

(deftype command ()
  "The ``command'' type enumerates the recognized Tablebase instruction
   variants."
  '(member
    ;; A...
    :access-cell
    :output-character
    :loop-times
    :nop
    :pause-until-input
    :execute-if-not-zero
    :start-control

    ;; B...
    :return-error
    :output-number
    :loop-until-zero
    :end-comment
    :start-comment
    :print-source-code
    :end-control

    ;; C...
    :input-number
    :cube
    :nop
    :nop
    :modulo
    :nop
    :terminate-program

    ;; D...
    :wait-one-second
    :clear-output
    :input-character
    :square
    :nop
    :store-string
    :suppress-next-newline

    ;; E...
    :add
    :set-to-zero
    :floor
    :set-to-infinity
    :nop
    :nop
    :export-output

    ;; F...
    :subtract
    :multiply
    :nop
    :open-calculator
    :supply-random-number
    :delete-program
    :wait-random-interval

    ;; G...
    :reset-memory
    :copy-value
    :repeat-thrice
    :output-memory-location
    :execute-haphazardly
    :return-interpreter-stats
    :set-to-decimal-number))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a registry for the castaldy of
   the Tablebase language identifier names to internally operating
   representations, modeled as a hash table which affiliates each
   two-character name to a ``command''."
  '(hash-table-of (simple-string 2) command))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an unsigned integer number
   greater than or equal to zero (0), but with no upper bourne, thus
   spanning the range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' type defines an association of attribute
   names to values, molded into a hash table whose keys represent the
   identifiers as keyword symbols, affiliated with arbitrary values."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(deftype attribute-list ()
  "The ``attribute-list'' type defines a flat list of attribute
   name-value pairs conformant to the property list (plist) convention,
   with each name followed by the value."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (key value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep key   'keyword)
                     (typep value 'T))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more abstract
   syntax tree nodes."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype tb-object ()
  "The ``tb-object'' type defines the valid object categories in
   currency during a Tablebase program's execution, namely, integers and
   the sentinels for infinity and negative infinity."
  '(or integer
       (member :infinity :-infinity)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as an association of
   coordinates to arbitrary objects, manifesting in the form of a hash
   table which maps a coordinate's string representation to a
   ``tb-object''."
  '(hash-table-of string tb-object))

;;; -------------------------------------------------------

(deftype pathname-designator ()
  "The ``pathname-designator'' type defines the valid sources for a
   file's obtention."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant object extracted from
   a piece of Tablebase source code."
  (type  (error "Missing token type.")  :type keyword)
  (value (error "Missing token value.") :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a word constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (whitespace-character-p candidate))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents an arithmetic sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun coordinate-string-p (word)
  "Determines whether the WORD represents a coordinate designator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string word))
  (flet
      ((sufficiently-long-p ()
        "Checks whether the WORD is sufficiently long to be a potential
         coordinate designator, returning on confirmation a ``boolean''
         value of ``T'', otherwise ``NIL''."
        (the boolean
          (not (null (>= (length word) 2)))))
       (starts-with-letter-p ()
        "Checks whether the WORD starts with a Latin alphabetic letter,
         returning on confirmation a ``boolean'' value of ``T'',
         otherwise ``NIL''."
        (the boolean
          (not (null
            (alpha-char-p (char word 0))))))
       (ends-in-number-p ()
        "Checks whether the WORD, proceeding from its second letter,
         consists of decimal digits only, returning on confirmation a
         ``boolean'' value of ``T'', otherwise ``NIL''."
        (the boolean
          (loop
            for index of-type fixnum from 1 below (length word)
            always (digit-char-p (char word index))))))
    (the boolean
      (not (null
        (and (sufficiently-long-p)
             (starts-with-letter-p)
             (ends-in-number-p)))))))

;;; -------------------------------------------------------

(defun unsigned-number-string-p (string)
  "Determines whether the STRING represents an unsigned integer number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string string))
  (the boolean
    (not (null
      (every #'digit-char-p string)))))

;;; -------------------------------------------------------

(defun signed-number-string-p (string)
  "Determines whether the STRING represents a signed integer number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string string))
  (let ((string-length (length string)))
    (declare (type fixnum string-length))
    (the boolean
      (not (null
        (and
          ;; Is the STRING sufficiently long to carry a sign and digits?
          (>= string-length 2)
          ;; Does the STRING commence with a sign?
          (sign-character-p (char string 0))
          ;; Are all characters following the sign decimal digits?
          (loop
            for index of-type fixnum from 1 below string-length
            always (digit-char-p (char string index)))))))))

;;; -------------------------------------------------------

(defun number-string-p (string)
  "Determines whether the STRING represents a signed or unsigned integer
   number, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string string))
  (the boolean
    (or
      (signed-number-string-p   string)
      (unsigned-number-string-p string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux
       (position 0)
       (character
         (when (array-in-bounds-p source position)
           (char source position))))))
  "The ``Lexer'' class serves to analyze a piece of Tablebase code in
   order to extract its significant objects in the form of tokens."
  (source    (error "Missing source.") :type string)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'' and
   ``character'' to eponymous local symbol macros, defines another
   local symbol macro ``eof-p'' which determines whether the LEXER's
   source is exhausted, evaluates the BODY forms, and returns the last
   evaluated form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer)
                (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer)))
            (eof-p
             (the boolean
               (null (lexer-character ,evaluated-lexer)))))
         (declare (type string              source)
                  (ignorable                source))
         (declare (type fixnum              position)
                  (ignorable                position))
         (declare (type (or null character) character)
                  (ignorable                character))
         (declare (type boolean             eof-p)
                  (ignorable                eof-p))
         (flet
             ((advance ()
               "Moves the POSITION cursor to the next character in the
                SOURCE, if possible, updates the current CHARACTER, and
                returns no value."
               (setf character
                 (when (array-in-bounds-p source (1+ position))
                   (char source (incf position))))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun lexer-read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   word, demarcated by whitespaces or the end of the source, and
   returns the content as a string."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the string
      (with-output-to-string (word)
        (declare (type string-stream word))
        (loop while (and (not eof-p) (word-character-p character)) do
          (write-char character word)
          (advance))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   word, interprets it, and returns a token representation."
  (declare (type Lexer lexer))
  (let ((word (lexer-read-word lexer)))
    (declare (type string word))
    (the Token
      (cond
        ((coordinate-string-p word)
          (make-token :coordinate word))
        ((number-string-p word)
          (make-token :number (parse-integer word)))
        (T
          (make-token :word word))))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more adjacent whitespaces and returns the
   modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and (not eof-p) (whitespace-character-p character)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each query with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        (T
          (lexer-read-identifier lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'equal)
  "Associates the recognized command names with the respective
   ``command'' objects.")

;;; -------------------------------------------------------

(flet
    ((register-identifier (name command)
      "Associates the NAME with the COMMAND in the +IDENTIFIERS+ table,
       superseding any extant entry with the NAME as its key, and
       returns no value."
      (declare (type string  name))
      (declare (type command command))
      (setf (gethash name +IDENTIFIERS+) command)
      (values)))
  
  (register-identifier "A1" :access-cell)
  (register-identifier "A2" :output-character)
  (register-identifier "A3" :loop-times)
  (register-identifier "A4" :nop)
  (register-identifier "A5" :pause-until-input)
  (register-identifier "A6" :execute-if-not-zero)
  (register-identifier "A7" :start-control)
  
  (register-identifier "B1" :return-error)
  (register-identifier "B2" :output-number)
  (register-identifier "B3" :loop-until-zero)
  (register-identifier "B4" :end-comment)
  (register-identifier "B5" :start-comment)
  (register-identifier "B6" :print-source-code)
  (register-identifier "B7" :end-control)
  
  (register-identifier "C1" :input-number)
  (register-identifier "C2" :cube)
  (register-identifier "C3" :nop)
  (register-identifier "C4" :nop)
  (register-identifier "C5" :modulo)
  (register-identifier "C6" :nop)
  (register-identifier "C7" :terminate-program)
  
  (register-identifier "D1" :wait-one-second)
  (register-identifier "D2" :clear-output)
  (register-identifier "D3" :input-character)
  (register-identifier "D4" :square)
  (register-identifier "D5" :nop)
  (register-identifier "D6" :store-string)
  (register-identifier "D7" :suppress-next-newline)
  
  (register-identifier "E1" :add)
  (register-identifier "E2" :set-to-zero)
  (register-identifier "E3" :floor)
  (register-identifier "E4" :set-to-infinity)
  (register-identifier "E5" :nop)
  (register-identifier "E6" :nop)
  (register-identifier "E7" :export-output)
  
  (register-identifier "F1" :subtract)
  (register-identifier "F2" :multiply)
  (register-identifier "F3" :nop)
  (register-identifier "F4" :open-calculator)
  (register-identifier "F5" :supply-random-number)
  (register-identifier "F6" :delete-program)
  (register-identifier "F7" :wait-random-interval)
  
  (register-identifier "G1" :reset-memory)
  (register-identifier "G2" :copy-value)
  (register-identifier "G3" :repeat-thrice)
  (register-identifier "G4" :output-memory-location)
  (register-identifier "G5" :execute-haphazardly)
  (register-identifier "G6" :return-interpreter-stats)
  (register-identifier "G7" :set-to-decimal-number)
  
  (values))

;;; -------------------------------------------------------

(defun command-name-p (name)
  "Checks whether the NAME identifies a Tablebase instruction,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash name +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun get-command (name)
  "Returns the command associated with the NAME, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type string name))
  (multiple-value-bind (command contains-name-p)
      (gethash name +IDENTIFIERS+)
    (declare (type (or null command) command))
    (declare (type T                 contains-name-p))
    (the command
      (if contains-name-p
        command
        (error "No valid command name: ~s." name)))))

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Checks whether the TOKEN represents a command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   A command token is defined as a coordinate whose row-column
   locator can be attested in the +IDENTIFIERS+ table."
  (declare (type Token token))
  (the boolean
    (not (null
      (and
        (token-type-p   token :coordinate)
        (command-name-p (token-value token)))))))

;;; -------------------------------------------------------

(defun command-token-of-p (token expected-command)
  "Checks whether the TOKEN represents the EXPECTED-COMMAND, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type command expected-command))
  (the boolean
    (not (null
      (and
        (command-token-p token)
        (eq (get-command (token-value token))
            expected-command))))))

;;; -------------------------------------------------------

(defun get-token-command (token)
  "Returns the command associated with the TOKEN, or signals an error
   of an unspecified type upon its disrespondency."
  (declare (type Token token))
  (the (or null command)
    (when (command-token-p token)
      (get-command
        (token-value token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class represents a subtree in an abstract syntax tree
   (AST), encapsulating all data requisite the description of a
   language construct encountered during a Tablebase program's parsing."
  (type       (error "Missing node type.")  :type keyword)
  (attributes (make-hash-table :test #'eql) :type attribute-map))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the TYPE, optionally
   comprehending the INITIAL-ATTRIBUTES."
  (declare (type keyword        type))
  (declare (type attribute-list initial-attributes))
  (let ((node (initialize-node type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes node))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value registered with the ATTRIBUTE-NAME in the NODE, or
   signals an error of an unspecified type upon its absence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-name-p)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-name-p))
    (the T
      (if contains-name-p
        attribute-value
        (error "Unrecognized attribute name ~s for node ~s."
          attribute-name node)))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (loop
    initially
      (format stream "(NODE ~s" (node-type node))
    for attribute-name
      of-type keyword
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    do
      (format stream ", ~s=~a" attribute-name attribute-value)
    finally
      (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser
    (lexer
     &aux (current-token (lexer-get-next-token lexer)))))
  "The ``Parser'' class assembles an abstract syntax tree (AST) from
   the tokens provided by a lexer."
  (lexer         (error "Missing lexer.") :type Lexer)
  (current-token (make-token :eof NIL)    :type Token))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) Node) parser-parse-access-cell))

(declaim (ftype (function (Parser) Node) parser-parse-control))

(declaim (ftype (function (Parser) (or null Node))
                parser-parse-instruction))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and
   ``current-token'' to eponymous local symbol macros, processes the
   BODY forms, and returns the last evaluated form's results.
   ---
   Applying itself to some mete of supererogation, the macro
   additionally defines three local functions helpful for the current
   token's processing:
   
     ------------------------------------------------------------------
     Local function       | Effect
     ---------------------+--------------------------------------------
     eat-current-token () | Returns the current token, while
                          | concomitantly replacing it by the next one
                          | from the lexer.
     ..................................................................
     eat (type)           | If the current token conforms to the
                          | specified TYPE, returns it, while
                          | concomitantly replacing it by the next one
                          | from the lexer; otherwise signals an error.
     ..................................................................
     eat-command (type)   | If the current token represents a command
                          | identifier of the specified TYPE, returns
                          | it, while concomitantly replacing it by the
                          | next one from the lexer; otherwise signals
                          | an error.
     ------------------------------------------------------------------"
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser)
                (ignorable   ,evaluated-parser))
       (symbol-macrolet
           ((lexer
             (the Lexer (parser-lexer ,evaluated-parser)))
            (current-token
             (the Token (parser-current-token ,evaluated-parser))))
         (declare (type Lexer lexer)
                  (ignorable  lexer))
         (declare (type Token current-token)
                  (ignorable  current-token))
         (flet
             ((eat-current-token ()
               "Returns the CURRENT-TOKEN while concomitantly replacing
                it in the PARSER by the lexer's next token."
               (the Token
                 (prog1 current-token
                   (setf current-token
                     (lexer-get-next-token lexer)))))
              (eat (expected-token-type)
               "Determines whether the CURRENT-TOKEN conforms to the
                EXPECTED-TOKEN-TYPE, on confirmation returning the
                CURRENT-TOKEN while concomitantly replacing it in the
                PARSER by the lexer's next token; otherwise, upon a
                disrespondency an error of an unspecified type is
                signaled."
               (declare (type keyword expected-token-type))
               (the Token
                 (if (token-type-p current-token expected-token-type)
                   (prog1 current-token
                     (setf current-token
                       (lexer-get-next-token lexer)))
                   (error "Expected a token of the type ~s, ~
                           but encountered ~s."
                     expected-token-type current-token))))
              (eat-command (expected-command-type)
               "Determines whether the CURRENT-TOKEN represents the
                EXPECTED-COMMAND-TYPE, on confirmation returning the
                CURRENT-TOKEN while concomitantly replacing it in the
                PARSER by the lexer's next token; otherwise, upon a
                disrespondency an error of an unspecified type is
                signaled."
               (declare (type keyword expected-command-type))
               (the Token
                 (if (command-token-of-p current-token
                                         expected-command-type)
                   (prog1 current-token
                     (setf current-token
                       (lexer-get-next-token lexer)))
                   (error "Expected a command token of the type ~s, ~
                           but encountered ~s."
                     expected-command-type current-token)))))
           ,@body)))))

;;; -------------------------------------------------------

(defun parser-parse-coordinate (parser)
  "Using the PARSER, parses a memory coordinate and returns a
   ``:coordinate'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Node
      (make-node :coordinate :value
        (token-value
          (eat :coordinate))))))

;;; -------------------------------------------------------

(defun parser-parse-literal-number (parser)
  "Using the PARSER, parses a decimal integer number and returns a
   ``:literal-number'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Node
      (make-node :literal-number
        :value (token-value (eat :number))))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Using the PARSER, parses a decimal integer number and returns a
   ``:literal-number'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (cond
      ((token-type-p current-token :number)
        (parser-parse-literal-number parser))
      ((command-token-of-p current-token :access-cell)
        (parser-parse-access-cell parser))
      (T
        (error "Unexpected number token: ~s." current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-niladic-instruction (parser node-type)
  "Using the PARSER, parses an instruction independent of parameters and
   returns a NODE-TYPE node representation thereof."
  (declare (type Parser  parser))
  (declare (type keyword node-type))
  (with-parser (parser)
    (eat-current-token)
    (the Node (make-node node-type))))

;;; -------------------------------------------------------

(defun parser-parse-unary-operation (parser)
  "Using the PARSER, parses a command which relies on a coordinate as
   its aefauld operand and returns a node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((operator (eat-current-token))
          (operand  (parser-parse-coordinate parser)))
      (declare (type Token operator))
      (declare (type Node  operand))
      (the Node
        (make-node (get-token-command operator)
          :coordinate operand)))))

;;; -------------------------------------------------------

(defun parser-parse-binary-operation (parser)
  "Using the PARSER, parses a binary arithmetic operation using and
   returns a node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((operator      (eat-current-token))
          (left-operand  (parser-parse-coordinate parser))
          (right-operand (parser-parse-coordinate parser)))
      (declare (type Token operator))
      (declare (type Node  left-operand))
      (declare (type Node  right-operand))
      (the Node
        (make-node (get-token-command operator)
          :left-operand  left-operand
          :right-operand right-operand)))))

;;; -------------------------------------------------------

;; A1
(defun parser-parse-access-cell (parser)
  "Using the PARSER, parses a cell access (\"A1\") command and returns
   a ``:access-cell'' node representation thereof."
  (declare (type Parser parser))
  (the Node
    (parser-parse-unary-operation parser)))

;;; -------------------------------------------------------

;; A3
(defun parser-parse-loop-times (parser)
  "Using the PARSER, parses a loop based on a specific repetition tally
   (command \"A3\") and returns a ``:loop-times'' node representation
   thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :loop-times)
    (let ((coordinate (parser-parse-coordinate parser))
          (control    (parser-parse-control    parser)))
      (declare (type Node coordinate))
      (declare (type Node control))
      (the Node
        (make-node :loop-times
          :coordinate coordinate
          :control    control)))))

;;; -------------------------------------------------------

;; A6
(defun parser-parse-execute-if-not-zero (parser)
  "Using the PARSER, parses a conditional construct based upon a
   non-zero cell test (command \"A6\") and returns an
   ``:execute-if-not-zero'' node repesentation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :execute-if-not-zero)
    (let ((coordinate (parser-parse-coordinate parser))
          (control    (parser-parse-control    parser)))
      (declare (type Node coordinate))
      (declare (type Node control))
      (the Node
        (make-node :execute-if-not-zero
          :coordinate coordinate
          :control    control)))))

;;; -------------------------------------------------------

;; B3
(defun parser-parse-loop (parser)
  "Using the PARSER, parses a non-zero-based while-loop (command \"B3\")
   and returns a ``:loop-until-zero'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :loop-until-zero)
    (let ((coordinate (parser-parse-coordinate parser))
          (control    (parser-parse-control    parser)))
      (declare (type Node coordinate))
      (declare (type Node control))
      (the Node
        (make-node :loop-until-zero
          :coordinate coordinate
          :control    control)))))

;;; -------------------------------------------------------

;; B5 ... B4
(defun parser-parse-comment (parser)
  "Using the PARSER, parses a comment block and returns a ``:comment''
   node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :start-comment)
    (loop do
      (cond
        ((token-type-p current-token :eof)
            (error "Unterminated comment."))
        ((command-token-of-p current-token :end-comment)
          (eat-command :end-comment)
          (return
            (make-node :comment)))
        (T
          (eat-current-token))))))

;;; -------------------------------------------------------

;; B6
(defun parser-parse-print-source-code (parser)
  "Using the PARSER, parses a quine command (\"B6\") and returns a
   ``:print-source-code'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :print-source-code)
    (the Node
      (make-node :print-source-code
        :code (lexer-source lexer)))))

;;; -------------------------------------------------------

;; F5
(defun parser-parse-supply-random-number (parser)
  "Using the PARSER, parses a random number generation command (\"F5\")
   and returns a ``:supply-random-number'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :supply-random-number)
    (let ((minimum (parser-parse-coordinate parser))
          (maximum (parser-parse-coordinate parser)))
      (declare (type Node minimum))
      (declare (type Node maximum))
      (the Node
        (make-node :supply-random-number
          :minimum minimum
          :maximum maximum)))))

;;; -------------------------------------------------------

;; G2
(defun parser-parse-copy-value (parser)
  "Using the PARSER, parses a cell copy command (\"G2\") and returns a
   ``:copy-value'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :copy-value)
    (the Node
      (make-node :copy-value
        :target (parser-parse-coordinate parser)
        :source (parser-parse-coordinate parser)))))

;;; -------------------------------------------------------

;; G3
(defun parser-parse-repeat-thrice (parser)
  "Using the PARSER, parses a thrice repeating loop command (\"G3\") and
   returns a ``:repeat-thrice'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :repeat-thrice)
    (let ((command (parser-parse-instruction parser)))
      (declare (type Node command))
      (the Node
        (make-node :repeat-thrice
          :command command)))))

;;; -------------------------------------------------------

;; G4
(defun parser-parse-output-memory-location (parser)
  "Using the PARSER, parses a cell finding command (\"G4\") and returns
   an ``:output-memory-location'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :output-memory-location)
    (the Node
      (make-node :output-memory-location
        :value (parser-parse-number parser)))))

;;; -------------------------------------------------------

;; G5
(defun parser-parse-execute-haphazardly (parser)
  "Using the PARSER, parses a random execution command (\"G5\") and
   returns an ``:execute-haphazardly'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :execute-haphazardly)
    (let ((command (parser-parse-instruction parser)))
      (declare (type Node command))
      (the Node
        (make-node :execute-haphazardly
          :command command)))))

;;; -------------------------------------------------------

;; G7
(defun parser-parse-set-to-decimal-number (parser)
  "Using the PARSER, parses a literal assignment command (\"G7\") and
   returns a ``:set-to-decimal-number'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :set-to-decimal-number)
    (let ((coordinate (parser-parse-coordinate parser))
          (value      (parser-parse-number     parser)))
      (declare (type Node coordinate))
      (declare (type Node value))
      (the Node
        (make-node :set-to-decimal-number
          :coordinate coordinate
          :value      value)))))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Using the PARSER, parses a single command and returns a node
   representation thereof.
   ---
   If the PARSER's current token does not denote an instruction, the
   ``NIL'' sentinel will be delivered."
  (declare (type Parser parser))
  (let ((command (get-token-command (parser-current-token parser))))
    (declare (type (or null command) command))
    (the (or null Node)
      (case command
        ((NIL)
          NIL)
        (:nop
          (parser-parse-niladic-instruction parser :nop))
        ;; A1
        (:access-cell
          (parser-parse-access-cell parser))
        ;; A2
        (:output-character
          (parser-parse-unary-operation parser))
        ;; A3
        (:loop-times
          (parser-parse-loop-times parser))
        ;; A5
        (:pause-until-input
          (parser-parse-niladic-instruction parser :pause-until-input))
        ;; A6
        (:execute-if-not-zero
          (parser-parse-execute-if-not-zero parser))
        ;; B1
        (:return-error
          (parser-parse-niladic-instruction parser :return-error))
        ;; B2
        (:output-number
          (parser-parse-unary-operation parser))
        ;; B3 (loop while not zero)
        (:loop-until-zero
          (parser-parse-loop parser))
        ;; B5 ... B4
        (:start-comment
          (parser-parse-comment parser))
        ;; B6
        (:print-source-code
          (parser-parse-print-source-code parser))
        ;; C1
        (:input-number
          (parser-parse-unary-operation parser))
        ;; C5
        (:modulo
          (parser-parse-binary-operation parser))
        ;; C2
        (:cube
          (parser-parse-unary-operation parser))
        ;; C7
        (:terminate-program
          (parser-parse-niladic-instruction parser :terminate-program))
        ;; D1
        (:wait-one-second
          (parser-parse-niladic-instruction parser :wait-one-second))
        ;; D2
        (:clear-output
          (parser-parse-niladic-instruction parser :clear-output))
        ;; D3
        (:input-character
          (parser-parse-unary-operation parser))
        ;; D4
        (:square
          (parser-parse-unary-operation parser))
        ;; D6
        (:store-string
          (parser-parse-unary-operation parser))
        ;; D7
        (:suppress-next-newline
          (parser-parse-niladic-instruction parser
            :suppress-next-newline))
        ;; E1
        (:add
          (parser-parse-binary-operation parser))
        ;; E2
        (:set-to-zero
          (parser-parse-unary-operation parser))
        ;; E3
        (:floor
          (parser-parse-binary-operation parser))
        ;; E4
        (:set-to-infinity
          (parser-parse-unary-operation parser))
        ;; E7
        (:export-output
          (parser-parse-niladic-instruction parser :export-output))
        ;; F1
        (:subtract
          (parser-parse-binary-operation parser))
        ;; F2
        (:multiply
          (parser-parse-binary-operation parser))
        ;; F4
        (:open-calculator
          (parser-parse-niladic-instruction parser :open-calculator))
        ;; F5
        (:supply-random-number
          (parser-parse-supply-random-number parser))
        ;; F6
        (:delete-program
          (parser-parse-niladic-instruction parser :delete-program))
        ;; F7
        (:wait-random-interval
          (parser-parse-niladic-instruction parser
            :wait-random-interval))
        ;; G1
        (:reset-memory
          (parser-parse-niladic-instruction parser :reset-memory))
        ;; G2
        (:copy-value
          (parser-parse-copy-value parser))
        ;; G3
        (:repeat-thrice
          (parser-parse-repeat-thrice parser))
        ;; G4
        (:output-memory-location
          (parser-parse-output-memory-location parser))
        ;; G5
        (:execute-haphazardly
          (parser-parse-execute-haphazardly parser))
        ;; G6
        (:return-interpreter-stats
          (parser-parse-niladic-instruction parser
            :return-interpreter-stats))
        ;; G7
        (:set-to-decimal-number
          (parser-parse-set-to-decimal-number parser))
        (otherwise
          (error "Unrecognized command token: ~s." command))))))

;;; -------------------------------------------------------

;; A7 ... B7
(defun parser-parse-control (parser)
  "Using the PARSER, parses a control block, demarcated by the command
   identifiers \"A7\" and \"B7\", and returns a ``:control'' node
   representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-command :start-control)
    (let ((instructions NIL))
      (declare (type node-list instructions))
      (loop do
        (cond
          ((token-type-p current-token :eof)
            (error "Unterminated control block."))
          ((command-token-of-p current-token :end-control)
            (eat-command :end-control)
            (loop-finish))
          (T
            (push (parser-parse-instruction parser) instructions))))
      (the Node
        (make-node :control :body (nreverse instructions))))))

;;; -------------------------------------------------------

(defun parser-parse-instructions (parser)
  "Using the PARSER, parses a sequence of zero or more instructions and
   returns a list comprehending their representations."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((instructions NIL))
      (declare (type node-list instructions))
      (loop do
        (case (token-type current-token)
          (:eof
            (loop-finish))
          (otherwise
            (push (parser-parse-instruction parser) instructions))))
      (the node-list
        (nreverse instructions)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Using the PARSER's tokens to assemble an abstract syntax tree (AST)
   and returns the same."
  (declare (type Parser parser))
  (the Node
    (make-node :program
      :statements (parser-parse-instructions parser))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun memory-cell-at (memory coordinate)
  "Returns the value of the MEMORY cell amenable to the COORDINATE,
   responding for a not explicitly specified unit with the default value
   of zero (0)."
  (declare (type memory memory))
  (declare (type string coordinate))
  (the tb-object
    (gethash coordinate memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory coordinate)
  "Sets the MEMORY cell at the COORDINATE to the NEW-VALUE and returns
   no value.
   ---
   Pursuing space-efficiency, a cell intended to receive the NEW-VALUE
   zero (0) will be deleted rather than updated, reminiscent of the
   default value 0."
  (declare (type tb-object new-value))
  (declare (type memory    memory))
  (declare (type string    coordinate))
  (if (zerop new-value)
    (remhash coordinate memory)
    (setf (gethash coordinate memory 0) new-value))
  (values))

;;; -------------------------------------------------------

(defun memory-reset (memory)
  "Sets all MEMORY cells to zero and returns no value."
  (declare (type memory memory))
  (clrhash memory)
  (values))

;;; -------------------------------------------------------

(defun memory-find-value (memory value)
  "Returns the coordinate of the first MEMORY cell ensconcing the VALUE,
   or ``NIL'' if no such salvatory can be detected."
  (declare (type memory  memory))
  (declare (type integer value))
  (let ((minimum-coordinate NIL))
    (declare (type (or null string) minimum-coordinate))
    (loop
      for coordinate
        of-type string
        being the hash-keys in memory
      using
        (hash-value cell-value)
      do
        (when (and (= cell-value value)
                   (or (null minimum-coordinate)
                       (string< coordinate
                                minimum-coordinate)))
          (setf minimum-coordinate coordinate)))
    (the (or null string) minimum-coordinate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation unary operations.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:method ((operator (eql :cube)) (operand integer))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type integer operand))
    (the integer
      (* operand operand operand)))
  (:method ((operator (eql :cube)) (operand (eql :infinity)))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type keyword operand)
             (ignore       operand))
    (the keyword :infinity))
  (:method ((operator (eql :cube)) (operand (eql :-infinity)))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type keyword operand)
             (ignore       operand))
    (the keyword :-infinity))
  
  (:method ((operator (eql :square)) (operand integer))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type integer operand))
    (the integer
      (* operand operand)))
  (:method ((operator (eql :square)) (operand (eql :infinity)))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type keyword operand)
             (ignore       operand))
    (the keyword :infinity))
  (:method ((operator (eql :square)) (operand (eql :-infinity)))
    (declare (type keyword operator)
             (ignore       operator))
    (declare (type keyword operand)
             (ignore       operand))
    (the keyword :infinity))
  
  (:documentation
    "Applies the unary OPERATOR on its sole OPERAND and returns a value
     appropriate for this combination."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation binary operations.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR on the LEFT-OPERAND and the
     RIGHT-OPERAND and returns a result appropriate for this
     combination."))

;;; -------------------------------------------------------

(defun get-dispatch-type (operand-type)
  "Returns for the OPERAND-TYPE a dispatching specifier compatible with
   the ``defmethod'' argument list."
  (declare (type symbol operand-type))
  (the T
    (case operand-type
      (integer    'integer)
      (:infinity  '(eql :infinity))
      (:-infinity '(eql :-infinity))
      (otherwise  (error "Invalid operand type: ~s." operand-type)))))

;;; -------------------------------------------------------

(defun get-declaration-type (operand-type operand-name)
  "Returns for the OPERAND-TYPE, designated by the OPERAND-NAME, a
   ``declare''-compatible type specifier."
  (declare (type symbol operand-type))
  (declare (type symbol operand-name))
  (the list
    (case operand-type
      (integer    `(declare (type integer ,operand-name)))
      (:infinity  `(declare (type keyword ,operand-name)))
      (:-infinity `(declare (type keyword ,operand-name)))
      (otherwise  (error "Invalid operand type ~s with name ~s."
                    operand-type operand-name)))))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    (operator (left-operand-type right-operand-type)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', employing the OPERATOR as the dispatching
   first parameter, norned ``operator'', the LEFT-OPERAND-TYPE for the
   second parameter ``left'', and the RIGHT-OPERATOR-TYPE for the third
   parameter ``right'', evaluates the BODY forms, and returns the last
   modified form's results."
  `(defmethod apply-binary-operator
       ((operator (eql ,operator))
        (left     ,(get-dispatch-type left-operand-type))
        (right    ,(get-dispatch-type right-operand-type)))
     (declare (type keyword operator))
     ,(get-declaration-type left-operand-type 'left)
     ,(get-declaration-type right-operand-type 'right)
     (declare (ignorable operator))
     (declare (ignorable left))
     (declare (ignorable right))
     ,@body))

;;; -------------------------------------------------------

(define-binary-operation :add (integer integer)
  (the integer
    (+ left right)))

;;; -------------------------------------------------------

(define-binary-operation :add (integer :infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :add (integer :-infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :add (:infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :add (:infinity :infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :add (:infinity :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :add (:-infinity integer)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :add (:-infinity :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :add (:-infinity :-infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (integer integer)
  (the integer
    (- left right)))

;;; -------------------------------------------------------

(define-binary-operation :subtract (integer :infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (integer :-infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:infinity :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:infinity :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:-infinity integer)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:-infinity :infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :subtract (:-infinity :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :multiply (integer integer)
  (the integer
    (* left right)))

;;; -------------------------------------------------------

(define-binary-operation :multiply (integer :infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (integer :-infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:infinity :infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:infinity :-infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:-infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:-infinity :infinity)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :multiply (:-infinity :-infinity)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :floor (integer integer)
  (the integer
    (floor left right)))

;;; -------------------------------------------------------

(define-binary-operation :floor (integer :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :floor (integer :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :floor (:infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :floor (:infinity :infinity)
  (the integer 1))

;;; -------------------------------------------------------

(define-binary-operation :floor (:infinity :-infinity)
  (the integer -1))

;;; -------------------------------------------------------

(define-binary-operation :floor (:-infinity integer)
  (the keyword :-infinity))

;;; -------------------------------------------------------

(define-binary-operation :floor (:-infinity :infinity)
  (the integer -1))

;;; -------------------------------------------------------

(define-binary-operation :floor (:-infinity :-infinity)
  (the integer 1))

;;; -------------------------------------------------------

(define-binary-operation :modulo (integer integer)
  (the integer
    (rem left right)))

;;; -------------------------------------------------------

(define-binary-operation :modulo (integer :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :modulo (integer :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:infinity :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:infinity :-infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:-infinity integer)
  (the keyword :infinity))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:-infinity :infinity)
  (the integer 0))

;;; -------------------------------------------------------

(define-binary-operation :modulo (:-infinity :-infinity)
  (the integer 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of random number operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shall-execute-command-p ()
  "Determines with a chance of 50 percent whether an action shall be
   issued or not, returning a ``boolean'' value of ``T'' upon its
   admission, otherwise ``NIL''."
  (the boolean
    (not (null
      (plusp (random 2))))))

;;; -------------------------------------------------------

(defgeneric get-random-number-in-range (minimum maximum)
  ;; Range: [minimum, maximum].
  (:method ((minimum integer) (maximum integer))
    (declare (type integer minimum))
    (declare (type integer maximum))
    (the integer
      (+ minimum
         (random (1+ (- maximum minimum))))))
  
  ;; Range: [minimum, +infinity].
  (:method ((minimum integer) (maximum (eql :infinity)))
    (declare (type integer minimum))
    (declare (type keyword maximum)
             (ignore       maximum))
    (the integer
      (+ minimum
         (random (1+ (- most-positive-fixnum minimum))))))
  
  ;; Range: [-infinity, maximum].
  (:method ((minimum (eql :infinity)) (maximum integer))
    (declare (type keyword minimum)
             (ignore       minimum))
    (declare (type integer maximum))
    (error "The minimum of +infinity is greater than the bounded ~
            maximum of ~d."
      maximum))
  
  ;; Range: [-infinity, +infinity].
  (:method ((minimum (eql :infinity)) (maximum (eql :infinity)))
    (declare (type keyword minimum)
             (ignore       minimum))
    (declare (type keyword maximum)
             (ignore       maximum))
    (the integer
      (+ most-negative-fixnum
         (random (1+ (- most-positive-fixnum most-negative-fixnum))))))
  
  (:documentation
    "Returns a random integer in the closed range [MINIMUM, MAXIMUM]."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-ascii-code-for-string (characters)
  "Returns the bits of the ASCII characters encoded in an unsigned byte."
  (declare (type string characters))
  (let ((encoded-string 0))
    (declare (type unsigned-byte encoded-string))
    (loop for character of-type character across characters do
      (setf encoded-string
        (ash encoded-string 8))
      (setf (ldb (byte 8 0) encoded-string)
        (char-code character)))
    (the unsigned-byte encoded-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "tb-object" operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tb-object-equals-zero-p (tb-object)
  "Determines whether the TB-OBJECT represents an integer value and
   equals zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type tb-object tb-object))
  (the boolean
    (not (null
      (and
        (integerp tb-object)
        (zerop    tb-object))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of date and time operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-date-and-time (timestamp)
  "Creates and returns a new string which represents the numeric
   TIMESTAMP's encoded date and time information in a human-readable
   form."
  (declare (type integer timestamp))
  (multiple-value-bind
      (second minute hour day month year day-of-week
       uses-daylight-saving-time-p time-zone)
      (decode-universal-time timestamp)
    (declare (type (integer 0 59) second))
    (declare (type (integer 0 59) minute))
    (declare (type (integer 0 23) hour))
    (declare (type (integer 0 31) day))
    (declare (type (integer 1 12) month))
    (declare (type integer        year))
    (declare (type (integer 0  6) day-of-week))
    (declare (type T              uses-daylight-saving-time-p)
             (ignore              uses-daylight-saving-time-p))
    (declare (type rational       time-zone)
             (ignore              time-zone))
    (the string
      (format NIL "~[Monday~;Tuesday~;Wednesday~;Thursday~;Friday~;~
                     Saturday~;Sunday~], ~
                   ~d-~2,'0d-~2,'0d, ~2,'0d:~2,'0d:~2,'0d"
        day-of-week year month day hour minute second))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Program-Halt-Interrupt".        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Program-Halt-Interrupt (condition)
  ()
  (:documentation
    "The condition ``Program-Halt-Interrupt'' serves to signal the
     request to terminate a Tablebase program immediately, as stated by
     the command \"C7\"."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing interpreter tree.")
    :type          Node
    :documentation "The abstract syntax tree (AST) to evaluate.")
   (memory
    :initarg       :memory
    :initform      (make-hash-table :test #'equalp)
    :type          memory
    :documentation "The program memory simulating a potentially infinite
                    two-dimesional grid of signed integer cells.")
   (suppress-newline-p
    :initarg       :suppress-newline-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the next print command shall
                    abstain from appending a newline.")
   (output-buffer
    :initarg       :output-buffer
    :initform      (make-array 0
                     :element-type 'character
                     :adjustable   T
                     :fill-pointer 0)
    :type          string
    :documentation "Employed for the purpose of exporting the program
                    output as a file.")
   (export-file
    :initarg       :export-file
    :initform      "Tablebase_export.dat"
    :type          pathname-designator
    :documentation "Designates the file path under which to store the
                    program output upon request of the \"E7\" command.")
   (input-file
    :initarg       :input-file
    :initform      NIL
    :type          (or null pathname-designator)
    :documentation "If the Tablebase code has been obtained from a file,
                    stores its path for contingent deletion requests
                    issued by the \"F6\" command.")
   (shall-delete-program-p
    :initarg       :shall-delete-program-p
    :initform      NIL
    :type          boolean
    :documentation "Flag which determines whether the Tablebase
                    program's source file has been marked for deletion
                    after its execution, as decreed by the \"F6\"
                    command.")
   (minimum-delay
    :initarg       :minimum-delay
    :initform      1
    :type          (integer 0 *)
    :documentation "The minimum throttle for command \"F7\").")
   (maximum-delay
    :initarg       :maximum-delay
    :initform      5
    :type          (integer 0 *)
    :documentation "The maximum throttle for command \"F7\").")
   (startup-time
    :initarg       :startup-time
    :initform      0
    :type          integer
    :documentation "The timestamp of the system at the interpreter's
                    inception, that is, the moment that its
                    ``interpreter-interpret'' function has been
                    invoked, requisite for the \"G6\" command.")
   (viewport-height
    :initarg       :viewport-height
    :initform      20
    :type          non-negative-integer
    :documentation "The number of lines comprising the screen's vertical
                    extent, employed as a warkloom for simulating the
                    console's clearance by introduction of a tantamount
                    quantity of newline which shift the extant output
                    into an invisible area."))
  (:documentation
    "The ``Interpreter'' class serves to evaluate an abstract syntax
     tree (AST) representation of a Tablebase program."))

;;; -------------------------------------------------------

(defun make-interpreter (tree &key (input-file      NIL)
                                   (viewport-height 10))
  "Creates and returns a new ``Interpreter'' dedicated to the abstract
   syntax TREE's evaluation."
  (declare (type Node tree))
  (the Interpreter
    (make-instance 'Interpreter
      :tree            tree
      :input-file      input-file
      :viewport-height viewport-height)))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Using the INTERPRETER as a visitor, processes the NODE,
     dispatching on the NODE-TYPE, and returns a value suitable for
     NODE."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Using the INTERPRETER, processes the NODE, dispatching on its type
   in order to invoke the eligible ``interpreter-dispatch-node''
   method implementation, and returns the respective results."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (interpreter-dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmacro define-node-dispatch
    (node-type
     (interpreter-variable node-variable)
     &body body)
  "Defines an implementation of the generic function
   ``interpreter-dispatch-node'' employing the INTERPRETER-VARIABLE as
   the first parameter's name, the NODE-TYPE for dispatching the second
   parameter with an automatically generated name, and the
   NODE-VARIABLE as the third parameter's identifier, ensconcing in the
   implementation the BODY forms."
  (let ((node-type-variable (gensym)))
    (declare (type symbol node-type-variable))
    `(defmethod interpreter-dispatch-node
         ((,interpreter-variable Interpreter)
          (,node-type-variable   (eql ,node-type))
          (,node-variable        Node))
       (declare (type Interpreter ,interpreter-variable)
                (ignorable        ,interpreter-variable))
       (declare (type keyword     ,node-type-variable)
                (ignore           ,node-type-variable))
       (declare (type Node        ,node-variable)
                (ignorable        ,node-variable))
       ,@body)))

;;; -------------------------------------------------------

(defun interpreter-visit-nodes (interpreter nodes)
  "Using the INTERPRETER, processes the NODES in order and returns the
   last evaluated node's results."
  (declare (type Interpreter interpreter))
  (declare (type node-list   nodes))
  (let ((result NIL))
    (declare (type T result))
    (dolist (node nodes)
      (declare (type Node node))
      (setf result (interpreter-visit-node interpreter node)))
    (the T result)))

;;; -------------------------------------------------------

(defun interpreter-cell-at (interpreter coordinate)
  "Returns the value of the INTERPRETER's memory cell specified by the
   COORDINATE, responding for an undefined cell with the default value
   zero (0)."
  (declare (type Interpreter interpreter))
  (declare (type string      coordinate))
  (the tb-object
    (memory-cell-at
      (slot-value interpreter 'memory) coordinate)))

;;; -------------------------------------------------------

(defun (setf interpreter-cell-at) (new-value interpreter coordinate)
  "Stores the NEW-VALUE in the INTERPRETER's memory cell amenable to the
   COORDINATE and returns no value."
  (declare (type tb-object   new-value))
  (declare (type Interpreter interpreter))
  (declare (type string      coordinate))
  (setf (memory-cell-at (slot-value interpreter 'memory) coordinate)
        new-value)
  (values))

;;; -------------------------------------------------------

(defun interpreter-resolve-coordinate (interpreter node)
  "Interprets the NODE, construed as a memory coordinate specifier,
   utilizing the INTERPRETER, and returns the associated cell value."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the tb-object
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter node))))

;;; -------------------------------------------------------

(defun interpreter-print (interpreter
                          format-control
                          &rest format-arguments)
  "Generates the ``format''-like formatted output specified by the
   FORMAT-CONTROL in conjunction with the FORMAT-ARGUMENTS, writes
   the thus produced content the standard output and concomitantly to
   the INTERPRETER's internal output buffer for contingent export, and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      format-control))
  (declare (type (list-of T) format-arguments))
  (with-slots (output-buffer suppress-newline-p) interpreter
    (declare (type string  output-buffer))
    (declare (type boolean suppress-newline-p))
    (apply #'format T             format-control format-arguments)
    (apply #'format output-buffer format-control format-arguments)
    (unless suppress-newline-p
      (format T             "~%")
      (format output-buffer "~%"))
    (setf suppress-newline-p NIL))
  (values))

;;; -------------------------------------------------------

(defun interpreter-start-timer (interpreter)
  "Starts the INTERPRETER's internal timer, requisite for the "
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'startup-time)
        (get-universal-time))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-get-run-duration (interpreter)
  "Returns the elapsed time betwixt the inception of the INTERPRETER's
   ``interpreter-interpret'' and the invocation of this operation as a
   non-negative integer tallying the number of seconds, being a
   requisite for the \"G6\" command."
  (declare (type Interpreter interpreter))
  (the integer
    (- (get-universal-time)
       (slot-value interpreter 'startup-time))))

;;; -------------------------------------------------------

(defun interpreter-terminate-program (interpreter)
  "Manages the termination of the INTERPRETER's operations and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (shall-delete-program-p input-file) interpreter
    (declare (type boolean                      shall-delete-program-p))
    (declare (type (or null pathname-designator) input-file)
                   (ignorable                    input-file))
    (when (and shall-delete-program-p
               input-file
               (probe-file input-file))
      (delete-file input-file)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (the T
    (prog1
      (interpreter-visit-nodes interpreter
        (node-attribute node :statements))
      (interpreter-terminate-program interpreter))))

;;; -------------------------------------------------------

(define-node-dispatch :coordinate (interpreter node)
  (the string
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :literal-number (interpreter node)
  (the integer
    (node-attribute node :value)))

;;; -------------------------------------------------------

;; A4, C3, C4, C6, D5, E5, E6, F3
(define-node-dispatch :nop (interpreter node)
  (values))

;;; -------------------------------------------------------

;; A1
(define-node-dispatch :access-cell (interpreter node)
  (the tb-object
    (interpreter-resolve-coordinate interpreter
      (node-attribute node :coordinate))))

;;; -------------------------------------------------------

;; A2
(define-node-dispatch :output-character (interpreter node)
  (interpreter-print interpreter "~c"
    (code-char
      (interpreter-cell-at interpreter
        (interpreter-visit-node interpreter
          (node-attribute node :coordinate)))))
  (values))

;;; -------------------------------------------------------

;; A3
(define-node-dispatch :loop-times (interpreter node)
  (let ((coordinate (node-attribute node :coordinate))
        (control    (node-attribute node :control)))
    (declare (type Node coordinate))
    (declare (type Node control))
    (let ((repetitions
           (interpreter-cell-at interpreter
             (interpreter-visit-node interpreter coordinate))))
      (declare (type tb-object repetitions))
      
      (cond
        ((integerp repetitions)
          (loop repeat repetitions do
            (interpreter-visit-node interpreter control)))
        ((eq repetitions :infinity)
          (loop do
            (interpreter-visit-node interpreter control)))
        ((eq repetitions :-infinity)
          NIL)
        (T
          (error "Invalid loop repetition count: ~s." repetitions)))))
  (values))

;;; -------------------------------------------------------

;; A5
(define-node-dispatch :pause-until-input (interpreter node)
  (format T "~&Please commit an input in order to continue: ")
  (read-line)
  (clear-input)
  (values))

;;; -------------------------------------------------------

;; A6
(define-node-dispatch :execute-if-not-zero (interpreter node)
  (let ((coordinate (node-attribute node :coordinate))
        (control    (node-attribute node :control)))
    (declare (type Node coordinate))
    (declare (type Node control)
             (ignorable control))
    (unless (tb-object-equals-zero-p
              (interpreter-resolve-coordinate interpreter coordinate))
      (interpreter-visit-node interpreter control)))
  (values))

;;; -------------------------------------------------------

;; A7 ... B7
(define-node-dispatch :control (interpreter node)
  (interpreter-visit-nodes interpreter
    (node-attribute node :body))
  (values))

;;; -------------------------------------------------------

;; B1
(define-node-dispatch :return-error (interpreter node)
  (interpreter-print interpreter
    "*** AN INSENSIBLE ERROR HAS TRANSPIRED ***")
  (values))

;;; -------------------------------------------------------

;; B2
(define-node-dispatch :output-number (interpreter node)
  (interpreter-print interpreter "~a "
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :coordinate))))
  (values))

;;; -------------------------------------------------------

;; B3
(define-node-dispatch :loop-until-zero (interpreter node)
  (let ((coordinate (node-attribute node :coordinate))
        (control    (node-attribute node :control)))
    (declare (type Node coordinate))
    (declare (type Node control))
    (loop
      until
        (tb-object-equals-zero-p
          (interpreter-resolve-coordinate interpreter coordinate))
      do
        (interpreter-visit-node interpreter control)))
  (values))

;;; -------------------------------------------------------

;; B5 ... B4
(define-node-dispatch :comment (interpreter node)
  (values))

;;; -------------------------------------------------------

;; B6
(define-node-dispatch :print-source-code (interpreter node)
  (interpreter-print interpreter
    (node-attribute node :code))
  (values))

;;; -------------------------------------------------------

;; C1
(define-node-dispatch :input-number (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :coordinate)))
    (prog2
      (format T "~&Please input an integer number: ")
      (parse-integer (read-line))
      (clear-input)))
  (values))

;;; -------------------------------------------------------

;; C2
(define-node-dispatch :cube (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :coordinate)))
    (apply-unary-operator :cube
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :coordinate))))
  (values))

;;; -------------------------------------------------------

;; C5
(define-node-dispatch :modulo (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand)))
    (apply-binary-operator :modulo
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :left-operand))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :right-operand))))
  (values))

;;; -------------------------------------------------------

;; C7
(define-node-dispatch :terminate-program (interpreter node)
  (signal 'Program-Halt-Interrupt)
  (values))

;;; -------------------------------------------------------

;; D1
(define-node-dispatch :wait-one-second (interpreter node)
  (sleep 1)
  (values))

;;; -------------------------------------------------------

;; D2
(define-node-dispatch :clear-output (interpreter node)
  ;; Print sufficient tally of newlines to simulate console purging.
  (format T "~v%" (slot-value interpreter 'viewport-height))
  ;; Purge export file content by clearing its output-buffer.
  (setf (fill-pointer (slot-value interpreter 'output-buffer)) 0)
  (values))

;;; -------------------------------------------------------

;; D3
(define-node-dispatch :input-character (interpreter node)
  (format T "~&Please input an ASCII character: ")
  (let ((input (read-char)))
    (declare (type character input))
    (clear-input)
    (setf
      (interpreter-cell-at interpreter
        (interpreter-visit-node interpreter
          (node-attribute node :coordinate)))
      (char-code input)))
  (values))

;;; -------------------------------------------------------

;; D4
(define-node-dispatch :square (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :coordinate)))
    (apply-unary-operator :square
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :coordinate))))
  (values))

;;; -------------------------------------------------------

;; D6
(define-node-dispatch :store-string (interpreter node)
  (format T "~&Please input a string: ")
  (let ((input (read-line)))
    (declare (type string input))
    (clear-input)
    (setf
      (interpreter-cell-at interpreter
        (interpreter-visit-node interpreter
          (node-attribute node :coordinate)))
      (get-ascii-code-for-string input)))
  (values))

;;; -------------------------------------------------------

;; D7
(define-node-dispatch :suppress-next-newline (interpreter node)
  (setf (slot-value interpreter 'suppress-newline-p) T)
  (values))

;;; -------------------------------------------------------

;; E1
(define-node-dispatch :add (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand)))
    (apply-binary-operator :add
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :left-operand))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :right-operand))))
  (values))

;;; -------------------------------------------------------

;; E2
(define-node-dispatch :set-to-zero (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :coordinate)))
    0)
  (values))

;;; -------------------------------------------------------

;; E3
(define-node-dispatch :floor (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand)))
    (apply-binary-operator :floor
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :left-operand))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :right-operand))))
  (values))

;;; -------------------------------------------------------

;; E4
(define-node-dispatch :set-to-infinity (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter :coordinate))
    :infinity)
  (values))

;;; -------------------------------------------------------

;; E7
(define-node-dispatch :export-output (interpreter node)
  (with-slots (export-file output-buffer) interpreter
    (declare (type pathname-designator export-file))
    (declare (type string              output-buffer))
    (with-open-file (export-file-stream export-file
                     :direction         :output
                     :element-type      'character
                     :if-exists         :append
                     :if-does-not-exist :create)
      (declare (type file-stream export-file-stream))
      (format export-file-stream "~a" output-buffer)))
  (values))

;;; -------------------------------------------------------

;; F1
(define-node-dispatch :subtract (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand)))
    (apply-binary-operator :subtract
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :left-operand))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :right-operand))))
  (values))

;;; -------------------------------------------------------

;; F2
(define-node-dispatch :multiply (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :left-operand)))
    (apply-binary-operator :multiply
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :left-operand))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :right-operand))))
  (values))

;;; -------------------------------------------------------

;; F4
(define-node-dispatch :open-calculator (interpreter node)
  (calculator-start (make-calculator))
  (values))

;;; -------------------------------------------------------

;; F5
(define-node-dispatch :supply-random-number (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :minimum)))
    (get-random-number-in-range
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :minimum))
      (interpreter-resolve-coordinate interpreter
        (node-attribute node :maximum))))
  (values))

;;; -------------------------------------------------------

;; F6
(define-node-dispatch :delete-program (interpreter node)
  (setf (slot-value interpreter 'shall-delete-program-p) T)
  (values))

;;; -------------------------------------------------------

;; F7
(define-node-dispatch :wait-random-interval (interpreter node)
  (sleep
    (get-random-number-in-range
      (slot-value interpreter 'minimum-delay)
      (slot-value interpreter 'maximum-delay)))
  (values))

;;; -------------------------------------------------------

;; G1
(define-node-dispatch :reset-memory (interpreter node)
  (memory-reset
    (slot-value interpreter 'memory))
  (values))

;;; -------------------------------------------------------

;; G2
(define-node-dispatch :copy-value (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (interpreter-visit-node interpreter
        (node-attribute node :target)))
    (interpreter-resolve-coordinate interpreter
      (node-attribute node :source)))
  (values))

;;; -------------------------------------------------------

;; G3
(define-node-dispatch :repeat-thrice (interpreter node)
  (let ((command-to-repeat (node-attribute node :command)))
    (declare (type Node command-to-repeat))
    (loop repeat 3 do
      (interpreter-visit-node interpreter command-to-repeat)))
  (values))

;;; -------------------------------------------------------

;; G4
(define-node-dispatch :output-memory-location (interpreter node)
  (let ((coordinate
         (memory-find-value
           (slot-value interpreter 'memory)
           (interpreter-visit-node interpreter
             (node-attribute node :value)))))
    (declare (type (or null string) coordinate))
    (when coordinate
      (interpreter-print interpreter " ~a " coordinate)))
  (values))

;;; -------------------------------------------------------

;; G5
(define-node-dispatch :execute-haphazardly (interpreter node)
  (when (shall-execute-command-p)
    (interpreter-visit-node interpreter
      (node-attribute node :command)))
  (values))

;;; -------------------------------------------------------

;; G6
(define-node-dispatch :return-interpreter-stats (interpreter node)
  (interpreter-print interpreter
    "Interpreter stats:~
     ~&Start time: ~a~
     ~&Run duration in seconds: ~d"
    (format-date-and-time (slot-value interpreter 'startup-time))
    (interpreter-get-run-duration interpreter))
  (values))

;;; -------------------------------------------------------

;; G7
(define-node-dispatch :set-to-decimal-number (interpreter node)
  (setf
    (interpreter-cell-at interpreter
      (node-attribute
        (node-attribute node :coordinate)
        :value))
    (interpreter-visit-node interpreter
      (node-attribute node :value)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) stored in the INTERPRETER
   and returns the last evaluated statement's result."
  (declare (type Interpreter interpreter))
  (the T
    (handler-case
      (progn
        (interpreter-start-timer interpreter)
        (interpreter-visit-node interpreter
          (slot-value interpreter 'tree))
        (interpreter-terminate-program interpreter))
      (Program-Halt-Interrupt ()
        (values)))))

;;; -------------------------------------------------------

(defun interpret-Tablebase (code)
  "Interprets the piece of Tablebase CODE and returns the last
   evaluated statement's result."
  (declare (type string code))
  (the T
    (interpreter-interpret
      (make-interpreter
        (parser-parse
          (make-parser
            (make-lexer code)))))))

;;; -------------------------------------------------------

(defun read-file-content (file)
  "Consumes the FILE's content and returns a string representation
   thereof."
  (declare (type pathname-designator file))
  (with-open-file (input file
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input))
    (let ((buffer
            (make-array (file-length input)
              :element-type 'character
              :adjustable   NIL
              :fill-pointer NIL)))
      (declare (type string buffer))
      (read-sequence buffer input)
      (the simple-string buffer))))

;;; -------------------------------------------------------

(defun load-Tablebase-file (file)
  "Loads a Tablebase program from the FILE, interprets it, and returns
   no value."
  (declare (type pathname-designator file))
  (the T
    (interpreter-interpret
      (make-interpreter
        (parser-parse
          (make-parser
            (make-lexer (read-file-content file))))
        :input-file file))))
