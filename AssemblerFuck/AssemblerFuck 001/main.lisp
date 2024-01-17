;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "AssemblerFuck", invented by the Esolang user "Sesshomariu"
;; and presented on April 15th, 2016, its conception begotten from a
;; reformulation of the "brainfuck" programming language from
;; Urban Mueller's efforts in a form suggestive of an assembly language.
;; 
;; 
;; Concept
;; =======
;; The AssemblerFuck programming language is founded upon brainfuck,
;; its conformation thus transmogrified as to subject the syntaxis and
;; treatment to an assembly language's mimicry.
;; 
;; == ASSEMBLERFUCK: BRAINFUCK AS AN ASSEMBLY LANGUAGE ==
;; AssemblerFuck applies to brainfuck a design conceived as an assembly
;; language's simulacrum, with each instruction commorant on a line of
;; its own, the identifier of the same being a prevenient token ere the
;; one or two operands' involvement.
;; 
;; 
;; Instructions
;; ============
;; AssemblerFuck's instruction set enumerates a quaruple cardinality,
;; by which, to a brainfuck doyen's amazement, the original membership's
;; tally has been numerically cleft atwain, its perimeter, however, not
;; inflicted with the potentials' curtailment, as the "MOV" command's
;; feelefold utility compensates the descendent to a status of
;; operative equipollence.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a foundational nortelry anenst
;; the language's operational competences.
;; 
;; Please heed that the the succedaneum segments in the treatise are
;; underlined via a catena of asterisks ("*"), their presence shall be
;; superseded by actual code in the ultimate AssemblerFuck program.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   ADD operand    | Increments the current cell value by the
;;       *******    | {operand}.
;;                  | If the new value exceeds the upper bourne of 255,
;;                  | the state wraps around along the same.
;;                  |--------------------------------------------------
;;                  | The {operand} must be a non-negative integer
;;                  | literal.
;;   ..................................................................
;;   SUB operand    | Decrements the current cell value by the
;;       *******    | {operand}.
;;                  | If the new value transcends the lower bourne of
;;                  | zero (0), the state wraps around along the same.
;;                  |--------------------------------------------------
;;                  | The {operand} must be a non-negative integer
;;                  | literal.
;;   ..................................................................
;;   MOV targ, obj  | Moves the object {obj} to the target {targ}.
;;       ****  ***  |--------------------------------------------------
;;                  | Valid forms for the target {targ} constitute:
;;                  | 
;;                  |   ----------------------------------------------
;;                  |   Target | Effect
;;                  |   -------+--------------------------------------
;;                  |   P      | Store in cell at cell pointer.
;;                  |   ..............................................
;;                  |   LEFT   | Move to cell immediately to the left
;;                  |          | of current cell.
;;                  |   ..............................................
;;                  |   RIGHT  | Move to cell immediately to the right
;;                  |          | of current cell.
;;                  |   ..............................................
;;                  |   OUT    | Move to output conduit.
;;                  |   ----------------------------------------------
;;                  |--------------------------------------------------
;;                  | Valid forms for the object {obj} constitute:
;;                  | 
;;                  |   ----------------------------------------------
;;                  |   Object | Effect
;;                  |   -------+--------------------------------------
;;                  |   P      | Value in cell at pointer.
;;                  |   ..............................................
;;                  |   IN     | Standard input character.
;;                  |   ----------------------------------------------
;;                  |--------------------------------------------------
;;                  | For a more comprehensive treatise upon the "MOV"
;;                  | target and object combinations, please consult
;;                  | the subsection "MOVE TARGETS AND OBJECTS" alow.
;;   ..................................................................
;;   UNTIL 0        | Repeatedly executes the {statements} until the
;;     statements   | current cell value equals zero (0).
;;     **********   |--------------------------------------------------
;;   END            | The {statements} must be a sequence of zero or
;;                  | more statements.
;;   ------------------------------------------------------------------
;; 
;; == MOVE TARGET AND OBJECTS ==
;; Begotten from intercourse betwixt the target and the object, the
;; "MOV" instruction satisfies several responsibilities whose dedication
;; in brainfuck would distribute across separate facilities. The
;; following tabular illustration shall be the claviger to the valid
;; combinations' communication:
;; 
;;   ------------------------------------------------------------------
;;   Target | Object | Effect
;;   -------+--------+-------------------------------------------------
;;   P      | IN     | Move input to current cell.
;;   ..................................................................
;;   OUT    | IN     | Move input to output.
;;   ..................................................................
;;   OUT    | P      | Move current cell value to output.
;;   ..................................................................
;;   LEFT   | P      | Move cell pointer left.
;;   ..................................................................
;;   RIGHT  | P      | Move cell pointer right.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-17
;; 
;; Sources:
;;   [esolang2023AssemblerFuck]
;;   The Esolang contributors, "AssemblerFuck", May 4th, 2023
;;   URL: "https://esolangs.org/wiki/AssemblerFuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type employing the ``deftype'' infrastructure with
   its ``satisfies'' predicate, its agnomination desumed from the
   TYPE-NAME, and its formal parameters that prescribes by the
   LAMBDA-LIST, the object to be fathomed being denoted by the
   CANDIDATE-VARIABLE and retaining accessibility to the BODY forms, the
   same provide the predicate implementation, with the desinent
   evaluated BODY form's primary result either avering the candidate's
   eligibility, by responding with a generalized boolean value other
   than ``NIL'', otherwise dismissing its covenableness via a ``NIL''
   output.
   ---
   This macro's causatum is tantamount to the following forbisen:
     (deftype TYPE-NAME (LAMBDA-LIST)
       (let ((predicate-name (gensym)))
         (declare (type symbol predicate-name))
         (setf (symbol-function predicate-name)
           #'(lambda (CANDIDATE-VARIABLE)
               (declare (type T    CANDIDATE-VARIABLE))
               (declare (ignorable CANDIDATE-VARIABLE))
               BODY))
         `(satisfies ,predicate-name)))"
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          (format NIL "The type ~s." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-name)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among these conforms to the KEY-TYPE and
   affiliates with a value whose fidelity commits it to the VALUE-TYPE,
   the both defaulting to the generic sentinel ``*''."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of AssemblerFuck
   keywords to representative tokens, realized as a hash table, the keys
   of which establish identifier names, while the values furnish the
   tokens."
  '(hash-table-of string Token))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   that adhere to the ELEMENT-TYPE, defaulting to the generic sentinel
   ``*''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   abstract syntax tree (AST) nodes, each represented by the
   ``AST-Node'' interface."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype move-object ()
  "The ``move-object'' type enumerates the recognized variants on
   objects for the AssemblerFuck \"MOV\" instruction."
  '(member :pointer :input))

;;; -------------------------------------------------------

(deftype move-target ()
  "The ``move-target'' type enumerates the recognized variants on
   targets for the AssemblerFuck \"MOV\" instruction."
  '(member :pointer :left :right :output))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), that is, an occupant of the integral
   range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value as an integral
   object compact of eight accolent bits, its woning the closed interval
   spanned by [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse vector of unsigned byte-valued
   cells by adminiculum of a hash table, the keys of which comprehend
   the signed integer keys, while the values lend a salvatory to a
   scalar ``octet'' each."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   same embraces in its compass, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class serves in the representation of a significant
   object extracted during a piece of AssemblerFuck's lexical
   analyzation, its diorism edified upon the twissel of a categorizing
   type and a descriptive value."
  (type  (error "Missing type.")  :type keyword :read-only T)
  (value (error "Missing value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized AssemblerFuck identifiers with their
   token representations.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token)
        "Associates the identifier NAME with the TOKEN in the
         +IDENTIFIERS+ table and returns no value."
        (declare (type string name))
        (declare (type Token  token))
        (setf (gethash name +IDENTIFIERS+) token)
        (values)))
  (register-identifier "ADD"   (make-token :add   "ADD"))
  (register-identifier "END"   (make-token :end   "END"))
  (register-identifier "IN"    (make-token :in    "IN"))
  (register-identifier "LEFT"  (make-token :left  "LEFT"))
  (register-identifier "OUT"   (make-token :out   "OUT"))
  (register-identifier "MOV"   (make-token :mov   "MOV"))
  (register-identifier "P"     (make-token :p     "P"))
  (register-identifier "RIGHT" (make-token :right "RIGHT"))
  (register-identifier "SUB"   (make-token :sub   "SUB"))
  (register-identifier "UNTIL" (make-token :until "UNTIL"))
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns for the IDENTIFIER a conable token derived from an
   AssemblerFuck keyword, or, if none such can be located, signals an
   error of an unspecified type."
  (declare (type string identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (error "No valid identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab))))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a constituent admissive
   to identifiers, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alpha-char-p candidate)))))



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
  "The ``Lexer'' class furnishes an entity capacitated to recognize and
   extract the significant constitutents of a piece of AssemblerFuck
   source code in the form of tokens."
  (source    (error "Missing source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the character at the LEXER's current position, while
   concomitantly advancing the position cursor to the next character in
   its source, if possible."
  (declare (type Lexer lexer))
  (symbol-macrolet
      ((source    (the string              (lexer-source    lexer)))
       (position  (the fixnum              (lexer-position  lexer)))
       (character (the (or null character) (lexer-character lexer))))
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the (or null character)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source
              (incf position))))))))

;;; -------------------------------------------------------

(defun skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent spaces and horizontal tabs, and
   returns no value."
  (declare (type Lexer lexer))
  (symbol-macrolet ((character
                      (the (or null character)
                        (lexer-character lexer))))
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (advance-lexer lexer)))
  (values))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Produces and returns from the LEXER's current character a new token
   whose type amounts to the TOKEN-TYPE and whose value assumes the
   consumed character, and advances its position cursor to the next
   location in its source."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (symbol-macrolet ((character
                      (the (or null character)
                        (lexer-character lexer))))
    (declare (type (or null character) character))
    (the Token
      (prog1
        (make-token token-type character)
        (advance-lexer lexer)))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier and returns a token representation of the referenced
   AssemblerFuck language keyword."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier-token
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (symbol-macrolet ((character
                            (the (or null character)
                              (lexer-character lexer))))
          (declare (type (or null character) character))
          (loop
            while (and character (identifier-character-p character))
            do    (write-char (advance-lexer lexer) identifier)))))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned, non-negative integer number, and returns an ``:integer''
   token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :integer
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (symbol-macrolet ((character
                              (the (or null character)
                                (lexer-character lexer))))
            (declare (type (or null character) character))
            (loop while (and character (digit-char-p character)) do
              (write-char (advance-lexer lexer) digits))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (symbol-macrolet
      ((position  (the fixnum              (lexer-position lexer)))
       (character (the (or null character) (lexer-character lexer))))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (skip-spaces    lexer)
          (get-next-token lexer))
        
        ((char= character #\Newline)
          (read-symbol lexer :newline))
        
        ((char= character #\,)
          (read-symbol lexer :comma))
        
        ((identifier-character-p character)
          (read-identifier lexer))
        
        ((digit-char-p character)
          (read-number lexer))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' interface establishes the foundry for all classes
   pursuing the representation of AssemblerFuck facilities in the
   form of abstract syntax tree (AST) nodes.")

;;; -------------------------------------------------------

(defmacro define-node-class (class-name (&optional super-class)
                             &rest slot-specifications)
  "Defines a structure utilizing the ``defstruct'' infrastructure, the
   same is stevened by the CLASS-NAME, inheriting from the SUPER-CLASS,
   in response to its omission, from ``AST-Node'', expanding the
   foundation by slots specified via the SLOT-SPECIFICATIONS, each such
   a two-element list of a symbolic slot name and a type specifier,
   automatically assigned to be mandatory during the construction, and
   designated as a read-only field."
  `(defstruct (,class-name
     (:include ,(or super-class 'AST-Node)))
     ,(if (stringp (first slot-specifications))
        (pop slot-specifications)
        (format NIL "Implements the class ``~a''." class-name))
     ,@(loop
         for (slot-name slot-type)
           of-type (symbol symbol)
           in      slot-specifications
         collect
           `(,slot-name
              (error "Missing value for slot ~a."
                (quote ,slot-name))
              :type      ,slot-type
              :read-only T))))

;;; -------------------------------------------------------

(define-node-class Integer-Node ()
  "The ``Integer-Node'' class serves to encapsulate an unsigned integer
   object in an abstract syntax tree (AST) node."
  (value non-negative-integer))

;;; -------------------------------------------------------

(define-node-class Target-Node ()
  "The ``Target-Node'' class serves to encapsulate a \"MOV\" instruction
   target."
  (target move-target))

;;; -------------------------------------------------------

(define-node-class Object-Node ()
  "The ``Target-Node'' class serves to encapsulate a \"MOV\" instruction
   object."
  (object move-object))

;;; -------------------------------------------------------

(define-node-class Add-Node ()
  "The ``Add-Node'' class serves to encapsulate an \"ADD\" instruction
   in conjunction with its operand."
  (value Integer-Node))

;;; -------------------------------------------------------

(define-node-class Sub-Node ()
  "The ``Sub-Node'' class serves to encapsulate a \"SUB\" instruction in
   conjunction with its operand."
  (value Integer-Node))

;;; -------------------------------------------------------

(define-node-class Mov-Node ()
  "The ``Mov-Node'' class serves to encapsulate a \"MOV\" instruction in
   conjunction with its target and object."
  (target Target-Node)
  (object Object-Node))

;;; -------------------------------------------------------

(define-node-class Block-Node ()
  "The ``Block-Node'' class serves to encapsulate a sequence of zero or
   more accolent AssemblerFuck statement in an abstract syntax tree
   (AST) node form."
  (statements node-list))

;;; -------------------------------------------------------

(define-node-class Until-Node ()
  "The ``Set-Node'' class serves to encapsulate an \"UNTIL\" instruction
   in conjunction with its statement block."
  (statements Block-Node))

;;; -------------------------------------------------------

(define-node-class Program-Node ()
  "The ``Program-Node'' class serves to encapsulate an AsseblerFuck++
   program in conjunction with its statements."
  (statements Block-Node))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (lexer
                             &aux (current-token
                                    (get-next-token lexer)))))
  "The ``Parser'' class is assigned the dever of assembling an
   AssemblerFuck program from a sequence of tokens."
  (lexer         (error "Missing lexer.")
                 :type      Lexer
                 :read-only T)
  (current-token (make-token :eof NIL)
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) Block-Node) parse-statement-block))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the same, while
   concomitantly querying and storing the same underlying lexer;
   otherwise an error of an unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (symbol-macrolet
      ((lexer         (the Lexer (parser-lexer         parser)))
       (current-token (the Token (parser-current-token parser))))
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (the Token
      (if (token-of-type-p current-token expected-token-type)
        (prog1 current-token
          (setf current-token
            (get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun eat-current-token (parser)
  "Returns the PARSER's current token, while concomitantly querying and
   storing the same underlying lexer."
  (declare (type Parser parser))
  (symbol-macrolet
      ((lexer         (the Lexer (parser-lexer         parser)))
       (current-token (the Token (parser-current-token parser))))
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (the Token
      (prog1 current-token
        (setf current-token
          (get-next-token lexer))))))

;;; -------------------------------------------------------

(defun skip-optional-newlines (parser)
  "Proceeding from the PARSER's current token, skips a catena of zero or
   more accolent newline tokens and returns no value"
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (loop while (token-of-type-p current-token :newline) do
      (eat-token parser :newline)))
  (values))

;;; -------------------------------------------------------

(defun eat-newlines (parser)
  "Expects the PARSER's subsequent tokens, proceeding from its current
   instance, to establish a catena of one or more newline tokens, on
   confirmation consuming these and returning no value; otherwise an
   error of an unspecified type is signaled."
  (declare (type Parser parser))
  (eat-token              parser :newline)
  (skip-optional-newlines parser)
  (values))

;;; -------------------------------------------------------

(defun expect-end-of-statement (parser)
  "Determines whether the PARSER's current token ostends a covenableness
   with a statement terminator, which incorporates in its diorism either
   a newline or an end-of-file (``:eof'') entity, in the former case the
   same, including all accolent peers, are skipped, in any case no value
   is returned, while a failure to subsume into this category
   instigates an error of an unspecifed type."
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (case (token-type current-token)
      (:eof      NIL)
      (:newline  (eat-newlines parser))
      (otherwise (error "Expected a newline or end of file, ~
                         but encountered the token ~s."
                   current-token))))
  (values))

;;; -------------------------------------------------------

(defun parse-value (parser)
  "Parses an integer number in the PARSER's context and returns an
   ``Integer-Node'' representation thereof."
  (declare (type Parser parser))
  (the Integer-Node
    (make-integer-node :value
      (token-value
        (eat-token parser :integer)))))

;;; -------------------------------------------------------

(defun parse-object (parser)
  "Parses an object in the PARSER's context and returns an
   ``Object-Node'' encapsulation thereof."
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (the Object-Node
      (prog1
        (case (token-type current-token)
          (:p        (make-object-node :object :pointer))
          (:in       (make-object-node :object :input))
          (otherwise (error "Invalid object token: ~s."
                       current-token)))
        (eat-current-token parser)))))

;;; -------------------------------------------------------

(defun parse-target (parser)
  "Parses a target in the PARSER's context and returns a ``Target-Node''
   encapsulation thereof."
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (the Target-Node
      (prog1
        (case (token-type current-token)
          (:p        (make-target-node :target :pointer))
          (:left     (make-target-node :target :left))
          (:right    (make-target-node :target :right))
          (:out      (make-target-node :target :output))
          (otherwise (error "Invalid target token: ~s."
                       current-token)))
        (eat-current-token parser)))))

;;; -------------------------------------------------------

(defun parse-statement (parser)
  "Parses an AssemblerFuck statement in the PARSER's context and
   returns an ``AST-Node'' encapsulation thereof."
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (the AST-Node
      (prog1
        (case (token-type current-token)
          (:add
            (eat-token parser :add)
            (make-add-node :value
              (parse-value parser)))
          
          (:sub
            (eat-token parser :sub)
            (make-sub-node :value
              (parse-value parser)))
          
          (:mov
            (eat-token parser :mov)
            (let ((target (parse-target parser)))
              (declare (type Target-Node target))
              (eat-token parser :comma)
              (let ((object (parse-object parser)))
                (declare (type Object-Node object))
                (the Mov-Node
                  (make-mov-node :target target :object object)))))
          
          (:until
            (eat-token parser :until)
            (let ((guard (eat-token parser :integer)))
              (declare (type Token guard))
              (unless (zerop (token-value guard))
                (error "Expected an zero-valued integer token, ~
                        but encountered ~s."
                  guard))
              (eat-newlines parser)
              (let ((statements (parse-statement-block parser)))
                (declare (type Block-Node statements))
                (eat-token parser :end)
                (make-until-node :statements statements))))
          
          (otherwise
            (error "Invalid statement token: ~s." current-token)))
        
        (expect-end-of-statement parser)))))

;;; -------------------------------------------------------

(defun parse-statement-block (parser)
  "Parses a catena of zero or more statements in the PARSER's context,
   each twissel's sepiment realized by one or more newline tokens, and
   returns a ``Block-Node'' encapsulation thereof."
  (declare (type Parser parser))
  (symbol-macrolet ((current-token
                      (the Token
                        (parser-current-token parser))))
    (declare (type Token current-token))
    (the Block-Node
      (make-block-node :statements
        (loop
          initially
            (skip-optional-newlines parser)
          until
            (or (token-of-type-p current-token :eof)
                (token-of-type-p current-token :end))
          collect
            (parse-statement parser))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses an AssemblerFuck program in the PARSER's context and returns
   a ``Program-Node'' encapsulation thereof."
  (declare (type Parser parser))
  (the Program-Node
    (prog1
      (make-program-node :statements
        (parse-statement-block parser))
      (eat-token parser :eof))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class defines the AssemblerFuck memory model in
   terms of a bilaterally infinite tape composed of unsigned byte-valued
   cells, operated upon by a mobile cell pointer, its dever delineated
   by the selection of the currently active cell."
  (cells   (make-hash-table :test #'eql)
           :type      cell-map
           :read-only T)
  (pointer 0
           :type      integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (symbol-macrolet
      ((cells   (the cell-map (memory-cells   memory)))
       (pointer (the integer  (memory-pointer memory))))
    (declare (type cell-map cells))
    (declare (type integer  pointer))
    (the octet
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, upon necessity
   wrapping its value around in order to accommodate the valid unsigned
   byte bournes of [0, 255], and returns no value."
  (declare (type Memory memory))
  (symbol-macrolet
      ((cells   (the cell-map (memory-cells   memory)))
       (pointer (the integer  (memory-pointer memory))))
    (declare (type cell-map cells))
    (declare (type integer  pointer))
    (setf (gethash pointer cells 0)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun cell-pointer-position (memory)
  "Returns the MEMORY's cell pointer location."
  (declare (type Memory))
  (the integer
    (memory-pointer memory)))

;;; -------------------------------------------------------

(defun (setf cell-pointer-position) (new-position memory)
  "Relocates the MEMORY's cell pointer to the NEW-POSITION and returns
   no value."
  (declare (type Memory))
  (setf (memory-pointer memory) new-position)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (program)))
  "The ``Interpreter'' class is allotted the dever of embuing an
   AssemblerFuck program, committed in the form of an abstract syntax
   tree (AST), with actual effect."
  (program (error "Missing program.")
           :type      Program-Node
           :read-only T)
  (memory  (make-memory)
           :type      Memory
           :read-only T))

;;; -------------------------------------------------------

(defun get-memory (interpreter)
  "Returns the program memory governed by the INTERPRETER's castaldy."
  (declare (type Interpreter interpreter))
  (the Memory
    (interpreter-memory interpreter)))

;;; -------------------------------------------------------

(defgeneric move (interpreter target object)
  (:documentation
    "Applies the AssemblerFuck \"MOV\" instruction to the TARGET and the
     OBJECT, deployed in the INTERPRETER's context, and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-move-implementation ((target-specializer-type
                                       object-specializer-type)
                                      &body body)
  "Provides a convenient facility for the implementations of the generic
   function ``move'' by nevening the first argument as the
   ``interpreter'' and specializing on the ``Interpreter'' class,
   designating the second argument as the ``target'' and specializing on
   the TARGET-SPECIALIZER-TYPE, and the third argument as the
   ``object'', whose specialization is imposed by the
   OBJECT-SPECIALIZER-TYPE, executing the BODY forms, and returning no
   value.
   ---
   A purlicue shall be behoovable for the administration of a
   facilitated ilk of nortelry concerning the bindings by their names
   and types in thus assembled argument list:
     --------------------------------------------
     Binding name | Binding type
     -------------+------------------------------
     interpreter  | Interpreter
     ............................................
     target       | (eql TARGET-SPECIALIZER-TYPE)
     ............................................
     object       | (eql OBJECT-SPECIALIZER-TYPE)
     --------------------------------------------"
  `(defmethod move ((interpreter Interpreter)
                    (target      (eql ,target-specializer-type))
                    (object      (eql ,object-specializer-type)))
     (declare (type Interpreter interpreter))
     (declare (ignorable        interpreter))
     (declare (type move-target target))
     (declare (ignorable        target))
     (declare (type move-object object))
     (declare (ignorable        object))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-move-implementation (:pointer :input)
  "Queries the standard input for a character and stores its ASCII code
   in the current cell.
   ---
   This operation implements the AssemblerFuck instruction
     MOV P, IN"
  (format T "~&>> ")
  (finish-output)
  (setf
    (current-cell-value
      (get-memory interpreter))
    (char-code
      (read-char)))
  (clear-input))

;;; -------------------------------------------------------

(define-move-implementation (:output :pointer)
  "Prints the character whose ASCII code responds to the current cell
   value to the standard output.
   ---
   This operation implements the AssemblerFuck instruction
     MOV OUT, P"
  (write-char
    (code-char
      (current-cell-value
        (get-memory interpreter)))))

;;; -------------------------------------------------------

(define-move-implementation (:output :input)
  "Queries the standard input and directly outputs the same ipsissima
   verba without its storage in the current cell.
   ---
   This operation implements the AssemblerFuck instruction
     MOV OUT, IN"
  (format T "~&>> ")
  (finish-output)
  (write-char
    (read-char))
  (clear-input))

;;; -------------------------------------------------------

(define-move-implementation (:left :pointer)
  "Translates the cell pointer one step to the left.
   ---
   This operation implements the AssemblerFuck instruction
     MOV LEFT, P"
  (decf
    (cell-pointer-position
      (get-memory interpreter))))

;;; -------------------------------------------------------

(define-move-implementation (:right :pointer)
  "Translates the cell pointer one step to the right.
   ---
   This operation implements the AssemblerFuck instruction
     MOV RIGHT, P"
  (incf
    (cell-pointer-position
      (get-memory interpreter))))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Processes the abstract syntax tree (AST) node in the INTERPRETER's
     context and return a value connable to this champarty."))

;;; -------------------------------------------------------

(defmacro define-node-visit ((node-class) &body body)
  "Furnishes a commodity for the implementation of the ``visit-node''
   generic function, the first argument of which is denoted by the name
   ``interpreter'' and specialized on the cognominal ``Interpreter''
   class, the second being fixated as ``node'', deriving its specializer
   from the NODE-CLASS, evaluates the BODY form, and returns the
   desinent form's results."
  `(defmethod visit-node ((interpreter Interpreter)
                          (node        ,node-class))
     (declare (type Interpreter interpreter))
     (declare (ignorable        interpreter))
     (declare (type ,node-class node))
     (declare (ignorable        node))
     ,@body))

;;; -------------------------------------------------------

(define-node-visit (Program-Node)
  (visit-node interpreter
    (program-node-statements node))
  (values))

;;; -------------------------------------------------------

(define-node-visit (Block-Node)
  (dolist (statement (block-node-statements node))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(define-node-visit (Until-Node)
  (loop until (zerop (current-cell-value (get-memory interpreter))) do
    (visit-node interpreter
      (until-node-statements node)))
  (values))

;;; -------------------------------------------------------

(define-node-visit (Integer-Node)
  (the non-negative-integer
    (integer-node-value node)))

;;; -------------------------------------------------------

(define-node-visit (Target-Node)
  (the move-target
    (target-node-target node)))

;;; -------------------------------------------------------

(define-node-visit (Object-Node)
  (the move-object
    (object-node-object node)))

;;; -------------------------------------------------------

(define-node-visit (Mov-Node)
  (move interpreter
    (visit-node interpreter
      (mov-node-target node))
    (visit-node interpreter
      (mov-node-object node)))
  (values))

;;; -------------------------------------------------------

(define-node-visit (Add-Node)
  (incf
    (current-cell-value
      (get-memory interpreter))
    (visit-node interpreter
      (add-node-value node)))
  (values))

;;; -------------------------------------------------------

(define-node-visit (Sub-Node)
  (decf
    (current-cell-value
      (get-memory interpreter))
    (visit-node interpreter
      (sub-node-value node)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the AssemblerFuck program maintained by the INTERPRETER
   and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (interpreter-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-AssemblerFuck (code)
  "Interprets the piece of AssemblerFuck source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-AssemblerFuck converter.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (candidate)
  "Determines whether the CANDIDATE represents a brainfuck command
   token, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-<>,.[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun find-next-brainfuck-command (brainfuck-code start)
  "Proceeding from the START position into the BRAINFUCK-CODE, returns
   the position of the first brainfuck command character, or, if none
   such could be detected, the BRAINFUCK-CODE string's length."
  (declare (type string brainfuck-code))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'brainfuck-command-p brainfuck-code :start start)
        (length brainfuck-code))))

;;; -------------------------------------------------------

(defun count-catena-length (brainfuck-code start subject)
  "Proceeding from the START position into the BRAINFUCK-CODE,
   tallies the number of consecutive occurrences of the SUBJECT and
   returns two values:
     (1) The number of accolent SUBJECT instances.
     (2) The position into the BRAINFUCK-CODE immediately succeeding
         the matching region."
  (declare (type string    brainfuck-code))
  (declare (type fixnum    start))
  (declare (type character subject))
  (the (values non-negative-integer fixnum)
    (loop
      for position
        of-type fixnum
        from    start
        below   (length brainfuck-code)
      while (char= (char brainfuck-code position) subject)
        count 1
        into  number-of-occurrences
      finally
        (return
          (values number-of-occurrences position)))))

;;; -------------------------------------------------------

(defun translate-brainfuck-to-AssemblerFuck (brainfuck-code
                                               &key (destination NIL))
  "Generates for the BRAINFUCK-CODE an equivalent AssemblerFuck
   program, writes it to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((position    0)
            (indentation 0))
        (declare (type fixnum  position))
        (declare (type integer indentation))
        (labels
            ((advance-to-next-command ()
              "Moves the POSITION cursor the next command in the
               BRAINFUCK-CODE and returns no value."
              (setf position
                (find-next-brainfuck-command brainfuck-code position))
              (values))
             
             (count-repetitions (command)
              "Proceeding from the current POSITION, counts the
               repetitions of the COMMAND in immediate succession,
               relocates the POSITION cursor to the first character
               succeeding the matching portion, and returns the
               supputated tally."
              (declare (type character command))
              (multiple-value-bind (repetitions new-position)
                  (count-catena-length brainfuck-code position command)
                (declare (type non-negative-integer repetitions))
                (declare (type fixnum               new-position))
                (setf position new-position)
                (the non-negative-integer repetitions)))
             
             (write-command (control-string &rest format-arguments)
              "Formats the CONTROL-STRING using the FORMAT-ARGUMENTS,
               prints these, preceded by the current INDENTATION, to the
               DESTINATION, and returns no value."
              (declare (type string      control-string))
              (declare (type (list-of T) format-arguments))
              (format destination "~&~vt~?" indentation control-string
                format-arguments)
              (values)))
          
          (advance-to-next-command)
          
          (loop while (< position (length brainfuck-code)) do
            (case (char brainfuck-code position)
              (#\+
                (write-command "ADD ~d"
                  (count-repetitions #\+)))
              (#\-
                (write-command "SUB ~d"
                  (count-repetitions #\-)))
              
              (#\>
                (write-command "MOV RIGHT, P")
                (incf position))
              
              (#\<
                (write-command "MOV LEFT, P")
                (incf position))
              
              (#\,
                (write-command "MOV P, IN")
                (incf position))
              
              (#\.
                (write-command "MOV OUT, P")
                (incf position))
              
              (#\[
                (write-command "UNTIL 0")
                (incf indentation 2)
                (incf position))
              
              (#\]
                (write-command "END")
                (decf indentation 2)
                (incf position))
              
              (otherwise NIL))
            
            (advance-to-next-command))))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-brainfuck-to-AssemblerFuck brainfuck-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-AssemblerFuck
  "ADD 8
   UNTIL 0
     MOV RIGHT, P
     ADD 4
     UNTIL 0
       MOV RIGHT, P
       ADD 2
       MOV RIGHT, P
       ADD 3
       MOV RIGHT, P
       ADD 3
       MOV RIGHT, P
       ADD 1
       MOV LEFT, P
       MOV LEFT, P
       MOV LEFT, P
       MOV LEFT, P
       SUB 1
     END
     MOV RIGHT, P
     ADD 1
     MOV RIGHT, P
     ADD 1
     MOV RIGHT, P
     SUB 1
     MOV RIGHT, P
     MOV RIGHT, P
     ADD 1
     UNTIL 0
       MOV LEFT, P
     END
     MOV LEFT, P
     SUB 1
   END
   MOV RIGHT, P
   MOV RIGHT, P
   MOV OUT, P
   MOV RIGHT, P
   SUB 3
   MOV OUT, P
   ADD 7
   MOV OUT, P
   MOV OUT, P
   ADD 3
   MOV OUT, P
   MOV RIGHT, P
   MOV RIGHT, P
   MOV OUT, P
   MOV LEFT, P
   SUB 1
   MOV OUT, P
   MOV LEFT, P
   MOV OUT, P
   ADD 3
   MOV OUT, P
   SUB 6
   MOV OUT, P
   SUB 8
   MOV OUT, P
   MOV RIGHT, P
   MOV RIGHT, P
   ADD 1
   MOV OUT, P
   MOV RIGHT, P
   ADD 2
   MOV OUT, P")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a user input of the
;; "null character".
(interpret-AssemblerFuck
  "
  MOV P, IN
    UNTIL 0
      MOV OUT, P
      MOV P, IN
    END
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-AssemblerFuck
  "
  MOV P, IN
  MOV OUT, P
  UNTIL 0
    SUB 2
    MOV RIGHT, P
    ADD 1
    UNTIL 0
      MOV RIGHT, P
      MOV RIGHT, P
    END
    MOV LEFT, P
    UNTIL 0
      MOV OUT, P
    END
    MOV LEFT, P
    MOV LEFT, P
  END
  ")

;;; -------------------------------------------------------

;; Translate a "Hello World!" program from brainfuck to AssemblerFuck.
(translate-brainfuck-to-AssemblerFuck
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
