;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "HS", invented by the Esolang user "Benett0222" and
;; presented on June 28th, 2023, the concept of which revolves around
;; the preponderant encoding of the language facilities in hash signs,
;; "#", their tally serves as the claviger to their distinguishment.
;; 
;; 
;; Concept
;; =======
;; The HS programming language is based upon an encoding of a rather
;; potent set of facilities in hash sign ("#") sequences, the tallies of
;; which map in an unambiguous manner to established tokens. Besides the
;; "#" symbol, only the colon ":" and whitespaces are vested with
;; significance.
;; 
;; All data in HS registers its castaldy in the form of variables, also
;; expressed in hash sign blocks, and unbounded in their quantity.
;; 
;; 
;; Architecture
;; ============
;; HS's memory model abstains from veridical intricacies, employing
;; variables, expressed in hash sign sequences whose cardinalities
;; exceed the threshold of 21 constituents, and which are conceived in
;; a tacit notion as being enumerated with subscripts commencing at one
;; (1), and not impounded along the upper extremum.
;; 
;; Their nature as nominal accessors to unbounded integer values redes
;; an associative structure to this departmentment's commitment. The
;; absence of ordering issues attends to an inclination towards a hash
;; table, the nimble amenability in the general case suffices for the
;; requisitum.
;; 
;; 
;; Data Types
;; ==========
;; The type system commorant in HS bifurcates into a twain of species:
;; imprimis, the unbounded integer objects; secondarily, the ASCII
;; character repertoire.
;; 
;; 
;; Syntax
;; ======
;; In regards of its donet, an HS program's representation incorporates
;; merely three symbols: the paravaunt hash sign ("#"), the parhedral
;; colon (":"), and whitespaces as sepiments.
;; 
;; 
;; Instructions
;; ============
;; HS's instruction set comprises a perimeter of rather mediocre
;; mickleness, admitting a simple conditional construct, two species of
;; loops, as well as input and output facilities.
;; 
;; == THREE TOKENS INFLUENCE THE INSTRUCTIONS ==
;; A listing of the pertinent characters, whose bailiwick touches the
;; subject of operations, shall be adduced:
;; 
;;   ------------------------------------------------------------------
;;   Character           | Purpose
;;   --------------------+---------------------------------------------
;;   #                   | Encodes commands in the tally of repetitions
;;                       | involving the hash symbol "#".
;;   ..................................................................
;;   :                   | Queries for a user input integer.
;;   ..................................................................
;;   space, tab, newline | Separates tokens, especially distinct
;;                       | sequences of hash signs ("#"), while itself
;;                       | accompassing no operational utility.
;;   ------------------------------------------------------------------
;; 
;; == HASH SIGN BLOCK LENGTHS ENCODE TOKENS ==
;; The below table limns a vinculum betwixt the hash encoding and the
;; represented facilities:
;; 
;;   ------------------------------------------------------------------
;;   No. #  | Effect
;;   -------+----------------------------------------------------------
;;   1      | Encodes the "if" token: Commences a condition block
;;          | which is activated if the subsequent antecedent holds,
;;          | segregated from its body by a "then" ("##") token, and
;;          | and which extends to the matching "end if" ("###") token.
;;   ..................................................................
;;   2      | Encodes the "then" token, employed exclusively in the
;;          | context of the "if" ("#") conditional in order to
;;          | demarcate the boundary betwixt the antecedent and the
;;          | body statements.
;;   ..................................................................
;;   3      | Encodes the "end if" token, employed as a demarcation of
;;          | the "if" and "then" body's statements section.
;;   ..................................................................
;;   4      | Encodes the "loop times" token: Commences an iterative
;;          | construct which repeats its body statements a specified
;;          | number of times. The next token is expected to yield a
;;          | numeric value. The loop is concluded via a
;;          | "end loop times" ("#####") token.
;;   ..................................................................
;;   5      | Encodes the "end loop times" token, employed as a
;;          | terminating demarcation of the "loop times" token.
;;   ..................................................................
;;   6      | Encodes the "loop infinitely" token: Commences an
;;          | infinitely repeating loop whose body statements are
;;          | demarcated by a "end loop infinitely" ("#######") token.
;;   ..................................................................
;;   7      | Encodes the "end loop infinitely" token, employed as a
;;          | terminating demarcation of the "loop infinitely" token.
;;   ..................................................................
;;   8      | Encodes the ">", or "greater than" token, signifying a
;;          | binary relational operator for use in the "if" ("#")
;;          | construct.
;;   ..................................................................
;;   9      | Encodes the "=", or "equal to", token that bears a
;;          | twifaced nature:
;;          |   (a) Imprimis employed as the assignment operator,
;;          |       trailing the variable name to its left, and
;;          |       preceding the value to assign on its right.
;;          |   (b) In its second role providing the "equal to" binary
;;          |       relational operator, employed for conditional
;;          |       probing in the "if" ("#") construct.
;;   ..................................................................
;;   10     | Encodes the "<", or "less than" token, signifying a
;;          | binary relational operator for use in the "if" ("#")
;;          | construct.
;;   ..................................................................
;;   11--20 | Enumerates the decimal digits zero (0) through nine (9).
;;   ..................................................................
;;   21     | Encodes the "print" token, which is expected to be
;;          | succeeded by zero or more expressions to print.
;;   ..................................................................
;;   >= 22  | Enumerates the variables "var{x}", where {x} conveys the
;;          | subscripts starting from one (1).
;;   ------------------------------------------------------------------
;; 
;; A more detailed variation on the aboon explication's token
;; correspondences shall be provided for further reference:
;; 
;;   ------------------------------------------------------------------
;;   No. # | Decoded token       | Apostille
;;   ------+---------------------+-------------------------------------
;;   1     | if                  | Utilizes "then" and "end if".
;;   ..................................................................
;;   2     | then                | Used in "if" only.
;;   ..................................................................
;;   3     | end if              | Concludes "if".
;;   ..................................................................
;;   4     | loop times          | Terminated by "end loop times".
;;   ..................................................................
;;   5     | end loop times      | Terminates "loop times".
;;   ..................................................................
;;   6     | loop infinitely     | Terminated by "end loop infinitely".
;;   ..................................................................
;;   7     | end loop infinitely | Terminates "loop infinitely".
;;   ..................................................................
;;   8     | >                   | Operator "greater than".
;;   ..................................................................
;;   9     | =                   | Operator "equal to" or assignment.
;;   ..................................................................
;;   10    | <                   | Operator "less than".
;;   ..................................................................
;;   11    | 0                   | Digit zero.
;;   ..................................................................
;;   12    | 1                   | Digit one.
;;   ..................................................................
;;   13    | 2                   | Digit two.
;;   ..................................................................
;;   14    | 3                   | Digit three.
;;   ..................................................................
;;   15    | 4                   | Digit four.
;;   ..................................................................
;;   16    | 5                   | Digit five.
;;   ..................................................................
;;   17    | 6                   | Digit six.
;;   ..................................................................
;;   18    | 7                   | Digit seven.
;;   ..................................................................
;;   19    | 8                   | Digit eight.
;;   ..................................................................
;;   20    | 9                   | Digit nine.
;;   ..................................................................
;;   21    | print               | Followed by expressions.
;;   ..................................................................
;;   22    | var1                | Variable number one.
;;   ..................................................................
;;   23    | var2                | Variable number two.
;;   ..................................................................
;;   ...   | ...                 | ...
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in the programming language
;; Common Lisp, employing a treble of stages compact of lexical
;; analyzation, parsing, and interpretation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-05
;; 
;; Sources:
;;   [esolang2023HS]
;;   The Esolang contributors, "HS", June 30th, 2023
;;   URL: "https://esolangs.org/wiki/HS"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same by default assumes the comprehensive
   ``T''."
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
  "The ``hash-table-of'' type defines a hash table compacto of zero or
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

(deftype token-type ()
  "The ``token-type'' type enumerates the recognized variants of token
   families."
  '(member
    :if
    :then
    :end-if
    :loop-times
    :end-loop-times
    :loop-infinitely
    :end-loop-infinitely
    :greater-than
    :equal
    :less-than
    :number
    :print
    :variable
    :input
    :eof))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   ``Node'' instances."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype decimal-digit ()
  "The ``decimal-digit'' type defines a decimal digit as an integer
   number impounded in the range [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype variable-name ()
  "The ``variable-name'' type defines a variable identifier as an
   integral number along the positive axis of [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variations of
   relationship operators."
  '(member
    :less-than
    :less-than-or-equal-to
    :equal-to
    :greater-than
    :greater-than-or-equal-to))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates the information appertaining to a
   significant object recognized in and extracted from a piece of HS
   source code."
  (type  (error "Missing token type.")  :type token-type)
  (value (error "Missing token value.") :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token      token))
  (declare (type token-type expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))

;;; -------------------------------------------------------

(defun expression-token-p (candidate)
  "Determines whether the CANDIDATE represents a token which
   unambiguously introduces an expression, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (not (null
      (member (token-type candidate)
        '(:input :number) :test #'eq)))))

;;; -------------------------------------------------------

(defun statement-token-p (candidate)
  "Determines whether the CANDIDATE represents a token associated with
   an statement's introduction, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (not (null
      (member (token-type candidate)
        '(:if
          :input
          :loop-infinitely
          :loop-times
          :number
          :print
          :variable)
        :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-hash-signs (number-of-hash-signs)
  "Returns for the NUMBER-OF-HASH-SIGNS, that represents a sequence of
   \"#\" characters, a representative ``Token'' object."
  (declare (type (integer 1 *) number-of-hash-signs))
  (the Token
    (cond
      ((= number-of-hash-signs 1)
        (make-token :if "if"))
      
      ((= number-of-hash-signs 2)
        (make-token :then "then"))
      
      ((= number-of-hash-signs 3)
        (make-token :end-if "end if"))
      
      ((= number-of-hash-signs 4)
        (make-token :loop-times "loop"))
      
      ((= number-of-hash-signs 5)
        (make-token :end-loop-times "end loop"))
      
      ((= number-of-hash-signs 6)
        (make-token :loop-infinitely "loop infinitely"))
      
      ((= number-of-hash-signs 7)
        (make-token :end-loop-infinitely "end loop infinitely"))
      
      ((= number-of-hash-signs 8)
        (make-token :greater-than ">"))
      
      ((= number-of-hash-signs 9)
        (make-token :equal "="))
      
      ((= number-of-hash-signs 10)
        (make-token :less-than "<"))
      
      ((<= 11 number-of-hash-signs 20)
        (make-token :number
          (- number-of-hash-signs 11)))
      
      ((= number-of-hash-signs 21)
        (make-token :print "print"))
      
      ((>= number-of-hash-signs 22)
        (make-token :variable
          (- number-of-hash-signs 21)))
      
      (T
        (error "Invalid number of hash signs: ~d."
          number-of-hash-signs)))))



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
                  (source
                   &aux (position 0)
                        (character
                          (when (array-in-bounds-p source position)
                            (char source position))))))
  "The ``Lexer'' is apportioned the wike of detecting and extracting
   from a piece of HS source code a series of significant objects,
   issued as ``Token'' instances."
  (source    (error "Missing source.") :type string)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (setf (lexer-character lexer)
    (when (array-in-bounds-p (lexer-source lexer)
            (1+ (lexer-position lexer)))
      (char (lexer-source lexer)
        (incf (lexer-position lexer)))))
  (values))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent whitespace characters and returns
   no value."
  (declare (type Lexer lexer))
  (loop
    while (and (lexer-character lexer)
               (whitespace-character-p (lexer-character lexer)))
    do    (lexer-advance lexer))
  (values))

;;; -------------------------------------------------------

(defun lexer-count-hash-signs (lexer)
  "Proceeding from the current position into the LEXER's source, tallies
   the number of consecutive hash signs (\"#\"), while concomitantly
   advancing, and returns their account."
  (declare (type Lexer lexer))
  (the (integer 0 *)
    (loop
      while (and (lexer-character lexer)
                 (char= (lexer-character lexer) #\#))
      do    (lexer-advance lexer)
      count 1)))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (case (lexer-character lexer)
      ((NIL)
        (make-token :eof NIL))
      
      ((#\Newline #\Space #\Tab)
        (lexer-skip-whitespaces lexer)
        (lexer-get-next-token   lexer))
      
      (#\#
        (decode-hash-signs
          (lexer-count-hash-signs lexer)))
      
      (#\:
        (prog1
          (make-token :input ":")
          (lexer-advance lexer)))
      
      (otherwise
        (error "Invalid character \"~c\" at position ~d."
          (lexer-character lexer)
          (lexer-position  lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Node
  "The ``Node'' interface establishes a common foundry for all classes
   intent on the representation of HS language facilities in the form of
   an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Variable-Node
  (:include Node))
  "The ``Variable-Node'' class serves in the representation of a
   variable's participation in a program, Procrustean in the regard of
   its indagative or manipulative deployment."
  (identifier (error "Missing identifier.") :type variable-name))

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include Node))
  "The ``Number-Node'' class establishes a representation for an integer
   literal in an abstract syntax tree (AST) node form."
  (value (error "Missing value.") :type decimal-digit))

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include Node))
  "The ``Input-Node'' class reifies an input request command as an
   abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include Node))
  "The ``Binary-Operation-Node'' class encapsulates a comparative
   relationship's participants, namely the left operand, binary
   operator, and the right operand."
  (operator      (error "Missing operator.")     :type binary-operator)
  (left-operand  (error "Missing left operand.") :type Node)
  (right-operand (error "Missing left operand.") :type Node))

;;; -------------------------------------------------------

(defstruct (Block-Node
  (:include Node))
  "The ``Block-Node'' class encapsulates an order list of zero or more
   statements, these themselves in a mis-en-abyme fashion instances of a
   ``Node'' subtype."
  (statements (error "Missing statements.") :type node-list))

;;; -------------------------------------------------------

(defstruct (If-Node
  (:include Node))
  "The ``If-Node'' class comprehends the representation of an \"if\"
   conditional without a \"then\" segment, its constituents enumerated
   by the antecedent, or condition, and a body of zero or more
   statements."
  (condition (error "Missing condition.") :type Binary-Operation-Node)
  (body      (error "Missing body.")      :type Block-Node))

;;; -------------------------------------------------------

(defstruct (Loop-Times-Node
  (:include Node))
  "The ``Loop-Times-Node'' class encapsulates the requisitums for a
   finite iteration's replication, comprehending the repetition count
   and the statements comprising its body."
  (repetitions (error "Missing repetitions.") :type Node)
  (body        (error "Missing body.")        :type Block-Node))

;;; -------------------------------------------------------

(defstruct (Loop-Infinitely-Node
  (:include Node))
  "The ``Loop-Infinitely-Node'' class encapsulates the data requisite
   for an infinite iteration's replication, comprehending the statements
   comprising its body."
  (body (error "Missing body.") :type Block-Node))

;;; -------------------------------------------------------

(defstruct (Print-Node
  (:include Node))
  "The ``Print-Node'' class models a print operation, embracing zero or
   more expressions as its arguments."
  (arguments (error "Missing arguments.") :type node-list))

;;; -------------------------------------------------------

(defstruct (Assignment-Node
  (:include Node))
  "The ``Assignment-Node'' class replicates a variable assignment,
   entailing both the destination variable and the expression or
   expressions to associate therewith."
  (variable    (error "Missing variable.")    :type Variable-Node)
  (expressions (error "Missing expressions.") :type node-list))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include Node))
  "The ``Program-Node'' accommodates the root node for a parses HS
   program, embracing a sequence of zero or more statements, themselves
   expressed in ``Node'' forms."
  (statements (error "Missing statements.") :type Block-Node))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) Node) parser-parse-statement))

;;; -------------------------------------------------------

(defstruct (Parser
  (:constructor make-parser
                  (lexer
                   &aux (current-token
                          (lexer-get-next-token lexer))
                        (next-token
                          (lexer-get-next-token lexer)))))
  "The ``Parser'' class appies itself to the wike of an abstract syntax
   tree's (AST) assemblage from a series of tokens produced by a lexer."
  (lexer         (error "Missing lexer.") :type Lexer)
  (current-token (make-token :eof NIL)    :type Token)
  (next-token    (make-token :eof NIL)    :type Token))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying the next from the underlying lexer; on a
   mismatch, an error of an unspecified type is signaled."
  (declare (type Parser     parser))
  (declare (type token-type expected-token-type))
  (the Token
    (prog1
      (parser-current-token parser)
      (if (token-type-p
            (parser-current-token parser)
            expected-token-type)
        (shiftf
          (parser-current-token parser)
          (parser-next-token    parser)
          (lexer-get-next-token
            (parser-lexer parser)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type
          (parser-current-token parser))))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Parses an integer literal utilizing the PARSER and returns a
   ``Number-Node'' representation thereof."
  (declare (type Parser parser))
  (the Number-Node
    (make-number-node :value
      (token-value
        (parser-eat parser :number)))))

;;; -------------------------------------------------------

(defun parser-parse-variable (parser)
  "Parses a variable utilizing the PARSER and returns a
   ``Variable-Node'' representation thereof."
  (declare (type Parser parser))
  (the Variable-Node
    (make-variable-node :identifier
      (token-value
        (parser-eat parser :variable)))))

;;; -------------------------------------------------------

(defun parser-parse-input (parser)
  "Parses an input command utilizing the PARSER and returns an
   ``Input-Node'' representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :input)
  (the Input-Node
    (make-input-node)))

;;; -------------------------------------------------------

(defun parser-parse-expression (parser)
  "Parses a single expression utilizing the PARSER and returns a
   suitable ``Node'' representation thereof."
  (declare (type Parser parser))
  (the Node
    (case (token-type (parser-current-token parser))
      (:input
        (parser-parse-input parser))
      (:number
        (parser-parse-number parser))
      (:variable
        (parser-parse-variable parser))
      (otherwise
        (error "No expression token: ~s."
          (parser-current-token parser))))))

;;; -------------------------------------------------------

(defun parser-parse-binary-operator (parser)
  "Parses a binary operator utilizing PARSER and returns a
   ``binary-operator'' representation thereof."
  (declare (type Parser parser))
  (flet ((operator-follows-p (current-token-type
                              &optional (next-token-type NIL))
          "Determines whether the PARSER's current token conforms to the
           CURRENT-TOKEN-TYPE and its next token matches the
           NEXT-TOKEN-TYPE, if the latter is specified at all, returning
           on confirmation a ``boolean'' value of ``T'', otherwise
           ``NIL''."
          (declare (type token-type           current-token-type))
          (declare (type (or null token-type) next-token-type))
          (the boolean
            (not (null
              (and
                (token-type-p
                  (parser-current-token parser)
                  current-token-type)
                (or
                  (null next-token-type)
                  (token-type-p
                    (parser-next-token parser)
                    next-token-type))))))))
    (the binary-operator
      (prog1
        (cond
          ((operator-follows-p :less-than :equal)
            :less-than-or-equal-to)
          ((operator-follows-p :less-than)
            :less-than)
          ((operator-follows-p :equal)
            :equal-to)
          ((operator-follows-p :greater-than :equal)
            :greater-than-or-equal-to)
          ((operator-follows-p :greater-than)
            :greater-than)
          (T
            (error "No binary operator token: ~s."
              (parser-current-token parser))))
        (parser-eat parser
          (token-type
            (parser-current-token parser)))))))

;;; -------------------------------------------------------

(defun parser-parse-condition (parser)
  "Parses a comparison expression utilizing the PARSER and returns a
   ``Binary-Operation-Node'' representation thereof."
  (declare (type Parser parser))
  (the Binary-Operation-Node
    (make-binary-operation-node
      :left-operand  (parser-parse-expression      parser)
      :operator      (parser-parse-binary-operator parser)
      :right-operand (parser-parse-expression      parser))))

;;; -------------------------------------------------------

(defun parser-parse-block (parser)
  "Parses a sequence of zero or more statements utilizing the PARSER and
   returns a ``Block-Node'' representation thereof."
  (declare (type Parser  parser))
  (the Block-Node
    (make-block-node :statements
      (loop
        while   (statement-token-p (parser-current-token parser))
        collect (parser-parse-statement parser)))))

;;; -------------------------------------------------------

(defun parser-parse-if (parser)
  "Parses an \"if\"-based condition utilizing the PARSER and returns an
   ``If-Node'' representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :if)
  (let ((condition (parser-parse-condition parser)))
    (declare (type Binary-Operation-Node condition))
    (parser-eat parser :then)
    (let ((body (parser-parse-block parser)))
      (declare (type Block-Node body))
      (parser-eat parser :end-if)
      (the If-Node
        (make-if-node :condition condition :body body)))))

;;; -------------------------------------------------------

(defun parser-parse-loop-times (parser)
  "Parses a finitely repeating loop utilizing the PARSER and returns a
   ``Loop-Times-Node'' representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :loop-times)
  (the Loop-Times-Node
    (prog1
      (make-loop-times-node
        :repetitions
          (parser-parse-expression parser)
        :body
          (parser-parse-block parser))
      (parser-eat parser :end-loop-times))))

;;; -------------------------------------------------------

(defun parser-parse-loop-infinitely (parser)
  "Parses an infinite loop utilizing the PARSER and returns a
   ``Loop-Infinitely-Node'' representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :loop-infinitely)
  (the Loop-Infinitely-Node
    (prog1
      (make-loop-infinitely-node :body
        (parser-parse-block parser))
      (parser-eat parser :end-loop-infinitely))))

;;; -------------------------------------------------------

(defun parser-variable-as-expression-follows-p (parser)
  "Determines whether the PARSER's current state comprehends a variable
   token without a subsequent assignment, thus signifying a variable as
   an expression, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Parser parser))
  (the boolean
    (not (null
      (and
        (token-type-p
          (parser-current-token parser)
          :variable)
        (not
          (token-type-p
            (parser-next-token parser)
            :equal)))))))

;;; -------------------------------------------------------

(defun parser-expression-follows-p (parser)
  "Determines whether the PARSER's current token represents an
   expression, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Parser parser))
  (the boolean
    (or (parser-variable-as-expression-follows-p parser)
        (expression-token-p
          (parser-current-token parser)))))

;;; -------------------------------------------------------

(defun parser-parse-expression-list (parser)
  "Parses a sequence of zero or more expressions utilizing the PARSER
   and returns a ``node-list'' comprehending the items."
  (declare (type Parser parser))
  (the node-list
    (loop
      while   (parser-expression-follows-p parser)
      collect (parser-parse-expression     parser))))

;;; -------------------------------------------------------

(defun parser-parse-print (parser)
  "Parses a print statement utilizing the PARSER and returns a
   ``Print-Node'' representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :print)
  (the Print-Node
    (make-print-node :arguments
      (parser-parse-expression-list parser))))

;;; -------------------------------------------------------

(defun parser-parse-assignment (parser)
  "Parses a variable assignment statement utilizing the PARSER and
   returns an ``Assignment-Node'' representation thereof."
  (declare (type Parser parser))
  (let ((variable (parser-parse-variable parser)))
    (declare (type Variable-Node variable))
    (parser-eat parser :equal)
    (let ((expressions (parser-parse-expression-list parser)))
      (declare (type node-list expressions))
      (the Assignment-Node
        (make-assignment-node
          :variable    variable
          :expressions expressions)))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  "Parses an aefauld statement utilizing the PARSER and returns a
   ``Node'' representation thereof."
  (declare (type Parser parser))
  (the Node
    (case (token-type (parser-current-token parser))
      (:if
        (parser-parse-if parser))
      (:print
        (parser-parse-print parser))
      (:variable
        (parser-parse-assignment parser))
      (:loop-times
        (parser-parse-loop-times parser))
      (:loop-infinitely
        (parser-parse-loop-infinitely parser))
      (otherwise
        (error "No statement token: ~s."
          (parser-current-token parser))))))

;;; -------------------------------------------------------

(defun parser-parse-program (parser)
  "Parses an HS program utilizing the PARSER and returns a
   ``Program-Node'' representation thereof."
  (declare (type Parser parser))
  (the Program-Node
    (make-program-node :statements
      (parser-parse-block parser))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Variable-Table".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Variable-Table
  (:constructor make-variable-table ()))
  "The ``Variable-Table'' class is encumbered with the onus referencing
   the castaldy of variables, their amenability designed in terms of
   positive integer objects that are affiliated with integer values."
  (variables (make-hash-table :test #'eql)
             :type (hash-table-of variable-name integer)))

;;; -------------------------------------------------------

(defun variable-table-get (variables name)
  "Returns for the variable identified by the NAME in the table of
   VARIABLES the associated integer object, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type Variable-Table variables))
  (declare (type variable-name  name))
  (the integer
    (or (gethash name (variable-table-variables variables))
        (error "No variable with the name ~d defined." name))))

;;; -------------------------------------------------------

(defun variable-table-set (variables name value)
  "Associates the variable identified by the NAME with the integer VALUE
   in the VARIABLES table and returns no value.
   ---
   If already extant, any entry answering to the NAME will be supplanted
   by this new definition."
  (declare (type Variable-Table variables))
  (declare (type variable-name  name))
  (declare (type integer        value))
  (setf (gethash name (variable-table-variables variables))
        value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Reference".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Reference
  (:constructor make-reference (identifier)))
  "The ``Reference'' class' bailiwick comprises the encapsulation of a
   variable identifier in a distinguished entity for the sake of its
   conscious handling during the interpretation context."
  (identifier (error "Missing identifier.") :type variable-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun satisfy-condition-p (operator left-operand right-operand)
  "Determines whether the LEFT-OPERAND and RIGHT-OPEARND, when supplied
   to the relational OPERATOR, satisfy its predicate, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type binary-operator operator))
  (declare (type integer         left-operand))
  (declare (type integer         right-operand))
  (the boolean
    (not (null
      (funcall
        (case operator
          (:less-than                #'<)
          (:less-than-or-equal-to    #'<=)
          (:equal-to                 #'=)
          (:greater-than             #'>)
          (:greater-than-or-equal-to #'>=)
          (otherwise
            (error "Invalid binary operator: ~s." operator)))
        left-operand right-operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class assumes the duty of accompassing efficacy
   to an HS program injected in the form of an abstract syntax tree
   (AST)."
  (tree      (error "Missing tree.") :type Program-Node)
  (variables (make-variable-table)   :type Variable-Table))

;;; -------------------------------------------------------

(defgeneric interpreter-resolve-value (interpreter object)
  (:documentation
    "Returns the OBJECT's value as evaluated in the INTERPRETER's
     context.")
  
  (:method ((interpreter Interpreter) (number integer))
    (declare (type Interpreter interpreter))
    (declare (type integer     number))
    (the integer number))
  
  (:method ((interpreter Interpreter) (reference Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (the integer
      (variable-table-get
        (interpreter-variables interpreter)
        (reference-identifier reference))))
  
  (:method ((interpreter T) (object T))
    (declare (type T interpreter))
    (declare (type T object))
    (error "Cannot resolve the value of the object ~s in the ~
            context of ~s."
      interpreter object)))

;;; -------------------------------------------------------

(defgeneric print-expression (interpreter expression)
  (:documentation
    "Writes the EXPRESSION, evaluated in the INTERPRETER's context, to
     the standard output in an appropriate form and returns no value.")
  
  (:method ((interpreter Interpreter)
            (number      integer))
    (declare (type Interpreter interpreter))
    (declare (type integer     number))
    (format T "~d" number)
    (values))
  
  (:method ((interpreter Interpreter)
            (reference   Reference))
    (declare (type Interpreter interpreter))
    (declare (type Reference   reference))
    (format T "~c"
      (code-char
        (interpreter-resolve-value interpreter reference)))
    (values))
  
  (:method ((interpreter T)
            (expression  T))
    (declare (type T interpreter))
    (declare (type T expression))
    (error "Cannot print the expression ~s in the context of ~s."
      expression interpreter)))

;;; -------------------------------------------------------

(defgeneric interpreter-visit-node (interpreter node)
  (:documentation
    "Evaluates the NODE in the INTERPRETER's context and returns a value
     appropriate for this collaboration."))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (interpreter-visit-node interpreter
    (program-node-statements node))
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
                                   (node        Input-Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Input-Node  node))
  (declare (ignore           node))
  (format T "~&>> ")
  (the integer
    (parse-integer
      (read-line))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Number-Node))
  (declare (type Interpreter interpreter))
  (declare (type Number-Node node))
  (the integer
    (number-node-value node)))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Variable-Node))
  (declare (type Interpreter   interpreter))
  (declare (type Variable-Node node))
  (the Reference
    (make-reference
      (variable-node-identifier node))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Print-Node))
  (declare (type Interpreter interpreter))
  (declare (type Print-Node  node))
  (dolist (argument (print-node-arguments node))
    (declare (type Node argument))
    (print-expression interpreter
      (interpreter-visit-node interpreter argument)))
  (values))

;;; -------------------------------------------------------

(defun concatenate-expressions (interpreter expressions)
  (declare (type Interpreter interpreter))
  (declare (type node-list   expressions))
  (the (integer 0 *)
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (loop for expression of-type Node in expressions do
          (format digits "~d"
            (interpreter-resolve-value interpreter
              (interpreter-visit-node interpreter expression))))))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Assignment-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Assignment-Node node))
  (let ((target
          (reference-identifier
            (interpreter-visit-node interpreter
              (assignment-node-variable node)))))
    (declare (type variable-name target))
    (variable-table-set
      (interpreter-variables interpreter)
      target
      (concatenate-expressions interpreter
        (assignment-node-expressions node))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Binary-Operation-Node))
  (declare (type Interpreter           interpreter))
  (declare (type Binary-Operation-Node node))
  (the boolean
    (satisfy-condition-p
      (binary-operation-node-operator node)
      (interpreter-resolve-value interpreter
        (interpreter-visit-node interpreter
          (binary-operation-node-left-operand node)))
      (interpreter-resolve-value interpreter
        (interpreter-visit-node interpreter
          (binary-operation-node-right-operand node))))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        If-Node))
  (declare (type Interpreter interpreter))
  (declare (type If-Node     node))
  (when (interpreter-visit-node interpreter
          (if-node-condition node))
    (interpreter-visit-node interpreter
      (if-node-body node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Loop-Times-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Loop-Times-Node node))
  (loop
    repeat
      (interpreter-resolve-value interpreter
        (interpreter-visit-node interpreter
          (loop-times-node-repetitions node)))
    do
      (interpreter-visit-node interpreter
        (loop-times-node-body node)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Loop-Infinitely-Node))
  (declare (type Interpreter          interpreter))
  (declare (type Loop-Infinitely-Node node))
  (loop do
    (interpreter-visit-node interpreter
      (loop-infinitely-node-body node)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Evaluates the abstract syntax tree (AST) evaluated in the
   INTERPRETER's context and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-HS (code)
  "Interprets the piece of HS source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "-".
;; 
;; Pseudocode:
;; 
;;   var1 = 45
;;   if 1 = 1 then
;;     print var1
;;   end if
(interpret-HS
  "###################### #########  ############### ################
   # ############ #########  ############ ##
   ##################### ######################
   ###")

;;; -------------------------------------------------------

;; Print "Hello World".
;; 
;; Pseudocode:
;; 
;;   var1 = 72   { ASCII code for letter "H". }
;;   var2 = 101  { ASCII code for letter "e". }
;;   var3 = 108  { ASCII code for letter "l". }
;;   var4 = 111  { ASCII code for letter "o". }
;;   var5 = 32   { ASCII code for space  " ". }
;;   var6 = 87   { ASCII code for letter "W". }
;;   var7 = 114  { ASCII code for letter "r". }
;;   var8 = 100  { ASCII code for letter "d". }
;;   print var1 var2 var3 var3 var4 var5 var6 var4 var7 var3 var8
(interpret-HS
  "
  ###################### ######### ################## ############# 
  ####################### ######### ############ ########### ############ 
  ######################## ######### ############ ########### ################### 
  ######################### ######### ############ ############ ############ 
  ########################## ######### ############## ############# 
  ########################### ######### ################### ################## 
  ############################ ######### ############ ############ ############### 
  ############################# ######### ############ ########### ########### 
  ##################### ###################### ####################### ######################## ######################## ######################### ########################## ########################### ######################### ############################ ######################## ############################# 
  ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This version repeatedly queries the user for an input.
;; 
;; Pseudocode:
;; 
;;   loop infinitely
;;     var1 = input
;;     if input = 0 then
;;       print 0
;;     end if
;;     if input = 1 then
;;       loop infinitely
;;         print 1
;;       end loop infinitely
;;     end if
;;   end loop infinitely
(interpret-HS
  "
   ######
   ###################### ######### :
   # ###################### ######### ########### ##
   ##################### ###########
   ###
   # ###################### ######### ############ ##
   ######
   ##################### ############
   #######
   ###
   #######
  ")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which accepts integer input, but
;; prints the associated ASCII character.
;; 
;; Pseudocode:
;; 
;;   loop infinitely
;;     var1 = input
;;     print var1
;;   end loop infinitely
(interpret-HS
  "######
     ###################### ######### :
     #####################  ######################
   #######")

;;; -------------------------------------------------------

;; Counter which prints asterisks ("*") a tally of times specified by
;; the user.
;; 
;; Pseudocode:
;; 
;;   var1 = input
;;   var2 = "*" (= 4, 2)
;;   loop times var1
;;     print var2
;;   end loop
;;   
(interpret-HS
  "
  ###################### ######### :
  
  
  ####################### ######### ############### #############
  
  
  #### ######################
    ##################### #######################
  #####
  ")
