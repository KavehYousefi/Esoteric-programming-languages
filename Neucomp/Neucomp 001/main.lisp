;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Neucomp", invented by the Esolang user "Benett0222" and
;; presented on July 1st, 2023, its proprium manifesting in the
;; indagation and modulation of an infinite dispansion of cells
;; capacitated to store signed integer scalar, themselves invested with
;; the amenability to subscript of the same species, the program's
;; conformation ensuing from a mimicry of a line-based assembly
;; language.
;; 
;; 
;; Concept
;; =======
;; The Neucomp programming language operates on an assembly-like
;; sequence of statements, any of these a dedicated line's commorant,
;; in order to manipulate an infinite random-access memory composed of
;; signed integer-valued cells.
;; 
;; == "NEUCOMP": A [NEU]MANN-STYLED [COMP]UTER ==
;; The language's agnomination, "Neucomp", serves as an index to its
;; designment's foundational concept, scilicet, the specification of a
;; "[Neu]mann-styled [Com]puter".
;; 
;; == THE SYNTAX MIMICKS AN ASSEMBLY LANGUAGE ==
;; The syntaxis aspires the attention of an assembly language's
;; simulacrum, distributing across dedicated lines its instructions,
;; each such composed of an operator name, entalented with a
;; compendiousness consanguinous to a mnemonic, and succeeded by zero
;; or more integral operands.
;; 
;; == THE PROGRAM MEMORY: A INFINITE ARRAY OF SIGNED INTEGERS ==
;; The Neucomp program memory subscribes to an infinite dispansion of
;; signed integer-valued cells, wisting of no imposition in their
;; capacity, and amenable to indices desumed from the same realm.
;; 
;; 
;; Syntax
;; ======
;; The Neucomp programming language replicates its operative intentions
;; in the source code as a sequence of lines, any comprehending at most
;; one instruction.
;; 
;; == EACH LINE ENTAILS AT MOST ONE INSTRUCTION ==
;; From a syntatical application of the conspectuity, a Neucomp
;; program's conformation subsumed into an ordered sequence of zero or
;; more lines, everichon among these either an aefauld instruction's
;; commorancy, or a blank specimen.
;; 
;; == AN INSTRUCTION: COMPOSED OF A MNEMONIC AND ITS OPERANDS ==
;; A statement line's designment proceeds from a differentiation into
;; an operation identifier and its zero or more operands, segregated
;; by means of spaces into tokens serelepes.
;; 
;; == COMMENTS ==
;; The provision for comments is defined by a parasceuastic hash sign,
;; "", in whose trail may be stated an arbitrary account of characters,
;; and which concludes with the first newline entity.
;; 
;; == GRAMMAR ==
;; A higher mete of formality's dation shall constitute the following
;; Extended Backus-Naur Form (ENBF) treatise's contribution:
;; 
;;   program            := [ newlines ]
;;                      ,  { innerLine }
;;                      ,  [ lastLine ]
;;                      ;
;;   statementList      := [ newlines ] , { innerLine } ;
;;   innerLine          := lineContent , newlines
;;   lastLine           := lineContent , [ newlines ] ;
;;   lineContent        := [ statement ] , [ comment ] ;
;;   
;;   comment            := "#" , { character - newline } ;
;;   
;;   statement          := ALUStatement
;;                      |  GoStatement
;;                      |  HaltStatement
;;                      |  IfStatement
;;                      |  InStatement
;;                      |  LetStatement
;;                      |  OutStatement
;;                      |  PrtStatement
;;                      ;
;;   ALUStatement       := "ALU"  , arithmetics ;
;;   GoStatement        := "Go"   , expression ;
;;   HaltStatement      := "Halt" ;
;;   IfStatement        := "If"   , statementList , "End" ;
;;   InStatement        := "In"   , expression ;
;;   LetStatement       := "Let"  , expression , expression ;
;;   OutStatement       := "Out"  , expression ;
;;   PrtStatement       := "Prt"  , expression ;
;;   
;;   arithmetics        := expression , binaryOperator , expression ;
;;   binaryOperator     := "+" | "-" | "*" | "/" ;
;;   predicate          := expression
;;                      ,  relationalOperator
;;                      ,  expression
;;                      ;
;;   relationalOperator := "=" | "!=" | "<" | "<=" | ">" | ">=" ;
;;   
;;   expression         := reference | integerLiteral
;;   reference          := "Val" , expression ;
;;   integerLiteral     := sign , digit , { digit } ;
;;   sign               := [ "+" | "-" ] ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   
;;   newlines           := newline , { newline } ;
;;   newline            := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The Neucomp instruction set enumerates an octuple contingency, the
;; bailiwicks inwith which these members are accommodated their wonings
;; including memory value assignments, basic arithmetics, input and
;; output handling, as well as conditional execution and a goto facility
;; having its foundry edified upon the line numbering.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be satisfied in the adhibition of
;; a cursory mete of nortelry anenst Neucomp's operative competences.
;; 
;; Please heed the demarcation of succedaneous tmemata by adminiculum
;; of a catena composed of asterisks ("*"), their parcels intended to
;; be superseded by actual Neucomp code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command                  | Effect
;;   -------------------------+----------------------------------------
;;   Let target source        | Stores the {source} value in the memory
;;       ****** ******        | cell amenable to the {target} index.
;;                            |----------------------------------------
;;                            | {target} must be an expression.
;;                            |----------------------------------------
;;                            | {source} must be an expression.
;;                            |----------------------------------------
;;                            | In a pseudocode diction, it holds:
;;                            |   memory[TARGET] <- SOURCE
;;   ..................................................................
;;   ALU left right op target | Applies the binary operation {op} to
;;       **** ***** ** ****** | the value in the memory cell amenable
;;                            | to the index {left} and the cell
;;                            | answering to the index {right}, and
;;                            | stores the result in the cell amenable
;;                            | to the {target} index.
;;                            |----------------------------------------
;;                            | {left} must be an expression.
;;                            |----------------------------------------
;;                            | {right} must be an expression.
;;                            |----------------------------------------
;;                            | {target} must be an expression.
;;                            |----------------------------------------
;;                            | For the homologated {op} operators
;;                            | please consult the subsection
;;                            | "ARITHMETIC OPERATORS".
;;                            |----------------------------------------
;;                            | In a pseudocode, it holds:
;;                            |   let leftVal    <- memory[LEFT]
;;                            |   let rightVal   <- memory[RIGHT]
;;                            |   let result     <- leftVal OP rightVal
;;                            |   memory[TARGET] <- result
;;   ..................................................................
;;   Out argument             | Prints the value of the memory cell
;;       ********             | amenable to the index {argument} to the
;;                            | standard output.
;;                            |----------------------------------------
;;                            | {argument} must be an expression.
;;                            |----------------------------------------
;;                            | In a pseudocode diction, it holds:
;;                            |   let value <- memory[ARGUMENT]
;;                            |   print value
;;   ..................................................................
;;   Prt argument             | Prints the character whose ASCII code
;;       ********             | corresponds to the value of the memory
;;                            | cell amenable to the index {argument}
;;                            | to the standard output.
;;                            |----------------------------------------
;;                            | {argument} must be an expression.
;;                            |----------------------------------------
;;                            | In a pseudocode diction, it holds:
;;                            |   let charCode <- memory[ARGUMENT]
;;                            |   print characterForCode(charCode)
;;   ..................................................................
;;   In target                | Queries the standard input for an
;;      ******                | character and stores its ASCII code in
;;                            | the memory cell amenable to the
;;                            | {target} index.
;;                            |----------------------------------------
;;                            | {target} must be an expression.
;;                            |----------------------------------------
;;                            | In a pseudocode diction, it holds:
;;                            |   let input    <- query for character
;;                            |   let charCode <- ASCII code for input
;;                            |   memory[TARGET] <- charCode
;;   ..................................................................
;;   If left rel right        | If the value stored in the memory cell
;;      **** *** *****        | amenable to the {left} index and that
;;     statements             | stored in the cell with the {right}
;;     **********             | index satisfy the relational operator
;;   End                      | {rel} in this exact order, executes
;;                            | the {statements}; otherwise skips to
;;                            | the position immediately succeeding the
;;                            | matching "End" token.
;;                            |----------------------------------------
;;                            | {left} must be an expression.
;;                            |----------------------------------------
;;                            | {right} must be an expression.
;;                            |----------------------------------------
;;                            | For the homologated {rel} operators,
;;                            | please consult the subsection
;;                            | "RELATIONAL OPERATORS".
;;                            |----------------------------------------
;;                            | {statements} must be a sequence of zero
;;                            | or more instructions.
;;   ..................................................................
;;   Go target                | Relocates the instruction pointer (IP)
;;      ******                | to the one-based program line specified
;;                            | by the {target}.
;;                            |----------------------------------------
;;                            | {target} must be an expression.
;;                            |----------------------------------------
;;                            | If the {target} designates a position
;;                            | beyond the admissible bournes of
;;                            |   1 <= target <= length(program)
;;                            | the program immediately terminates.
;;   ..................................................................
;;   Halt                     | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; == EXPRESSIONS ==
;; Expressions, the foundry for the instruction operands, bifurcate into
;; a twissel of conceivable classes, namely, integer literals and
;; cell references:
;; 
;;   (1) INTEGER LITERALS
;;       A single species of literal datum registers its participation
;;       in the program, its incarnation signed or unsigned integer
;;       numbers of any mickleness, expressed in the decimal system.
;;       Ligated in their construe's dependency upon the concrete
;;       instruction, these numeric constituents may either contribute
;;       their immediate value, or serve as a memory address'
;;       designator.
;;   
;;   (2) CELL REFERENCES
;;       An explicit cell reference, serving to educe a memory unit's
;;       content, ensues from the communication via the "Val" keyword,
;;       whence follows either a direct integer literal as the address,
;;       or a modus akin to a mise en abyme produced through arbitrarily
;;       deep nestings of "Val" requests.
;; 
;; == ARITHMETIC OPERATORS ==
;; Neucomp wists of a quadruple contingency for the causata woning in
;; the arithmetic operations' realm, thilk in this programming language
;; is confined to a twifold arity, admitting on both places signed
;; integer constituents:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Description
;;   ---------+--------------------------------------------------------
;;   +        | ADDITION: Returns the sum obtained by adding to the
;;            | left operand the right one.
;;   ..................................................................
;;   -        | SUBTRACTION: Returns the difference obtained by
;;            | subtracting from the left operand the right one.
;;   ..................................................................
;;   *        | MULTIPLICATION: Returns the product obtained by
;;            | multiplying the left operand by the right one.
;;   ..................................................................
;;   /        | INTEGER DIVISION: Returns the quotient obtained by
;;            | dividing the left operand by the right one, the result
;;            | being rounded to the nearest integer.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONAL OPERATORS ==
;; A sextuple of relational operators serves in the governance of
;; predicates betwixt a twissel of integer participants, this
;; membership's deployment restricted to the "If" instruction's
;; antecedent:
;; 
;;   ------------------------------------------------------------------
;;   Relation | Description
;;   ---------+--------------------------------------------------------
;;   =        | EQUAL: Is satisfied if the left operand and the right
;;            | operand are equal.
;;   ..................................................................
;;   !=       | NOT EQUAL: Is satisfied if the left operand does not
;;            | equal the right operand.
;;   ..................................................................
;;   <        | LESS THAN: Is satisfied if the left operand is strictly
;;            | less than the right operand.
;;   ..................................................................
;;   <=       | LESS THAN OR EQUAL TO: Is satisfied if the left operand
;;            | is less than or equal to the right operand.
;;   ..................................................................
;;   >        | GREATER THAN: Is satisfied if the left operand is
;;            | strictly greater than the right operand.
;;   ..................................................................
;;   >=       | GREATER THAN OR EQUAL TO: Is satisfied if the left
;;            | operand is greater than or equal to the right operand.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation represents an effort in the
;; programming language Common Lisp, its realization ultimately a
;; produced obtained by intrining the stages of lexical analyzation,
;; parsing, and actual interpretation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-03-19
;; 
;; Sources:
;;   [esolang2024Neucomp]
;;   The Esolang contributors, "Neucomp", May 11th, 2024
;;   URL: "https://esolangs.org/wiki/Neucomp"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (name (candidate-name &rest lambda-list)
                              &body body)
  "Defines a derived type whose agnomation is desumed from the NAME,
   the formal parameters registering an appropriation of the
   LAMBDA-LIST, and whose probed subject is assigned the identification
   of the CANDIDATE-NAME, evaluating the BODY forms, with the desinent
   form's primary return value construed as the docimasy's conclusion,
   a \"generalized boolean\" value of \"true\" serving in conformance
   signification, while \"false\" bears a rejection's mark.
   ---
   The first BODY form, if rsolving to a string object, will be imputed
   to contribute the derived type's documentation string, and will
   subsequently be appropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   '*)
                                             (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among these complies with the KEY-TYPE and
   answers with a value of the VALUE-TYPE, for both is designated the
   default in the generic ``*'' sentinel."
  (and
    (hash-table-p candidate)
    (or
      (and (eq key-type   '*)
           (eq value-type '*))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and
            (or (eq    key-type      '*)
                (typep current-key   key-type))
            (or (eq    value-type    '*)
                (typep current-value value-type)))))))

;;; -------------------------------------------------------

(define-custom-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, every member of which complies with the ELEMENT-TYPE, to
   whom is assigned the generic sentinel ``*'' as the default."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (loop
        for    current-element of-type T in (the list candidate)
        always (typep current-element element-type)))))

;;; -------------------------------------------------------

(deftype arithmetic-operator ()
  "The ``arithmetic-operator'' type enumerates the recognized variation
   of binary arithmetic operators."
  '(member
    :plus
    :minus
    :times
    :divided))

;;; -------------------------------------------------------

(deftype relational-operator ()
  "The ``relational-operator'' type enumerates the recognized variation
   of binary relational operators."
  '(member
    :equal-to
    :not-equal-to
    :less-than
    :less-than-or-equal-to
    :greater-than
    :greater-than-or-equal-to))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Neucomp program as a
   one-dimensional simple array of ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a unilateral affiliation in a Neucomp
   program betwixt a conditional \"If\" instruction and its matching
   \"End\" demarcation, its reification educed as a hash table whose
   keys acquire the zero-based \"If\" command's indices into the
   parsed program sequence, responding with the \"End\" position
   designators desumed from the same conceptual and logical vale."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Neucomp program memory as a sparse
   vector of signed integer-indexed cells, each such a salvatory for
   a scalar signed integer number, and realized as a hash table whose
   keys accommodate the integral subscript, answering with integer
   numbers of any sign and mickleness."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\"
   value, returning for a non-``NIL'' input a ``boolean'' value of
   ``T''; otherwise, for a ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a spacing character,
   the diorism apportioned to whom amplects the twissel of space and
   horizontal tab, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Return)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents an arithmetic sign,
   that is, either plus (\"+\") or minus (\"-\"), returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\+)
          (char= candidate #\-)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (source desideratum start-point)
  "Determines whether the DESIDERATUM commences in the SOURCE at the
   START-POINT, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type simple-string source))
  (declare (type simple-string desideratum))
  (declare (type fixnum        start-point))
  (the boolean
    (get-boolean-value-of
      (string= source desideratum
        :start1 start-point
        :end1   (min (+ start-point (length desideratum))
                     (length source))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted from a piece of Neucomp source code, and assigned a
   diorism compact of a twissel that references the categorizing class
   and the detailing value."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (candidate expected-type)
  "Determines whether the CANDIDATE token complies with the
   EXPECTED-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token   candidate))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type candidate)
          expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global tokens.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Token +EOF-TOKEN+))
(declaim (type Token +NEWLINE-TOKEN+))

;;; -------------------------------------------------------

(defparameter +EOF-TOKEN+
  (make-eof-token)
  "A globally visible representation of the end-of-file (EOF) token,
   which signifies a source's exhaustion.")

(defparameter +NEWLINE-TOKEN+
  (make-token :newline #\Newline)
  "A globally visible representation of an newline, linefeed, or
   carriage return character in a token mold.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of simple-string Token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Allies the recognized Neucomp identifiers with representative
   ``Token'' objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (identifier token-type token-value)
        "Associates the IDENTIFIER with a fresh token composed of the
         categorizing TOKEN-TYPE and the detailing TOKEN-VALUE, stores
         this affiliation in the global +IDENTIFIERS+ table, and returns
         no value.
         ---
         Any extant entry amenable to the IDENTIFIER key will be tacitly
         superseded by the new entry."
        (declare (type simple-string identifier))
        (declare (type keyword       token-type))
        (declare (type simple-string token-value))
        (setf (gethash identifier +IDENTIFIERS+)
          (make-token token-type token-value))
        (values)))
  (register-identifier "ALU"  :alu  "ALU")
  (register-identifier "End"  :end  "End")
  (register-identifier "Go"   :go   "Go")
  (register-identifier "Halt" :halt "Halt")
  (register-identifier "If"   :if   "If")
  (register-identifier "In"   :in   "In")
  (register-identifier "Let"  :let  "Let")
  (register-identifier "Out"  :out  "Out")
  (register-identifier "Prt"  :prt  "Prt")
  (register-identifier "Val"  :val  "Val")
  (values))

;;; -------------------------------------------------------

(defun look-up-identifier-token (name)
  "Returns for the identifier NAME the representative token or, upon its
   disrespondency, signals an error of an unspecified type."
  (declare (type string name))
  (the Token
    (or (gethash name +IDENTIFIERS+)
        (error "Invalid identifier: ~s." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (original-source
     &aux (source    (coerce original-source 'simple-string))
          (position  0)
          (character
            (when (array-in-bounds-p source position)
              (schar source position))))))
  "The ``Lexer'' class applies itself to the lexical analyzation of a
   piece of Neucom source code, whence ensues the delivery of its
   incorporated tokens."
  (source    (error "Missing lexer source.")
             :type      simple-string
             :read-only T)
  (position  (error "Missing lexer position.")
             :type      fixnum
             :read-only NIL)
  (character (error "Missing lexer character.")
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slot ``source'' to the local symbol
   macro ``$source'', ``position'' to ``$position'', and ``character''
   to ``$character'', evaluates the BODY forms, and returns the desinent
   form's results.
   ---
   As a parergon to the symbolic definitions, a twain of operative
   warklumes is accoutred as a twifold commodity's incarnation in two
   local function implementations:
     ------------------------------------------------------------------
     Function                     | Causatum
     -----------------------------+------------------------------------
     advance-to-next-character () | Advances the LEXER's position
                                  | cursor to the subsequent character
                                  | and returns no value.
     ..................................................................
     advance-by-offset (offset)   | Translates the LEXER's position
                                  | cursor relative to its current
                                  | state by the OFFSET and returns no
                                  | value.
     ------------------------------------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           (($source
             (the simple-string
               (lexer-source ,evaluated-lexer)))
            ($position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            ($character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type simple-string       $source)
                  (ignorable                $source))
         (declare (type fixnum              $position)
                  (ignorable                $position))
         (declare (type (or null character) $character)
                  (ignorable                $character))
         (flet
             ((advance-to-next-character ()
               "Advances the LEXER's position cursor to the next
                character, if possible, and returns no value."
               (setf $position
                 (min
                   (1+ $position)
                   (length $source)))
               (setf $character
                 (when (array-in-bounds-p $source $position)
                   (schar $source $position)))
               (values))
              
              (advance-by-offset (offset)
               "Advances the LEXER's position cursor by the OFFSET
                relative to its contemporaneous location and returns no
                value."
               (declare (type fixnum offset))
               (incf $position offset)
               (setf $character
                 (when (array-in-bounds-p $source $position)
                   (schar $source $position)))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent spaces and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and $character (space-character-p $character)) do
      (advance-to-next-character)))
  (values))

;;; -------------------------------------------------------

(defun has-reached-end-of-line-p (lexer)
  "Determines whether the LEXER has either reached a newline character
   or the end of its maintained source, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (get-boolean-value-of
      (with-lexer (lexer)
        (or (null                $character)
            (newline-character-p $character))))))

;;; -------------------------------------------------------

(defun skip-comment (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a commentary tmema, its cessure realized either in the immediate
   prevenience of a newline character or in the source's exhaustion, and
   returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop until (has-reached-end-of-line-p lexer) do
      (advance-to-next-character)))
  (values))

;;; -------------------------------------------------------

(defun read-identifier-name (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier name and returns a string representation thereof."
  (declare (type Lexer lexer))
  (the string
    (with-output-to-string (identifier)
      (declare (type string-stream identifier))
      (with-lexer (lexer)
        (loop while (and $character (alpha-char-p $character)) do
          (write-char $character identifier)
          (advance-to-next-character))))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a Neucomp identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (look-up-identifier-token
      (read-identifier-name lexer))))

;;; -------------------------------------------------------

(defun integer-number-follows-p (lexer)
  "Proceeding from the current position into the LEXER, determines
   whether an integer literal follows, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (get-boolean-value-of
      (with-lexer (lexer)
        (when $character
          (or
            (digit-char-p $character)
            (and
              ;; Current character is an arithmetic sign, ...
              (sign-character-p $character)
              ;; ... another character follows, ...
              (array-in-bounds-p $source
                (1+ $position))
              ;; which constitutes a decimal digit?
              (digit-char-p
                (schar $source
                  (1+ $position))))))))))

;;; -------------------------------------------------------

(defun read-optional-sign-into (lexer destination)
  "Proceeding from the current position into the LEXER's source, reads
   an optional arithmetic sign, writes thilk to the DESTINATION, if
   present, and returns no value."
  (declare (type Lexer         lexer))
  (declare (type string-stream destination))
  (with-lexer (lexer)
    (cond
      ((null $character)
        (error "No integer constituents detected commencing at ~
                position ~d."
          $position))
      ((sign-character-p $character)
        (write-char $character destination)
        (advance-to-next-character))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(defun read-digits-into (lexer destination)
  "Proceeding from the current position into the LEXER's source, reads
   a sequence of zero or more decimal digits, writes thilk to the
   DESTINATION, and returns no value."
  (declare (type Lexer         lexer))
  (declare (type string-stream destination))
  (with-lexer (lexer)
    (loop while (and $character (digit-char-p $character)) do
      (write-char $character destination)
      (advance-to-next-character)))
  (values))

;;; -------------------------------------------------------

(defun read-signed-integer (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a signed or unsigned integer number and returns thilk."
  (declare (type Lexer lexer))
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (read-optional-sign-into lexer digits)
        (read-digits-into        lexer digits)))))

;;; -------------------------------------------------------

(defun read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an unsigned integer number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (read-signed-integer lexer))))

;;; -------------------------------------------------------

(defun character-sequence-follows-p (lexer expected-characters)
  "Proceeding from the current position into the LEXER's source,
   determines whether its subsequent characters replicate the
   EXPECTED-CHARACTERS, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Lexer         lexer))
  (declare (type simple-string expected-characters))
  (the boolean
    (with-lexer (lexer)
      (substring-starts-at-p $source expected-characters $position))))

;;; -------------------------------------------------------

(defun match-character-sequence (lexer expected-characters)
  "Proceeding from the current position into the LEXER's source,
   determines whether its subsequent characters replicate the
   EXPECTED-CHARACTERS, returning on confirmation a ``boolean'' value
   of ``T'', while concomitantly advancing the position cursor to the
   index immediately succeeding the matching tmema; otherwise produces
   ``NIL'' without any advancement."
  (declare (type Lexer         lexer))
  (declare (type simple-string expected-characters))
  (the boolean
    (with-lexer (lexer)
      (when (character-sequence-follows-p lexer expected-characters)
        (advance-by-offset
          (length expected-characters))
        T))))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Creates and returns a fresh token which combines the categorizing
   TOKEN-TYPE with the LEXER's current character as its value, while
   concomitantly advancing its position cursor to the next character in
   its source."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (with-lexer (lexer)
      (prog1
        (make-token token-type $character)
        (advance-to-next-character)))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   the same end-of-file (EOF) token instance."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ;; Cessation of the provenance.
        ((null $character)
          +EOF-TOKEN+)
        
        ;; Negligible content (spaces and comments).
        ((char= $character #\#)
          (skip-comment   lexer)
          (get-next-token lexer))
        ((space-character-p $character)
          (skip-spaces    lexer)
          (get-next-token lexer))
        
        ;; Sepiments.
        ((newline-character-p $character)
          (prog1 +NEWLINE-TOKEN+
            (advance-to-next-character)))
        
        ;; Identifiers and literals.
        ((alpha-char-p $character)
          (read-identifier lexer))
        ((integer-number-follows-p lexer)
          (read-number lexer))
        
        ;; Arithmetic operators.
        ((char= $character #\+)
          (read-symbol lexer :plus))
        ((char= $character #\-)
          (read-symbol lexer :minus))
        ((char= $character #\*)
          (read-symbol lexer :times))
        ((char= $character #\/)
          (read-symbol lexer :divided))
        
        ;; Relational operators.
        ((char= $character #\=)
          (read-symbol lexer :equal-to))
        ((match-character-sequence lexer "!=")
          (make-token :not-equal-to "!="))
        ((match-character-sequence lexer "<=")
          (make-token :less-than-or-equal-to "<="))
        ((char= $character #\<)
          (read-symbol lexer :less-than))
        ((match-character-sequence lexer ">=")
          (make-token :greater-than-or-equal-to ">="))
        ((char= $character #\>)
          (read-symbol lexer :greater-than))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface serves in the encapsulation of a Neucomp
   instruction operand in a cohesive unit.")

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:constructor make-literal-operand (value))
  (:include     Operand))
  "The ``Literal-Operand'' class serves in the encapsulation of a
   literal integer number in an operand's guise."
  (value (error "Missing literal operand value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Reference-Operand
  (:constructor make-reference-operand (target))
  (:include     Operand))
  "The ``Reference-Operand'' class serves in the encapsulation of a
   reference to either an integer literal or a parhedral reference."
  (target (error "Missing reference target.")
          :type      Operand
          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface serves in the representation of a
   Neucomp instruction.")

;;; -------------------------------------------------------

(defstruct (ALU-Instruction
  (:include Instruction))
  "The ``ALU-Instruction'' class applies itself to the representation of
   the Neucomp \"ALU\" instruction, dedicated to supputate of a binary
   arithmetic expression and its transfer into a specified program
   memory cell."
  (left-operand  (error "Missing left ALU instruction operand.")
                 :type      Operand
                 :read-only T)
  (right-operand (error "Missing right ALU instruction source.")
                 :type      Operand
                 :read-only T)
  (operator      (error "Missing ALU instruction operator.")
                 :type      arithmetic-operator
                 :read-only T)
  (target        (error "Missing ALU instruction target.")
                 :type      Operand
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (End-Instruction
  (:include Instruction))
  "The ``End-Instruction'' class applies itself to the representation of
   the Neucomp \"End\" instruction, dedicated to the demarcation of a
   conditional \"If\" block's conclusion.")

;;; -------------------------------------------------------

(defstruct (Go-Instruction
  (:include Instruction))
  "The ``Go-Instruction'' class applies itself to the representation of
   the Neucomp \"Go\" instruction, dedicated to an unconditional
   line-based instruction pointer (IP) relocation."
  (target (error "Missing Go instruction target.")
          :type      Operand
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Halt-Instruction
  (:include Instruction))
  "The ``Halt-Instruction'' class applies itself to the representation
   of the Neucomp \"Halt\" instruction, dedicated to an unconditional
   program termination.")

;;; -------------------------------------------------------

(defstruct (If-Instruction
  (:include Instruction))
  "The ``If-Instruction'' class applies itself to the representation of
   the Neucomp \"If\" instruction, dedicated to the definition of a
   conditional execution block."
  (left-operand  (error "Missing left If instruction operand.")
                 :type      Operand
                 :read-only T)
  (right-operand (error "Missing right If instruction source.")
                 :type      Operand
                 :read-only T)
  (operator      (error "Missing If instruction operator.")
                 :type      relational-operator
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (In-Instruction
  (:include Instruction))
  "The ``In-Instruction'' class applies itself to the reprsentation of
   the Neucomp \"In\" instruction, dedicated to the reception of user
   input and its transfer into a program memory cell."
  (target (error "Missing In instruction target.")
          :type      Operand
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Let-Instruction
  (:include Instruction))
  "The ``Let-Instruction'' class applies itself to the representation of
   the Neucomp \"Let\" instruction, dedicated to the assignment of a
   value to a program memory cell."
  (target (error "Missing Let instruction target.")
          :type      Operand
          :read-only T)
  (source (error "Missing Let instruction source.")
          :type      Operand
          :read-only T))

;;; -------------------------------------------------------

(defstruct  (Out-Instruction
  (:include Instruction))
  "The ``Out-Instruction'' class applies itself to the representation of
   the Neucomp \"Out\" instruction, dedicated to the output of a literal
   number or a program memory cell's value in its verbatim numeric
   form."
  (argument (error "Missing Out instruction argument.")
            :type      Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct  (Prt-Instruction
  (:include Instruction))
  "The ``Prt-Instruction'' class applies itself to the representation of
   the Neucomp \"Prt\" instruction, dedicated to the output of a literal
   number or a program memory cell's value in the form of the character
   whose ASCII code concurs with the numeric code."
  (argument (error "Missing Prt instruction argument.")
            :type      Operand
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' from the INSTRUCTIONS list."
  (declare (type (list-of Instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser
    (lexer
     &aux (current-token (get-next-token lexer)))))
  "The ``Parser'' class attends to the dever of a Neucomp program's
   assemblage from a token stream, thilk's provenance manifests in a
   dedicated lexer entity."
  (lexer         (error "Missing lexer.")
                 :type Lexer
                 :read-only T)
  (current-token (error "Missing current token.")
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slot ``lexer'' to the local symbol
   macro ``$lexer'' and the ``current-token'' to ``$current-token'',
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   As a parergon to the to the facility for eath slot access, an
   adminicular local function's furnishment partakes of the dation:
     ------------------------------------------------------------------
     Function                  | Causatum
     --------------------------+---------------------------------------
     eat-token (expected-type) | If the current token matches the
                               | EXPECTED-TYPE, returns the probed
                               | token, while replacing it in the
                               | PARSER with the next one; otherwise
                               | signals an error.
     ------------------------------------------------------------------"
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser)
                (ignorable   ,evaluated-parser))
       (symbol-macrolet
           (($lexer
             (the Lexer
               (parser-lexer ,evaluated-parser)))
            ($current-token
             (the Token
               (parser-current-token ,evaluated-parser))))
         (declare (type Lexer $lexer)
                  (ignorable  $lexer))
         (declare (type Token $current-token)
                  (ignorable  $current-token))
         (flet
             ((eat-current-token ()
               "Returns the PARSER's current token, while concomitantly
                querying the subsequent instance from the underlying
                lexer and replacing the same in the PARSER."
               (the Token
                 (prog1 $current-token
                   (setf $current-token
                     (get-next-token $lexer))))))
           ,@body)))))

;;; -------------------------------------------------------

(defun parse-expression (parser)
  "Parses a numeric expression, this either subsuming into the species
   of an integer literal or a reference, employing the PARSER's context,
   and returns a conable ``Operand'' representation thereof."
  (declare (type Parser parser))
  (the Operand
    (with-parser (parser)
      (case (token-type $current-token)
        (:number
          (make-literal-operand
            (token-value
              (eat-current-token))))
        (:val
          (eat-current-token)
          (make-reference-operand
            (parse-expression parser)))
        (otherwise
          (error "No expression token: ~s." $current-token))))))

;;; -------------------------------------------------------

(defun parse-arithmetic-operator (parser)
  "Parses an arithmetic operator in the PARSER's context and returns a
   covenable ``arithmetic-operator'' representation thereof."
  (declare (type Parser parser))
  (the arithmetic-operator
    (with-parser (parser)
      (if (typep (token-type $current-token) 'arithmetic-operator)
        (token-type
          (eat-current-token))
        (error "No arithmetic operator token: ~s." $current-token)))))

;;; -------------------------------------------------------

(defun parse-relational-operator (parser)
  "Parses a relational operator in the PARSER's context and returns a
   covenable ``relational-operator'' representation thereof."
  (declare (type Parser parser))
  (the relational-operator
    (with-parser (parser)
      (if (typep (token-type $current-token) 'relational-operator)
        (token-type
          (eat-current-token))
        (error "No relational operator token: ~s." $current-token)))))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Attempts to parse a Neucomp instruction by the PARSER's adminiculum,
   returning on confirmation a connable ``Instruction'' representation
   of its result; otherwise responds with ``NIL''."
  (declare (type Parser parser))
  (the (or null Instruction)
    (with-parser (parser)
      (case (token-type $current-token)
        (:alu
          (eat-current-token)
          (make-alu-instruction
            :left-operand  (parse-expression          parser)
            :right-operand (parse-expression          parser)
            :operator      (parse-arithmetic-operator parser)
            :target        (parse-expression          parser)))
        
        (:end
          (eat-current-token)
          (make-end-instruction))
        
        (:go
          (eat-current-token)
          (make-go-instruction :target (parse-expression parser)))
        
        (:halt
          (eat-current-token)
          (make-halt-instruction))
        
        (:if
          (eat-current-token)
          (make-if-instruction
            :left-operand  (parse-expression          parser)
            :operator      (parse-relational-operator parser)
            :right-operand (parse-expression          parser)))
        
        (:in
          (eat-current-token)
          (make-in-instruction :target (parse-expression parser)))
        
        (:let
          (eat-current-token)
          (make-let-instruction
            :target (parse-expression parser)
            :source (parse-expression parser)))
        
        (:out
          (eat-current-token)
          (make-out-instruction :argument (parse-expression parser)))
        
        (:prt
          (eat-current-token)
          (make-prt-instruction :argument (parse-expression parser)))
        
        (otherwise
          NIL)))))

;;; -------------------------------------------------------

(defun skip-newlines (parser)
  "Skips a sequence of zero or more accolent newline tokens in the
   PARSER's context and returns no value."
  (declare (type Parser parser))
  (with-parser (parser)
    (loop while (token-is-of-type-p $current-token :newline) do
      (eat-current-token)))
  (values))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Assembles a Neucomp instruction list with the PARSER as an
   adminiculum and returns a fresh ``program'' representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (make-program
      (loop
        initially
          (skip-newlines parser)
        
        append
          (let ((next-instruction (parse-instruction parser)))
            (declare (type (or null Instruction) next-instruction))
            (when next-instruction
              (list next-instruction)))
        
        do
          (case (token-type $current-token)
            (:newline
              (skip-newlines parser))
            (:eof
              (loop-finish))
            (otherwise
              (error "An invalid token succeeds a statement: ~s."
                $current-token)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-jump-table ()
  "Creates and returns an initially vacant ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun build-jump-table-for (program)
  "Creates and returns a fresh ``jump-table'' which applies itself to
   the castaldy of the vincula betwixt the jumelles of
   ``If-Instruction''s and the matching ``End-Instruction'' objects."
  (declare (type program program))
  (let ((jump-table               (prepare-empty-jump-table))
        (if-instruction-locations NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) if-instruction-locations))
    (loop
      for current-instruction
        of-type Instruction
        across  program
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (if-instruction-p current-instruction) do
        (push current-position if-instruction-locations)
      else if (end-instruction-p current-instruction) do
        (if if-instruction-locations
          (let ((start-point (pop if-instruction-locations)))
            (declare (type fixnum start-point))
            (setf (gethash start-point jump-table) current-position))
          (error "Unmatched \"End\" instruction in line ~d."
            (1+ current-position)))
      end
      
      finally
        (when if-instruction-locations
          (setf if-instruction-locations
            (nreverse if-instruction-locations))
          (error "Unmatched \"If\" instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length if-instruction-locations)
            if-instruction-locations)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun locate-jump-destination (jump-table point-of-departure)
  "Returns the zero-based position of the \"End\" instruction allied
   with the \"If\" operation at the POINT-OF-DEPARTURE in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No corresponding \"End\" instruction detected ~
                for the \"If\" command at position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-pristine-memory ()
  "Creates and returns a ``memory'' instance in its original state."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun get-memory-cell-at (memory quesited-index)
  "Returns the value stored in the MEMORY cell amenable to the
   QUESITED-INDEX."
  (declare (type memory  memory))
  (declare (type integer quesited-index))
  (the integer
    (gethash quesited-index memory 0)))

;;; -------------------------------------------------------

(defun set-memory-cell-at (memory target-index new-value)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the TARGET-INDEX
   and returns no value."
  (declare (type memory  memory))
  (declare (type integer target-index))
  (declare (type integer new-value))
  (setf (gethash target-index memory) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of relational operators.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-relational-operator (operator
                                       left-operand
                                       right-operand)
  (:documentation
    "Applies the binary relational OPERATOR to the LEFT-OPERAND and
     RIGHT-OPERAND in this exact order, returning on its satisfaction a
     ``boolean'' value of ``T'', otherwise ``NIL''."))

;;; -------------------------------------------------------

(defmacro define-relational-operator
    (operator (left-operand-name right-operand-name)
     &body body)
  "Defines an implementation of the generic function
   ``apply-relational-operator'' primarily dispatching on its first
   argument, nevened automatically and ``eql''-specialized on the
   OPERATOR, employing as the second argument the agnomination supplied
   by the LEFT-OPERAND-NAME, specialized on an integer, and the third
   input constituting an specialization on an integer whose stevening
   is desumed from the RIGHT-OPERAND-NAME, evaluates the BODY forms,
   which are ensconced in an implicit ``progn'', and thilk converts the
   desinent BODY form's primary return value into a ``boolean''
   tantamount, returning this response."
  (let ((operator-name (gensym)))
    (declare (type symbol operator-name))
    `(defmethod apply-relational-operator
         ((,operator-name      (eql ,operator))
          (,left-operand-name  integer)
          (,right-operand-name integer))
       (declare (type relational-operator ,operator-name)
                (ignore                   ,operator-name))
       (declare (type integer             ,left-operand-name)
                (ignorable                ,left-operand-name))
       (declare (type integer             ,right-operand-name)
                (ignorable                ,right-operand-name))
       (the boolean
         (get-boolean-value-of
           (progn ,@body))))))

;;; -------------------------------------------------------

(define-relational-operator :equal-to (left-operand right-operand)
  (= left-operand right-operand))

;;; -------------------------------------------------------

(define-relational-operator :not-equal-to (left-operand right-operand)
  (/= left-operand right-operand))

;;; -------------------------------------------------------

(define-relational-operator :less-than (left-operand right-operand)
  (< left-operand right-operand))

;;; -------------------------------------------------------

(define-relational-operator :less-than-or-equal-to (left-operand
                                                    right-operand)
  (<= left-operand right-operand))

;;; -------------------------------------------------------

(define-relational-operator :greater-than (left-operand right-operand)
  (> left-operand right-operand))

;;; -------------------------------------------------------

(define-relational-operator :greater-than-or-equal-to (left-operand
                                                       right-operand)
  (>= left-operand right-operand))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operators.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-arithmetic-operator (operator
                                       left-operand
                                       right-operand)
  (:documentation
    "Applies the binary arithmetic OPERATOR to the LEFT-OPERAND and
     RIGHT-OPERAND in this exact order and returns the respective
     numeric result."))

;;; -------------------------------------------------------

(defmacro define-arithmetic-operator
    (operator (left-operand-name right-operand-name)
     &body body)
  "Defines an implementation of the generic function
   ``apply-arithmetic-operator'', the first formal parameter of which
   acquires its stevening in an automatic fashion and
   ``eql''-specializes on the OPERATOR, the second argument is yclept
   by the LEFT-OPERAND-NAME, specialized on an integer, the third input,
   iterum an integer number, enjoys the RIGHT-OPERAND-NAME's nominal
   dation, the implementation ensconcing the BODY forms in an implicit
   ``progn'', evaluating these, and returning the desinent form's
   results."
  (let ((operator-name (gensym)))
    (declare (type symbol operator-name))
    `(defmethod apply-arithmetic-operator
         ((,operator-name (eql ,operator))
          (,left-operand-name  integer)
          (,right-operand-name integer))
       (declare (type arithmetic-operator ,operator-name)
                (ignorable                ,operator-name))
       (declare (type integer             ,left-operand-name)
                (ignorable                ,left-operand-name))
       (declare (type integer             ,right-operand-name)
                (ignorable                ,right-operand-name))
       (the integer
         (progn ,@body)))))

;;; -------------------------------------------------------

(define-arithmetic-operator :plus (augend addend)
  (+ augend addend))

;;; -------------------------------------------------------

(define-arithmetic-operator :minus (minuend subtrahend)
  (- minuend subtrahend))

;;; -------------------------------------------------------

(define-arithmetic-operator :times (multiplier multiplicand)
  (* multiplier multiplicand))

;;; -------------------------------------------------------

(define-arithmetic-operator :divided (dividend divisor)
  (nth-value 0
    (round dividend divisor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &aux (jump-table (build-jump-table-for program)))))
  "The ``Interpreter'' class is apportioned the onus of accompassing
   actual efficacy to a Neucomp program specified as a sequence of
   instructions."
  (program      (error "Missing program.")
                :type      program
                :read-only T)
  (ip           0
                :type      integer
                :read-only NIL)
  (has-jumped-p NIL
                :type      boolean
                :read-only NIL)
  (jump-table   (error "Missing jump table.")
                :type      jump-table
                :read-only T)
  (memory       (prepare-pristine-memory)
                :type      memory
                :read-only T))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``program'' to the local
   symbol macro ``$program'', its ``ip'' to ``$ip'', the
   ``has-jumped-p'' flag to ``$has-jumped-p'', ``jump-table'' to
   ``$jump-table'', and ``memory'' to ``$memory'', evaluates the BODY
   forms, and returns the desinent form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter)
                (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($program
             (the program
               (interpreter-program ,evaluated-interpreter)))
            ($ip
             (the integer
               (interpreter-ip ,evaluated-interpreter)))
            ($has-jumped-p
             (the boolean
               (interpreter-has-jumped-p ,evaluated-interpreter)))
            ($jump-table
             (the jump-table
               (interpreter-jump-table ,evaluated-interpreter)))
            ($memory
             (the memory
               (interpreter-memory ,evaluated-interpreter))))
         (declare (type program    $program)
                  (ignorable       $program))
         (declare (type integer    $ip)
                  (ignorable       $ip))
         (declare (type boolean    $has-jumped-p)
                  (ignorable       $has-jumped-p))
         (declare (type jump-table $jump-table)
                  (ignorable       $jump-table))
         (declare (type memory     $memory)
                  (ignorable       $memory))
         ,@body))))

;;; -------------------------------------------------------

(defun go-to-line (interpreter target)
  "Relocates the INTERPRETER's instruction pointer (IP) to the one-based
   TARGET line index and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     target))
  (with-interpreter (interpreter)
    (psetf
      $ip           (1- target)
      $has-jumped-p T))
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances to the next instruction in the Neucomp program consigned to
   the INTERPRETER's castaldy."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (if $has-jumped-p
      (setf $has-jumped-p NIL)
      (incf $ip)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-matching-end-instruction (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to currently
   reside on an \"If\" operation, relocates thilk to the line of the
   matching \"End\" instruction and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (locate-jump-destination $jump-table $ip)))
  (values))

;;; -------------------------------------------------------

(defun program-has-completed-p (interpreter)
  "Determines whether the Neucomp program consigned to the INTERPRETER's
   castaldy has been halted, either because of its desinent
   instruction's patration, a relocation to an invalid line index, or an
   explicit \"Halt\" behest's actuation, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-interpreter (interpreter)
      (not (array-in-bounds-p $program $ip)))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction located at the INTERPRETER's current
   instruction pointer (IP) position."
  (declare (type Interpreter interpreter))
  (the Instruction
    (with-interpreter (interpreter)
      (aref $program $ip))))

;;; -------------------------------------------------------

(defun halt-program (interpreter)
  "Halts the Neucomp program consigned to the INTERPRETER's castaldy by
   a relocation of its instruction pointer (IP) outside of the
   admissible bournes and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (length $program)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    (instruction-class (interpreter-name instruction-name)
     &body body)
  "Defines an implementation of the generic function
   ``process-instruction'', its first formal parameter's agnomination
   constituting a verbatim appropriation of the INTERPRETER-NAME,
   generalizing on the ``Interpreter'' class, its second argument
   acquiring the stevening from the INSTRUCTION-NAME, generalizing, and
   ultimately dispatching, on the INSTRUCTION-CLASS, evaluates the BODY
   forms, and returns no value."
  `(defmethod process-instruction
       ((,interpreter-name  Interpreter)
        (,instruction-name ,instruction-class))
     (declare (type Interpreter        ,interpreter-name)
              (ignorable               ,interpreter-name))
     (declare (type ,instruction-class ,instruction-name)
              (ignorable               ,instruction-name))
     ,@body
     (values)))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Returns the value of the OPERAND as construed in the INTERPRETER's
     context.")
  
  (:method ((interpreter Interpreter)
            (operand     Literal-Operand))
    "Returns the literal OPERAND's value while ignoring the
     INTERPRETER."
    (declare (type Interpreter     interpreter)
             (ignorable            interpreter))
    (declare (type Literal-Operand operand))
    (the integer
      (literal-operand-value operand)))
  
  (:method ((interpreter Interpreter)
            (operand     Reference-Operand))
    "Returns the value of the cell in the INTERPRETER's memory amenable
     to the reference OPERAND's target."
    (declare (type Interpreter       interpreter)
             (ignorable              interpreter))
    (declare (type Reference-Operand operand))
    (the integer
      (with-interpreter (interpreter)
        (get-memory-cell-at $memory
          (resolve-operand interpreter
            (reference-operand-target operand)))))))

;;; -------------------------------------------------------

(defun query-cell-value (interpreter operand)
  "Returns the value stored in the INTERPRETER's memory cell designated
   by the OPERAND."
  (declare (type Interpreter interpreter))
  (declare (type Operand     operand))
  (the integer
    (with-interpreter (interpreter)
      (get-memory-cell-at $memory
        (resolve-operand interpreter operand)))))

;;; -------------------------------------------------------

(defun set-cell-value (interpreter operand new-value)
  "Stores the NEW-VALUE in the INTERPRETER's memory cell designated by
   the OPERAND and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Operand     operand))
  (declare (type integer     new-value))
  (with-interpreter (interpreter)
    (set-memory-cell-at $memory
      (resolve-operand interpreter operand)
      new-value))
  (values))

;;; -------------------------------------------------------

(defun if-instruction-is-satisfied-p (interpreter instruction)
  "Determines whether the \"If\" INSTRUCTION's antecedent, as probed in
   the INTERPRETER's context, shall be considered as satisfied,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter    interpreter))
  (declare (type If-Instruction instruction))
  (the boolean
    (apply-relational-operator
      (if-instruction-operator instruction)
      (query-cell-value interpreter
        (if-instruction-left-operand instruction))
      (query-cell-value interpreter
        (if-instruction-right-operand instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor ALU-Instruction (interpreter instruction)
  (set-cell-value interpreter
    (alu-instruction-target instruction)
    (apply-arithmetic-operator
      (alu-instruction-operator instruction)
      (query-cell-value interpreter
        (alu-instruction-left-operand instruction))
      (query-cell-value interpreter
        (alu-instruction-right-operand instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor End-Instruction (interpreter instruction))

;;; -------------------------------------------------------

(define-instruction-processor Go-Instruction (interpreter instruction)
  (go-to-line interpreter
    (resolve-operand interpreter
      (go-instruction-target instruction))))

;;; -------------------------------------------------------

(define-instruction-processor Halt-Instruction (interpreter instruction)
  (halt-program interpreter))

;;; -------------------------------------------------------

(define-instruction-processor If-Instruction (interpreter instruction)
  (unless (if-instruction-is-satisfied-p interpreter instruction)
    (jump-to-matching-end-instruction interpreter)))

;;; -------------------------------------------------------

(define-instruction-processor In-Instruction (interpreter instruction)
  (format T "~&>> ")
  (finish-output)
  (set-cell-value interpreter
    (in-instruction-target instruction)
    (char-code
      (read-char NIL NIL #\Null)))
  (clear-input))

;;; -------------------------------------------------------

(define-instruction-processor Let-Instruction (interpreter instruction)
  (set-cell-value interpreter
    (let-instruction-target instruction)
    (resolve-operand interpreter
      (let-instruction-source instruction))))

;;; -------------------------------------------------------

(define-instruction-processor Out-Instruction (interpreter instruction)
  (format T "~&~d~%"
    (query-cell-value interpreter
      (out-instruction-argument instruction)))
  (finish-output))

;;; -------------------------------------------------------

(define-instruction-processor Prt-Instruction (interpreter instruction)
  (format T "~c"
    (code-char
      (query-cell-value interpreter
        (prt-instruction-argument instruction))))
  (finish-output))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Neucomp program assigned to the INTERPRETER's
   bailiwick and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-completed-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Neucomp (code)
  "Interprets the piece of Neucomp source CODE and returns no value."
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

;; Store the value 65 in the memory cell amenable to the index 1 and
;; output its corresponding ASCII character, "A".
(interpret-Neucomp
  "Let 1 65
   Prt 1")

;;; -------------------------------------------------------

;; Print "Hello World".
(interpret-Neucomp
  "Let 1 72  #H
   Let 2 101 #e
   Let 3 108 #ll
   Let 4 111 #o
   Let 5 32  # 
   Let 6 87  #W
   Let 7 114 #r
   Let 8 100 #d
   Prt 1
   Prt 2
   Prt 3
   Prt 3
   Prt 4
   Prt 5
   Prt 6
   Prt 4
   Prt 7
   Prt 3
   Prt 8")

;;; -------------------------------------------------------

;; Cat program which gathers user input characters in subsequent
;; cells until a newline character has been issued, subsequently
;; printing these characters, ere terminating the program.
(interpret-Neucomp
  "Let 1 1
   Let 2 2
   Let 10 10
   ALU 2 1 + 2
   In Val 2
   If Val 2 = 10
     Go 10
   End
   Go 3
   Let 2 2
   ALU 2 1 + 2
   If Val 2 = 10
     Halt
   End
   Prt Val 2
   Go 11")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Neucomp
  "In 10
   Let 0 48
   Let 1 49
   If 10 = 0
     Prt 10
   End
   If 10 = 1
     Prt 10
     Go 8
   End")

;;; -------------------------------------------------------

;; "Looping counter" which counts from inclusive one (1) to inclusive
;; ten (10).
;; 
;; Memory layout:
;;   memory[0] <- 0 (employed during zero tests for iterations)
;;   memory[1] <- 1 (employed as a subtrahend for counting variables)
;;   memory[2] <- number of remaining lines to print (10 down to 0)
;;   memory[3] <- current line index (1 up to 10)
;;   memory[4] <- asterisk counter for current line (1 up to memory[3])
;;   memory[5] <- 42 (ASCII code for "*")
;;   memory[6] <- 10 (ASCII code for newline)
(interpret-Neucomp
  "
  Let 0  0
  Let 1  1
  Let 2 10
  Let 3  0
  Let 4  0
  Let 5 42
  Let 6 10
  
  # Line 8:
  If 2 = 0
    Halt
  End
  
  # Increment current line index.
  ALU 3 1 + 3
  # Adjust number of asterisks to print to conflate with line index.
  Let 4 Val 3
  
  # Line 13:
  If 4 > 0
    Prt 5
    ALU 4 1 - 4
    Go 13
  End
  
  Prt 6
  
  # Decrement number of remaining lines to print.
  ALU 2 1 - 2
  
  Go 8
  ")
