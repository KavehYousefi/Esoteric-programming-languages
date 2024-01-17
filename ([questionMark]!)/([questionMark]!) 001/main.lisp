;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "(?!)", invented by the Esolang user "MadMax" and presented
;; on May 11th, 2019, its dioristic proprium wones in the donat's
;; restriction to ecphonemes ("!"), erotemes ("?"), and parentheses
;; ("(" and ")"), the utility of which appertains to a non-negative
;; integer scalar's modification and output in character form.
;; 
;; 
;; Concept
;; =======
;; The (?!) programming language employs a quadruple instruction set,
;; the symbols of which constitute a derivation from its agnomination,
;; operating upon a non-negative integer, its telos the output of
;; messages.
;; 
;; == THE PROGRAM MEMORY: A SCALAR REGISTER ==
;; The program's data castaldy amounts to a single register, or, in an
;; alternative vista, a tape imbued an aefauld constituent's
;; participation, the same's capacity amplects a scalar non-negative
;; integer greater than or equal to zero.
;; 
;; 
;; Syntax
;; ======
;; The (?!) programming language's syntax subscribes to a very eath
;; exposition, wisting merely of a symbolic quartet for its operations'
;; instigation.
;; 
;; == INSTRUCTIONS ==
;; Operational constituents are expressed either in a single character
;; or as a parenthesized composition, the former specimen enumerates the
;; exclamation ("!") and the question mark ("?"), whereas the complex
;; second moeity imposes the left parenthesis ("(") and its concluding
;; peer, the right parenthesis (")").
;; 
;; == WHITESPACES ==
;; The insertion of whitespaces, a term whose perimeter tolerates the
;; simple space, the horizontal tab, and newline entities, registers a
;; question of one's personal delectation at any mete and spatiality.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) description shall augment the
;; syntaxis' treatise with improved formality:
;; 
;;   program          := { command , whitespaces } ;
;;   command          := incrementCommand
;;                    |  outputCommand
;;                    |  loopCommand
;;                    |  resetCommand
;;                    ;
;;   loopCommand      := "(" , loopBody , ")" ;
;;   loopBody         := { incrementCommand | loopCommand } ;
;;   incrementCommand := "?" ;
;;   outputCommand    := "!" ;
;;   resetCommand     := "(!)" ;
;;   whitespaces      := { whitespace } ;
;;   whitespace       := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; (?!)'s instruction set tallies a cardinality of four participants,
;; such entalented with the potentials to increment the register, reset
;; its state, output its content in an ASCII character form, and
;; administer an iterance for its value's multiplication.
;; 
;; == OVERVIEW ==
;; A basic grade of nortelry's adhibition shall be the following tabular
;; illustration's contribution.
;; 
;; Please heed that succedaneous segments are designated by an underline
;; compact of asterisks ("*"), their occurrencies of which ought to be
;; substituted by actual (?!) code in a program.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   ?        | Increments the register by one (1).
;;   ..................................................................
;;   !        | Prints the character whose ASCII code corresponds to
;;            | the register value to the standard output.
;;   ..................................................................
;;   (factor) | Multiplies the register value by the {factor}, that is,
;;    ******  | the number of incrementations, expressed via "?"
;;            | tokens, in the parenthesis twain "(" ... ")".
;;            |--------------------------------------------------------
;;            | The {factor} must be a sequence of zero or more "?"
;;            | and/or "(...)" commands.
;;            |--------------------------------------------------------
;;            | From a technical perspective, the contingency for
;;            | nesting imposes a hierarchical technique: Every loop
;;            | accommodates a context of its own, incorporating a
;;            | provisional accumulator, the value of which, if in a
;;            | nested iteration's compernage, determines the
;;            | multiplier, while the embedded loop's context
;;            | establishes the multiplicand. These echolons of
;;            | multiplications descend unto the permanent program
;;            | accumulator, which is multiplied, by the top-level
;;            | loop, in order to receive the ultimae factor.
;;            | 
;;            | A forbisen adduced, the following principle holds:
;;            | 
;;            |   ??? ( ?? ( ???? ) )
;;            |   (3  * (2 *  4))
;;            |   = 24
;;   ..................................................................
;;   (!)      | Resets the register value to zero (0).
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-13
;; 
;; Sources:
;;   [esolang2019(?!)]
;;   The Esolang contributors, "(?!)", May 29th, 2019
;;   URL: "https://esolangs.org/wiki/(%3F!)"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the
   default value being the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (flet ((matches-element-type-p (element)
                  "Determines whether the ELEMENT conforms to the
                   ELEMENT-TYPE, returning on confirmation a ``boolean''
                   value of ``T'', otherwise ``NIL''."
                  (declare (type T element))
                  (the boolean
                    (not (null
                      (typep element element-type))))))
            (and
              (listp candidate)
              (every #'matches-element-type-p
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype token-type ()
  "The ``token-type'' type enumerates the admissive categories of
   tokens."
  '(member
    :ecphoneme
    :eof
    :eroteme
    :left-parenthesis
    :right-parenthesis))

;;; -------------------------------------------------------

(deftype command-list ()
  "The ``command-list'' type defines a list composed of zero or more
   ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer datum greater
   than or equal to zero (0), bourneless along the upper march, and as
   thus a occupant of the interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a stack composed of zero or more
   non-negative integer numbers."
  '(list-of non-negative-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Token" class.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token     (type value))
  (:constructor make-eof-token (&aux (type :eof) (value NIL))))
  "The ``Token'' class serves in the encapsulation of a significant
   object extracted during a piece of (?!) source code's lexical
   analyzation."
  (type  (error "Missing token type.")  :type token-type :read-only T)
  (value (error "Missing token value.") :type T          :read-only T))

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

(defmacro token-case (probed-token &rest cases)
  "Evaluates the PROBED-TOKEN and employs its token type as the object
   for the contingent selection of the first covenable among the CASES,
   where each case is a list compact of one or more elements, the first
   imposing the key to juxtapose the probed token type with, the
   remaining items supplying the forms to execute upon the case's
   admission, with the desinent one's results to be returned.
   ---
   If none of the CASES ostends covenableness, no further causatum
   applies, and the ``NIL'' value constitutes the macro's response.
   ---
   The syntax of ``token-case'' follows, and is edified upon, Common
   Lisp's autochthonous ``case'' macro, such that the sentinel keys
   ``otherwise'' and ``T'' also may partake of the CASES' specification
   in order to handle the default case."
  (let ((evaluated-token (gensym)))
    (declare (type symbol evaluated-token))
    `(let ((,evaluated-token ,probed-token))
       (declare (type Token ,evaluated-token))
       (case (token-type ,evaluated-token)
         ,@cases))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   on confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Lexer" class.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class applies itself to a piece of (?!) source code's
   lexical analyzation, it pursuit's telos that to extract and deliver
   the incorporated tokens."
  (source    (error "Missing lexer source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slot ``source'' to a local symbol
   macro ``$source'', ``position'' to ``$position'', and ``character''
   to ``$character'', evaluates the BODY forms, and returns the desinent
   form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (symbol-macrolet
           (($source
             (the string
               (lexer-source ,evaluated-lexer)))
            ($position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            ($character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         (flet
             ((advance-lexer ()
               "Advances the LEXER's position cursor to the next
                character in it source, if possible, and returns no
                value."
               (setf $character
                 (when (array-in-bounds-p $source (1+ $position))
                   (char $source
                     (incf $position))))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent whitespace characters and returns
   no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and $character (whitespace-character-p $character)) do
      (advance-lexer)))
  (values))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Reads the LEXER's currnt character, advances its position cursor to
   the next location in its source, and returns a new token whose type
   derives from the TOKEN-TYPE and whose value assumes the recently
   consumed character."
  (declare (type Lexer      lexer))
  (declare (type token-type token-type))
  (with-lexer (lexer)
    (the Token
      (make-token token-type
        (prog1 $character
          (advance-lexer))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null $character)
          (make-eof-token))
        
        ((whitespace-character-p $character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        
        ((char= $character #\?)
          (read-symbol lexer :eroteme))
        
        ((char= $character #\!)
          (read-symbol lexer :ecphoneme))
        
        ((char= $character #\()
          (read-symbol lexer :left-parenthesis))
        
        ((char= $character #\))
          (read-symbol lexer :right-parenthesis))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            $character $position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface establishes a common foundry for all
   classes whose dever imposes the representation of a (?!) operation.")

;;; -------------------------------------------------------

(defstruct (Increment-Command
  (:include Command))
  "The ``Increment-Command'' class serves in the representation of the
   (?!) \"?\" operation, its causatum resolves to the accumulator's
   incrementation by a magnitude of one (1).")

;;; -------------------------------------------------------

(defstruct (Output-Command
  (:include Command))
  "The ``Output-Command'' class serves in the representation of the
   (?!) \"~\" operation, its causatum resolves to the accumulator's
   printing in the form of the character whose ASCII code conflates with
   the storage's state.")

;;; -------------------------------------------------------

(defstruct (Reset-Command
  (:include Command))
  "The ``Reset-Command'' class serves in the representation of the (?!)
   \"(!)\" operation, a macro whose contribution wones in the
   accumulator value's relapse to the incipial state of zero (0).")

;;; -------------------------------------------------------

(defstruct (Loop-Command
  (:include Command))
  "The ``Loop-Command'' class serves in the representation of the (?!)
   \"(...)\" operation, the language's aefauld iterance construct, the
   effect of which is realized in the execution of a sequence of zero or
   more incrementation (\"?\") or loop (\"(...)\") operations by a
   cycle tally tantamount to the accumulator value at the iteration
   command's invocation."
  (statements NIL :type command-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Parser" class.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (lexer
                             &aux (current-token
                                    (get-next-token lexer)))))
  "The ``Parser'' class' is apportioned that onus which allocates its
   bailiwick to the assemblage of an executable (?!) program from the
   sequence of tokens."
  (lexer         (error "Missing lexer for the parser.")
                 :type      Lexer
                 :read-only T)
  (current-token (make-eof-token)
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slot ``lexer'' to the local symbol
   macro ``$lexer'' and its slot ``current-token'' to
   ``$current-token'', executes the BODY forms, and returns the desinent
   form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (symbol-macrolet
           (($lexer
              (the Lexer
                (parser-lexer ,evaluated-parser)))
            ($current-token
              (the Token
                (parser-current-token ,evaluated-parser))))
         (declare (type Lexer $lexer))
         (declare (ignorable  $lexer))
         (declare (type Token $current-token))
         (declare (ignorable  $current-token))
         (flet
             ((eat-token (expected-token-type)
               (declare (type token-type expected-token-type))
               (if (token-type-p $current-token expected-token-type)
                 (setf $current-token
                   (get-next-token $lexer))
                 (error "Expected a token of the type ~s, ~
                         but encountered ~s."
                   expected-token-type $current-token))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun parse-increment-command (parser)
  "Parses an increment command (\"?\") in the PARSER's context and
   returns an ``Increment-Command'' encapsulation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-token :eroteme)
    (the Increment-Command
      (make-increment-command))))

;;; -------------------------------------------------------

(defun parse-output-command (parser)
  "Parses an output command (\"!\") in the PARSER's context and returns
   an ``Output-Command'' encapsulation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-token :ecphoneme)
    (the Output-Command
      (make-output-command))))

;;; -------------------------------------------------------

(defun parse-reset-command (parser)
  "Parses an reset command (\"(!)\") in the PARSER's context and returns
   a ``Reset-Command'' encapsulation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat-token :ecphoneme)
    (eat-token :right-parenthesis)
    (the Reset-Command
      (make-reset-command))))

;;; -------------------------------------------------------

(defun parse-loop-body (parser)
  "Expected to its commorancy to be in a loop body's viscerals, past the
   opening parenthesis, parses the iteration construct's body in the
   PARSER's context and returns a ``Command'' representation of its
   content."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Loop-Command
      (make-loop-command :statements
        (prog1
          (loop
            until (token-type-p $current-token :right-parenthesis)
            collect
              (token-case $current-token
                ;; Missing ")". => Unterminated loop body.
                (:eof
                  (error "Unterminated loop body."))
                ;; Invalid output or reset command in loop.
                (:ecphoneme
                  (error "Neither the reset or the output command may ~
                          appear in a loop."))
                ;; Increment command.
                (:eroteme
                  (parse-increment-command parser))
                ;; Nested loop.
                (:left-parenthesis
                  (eat-token :left-parenthesis)
                  (parse-loop-body parser))
                (otherwise
                  (error "Invalid token in loop body: ~s."
                    $current-token))))
          (eat-token :right-parenthesis))))))

;;; -------------------------------------------------------

(defun parse-command (parser)
  "Parses a single command in the PARSER's context and returns a
   ``Command'' encapsulation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Command
      (token-case $current-token
        (:eroteme
          (parse-increment-command parser))
        (:ecphoneme
          (parse-output-command parser))
        ;; Top-level loop or reset command.
        (:left-parenthesis
          (eat-token :left-parenthesis)
          (token-case $current-token
            (:eof
              (error "Unterminated opening parenthesis."))
            ;; (!)
            (:ecphoneme
              (parse-reset-command parser))
            (otherwise
              (parse-loop-body parser))))
        (otherwise
          (error "No statement token: ~s." $current-token))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a (?!) program and returns an ordered list of its encountered
   commands."
  (declare (type Parser parser))
  (with-parser (parser)
    (the command-list
      (loop until (token-type-p $current-token :eof) collect
        (parse-command parser)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter ()))
  "The ``Interpreter'' class is embued with responsibility of
   encapsulating the (?!) program state, most prominently providing adit
   to its accumulator via stack of non-negative integers, the same
   accommodates the potentials for nested iterations."
  (accumulator-stack (list 0) :type stack :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``accumulator-stack'' to
   the local symbol macro ``$accumulator-stack'', executes the BODY
   forms, and returns the desinent form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (symbol-macrolet
           (($accumulator-stack
             (the stack
               (interpreter-accumulator-stack ,evaluated-interpreter))))
         (declare (type stack $accumulator-stack))
         (declare (ignorable  $accumulator-stack))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the (?!) COMMAND in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter) (command Increment-Command))
    (declare (type Interpreter       interpreter))
    (declare (type Increment-Command command))
    (declare (ignore                 command))
    (with-interpreter (interpreter)
      (incf (first $accumulator-stack)))
    (values))
  
  (:method ((interpreter Interpreter) (command Output-Command))
    (declare (type Interpreter    interpreter))
    (declare (type Output-Command command))
    (declare (ignore              command))
    (with-interpreter (interpreter)
      (write-char
        (code-char
          (first $accumulator-stack))))
    (values))
  
  (:method ((interpreter Interpreter) (command Reset-Command))
    (declare (type Interpreter   interpreter))
    (declare (type Reset-Command command))
    (declare (ignore             command))
    (with-interpreter (interpreter)
      (setf (first $accumulator-stack) 0))
    (values))
  
  (:method ((interpreter Interpreter) (command Loop-Command))
    (declare (type Interpreter  interpreter))
    (declare (type Loop-Command command))
    (with-interpreter (interpreter)
      (let ((body (loop-command-statements command)))
        (declare (type command-list body))
        (push 0 $accumulator-stack)
        (dolist (statement body)
          (declare (type Command statement))
          (process-command interpreter statement))
        (let ((multiplicand (pop $accumulator-stack)))
          (declare (type non-negative-integer multiplicand))
          (unless (zerop multiplicand)
          (setf (first $accumulator-stack)
            (* (first $accumulator-stack) multiplicand))))))
    (values)))

;;; -------------------------------------------------------

(defun interpret-program (interpreter program)
  "Interpretes the (?!) PROGRAM in the INTERPRETER's context and returns
   no value."
  (declare (type Interpreter  interpreter))
  (declare (type command-list program))
  (dolist (command program)
    (declare (type command command))
    (process-command interpreter command))
  (values))

;;; -------------------------------------------------------

(defun interpret-|(?!)| (code)
  "Interprets the piece of (?!) source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter)
    (parse-program
      (make-parser
        (make-lexer code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "HELLO WORLD!".
(interpret-|(?!)| "???????(??????????)??!(!) ?????????????(?????)????!(!) ???????????????(?????)?!(!) ???????????????(?????)?!(!) ???????????????(?????)????!(!) ??????????(???)??!(!) ?????????????????(?????)??!(!) ???????????????(?????)????!(!) ??????????(????????)??!(!) ???????????????(?????)?!(!) ?????????????(?????)???!(!) ??????????(???)???!(!)")

;;; -------------------------------------------------------

;; Employ nested iterations in order to replicate the letter "A", the
;; ASCII code of which amounts to 65.
(interpret-|(?!)| "??(???(??(?????)))?????!")
