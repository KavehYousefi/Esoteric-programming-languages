;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "🕳️", presented by the Esolang user "Username1234" in the
;; year 2022, and focused upon input and output facilities expressed in
;; Unicode symbols.
;; 
;; 
;; Concept
;; =======
;; The 🕳️ programming language, nevend by a Unicode emoji signifying a
;; hole, employs several operations in order to capacitate input and
;; output transmissions, restricted to strings, tharfing, however, any
;; further competence.
;; 
;; 
;; Architecture
;; ============
;; 🕳️'s desistence for data storage, and its cynosure attached
;; constantly to input and output operations, excludes programs from the
;; adherence to any architectural deliberations.
;; 
;; 
;; Data Types
;; ==========
;; 🕳️ employs merely strings, of any length, for its data
;; representation.
;; 
;; 
;; Syntax
;; ======
;; The language's syntactical diorism is based upon single-character
;; commands of zero or one parameter and a solitary loop construct
;; expressed in braces ("{" and "}"), with literals constrained to
;; double-quoted strings.
;; 
;; The instruction set's subsumption into a categorical triad, namely
;; 
;;   (a) commands without argument
;;   (b) commands with one argument
;;   (c) code blocks
;; 
;; reverberates in its syntactical perspective.
;; 
;; Niladic operations (a) are composed of a single Unicode character
;; unambiguously identifying the construct.
;; 
;; The two unary operations' (b) structure conflates with the prior
;; species, extended, however, by a jumelle of asterisks, "*" and "*",
;; which, with immediacy following the name, ensconce an expression that
;; must resolve to a string in order to supply the requisite operand.
;; 
;; The aefauld exponent of the code block (c) ilk, the loop facility
;; amplects by two braces' adminiculum, "{" and "}", the statements to
;; repeatedly execute.
;; 
;; == STRINGS ==
;; A single specimen among the possible expressions affiliates with the
;; literal specification, manifested in strings whose demarcation by
;; a single twain of double quotes ('"' and '"') permits a sequence of
;; zero or more constituents.
;; 
;; == WHITESPACES ==
;; A contrast to the stringency governing whitespaces, which embrace the
;; space, horizontal tab and newline character, inside of an instruction
;; and its potential argument, as well as in the interstice, which
;; interdicts any occurrences outwith strings, their insertion
;; surrounding such operational units is delivered to one's personal
;; deliberations. This tolerance also holds for a loop's body,
;; demarcated by the braces, "{" and "}".
;; 
;; == COMMENTS ==
;; No provisions for comments are accommodated in this language
;; rendition.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation applies to
;; the language's donat:
;; 
;;   statementList        := { optionalSpaces | statement } ;
;;   statement            := inputToOutputCommand
;;                        |  outputCommand
;;                        |  loopCommand
;;                        |  expression
;;                        ;
;;   expression           := string
;;                        |  inputCommand
;;                        |  reverseCommand
;;                        ;
;;   inputToOutputCommand := "🕳️" ;
;;   inputCommand         := "❗" ;
;;   outputCommand        := "❓*" , expression , "*" ;
;;   reverseCommand       := "⚠️*" , expression , "*" ;
;;   loopCommand          := "{" , statementList , "}" ;
;;   string               := '"'
;;                        ,  { stringCharacter | escapedCharacter }
;;                        ,  '"' ;
;;   stringCharacter      := character - '"' ;
;;   escapedCharacter     := "\" , character ;
;;   optionalSpaces       := { whitespace } ;
;;   whitespace           := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The 🕳️ language's instruction set is compact of a quintuple of
;; members, bifurcating into a twain of expressions, themselves either
;; independently utile or homologated as arguments to other commands,
;; and a treble of statements desisting from returning data.
;; 
;; == OVERVIEW ==
;; An apercu shall adhibit a cursory mete of norterly concerning the
;; language's instructions. Please note that arguments, if a command's
;; constituents, are underlined by a sequence of carets ("^") as a
;; distinguishing warkloom from their surroundings. Any other content
;; intends its verbatim appropriation.
;; 
;;   ------------------------------------------------------------------
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   🕳️           | Queries the user for a line of input, prints the
;;                | same immediately to the standard output, and
;;                | returns no value.
;;   ..................................................................
;;   ❗            | Queries the user for a line of input and returns
;;                | the same as a string.
;;   ..................................................................
;;   ⚠️*string*   | Reverses and returns the {string}.
;;       ^^^^^^   |
;;   ..................................................................
;;   ❓*string*    | Prints the {string} to the standard output and
;;      ^^^^^^    | returns no value.
;;   ..................................................................
;;   {statements} | Infinitely executes the zero or more {statements}
;;    ^^^^^^^^^^  | and returns no value.
;;   ------------------------------------------------------------------
;; 
;; == PARAMETERS ==
;; Parameters always amount to string objects, either expressed in
;; direct terms, by literals' adminiculum, or through expressions. Three
;; distinct provenances can be identified for their obtention:
;; 
;;   (1) Literal strings, ensconced in double quotes ("...").
;;   (2) Strings obtained via user input, as in "🕳️" and "❗".
;;   (3) Strings obtained via expression evaluation, such as with "⚠️".
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in Common Lisp, employing for
;; epideictic purposes special variables as surrogates for classes and
;; parameters to functions.
;; 
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; Please remember that special variables, ligated into a consanguinity
;; with global variables as a general species, and exacerbated by their
;; implicit and contingently arbitrary declarations, merit the wite of
;; encumbering programs with superfluous complexity.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-16
;; 
;; Sources:
;;   [esolang2022🕳️]
;;   The Esolang contributors, "🕳️", 2022
;;   URL: "https://esolangs.org/wiki/%F0%9F%95%B3%EF%B8%8F"
;;   
;;   [emojipedia2023hole]
;;   Emojipedia, "🕳️ Hole", 2023
;;   URL: "https://emojipedia.org/hole/"
;;   Notes:
;;     - Describes the "hole" emoji "🕳️".
;;   
;;   [stackoverflow2012q41091118]
;;   The Stack Overflow contributors,
;;     "What's the canonical way to join strings in a list?", 2012
;;   URL: "https://stackoverflow.com/a/41091118"
;;   Notes:
;;     - Demonstrates the usance of special variables in the context of
;;       the ``format'' function.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype token (&optional (value-type T))
  "The ``token'' type represents a token as a significant object
   extracted from a piece of 🕳️ source code during the lexical
   analyzation procedure, compact of a categorizing type and a value for
   more detailed description, and manifesting in a cons that specifies
   the former as a keyword symbol, accompanied by the VALUE-TYPE, which
   defaults to the comprehensive ``T''."
  `(cons keyword ,value-type))

;;; -------------------------------------------------------

(deftype node ()
  "The ``node'' type defines an abstract syntax tree (AST) node as a
   cons whose left compartment represents the node type as a keyword
   symbol, followed by a right moeity as a representative of its
   optional attribute, potentially ``NIL''-valued  if unnecessary."
  '(cons keyword T))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``node''
   objects."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element 'node)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a new token categorized through the TYPE and
   described in more detail by the VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the token (cons type value)))

;;; -------------------------------------------------------

(defun token-type (token)
  "Returns the TOKEN's type."
  (declare (type token token))
  (the keyword (car token)))

;;; -------------------------------------------------------

(defun token-value (token)
  "Returns the TOKEN's value."
  (declare (type token token))
  (the T (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN belongs to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eof-p ()
  "Determines whether the lexer *SOURCE* is exhausted, signified by its
   *POSITION* cursor's transcendence of the proper bounds, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (special *source*))
  (declare (special *position*))
  (the boolean
    (not (array-in-bounds-p *source* *position*))))

;;; -------------------------------------------------------

(defun expect-character (expected-character)
  "Determines whether the *CURRENT-CHARACTER* in the *SOURCE* equals the
   EXPECTED-CHARACTER, on confirmation simply terminating while
   returning no value, otherwise signaling an error of an unspecified
   type."
  (declare (special *source*))
  (declare (special *position*))
  (cond
    ((eof-p)
      (error "Expected the character \"~c\" at position ~d, but
              encountered end of file (EOF)."
        expected-character *position*))
    ((char/= (char *source* *position*) expected-character)
      (error "Expected the character \"~c\" at position ~d, but
              encountered \"~c\"."
        expected-character *position*
        (char *source* *position*)))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun read-string ()
  "Commencing at the *CURRENT-POSITION* into the *SOURCE*, reads a
   string of arbitrary length ensconced in double quotes ('\"'), and
   returns a ``:string'' token representation thereof."
  (declare (special *source*))
  (declare (special *position*))
  ;; Skip introducing double quote ('"').
  (expect-character #\")
  (incf *position*)
  (the (token string)
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop do
          (cond
            ((eof-p)
              (error "Unterminated string at position ~d." *position*))
            ((char= (char *source* *position*) #\")
              (incf *position*)
              (loop-finish))
            ((char= (char *source* *position*) #\\)
              (incf *position*)
              (cond
                ((eof-p)
                  (error "Unterminated escape sequence at position ~d."
                    *position*))
                (T
                  (write-char (char *source* *position*) content)
                  (incf *position*))))
            (T
              (write-char (char *source* *position*) content)
              (incf *position*))))))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   on confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun matches-string-p (expected-string)
  "Commencing at the *CURRENT-POSITION* into the *SOURCE*, determines
   whether the consecutive characters replicate the EXPECTED-STRING, on
   confirmation empighting the *CURRENT-POSITION* at the location in the
   *SOURCE* immediately succeeding the matching portion, while returning
   the ``boolean'' value ``T'', otherwise simply returning ``NIL''
   without a position cursor movement."
  (declare (special     *source*))
  (declare (special     *position*))
  (declare (type string expected-string))
  (the boolean
    (let ((start-position *position*))
      (declare (type fixnum *position*))
      (loop
        for expected-character of-type character across expected-string
        do
          (cond
            ((eof-p)
              (setf *position* start-position)
              (return NIL))
            ((char/= (char *source* *position*) expected-character)
              (setf *position* start-position)
              (return NIL))
            (T
              (incf *position*)))
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun get-next-token ()
  "Returns the next token.
   ---
   Upon its source's exhaustion, each query is answered with a fresh
   end-of-file (EOF) token."
  (declare (special *source*))
  (declare (special *position*))
  (the token
    (cond
      ((eof-p)
        (make-token :EOF NIL))
      
      ((whitespace-character-p (char *source* *position*))
        (prog1
          (make-token :space (char *source* *position*))
          (incf *position*)))
      
      ((char= (char *source* *position*) #\{)
        (prog1
          (make-token :left-brace (char *source* *position*))
          (incf *position*)))
      
      ((char= (char *source* *position*) #\})
        (prog1
          (make-token :right-brace (char *source* *position*))
          (incf *position*)))
      
      ((char= (char *source* *position*) #\*)
        (prog1
          (make-token :asterisk (char *source* *position*))
          (incf *position*)))
      
      ((char= (char *source* *position*) #\")
        (read-string))
      
      ((matches-string-p "🕳️")
        (make-token :input-to-output "🕳️"))
      
      ((char= (char *source* *position*) #\❗)
        (prog1
          (make-token :input #\❗)
          (incf *position*)))
      
      ((matches-string-p "⚠️")
        (make-token :reverse "⚠️"))
      
      ((char= (char *source* *position*) #\❓)
        (prog1
          (make-token :output #\❓)
          (incf *position*)))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          (char *source* *position*) *position*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) node.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-node (type &optional (attribute NIL))
  "Creates and returns a new node, categorized by the TYPE, and
   optionally comprehending the ATTRIBUTE, which defaults to ``NIL''."
  (declare (type keyword type))
  (declare (type T       attribute))
  (the node (cons type attribute)))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the NODE's type."
  (declare (type node node))
  (the keyword
    (car node)))

;;; -------------------------------------------------------

(defun node-attribute (node)
  "Returns the NODE's attribute, if extant, or ``NIL'' upon its
   absence."
  (declare (type node node))
  (the T
    (cdr node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () node)      parse-expression))
(declaim (ftype (function () node-list) parse-statement-list))

;;; -------------------------------------------------------

(defun consume-token ()
  "Returns the *CURRENT-TOKEN*, while concomitantly querying the next
   one from the lexer and storing it in *CURRENT-TOKEN*."
  (declare (special *current-token*))
  (the token
    (prog1 *current-token*
      (setf *current-token*
        (get-next-token)))))

;;; -------------------------------------------------------

(defun expect-token (expected-token-type)
  "Determines whether the *CURRENT-TOKEN* matches the
   EXPECTED-TOKEN-TYPE, on confirmation returning this token, while
   concomitantly quering the next one from the lexer and storing it in
   *CURRENT-TOKEN*, otherwise signaling an error of an unspecified
   type."
  (declare (special      *current-token*))
  (declare (type keyword expected-token-type))
  (the token
    (prog1 *current-token*
      (if (token-type-p *current-token* expected-token-type)
        (setf *current-token* (get-next-token))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type *current-token*)))))

;;; -------------------------------------------------------

(defun parse-string ()
  "Parses a string and returns a ``:string'' node representation
   thereof."
  (declare (special *current-token*))
  (the node
    (make-node :string
      (token-value
        (consume-token)))))

;;; -------------------------------------------------------

(defun parse-input-to-output ()
  "Parses an input-to-output command, denoted by \"🕳️\", and returns an
   ``:input-to-output'' node representation thereof."
  (declare (special *current-token*))
  (expect-token :input-to-output)
  (the node
    (make-node :input-to-output)))

;;; -------------------------------------------------------

(defun parse-input ()
  "Parses an input command, denoted by \"❗\", and returns an ``:input''
   node representation thereof."
  (declare (special *current-token*))
  (expect-token :input)
  (the node
    (make-node :input)))

;;; -------------------------------------------------------

(defun parse-argument ()
  "Parses a statement argument, delimited by a jumelle of asterisks,
   \"*\" and \"*\", that amplects an aefauld expression, and returns a
   node representation thereof."
  (declare (special *current-token*))
  (expect-token :asterisk)
  (the node
    (prog1
      (parse-expression)
      (expect-token :asterisk))))

;;; -------------------------------------------------------

(defun parse-reverse-string ()
  "Parses a string reversal command, introduced by \"⚠️\", and returns
   a ``:reverse-string'' node representation thereof."
  (declare (special *current-token*))
  (expect-token :reverse)
  (the node
    (make-node :reverse-string
      (parse-argument))))

;;; -------------------------------------------------------

(defun parse-expression ()
  "Parses an aefauld expression and returns a node representation
   thereof."
  (declare (special *current-token*))
  (the node
    (case (token-type *current-token*)
      (:string
        (parse-string))
      (:input
        (parse-input))
      (:reverse
        (parse-reverse-string))
      (otherwise
        (error "No expression token: ~s." *current-token*)))))

;;; -------------------------------------------------------

(defun parse-output ()
  "Parses an output statement, introduced by \"❓\", and returns an
   ``:output'' node representation thereof."
  (declare (special *current-token*))
  (expect-token :output)
  (the node
    (make-node :output
      (parse-argument))))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Skips a sequence of zero or more accolent whitespace tokens and
   returns no value."
  (declare (special *current-token*))
  (loop while (token-type-p *current-token* :space) do
    (consume-token))
  (values))

;;; -------------------------------------------------------

(defun parse-loop ()
  "Parses an iteration construct, delimited by a jumelle of braces,
   \"{\" and \"}\", and returns a ``:loop'' node representation
   thereof."
  (declare (special *current-token*))
  (expect-token :left-brace)
  (skip-spaces)
  (let ((statements (parse-statement-list)))
    (declare (type node-list statements))
    (skip-spaces)
    (expect-token :right-brace)
    (the node
      (make-node :loop statements))))

;;; -------------------------------------------------------

(defun parse-statement ()
  "Parses an aefauld statement and returns a node representation
   thereof."
  (declare (special *current-token*))
  (the node
    (case (token-type *current-token*)
      (:input-to-output
        (parse-input-to-output))
      (:output
        (parse-output))
      (:left-brace
        (parse-loop))
      (otherwise
        (parse-expression)))))

;;; -------------------------------------------------------

(defun parse-statement-list ()
  "Parses zero or more statements, contingently preceded and/or
   separated by whitespaces, and returns a list encompassing the same in
   the order of their occurrences."
  (declare (special *current-token*))
  (the node-list
    (loop
      do (skip-spaces)
      
      if (token-type-p *current-token* :eof) do
        (loop-finish)
      else if (token-type-p *current-token* :right-brace) do
        (loop-finish)
      else
        collect (parse-statement))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Parses a program composed of zero or more statements and returns a
   ``:program'' node representation thereof."
  (declare (special *current-token*))
  (the node
    (prog1
      (make-node :program
        (parse-statement-list))
      (skip-spaces)
      (expect-token :eof))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-user-input ()
  "Queries the user for a line of input and returns it as a string."
  (format T "~&>> ")
  (the string
    (prog1
      (read-line)
      (clear-input))))

;;; -------------------------------------------------------

(defgeneric dispatch-node (node-type node)
  (:documentation
    "Evaluates the NODE, dispatched by its NODE-TYPE as the
     discriminating criterion, and returns a value appropriate for
     it."))

;;; -------------------------------------------------------

(defun visit-node (node)
  "Invokes the eligible ``dispatch-node'' by dispatching on the NODE's
   type and returns the result yielded by the method."
  (declare (type node node))
  (the (or null string)
    (dispatch-node (node-type node) node)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :program))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (dolist (statement (node-attribute node))
    (declare (type node statement))
    (visit-node statement))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :string))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (the string
    (node-attribute node)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :input-to-output))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (declare (ignore       node))
  (format T "~&~a"
    (get-user-input))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :input))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (declare (ignore       node))
  (the string
    (get-user-input)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :reverse-string))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (the string
    (reverse
      (visit-node
        (node-attribute node)))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :output))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (format T "~&~a"
    (visit-node
      (node-attribute node)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((node-type (eql :loop))
                          (node      cons))
  (declare (type keyword node-type))
  (declare (ignore       node-type))
  (declare (type node    node))
  (loop do
    (dolist (body-statement (node-attribute node))
      (declare (type node body-statement))
      (visit-node body-statement)))
  (values))

;;; -------------------------------------------------------

(defun interpret-🕳️ (code)
  "Interprets the piece of 🕳️ source CODE and returns no value."
  (declare (type string code))
  (let ((*source*   code)
        (*position* 0))
    (declare (type string *source*))
    (declare (special     *source*))
    (declare (type fixnum *position*))
    (declare (special     *position*))
    (let ((*current-token* (get-next-token)))
      (declare (type token *current-token*))
      (declare (special    *current-token*))
      (let ((tree (parse-program)))
        (declare (type node tree))
        (visit-node tree))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-🕳️ "❓*\"Hello, World!\"*")

;;; -------------------------------------------------------

;; Reverse the text "!dlroW ,olleH" and print it, yielding the message
;; "Hello, World!".
(interpret-🕳️ "❓*⚠️*\"!dlroW ,olleH\"**")

;;; -------------------------------------------------------

;; One-time cat program which profits from the dedicated input-to-output
;; instruction "🕳️".
(interpret-🕳️ "🕳️")

;;; -------------------------------------------------------

;; Infinite cat program which profits from the dedicated input-to-output
;; instruction "🕳️".
(interpret-🕳️ "{🕳️}")

;;; -------------------------------------------------------

;; One-time cat program which combines the separate output and input
;; operations for its purpose.
(interpret-🕳️ "❓*❗*")

;;; -------------------------------------------------------

;; Query the user for an input, reverse and print it.
(interpret-🕳️ "❓*⚠️*❗**")
