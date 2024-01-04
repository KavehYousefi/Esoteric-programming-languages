;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "EsoChan", invented by the Esolang user "SpaceByte" and
;; presented on July 21st, 2022, the format of which ostends a lealty
;; to the "story" design perpetuated inside of the imageboard "4chan",
;; where each line's incipiency is denoted by a closing angular bracket
;; (">"), an arrangement also stevened, by its conspicable tincture in
;; verd, as "green text" or "greentext".
;; 
;; 
;; Concept
;; =======
;; The Esolang programming language subscribes to the expression of its
;; programs in concord with the short 4chan stories format, operating
;; upon a set of integer-valued variables with a syntaxis approximately
;; natural in its exposition.
;; 
;; == ESOCHAN = [ESO]TERIC + 4[CHAN] ==
;; The EsoChan programming language's proprium derives from the diction
;; and formatting employed in the imageboard 4chan, more concretely, its
;; "green text" foundry for stories, a curtailed ilk of relation that
;; possesses the kenspeckle wont to introduce each of its lines, in
;; which the entirety of narration is found in its concameration, by a
;; closing angular bracket, ">", advancing in lealty within a purview of
;; tacitly agreed regulations.
;; 
;; == VARIABLES FURNISH THE PROGRAM MEMORY ==
;; The patration of the program's memory is realized in a set of
;; variables, pinioned by no bourne anenst their cardinality, and
;; admitted the apportionment of selected agnominations, any among these
;; unit's entrusted to a scalar signed integer's castaldy.
;; 
;; 
;; Syntax
;; ======
;; An EsoChan program's conformation proceeds by adminiculum of lines,
;; each such either a behest, introduced by the dextrally aligned
;; angular bracket ">" and succeeded by a single instruction whose donat
;; is desumed from an emulation of natural language, or if no parasceve
;; for efficacy is furnished, a construe for a comment.
;; 
;; == INSTRUCTIONS ==
;; A line commencing with the ">" symbol subjects to the interpretation
;; of a command, compact of one or more subsequent tokens in a mold akin
;; to natural language, contingently bearing zero or one argument, and
;; concluding with the end of the line.
;; 
;; Any line absent from the sentinel ">" is considered a commentary
;; extent.
;; 
;; == ARGUMENTS ==
;; Arguments may be of a liberal apportionment, either assuming a signed
;; or unsigned integer literal, a string, the same does not rely on
;; quotations for its marches' emphasis, or a variable identifier.
;; 
;; == COMMENTS ==
;; A line's destitution of a parasceuastic "greater than" (">") symbol
;; immediately disqualifies it from accompassing any causatum, and thus
;; renders such a horizontal expanse a commentary unit.
;; 
;; == GRAMMAR ==
;; A formulation of the language's donet in accordance with the Extended
;; Backus-Naur Form's (EBNF) regulations shall be proffered in the
;; following:
;; 
;;   program          := { innerCommandLine | innerCommentLine }
;;                    ,  [ lastCommandLine  | lastCommentLine  ]
;;   
;;   innerCommentLine := { character - ">" } , { character } , newline ;
;;   lastCommentLine  := { character - ">" } , { character } ;
;;   innerCommandLine := ">" , command , newline ;
;;   lastCommandLine  := ">" , command ;
;;   command          := askCommand
;;                    |  beCommand
;;                    |  freezeCommand
;;                    |  goCommand
;;                    |  gotoCommand
;;                    |  ifCommand
;;                    |  ifnotCommand
;;                    |  printCommand
;;                    |  setCommand
;;                    |  viewCommand
;;                    ;
;;   
;;   askCommand       := "ask" ;
;;   beCommand        := "be" , "program" ;
;;   freezeCommand    := "freeze" , "command" ;
;;   goCommand        := goDownCommand | goUpCommand | goZeroCommand ;
;;   goDownCommand    := "go" , "down" , integer ;
;;   goUpCommand      := "go" , "up" , integer ;
;;   goZeroCommand    := "go" , "0" , [ integer ] ;
;;   gotoCommand      := "goto" , ( integer | variable ) ;
;;   ifCommand        := "if" , integer ;
;;   ifnotCommand     := "ifnot" , integer ;
;;   printCommand     := printLinebreak | printVariable | printString ;
;;   printString      := "print" , { character - newline } ;
;;   printVariable    := "print" , "character" ;
;;   printLinebreak   := "print" , "linebreak" ;
;;   setCommand       := "set" , "it" , "to" , integer ;
;;   viewCommand      := "view" , variable ;
;;   
;;   variable         := variableChar , { variableChar } ;
;;   variableChar     := character - ( space | tab | newline ) ;
;;   space            := " "  ;
;;   tab              := "\t" ;
;;   newline          := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A tenfold cardinality governs the operative competences assigned to
;; EsoChan, its perimeter being such to enlist the creation and
;; modification of variables, basic arithmetics withal, input and output
;; communications, and several conditional and unconditional control
;; flow mechanisms.
;; 
;; == OVERVIEW ==
;; A basic and cursory grade of nortelry's communication shall be the
;; following apercu's dever.
;; 
;; Please heed that succedaneous segments are denoted by the adminicle
;; of an underline compact of asterisks ("*"), and intended to be
;; substituted by veridical EsoChan code in the actual program.
;; 
;;   ------------------------------------------------------------------
;;   Command              | Effect
;;   ---------------------+--------------------------------------------
;;   be program           | Activates, starts, or resumes the program,
;;                        | or, in the EsoChan language's diction,
;;                        | "un-freezes" the same.
;;                        |--------------------------------------------
;;                        | Please note that, at any program's
;;                        | inchoation, the program is inactive, or
;;                        | frozen", and thus irresponsive to any
;;                        | operation except for "be program", whose
;;                        | adhibition activates, or "un-freezes" the
;;                        | execution.
;;                        |--------------------------------------------
;;                        | The program can be temporarily deactivated
;;                        | by an invocation of the "freeze program"
;;                        | operation, and reactivated via an iterum
;;                        | "be program".
;;   ..................................................................
;;   freeze program       | Deactivates, or "freezes" or "re-freezes",
;;                        | the program, in which state all commands,
;;                        | except for the "be program" operation, are
;;                        | rendered ineffectual.
;;   ..................................................................
;;   view variableName    | If a variable designated by the
;;        ************    | {variableName} exists, it is selected as
;;                        | the highlighted variable; otherwise, if not
;;                        | extent, a new variable with this identifier
;;                        | is created, initialized to the default
;;                        | value of zero (0), and subsequently defined
;;                        | as the highlighted variable.
;;                        |--------------------------------------------
;;                        | The {variableName} must be a variable name.
;;   ..................................................................
;;   set it to value      | Stores the {value} in the highlighted
;;             *****      | variable.
;;                        |--------------------------------------------
;;                        | The {value} must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   go down amount       | Subtracts the {amount} from the value of
;;           ******       | the highlighted variable.
;;                        |--------------------------------------------
;;                        | The {amount} must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   go up amount         | Adds the {amount} to the value of the
;;         ******         | highlighted variable.
;;                        |--------------------------------------------
;;                        | The {amount} must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   go 0 amount          | Resets the highlighted variable's value to
;;        ******          | to zero (0).
;;                        |--------------------------------------------
;;                        | The {amount} is an optional argument; if
;;                        | specified, it must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   ask                  | Queries the standard input for a character
;;                        | and stores its ASCII code in the
;;                        | highlighted variable.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   print linebreak      | Prints a single newline character.
;;   ..................................................................
;;   print character      | Prints the character whose ASCII value
;;                        | corresponds to the highlighted variable's
;;                        | numeric value.
;;                        |--------------------------------------------
;;                        | Please note that no linebreak is appended
;;                        | to the output.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   print string content | Prints the {content}.
;;                ******* |--------------------------------------------
;;                        | The {content} embraces the entire rest of
;;                        | line up to but not including the newline or
;;                        | end of file (EOF) sentinel.
;;                        |--------------------------------------------
;;                        | Please note that no linebreak is appended
;;                        | to the output.
;;   ..................................................................
;;   goto lineNumber      | Relocates the instruction pointer (IP) to
;;        **********      | the line designated by the zero-based
;;                        | {lineNumber}.
;;                        |--------------------------------------------
;;                        | The {lineNumber} must either be a
;;                        | non-negative integer number or a variable
;;                        | name whose numeric value is construed as
;;                        | the destination line number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | the {lineNumber} references a variable and
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   if guard             | If the highlighted variable's value equals
;;      *****             | the {guard}, executes the subsequent
;;                        | command, otherwise skips the same.
;;                        |--------------------------------------------
;;                        | The {guard} must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ..................................................................
;;   ifnot guard          | If the highlighted variable's value does
;;         *****          | not equal the {guard}, executes the
;;                        | subsequent command, otherwise skips the
;;                        | same.
;;                        |--------------------------------------------
;;                        | The {guard} must be a signed or unsigned
;;                        | integer number.
;;                        |--------------------------------------------
;;                        | An error of the type
;;                        | "NoHighlightedVariableError" is signaled if
;;                        | at the instant of this operation's
;;                        | invocation no variable has been
;;                        | highlighted.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-30
;; 
;; Sources:
;;   [esolang2022EsoChan]
;;   The Esolang contributors, "EsoChan", July 21st, 2022
;;   URL: "https://esolangs.org/wiki/EsoChan"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member among which subscribes to the ELEMENT-TYPE, its
   default chosen as the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (flet ((matches-element-type-p (element)
            "Determines whether the ELEMENT adheres to the ELEMENT-TYPE,
             returning on confirmation a ``boolean'' value of ``T'',
             otherwise ``NIL''."
            (declare (type T element))
            (the boolean
              (not (null
                (typep element element-type))))))
      (setf (symbol-function predicate)
        #'(lambda (candidate)
            (declare (type T candidate))
            (and
              (listp candidate)
              (every #'matches-element-type-p
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which assumes the KEY-TYPE and answers to
   a value of the VALUE-TYPE, both in its default associated with the
   generic sentinel ``*''."
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
   species."
  '(member :eof :space :word))

;;; -------------------------------------------------------

(deftype parselet ()
  "The ``parselet'' type defines a partial parser, intended for the
   assemblage of a command from a token sequence, and realized as a
   niladic function which responds with either a command or the ``NIL''
   object."
  '(function () *))

;;; -------------------------------------------------------

(deftype parselet-table ()
  "The ``parselet-table'' type defines a mapping of EsoChan command
   identifiers to parselets, the former are realized as strings, while
   the latter assume a function form, nominally not further specified,
   as such type identification eludes the Common Lisp declaration
   system's faculties, whereas the conceptual form imputes a
   ``parselet'' target."
  '(hash-table-of string function))

;;; -------------------------------------------------------

(deftype command-name ()
  "The ``command-name'' type enumerates the recognized variations on
   EsoChan operations."
  '(member
    :be-program
    :freeze-program
    :view-variable
    :set-variable
    :increment-variable
    :decrement-variable
    :reset-variable
    :print-linebreak
    :print-variable
    :print-string
    :input-character
    :goto
    :if
    :ifnot))

;;; -------------------------------------------------------

(deftype argument-kind ()
  "The ``argument-kind'' type enumerates the recognized species of
   command arguments."
  '(member :integer :string :variable))

;;; -------------------------------------------------------

(deftype esochan-program ()
  "The ``esochan-program'' type defines an executable EsoChan program as
   a vector composed of zero or more ``Command'' objects."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype program-state ()
  "The ``program-state'' type enumerates the activity states of an
   EsoChan program or \"story\"."
  '(member :frozen :unfrozen))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines a mapping of variable names to
   their respective integer values, the same kithes itself in the form
   of a hash table, the keys of which maintain the identifiers,
   answering with the integral variable values."
  '(hash-table-of string integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Argument" class.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Argument
  (:constructor make-argument (kind value)))
  "The ``Argument'' class encapsulates the requisite information to
   replicate a command argument, amplecting the operand's species and
   its concrete value."
  (kind  (error "Missing kind.")  :type argument-kind)
  (value (error "Missing value.") :type (or integer string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Command" class.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (name &optional (argument NIL))))
  "The ``Command'' type furnishes a representation of an EsoChan
   command, identified in its effect by the name, and in its operation's
   support by an optional argument."
  (name     (error "Missing name.") :type command-name)
  (argument NIL                     :type (or null Argument)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (or null string) *line*))
(declaim (type fixnum           *line-length*))
(declaim (type fixnum           *row*))
(declaim (type fixnum           *column*))
(declaim (type character        *character*))
(declaim (type boolean          *end-of-line-p*))
(declaim (type token-type       *token-type*))
(declaim (type (or null string) *token*))

;;; -------------------------------------------------------

(defparameter *line* NIL
  "The contemporaneously processed line string.")

(defparameter *line-length* 0
  "The character tally comprising the *LINE*.")

(defparameter *row* 0
  "The zero-based index of the contemporaneously processed *LINE* among
   the entire program lines.")

(defparameter *column* 0
  "The zero-based index into the contemporaneously processed *LINE*,
   designating the attended character's location inwith the same.")

(defparameter *token-type* :eof
  "The type of the most requently acquired token from the *LINE*.")

(defparameter *token-value* NIL
  "The value of the most requently acquired token from the *LINE*.")

;;; -------------------------------------------------------

(define-symbol-macro *character*
  (the character
    (char *line* *column*)))

(define-symbol-macro *end-of-line-p*
  (the boolean
    (not (null
      (>= *column* *line-length*)))))

;;; -------------------------------------------------------

(defun on-character-p (expected-character)
  "Determines whether the *LINE*'s current *CHARACTER* equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (not (null
      (and (not *end-of-line-p*)
           (char= *character* expected-character))))))

;;; -------------------------------------------------------

(defun on-space-p ()
  "Determines whether the *LINE*'s current *CHARACTER* constitutes a
   space or horizontal tab, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (and (not *end-of-line-p*)
           (or (char= *character* #\Space)
               (char= *character* #\Tab)))))))

;;; -------------------------------------------------------

(defun advance-to-next-column ()
  "Advances the *COLUMN* cursor to the next position in the *LINE*, if
   possible, and returns no value."
  (setf *column*
    (min (1+ *column*) *line-length*))
  (values))

;;; -------------------------------------------------------

(defun read-next-token ()
  "Proceeding from the *COLUMN* into the *LINE*, returns the next token,
   if possible, otherwise ``NIL'', stores the response in the global
   *TOKEN-TYPE* and *TOKEN-VALUE* variables, and returns the latter."
  (cond
    (*end-of-line-p*
      (setf *token-type*  :eof)
      (setf *token-value* NIL))
    ((on-space-p)
      (setf *token-type*  :space)
      (setf *token-value*
        (with-output-to-string (token)
          (declare (type string-stream token))
          (loop while (on-space-p) do
            (write-char *character* token)
            (advance-to-next-column)))))
    (T
      (setf *token-type*  :word)
      (setf *token-value*
        (with-output-to-string (token)
          (declare (type string-stream token))
          (loop until (or *end-of-line-p* (on-space-p)) do
            (write-char *character* token)
            (advance-to-next-column))))))
  (the (or null string) *token-value*))

;;; -------------------------------------------------------

(defun token-matches-p (expected-token)
  "Determines whether the currently loaded *TOKEN* equals the
   EXPECTED-TOKEN, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string expected-token))
  (the boolean
    (not (null
      (and *token-value*
           (string= *token-value* expected-token))))))

;;; -------------------------------------------------------

(defun expect-word (expected-word)
  "Determines whether the *LINE*'s subsequent token replicates the
   EXPECTED-WORD, on confirmation returning the same, otherwise
   signaling an error of an unspecified type."
  (declare (type string expected-word))
  (the string
    (or
      (when (string= *token-value* expected-word)
        (prog1 *token-value*
          (read-next-token)))
      (error "Expected the word ~s, but encountered ~s."
        expected-word *token-value*))))

;;; -------------------------------------------------------

(defun read-rest-of-line ()
  "Proceeding from the *COLUMN* into the *LINE*, sets the *TOKEN-VALUE*
   to the entire remaining portion of the latter until its desinence as
   a string, returns this *TOKEN-VALUE*, and concomitantly empights the
   *COLUMN* cursor to the *LINE*'s tail."
  (setf *token-type* :word)
  (setf *token-value*
    (format NIL "~a"
      (subseq *line* *column*)))
  (setf *column* *line-length*)
  (the string *token-value*))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Proceeding from the current *COLUMN* into the *LINE*, skips a catena
   of zero or more accolent spaces or tabs and either returns the new
   *TOKEN*, or ``NIL'' upon the *LINE*'s exhaustion."
  (when (eq *token-type* :space)
    (loop
      while   (on-space-p)
      do      (advance-to-next-column)
      finally (read-next-token)))
  (the (or null string) *token-value*))

;;; -------------------------------------------------------

(defun expect-end-of-line ()
  "Determines whether, proceeding from the *COLUMN* into the *LINE*, the
   remaining characters do not bear any content, exempted from this
   prohibition being only spaces and horizontal tabs, on confirmation
   advancing the *COLUMN* cursor to the *LINE*'s desinent position,
   while returning no value, otherwise signaling an error of an
   unspecified type."
  (skip-spaces)
  (cond
    (*end-of-line-p*
      (setf *token-type*  :eof)
      (setf *token-value* NIL))
    (T
      (error "Expected no content to conclude the line, but encountered ~
              the character sequence ~s."
        (subseq *line* *column*))))
  (values))

;;; -------------------------------------------------------

(defun prepare-line (line)
  "Configures the lexer in order to accommodate the LINE's evaluation
   and returns no value."
  (declare (type string line))
  (setf *line*        line)
  (setf *line-length* (length line))
  (setf *column*      0)
  (incf *row*         1)
  (read-next-token)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type parselet-table *parselets*))

;;; -------------------------------------------------------

(defparameter *parselets*
  (make-hash-table :test #'equal)
  "Associates command names with responsible parselet functions.")

;;; -------------------------------------------------------

(defun register-parselet (identifier parselet)
  "Associates the command IDENTIFIER with the PARSELET in the
   *PARSELETS* table and returns no value."
  (declare (type string   identifier))
  (declare (type parselet parselet))
  (setf (gethash identifier *parselets*) parselet)
  (values))

;;; -------------------------------------------------------

(defun get-parselet (identifier)
  "Returns a parselet whose competence homologates its procession of the
   command designated by the IDENTIFIER, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type string identifier))
  (the function
    (or (gethash identifier *parselets*)
        (error "No parselet associated with ~s." identifier))))

;;; -------------------------------------------------------

(defun apply-parselet (identifier)
  "Invokes the parselet convenable to the command IDENTIFIER and returns
   the parsed ``Command'' instance."
  (declare (type string identifier))
  (the Command
    (funcall
      (get-parselet identifier))))

;;; -------------------------------------------------------

(defun parse-integer-argument ()
  "Parses the current *TOKEN-VALUE* as a signed or unsigned integer
   and returns an ``Argument'' encapsulation thereof, while
   concomitantly advancing to the next token on the *LINE*."
  (the Argument
    (prog1
      (make-argument :integer
        (parse-integer *token-value*))
      (read-next-token))))

;;; -------------------------------------------------------

(defun parse-variable-argument ()
  "Parses the current *TOKEN-VALUE* as a variable name and returns an
   ``Argument'' encapsulation thereof, while concomitantly advancing to
   the next token on the *LINE*."
  (the Argument
    (prog1
      (make-argument :variable *token-value*)
      (read-next-token))))

;;; -------------------------------------------------------

(defun parse-integer-or-variable-argument ()
  "Parses the current *TOKEN-VALUE* as a signed or unsigned integer or
   a variable name and returns an ``Argument'' encapsulation thereof,
   while concomitantly advancing to the next token on the *LINE*."
  (the Argument
    (handler-case
      (parse-integer-argument)
      (error ()
        (prog1
          (make-argument :variable *token-value*)
          (read-next-token))))))

;;; -------------------------------------------------------

(defun parse-line (line)
  "Parses the LINE and, if possible, returns a ``Command''
   representation of its behest, otherwise the ``NIL'' value."
  (declare (type string line))
  (prepare-line line)
  (skip-spaces)
  (the (or null Command)
    (when (token-matches-p ">")
      (expect-word ">")
      (skip-spaces)
      (apply-parselet *token-value*))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Parses the piece of EsoChan source CODE and returns a vector of its
   retrieved commands."
  (declare (type string code))
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (the esochan-program
      (coerce
        (loop
          for current-line
            of-type (or null string)
            =       (read-line code-stream NIL NIL)
          
          while current-line
          
          for current-command
            of-type (or null Command)
            =       (parse-line current-line)
          
          when current-command
            collect current-command)
        '(simple-array Command (*))))))

;;; -------------------------------------------------------

(register-parselet "ask"
  #'(lambda ()
      (expect-word "ask")
      (the Command
        (prog1
          (make-command :input-character)
          (expect-end-of-line)))))

;;; -------------------------------------------------------

(register-parselet "be"
  #'(lambda ()
      (expect-word "be")
      (skip-spaces)
      (expect-word "program")
      (expect-end-of-line)
      (the Command
        (make-command :be-program))))

;;; -------------------------------------------------------

(register-parselet "freeze"
  #'(lambda ()
      (expect-word "freeze")
      (skip-spaces)
      (expect-word "program")
      (expect-end-of-line)
      (the Command
        (make-command :freeze-program))))

;;; -------------------------------------------------------

(register-parselet "go"
  #'(lambda ()
      (expect-word "go")
      (skip-spaces)
      
      (the Command
        (cond
          ((token-matches-p "up")
            (read-next-token)
            (skip-spaces)
            (let ((amount (parse-integer-argument)))
              (declare (type Argument amount))
              (expect-end-of-line)
              (make-command :increment-variable amount)))
          
          ((token-matches-p "down")
            (read-next-token)
            (skip-spaces)
            (let ((amount (parse-integer-argument)))
              (declare (type Argument amount))
              (expect-end-of-line)
              (make-command :decrement-variable amount)))
          
          ((token-matches-p "0")
            (read-next-token)
            (skip-spaces)
            (if *end-of-line-p*
              (prog1
                (make-command :reset-variable)
                (expect-end-of-line))
              (let ((amount (parse-integer-argument)))
                (declare (type Argument amount))
                (prog1
                  (make-command :reset-variable amount)
                  (expect-end-of-line)))))
          
          (T
            (error "Invalid \"go\" argument: ~s." *token-value*))))))

;;; -------------------------------------------------------

(register-parselet "goto"
  #'(lambda ()
      (expect-word "goto")
      (skip-spaces)
      (the Command
        (prog1
          (make-command :goto
            (parse-integer-or-variable-argument))
          (expect-end-of-line)))))

;;; -------------------------------------------------------

(register-parselet "if"
  #'(lambda ()
      (expect-word "if")
      (skip-spaces)
      (the Command
        (prog1
          (make-command :if
            (parse-integer-argument))
          (expect-end-of-line)))))

;;; -------------------------------------------------------

(register-parselet "ifnot"
  #'(lambda ()
      (expect-word "ifnot")
      (skip-spaces)
      (the Command
        (prog1
          (make-command :ifnot
            (parse-integer-argument))
          (expect-end-of-line)))))

;;; -------------------------------------------------------

(register-parselet "print"
  #'(lambda ()
      (expect-word "print")
      (skip-spaces)
      
      (cond
        ((eq *token-type* :eof)
          (error "Missing \"print\" parameter."))
        
        ((token-matches-p "linebreak")
          (expect-end-of-line)
          (the Command
            (make-command :print-linebreak)))
        
        ((token-matches-p "character")
          (expect-end-of-line)
          (the Command
            (make-command :print-variable)))
        
        ((token-matches-p "string")
          (read-next-token)
          (the Command
            (make-command :print-string
              (make-argument :string
                (read-rest-of-line)))))
        
        (T
          (error "Invalid print type: ~s." *token-value*)))))

;;; -------------------------------------------------------

(register-parselet "set"
  #'(lambda ()
      (expect-word "set")
      (skip-spaces)
      (expect-word "it")
      (skip-spaces)
      (expect-word "to")
      (skip-spaces)
      (the Command
        (prog1
          (make-command :set-variable
            (parse-integer-argument))
          (expect-end-of-line)))))

;;; -------------------------------------------------------

(register-parselet "view"
  #'(lambda ()
      (expect-word "view")
      (skip-spaces)
      (the Command
        (prog1
          (make-command :view-variable
            (parse-variable-argument))
          (expect-end-of-line)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition No-Highlighted-Variable-Error (simple-error)
  ()
  (:default-initargs
    :format-control "No variable has been highlighted yet.")
  (:documentation
    "The ``No-Highlighted-Variable-Error'' condition type serves in the
     signaling of an anomalous situation instigated by the attempt to
     perquire or modify a program's highlighted, while such entity's
     specification eludes the system."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Variable-Registry" class.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Registry ()
  ((variables
    :initform      (make-hash-table :test #'equal)
    :type          variable-table
    :documentation "Associates the registered variable names with their
                    integral values.")
   (highlighted-variable
    :initform      NIL
    :type          (or null string)
    :documentation "Designates the currently highlighted variable by its
                    name, the same constitutes the key of its
                    representation in the VARIABLES hash table."))
  (:documentation
    "The ``Variable-Registry'' class attains the dever of a set of
     variables' castaldy, an accommodation whose foundry proceeds from
     an associative principle, associating the umabiguous identifiers
     with the variable values, while engaging in the concomitant
     supererogation that appertains to the highlighted member's
     attendance."))

;;; -------------------------------------------------------

(defun make-variable-registry ()
  "Creates and returns a new ``Variable-Registry''."
  (the Variable-Registry
    (make-instance 'Variable-Registry)))

;;; -------------------------------------------------------

(defun contains-variable-p (registry name)
  "Determines whether the variable REGISTRY entails a variable amenable
   to the NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Variable-Registry registry))
  (declare (type string            name))
  (with-slots (variables) registry
    (declare (type variable-table variables))
    (the boolean
      (not (null
        (nth-value 1
          (gethash name variables)))))))

;;; -------------------------------------------------------

(defun ensure-variable (registry name)
  "Ascertains the existency of a variable designated by the NAME in the
   variable REGISTRY by storing such with a default state of zero (0)
   upon its absence, otherwise inducing no causatum, in any case
   returning no value."
  (declare (type Variable-Registry registry))
  (declare (type string            name))
  (unless (contains-variable-p registry name)
    (with-slots (variables) registry
      (declare (type variable-table variables))
      (setf (gethash name variables) 0)))
  (values))

;;; -------------------------------------------------------

(defun variable-value (registry name)
  "Returns the value of the variable designated by the NAME in the
   REGISTRY, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Variable-Registry registry))
  (declare (type string            name))
  (with-slots (variables) registry
    (declare (type variable-table variables))
    (multiple-value-bind (variable-value contains-name-p)
        (gethash name variables)
      (declare (type (or null integer) variable-value))
      (declare (type T                 contains-name-p))
      (the integer
        (if contains-name-p
          variable-value
          (error "Unrecognized variable name: ~s." name))))))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value registry name)
  "Associates the NEW-VALUE with the variable designated by the NAME in
   the variable REGISTRY, on necessity preceded by its declaration and
   initialization to the default state of zero (0), and returns no
   value."
  (declare (type integer           new-value))
  (declare (type Variable-Registry registry))
  (declare (type string            name))
  (with-slots (variables) registry
    (declare (type variable-table variables))
    (setf (gethash name variables) new-value))
  (values))

;;; -------------------------------------------------------

(defun get-highlighted-variable (registry)
  "Returns the name of the highlighted variable in the REGISTRY, or
   ``NIL'' if none such exists."
  (declare (type Variable-Registry registry))
  (the (or null string)
    (slot-value registry 'highlighted-variable)))

;;; -------------------------------------------------------

(defun highlight-variable (registry name)
  "Defines the variable amenable to the NAME in the variable REGISTRY as
   the highlighted instance, contingently creating and initializing such
   to the default state of zero (0) if not yet present, and returns no
   value."
  (declare (type Variable-Registry registry))
  (declare (type string            name))
  (ensure-variable registry name)
  (with-slots (highlighted-variable) registry
    (declare (type (or null string) highlighted-variable))
    (setf highlighted-variable name))
  (values))

;;; -------------------------------------------------------

(defun highlighted-variable-value (registry)
  "Returns the valuee stored in the variable REGISTRY's highlighted
   variable, or signals an error of the type
   ``No-Highlighted-Variable-Error'' if none such exists."
  (declare (type Variable-Registry registry))
  (with-slots (highlighted-variable) registry
    (the integer
      (if highlighted-variable
        (variable-value registry highlighted-variable)
        (error 'No-Highlighted-Variable-Error)))))

;;; -------------------------------------------------------

(defun (setf highlighted-variable-value) (new-value registry)
  "Stores the NEW-VALUE in the variable REGISTRY's highlighted variable,
   if such exists, and returns no value, otherwise signals an error of
   the type ``No-Highlighted-Variable-Error''."
  (declare (type Variable-Registry registry))
  (with-slots (highlighted-variable) registry
    (if highlighted-variable
      (setf (variable-value registry highlighted-variable) new-value)
      (error 'No-Highlighted-Variable-Error)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Interpreter" class.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing EsoChan program.")
    :type          esochan-program
    :documentation "The EsoChan instructions to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) location in
                    the PROGRAM.")
   (jump-target
    :initform      NIL
    :type          (or null integer)
    :documentation "The explicitly specified goto destination, if such
                    has been requested, which, in the respective case,
                    supersedes the usual instruction pointer (IP)
                    behavior of a simple advancement to the next
                    position in the PROGRAM.")
   (program-state
    :initform      :frozen
    :type          program-state
    :documentation "Maintains the current activity state of the PROGRAM
                    or \"story\".")
   (variables
    :initform      (make-variable-registry)
    :type          Variable-Registry
    :documentation "Maps the registered variable names to their integral
                    values."))
  (:documentation
    "The ``Interpreter'' class applies itself to the assignment of
     effect to an EsoChan command sequence."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' dedicated to the evaluation
   of the EsoChan PROGRAM."
  (declare (type esochan-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (interpreter)
  "Relocates the INTERPRETER's instruction pointer to the next line in
   its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip jump-target) interpreter
    (declare (type fixnum            ip))
    (declare (type (or null integer) jump-target))
    (shiftf ip
      (if jump-target
        (prog1 jump-target
          (setf jump-target NIL))
        (1+ ip))))
  (values))

;;; -------------------------------------------------------

(defun go-to-line (interpreter destination-line)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   DESTINATION-LINE and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     destination-line))
  (with-slots (jump-target) interpreter
    (declare (type (or null integer) jump-target))
    (setf jump-target destination-line))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the EsoChan program governed by the INTERPRETER
   has been completed in its execution, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type esochan-program program))
    (declare (type fixnum          ip))
    (the boolean
      (not (null
        (>= ip (length program)))))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the currently processed command in the INTERPRETER's program,
   or signals an error of an unspecified type if the same is already
   been completed."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type esochan-program program))
    (declare (type fixnum          ip))
    (the Command
      (aref program ip))))

;;; -------------------------------------------------------

(defun program-frozen-p (interpreter)
  "Determines whether the program maintained by the INTERPRETER is
   frozen, or deactivated, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program-state) interpreter
    (declare (type program-state program-state))
    (the boolean
      (not (null
        (eq program-state :frozen))))))

;;; -------------------------------------------------------

(defun freeze-program (interpreter)
  "Freezes, or deactivates, the INTERPRETER's program and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (program-state) interpreter
    (declare (type program-state program-state))
    (setf program-state :frozen)))

;;; -------------------------------------------------------

(defun unfreeze-program (interpreter)
  "Unfreezes, or activates, the INTERPRETER's program and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (program-state) interpreter
    (declare (type program-state program-state))
    (setf program-state :unfrozen)))

;;; -------------------------------------------------------

(defgeneric dispatch-command (interpreter command-name command)
  (:documentation
    "Evaluates the COMMAND, dispatched on the COMMAND-NAME, in the
     INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defun process-command (interpreter command)
  "Processes the COMMAND in the INTERPRETER's context and returns no
   value.
   ---
   This operation administers no effort of its own, instead relaying its
   dever to the eligible generic function ``dispatch-command''
   implementation based upon the COMMAND's name."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (dispatch-command interpreter
    (command-name command)
    command)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :be-program))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (unfreeze-program interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :decrement-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (let ((amount (argument-value (command-argument command))))
        (declare (type integer amount))
        (decf (highlighted-variable-value variables) amount))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :freeze-program))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (freeze-program interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :goto))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (let ((destination (command-argument command)))
      (declare (type Argument destination))
      (case (argument-kind destination)
        (:integer
          (go-to-line interpreter
            (argument-value destination)))
        (:variable
          (go-to-line interpreter
            (highlighted-variable-value
              (argument-value destination))))
        (otherwise
          (error "Invalid goto destination: ~s." destination)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :if))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (let ((guard (argument-value (command-argument command))))
        (declare (type integer guard))
        (unless (= (highlighted-variable-value variables) guard)
          (advance-instruction-pointer interpreter)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :ifnot))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (let ((guard (argument-value (command-argument command))))
        (declare (type integer guard))
        (when (= (highlighted-variable-value variables) guard)
          (advance-instruction-pointer interpreter)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :increment-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (let ((amount (argument-value (command-argument command))))
        (declare (type integer amount))
        (incf (highlighted-variable-value variables) amount))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :input-character))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (setf (highlighted-variable-value variables)
        (prog2
          (finish-output)
          (char-code
            (read-char))
          (clear-input)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :print-linebreak))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (unless (program-frozen-p interpreter)
    (terpri))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :print-string))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (format T "~a"
      (argument-value
        (command-argument command))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :print-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (write-char
        (code-char
          (highlighted-variable-value variables)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :reset-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (declare (ignore            command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (setf (highlighted-variable-value variables) 0)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :set-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (setf (highlighted-variable-value variables)
        (argument-value
          (command-argument command)))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-command ((interpreter  Interpreter)
                             (command-name (eql :view-variable))
                             (command      Command))
  (declare (type Interpreter  interpreter))
  (declare (type command-name command-name))
  (declare (ignore            command-name))
  (declare (type Command      command))
  (unless (program-frozen-p interpreter)
    (with-slots (variables) interpreter
      (declare (type Variable-Registry variables))
      (highlight-variable variables
        (argument-value
          (command-argument command)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the EsoChan program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter))
    (advance-instruction-pointer interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-EsoChan (code)
  "Interprets the piece of EsoChan source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-EsoChan
  "> be program
   > view userInput
   > print string Please enter an ASCII character: 
   > ask
   > print linebreak
   > print character
   > print linebreak
   > goto 2")

;;; -------------------------------------------------------

;; Query the user for a character, determine whether the input
;; represents the minuscular letter "h", upon confirmation printing an
;; affirmative message, otherwise resort to a ludibund vitriol.
(interpret-EsoChan
  "
  > be program
  > print string CLICK THE COOLEST LETTER
  > view ascii
  > set it to 0
  > print linebreak
  > ask
  > print linebreak
  > ifnot 104
  > freeze program
  > print string WOW YOURE COOL YOU PRESSED h
  > print linebreak
  > print string ISNT THAT AMAZING
  > be program
  > if 104
  > freeze program
  > print string HOW DARE YOU. h IS CLEARLY THE COOLEST LETTER YOU SHALL BE EXECUTED FOR YOUR DISAGREEMENT
  > print linebreak
  > print string DIE
  > be program
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-EsoChan
  "
  > be program
  > view userInput
  
  > print string Please input 0 or 1:
  > print linebreak
  > ask
  
  Input of 0? => Jump to desinent line, which prints and terminats.
  > ifnot 49
  > goto 9
  
  Input of 1? => Print input, and repeat infinitely.
  > print character
  > goto 7
  
  > print character
  ")
