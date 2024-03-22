;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Dots", invented by the Esolang user "EverythingEli" and
;; presented on September 12th, 2020, the chief constituents of whose
;; programs are contributed by the punctuation characters dot ("."),
;; colon (":"), and semicolon (";"), begetting a mere triad in
;; competences in the printing of text, the definition of variables, and
;; the program's imperative termination.
;; 
;; 
;; Concept
;; =======
;; The Dots programming language's kenspeckle attribute wones in the
;; deployment of dots, colons, and semicolons for the arrangement of its
;; programs, permitting the printing of text, the establishment of
;; variables, and the program's halting, which, as a mandatory
;; constituent, ought to be transpire at the command sequence's
;; desinence.
;; 
;; 
;; Syntax
;; ======
;; A Dots program constitutes a composition of zero or more
;; instructions, everichon of whose framework being established by a
;; subset of punctuation characters, namely dots (".") for an
;; operation's identification, colons (":") as an argument designator,
;; and semicolons (";") with the purpose of the command's conclusion.
;; Whitespaces betwixt such compounds are tolerated and ignored.
;; 
;; == INSTRUCTIONS ==
;; The three available operation's recognition and differentiation
;; registers a dependency upon a sequence of one or more accolent dots
;; ("..."), succeeded in their designment by zero through three
;; arguments, and always concluding in a semicolon (";").
;; 
;; == ARGUMENTS ==
;; An argument's introduction, as well as its segregation from its peer,
;; proceeds by adminiculum of a single colon (":"), whence transpires
;; either a variable identifier or a string.
;; 
;; == STRINGS ==
;; Strings assume a design composed of zero or more character, whose
;; diorism does not embrace the special significant established in the
;; semicolon (";"). No further demarcation, such as double quotation
;; marks, participates in their specification.
;; 
;; == VARIABLE IDENTIFIERS ==
;; A variable name's componency enumerates any graphical character
;; except the space and the treble of signification constituents
;; manifesting in the dot ("."), colon (":"), and semicolon (";").
;; 
;; == WHITESPACES ==
;; Their occupancy betwixt and around instructions is adhibited
;; tolerance for whitespaces, which mentions in its perimeterr the
;; space, horizontal tab, and newline characters; counterdistinguished
;; therefrom, such adscititious content, except for their ipsissima
;; verba assumption in strings, does not enjoy the same mete of
;; patience.
;; 
;; == COMMENTS ==
;; The current Dots language iteration does not accommodate any
;; contingencies for commentary additions.
;; 
;; == GRAMMAR ==
;; The language's donat shall be imbued with enhanced formality by an
;; Extended Backus-Naur Form (EBNF) specification:
;; 
;;   program        := { command | padding } , halt , padding ;
;;   command        := halt | print | defineVariable ;
;;   halt           := ".;" ;
;;   print          := ".." , ( identifier | stringLiteral ) , ";" ;
;;   defineVariable := "..."
;;                  ,  ":" , identifier
;;                  ,  ":" , stringLiteral
;;                  ,  ";"
;;                  ;
;;   stringLiteral  := { character - reservedSymbol } ;
;;   identifier     := identifierChar , { identifierChar } ;
;;   identifierChar := character - ( whitespace | reservedSymbol ) ;
;;   reservedSymbol := "." | ":" | ";" ;
;;   padding        := { whitespace } ;
;;   whitespace     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Dots's facilities enumerates a mere treble of participants, such are
;; entrusted with the definition of variables with string contents, the
;; printing of literal or variable messages, and a halt instruction, the
;; presence of which at the program's desinent position imposes an
;; imperative.
;; 
;; The number of constitution of the arguments to any such operation
;; derives from the concrete specimen.
;; 
;; == OVERVIEW ==
;; The Dots language's operative competences shall experience a cursory
;; treatise in the following tabulation.
;; 
;; Please heed that succedaneous segments are underlined via a catena of
;; asterisks ("*"), and expected to be superseded by actual Dots code in
;; the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command            | Effect
;;   -------------------+----------------------------------------------
;;   .;                 | Immediately terminates the program.
;;                      |----------------------------------------------
;;                      | This instruction must constitute the desinent
;;                      | operation in the program.
;;                      |----------------------------------------------
;;                      | If the program lacks this command at its
;;                      | final position, an error of the type
;;                      | "MissingHaltCommandError" is signaled.
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   ..:content;        | Prints the {content} to the standard output.
;;      *******         |----------------------------------------------
;;                      | The {content} must be a sequence of zero or
;;                      | more characters, except for the semicolon,
;;                      | ";". If a valid variable identifier, and
;;                      | responding to a previously defined variable,
;;                      | its value is inserted; otherwise the verbatim
;;                      | character sequence is utilized.
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   ...:varName:value; | Defines a variable amenable to the name
;;       ******* *****  | {varName}, containing the object {value}.
;;                      | If a variable by this name already exists,
;;                      | its content is tacitly superseded.
;;                      |----------------------------------------------
;;                      | {varName} must be a valid variable
;;                      | identifier, that is, a sequence of one or
;;                      | more character except for:
;;                      |   - whitespaces (space, tab, and newline)
;;                      |   - the special symbol dot (".")
;;                      |   - the special symbol colon (":")
;;                      |   - the special symbol semicolon (";").
;;                      |----------------------------------------------
;;                      | If {varName} diverges from the valid name
;;                      | conformation, an error of the type
;;                      | "InvalidVariableNameError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-21
;; 
;; Sources:
;;   [esolang2022Dots]
;;   The Esolang contributors, "Dots", August 6th, 2022
;;   URL: "https://esolangs.org/wiki/Dots"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype unsigned-integer ()
  "The ``unsigned-integer'' type defines an integral object whose
   lower inclusive march is imposed by zero (0), but which does not
   install a natural upper extremum, by which diorism its occupancy
   resolves to the integer range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype command-kind ()
  "The ``command-type'' type enumerates the recognized variants of Dots
   instructions."
  '(member :halt :print :define-variable))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for the
   same holds the default species of ``T''."
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
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each key of which conforms to the
   KEY-TYPE and associates with a value of the VALUE-TYPE, for both
   holds the default species of ``T''."
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

(deftype program ()
  "The ``program'' type defines an executable Dots program as a list of
   zero or more ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   ilk of which includes, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean tantamount to the OBJECT, a more specific
   expression of the \"generalized boolean\" concept, responding with
   ``T'' for a non-``NIL'' OBJECT, otherwise returning ``NIL'' for a
   ``NIL'' input."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun variable-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for a
   variable name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and (graphic-char-p candidate)
           (not (whitespace-character-p candidate))
           (not (find candidate ".:;" :test #'char=))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (kind &rest arguments)))
  "The ``Command'' class serves in the encapsulation of a Dots
   instruction, compact of its categorizing kind and a list of zero or
   more requisite operands."
  (kind      (error "Missing command kind.")
             :type      command-kind
             :read-only T)
  (arguments (error "Missing command arguments.")
             :type      (list-of string)
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Dots-Error ()
  ()
  (:documentation
    "The ``Dots-Error'' condition type serves as a common foundry for
     all concrete species of anomalous communication media appertaining
     to a Dots program's lexical analysis, parsing, or evaluation."))

;;; -------------------------------------------------------

(define-condition Invalid-Variable-Name-Error (Dots-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        invalid-variable-name-error-offending-name
    :type          string
    :documentation "The invalid identifier which lacked in concinnity
                    with a respected variable name."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Variable-Name-Error condition))
      (declare (type destination                 stream))
      (format stream "The identifier ~s does not constitute a ~
                      valid variable name."
        (invalid-variable-name-error-offending-name condition))))
  (:documentation
    "The ``Invalid-Variable-Name-Error'' condition type serves in the
     communication of an anomalous situations begotten by the attempt to
     define a variable whose name does not follow the conformant
     stipulations."))

;;; -------------------------------------------------------

(define-condition Missing-Halt-Command-Error (Dots-Error)
  ((offended-program
    :initarg       :offended-program
    :initform      (error "Missing offended program.")
    :reader        missing-halt-command-error-offended-program
    :type          program
    :documentation "The Dots malformed instruction sequence which lacks
                    the concluding halt instruction (\".;\")."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Halt-Command-Error condition))
      (declare (type destination                stream))
      (format stream "The Dots program ~s does not terminate in a ~
                      halting \".;\" instruction."
        (missing-halt-command-error-offended-program condition))))
  (:documentation
    "The ``Missing-Halt-Command-Error'' condition type serves in the
     signaling of an anomalous situation whose etiology accounts for a
     missing halt instruction (\".;\") at the program's desinent
     position."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-position-p (source position)
  "Determines whether the POSITION constitutes a valid index into the
   SOURCE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p source position))))

;;; -------------------------------------------------------

(defun character-at-equals-p (source position expected-character)
  "Determines whether the character at the POSITION into the SOURCE
   exists and equals the EXPECTED-CHARACTER, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the boolean
    (get-boolean-value-of
      (and (valid-position-p source position)
           (char= (char source position) expected-character)))))

;;; -------------------------------------------------------

(defun expect-character (source position expected-character)
  "Determines whether the character at the POSITION into the SOURCE
   exists and equals the EXPECTED-CHARACTER, returning on confirmation
   the index immediately succeeding the POSITION, otherwise signaling an
   error of an unspecified type"
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the fixnum
    (or (and (character-at-equals-p source position expected-character)
             (1+ position))
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered ~a."
          expected-character position
          (if (valid-position-p source position)
            (char source position)
            "end of file")))))

;;; -------------------------------------------------------

(defun locate-character (source start desired-character)
  "Proceeding from the START position into the SOURCE, returns the index
   of the next instance of the DESIRED-CHARACTER, or, upon its absence,
   returns the SOURCE's length."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type character desired-character))
  (the fixnum
    (or (position desired-character source :start start :test #'char=)
        (length source))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position in the
   SOURCE of the first non-whitespace character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun valid-variable-name-p (identifier)
  "Determines whether the IDENTIFIER constitutes a valid variable name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string identifier))
  (the boolean
    (get-boolean-value-of
      (and (plusp (length identifier))
           (every #'variable-character-p identifier)))))

;;; -------------------------------------------------------

(defun validate-variable-name (identifier)
  "Determines whether the IDENTIFIER constitutes a valid variable name,
   returning on confirmation the verbatim IDENTIFIER; otherwise signals
   an error of an unspecified type."
  (declare (type string identifier))
  (the string
    (or (and (valid-variable-name-p identifier)
             identifier)
        (error 'Invalid-Variable-Name-Error
          :offending-name identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-dots (source start)
  "Proceeding from the START position into the SOURCE, tallies the
   number of subsequent dots (\".\") and returns two values:
     (1) A non-negative integer count of the retrieved dots.
     (2) The position into the SOURCE of the first non-dot character."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values unsigned-integer fixnum)
    (loop
      for   position of-type fixnum from start below (length source)
      while (char= (char source position) #\.)
      count 1 into number-of-dots
      finally
        (return
          (values number-of-dots position)))))

;;; -------------------------------------------------------

(defun read-string-literal (source start)
  "Proceeding from the START position into the SOURCE, reads a string
   literal and returns two values:
     (1) The consumed string object.
     (2) The position into the SOURCE immediately succeeding the string
         literal's occupied parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (locate-character source start #\;)))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (subseq source start end)
        end))))

;;; -------------------------------------------------------

(defun read-variable (source start)
  "Proceeding from the START position into the SOURCE, reads a variable
   name and returns two values:
     (1) The consumed variable name, proceeding at most to the nearest
         colon (\":\").
     (2) The position into the SOURCE immediately succeeding the
         variable name's occupied parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (locate-character source start #\:)))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (validate-variable-name
          (subseq source start end))
        end))))

;;; -------------------------------------------------------

(defun parse-halt-command (source start)
  "Proceeding from the START position into the SOURCE, parses a Dots
   halt instruction and returns two values:
     (1) A ``Command'' representation of the parsed halt operation.
     (2) The position into the SOURCE immediately succeeding the
         command's occupied parcel therein."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Command fixnum)
    (values
      (make-command :halt)
      (expect-character source start #\;))))

;;; -------------------------------------------------------

(defun parse-print-command (source start)
  "Proceeding from the START position into the SOURCE, parses a Dots
   print instruction and returns two values:
     (1) A ``Command'' representation of the parsed print operation.
     (2) The position into the SOURCE immediately succeeding the
         command's occupied parcel therein."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (expect-character source start #\:)))
    (declare (type fixnum position))
    (multiple-value-bind (message end)
        (read-string-literal source position)
      (declare (type string message))
      (declare (type fixnum end))
      (the (values Command fixnum)
        (values
          (make-command :print message)
          (expect-character source end #\;))))))

;;; -------------------------------------------------------

(defun parse-define-variable-command (source start)
  "Proceeding from the START position into the SOURCE, parses a Dots
   variable definition instruction and returns two values:
     (1) A ``Command'' representation of the parsed variable definition
         operation.
     (2) The position into the SOURCE immediately succeeding the
         command's occupied parcel therein."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position       (expect-character source start #\:))
        (variable-name  "")
        (variable-value ""))
    (declare (type fixnum position))
    (declare (type string variable-name))
    (declare (type string variable-value))
    (setf (values variable-name position)
      (read-variable source position))
    (setf position
      (expect-character source position #\:))
    (setf (values variable-value position)
      (read-string-literal source position))
    (setf position
      (expect-character source position #\;))
    (the (values Command fixnum)
      (values
        (make-command :define-variable variable-name variable-value)
        position))))

;;; -------------------------------------------------------

(defun empty-program-p (program)
  "Determines whether the PROGRAM is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type program program))
  (the boolean
    (null program)))

;;; -------------------------------------------------------

(defun program-ends-in-halt-command-p (program)
  "Determines whether the PROGRAM's desinent instruction constitutes a
   halt operation (\".;\"), returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type program program))
  (the boolean
    (get-boolean-value-of
      (eq (command-kind (car (last program))) :halt))))

;;; -------------------------------------------------------

(defun validate-program (program)
  "Validates the integrity of the Dots PROGRAM by ascertainment of a
   halt (\".;\") instruction at its desinent position, returning on
   confirmation the PROGRAM itself, otherwise signaling an error of an
   unspecified type."
  (declare (type program program))
  (the program
    (if (or (empty-program-p program)
            (not (program-ends-in-halt-command-p program)))
      (error 'Missing-Halt-Command-Error :offended-program program)
      program)))

;;; -------------------------------------------------------

(defun extract-commands (source)
  "Extracts from the piece of Dots SOURCE code a list of its commands
   and returns the same."
  (declare (type string source))
  (let ((position (skip-whitespaces source 0)))
    (declare (type fixnum position))
    (flet ((process-dots ()
            "Proceeding from the current POSITION into the SOURCE,
             tallies the number of subsequent dots (\".\"), updates the
             POSITION cursor to the NEW-POSITION, and returns the
             determined symbol account."
            (multiple-value-bind (number-of-dots new-position)
                (count-dots source position)
              (declare (type unsigned-integer number-of-dots))
              (declare (type fixnum           new-position))
              (setf position new-position)
              (the unsigned-integer number-of-dots)))
           (collect-command (command new-position)
            "Updates the current POSITION cursor to the NEW-POSITION,
             contingently skipping all immediately succeeding
             whitespaces in the SOURCE, and returns the COMMAND."
            (declare (type Command command))
            (declare (type fixnum  new-position))
            (the Command
              (prog1 command
                (setf position
                  (skip-whitespaces source
                    new-position))))))
      (the program
        (validate-program
          (loop while (< position (length source)) collect
            (let ((number-of-dots (process-dots)))
              (declare (type unsigned-integer number-of-dots))
              (multiple-value-call #'collect-command
                (case number-of-dots
                  (1
                    (parse-halt-command source position))
                  (2
                    (parse-print-command source position))
                  (3
                    (parse-define-variable-command source position))
                  (otherwise
                    (error "No command is associated with ~r dot~:p, ~
                            as detected on position ~d."
                      number-of-dots position)))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          program
    :documentation "The Dots program to evaluate.")
   (variables
    :initform      (make-hash-table :test #'equal)
    :reader        get-variables
    :type          (hash-table-of string string)
    :documentation "Associates the defined variable names with their
                    values."))
  (:documentation
    "The ``Interpreter'' class applies itself to the dever of
     accompassing actual effect to a parsed Dots program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns an ``Interpreter'' nuncupated to the evaluation
   of the Dots PROGRAM."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Evaluates the Dots program maintained by the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  (loop for command of-type Command in (get-program interpreter) do
    (case (command-kind command)
      (:halt
        (loop-finish))
      (:print
        (let ((argument (first (command-arguments command))))
          (declare (type string argument))
          (format T "~a"
            (gethash argument
              (get-variables interpreter)
              argument))))
      (:define-variable
        (destructuring-bind (variable-name variable-value)
            (command-arguments command)
          (declare (type string variable-name))
          (declare (type string variable-value))
          (setf (gethash variable-name (get-variables interpreter))
                variable-value)))
      (otherwise
        (error "Invalid command: ~s." command))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Dots (code)
  "Interprets the piece of Dots source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Dots
  "..:Hello World!;
  .;")

;;; -------------------------------------------------------

;; Print "Hello, World!" by employing a variable.
(interpret-Dots
  "...:greeting:Hello, World!;
   ..:greeting;
   .;")

;;; -------------------------------------------------------

;; Print the message
;;   "Marco
;;    Polo"
;; utilizing variables and a linebreak in the output command.
(interpret-Dots
  "...:call:Marco;
   ...:response:Polo;
   ..:call;
   ..:
;
   ..:response;
   .;")
