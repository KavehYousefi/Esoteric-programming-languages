;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Strvar", invented by the Esolang user
;; "PythonshellDebugwindow" and presented in the year 2020, which is
;; founded upon the principles of string rewriting, capacitating the
;; definition of variables and their substitutions in output strings.
;; 
;; 
;; Concept
;; =======
;; The Strvar programming language's haecceity involves the assignment
;; of strings to variables and the substitution of variable names in
;; strings by their corresponding values.
;; 
;; == Strvar: [STR]INGS AND [VAR]IABLES ==
;; The agnomination whose apportionment permits the language's accosting
;; most likely relays to the allusion to its two most peisant
;; components: *str*ings and *var*iables.
;; 
;; == STRINGS AND VARIABLES, AND VARIABLES IN STRINGS ==
;; A Strvar program's competence amplects two abilities:
;; 
;;   (1) Variables can be assigned string literals.
;;   (2) String literal sections that incorporate variable names are
;;       superseded by the respective variable values.
;; 
;; == TWO COMMANDS: ASSIGNMENT AND OUTPUT ==
;; Strvar's instruction set comprehends a tally of two operations only,
;; the champarty of which conflates into a compound causatum:
;; 
;;   (1) Assignment:
;;         Associates a variable name with a string literal. Already
;;         extant entries by the same agnomination are simply
;;         superseded through the more recent contributor.
;;   
;;   (2) Output:
;;         Replaces variable names in a string literal by their values,
;;         resolves escape codes, and writes the result to the standard
;;         output.
;; 
;; == STRING REWRITING: SEARCH VARIABLE, PLACE VALUE, REPEAT ==
;; The string rewriting procedure is realized in the following iterative
;; stages:
;; 
;;   (1) Search for hitherto defined variable names in the string
;;       literal to evaluate. If such is detected, substitute the first
;;       variable detection by its value. Conflicts betwixt two or more
;;       eligible contenders are resolved by recency: The variable name
;;       most recently assigned appropriates supremity.
;;   (2) If further variables are embedded in the updated string, return
;;       to step (1); otherwise resolve all contingent escape codes, and
;;       consider the string literal's evaluation as completed.
;; 
;; The following pseudocode shall administer a formal conspectus for the
;; language's operational principles:
;; 
;;   { Substitutes variable names by their values in the string, using }
;;   { a variable map for their recognition.                           }
;;   function substituteVariables (string, variables)
;;     with
;;       string    --- a string whose variable names shall be replaced.
;;       variables --- a registry containing variable name-value pairs.
;;     
;;     let resultString <- string
;;     
;;     { Repeatedly replace variable names in the resultString. }
;;     while resultString contains a variable name from the variables do
;;       replace variable name in resultString by variable value
;;     end while
;;     
;;     return resultString
;;   end
;;   
;;   { Replaces escape codes in a string. }
;;   function evaluateEscapeCodes (string)
;;     with
;;       string --- the string to evaluate.
;;     
;;     let evaluatedString <- string
;;     evaluatedString <- resolve escape codes in evaluatedString
;;     return evaluatedString
;;   end function
;;   
;;   procedure defineVariable (variables, name, value)
;;     with
;;       variables --- a registry containing variable name-value pairs.
;;       name      --- the name of the variable to assign or re-assign.
;;       value     --- the string to associate with the variable name.
;;     
;;     if variables contains an entry with the name then
;;       replace variables entry identified by the name with the value
;;     else
;;       append entry (name, value) to variables
;;     end if
;;   end procedure
;;   
;;   for each line x do
;;     if x is an assignment line then
;;       let variableName   <- left side of assignment
;;       let variableValue  <- right side of assignment
;;       let evaluatedValue <- evaluateEscapeCodes(variableValue)
;;       defineVariable(variables, variableName, evaluatedValue)
;;     else if x is a string literal then
;;       let newString <- substituteVariables(string)
;;       newString <- evaluateEscapeCodes(newString)
;;       print newString
;;     else if x is an empty line then
;;       skip
;;     else
;;       error: invalid line type
;;     end if
;;   end for
;; 
;; 
;; Architecture
;; ============
;; Strvar's independence from any architectural designment originates
;; from its commitment to variables as the aefauld vehicles of
;; information storage. This variable registry is subject to the
;; developer's private deliberations, but must admit the disambiguating
;; quality for a variable name's affiliation with its string value.
;; 
;; 
;; Data Types
;; ==========
;; A very rudimentary type system applies to Strvar's furnishings, as
;; the singular species acquainted to the same is exhausted by strings
;; of an arbitrary length.
;; 
;; 
;; Syntax
;; ======
;; A Strvar program consists of a sequence of zero or more lines, either
;; being blank or bearing a single instruction, in the latter case
;; signifying its termination by a semicolon at the desinence.
;; 
;; == COMMANDS APPEAR LINEWISE ==
;; Every non-blank line comprehends at most one command, either a string
;; literal or a variable assignment.
;; 
;; Spaces may appear at any position around tokens, including variable
;; names, which, as counterdistinguished from string literals that
;; assign to any content veridical significance, ignore these specimens.
;; 
;; Every command line must be terminated by exactly one semicolon (";"),
;; a constraint not shared by empty lines.
;; 
;; == STRINGS ==
;; The string diorism proceeds by the ensconcement of a sequence of zero
;; or more characters in double quotation marks. An element of
;; particular interpretation wones in the escape codes, such refers to
;; short sequences introduced by a backlash "\" and adhibited a special
;; species of construe, the produce of the same replaces the encoded
;; structure:
;; 
;;   ------------------------------------------------------------------
;;   Escape code | Effect
;;   ------------+-----------------------------------------------------
;;   \"          | Is replaced by a double quotation mark.
;;   ..................................................................
;;   \r          | Is replaced by a carriage return character.
;;   ..................................................................
;;   \n          | Is replaced by a newline character.
;;   ..................................................................
;;   \t          | Is replaced by a horizontal tab.
;;   ..................................................................
;;   \xhh        | Is replaced by the decimal value corresponding to
;;     **        | the two-character hexadecimal sequence {hh}. The
;;               | reproducible range, as a corollary, spans the
;;               | unsigned byte interval [0, 255].
;;   ------------------------------------------------------------------
;; 
;; == VARIABLE NAMES ==
;; Variable names are defined in terms of one or more characters except
;; the following:
;; 
;;   ------------------------------------------------------------------
;;   Prohibited character | Encheson
;;   ---------------------+--------------------------------------------
;;   "#"                  | Introduces a comment.
;;   ..................................................................
;;   ";"                  | Terminates a non-blank command line.
;;   ..................................................................
;;   "="                  | Separates the variable name from its value.
;;   ------------------------------------------------------------------
;; 
;; The interspersion of spaces is homologated, but does not contribute
;; to the actual identifier; that is, the variable designation
;; 
;;   my var iable
;; 
;; is paregal to the more compendious
;; 
;;   myvariable
;; 
;; == SPACES ==
;; Spaces, a term whose amplectation extends around the space as well as
;; the horizontal tab character, may be interspersed with a generosity
;; only confined to one's own deliberations. Even betwixt a variable
;; identifier's characters such sepiments enjoy homologation, in a mete
;; supputated to the equipollence of their disregard.
;; 
;; Please note, however, that their presence in a string literal's
;; boundaries experiences verbatim preservation.
;; 
;; == COMMENTS ==
;; Comments are introduced by mediation of the hash sign, "#", extending
;; to the end of the line.
;; 
;; == GRAMMAR ==
;; A description of the Strvar syntaxis in the Extended Backus-Naur Form
;; (EBNF) shall be adduced below:
;; 
;;   program           := { innerLine } , [ terminalLine ] ;
;;   innerLine         := [ assignmentCommand | stringCommand ]
;;                     ,  [ comment ]
;;                     ,  newline
;;   terminalLine      := [ assignmentCommand | stringCommand ]
;;                     ,  [ comment ]
;;                     ;
;;   
;;   assignmentCommand := variable , "=" , stringLiteral , ";" ;
;;   stringCommand     := stringLiteral , ";" ;
;;   
;;   variable          := variableCharacter , { variableCharacter } ;
;;   variableCharacter := character - ( quote | "#" | ";" | "=" ) ;
;;   stringLiteral     := quote , { stringConstituent } , quote ;
;;   stringConstituent := stringCharacter | escapeCode ;
;;   escapeCode        := "\"
;;                     ,  ( "r" | "n" | '"' | "t" |  hexadecimalCode )
;;                     ;
;;   hexadecimalCode   := "x" , hexadecimalDigit , hexadecimalDigit ;
;;   hexadecimalDigit  := "0" | "1" | "2" |  "3" |  "4" | "5" | "6"
;;                     |  "7" | "8" | "9" | "10" | "11"
;;                     |  "A" | "B" | "C" | "D"  |  "D" | "F"
;;                     ;
;;   stringCharacter   := { character - quote } ;
;;   quote             := '"' ;
;; 
;; 
;; Instructions
;; ============
;; Strvar's instruction set is already exhausted by its twain of
;; commands, enumerating the string literal, intended for output
;; purposes, and the variable assignment that associates such a literal
;; with an address.
;; 
;; == OVERVIEW ==
;; A tabular explication of the operation twain shall be presented
;; below:
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   string            | Substitutes the contained variable names in
;;   ******            | the {string} by their associated values,
;;                     | repeating the process until none such can be
;;                     | detected, resolves all contingent escape
;;                     | codes, and prints the thus evaluated string
;;                     | to the standard output.
;;                     |-----------------------------------------------
;;                     | The {string} must be a string literal.
;;   ..................................................................
;;   variable = string | Substitutes the contained variable names in
;;   ********   ****** | the {string} by their associated values,
;;                     | repeating the process until none such can be
;;                     | detected, and associates the evaluated string
;;                     | with the {variable}.
;;                     |-----------------------------------------------
;;                     | The {variable} must be a valid variable name.
;;                     |-----------------------------------------------
;;                     | The {string} must be a string literal.
;;                     |-----------------------------------------------
;;                     | If a variable identified by the {variable}
;;                     | name already exists, its value is tacitly
;;                     | replaced by the new content.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Conduced by its meticulous expositions, the original specification
;; obviates the preponderance of uncertainties admissive to one's
;; ideation. Natheless, a scant set of dubious passages retains an orra
;; status, such shall be the following treatise's material.
;; 
;; == WHICH ORDER GOVERNS THE DETECTION OF VARIABLES? ==
;; Strvar's protolog relates of the essential variable substitution step
;; in a manner rather ligated to compendiousness that in the incipient
;; stage
;; 
;;   "1. The first occurrence of a variable name in the string is
;;       replaced with the variable's value."
;; 
;; A residue of this curtailed diction emanates in the guise of some
;; adumbration concerning the concrete order of the variables'
;; eligibility probing. If, par example, the following variable names
;; have been defined --- irregardless of associated values:
;; 
;;   a
;;   ab
;;   abc
;; 
;; and a string literal invested with the content
;; 
;;   "abc"
;; 
;; shall be perquired, the fact of all three candidates' admission can
;; be avered.
;; 
;; The choices for a selection bifurcate into a twain of contingencies:
;; 
;;   (1) The most recently assigned variable takes precedence.
;;   (2) The longest variable name is preferred.
;; 
;; It has been adjudged that the first option (1), the inclination
;; towards recency as the triage criterion, shall be implemented, withal
;; proceeding in concord with re-assignments as more potent expressions
;; than the variables' preterite states.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has been realized in the programming
;; language Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-28
;; 
;; Sources:
;;   [esolang2020Strvar]
;;   The Esolang contributors, "Strvar", 2020
;;   URL: "https://esolangs.org/wiki/Strvar"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized variety of
   Strvar operations, augmented by the ``:nop'' sentinel for
   no-operation (NOP) specimens."
  '(member :assignment :nop :string))

;;; -------------------------------------------------------

(deftype association-list-of (indicator-type value-type)
  "The ``associaton-list-of'' type defines an associaton list, or alist,
   composed of zero or more entries, each member of which constitutes a
   cons whose first moeity is of the INDICATOR-TYPE, aand whose second
   compartment assumes the VALUE-TYPE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for element of-type T in (the list candidate)
              always (typep element
                       `(cons ,indicator-type ,value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype variable-entry ()
  "The ``variable-entry'' type defines an entry in a ``Variable-Map'',
   that is, an element of its underlying association list, thus
   conforming to a cons whose left datum comprehends a variable name in
   string form, affiliated with a string value in the dextral
   compartment."
  '(cons string string))

;;; -------------------------------------------------------

(deftype list-of (element-type)
  "The ``list-of'' type defines a list composed of zero or more
   elements, each speciment of which conforms to the ELEMENT-TYPE."
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, with the claim of exhaustion, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for a
   variable name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (find candidate " \"#;=" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (or null string)    *current-source*))
(declaim (type fixnum              *current-position*))
(declaim (type (or null character) *current-character*))

;;; -------------------------------------------------------

(defparameter *current-source* NIL
  "Stores the currently processed line.")

(defparameter *current-position* 0
  "Maintains the contemporaneous position into the *CURRENT-SOURCE*.")

(defparameter *current-character* NIL
  "Designates the character at the *CURRENT-POSITION* into the
   *CURRENT-SOURCE*.
   ---
   A value of ``NIL'' acts as a sentinel for the exhaustion of the
   *CURRENT-SOURCE*, that is, the end of the same.")

;;; -------------------------------------------------------

(defun initialize-source (new-source)
  (declare (type string new-source))
  (setf *current-source*    new-source)
  (setf *current-position*  0)
  (setf *current-character*
    (when (array-in-bounds-p *current-source* *current-position*)
      (char *current-source* *current-position*)))
  (values))

;;; -------------------------------------------------------

(defun advance ()
  "Returns the *CURRENT-CHARACTER*, ere translating the
   *CURRENT-POSITION* cursor to the next position into *CURRENT-SOURCE*,
   if possible, and finally updating the *CURRENT-CHARACTER*."
  (the (or null character)
    (prog1 *current-character*
      (setf *current-character*
        (when (array-in-bounds-p *current-source*
                (1+ *current-position*))
          (char *current-source* (incf *current-position*)))))))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Proceeding from the *CURRENT-POSITION* into the *CURRENT-SOURCE*,
   skips a sequence of zero or more accolent spaces, and returns no
   value."
  (loop
    while (and *current-character*
               (space-character-p *current-character*))
    do (advance))
  (values))

;;; -------------------------------------------------------

(defun skip-comment ()
  "Determines whether, proceeding from the *CURRENT-POSITION* into the
   *CURRENT-SOURCE*, a comment start, on confirmation designating the
   line as finished by relocating the position cursor to its end, in any
   case returning no value."
  (when (and *current-character*
             (char= *current-character* #\#))
    (setf *current-position*  (length *current-source*))
    (setf *current-character* NIL))
  (values))

;;; -------------------------------------------------------

(defun expect-character (expected-character)
  "Determines whether the *CURRENT-CHARACTER* equals the
   EXPECTED-CHARACTER, on confirmation returning the
   *CURRENT-CHARACTER*, while advancing to the next position into the
   *SOURCE*; otherwise an error of an unspecified type is signaled."
  (declare (type character expected-character))
  (the character
    (or (and *current-character*
             (char= *current-character* expected-character)
             (advance))
        (error "Expected the character \"~c\" at position ~d, but ~
                encountered ~a."
          expected-character *current-position* *current-character*))))

;;; -------------------------------------------------------

(defun expect-string ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, attempts to
   read a double-quoted string literal, on success returning a string
   representation thereof, pruned of its delimiter twain; aliter emits
   an error of an unspecfied type."
  (expect-character #\")
  (the string
    (with-output-to-string (content)
      (declare (type string-stream content))
      (loop do
        (case *current-character*
          ((NIL)
            (error "Unterminated string literal at position ~d."
              *current-position*))
          (#\"
            (advance)
            (loop-finish))
          (otherwise
            (write-char (advance) content)))))))

;;; -------------------------------------------------------

(defun expect-end-of-line ()
  "Determines whether, proceeding from the *CURRENT-POSITION* into the
   *CURRENT-SOURCE*, no effective content, which excludes spaces and
   a contingent comment section, follows, on confirmation skipping these
   insignificant elements and returning no value; otherwise an error of
   an unspecified type is signaled."
  (skip-spaces)
  (skip-comment)
  (when *current-character*
    (error "Expected the end of the current line, but encountered the ~
            character \"~c\" at position ~d."
      *current-character* *current-position*))
  (values))

;;; -------------------------------------------------------

(defun expect-end-of-statement ()
  "Determines whether, proceeding from the *CURRENT-POSITION* into the
   *CURRENT-SOURCE*, the semicolon (\";\") as a statement terminator,
   contingently preceded by spaces, follows, on confirmation skipping
   the same and returning no value; aliter an error of an unspecified
   type is signaled."
  (skip-spaces)
  (case *current-character*
    ((NIL)
      (error "The statement line has terminated without the ~
              concluding semicolon (\";\")."))
    (#\;
      (advance)
      (expect-end-of-line))
    (otherwise
      (error "Expected a statement terminating semicolon (\";\"), ~
              but encountered the character \"~c\" at position ~d."
        *current-character* *current-position*)))
  (values))

;;; -------------------------------------------------------

(defun read-variable-name ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   variable name and returns a string representation thereof."
  (the string
    (with-output-to-string (name)
      (declare (type string-stream name))
      (loop
        do    (skip-spaces)
        while (identifier-character-p *current-character*)
        do    (write-char (advance) name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface attends to the establishment of a
   common foundry for all species of Strvar instructions."
  (type (error "Missing instruction type.") :type instruction-type))

;;; -------------------------------------------------------

(defstruct (NOP-Instruction
  (:include     Instruction)
  (:constructor make-nop-instruction (&aux (type :nop))))
  "The ``NOP-Instruction'' class represents a no-operation (NOP), that
   is, an empty or blank line.")

;;; -------------------------------------------------------

(defstruct (String-Instruction
  (:include     Instruction)
  (:constructor make-string-instruction (value &aux (type :string))))
  "The ``String-Instruction'' class represents a string literal
   statement."
  (value (error "Missing value.") :type string))

;;; -------------------------------------------------------

(defstruct (Assignment-Instruction
  (:include     Instruction)
  (:constructor make-assignment-instruction (variable value
                                             &aux (type :assignment))))
  "The ``Assignment-Instruction'' class represents a variable assignment
   statement, compact of a sinistrally located variable name and the
   dextral compernage in the form of a string literal intended for the
   affiliation with the former."
  (variable (error "Missing variable.") :type string)
  (value    (error "Missing value.")    :type string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-line (line)
  "Processes a LINE and returns an ``Instruction'' representation of its
   contents.
   ---
   Please note that blank lines, either with or without a comment in
   their perimeter, will emit a ``NOP-Instruction'', signifying an
   ignorable presence."
  (declare (type string line))
  
  (initialize-source line)
  
  (skip-spaces)
  
  (the Instruction
    (cond
      ;; Blank line?
      ((null *current-character*)
        (make-nop-instruction))
      
      ;; Blank line with comment line?
      ((char= *current-character* #\#)
        (make-nop-instruction))
      
      ;; String literal line?
      ((char= *current-character* #\")
        (let ((string-literal (expect-string)))
          (declare (type string string-literal))
          (expect-end-of-statement)
          (make-string-instruction string-literal)))
      
      ;; Variable assignment line?
      ((identifier-character-p *current-character*)
        (let ((variable-name (read-variable-name)))
          (declare (type string variable-name))
          (skip-spaces)
          (expect-character #\=)
          (skip-spaces)
          (let ((value (expect-string)))
            (declare (type string value))
            (expect-end-of-statement)
            (make-assignment-instruction variable-name value))))
      
      ;; Invalid character on the line?
      (T
        (error "Invalid character \"~c\" at position ~d."
          *current-character* *current-position*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable map.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Map ()
  ((entries
    :initarg       :entries
    :initform      NIL
    :accessor      variable-map-entries
    :type          (association-list-of string string)
    :documentation "Maintains the variables as name-value twains,
                    automatically sorted succeeding a new entry's
                    insertion."))
  (:documentation
    "The ``Variable-Map'' class applies itself to the castaldy of an
     arbitrary number of variables, amenable to access by their names,
     and manifesting in an association list."))

;;; -------------------------------------------------------

(defun make-variable-map ()
  "Creates and returns a new empty ``Variable-Map''."
  (the Variable-Map
    (make-instance 'Variable-Map)))

;;; -------------------------------------------------------

(defun get-variable-entry (variable-map name)
  "Returns a reference to the entry for the variable NAME in the
   VARIABLE-MAP in the form of a cons composed of two strings, or the
   ``NIL'' value if none such exists.
   ---
   The affirmative result object's state as a direct cons reference
   capacitates the entry's immediate manipulation, especially anenst the
   variable value whose woning is realized in the cell's second moeity,
   the \"cdr\" part. Please abstain from manipulations targeted at the
   key, the variable name commorant in the first compartment, as its
   sanity must be vouched for the VARIABLE-MAP entries' correct
   sorting."
  (declare (type Variable-Map variable-map))
  (declare (type string       name))
  (the (or null variable-entry)
    (assoc name
      (variable-map-entries variable-map)
      :test #'string=)))

;;; -------------------------------------------------------

(defun get-variable-value (variable-map name)
  "Returns the value associated with the variable NAME in the
   VARIABLE-MAP, or returns an error of an unspecified type if no such
   association holds."
  (declare (type Variable-Map variable-map))
  (declare (type string       name))
  (the string
    (or (cdr (get-variable-entry variable-map name))
        (error "No variable with the name ~s found." name))))

;;; -------------------------------------------------------

(defun sort-variables (variable-map)
  "Sorts the entries in the VARIABLE-MAP in case-insensitive
   lexicographical order of ascending variable names and returns the
   modified VARIABLE-MAP."
  (declare (type Variable-Map variable-map))
  (setf (variable-map-entries variable-map)
    (sort
      (variable-map-entries variable-map)
      #'string-lessp
      :key #'car))
  (values))

;;; -------------------------------------------------------

(defun set-variable (variable-map name value)
  "Registers a variable with the NAME and the VALUE at the VARIABLE-MAP,
   contingently substituting an already extant entry with the same NAME,
   and returns no value."
  (declare (type Variable-Map variable-map))
  (declare (type string       name))
  (declare (type string       value))
  (let ((extant-entry (get-variable-entry variable-map name)))
    (declare (type (or null variable-entry) extant-entry))
    (cond
      (extant-entry
        (setf (cdr extant-entry) value))
      (T
        (push (cons name value)
              (variable-map-entries variable-map))
        (sort-variables variable-map))))
  (values))

;;; -------------------------------------------------------

(defun get-variable-names (variable-map)
  "Returns a list of the VARIABLE-MAP's registered variable names,
   following the internally vouched case-insensitive lexicographical
   ordering according to the ascending names."
  (declare (type Variable-Map))
  (the (list-of string)
    (mapcar #'car
      (variable-map-entries variable-map))))

;;; -------------------------------------------------------

(defmethod print-object ((variable-map Variable-Map) stream)
  (declare (type Variable-Map variable-map))
  (declare (type destination  stream))
  (format stream "~&Variable-Map:")
  (dolist (entry (variable-map-entries variable-map))
    (declare (type variable-entry entry))
    (format stream "~&  ~s => ~s"
      (car entry)
      (cdr entry))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-hexadecimal-digit ()
  "Proceeding from the *CURRENT-POSITION* into the *CURRENT-SOURCE*,
   reads a single hexadecimal digit and returns its decimal value, which
   constitutes an integer in the range [0, 15]."
  (the (integer 0 15)
    (cond
      ((null *current-character*)
        (error "Expected a hexadecimal digit at position ~d, ~
                but encountered the end of the line."
          *current-position*))
      ((digit-char-p *current-character* 16)
        (prog1
          (digit-char-p *current-character* 16)
          (advance)))
      (T
        (error "Expected a hexadecimal digit at position ~d, ~
              but encountered \"~c\"."
          *current-position* *current-character*)))))

;;; -------------------------------------------------------

(defun read-hexadecimal-number ()
  "Proceeding from the *CURRENT-POSITION* into the *CURRENT-SOURCE*,
   reads a twain of consecutive hexadecimal digits and returns its
   decimal value, which constitutes an integer in the range [0, 255]."
  (the (integer 0 255)
    (dpb
      (read-hexadecimal-digit)
      (byte 4 4)
      (dpb
        (read-hexadecimal-digit)
        (byte 4 0)
        0))))

;;; -------------------------------------------------------

(defun read-escape-code (destination)
  "Consumes an escape code, writes it to the DESTINATION in a format
   fitten for its nature, and returns no value."
  (declare (type string-stream destination))
  (expect-character #\\)
  (case *current-character*
    ((NIL)
      (error "Unterminated escape code at position ~d."
        *current-position*))
    (#\r
      (advance)
      (write-char #\Return destination))
    (#\n
      (advance)
      (write-char #\Newline destination))
    (#\"
      (advance)
      (write-char #\" destination))
    (#\t
      (advance)
      (write-char #\Tab destination))
    ;; Hexadecimal number of the format
    ;;   \xhh
    ;; where
    ;;   hh --- two hexadecimal digits.
    (#\x
      (advance)
      (format destination "~d"
        (read-hexadecimal-number)))
    (otherwise
      (write-char *current-character* destination)))
  (values))

;;; -------------------------------------------------------

(defun evaluate-string (string)
  "Proceeding from the *CURRENT-POSITION* into the *CURRENT-SOURCE*,
   attempts to read a double-quoted string literal, on success returning
   a string representation thereof, pruned of its delimiter twain;
   aliter emits an error of an unspecified type."
  (initialize-source string)
  (the string
    (with-output-to-string (content)
      (declare (type string-stream content))
      (loop do
        (case *current-character*
          ((NIL)
            (loop-finish))
          (#\\
            (read-escape-code content))
          (otherwise
            (write-char (advance) content)))))))

;;; -------------------------------------------------------

(defun seek-variable (source variables)
  "Searches in the SOURCE for the first occurrences of a variable name
   from the VARIABLES map, returning two values:
     (1) If a match could be confirmed, the name of the first eligible
         variable; aliter ``NIL''.
     (2) If a match could be confirmed, the position in the SOURCE of
         the matching variable name's start; aliter ``NIL''."
  (declare (type string       source))
  (declare (type Variable-Map variables))
  (the (values (or null string) (or null fixnum))
    (loop
      for candidate-variable
        of-type string
        in      (get-variable-names variables)
      for position-in-source
        of-type (or null fixnum)
        =       (search candidate-variable source :test #'string=)
      when position-in-source do
        (return
          (values candidate-variable position-in-source))
      finally
        (return
          (values NIL NIL)))))

;;; -------------------------------------------------------

(defun replace-string-section (source start excision-extent new-content)
  "Returns a new string by removing from the SOURCE, proceeding from the
   inclusive START position and encompasing the EXCISION-EXTENT number
   of consecutive characters, a section, which is subsequently replaced
   by the complete NEW-CONTENT.
   ---
   The SOURCE string remains unmodified."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum excision-extent))
  (declare (type string new-content))
  (the string
    (with-output-to-string (modified-source)
      (declare (type string-stream modified-source))
      ;; Copy the SOURCE segment from its beginning until the position
      ;; immediately preceding the START location.
      (format modified-source "~a"
        (subseq source 0 start))
      ;; Insert the complete NEW-CONTENT.
      (format modified-source "~a" new-content)
      ;; Copy the SOURCE segment following the EXCISION-EXTENT tally of
      ;; characters after the START location, and extending to the
      ;; SOURCE's end.
      (format modified-source "~a"
        (subseq source (+ start excision-extent))))))

;;; -------------------------------------------------------

(defun substitute-variables (variables string)
  "Creates and returns a new string based upon the input STRING by
   repeated substitutions of the embedded variable names as specified by
   the VARIABLES map.
   ---
   A consectary of its iterative procedure, this function may not be
   able to terminate, forecause being captured inside of an infinite
   loop."
  (declare (type Variable-Map variables))
  (declare (type string       string))
  (let ((new-string        string)
        (embedded-variable NIL)
        (start-position    NIL))
    (declare (type string           new-string))
    (declare (type (or null string) embedded-variable))
    (declare (type (or null fixnum) start-position))
    (loop do
      (multiple-value-setq
        (embedded-variable start-position)
        (seek-variable new-string variables))
      (if (and embedded-variable start-position)
        (setf new-string
          (replace-string-section new-string start-position
            (length embedded-variable)
            (get-variable-value variables embedded-variable)))
        (loop-finish)))
    (the string new-string)))

;;; -------------------------------------------------------

(defun interpret-Strvar (code)
  "Interprets the piece of Strvar source CODE and returns no value."
  (declare (type string code))
  
  (let ((variables (make-variable-map)))
    (declare (type Variable-Map variables))
    
    (with-input-from-string (code-stream code)
      (declare (type string-stream code-stream))
      
      (loop
        for line
          of-type (or null string)
          =       (read-line code-stream NIL NIL)
        
        while line
        
        for current-instruction
          of-type Instruction
          =       (evaluate-line line)
        
        do
          (case (instruction-type current-instruction)
            (:assignment
              ;; Substitute the string literal's variables and store the
              ;; result into the variable.
              (set-variable variables
                (assignment-instruction-variable current-instruction)
                (substitute-variables variables
                  (assignment-instruction-value current-instruction))))
            
            (:string
              ;; Substitute the variables.
              (setf (string-instruction-value current-instruction)
                (substitute-variables variables
                  (string-instruction-value current-instruction)))
              ;; Print the string literal with its escape codes
              ;; resolved.
              (format T "~a~%"
                (evaluate-string
                  (string-instruction-value current-instruction))))
            
            ;; Skip blank lines.
            (:nop
              NIL)
            
            (otherwise
              (error "Unrecognized instruction: ~s."
                current-instruction))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Strvar "\"Hello, World!\";")

;;; -------------------------------------------------------

;; Print "Hello, World!" utilizing variable substitutions.
(interpret-Strvar
  "h = \"Hello,\";
   w = \"World!\";
   \"h w\";")

;;; -------------------------------------------------------

;; Enters an infinite loop as the variable with the name "w" is
;; comprehended and detected repeatedly in its value "world!".
(interpret-Strvar
  "h = \"Hello,\";
   w = \"world!\";
   \"h w\";")

;;; -------------------------------------------------------

;; Truth-machine which simulates an input of zero (0): Prints the value
;; zero (0) once and terminates.
(interpret-Strvar
  "1 = \"0\";
   \"1\";")

;;; -------------------------------------------------------

;; Truth-machine which simulates an input of one (1): Enters an infinite
;; loop, thus printing no value.
(interpret-Strvar
  "1 = \"1\";
   \"1\";")

;;; -------------------------------------------------------

;; Print "c".
(interpret-Strvar
  "a = \"b\";
   b = \"c\";
   \"a\";")

;;; -------------------------------------------------------

;; Print "x".
(interpret-Strvar
  "b = \"x\";
   a = \"b\";
   b = \"y\";
   \"a\";")

;;; -------------------------------------------------------

;; Print "1 2 3".
(interpret-Strvar
  "one = \"1\";
   two = \"one 2\";
   three = \"two 3\";
   \"three\";")
