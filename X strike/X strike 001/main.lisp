;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "X strike", invented by the Esolang user "Infinitehexagon"
;; and presented on October 10th, 2023, the diorism of which resides in
;; its predominant employment of decussate symbols in order to
;; manipulate a unidirectionally infinite expanse of integer-valued
;; cells.
;; 
;; 
;; Concept
;; =======
;; The X strike programming language operates on a peculiar format of
;; instructions in order to manipulate a tape composed of an infinite
;; account of signed integer cells.
;; 
;; == X STRIKE: "X" IN MANY FORMS ==
;; The X strike programming language imposes a dioristic forbisen on its
;; quadruple operations by a prefixion with either the majuscle "X" or
;; its miniscular form "x", conditionally succeeded by an ecphoneme
;; ("!"), a mandatory multiplication sign ("×"), and concluded with one
;; or more numeric arguments desumed from the signed integral realm.
;; 
;; == THE MEMORY: A UNIDIRECTIONALLY INFINITE CATENA OF INTEGERS ==
;; The program memory's model enumerates an aefauld tape, its componency
;; a unilateral infinite dispansion of cells, each such a signed
;; scalar integer's salvatory, and amenable to a non-negative integer
;; index greater than or equal to zero (0).
;; 
;; Endowed in their incipial state with a value of zero (0), operations
;; exist to gradually increment and decrement such a unit's content.
;; 
;; A cell pointer, during the program inchoation empighted on the cell
;; at the index zero (0), designates at any instant the currently active
;; cell, the cynosure of a preponderance among the indagative and
;; manipulative warklumes. Its status as a mobile entity capacitates
;; the cell pointer's stillatitious perambulation along the tape.
;; 
;; 
;; Syntax
;; ======
;; X strike arranges its programs in a linewise fashion, each such
;; distribution's parcel at most a single instruction's bailiwick,
;; composed of an identifying preamble for the operation's stevening,
;; and in its patration aided by one or more numeric arguments from the
;; integral range.
;; 
;; == GRAMMAR ==
;; The language's donat shall be elucidated by an Extended Backus-Naur
;; Form's adminiculum:
;; 
;;   program            := { innerLine } , [ lastLine ] ;
;;   innerLine          := innerCommandLine | innerBlankLine ;
;;   innerCommandLine   := command , newline ;
;;   innerBlankLine     := newline ;
;;   lastLine           := lastCommandLine ;
;;   lastCommandLine    := command ;
;;   command            := increment | decrement | goto | inputOutput ;
;;   increment          := "X!×" , nonNegativeInteger ;
;;   decrement          := "x!×" , nonNegativeInteger ;
;;   goto               := "X×"  , nonNegativeInteger
;;                      ,  "×"   , signedInteger
;;                      ;
;;   inputOutput        := "x^" , signedInteger
;;                      ,  "×"  , signedInteger
;;                      ,  "×"  , signedInteger
;;                      ;
;;   signedInteger      := [ "+" | "-" ] , digit , { digit } ;
;;   nonNegativeInteger := [ "+" ]       , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;;   newline            := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; X strike's instruction roster enumerates a quadruple capacity, to
;; whom siccan competences' dation are granted as the incrementation and
;; deduction of a tape cell's value, with a concomitant sinistral or
;; dextral relocation, numeric input and output facilities, and a
;; line-based goto control flow mechanism.
;; 
;; == OVERVIEW ==
;; The following apercu shall impart a cursory mete of gnarity's
;; adhibition concerning the language's operation features.
;; 
;; Please heed that succedaneous segments in a command are underlined
;; via a catena of asterisks ("*"), the spatial dedication ought to be
;; superseded by actual X strike code in the final program.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   X!×value       | Increments the current cell by the {value} and
;;      *****       | subsequently translates the cell pointer one step
;;                  | to the right.
;;                  |--------------------------------------------------
;;                  | {value} must be a non-negative integer number
;;                  | greater than or equal to zero (0).
;;   ..................................................................
;;   x!×value       | Decrements the current cell by the {value} and
;;      *****       | subsequently translates the cell pointer one step
;;                  | to the left, if possible, otherwise retaining its
;;                  | current cell pointer location.
;;                  |--------------------------------------------------
;;                  | {value} must be a non-negative integer number
;;                  | greater than or equal to zero (0).
;;   ..................................................................
;;   X×line×guard   | If the current cell value equals the {guard},
;;     **** *****   | relocates the instruction pointer to the line
;;                  | designated by the one-based {line} index;
;;                  | otherwise exercises no effect.
;;                  |--------------------------------------------------
;;                  | {line} must be an unsigned integer literal
;;                  | greater than or equal to one (1).
;;                  |--------------------------------------------------
;;                  | {value} must be a signed or unsigned integer
;;                  | literal.
;;                  |--------------------------------------------------
;;                  | Adhering to a pseudocode formulation, the
;;                  | following diorism holds:
;;                  | 
;;                  |   if memory[pointer] = {guard} then
;;                  |     jump to line at position {line}
;;                  |   end if
;;   ..................................................................
;;   x^in×out×guard | Depending on the {guard} and its contingent
;;     ** *** ***** | appropriation as a memory cell index,
;;                  | conditionally queries for a user input integer
;;                  | stores it in the cell at the index {in} and/or
;;                  | outputs the value of the cell at the index {out}
;;                  | to the standard output.
;;                  |--------------------------------------------------
;;                  | For the input communication, the following
;;                  | stipulations are imposed: The standard input is
;;                  | queried for an integer number and stored in the
;;                  | cell with the index {in} if either of these
;;                  | prerequisites hold:
;;                  | 
;;                  |   (a) {guard} is greater than or equal to
;;                  |       zero (0), the cell value at the index
;;                  |       {guard} does not equal zero, and {in}
;;                  |       is greater than or equal to zero (0).
;;                  |   (b) {guard} is less than zero (0) and {in}
;;                  |       is greater than or equal to zero (0).
;;                  | 
;;                  | Otherwise, no input is queried.
;;                  |--------------------------------------------------
;;                  | For the output communication, the following
;;                  | stipulations are imposed: The standard output
;;                  | prints the  value of the cell with the index
;;                  | {out} if either of these prerequisites hold:
;;                  |
;;                  |   (a) {guard} is greater than or equal to
;;                  |       zero (0), the cell value at the index
;;                  |       {guard} does not equal zero, and {out}
;;                  |       is greater than or equal to zero (0).
;;                  |   (b) {guard} is less than zero (0) and {out}
;;                  |       is greater than or equal to zero (0).
;;                  | 
;;                  | Otherwise, no output dispay is issued.
;;                  |--------------------------------------------------
;;                  | {guard} must be a signed or unsigned integer
;;                  | number. If negative, it is not further deployed
;;                  | during this operation.
;;                  |--------------------------------------------------
;;                  | {in} must be a signed or unsigned integer number.
;;                  | If negative, no input is queried.
;;                  |--------------------------------------------------
;;                  | {out} must be a signed or unsigned integer
;;                  | number. If negative, no output is issued.
;;                  |--------------------------------------------------
;;                  | Adhering to a pseudocode formulation, the
;;                  | following diorism holds:
;;                  | 
;;                  |   let guardSatisfied <- false
;;                  |   
;;                  |   if {guard} < 0 then
;;                  |     guardSatisfied <- true
;;                  |   else if memory[{guard}] != 0 then
;;                  |     guardSatisfied <- true
;;                  |   else
;;                  |     guardSatisfied <- false
;;                  |   end if
;;                  |   
;;                  |   if guardSatisfied and ({in} >= 0) then
;;                  |     memory[{in}] <- query integer from input
;;                  |   end if
;;                  |   
;;                  |   if guardSatisfied and ({out} >= 0) then
;;                  |     print memory[{out}]
;;                  |   end if
;;   ------------------------------------------------------------------
;; 
;; == INPUT/OUTPUT ==
;; A conspicable complexity embues the input/output operation
;; 
;;   x^in×out×guard
;; 
;; whence proceeds the vindication of its arguments' interplay, in
;; particular such perspective airted at the {guard}'s influence.
;; 
;; The following table shall juxtapose, in its two sinistrla ranks, the
;; possible modes by which the {guard}'s deportment is helmed, with the
;; respondency expected in the input and output conduits, the former
;; stores its numeric reception in the cell designated by the index
;; {in}, while the latter's provenance for its print datum is elicited
;; from the {out} memory location.
;; 
;;   -------------------------------------------------------------
;;              Guard           |     Input      |    Output
;;   Value | Mode               | Value | Effect | Value | Effect
;;   ------+--------------------+-------+--------+-------+--------
;;   <  0  | Guard Ignored      | <  0  | No     | <  0  | No
;;         |                    |................|................
;;         |                    | >= 0  | Yes    | >= 0  | Yes
;;   ---------------------------|----------------|----------------
;;   >= 0  | Conditional:       | <  0  | No     | <  0  | No
;;         | memory[guard] != 0 |................|................
;;         |                    | >= 0  | Yes    | >= 0  | Yes
;;         |--------------------|----------------|----------------
;;         | Conditional:       | <  0  | No     | <  0  | No
;;         | memory[guard]  = 0 |................|................
;;         |                    | >= 0  | No     | >= 0  | No
;;   -------------------------------------------------------------
;; 
;; The {guard}, is eath in its admission to one's conspectuity, solely
;; fathoms the input/output channels' activity if itself is accommodated
;; a non-negative integer sentinel, the memory cell located at the
;; index {guard} being in this circumstance the quesited object whose
;; divergence from zero (0) either homologates a further inquisition of
;; the communication departments' parameters, or, if the guard cell
;; responds with zero (0), the complete operation is discarded.
;; 
;; In any other case, that is, with the {guard} less than zero (0), a
;; partially unconditional mode governs the entire operation, by which
;; agency the input and output facilities themselves exercise as the
;; aefauld nomothetes the behest for their services.
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-21
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Xstrike]
;;   The Esolang contributors, "X strike", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/X_strike"
;;   
;;   [stackoverflow2010q2954642]
;;   The Stack Overflow contributors,
;;     "Methods and properties in scheme: is OOP possible in Scheme?",
;;     June 2nd, 20210
;;   URL: "https://stackoverflow.com/questions/2954642/
;;         methods-and-properties-in-scheme-is-oop-possible-in-scheme"
;;   Notes:
;;     - Demonstrates the deployment of closures as an avenue for the
;;       emulation of classes in the Scheme programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type (type-name
                                  (candidate-variable &rest arguments)
                                  &body body)
  "Defines a derivated type utilizing  the ``deftype'' infrastructure
   in conjunction with the ``satisfies'' type specifier, the new type
   being stevened by the TYPE-NAME and its lambda list extracted from
   the ARGUMENTS, while the probed object's agnomination resorts to the
   CANDIDATE-VARIABLE, the same may be accessed by the BODY forms, with
   the latter either producing a generalized Boolean truth value of
   \"true\" for a matching candidate, or ``NIL'' for the docimasy's
   negation."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@arguments)
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Definition of the type ~s." type-name))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   that obey the ELEMENT-TYPE, its default imposed by the generic
   sentinel ``*''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   '*)
                                                 (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which assumes the KEY-TYPE and responds to
   a value of the VALUE-TYPE, both species defaulting to the generic
   sentinel ``*''."
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

(deftype program ()
  "The ``program''  type defines an executable X strike program as a
   vector of zero or more ``Command'' objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines a non-negative integer
   number of no upper bourne's imposition, which conditions the valid
   interval of [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of integer-valued
   cells, amenable to non-negative integer indices, and modeled by a
   hash table, the key of which assume the cell ``non-negative-integer''
   specifiers, and map to signed integer cell values."
  '(hash-table-of non-negative-integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which amplects, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab))))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign, that
   is, either the plus (\"+\") or minus (\"-\") symbol, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\+)
          (char= candidate #\-))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-lexer (name)
  "Defines and returns a new function designated by the NAME which
   serves as a lexical analyzer, or lexer, by its state's castaldy via
   closures.
   ---
   The thus generated functional object assumes the succedaneum Of a
   traditional object-oriented solution, usually a class, and, in an act
   of supererogation, as a concomitant an instance's provision."
  `(let ((source   "")
         (position 0))
     (declare (type string source))
     (declare (type fixnum position))
     
     (symbol-macrolet
         ((character
           (the (or null character)
             (when (array-in-bounds-p source position)
               (char source position)))))
       (declare (type (or null character) character))
       
       (defun ,name (command &rest arguments)
         "Executes the operation designated by the COMMAND, optionally
          deriving its requisite inputs from the ARGUMENTS, and returns
          a result covenable for the former's specification."
         (declare (type keyword     command))
         (declare (type (list-of T) arguments))
         (declare (ignorable        arguments))
         (the T
           (case command
             (:get-source
               (the string source))
             
             (:set-source
               (destructuring-bind (new-source) arguments
                 (declare (type string new-source))
                 (setf source new-source))
               (setf position 0)
               (values))
             
             (:get-position
               (the fixnum position))
             
             (:set-position
               (destructuring-bind (new-position) arguments
                 (declare (type fixnum new-position))
                 (setf position new-position))
               (values))
             
             (:get-character
               (the (or null character) character))
             
             (:advance
               (the (or null character)
                 (prog1 character
                   (when (array-in-bounds-p source position)
                     (incf position)))))
             
             (:character-equals-p
               (destructuring-bind (expected-character) arguments
                 (declare (type character expected-character))
                 (let ((current-character character))
                   (declare (type (or null character)
                                  current-character))
                   (the boolean
                     (not (null
                       (and current-character
                            (char= current-character
                                   expected-character))))))))
             
             (:character-satisfies-p
               (destructuring-bind (predicate) arguments
                 (declare (type (function (character) *) predicate))
                 (let ((current-character character))
                   (declare (type (or null character)
                                  current-character))
                   (the boolean
                     (not (null
                       (and current-character
                            (funcall predicate current-character))))))))
             
             (otherwise
               (error "Unrecognized command name: ~s." command))))))))

;;; -------------------------------------------------------

;; Generate the globally accessible lexer instances.
(define-lexer line-lexer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface accommodates a foundry for all concrete
   classes to whom the dever of a nature are apportioned that delineates
   a particular X strike operation.")

;;; -------------------------------------------------------

(defstruct (Decrement-Command
  (:include     Command)
  (:constructor make-decrement-command (value)))
  "The ``Decrement-Command'' implements the ``Command'' interface in its
   purpose to replicate the X strike decrement command."
  (value 0 :type non-negative-integer :read-only T))

;;; -------------------------------------------------------

(defstruct (Goto-Command
  (:include     Command)
  (:constructor make-goto-command (target-line guard)))
  "The ``Goto-Command'' implements the ``Command'' interface in its
   purpose to replicate the X strike goto command."
  (target-line 0 :type integer :read-only T)
  (guard       0 :type integer :read-only T))

;;; -------------------------------------------------------

(defstruct (Increment-Command
  (:include     Command)
  (:constructor make-increment-command (value)))
  "The ``Increment-Command'' implements the ``Command'' interface in its
   purpose to replicate the X strike increment command."
  (value 0 :type non-negative-integer :read-only T))

;;; -------------------------------------------------------

(defstruct (Input/Output-Command
  (:include     Command)
  (:constructor make-input/output-command (input-cell
                                           output-cell
                                           guard-cell)))
  "The ``Input/Output-Command'' implements the ``Command'' interface in
   its purpose to replicate the X strike input/output command."
  (input-cell  0 :type integer :read-only T)
  (output-cell 0 :type integer :read-only T)
  (guard-cell  0 :type integer :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expect-character (expected-character)
  "Determines whether the LINE-LEXER's current character matches the
   EXPECTED-CHARACTER, on confirmation advancing beyond the same to the
   next position in its source, while returning no value; otherwise
   signals an error of an unspecified type."
  (declare (type character expected-character))
  (if (line-lexer :character-equals-p expected-character)
    (line-lexer :advance)
    (error "Expected the character \"~c\", but encountered \"~c\" ~
            at position ~d."
      expected-character
      (line-lexer :get-character)
      (line-lexer :get-position)))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Proceeding from the current position into the LINE-LEXER's source,
   skips a sequence of zero or more accolent spaces and returns no
   value."
  (loop while (line-lexer :character-satisfies-p #'space-character-p) do
    (line-lexer :advance))
  (values))

;;; -------------------------------------------------------

(defun read-integer ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes and returns a signed or unsigned integer number."
  (skip-spaces)
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        ;; Read optional signum.
        (when (line-lexer :character-satisfies-p #'sign-character-p)
          (write-char (line-lexer :advance) digits))
        ;; Read decimal digits.
        (loop
          while (line-lexer :character-satisfies-p #'digit-char-p)
          do    (write-char (line-lexer :advance) digits))))))

;;; -------------------------------------------------------

(defun ascertain-non-negative-integer (number)
  "Determines whether the NUMBER represents a non-negative integer
   object, on confirmation simply returning no value, otherwise an error
   of an unspecified type is signaled."
  (declare (type integer number))
  (the (integer 0 *)
    (if (minusp number)
      (error "The number ~d does not constitute a non-negative integer."
        number)
      number)))

;;; -------------------------------------------------------

(defun read-raw-argument ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an integer argument without a prevenient multiplication sign
   (\"×\"), and returns the integer value."
  (the integer
    (read-integer)))

;;; -------------------------------------------------------

(defun read-prefixed-argument ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an integer argument with a prevenient multiplication sign
   (\"×\"), and returns the integer value."
  (skip-spaces)
  (expect-character #\×)
  (the integer
    (read-integer)))

;;; -------------------------------------------------------

(defun expect-end-of-line ()
  "Determines whether, proceeding from the LINE-LEXER's current
   position, its source comprehends no effective content, spaces and
   tabs being exempted from this diorism, on confirmation advancing the
   position cursor to the line's desinence, while returning no value;
   otherwise an error of an unspecified type is signaled."
  (skip-spaces)
  (when (line-lexer :get-character)
    (error "Expected the end of the line, but encountered the ~
            character \"~c\" at position ~d."
      (line-lexer :get-character)
      (line-lexer :get-position)))
  (values))

;;; -------------------------------------------------------

(defun parse-increment-command ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an X strike increment command and returns an
   ``Increment-Command'' representation thereof.
   ---
   The command's syntaxis obeys the weftage
     X!×{value}
   where {value} specifeis the value to increment by."
  (expect-character #\!)
  (let ((value (ascertain-non-negative-integer
                 (read-prefixed-argument))))
    (declare (type non-negative-integer value))
    (expect-end-of-line)
    (the Increment-Command
      (make-increment-command value))))

;;; -------------------------------------------------------

(defun parse-decrement-command ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an X strike decrement command and returns a
   ``Decrement-Command'' representation thereof.
   ---
   The command's syntaxis obeys the weftage
     x!×{value}
   where the {value} specifies the value to decrement by."
  (expect-character #\!)
  (let ((value (ascertain-non-negative-integer
                 (read-prefixed-argument))))
    (declare (type non-negative-integer value))
    (expect-end-of-line)
    (the Decrement-Command
      (make-decrement-command value))))

;;; -------------------------------------------------------

(defun parse-goto-command ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an X strike goto command and returns a ``Goto-Command''
   representation thereof.
   ---
   The command's syntaxis obeys the weftage
     X×{targetLine}×{guard}
   where the {targetLine} specifies the index of the line to jump to if
   the current cell value equals the {guard}."
  (let ((target-line-index (read-prefixed-argument)))
    (declare (type integer target-line-index))
    (let ((guard (read-prefixed-argument)))
      (declare (type integer guard))
      (expect-end-of-line)
      (the Goto-Command
        (make-goto-command target-line-index guard)))))

;;; -------------------------------------------------------

(defun parse-input/output-command ()
  "Proceeding from the current position into the LINE-LEXER's source,
   consumes an X strike input/output command and returns an
   ``Input/Output-Command'' representation thereof.
   ---
   The command's syntaxis obeys the weftage
     x^{inputCell}×{outputCell}×{guardCell}
   where the {inputCell} specifies the index of the cell to store the
   numeric user input in, the {outputCell} designates the index of the
   cell whose value to print in its ASCII character form, and the
   {guardCell} imposes the index of the cell whose value ought to be
   non-zero in order to actuate these causata."
  (expect-character #\^)
  (let ((input-target  (read-raw-argument))
        (output-source (read-prefixed-argument))
        (guard-cell    (read-prefixed-argument)))
    (declare (type integer input-target))
    (declare (type integer output-source))
    (declare (type integer guard-cell))
    (expect-end-of-line)
    (the Input/Output-Command
      (make-input/output-command
        input-target
        output-source
        guard-cell))))

;;; -------------------------------------------------------

(defun parse-line ()
  "Parses the LINE-LEXER's source and either returns a ``Command''
   representation of its incorporated instruction, or the ``NIL'' value
   upon the line's vacancy."
  (skip-spaces)
  (the (or null Command)
    (case (line-lexer :get-character)
      ((NIL)
        NIL)
      ;; Increment or goto command.
      (#\X
        (line-lexer :advance)
        (skip-spaces)
        (case (line-lexer :get-character)
          ((NIL)
            (error "Unterminated command starting with \"X\"."))
          ;; X!×a
          (#\!
            (parse-increment-command))
          ;; X×a×b
          (#\×
            (parse-goto-command))
          (otherwise
            (error "Unrecognized command: \"X~c\"."
              (line-lexer :get-character)))))
      ;; Decrement or input/output command.
      (#\x
        (line-lexer :advance)
        (skip-spaces)
        (case (line-lexer :get-character)
          ((NIL)
            (error "Unterminated command starting with \"x\"."))
          ;; x!×a
          (#\!
            (parse-decrement-command))
          ;; x^a×b×c
          (#\^
            (parse-input/output-command))
          (otherwise
            (error "Unrecognized command: \"x~c\"."
              (line-lexer :get-character)))))
      (otherwise
        (error "The character \"~c\" at position ~d does not ~
                introduce any recognized command."
          (line-lexer :get-character)
          (line-lexer :get-position))))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Extracts from the piece of X strike source CODE the incorporated
   commands and returns a ``program'' representation thereof."
  (declare (type string code))
  (flet ((envelope-in-list (optional-command)
          "If the OPTIONAL-COMMAND constitutes a non-``NIL'' value,
           returns a singleton list with the same as its aefauld
           element, otherwise responds with the empty list ``NIL''."
          (declare (type (or null Command) optional-command))
          (the (list-of Command)
            (when optional-command
              (list optional-command)))))
    (with-input-from-string (input code)
      (declare (type string-stream input))
      (the program
        (coerce
          (loop
            for    line of-type (or null string) = (read-line input NIL)
            while  line
            do     (line-lexer :set-source line)
            append (envelope-in-list (parse-line)))
        '(simple-array Command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-table
    :documentation "A sparse vector of integer-valued cells, realized by
                    a hash table, the keys of which furnish the cell
                    indices and map to the units' values.")
   (pointer
    :initform      0
    :type          non-negative-integer
    :documentation "The cell pointer which, by storing the respective
                    key into the CELLS table, references the currently
                    active cell."))
  (:documentation
    "The ``Memory'' class establishes an implementation of the X strike
     program memory, representing the contingency for an infinite tally
     of cells by a hash table-based sparse vector, its entire dispanse
     the cell pointer's purview for the currently responsive unit's
     memorization."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a fresh ``Memory'' instance."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun cell-value-at (memory index)
  "Returns the value of the MEMORY cell located at the INDEX."
  (declare (type Memory               memory))
  (declare (type non-negative-integer index))
  (with-slots (cells) memory
    (declare (type cell-table cells))
    (the integer
      (gethash index cells 0))))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell located at the INDEX and
   returns no value."
  (declare (type integer              new-value))
  (declare (type Memory               memory))
  (declare (type non-negative-integer index))
  (with-slots (cells) memory
    (declare (type cell-table cells))
    (setf (gethash index cells 0) new-value))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (cell-value-at memory
      (slot-value memory 'pointer))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the current MEMORY cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (pointer) memory
    (declare (type non-negative-integer pointer))
    (setf (cell-value-at memory pointer) new-value))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left, if
   possible, and returns no value."
  (declare (type Memory memory))
  (with-slots (pointer) memory
    (declare (type non-negative-integer pointer))
    (when (plusp pointer)
      (decf (slot-value memory 'pointer))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun non-negative-p (candidate)
  "Determines whether the CANDIDATE represents a non-negative integer
   number, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (not (minusp candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          program
    :documentation "An executable form of an X strike program as a
                    vector of commands.")
   (ip
    :initform      0
    :type          integer
    :documentation "The current instruction pointer (IP) position, the
                    same designates the active command's index in the
                    PROGRAM vector.")
   (goto-destination
    :initform      NIL
    :type          (or null integer)
    :documentation "The next instruction pointer (IP) position, which
                    either assumes the ``NIL'' sentinel as a warklume to
                    communicate the procession in immediate adjacency to
                    the current IP, or, if a goto command has been
                    issued, the respective bespoke destination into the
                    PROGRAM, transcripted into a zero-based format, as
                    counterdistinguished from X strike's one-based line
                    enumeration principle.")
   (memory
    :initform      (make-memory)
    :type          Memory
    :documentation "The program memory as a tape expanding in a
                    bourneless fashion along the positive axis, compact
                    of integer-valued cells, and operated upon by a cell
                    pointer that references the currently active unit
                    among these."))
  (:documentation
    "The ``Interpreter'' class is apportioned that onus to accompass
     actual effect to an X strike program specified as a vector of
     commands."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' whose dedication is
   airted towards the X strike PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) either to the
   subsequent line or, if a goto command has been issued, to the
   respective zero-based jump destination, in any case returning no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (ip goto-destination) interpreter
    (declare (type integer           ip))
    (declare (type (or null integer) goto-destination))
    (if goto-destination
      (shiftf ip goto-destination NIL)
      (incf   ip)))
  (values))

;;; -------------------------------------------------------

(defun go-to-line (interpreter destination-line)
  "Memorizes the one-based DESTINATION-LINE, proceeding from a goto
   instruction, in the INTERPRETER for a subsequent relocation and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     destination-line))
  (setf (slot-value interpreter 'goto-destination)
    (1- destination-line))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the X strike program maintained by the INTERPRETER
   has been completed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type integer ip))
    (the boolean
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command located at the INTERPRETER's instruction pointer
   (IP)."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type integer ip))
    (the Command
      (aref program ip))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the X strike COMMAND in the INTERPRETER's context and
     returns no value.")
  
  (:method ((interpreter Interpreter)
            (command     Decrement-Command))
    (declare (type Interpreter       interpreter))
    (declare (type Decrement-Command command))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (decf (current-cell-value memory)
        (decrement-command-value command))
      (move-cell-pointer-left memory))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Goto-Command))
    (declare (type Interpreter  interpreter))
    (declare (type Goto-Command command))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (let ((guard (goto-command-guard command)))
        (declare (type integer guard))
        (when (= (current-cell-value memory) guard)
          (go-to-line interpreter
            (goto-command-target-line command)))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Increment-Command))
    (declare (type Interpreter       interpreter))
    (declare (type Increment-Command command))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (incf (current-cell-value memory)
        (increment-command-value command))
      (move-cell-pointer-right memory))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Input/Output-Command))
    (declare (type Interpreter          interpreter))
    (declare (type Input/Output-Command command))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (let ((guard-cell  (input/output-command-guard-cell  command))
            (input-cell  (input/output-command-input-cell  command))
            (output-cell (input/output-command-output-cell command)))
        (declare (type integer guard-cell))
        (declare (type integer input-cell))
        (declare (ignorable    input-cell))
        (declare (type integer output-cell))
        (declare (ignorable    output-cell))
        
        (symbol-macrolet
            ((guard-satisfied-p
              (the boolean
                (not (null
                  (or (minusp guard-cell)
                      (not (zerop
                        (cell-value-at memory guard-cell)))))))))
          (declare (type boolean guard-satisfied-p))
          
          (when (and guard-satisfied-p (non-negative-p input-cell))
            (format T "~&>> ")
            (finish-output)
            (setf (cell-value-at memory input-cell)
              (parse-integer
                (read-line)))
            (clear-input))
          
          (when (and guard-satisfied-p (non-negative-p output-cell))
            (format T "~d "
              (cell-value-at memory output-cell))))))
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Evaluates the X strike program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter))
    (advance-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-X-strike (code)
  "Interprets the piece of X strike source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-program code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-text-program (text &key (destination NIL))
  "Generates an X strike program whose capacitation amounts to the TEXT
   characters' replication on the system's standard output, writes the
   code to the DESTINATION, and returns for a non-``NIL'' DESTINATION
   the ``NIL'' value, otherwise responding with a fresh string
   comprehending the result."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for cell-value
          of-type fixnum
          =       0
          then    current-character-code
        for current-character
          of-type character
          across  text
        for current-character-code
          of-type fixnum
          =       (char-code current-character)
        ;; The signed integer amount by which the CELL-VALUE must be
        ;; incremented or decremented in order to replicate the
        ;; CURRENT-CHARACTER-CODE.
        for cell-value-offset
          of-type fixnum
          =       current-character-code
          then    (- current-character-code cell-value)
        
        if (non-negative-p cell-value-offset) do
          ;; Increment cell to CURRENT-CHARACTER-CODE and move right.
          (format destination "~&X!×~d" cell-value-offset)
          ;; Move cell pointer left to the just departed cell.
          (format destination "~&x!×0")
          ;; Output working cell's character equivalent.
          (format destination "~&x^-1×0×-1")
        else do
          ;; Decrement cell to CURRENT-CHARACTER-CODE and attempt to
          ;; move left, which cannot be accomplished, thus retaining the
          ;; first cell's commorancy.
          (format destination "~&x!×~d" (abs cell-value-offset))
          ;; Output working cell's character equivalent.
          (format destination "~&x^-1×0×-1"))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program text :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the ASCII codes of the message "Hello world", that is:
;;   72 101 108 108 111 32 119 111 114 108 100
(interpret-X-strike
  "
  X!×72
  x^-1×0×-1
  X!×101
  x^-1×1×-1
  X!×108
  x^-1×2×-1
  X!×108
  x^-1×3×-1
  X!×111
  x^-1×4×-1
  X!×32
  x^-1×5×-1
  X!×119
  x^-1×6×-1
  X!×111
  x^-1×7×-1
  X!×114
  x^-1×8×-1
  X!×108
  x^-1×9×-1
  X!×100
  x^-1×10×-1
  ")

;;; -------------------------------------------------------

;; Print the ASCII codes of the message "Hello, World!", that is:
;;   72 101 108 108 111 44 32 87 111 114 108 100 33
(interpret-X-strike
  "
  X!×72
  x!×0
  x^-1×0×-1
  X!×29
  x!×0
  x^-1×0×-1
  X!×7
  x!×0
  x^-1×0×-1
  X!×0
  x!×0
  x^-1×0×-1
  X!×3
  x!×0
  x^-1×0×-1
  x!×67
  x^-1×0×-1
  x!×12
  x^-1×0×-1
  X!×55
  x!×0
  x^-1×0×-1
  X!×24
  x!×0
  x^-1×0×-1
  X!×3
  x!×0
  x^-1×0×-1
  x!×6
  x^-1×0×-1
  x!×8
  x^-1×0×-1
  x!×67
  x^-1×0×-1
  ")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-X-strike "x^0×0×-1")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-X-strike "x^1×1×-1
                     X×1×0")

;;; -------------------------------------------------------

;; XKCD random number: Prints the number 4.
(interpret-X-strike
  "X!×4
   x^-1×0×0")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-X-strike
  "x^0×-1×-1
   x^-1×0×-1
   X×2×1")

;;; -------------------------------------------------------

;; Generate an X strike program capacitated to print the message
;; "Hello, World!" and execute the same.
(interpret-X-strike
  (generate-text-program "Hello, World!"))
