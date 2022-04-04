;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements an interpreter for the esoteric programming
;; language "Something?Oops!", a creation by the Esolang user "A", and
;; endowed with a syntax as potent a diorism as the mode of its
;; construe.
;; 
;; Concept
;; =======
;; Something?Oops! constitutes an esoteric programming language imbued
;; with variables as the sole data conditory, and exercising effect by
;; the interplay of the arguments in the context of a strictly
;; homogenous syntax. A uniform ilk of statement exists, the utilization
;; of the variables, each a vehicle to a special reaction in response to
;; its reading and a second to its modification, the employment and
;; combination of which lends multifarious functionality to the
;; instructions.
;; 
;; == EACH STATEMENT OBEYS TO THE SAME PATTERN ==
;; If any primality shall be apportioned significance as the manifest of
;; the language's diorism, Something?Oops! ought to be meted against its
;; unwaiving conformance to a stringent syntax anenst its instructions.
;; Any statement is expressed by a single pattern
;;   
;;   {leftOperand} {rightOperand}Oops!{target}
;; 
;; where the {leftOperand} and the {rightOperand} ought to be
;; substituted either by a nonnegative integer or one of the four
;; recognized variable names. The {target} may merely assume the latter
;; option. The application of the variables, in particular the dichotomy
;; regarding their reading or writing, furcates into the various
;; discriminated effects.
;; 
;; == NONNEGATIVE INTEGERS ESTABLISH THE CURRENCY ==
;; The manifestation of data is restricted to the management of
;; nonnegative integer objects, either stated as literals, stored in
;; variables for later retrieval or particular side effects, requested
;; from the user as a token of his involvement, or subjected to a
;; verbatim reproduction in the standard output. The lower march of zero
;; does not correlate to any constraint on the opposite laterality,
;; leaving a theoretical range of [0, +infinity].
;; 
;; == FUNCTIONALITY MEANS SIDE EFFECTS IN ACCESSING VARIABLES ==
;; The donat promulgated by Something?Oops! in reference to its variable
;; system defines a set exhausted by four members, each of whom is
;; apportioned a single character's agnomination, and, in conjunction
;; with the nominal aspect, a particular agency. The arbitrariness
;; inherent in commonly apprehended notions of "variables" does not
;; reverberate without stipulations in this language, as each
;; participant of this group partakes of a unique haecceity, the same
;; emanates as a dependent of the concrete access mode, that is, in
;; response to being either read or updated.
;; 
;; The following table shall be ordained to a quick yet comprehensible
;; apercu of the variables and their resonance:
;;   
;;   Variable | Role                | Description
;;   ---------+---------------------+----------------------------------
;;    !       | normal variable     | On reading returns its current
;;            |                     | value.
;;            |                     | On writing changes its value.
;;   ..................................................................
;;    ?       | input               | On reading queries the user for
;;            |                     | a nonnegative integer number
;;            |                     | and it.
;;            |                     | On writing does not produce any
;;            |                     | effect, abstaining from changing
;;            |                     | itself.
;;   ..................................................................
;;    .       | output              | On reading returns its current
;;            |                     | value.
;;            |                     | On writing changes its value and
;;            |                     | concomitantly prints this new
;;            |                     | datum to the standard output.
;;   ..................................................................
;;    ,       | instruction counter | On reading replies with the
;;            |                     | current instruction (line) index.
;;            |                     | On writing jumps to the
;;            |                     | designated instruction (line).
;;            |                     | The numbering is one-based.
;; 
;; A more functionality-oriented reformulation as a distinguished aspect
;; of the above table shall be the following enumeration's object:
;;   
;;   - If intending to read or write a variable without further effects,
;;     please use the "!" variable.
;;   - If intending to request a numeric user input, please read the "?"
;;     variable.
;;   - If intending to print a value, please write to the "." variable.
;;   - If intending to request the current line number, please read the
;;     "," variable.
;;   - If intending to jump to a specific line (instruction), please
;;     write to the "," variable.
;; 
;; Operating a program thus describes the resolution of harnessing the
;; variables' side effects by insertion as arguments into the uniform
;; syntax.
;; 
;; 
;; Architecture
;; ============
;; The language's nature does incorporate the concept for any particular
;; architecture's accommodation. The variables' paravaunt significance,
;; however, offer an aperture for contemplations regarding the avail of
;; their castaldy --- yet without the imposition of stipulations.
;; 
;; 
;; Data Types
;; ==========
;; Something?Oops!'s data manifestation ascribes to the exclusive
;; bailiwick of nonnegative integer values. By means of indirect
;; association, the variable registry establishes a data type of its
;; own, a reconditeness inhabits it by the abstract definition of its
;; intrinsics.
;; 
;; == NONNEGATIVE INTEGERS ==
;; The currency of any transaction in the compass of the language,
;; materializing in literal indications, tokens along the input and
;; output conduits, instruction pointer trackings, as well as variable
;; storages, partakes of operations in the form of nonnegative integer
;; objects, thus spanning the range [0, +infinity].
;; 
;; == VARIABLE REGISTRY ==
;; The constrained circumference of variable contingencies, enumerated
;; already by the quadruple "!", "?", ".", and ",", does not levy an
;; imposition too fastidious upon its conception. This haecceity's
;; ultimate realization constitutes a dependency upon one's personal
;; propensity.
;; 
;; 
;; Syntax
;; ======
;; The language's stricture in construction, intimated already by the
;; apportionment of one line to each instruction, propagates into the
;; starkly ritualized weftage embodied in its constituents.
;; 
;; == GRAMMAR ==
;; An explication of the syntax is capacitated in the following Extended
;; Backus-Naur Form (EBNF):
;;   
;;   program    := [ statement , { newlines , statement } ] , { newline } ;
;;   statement  := identifier , space , identifier , "?Oops" , variable ;
;;   identifier := integer | variable ;
;;   integer    := digit , { digit } ;
;;   digit      := "0" | "1" | "2" | "3" | "4"
;;              |  "5" | "6" | "7" | "8" | "9" ;
;;   variable   := "!" | "?" | "." | "," ;
;;   space      := " " ;
;;   newlines   := newline , { newline } ;
;;   newline    := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The stringency applied to its design allots not imposition of
;; impotence upon the language's functional roster. Something?Oops!'s
;; versatility comprehend in its potentials input and output
;; intelligence, data storage by mediation of variables, as much as a
;; goto mechanism used to capacitate navigation across the instructions.
;; 
;; == VARIABLES AND THEIR DEPLOYMENT DEFINE THE EFFECTS ==
;; A surrogate to the principle manifesting a piece of functionality in
;; a dedicated instruction, Something?Oops! aguises all commands into a
;; uniform weftage, the participants of which, being the four distinct
;; variables, each with side effects of its own issued during reading
;; from or writing to the selfsame, expose the various operations.
;; Whether a variable is applied in the context of its indagation or
;; subject to a modification depends upon its location inside of the
;; fixed syntax
;;   
;;   {leftOperand} {rightOperand}?Oops!{target}
;; 
;; A variable inserted into the {leftOperand} or {rightOperand} role
;; will be read, eventuating the particular causatum of this access
;; mode; its deployment as a {target}, on the other hand, refers to its
;; modification, the concomitant of that establishes the respective
;; epiphenomon.
;; 
;; A summary perspective on the behaviors of the variable quadruple in
;; the context of reading and writing a member shall be adduced below:
;;   
;;   Variable | Effect on reading         | Effect on writing          
;;   ---------+---------------------------+----------------------------
;;    !       | Return value.             | Set value.
;;   ..................................................................
;;    ?       | Prompt a nonnegative      | No effect.
;;            | integer from the user and | 
;;            | store it in the variable. | 
;;   ..................................................................
;;    .       | Return value.             | Set value and print the
;;            |                           | new value to the standard
;;            |                           | output.
;;   ..................................................................
;;    ,       | Return value. This always | Set value and move the
;;            | resolves to the one-based | instruction pointer to the
;;            | current line number.      | instruction corresponding
;;            |                           | to the new value.
;; 
;; A visual correlation betwixt the function avails of language and the
;; variables' agency shall be limned in the following table:
;;   
;;   Functionality | Variable
;;   --------------+---------------------------------------------------
;;    Store value  | To store a nonnegative integer value without
;;                 | further side effects, write to the "!" variable.
;;                 | You may retrieve the value by reading from "!".
;;   ..................................................................
;;    Input        | To query a nonnegative integer input from the
;;                 | user, read the "?" variable.
;;                 | This variable cannot be written to.
;;   ..................................................................
;;    Output       | To print a nonnegative integer number to the
;;                 | standard output, write to the "." variable.
;;                 | The output remains stored in this variable and may
;;                 | be retrieved by reading the "." variable.
;;   ..................................................................
;;    Get line     | To query the current instruction index, read the
;;                 | "," variable. The numbering starts at one (1).
;;   ..................................................................
;;    Goto         | To jump to a specific instruction, write the
;;                 | instruction index to the "," variable. The index
;;                 | starts at one (1).
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The ascertainment of its virtues, the concinnity commorant in the
;; persuit and achievement of both nexility and lucidity, does not
;; disencumber Something?Oops!'s specification from a few blemishes
;; inflicting its patration; a selection thereof shall be subject to
;; further delineation.
;; 
;; == WHICH DEFAULT VALUES APPLY TO THE VARIABLES? ==
;; The four exclusive participants in the definition of variables
;; constitute entities amenable to both reading and writing. Virtually
;; impeccable considering their effects, the treatise under
;; administration by the original specification can be wited with a
;; deficiency of supplying their default values. Natheless, an
;; extrapolation can be exercised with the aim of the following
;; establishments:
;;   
;;   - The variables "!", "?", and "." assume a default value of zero.
;;   - The variable "," defaults to the incipient line number: one.
;; 
;; == HOW ARE INSTRUCTIONS ENUMERATED? ==
;; By adminiculum of its instruction counter variable ",", a program
;; is endowed with the facility of navigating liberally across its
;; instructions, each a private line's occupant. Certitude, however,
;; eludes the definite enumeration of the lines regarding their starting
;; value as zero (0) or one (1). Based upon the truth-machine example,
;; and corroborated by other programming languages' consuetude, an
;; indexing beginning with one (1) for the first line is imputed.
;; 
;; 
;; Implementation
;; ==============
;; A compernage of its consumed subject, the interpreter merits its
;; credit as a plain solution paregal to the Something?Oops! programming
;; language itself. The source code, after being divided into lines,
;; experiences these constituents' ordered deliverance to a parsing
;; function, producing from each such row an instruction of three
;; components' contribution --- its arguments. The interpreter itself
;; operates on the resulting instruction vector to embue it with actual
;; effect.
;; 
;; == INTERPRETATION EMBRACES THREE STAGES ==
;; The interpretation of a program constitutes a process designated by
;; a treble graduation:
;;   
;;   (1) The Something?Oops! source code is split into lines.
;;   (2) Each non-negative line is parsed into an instruction and
;;       gleaned in a vector.
;;   (3) The instruction vector is processed by the interpreter.
;; 
;; == (1) A PROGRAM CONSISTS OF LINES ==
;; Its stringency and simplicity vindicates the avoidance of a lexical
;; analyzer for the language, and compels the implementation to directly
;; operate on a line produced by splitting the source code at
;; linebreaks.
;; 
;; == (2) LINES PARSE INTO INSTRUCTIONS ==
;; Each nonempty code row transliterates into a single ``Instruction''
;; class object, the components of the same resolve to ``Operand''
;; instances, themselves being delineated by a type to discriminate
;; betwixt an integer literal and a variable, as well as a value, for
;; the former the numeric datum, for the latter the identifier
;; character.
;; 
;; == (3) THE INTERPRETER CONSUMES THE INSTRUCTIONS ==
;; The generated instruction vector's dependent, the interpreter itself
;; maintains a particular set of facilities, including an instruction
;; pointer as a reference to the currently processed statement and the
;; variable registry as the placeholders' reification, associating with
;; any of the identifier quadruples the current value as a nonnegative
;; integer datum. An augmented degree of comfort in accessing the
;; variables is achieved by their storage in a hash table, mapping to a
;; variable name its content.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-03-31
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Something%3FOops!"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated to a
   value of the VALUE-TYPE, both defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype nonnegative-integer ()
  "The ``nonnegative-integer'' type defines an integer object with a
   minimum of zero, but unbounded toward the upper extremum."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype variable-registry ()
  "The ``variable-registry'' type defines the salvatory dedicated to the
   program variables' castaldy as a hash table of character keys
   associated with nonnegative integer values."
  '(hash-table-of character nonnegative-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "The ``Operand'' class describes an argument induced into an
   ``Instruction'' in order to compound into a statement."
  (type  (error "Missing operand type.") :type keyword)
  (value NIL                             :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction ()))
  "The ``Instruction'' class delineates a composite entailing the treble
   constituents of operands that in coefficiency define a statement."
  (left-operand  NIL :type (or null Operand))
  (right-operand NIL :type (or null Operand))
  (target        NIL :type (or null Operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variable-character-p (character)
  "Checks whether the CHARACTER represents a recognized variable name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean (not (null (find character "!?.," :test #'char=)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun instruction-line-p (line)
  "Checks whether the LINE is non-empty, that is, neither the ``NIL''
   object nor composed of whitespaces only, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string line))
  (the boolean
    (not (null (and line (notevery #'whitespace-character-p line))))))

;;; -------------------------------------------------------

(defun parse-line (line)
  "Parses an instruction LINE and creates and returns the
   ``Instruction'' representing ensconced statement."
  (declare (type string line))
  
  (let ((instruction (make-instruction))
        (position    0)
        (character   (char line 0)))
    (declare (type Instruction         instruction))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character in the LINE,
           if possible, updates the current CHARACTER, and returns no
           value."
          (setf character
            (when (< position (1- (length line)))
              (char line (incf position))))
          (values))
         
         (expect-character (expected-character)
          "Checks whether the current CHARACTER equals the
           EXPECTED-CHARACTER, on confirmation advancing the POSITION
           cursor to the next character in the LINE and returning no
           value, otherwise signaling an error."
          (declare (type character expected-character))
          (if (and character (char= character expected-character))
            (advance)
            (error "Expected the character '~c', but encountered '~c'."
              expected-character character))
          (values))
         
         (expect-space ()
          "Expects to encounter a space character at the current
           POSITION, on confirmation advancing the POSITION cursor to
           the next character in the LINE and returning no value,
           otherwise signaling an error."
          (expect-character #\Space)
          (values))
         
         (expect-string (expected-string)
          "Starting at the current POSITION, expects to find the
           EXPECTED-STRING, relocating the POSITION cursor on
           confirmation to the first character in the LINE succeeding
           the same and returning no value, otherwise signaling an
           error."
          (declare (type string expected-string))
          (loop
            for expected-character
              of-type character
              across  expected-string
            unless (and character (char= character expected-character))
              do (error "Could not find the string ~s." expected-string)
            do
              (advance))
          (values))
         
         (skip-spaces ()
          "Starting at the current POSITION, skips zero or more adjacent
           whitespaces, relocating the POSITION cursor to the first
           non-whitespace character, and returns no value."
          (loop
            while (and character (whitespace-character-p character))
            do    (advance))
          (values))
         
         (read-integer ()
          "Starting at the current POSITION, reads a nonnegative integer
           number and returns it in the form of an ``Operand''."
          (the Operand
            (make-operand :integer
              (parse-integer
                (with-output-to-string (digits)
                  (declare (type string-stream digits))
                  (loop
                    while (and character (digit-char-p character))
                    do
                      (write-char character digits)
                      (advance)))))))
         
         (read-variable ()
          "Starting at the current POSITION, reads the next character,
           interprets it as a variable name, and returns it in the form
           of an ``Operand''."
          (the Operand
            (make-operand :variable
              (prog1
                character
                (advance)))))
         
         (read-identifier ()
          "Starting at the current POSITION, either reads a nonnegative
           integer number or a variable and returns it in the form of
           an ``Operand''."
          (the Operand
            (cond
              ((digit-char-p character)
                (read-integer))
              ((variable-character-p character)
                (read-variable))
              (T
                (error "Invalid identifier character: ~s."
                  character))))))
      
      (skip-spaces)
      (setf (instruction-left-operand instruction) (read-identifier))
      (expect-space)
      (setf (instruction-right-operand instruction) (read-identifier))
      (expect-string "?Oops")
      (setf (instruction-target instruction) (read-variable))
      (skip-spaces))
    
    (the Instruction instruction)))

;;; -------------------------------------------------------

(defun generate-instructions (code)
  "Produces a one-dimensional simple array of ``Instruction'' objects
   from the piece of Something?Oops! CODE and returns the same.
   ---
   Please note that empty lines, entailing such being composites of
   whitespaces only, subscribe to no contribution to the resulting
   instruction vector."
  (declare (type string code))
  (with-input-from-string (code-stream code)
    (declare (type string-stream code-stream))
    (the (simple-array Instruction (*))
      (coerce
        (loop
          for line
            of-type (or null string)
            =       (read-line code-stream NIL)
          while line
          when (instruction-line-p line)
            collect (parse-line line))
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-variables ()
  "Creates and initializes the four program variables and returns them
   as part of a ``variable-registry''."
  (let ((variables (make-hash-table :test #'eql)))
    (declare (type variable-registry variables))
    (setf (gethash #\! variables) 0)
    (setf (gethash #\? variables) 0)
    (setf (gethash #\. variables) 1)
    (setf (gethash #\, variables) 0)
    (the variable-registry variables)))

;;; -------------------------------------------------------

(defun prompt-for-input ()
  "Repeatedly prompts the user for a nonnegative integer input until
   such is provided, in which instance it is returned.
   ---
   While the user communication proceeds along the standard input, the
   standard output is employed for displaying a prompt text,
   contingently multiple times."
  (flet ((integer-string-p (string)
          "Checks whether the STRING represents a nonnegative integer,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type (or null string) string))
          (the boolean
            (not (null
              (when string
                (let ((normalized-string
                        (string-trim '(#\Space #\Tab) string)))
                  (declare (type string normalized-string))
                  (and (plusp (length normalized-string))
                       (every #'digit-char-p normalized-string)))))))))
    (the nonnegative-integer
      (loop do
        (format T "~&Please enter a nonnegative integer: ")
        (let ((input (read-line)))
          (declare (type (or null string) input))
          (clear-input)
          (when (integer-string-p input)
            (return (parse-integer input))))))))

;;; -------------------------------------------------------

(defun interpret-instructions (instructions)
  "Interprets the Something?Oops! INSTRUCTIONS and returns no value."
  (declare (type (vector Instruction *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (variables   (build-variables)))
      (declare (type fixnum                ip))
      (declare (type (or null Instruction) instruction))
      (declare (type variable-registry     variables))
      
      (labels
          ((advance-to-next-line ()
            "Moves to the instruction pointer IP to the next line, if
             possible, updates the current INSTRUCTION, and returns no
             value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (move-to-line (line-index)
            "Moves the instruction pointer IP to the entry among the
             INSTRUCTIONS designated by the zero-based LINE-INDEX, and
             returns no value."
            (declare (type nonnegative-integer line-index))
            (setf ip line-index)
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (variable-value (variable-name)
            "Returns the value of the variable amenable to the
             VARIABLE-NAME."
            (declare (type character variable-name))
            (the nonnegative-integer
              (gethash variable-name variables)))
           
           ((setf variable-value) (new-value variable-name)
            "Sets the value of the variable amenable to the
             VARIABLE-NAME to the NEW-VALUE and returns no value."
            (declare (type nonnegative-integer new-value))
            (declare (type character           variable-name))
            (setf (gethash variable-name variables 0) new-value)
            (values))
           
           (read-variable (variable-name)
            "Returns the value of the variable designated by the
             VARIABLE-NAME, contingently effectuating its side effect."
            (declare (type character variable-name))
            (the nonnegative-integer
              (case variable-name
                ;; Return the variable value.
                (#\!
                  (variable-value variable-name))
                
                (#\?
                  (setf (variable-value variable-name)
                        (prompt-for-input))
                  (variable-value variable-name))
                
                ;; Return the variable value.
                (#\.
                  (variable-value variable-name))
                
                ;; Return the variable value.
                (#\,
                  (variable-value variable-name))
                
                (otherwise
                  (error "Attempt to read an unrecognized variable: ~s."
                    variable-name)))))
           
           (set-variable (variable-name new-value)
            "Updates the content of the variable designated by the
             VARIABLE-NAME to the NEW-VALUE, contingently effectuating
             its appertaining side effect, and returns no value."
            (declare (type character           variable-name))
            (declare (type nonnegative-integer new-value))
            
            (case variable-name
              ;; Update the variable value.
              (#\!
                (setf (variable-value variable-name) new-value))
              
              ;; Cannot set the user input variable.
              (#\?
                NIL)
              
              ;; Update the variable value and print it to the console.
              (#\.
                (setf (variable-value variable-name) new-value)
                (format T "~d " new-value))
              
              ;; Update the variable value and employ it as the new
              ;; instruction pointer.
              (#\,
                (setf (variable-value variable-name) new-value)
                (move-to-line (1- new-value)))
              
              (otherwise
                (error "Attempt to set an unrecognized variable: ~s."
                  variable-name)))
            
            (values))
           
           (update-instruction-counter-variable ()
            "Updates the value of the instruction counter variable
             \",\" to reflect the one-based current line number and
             returns no value."
            (setf (variable-value #\,) (1+ ip))
            (values))
           
           (resolve-operand (operand)
            "Returns the OPERAND's value as a nonnegative integer.
             ---
             If the OPERAND's nature subscribes to a variable,
             additional side effects may be produced."
            (declare (type Operand operand))
            (the nonnegative-integer
              (case (operand-type operand)
                (:integer
                  (operand-value operand))
                (:variable
                  (read-variable (operand-value operand)))
                (otherwise
                  (error "Invalid operand: ~s." operand))))))
        
        (loop while instruction do
          (update-instruction-counter-variable)
          
          (let ((left-operand  (instruction-left-operand  instruction))
                (right-operand (instruction-right-operand instruction))
                (target        (instruction-target        instruction)))
            (declare (type Operand left-operand))
            (declare (type Operand right-operand))
            (declare (type Operand target))
            
            (let ((left-argument  (resolve-operand left-operand))
                  (right-argument (resolve-operand right-operand)))
              (declare (type nonnegative-integer left-argument))
              (declare (type nonnegative-integer right-argument))
              
              (set-variable (operand-value target)
                (+ left-argument
                   right-argument))
              
              (unless (char= (operand-value target) #\,)
                (advance-to-next-line))))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Something?Oops! (code)
  "Interprets the piece of Something?Oops! CODE and returns no value."
  (declare (type string code))
  (interpret-instructions
    (generate-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-instructions
  (generate-instructions
   "? 2?Oops,
    0 5?Oops,
    0 1?Oops.
    0 3?Oops,
    0 0?Oops."))

;;; -------------------------------------------------------

;; Employ the instruction counter (line numbering variable ",") for
;; printing the numbers 1, 2, 3.
(interpret-Something?Oops!
  "0 ,?Oops.
   0 ,?Oops.
   0 ,?Oops.")
