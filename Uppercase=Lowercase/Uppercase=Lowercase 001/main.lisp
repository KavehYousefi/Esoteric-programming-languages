;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Uppercase=Lowercase", invented by the Esolang user
;; "ChuckEsoteric08" and presented on May 1st, 2023, the dioristic
;; element of its haecceity, besides the similitude to assembly
;; languages, appertains to the equiparation of upper-case and
;; lower-case characters as majuscles for input and output purposes.
;; 
;; 
;; Concept
;; =======
;; The Uppercase=Lowercase programming language assumes a guise akin to
;; an assembler language, capacitated to increment and decrement an
;; infinite tape of cells, these being amenable to indices starting with
;; one (1), performing conditional goto operations via labels, and
;; issuing character-based input and output.
;; 
;; Its kenspeckle attribute, whence emanates its euonym, maintains its
;; woning in the fact that, during input and output, letters are handled
;; as majuscles, that is, upper-case and lower-case conflate into the
;; former appearance.
;; 
;; 
;; Architecture
;; ============
;; Uppercase=Lowercase's memory model employs an infinite tally of
;; cells amenable to integer indices greater than or equal to one (1),
;; but not impounded towards the upper march.
;; 
;; Each cell, at its inchoation set to zero (0), may store a integer
;; scalar of any sign and magnitude.
;; 
;; 
;; Data Types
;; ==========
;; The type system ostends a bivial exercise, supplemented to the
;; paravant integer type's compernage the parhedral ASCII character
;; repertoire for communication purposes.
;; 
;; == SIGNED INTEGERS OCCUPY A PARAVANT ECHOLON ==
;; The integer type admits both negative and positive values, without
;; a constraint's imposition upon their mickleness.
;; 
;; == CHARACTERS OPERATE ALONG THE INPUT/OUTPUT CONDUITS ==
;; The character species, its reification accounting for the ASCII
;; repertoire, occupies the communiation channels.
;; 
;; 
;; Syntax
;; ======
;; An Uppercase=Lowercase program is compact of zero or more
;; instructions, each introduced via a three-character identifier, and
;; succeeded by one or more operands, desumed from its integer and
;; string species. Each token is separated from the other by at least
;; one whitespace. A sui generis property of the language applies to
;; the input and output conduits, which equiparate both minuscular and
;; majuscular entities as upper-case characters.
;; 
;; == GRAMMAR ==
;; Uppercase=Lowercase's donat shall be subjected to a more formal
;; species of description by adminiculum of the Extended Backus-Naur
;; Form (EBNF):
;; 
;;   program          := padding
;;                    ,  [ command ]
;;                    ,  padding
;;                    ,  { separator , command }
;;                    ,  padding
;;                    ;
;;   
;;   command          := labelDeclaration
;;                    |  decrement
;;                    |  increment
;;                    |  input
;;                    |  output
;;                    ;
;;   
;;   labelDeclaration := "lbl" , separator , labelName ;
;;   decrement        := "dec"
;;                    ,  separator , cellOperand
;;                    ,  separator , numericOperand
;;                    ,  separator , labelName
;;                    ;
;;   increment        := "inc"
;;                    ,  separator , cellOperand
;;                    ,  separator , numericOperand
;;                    ;
;;   input            := "inp" , separator , cellOperand ;
;;   output           := "out" , separator , cellOperand ;
;;   
;;   numericOperand   := integer , cellOperand ;
;;   cellOperand      := cellIndex | cellReference ;
;;   cellReference    := "*" , digit , { digit } ;
;;   cellIndex        := digit , { digit } ;
;;   labelName        := integer | string ;
;;   
;;   string           := firstStringChar , { subseqStringChar } ;
;;   firstStringChar  := letter | underline ;
;;   subseqStringChar := letter | underline | digit ;
;;   underline        := "_" ;
;;   integer          := [ "+" | "-" ] , digit , { digit } ;
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9"
;;                    ;
;;   
;;   padding          := [ separator ] ;
;;   separator        := whitespace , { whitespace } ;
;;   whitespace       := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A quintuple tally of members exhausts Uppercase=Lowercase's
;; instruction set, incorporating basic arithmetics, conditional goto
;; control flow, as well as character input and output.
;; 
;; == OPERANDS ==
;; The various operand types' excellent role vindicates a section of its
;; own in order to communicate a sufficently designed intelligence.
;; 
;; Please heed that placeholder sections are underlined with a double
;; underline ("="), expected to be supplanted by valid
;; Uppercase=Lowercase code:
;; 
;;   ------------------------------------------------------------------
;;   Operand   | Syntax  | Description
;;   ----------+---------+---------------------------------------------
;;   Integer   | digits  | An integer literal, optionally preceded by a
;;             | ======  | sign, "+" or "-", and composed of one or
;;             |         | more decimal {digits}.
;;             |         |---------------------------------------------
;;             |         | Depending on the context, the value may
;;             |         | either be construed as a literal number, a
;;             |         | cell index --- imposing a strictly positive
;;             |         | value greater than or equal to one (1) ---,
;;             |         | or a label identifier.
;;   ..................................................................
;;   Reference | *digits | An integer value, unsigned and composed of
;;             |  ====== | one or more decimal {digits}, and always
;;             |         | employed in the agency of a memory cell
;;             |         | reference.
;;             |         |---------------------------------------------
;;             |         | If used in the context of an integer
;;             |         | literal, the value instead selects the cell
;;             |         | at the index {digits}.
;;             |         |---------------------------------------------
;;             |         | If employed in a cell index situation, an
;;             |         | indirect reference is assumed, that is, the
;;             |         | value of the cell at the index {digits} is
;;             |         | queried and used itself as the ultimate cell
;;             |         | cell index to obtain, namely:
;;             |         |   let actualIndex <- memory[{digits}]
;;             |         |   let result      <- memory[actualIndex]
;;             |         | Or, more compendious:
;;             |         |   let result <- memory[memory[{digits}]]
;;   ..................................................................
;;   String    | chars   | A string literal of one or more characters,
;;             | =====   | the first of which must constitute a Latin
;;             |         | or underscore ("_"), followed by zero or
;;             |         | more letters, underscores, or decimal
;;             |         | digits.
;;             |         |---------------------------------------------
;;             |         | String literals are exclusively utilized for
;;             |         | label identifiers.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; The five commands servicable to the programmer are listed in the
;; apercu below:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   lbl name       | Declares a label identified by the {name}.
;;       ****       |-------------------------------------------------
;;                  | The {name} must an integer or string literal.
;;   ..................................................................
;;   inc x y        | Increments the value of the cell {x} by {y}.
;;       * *        |--------------------------------------------------
;;                  | {x} must be an integer literal or cell reference.
;;   ..................................................................
;;   dec x y target | If the value of the cell {x} is less than {y},
;;       * * ****** | jumps to the label identified by the {target};
;;                  | otherwise decrements the cell {x} by {y}.
;;                  |--------------------------------------------------
;;                  | {x} must be an integer literal or cell reference.
;;                  |--------------------------------------------------
;;                  | {y} must be an integer literal or cell reference.
;;                  |--------------------------------------------------
;;                  | {target} must be an integer or string literal.
;;   ..................................................................
;;   inp x          | Queries the standard input for an ASCII
;;       *          | character, converts it to upper-case, and stores
;;                  | its resulting character code in the cell {x}.
;;                  |--------------------------------------------------
;;                  | {x} must be an integer literal or cell reference.
;;   ..................................................................
;;   out x          | Prints the ASCII character whose code corresponds
;;       *          | to the value of the cell {x} to the standard
;;                  | output, contingently converting the character to
;;                  | upper-case, if not already transpired, ere
;;                  | issuing the display.
;;                  |--------------------------------------------------
;;                  | {x} must be an integer literal or cell reference.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the eath nature of its subject, the Uppercase=Lowercase
;; protolog is inflicted with a few elements of ambiguity, a subset of
;; which shall be the following treatise's cynosure.
;; 
;; == WHICH REGULATIONS APPLY TO LABEL IDENTIFIERS? ==
;; The original specification desists from an accoutrement regarding the
;; goto label's designation; a consectary of this, the requirements for
;; its agnomination remain ensconced in a form of crepuscle.
;; 
;; It has been adjudged, ensuing from experience and the absence of
;; ambiguous inflictions, to homologate both integer numbers and strings
;; composed of letters, decimal digits, and the underscore ("_") to
;; partake in such a composition. The first and third of species may be
;; produced at a name's inchoation, whereas the zero or more subsequent
;; elements will be desumed from the entire treble.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in the programming language
;; Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-17
;; 
;; Sources:
;;   [esolang2023Uppercase=Lowercase]
;;   The Esolang contributors, "Uppercase=Lowercase", May 1st, 2023
;;   URL: "https://esolangs.org/wiki/Uppercase%3DLowercase"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized variants of
   instruction types."
  '(member
    :dec
    :inc
    :inp
    :lbl
    :out))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Uppercase=Lowercase
   program as a vector of zero or more ``Instruction'' objects."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines a valid index into the program memory
   as a positive integer number greater than or equal to zero, but
   unconstrained regarding the upper extremum."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, the keys of which conform to the KEY-TYPE and associate
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype label-name ()
  "The ``label-name'' type defines the valid set of objects homologated
   to represent label names, which includes integers and strings."
  '(or integer string))

;;; -------------------------------------------------------

(deftype label-map ()
  "The ``label-map'' type defines a mapping of label names to the point
   of their declaration, implemented as a hash table that associates the
   ``label-name'' with the ``fixnum'' position in the instruction
   vector."
  '(hash-table-of label-name fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface serves in the description of an instruction
   operand.")

;;; -------------------------------------------------------

(defstruct (Numeric-Operand
  (:include Operand))
  "The ``Numeric-Operand'' interface extends the ``Operand'' by its
   dedication to ligating concrete operands of numeric value.")

;;; -------------------------------------------------------

(defstruct (Integer-Operand
  (:include     Numeric-Operand)
  (:constructor make-integer-operand (value)))
  "The ``Integer-Operand'' class serves in the encapsulation of an
   integer literal, as opposed to directly expressed references."
  (value (error "Missing value.") :type integer))

;;; -------------------------------------------------------

(defstruct (Reference-Operand
  (:include     Numeric-Operand)
  (:constructor make-reference-operand (cell)))
  "The ``Reference-Operand'' class serves in the encapsulation of an
   explicitly stated cell reference, introduced via an asterisk \"*\" in
   the Uppercase=Lowercase source code."
  (cell (error "Missing value.") :type cell-index))

;;; -------------------------------------------------------

(defstruct (String-Operand
  (:include     Operand)
  (:constructor make-string-operand (name)))
  "The ``String-Operand'' class serves in the encapsulation of a
   non-numeric identifier, restricted to the usance in the context of a
   label name."
  (name (error "Missing name.") :type string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface accoutres a common base for all classes
   intent on representing Uppercase=Lowercase operations.")

;;; -------------------------------------------------------

(defstruct (Decrement-Instruction
  (:include     Instruction)
  (:constructor make-decrement-instruction
                  (minuend subtrahend label-name)))
  "The ``Decrement-Instruction'' encapsulates the information necessary
   for replicating a decrementation (\"dec\") operation's invocation."
  (minuend    (error "Missing minuend.")    :type Numeric-Operand)
  (subtrahend (error "Missing subtrahend.") :type Numeric-Operand)
  (label-name (error "Missing label name.") :type Operand))

;;; -------------------------------------------------------

(defstruct (Increment-Instruction
  (:include     Instruction)
  (:constructor make-increment-instruction (augend addend)))
  "The ``Increment-Instruction'' encapsulates the information necessary
   for replicating an incrementation (\"inc\") operation's invocation."
  (augend (error "Missing augend.") :type Numeric-Operand)
  (addend (error "Missing addend.") :type Numeric-Operand))

;;; -------------------------------------------------------

(defstruct (Input-Instruction
  (:include     Instruction)
  (:constructor make-input-instruction (cell)))
  "The ``Input-Instruction'' encapsulates the information necessary for
   replicating an input (\"inp\") operation's invocation."
  (cell (error "Missing cell.") :type Numeric-Operand))

;;; -------------------------------------------------------

(defstruct (Label-Declaration-Instruction
  (:include     Instruction)
  (:constructor make-label-declaration-instruction (label-name)))
  "The ``Label-Declaration-Instruction'' encapsulates the information
   necessary for replicating a label declaration (\"lbl\") operation's
   invocation."
  (label-name (error "Missing label name.") :type Operand))

;;; -------------------------------------------------------

(defstruct (Output-Instruction
  (:include     Instruction)
  (:constructor make-output-instruction (cell)))
  "The ``Output-Instruction'' encapsulates the information necessary for
   replicating an output (\"out\") operation's invocation."
  (cell (error "Missing cell.") :type Numeric-Operand))



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

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign, that
   is, either \"+\" (plus) or \"-\" (minus), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun label-name-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid label name
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (char= candidate #\_))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token parsing operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-command-type (token)
  "Interprets and the TOKEN as a command type, or instruction name, and
   returns the associated ``command-type'' identifier; signaling an
   error of an unspecified type upon its ineligibility."
  (declare (type string token))
  (the command-type
    (cond
      ((string= token "dec") :dec)
      ((string= token "inc") :inc)
      ((string= token "inp") :inp)
      ((string= token "lbl") :lbl)
      ((string= token "out") :out)
      (T (error "Unrecognized command name: ~s." token)))))

;;; -------------------------------------------------------

(defun parse-operand (token)
  "Parses the TOKEN as an operand and returns an ``Operand''
   representation thereof."
  (declare (type string token))
  (the Operand
    (cond
      ((zerop (length token))
        (error "Empty operand token."))
      ((char= (char token 0) #\*)
        (make-reference-operand
          (parse-integer token :start 1)))
      ((digit-char-p (char token 0))
        (make-integer-operand
          (parse-integer token)))
      ((sign-character-p (char token 0))
        (make-integer-operand
          (parse-integer token)))
      ((every #'label-name-character-p token)
        (make-string-operand token))
      (T
        (error "Invalid operand token: ~s." token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-token (source start)
  "Proceeding from the START position into the SOURCE, reads and returns
   a token extending immediately before the first whitespace character
   or the end of the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (content (make-string-output-stream))
      (declare (type string-stream content))
      (loop
        for position of-type fixnum from start below (length source)
        for current-character of-type character = (char source position)
        until (whitespace-character-p current-character)
        do    (write-char current-character content)
        finally
          (return
            (values
              (get-output-stream-string content)
              position))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the location of the
   first non-whitespace character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION in the SOURCE, or ``NIL'' if
   the location transcends the SOURCE's boundaries."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun expect-separator (source start)
  "Proceeding from the START position into the SOURCE, expects at least
   one whitespace character, on confirmation skipping a sequence of one
   or more whitespaces and returning the location in the SOURCE of the
   first non-whitespace character; otherwise signaling an error of an
   unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-character
          (get-character-at source start)))
    (declare (type (or null character) current-character))
    (the fixnum
      (cond
        ((null current-character)
          (error "Expected a whitespace, but encountered EOF at ~
                  position ~d."
            start))
        ((not (whitespace-character-p current-character))
          (error "Expected a whitespace, but encountered \"~c\" at ~
                  position ~d."
            current-character start))
        (T
          (skip-whitespaces source start))))))

;;; -------------------------------------------------------

(defun read-command-type (source start)
  "Proceeding from the START position into the SOURCE, reads the next
   token, interprets it as an instruction name, and returns the
   corresponding ``command-type''."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (command-type-token end)
      (read-token source start)
    (declare (type string command-type-token))
    (declare (type fixnum end))
    (the (values command-type fixnum)
      (values
        (parse-command-type command-type-token)
        end))))

;;; -------------------------------------------------------

(defun expect-command-coda (source start)
  "Proceeding from the START position into the SOURCE, expects an
   instruction's concluding section, either resolving to the immediate
   end of the SOURCE, or to a sequence of one or more whitespaces,
   adminicular to a potential successor operation, on confirmation
   returning the position of the end of the SOURCE, or that following
   the trailing whitespaces; otherwise an error of an unspecified type
   is signaled."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-character
          (get-character-at source start)))
    (declare (type (or null character) current-character))
    (the fixnum
      (cond
        ((null current-character)
          start)
        ((whitespace-character-p current-character)
          (skip-whitespaces source start))
        (T
          (error "Expected whitespaces or the end of file to succeed ~
                  a command, but encountered \"~c\" at position ~d."
            current-character start))))))

;;; -------------------------------------------------------

(defun read-instruction (source start)
  "Proceeding from the START position into the SOURCE, reads an
   instruction, composed of the command name and one or more operands,
   and returns two values:
     (1) The parsed ``Instruction''.
     (2) The position into the SOURCE immediately following the
         instruction definition, that is, the command name, the
         operands, and, if the SOURCE is not yet exhausted, a sequence
         of one or more adjacent whitespaces."
  (declare (type fixnum start))
  (let ((position start))
    (declare (type fixnum position))
    (flet
        ((consume-operand (&optional (expected-operand-class 'Operand))
          "Consumes the next token, interprets it as an instruction
           operand, and determines whether it conforms to the
           EXPECTED-OPERAND-CLASS, on confirmation returning the
           operand, otherwise signaling an error of an unspecified
           type."
          (declare (type symbol expected-operand-class))
          (setf position (expect-separator source position))
          (let ((operand-token ""))
            (declare (type string operand-token))
            (multiple-value-setq (operand-token position)
              (read-token source position))
            (let ((operand (parse-operand operand-token)))
              (declare (type Operand operand))
              (the Operand
                (if (typep operand expected-operand-class)
                  operand
                  (error "Expected an operand of the type ~s, ~
                          but received ~s."
                    expected-operand-class operand)))))))
      
      (let ((command-type NIL))
        (declare (type (or null command-type) command-type))
        (multiple-value-setq (command-type position)
          (read-command-type source start))
        
        (the (values Instruction fixnum)
          (values
            (case command-type
              (:dec
                (make-decrement-instruction
                  (consume-operand 'Numeric-Operand)
                  (consume-operand 'Numeric-Operand)
                  (consume-operand 'Operand)))
              
              (:inc
                (make-increment-instruction
                  (consume-operand 'Numeric-Operand)
                  (consume-operand 'Numeric-Operand)))
              
              (:inp
                (make-input-instruction
                  (consume-operand 'Numeric-Operand)))
              
              (:lbl
                (make-label-declaration-instruction
                  (consume-operand 'Operand)))
              
              (:out
                (make-output-instruction
                  (consume-operand)))
              
              (otherwise
                (error "Unrecognized command type: ~s." command-type)))
            
            (expect-command-coda source position)))))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts the instructions from the piece of Uppercase=Lowercase
   SOURCE code and returns these as a one-dimensional simple array of
   ``Instruction''s."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet
        ((process-whitespaces ()
          "Proceeding from the current POSITION in the SOURCE, skips a
           sequence of zero or more accolent whitespaces, updates the
           POSITION to the first non-whitespace location, and returns no
           value."
          (setf position
            (skip-whitespaces source position))
          (values))
         
         (process-instruction (new-instruction new-position)
          "Updates the POSITION cursor to the NEW-POSITION and returns
           the NEW-INSTRUCTION."
          (declare (type Instruction new-instruction))
          (declare (type fixnum      new-position))
          (setf position new-position)
          (the Instruction new-instruction)))
      (the program
        (loop
          initially
            (process-whitespaces)
          while
            (< position (length source))
          collect
            (multiple-value-call #'process-instruction
              (read-instruction source position))
          into
            instructions
          finally
            (return
              (coerce instructions
                '(simple-array Instruction (*)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class occupies the wike of a program's memory, a
   composition of a theoreticaly infinite tally of cells, amenable to
   one-based indices, and each an aefauld signed integer's salvatory.
   ---
   The storage entity is implemented as a sparse vector, based upon a
   hash table, the keys of which furnish the cell indices, whereas the
   values store the cell datum."
  (cells (make-hash-table :test #'eql)
         :type (hash-table-of cell-index integer)))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the value of the MEMORY cell addressed by the INDEX."
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (the integer
    (gethash index
      (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell addresssed by the INDEX and
   returns no value."
  (declare (type integer    new-value))
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (setf (gethash index
          (memory-cells memory) 0)
        new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label map.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-label-map ()
  "Creates and returns an empty ``label-map''."
  (the label-map
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun label-map-register (label-map name position)
  "Affiliates the label NAME with its POSITION in an Uppercase=Lowercase
   program's instruction sequence, stores this correspondence in the
   LABEL-MAP, and returns no value."
  (declare (type label-map  label-map))
  (declare (type label-name position))
  (setf (gethash name label-map) position)
  (values))

;;; -------------------------------------------------------

(defun label-map-get-position (label-map name)
  "Returns the position in the Uppercase=Lowercase instruction sequence
   allied with the label NAME as registered at the LABEL-MAP, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type label-map  label-map))
  (declare (type label-name name))
  (the fixnum
    (or (gethash name label-map)
        (error "No label with the name ~s defined." name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Instruction-Visitor".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction-Visitor
  "The ``Instruction-Visitor'' interface establishes a common foundry
   for all entities pursuing the traversal of an Uppercase=Lowercase
   program's instructions.")

;;; -------------------------------------------------------

(defgeneric visitor-process-instruction (visitor instruction)
  (:documentation
    "Processes the INSTRUCTION in the VISITOR's context and returns
     no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of labeler.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Labeler
  (:include     Instruction-Visitor)
  (:constructor make-labeler (program)))
  "The ``Labeler'' class implements the ``Instruction-Visitor''
   interface in order to collect and register labels, the thus produced
   mapping is offered to interested entities.
   ---
   The ``Labeler'''s services are intended to apply in the fashion of a
   preprocessor, inducing effect in the intermede of an interpreter's
   creation and its instructions' evaluation. In concrete diction, an
   ``Interpreter'' class' instantiation is followed by the ``Labeler'''s
   generation and application on the same instructions, the compiled
   label map of which enters into the interpreter, ere the same naits
   this piece of gnarity for its own traversal of the program."
  (program (error "Missing program.") :type program)
  (ip      0                          :type fixnum)
  (labels  (build-label-map)          :type label-map))

;;; -------------------------------------------------------

(defun labeler-finished-p (labeler)
  "Determines whether the LABELER has executed all of its instructions,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Labeler labeler))
  (the boolean
    (not (null
      (>= (labeler-ip labeler)
          (length (labeler-program labeler)))))))

;;; -------------------------------------------------------

(defgeneric derive-label-name (operand)
  (:documentation
    "Produces and returns a label name from the OPERAND's state.")
  
  (:method ((operand Integer-Operand))
    (declare (type Integer-Operand operand))
    (the integer
      (integer-operand-value operand)))
  
  (:method ((operand String-Operand))
    (declare (type String-Operand operand))
    (the string
      (string-operand-name operand)))
  
  (:method ((operand T))
    (declare (type T operand))
    (error "Cannot derive a label name from the operand ~s." operand)))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((labeler     Labeler)
     (instruction Label-Declaration-Instruction))
  (declare (type Labeler                       labeler))
  (declare (type Label-Declaration-Instruction instruction))
  (label-map-register
    (labeler-labels labeler)
    (derive-label-name
      (label-declaration-instruction-label-name instruction))
    (labeler-ip labeler))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((labeler     Labeler)
     (instruction Instruction))
  (declare (type Labeler     labeler))
  (declare (ignore           labeler))
  (declare (type Instruction instruction))
  (declare (ignore           instruction))
  (values))

;;; -------------------------------------------------------

(defun labeler-define-labels (labeler)
  "Processes the LABELER's instructions in order to gather the label
   definitions, which are returned in a ``label-map''."
  (declare (type Labeler labeler))
  (loop until (labeler-finished-p labeler) do
    (visitor-process-instruction labeler
      (aref (labeler-program labeler)
        (labeler-ip labeler)))
    (incf (labeler-ip labeler)))
  (the label-map
    (labeler-labels labeler)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:include     Instruction-Visitor)
  (:constructor make-interpreter (program)))
  "The ``Interpreter'', implementing the ``Instruction-Visitor''
   interface, furnishes an entity amenable to accompassing operational
   effect to a sequence of Uppercase=Lowercase instructions."
  (program (error "Missing program.") :type program)
  (ip      0                          :type fixnum)
  (labels  (build-label-map)          :type label-map)
  (memory  (make-memory)              :type memory))

;;; -------------------------------------------------------

(defun interpreter-finished-p (interpreter)
  "Determines whether the INTERPRETER's operation has ceased by a
   causatum of its instruction pointer's (IP) exhaustion of the
   maintained program, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-program interpreter)))))))

;;; -------------------------------------------------------

(defun interpreter-current-instruction (interpreter)
  "Returns the INTERPRETER's currently active instruction, specified by
   its instruction pointer (IP), or signals an error of an unspecified
   type upon the latter's transgression of the instruction vector's
   boundaries."
  (declare (type Interpreter interpreter))
  (the Instruction
    (aref
      (interpreter-program interpreter)
      (interpreter-ip      interpreter))))

;;; -------------------------------------------------------

(defun interpreter-go-to-label (interpreter label-name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the position
   in its program associated with the LABEL-NAME and returns no value.
   ---
   If no corresponding label exists, an error of an unspecified type is
   signaled."
  (declare (type Interpreter interpreter))
  (declare (type label-name  label-name))
  (setf (interpreter-ip interpreter)
    (label-map-get-position
      (interpreter-labels interpreter)
      label-name))
  (values))

;;; -------------------------------------------------------

(defgeneric resolve-operand-value (interpreter operand)
  (:documentation
    "Returns the OPERAND's value in the INTERPRETER's context.")
  
  (:method ((interpreter Interpreter)
            (operand     Integer-Operand))
    (declare (type Interpreter     interpreter))
    (declare (ignore               interpreter))
    (declare (type Integer-Operand operand))
    (the integer
      (integer-operand-value operand)))
  
  (:method ((interpreter Interpreter)
            (operand     Reference-Operand))
    (declare (type Interpreter       interpreter))
    (declare (type Reference-Operand operand))
    (the integer
      (memory-cell-at
        (interpreter-memory interpreter)
        (reference-operand-cell operand))))
  
  (:method ((interpreter Interpreter)
            (operand     String-Operand))
    (declare (type Interpreter    interpreter))
    (declare (ignore              interpreter))
    (declare (type String-Operand operand))
    (the string
      (string-operand-name operand))))

;;; -------------------------------------------------------

(defun interpreter-cell-at (interpreter operand)
  "Returns the cell stored in the INTERPRETER's memory which answers to
   the OPERAND's value."
  (declare (type Interpreter     interpreter))
  (declare (type Numeric-Operand operand))
  (the integer
    (memory-cell-at
      (interpreter-memory interpreter)
      (resolve-operand-value interpreter operand))))

;;; -------------------------------------------------------

(defun (setf interpreter-cell-at) (new-value interpreter operand)
  "Stores the NEW-VALUE in the INTERPRETER memory's cell specified by
   the OPERAND's value and returns no value."
  (declare (type integer         new-value))
  (declare (type Interpreter     interpreter))
  (declare (type Numeric-Operand operand))
  (setf (memory-cell-at
          (interpreter-memory interpreter)
          (resolve-operand-value interpreter operand))
        new-value)
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((interpreter Interpreter)
     (instruction Decrement-Instruction))
  (declare (type Interpreter           interpreter))
  (declare (type Decrement-Instruction instruction))
  (let ((x (interpreter-cell-at interpreter
             (decrement-instruction-minuend instruction)))
        (y (resolve-operand-value interpreter
             (decrement-instruction-subtrahend instruction))))
    (declare (type integer x))
    (declare (type integer y))
    (if (< x y)
      (interpreter-go-to-label interpreter
        (resolve-operand-value interpreter
          (decrement-instruction-label-name instruction)))
      (decf
        (interpreter-cell-at interpreter
          (decrement-instruction-minuend instruction))
        y)))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((interpreter Interpreter)
     (instruction Increment-Instruction))
  (declare (type Interpreter           interpreter))
  (declare (type Increment-Instruction instruction))
  (incf (interpreter-cell-at interpreter
          (increment-instruction-augend instruction))
        (resolve-operand-value interpreter
          (increment-instruction-addend instruction)))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((interpreter Interpreter)
     (instruction Input-Instruction))
  (declare (type Interpreter       interpreter))
  (declare (type Input-Instruction instruction))
  (setf (interpreter-cell-at interpreter
          (input-instruction-cell instruction))
        (prog2
          (format T "~&>> ")
          (char-code
            (char-upcase
              (read-char)))
          (clear-input)))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((interpreter Interpreter)
     (instruction Label-Declaration-Instruction))
  (declare (type Interpreter                   interpreter))
  (declare (ignore                             interpreter))
  (declare (type Label-Declaration-Instruction instruction))
  (declare (ignore                             instruction))
  (values))

;;; -------------------------------------------------------

(defmethod visitor-process-instruction
    ((interpreter Interpreter)
     (instruction Output-Instruction))
  (declare (type Interpreter        interpreter))
  (declare (type Output-Instruction instruction))
  (format T "~:@(~c~)"
    (code-char
      (interpreter-cell-at interpreter
        (output-instruction-cell instruction))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the Uppercase=Lowercase program maintained by the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  
  ;; Collect all label declarations employing the ``Labeler'' class'
  ;; services.
  (setf (interpreter-labels interpreter)
    (labeler-define-labels
      (make-labeler
        (interpreter-program interpreter))))
  
  ;; Process the instruction.
  (loop until (interpreter-finished-p interpreter) do
    (visitor-process-instruction interpreter
      (interpreter-current-instruction interpreter))
    (incf (interpreter-ip interpreter)))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Uppercase=Lowercase (code)
  "Interprets the piece of Uppercase=Lowercase source CODE and returns
   no value."
  (interpreter-interpret
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Uppercase=Lowercase
  "inp 1
   out 1")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Uppercase=Lowercase
  "lbl start
   inp 1
   out 1
   dec 1 256 start")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Uppercase=Lowercase
  "
  inp 1
  inc 2 *1
  
  dec 1 48 terminate_on_error
  dec 1 1  end_if_zero
  
  lbl repeat_if_one
  out 2
  dec 2 50 repeat_if_one
  
  lbl end_if_zero
  out 2
  
  lbl terminate_on_error
  ")

;;; -------------------------------------------------------

;; Print the message "HELLO, WORLD!" by employing indirect referencing
;; and jump-based iteration.
;; 
;; The memory layout assumes the following:
;;   memory[ 1] = 72 (= ASCII code of "H")
;;   memory[ 2] = 69 (= ASCII code of "E")
;;   memory[ 3] = 76 (= ASCII code of "L")
;;   memory[ 4] = 76 (= ASCII code of "L")
;;   memory[ 5] = 79 (= ASCII code of "O")
;;   memory[ 6] = 44 (= ASCII code of ",")
;;   memory[ 7] = 32 (= ASCII code of " ")
;;   memory[ 8] = 87 (= ASCII code of "W")
;;   memory[ 9] = 79 (= ASCII code of "O")
;;   memory[10] = 82 (= ASCII code of "R")
;;   memory[11] = 76 (= ASCII code of "L")
;;   memory[12] = 68 (= ASCII code of "D")
;;   memory[13] = 33 (= ASCII code of "!")
;;   
;;   memory[14] = 1  (keeps indirect reference to memory indices
;;                    from 1 to inclusive 13; accessing the actual
;;                    cell memory[memory[14]] prints the respective
;;                    message character)
;;   memory[15] = 14 (decrementing counter which tallies the number of
;;                    printed characters; terminates by jumping to
;;                    the "end" label if memory[15] <= 0)
(interpret-Uppercase=Lowercase
  "
  inc  1 72
  inc  2 69
  inc  3 76
  inc  4 76
  inc  5 79
  inc  6 44
  inc  7 32
  inc  8 87
  inc  9 79
  inc 10 82
  inc 11 76
  inc 12 68
  inc 13 33
  
  inc 14 1
  inc 15 12
  
  lbl print
  out *14
  inc  14  1
  dec  15  1 end
  dec  15 15 print
  
  lbl end
  ")
