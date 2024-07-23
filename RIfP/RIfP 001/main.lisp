;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "RIfP", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 6th, 2023, the diorism of which wones in its
;; reliance on string-valued variables whose modification by
;; substitutions and conditional goto redirections helms the program's
;; control flow.
;; 
;; 
;; Concept
;; =======
;; The RIfP programming language's foundry proceeds from the notion of
;; string-valued variables whose contents' substitutions in conjunction
;; with conditional code navigation establishes the claviger of its
;; efficacy.
;; 
;; == "RIfP": REPLACE IF POSSIBLE ==
;; The agnomination "RIfP" bewrays the paravaunt substance of the
;; language's as a warklume for the substitution of strings governed by
;; a variable's castaldy, ordained thus to "[R]eplace[If][P]ossible".
;; 
;; == THE MEMORY: STRING-VALUED VARIABLES ==
;; RIfP data castaldy wists of variables as the aefauld vessels for
;; the contents' maintenance, homologating an arbitrary mickleness'
;; participation in this regard by affiliating an identifier begotten
;; in its norning by the developer's personal deliberation as a handle
;; for a string of zero or more characters' ensconcement.
;; 
;; 
;; Instructions
;; ============
;; The RIfP programming language's instruction set enumerates a
;; quadruple cardinality, the perimeter of its competences amplecting
;; the declaration, substitution, and output of variables, as well as
;; an epiphenomenal jump-based control flow mechanism dependent upon
;; label names.
;; 
;; == OVERVIEW ==
;; The following apercu's onus shall be the adhibition of a cursory mete
;; of gnarity concerning the RIfP programming language's operative
;; faculties.
;; 
;; Please heed that succedaneous segments are underlined via a catena
;; of asterisks ("*") and intended for their subsequent substitution
;; by actual RIfP code in the program's ultimity.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ------------------------+-----------------------------------------
;;   @varName=value          | If a variable amenable to the {varName}
;;    ******* *****          | does not yet exist, declare such and
;;                           | initializes the same with the {value},
;;                           | otherwise overrides the already extant
;;                           | variable's content by the {value}.
;;                           |-----------------------------------------
;;                           | {varName} must be a valid variable
;;                           | identifier.
;;                           |-----------------------------------------
;;                           | {value} may be any string.
;;   ..................................................................
;;   !varName old=new target | If the value of the variable designated
;;    ******* *** *** ****** | by the {varName} contains the {old}
;;                           | string, substitutes its first occurrence
;;                           | by the {new} string and relocates the
;;                           | instruction pointer (IP) to the label
;;                           | amenable to the {target}; otherwise
;;                           | accompasses no causatum.
;;                           |-----------------------------------------
;;                           | {varName} must be a valid variable
;;                           | identifier.
;;                           |-----------------------------------------
;;                           | {old} may be any string.
;;                           |-----------------------------------------
;;                           | {new} may be any string. A value of "[]"
;;                           | is tantamount to an empty string.
;;                           |-----------------------------------------
;;                           | {target} must be a valid label name,
;;                           | designating a label which may or may not
;;                           | yet exist.
;;   ..................................................................
;;   $varName                | Prints the content of the variable
;;    *******                | answering to the {varName} to the
;;                           | standard output.
;;                           |-----------------------------------------
;;                           | {varName} must be a valid variable
;;                           | identifier.
;;   ..................................................................
;;   ;labelName              | Declares a label identified by the
;;    *********              | {labelName}.
;;                           |-----------------------------------------
;;                           | {labelName} must be a valid label name
;;                           | having not yet reserved.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-06-18
;; 
;; Sources:
;;   [esolang2024RIfP]
;;   The Esolang contributors, "RIfP", January 21st, 2024
;;   URL: "https://esolangs.org/wiki/RIfP"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose zero or more
   entries are composed of keys complying to the KEY-TYPE and values
   of the VALUE-TYPE, both defaulting to the comprehensive ``T''."
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
  "The ``program'' type defines an RIfP program as a one-dimensional
   simple array of ``Command'' objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines an association betwixt label names
   and their zero-based line numbers in an RIfP program, realized as a
   hash table, the keys of which contribute the label name strings and
   answer to values as fixnum line numbers."
  '(hash-table-of string fixnum))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines an association of variable names
   with their respective values, manifesting in a hash table, the keys
   of which impose strings, being allied to string values."
  '(hash-table-of string string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean tantamount to the OBJECT, proceeding from its
   \"generalized boolean\" construe, returning for a non-``NIL'' input
   the ``T'' value, otherwise responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE belongs to the diorism entailing the
   space characters, silicet, the space and horizontal tab entities,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun character-equals-p (first-character second-character)
  "Determines whether the FIRST-CHARACTER does not equal ``NIL'' and
   represents a character tantamount to the SECOND-CHARACTER, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (or null character) first-character))
  (declare (type character           second-character))
  (the boolean
    (get-boolean-value-of
      (and first-character
           (char= first-character second-character)))))

;;; -------------------------------------------------------

(defun get-character-at-index (source index)
  "Returns the character at the INDEX in the SOURCE, or responds with
   ``NIL'' upon the position's transcendence of the SOURCE's valid
   bournes."
  (declare (type string source))
  (declare (type fixnum index))
  (the (or null character)
    (when (array-in-bounds-p source index)
      (char source index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-spaces (subject)
  "Returns a fresh string obtained by removing all space and horizontal
   tab characters from the SUBJECT.
   ---
   The SUBJECT remains unaltered."
  (declare (type string subject))
  (the string
    (remove-if #'space-character-p subject)))

;;; -------------------------------------------------------

(defun empty-string-p (candidate)
  "Determines whether the CANDIDATE designates an empty string, either
   by its complete absence of any character, or by its conflation with
   the particular null string sentinel \"[]\", returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (or (zerop (length candidate))
          (every #'space-character-p candidate)
          (string= (remove-spaces candidate) "[]")))))

;;; -------------------------------------------------------

(defun locate-in-string (haystack needle)
  "Returns the first position of the NEEDLE in the string HAYSTACK, or
   responds with ``NIL'' upon its absence."
  (the (or null fixnum)
    (search needle haystack :test #'char=)))

;;; -------------------------------------------------------

(defun replace-substring (source start extent new-content)
  "Returns a fresh string which derives from the SOURCE, the EXTENT
   tally of character commencing from the START position being elided,
   and the disjoint SOURCE parcels spliced via the the NEW-CONTENT."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum extent))
  (declare (type string new-content))
  (let ((end (+ start extent)))
    (declare (type fixnum end))
    (the string
      (format NIL "~a~a~a"
        (subseq source 0 start)
        (or (and (empty-string-p new-content) "")
            new-content)
        (subseq source end)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface establishes a common foundry for all
   classes pursuing the representation of a RIfP instruction.")

;;; -------------------------------------------------------

(defstruct (Label-Declaration-Command
  (:include Command))
  "The ``Label-Declaration-Command'' class serves in the encapsulation
   of a RIfP instruction dedicated to the declaration of a label.
   ---
   The modeled RIfP instruction complies with the forbisen
     ;{labelName}
   where {labelName} designates the name of the label to establish."
  (label (error "Missing label name.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Output-Command
  (:include Command))
  "The ``Output-Command'' class serves in the encapsulation of a RIfP
   instruction dedicated to the printing of a variable's value.
   ---
   The modeled RIfP instruction complies with the forbisen
     ${variable}
   where {variable} designates a variable name."
  (variable (error "Missing variable.")
            :type      string
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Substitution-Command
  (:include Command))
  "The ``Substitution-Command'' serves in the encapsulation of a RIfP
   instruction dedicated to the substitution of a variable's value and
   a conditional label jump epiphenomenon.
   ---
   The modeled RIfP instruction complies with the forbisen
     !{variable} {needle}={substitute} {targetLabel}
   where {variable} designates the name of the variable to seek the
   {needle} in, on confirmation replacing the first occurrency by the
   {substitute}, ere relocating the control flow to the {targetLabel}."
  (variable     (error "Missing variable.")
                :type      string
                :read-only T)
  (needle       (error "Missing needle.")
                :type      string
                :read-only T)
  (substitute   (error "Missing substitute.")
                :type      string
                :read-only T)
  (target-label (error "Missing target label.")
                :type      string
                :read-only T))

;;; -------------------------------------------------------

(defstruct (Variable-Declaration-Command
  (:include Command))
  "The ``Variable-Declaration-Command'' class serves in the
   encapsulation of a RIfP instruction dedicated to the declaration of
   a variable.
   ---
   The modeled RIfP instruction complies with the forbisen
     @{variable}={value}
   where {variable} designates the variable name to declare, while
   {value} supplies its initial content."
  (variable (error "Missing declare command variable.")
            :type      string
            :read-only T)
  (value    (error "Missing declare command value.")
            :type      string
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Scanner" class.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      ""
    :type          string
    :documentation "The string to perquire.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current index into the SOURCE."))
  (:documentation
    "The ``Scanner'' class provides an entity endowed with such
     capacitation to permit the detection and extraction of significant
     content from a source string."))

;;; -------------------------------------------------------

(defmacro with-scanner ((scanner) &body body)
  "Evaluates the SCANNER, binds its slot ``source'' to the local symbol
   macro ``$source'' and ``position'' to ``$position'', evaluates the
   BODY forms, and returns the desinent form's results."
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Scanner ,evaluated-scanner))
       (declare (ignorable    ,evaluated-scanner))
       (symbol-macrolet
           (($source
             (the string
               (slot-value ,evaluated-scanner 'source)))
            ($position
             (the fixnum
               (slot-value ,evaluated-scanner 'position)))
            ($character
             (the (or null character)
               (when (array-in-bounds-p $source $position)
                 (char $source $position)))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defun advance-to-next-character (scanner)
  "Advances the SCANNER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (when (array-in-bounds-p $source $position)
      (incf $position)))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more accolent spaces and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop while (and $character (space-character-p $character)) do
      (advance-to-next-character scanner)))
  (values))

;;; -------------------------------------------------------

(defun expect-character (scanner expected-character)
  "Determines whether the character at the current position into the
   SCANNER's source matches the EXPECTED-CHARACTER, returning on
   confirmation no value, while concomitantly advancing the position
   cursor to the next location in the same; otherwise signals an error
   of an unspecified type."
  (declare (type Scanner   scanner))
  (declare (type character expected-character))
  (with-scanner (scanner)
    (cond
      ((null $character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered the source exhausted."
          expected-character $position))
      ((not (character-equals-p $character expected-character))
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-character $position $character))
      (T
        (advance-to-next-character scanner))))
  (values))

;;; -------------------------------------------------------

(defun expect-assignment (scanner)
  "Determines whether the character at the current position into the
   SCANNER's source represents an equality symbol (\"=\"), returning on
   confirmation no value, while concomitantly advancing the position
   cursor to the next location in the same; otherwise signals an error
   of an unspecified type."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (case $character
      ((NIL)
        (error "Expected a \"=\" at position ~d, but encountered the ~
                source exhausted."
          $position))
      (#\=
        (advance-to-next-character scanner))
      (otherwise
        (error "Expected a \"=\" at position ~d, but encountered ~
                \"~c\"."
          $position $character))))
  (values))

;;; -------------------------------------------------------

(defun expect-spacing (scanner)
  "Determines whether the character at the current position into the
   SCANNER's source represents a space, returning on confirmation no
   value, while concomitantly advancing the position cursor to the next
   non-space location in the same; otherwise signals an error
   of an unspecified type."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (cond
      ((null $character)
        (error "Expected a space at position ~d, but encountered the ~
                source exhausted."
          $position))
      ((space-character-p $character)
        (skip-spaces scanner))
      (T
        (error "Expected a space at position ~d, but encountered ~
                \"~c\"."
          $position $character))))
  (values))

;;; -------------------------------------------------------

(defun read-space-separated-word (scanner)
  "Proceeding from the current position into the SCANNER's source,
   reads a word whose desinence is either demarcated by a space
   character or the source's dextral bourne, returning a fresh string
   representation of the thus extracted token."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the string
      (subseq $source $position
        (setf $position
          (or (position-if #'space-character-p $source :start $position)
              (length $source)))))))

;;; -------------------------------------------------------

(defun read-left-hand-side (scanner)
  "Proceeding from the current position into the SCANNER's source,
   reads a word whose desinence is either demarcated by an equality sign
   (\"=\") or the source's dextral bourne, returning a fresh string
   representation of the thus extracted token."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the string
      (subseq $source $position
        (setf $position
          (or (position #\= $source :start $position :test #'char=)
              (length $source)))))))

;;; -------------------------------------------------------

(defun read-rest-of-source (scanner)
  "Proceeding from the current position into the SCANNER's source, its
   entirety and returns a fresh string representation of the thus
   processed portion."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the string
      (subseq $source $position
        (setf $position
          (length $source))))))

;;; -------------------------------------------------------

(defun source-exhausted-p (scanner)
  "Determines whether the SCANNER's position cursor has transgressed
   beyond its source's valid bournes, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Scanner scanner))
  (the boolean
    (get-boolean-value-of
      (null (slot-value scanner 'character)))))

;;; -------------------------------------------------------

(defun set-scanner-source (scanner new-source)
  "Alters the SCANNER's source to the NEW-SOURCE, resets the internally
   managed position cursor, and returns no value."
  (declare (type Scanner scanner))
  (declare (type string  new-source))
  (with-scanner (scanner)
    (psetf $source   new-source
           $position 0))
  (values))

;;; -------------------------------------------------------

(defun parse-substitution-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   parses and returns a ``Substitution-Command''."
  (declare (type Scanner scanner))
  (expect-character scanner #\!)
  (the Substitution-Command
    (make-substitution-command
      :variable
        (prog1
          (read-space-separated-word scanner)
          (expect-spacing scanner))
      :needle
        (prog1
          (read-left-hand-side scanner)
          (expect-assignment scanner))
      :substitute
        (prog1
          (read-space-separated-word scanner)
          (expect-spacing scanner))
      :target-label
        (read-rest-of-source scanner))))

;;; -------------------------------------------------------

(defun parse-variable-declaration-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   parses and returns a ``Variable-Declaration-Command''."
  (declare (type Scanner scanner))
  (expect-character scanner #\@)
  (the Variable-Declaration-Command
    (make-variable-declaration-command
      :variable
        (prog1
          (read-left-hand-side scanner)
          (expect-assignment   scanner))
      :value
        (read-rest-of-source scanner))))

;;; -------------------------------------------------------

(defun parse-output-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   parses and returns a ``Output-Command''."
  (declare (type Scanner scanner))
  (expect-character scanner #\$)
  (the Output-Command
    (make-output-command
      :variable (read-rest-of-source scanner))))

;;; -------------------------------------------------------

(defun parse-label-declaration-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   parses and returns a ``Label-Declaration-Command''."
  (declare (type Scanner scanner))
  (expect-character scanner #\;)
  (the Label-Declaration-Command
    (make-label-declaration-command
      :label (read-rest-of-source scanner))))

;;; -------------------------------------------------------

(defun parse-command (scanner)
  "Parses the SCANNER's source and returns an RIfP ``Command''
   representation of the entailed instruction, or, if same constitutes
   a blank line, responds with the ``NIL'' value."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (with-scanner (scanner)
    (the (or null Command)
      (case $character
        ;; No operation.
        ((NIL) NIL)
        
        ;; !variable needle=substitute targetLabel
        (#\! (parse-substitution-command scanner))
        
        ;; @variable=value
        (#\@ (parse-variable-declaration-command scanner))
        
        ;; $variableToPrint
        (#\$ (parse-output-command scanner))
        
        ;; ;labelDeclaration
        (#\; (parse-label-declaration-command scanner))
        
        (otherwise
          (error "The character \"~c\" does not introduce an RIfP ~
                  instruction."
            $character))))))

;;; -------------------------------------------------------

(defun parse-commands (source)
  "Extracts from the piece of RIfP SOURCE code the entailed instructions
   and returns a one-dimensional simple array of ``Command'' objects."
  (declare (type string source))
  (let ((scanner (make-instance 'Scanner)))
    (declare (type Scanner scanner))
    (with-input-from-string (input-stream source)
      (declare (type string-stream input-stream))
      (the program
        (coerce
          (loop
            for current-line
              of-type (or null string)
              =       (read-line input-stream NIL NIL)
            while current-line append
              (progn
                (set-scanner-source scanner current-line)
                (let ((current-command (parse-command scanner)))
                  (declare (type (or null Command) current-command))
                  (when current-command
                    (list current-command)))))
          '(simple-array Command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-label-table ()
  "Creates and returns an initially vacant ``label-table''."
  (the label-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun contains-label-p (labels label-name)
  "Determines whether the label table LABELS comprehends a label
   identified by the LABEL-NAME, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type label-table labels))
  (declare (type string      label-name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash label-name labels)))))

;;; -------------------------------------------------------

(defun register-label (labels label-name position)
  "Associates the POSITION in an RIfP program with the LABEL-NAME's
   declaration in the LABELS registry and returns no value.
   ---
   If the LABEL-NAME has already been declared, an error of an
   unspecified type is signaled."
  (declare (type label-table labels))
  (declare (type string      label-name))
  (declare (type fixnum      position))
  (if (contains-label-p labels label-name)
    (error "A label with the name ~s already exists." label-name)
    (setf (gethash label-name labels) position))
  (values))

;;; -------------------------------------------------------

(defun get-label-position (labels label-name)
  "Returns the position in the program corresponding to the LABEL-NAME
   as registered in the LABELS registry, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type label-table labels))
  (declare (type string      label-name))
  (multiple-value-bind (position contains-label-p)
      (gethash label-name labels)
    (declare (type (or null fixnum) position))
    (declare (type T                contains-label-p))
    (the fixnum
      (or (and contains-label-p position)
          (error "The label ~s has not yet been defined."
            label-name)))))

;;; -------------------------------------------------------

(defun collect-labels (program)
  "Returns a fresh ``label-table'' which associates the label names
   entailed in the PROGRAM with their zero-based locations."
  (declare (type program program))
  (let ((labels (make-empty-label-table)))
    (declare (type label-table))
    (loop
      for command  of-type Command across program
      and position of-type fixnum  from   0 by 1
      when (label-declaration-command-p command) do
        (register-label labels
          (label-declaration-command-label command)
          position))
    (the label-table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-variable-table ()
  "Creates and returns a fresh, initially vacant ``variable-table''."
  (the variable-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun contains-variable-p (variables name)
  "Determines whether the VARIABLES table comprehends a variable
   amenable to the NAME, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type variable-table variables))
  (declare (type string         name))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash name variables)))))

;;; -------------------------------------------------------

(defun get-variable-value (variables name)
  "Returns the value associated with the variable name in the VARIABLES
   table, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type variable-table variables))
  (declare (type string         name))
  (the string
    (or (and (contains-variable-p variables name)
             (gethash name variables))
        (error "Unrecognized variable name: ~s." name))))

;;; -------------------------------------------------------

(defun set-variable-value (variables name value)
  "Stores the VALUE in the variable amenable to the NAME in the
   VARIABLES, overriding any such entry if already extant, and returns
   no value."
  (declare (type variable-table variables))
  (declare (type string         name))
  (declare (type string         value))
  (setf (gethash name variables) value)
  (values))

;;; -------------------------------------------------------

(defun locate-in-variable (variables name needle)
  "Returns the first position of the NEEDLE in the content of the
   variable designated by the NAME among the VARIABLES, or responds with
   ``NIL'' upon its absence."
  (the (or null fixnum)
    (search needle
      (get-variable-value variables name)
      :test #'char=)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing RIfP program.")
    :type          program
    :documentation "The RIfP program as a vector of instructions.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The zero-based index of the currently processed
                    command in the PROGRAM.")
   (labels
    :initform      (make-empty-label-table)
    :type          label-table
    :documentation "Affiliates the label names with the zero-based
                    locations of their declaration in the PROGRAM.")
   (variables
    :initform      (make-empty-variable-table)
    :type          variable-table
    :documentation "Maps the declared variable names to their string
                    values."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of accompassing
     actual efficacy to a parsed RIfP program."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slot ``program'' to the local
   symbol macro ``$program'', ``ip'' to ``$ip'', ``labels'' to
   ``$labels'', and ``variables'' to ``$variables'', evaluates the BODY
   forms, and returns the desinent form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($program
             (the program
               (slot-value ,evaluated-interpreter 'program)))
            ($ip
             (the fixnum
               (slot-value ,evaluated-interpreter 'ip)))
            ($labels
             (the label-table
               (slot-value ,evaluated-interpreter 'labels)))
            ($variables
             (the variable-table
               (slot-value ,evaluated-interpreter 'variables))))
         (declare (type program        $program))
         (declare (ignorable           $program))
         (declare (type fixnum         $ip))
         (declare (ignorable           $ip))
         (declare (type label-table    $labels))
         (declare (ignorable           $labels))
         (declare (type variable-table $variables))
         (declare (ignorable           $variables))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Garners and registers all label declarations exposed in the RIfP
   program subjected to the INTERPRETER's castaldy and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $labels
      (collect-labels $program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the RIfP
   PROGRAM's processing."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the RIfP command in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (command     Variable-Declaration-Command))
    (declare (type Interpreter                  interpreter))
    (declare (type Variable-Declaration-Command command))
    (with-interpreter (interpreter)
      (set-variable-value $variables
        (variable-declaration-command-variable command)
        (variable-declaration-command-value    command))
      (incf $ip))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Substitution-Command))
    (declare (type Interpreter          interpreter))
    (declare (type Substitution-Command command))
    (with-interpreter (interpreter)
      (let ((needle   (substitution-command-needle   command))
            (variable (substitution-command-variable command)))
        (declare (type string needle))
        (declare (type string variable))
        (let ((needle-position
                (locate-in-variable $variables variable needle)))
          (declare (type (or null fixnum) needle-position))
          (cond
            (needle-position
              (set-variable-value $variables variable
                (replace-substring
                  (get-variable-value $variables variable)
                  needle-position
                  (length needle)
                  (substitution-command-substitute command)))
              (setf $ip
                (get-label-position $labels
                  (substitution-command-target-label command))))
            (T
              (incf $ip))))))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Output-Command))
    (declare (type Interpreter    interpreter))
    (declare (type Output-Command command))
    (with-interpreter (interpreter)
      (format T "~a"
        (get-variable-value $variables
          (output-command-variable command)))
      (incf $ip))
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Label-Declaration-Command))
    (declare (type Interpreter               interpreter))
    (declare (type Label-Declaration-Command command))
    (declare (ignore                         command))
    (with-interpreter (interpreter)
      (incf $ip))
    (values)))

;;; -------------------------------------------------------

(defun program-execution-completed-p (interpreter)
  "Determines whether the INTERPRETER program's execution has been
   completed, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (the boolean
      (get-boolean-value-of
        (>= $ip (length $program))))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the RIfP program governed by the INTERPRETER and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (loop until (program-execution-completed-p interpreter) do
      (process-command interpreter
        (aref $program $ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-RIfP (code)
  "Interprets the piece of RIfP source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-RIfP
  "@hw=Hello, World!
   $hw")

;;; -------------------------------------------------------

;; Truth-machine with simulated input of zero (0).
(interpret-RIfP
  "@inp=0
   ;loop
   $inp
   !inp 1=1 loop")

;;; -------------------------------------------------------

;; Truth-machine with simulated input of one (1).
(interpret-RIfP
  "@inp=1
   ;loop
   $inp
   !inp 1=1 loop")

;;; -------------------------------------------------------

;; "Cyclic Tag" interpreter.
(interpret-RIfP
  "
  @data=^1#
  ;replaceprogram
  @program=^011;10;101#
  ;interpret
  $data
  !data ^#=# halt
  !program ^0=0^ is0
  !program ^1=1^ is1
  !program ^;=;^ is;
  !program ^#=# loop
  ;is;
  !data ^1=^ interpret
  !data ^0=^ interpret
  ;is0
  !data ^1=^1 append0
  !data ^0=^0 interpret
  ;is1
  !data ^1=^1 append1
  !data ^0=^0 interpret
  ;append1
  !data 0#=01# interpret
  !data 1#=11# interpret
  ;append0
  !data 0#=00# interpret
  !data 1#=10# interpret
  ;loop
  !data #=# replaceprogram
  ")
