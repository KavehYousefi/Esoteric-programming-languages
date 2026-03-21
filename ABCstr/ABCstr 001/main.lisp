;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ABCstr", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 23rd, 2023, its designment's entheus the "Novice"
;; language of the user "Keymaker", with a diorism's woning in the
;; repeated modulation of the program's memory, a string, via
;; substitutions, operating in champarty with a label-based control flow
;; duction mechanism for its entelechy's chevisance.
;; 
;; 
;; Concept
;; =======
;; The ABCstr programming language constitutes a specimen whose
;; dioristic contribution appertains to the modulation of the aefauld
;; memory component, a string, through substitutions, wisting as
;; conditional epiphenomena of input, output, or control flow duction
;; facilities, the latter based upon program labels.
;; 
;; 
;; Instructions
;; ============
;; The ABCstr instruction set is edified upon a murnival in membership,
;; a trisculc, forming the preponderance among the mountance, nuncupated
;; to the substitution of substrings in the program memory, while an
;; aefauld specimen's wike is governed by the definition of labels.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall the satisfied in a cursory mete
;; of nortelry's adhibition concerning the language's operative
;; warklumes, ere further details' vouchsafements will be proffered in
;; the subsequent sections.
;; 
;; Please heed the demarcation of succedaneum tmemata by a catena
;; composed of asterisks ("*"), their parcels being intended for a
;; supersession by actual ABCstr code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command              | Effect
;;   ---------------------+--------------------------------------------
;;   delendum=succedaneum | If the {delendum} is entailed in the
;;   ******** *********** | memory, substitutes its first occurrency
;;                        | by the {succedaneum} and advances to the
;;                        | next command; otherwise, relocates the
;;                        | instruction pointer (IP) to the line
;;                        | designated by the label amenable to the
;;                        | {delendum}. If no such label exists, simply
;;                        | advances the instruction pointer to the
;;                        | next command.
;;                        |--------------------------------------------
;;                        | The {delendum} must constitute zero or more
;;                        | characters, except for newline entities and
;;                        | the equality sign "=".
;;   ..................................................................
;;   delendum-succedaneum | If the {delendum} is entailed in the
;;   ******** *********** | memory, substitutes its first occurrency
;;                        | by the {succedaneum}, prints the {delendum}
;;                        | to the standard output conduit, and
;;                        | advances to the next command; otherwise,
;;                        | relocates the instruction pointer (IP) to
;;                        | the line designated by the label amenable
;;                        | to the {delendum}. If no such label exists,
;;                        | simply advances the instruction pointer to
;;                        | the next command.
;;                        |--------------------------------------------
;;                        | The {delendum} must constitute zero or more
;;                        | characters, except for newline entities and
;;                        | the equality sign "-".
;;   ..................................................................
;;   :delendum            | If the {delendum} is entailed in the
;;    ********            | memory, queries the standard input conduit
;;                        | for a line of characters, serving as the
;;                        | succedaneum, substitutes the {delendum}'s
;;                        | first occurrency by the succedaneum, and
;;                        | advances to the next command; otherwise,
;;                        | relocates the instruction pointer (IP) to
;;                        | the line designated by the label amenable
;;                        | to the {delendum}. If no such label exists,
;;                        | simply advances the instruction pointer to
;;                        | the next command.
;;                        |--------------------------------------------
;;                        | The {delendum} must constitute zero or more
;;                        | characters, except for newline entities,
;;                        | the equality sign "-", the hyphen "-", and
;;                        | the colon ":".
;;   ..................................................................
;;   labelName            | Defines a new label amenable to the name
;;   *********            | {labelName}, intended for future
;;                        | relocations of the instruction pointer
;;                        | (IP).
;;                        |--------------------------------------------
;;                        | The {labelName} must constitute zero or
;;                        | more characters, except for newline
;;                        | entities, the equality sign "-", the hyphen
;;                        | "-", and the colon ":".
;;   ------------------------------------------------------------------
;; 
;; == DETAILED EXPOSITION ==
;; A more potent mete of gnarity's adhibition concerning the operative
;; avails shall be assigned to the subsequent sections.
;; 
;; --------------------------------------------------------------------
;; 
;; == SUBSTITUTE BY LITERAL OR JUMP ==
;; Substitution by a literal.
;; 
;; Syntax:
;;   delendum=succedaneum
;;   ******** ***********
;; 
;; Summary:
;;   Replaces a string by another one, if possible; otherwise, jumps to
;;   a label.
;; 
;; Description:
;;   If the {delendum} can be found in the current memory string,
;;   locates its first occurrence, substitutes thilk by the
;;   {succedaneum}, and advances to the subsequent instruction in the
;;   program. Otherwise, upon the {delendum}'s absence from the memory,
;;   relocates the instruction pointer (IP) to the label norned by the
;;   {succedaneum}. If no label amenable to the {delendum}'s name can be
;;   located, the instruction pointer simply advances to the next
;;   command.
;;   
;;   The {delendum} must constitute a sequence enumerating zero or more
;;   characters, with the following set's membership deprived of its
;;   homologation:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "="        (ASCII code: 61).
;;   
;;   The {succedaneum} must constitute a sequence enumerating zero or
;;   more characters, with the following set's membership deprived of
;;   its homologation:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "="        (ASCII code: 61).
;; 
;; Pseucode:
;;   if memory contains delendum then
;;     let position <- index of delendum in memory
;;     memory[position, position + length(delendum)] <- succedaneum
;;     ip                                            <- ip + 1
;;   else if label with name succedaneum exists then
;;     ip <- position of succedaneum label in program
;;   else
;;     ip <- ip + 1
;;   end if
;; 
;; Exceptional situations:
;;   - None.
;; 
;; Side effects:
;;   - If the {delendum} exists in the memory string, its first
;;     occurrence therein is replaced by the {succedaneum}.
;;   - If the {delendum} does not exist in the memory string, the
;;     instruction pointer (IP) is redirected to the label nevened by
;;     {delendum}.
;; 
;; Notes:
;;   - Only the first occurrence of the {delendum} in the memory string
;;     will be substituted by the succedaneum, if present.
;; 
;; --------------------------------------------------------------------
;; 
;; == SUBSTITUTE BY LITERAL AND PRINT, OR JUMP ==
;; Substitution by a literal with concomitant printing of the replaced
;; datum.
;; 
;; Syntax:
;;   delendum-succedaneum
;;   ******** ***********
;; 
;; Summary:
;;   Replaces a string by another one, while printing the replaced
;;   content, if possible; otherwise, jumps to a label.
;; 
;; Description:
;;   If the {delendum} can be found in the current memory string,
;;   locates its first occurrence, prints the {delendum} to the standard
;;   output conduit, substitutes thilk by the {succedaneum}, and
;;   advances to the subsequent instruction in the program. Otherwise,
;;   upon the {delendum}'s absence from the memory, relocates the
;;   instruction pointer (IP) to the label norned by the {succedaneum}.
;;   If no label amenable to the {delendum}'s name can be located, the
;;   instruction pointer simply advances to the next command.
;;   
;;   The {delendum} must constitute a sequence enumerating zero or more
;;   characters, with the following set's membership deprived of its
;;   homologation:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "-"        (ASCII code: 45).
;;   
;;   The {succedaneum} must constitute a sequence enumerating zero or
;;   more characters, with the following set's membership deprived of
;;   its homologation:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "-"        (ASCII code: 45).
;; 
;; Pseucode:
;;   if memory contains delendum then
;;     print delendum
;;     let position <- index of delendum in memory
;;     memory[position, position + length(delendum)] <- succedaneum
;;     ip                                            <- ip + 1
;;   else if label with name succedaneum exists then
;;     ip <- position of succedaneum label in program
;;   else
;;     ip <- ip + 1
;;   end if
;; 
;; Exceptional situations:
;;   - None.
;; 
;; Side effects:
;;   - If the {delendum} exists in the memory string, it its printed to
;;     the standard output conduit.
;;   - If the {delendum} exists in the memory string, its first
;;     occurrence therein is replaced by the {succedaneum}.
;;   - If the {delendum} does not exist in the memory string, the
;;     instruction pointer (IP) is redirected to the label nevened by
;;     {delendum}.
;; 
;; Notes:
;;   - The {delendum}, if found in the memory string, will be printed to
;;     the standard output conduit without any appendix; that is,
;;     neither a linebreak nor a space is appended.
;;   - Only the first occurrence of the {delendum} in the memory string
;;     will be substituted by the succedaneum, if present.
;; 
;; --------------------------------------------------------------------
;; 
;; == SUBSTITUTE BY INPUT OR JUMP
;; Substitution by a user input.
;; 
;; Syntax:
;;   :delendum
;;    ********
;; 
;; Summary:
;;   Replaces a string by the user input, if possible; otherwise, jumps
;;   to a label.
;; 
;; Description:
;;   If the {delendum} can be found in the current memory string,
;;   locates its first occurrence, queries the standard input conduit
;;   for a line of string, substitutes the {delendum} by this input, and
;;   advances to the subsequent instruction in the program. Otherwise,
;;   upon the {delendum}'s absence from the memory, relocates the
;;   instruction pointer (IP) to the label norned by the user input. If
;;   no label amenable to the {delendum}'s name can be located, the
;;   instruction pointer simply advances to the next command.
;;   
;;   The {delendum} must constitute a sequence enumerating zero or more
;;   characters, with the following set's membership deprived of its
;;   homologation:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "-"        (ASCII code: 45).
;;     - The command identifier ":"        (ASCII code: 58).
;;     - The command identifier "="        (ASCII code: 61).
;; 
;; Pseucode:
;;   
;;   if memory contains delendum then
;;     let succedaneum <- query a line of string
;;     let position    <- index of delendum in memory
;;     memory[position, position + length(delendum)] <- succedaneum
;;     ip                                            <- ip + 1
;;   else if label with name delendum exists then
;;     ip <- position of delendum label in program
;;   else
;;     ip <- ip + 1
;;   end if
;; 
;; Exceptional situations:
;;   - None.
;; 
;; Side effects:
;;   - The standard input conduit is queried for a line.
;;   - If the {delendum} exists in the memory string, its first
;;     occurrence therein is replaced by the {succedaneum}.
;;   - If the {delendum} does not exist in the memory string, the
;;     instruction pointer (IP) is redirected to the label nevened by
;;     {delendum}.
;; 
;; Notes:
;;   - Only the first occurrence of the {delendum} in the memory string
;;     will be substituted by the succedaneum, if present.
;; 
;; --------------------------------------------------------------------
;; 
;; == DEFINE LABEL ==
;; Definition of a label as a jump target.
;; 
;; Syntax:
;;   label
;;   *****
;; 
;; Summary:
;;   Defines a line label in the program.
;; 
;; Description:
;;   Defines a label agnominated by {label} in the program, intended as
;;   an anchor for subsequent jump operations.
;;   
;;   A label name's componency admits any character except for the
;;   following specimens:
;;     - The newline or linefeed character (ASCII code: 10).
;;     - The vertical tabulation character (ASCII code: 11).
;;     - The form feed character           (ASCII code: 12).
;;     - The carriage return character     (ASCII code: 13).
;;     - The command identifier "-"        (ASCII code: 45).
;;     - The command identifier ":"        (ASCII code: 58).
;;     - The command identifier "="        (ASCII code: 61).
;; 
;; Pseucode:
;;   labelTable[label] <- position of label in program
;; 
;; Exceptional situations:
;;   - If a label amenable to the name {label} is defined twice or more
;;     times in the program, an error of the type "DuplicateLabelError"
;;     is signaled.
;; 
;; Side effects:
;;   - A new label is registered for the program.
;; 
;; Notes:
;;   None.
;; 
;; ====================================================================
;; 
;; == COMPLETE PSEUDOCODE ==
;; The following epexegesis shall filst in the quadruple operation set's
;; apprehension by a unifying pseudocode formulation's vouchsafement:
;; 
;; { Substitute by constant or jump. }
;; if line contains one "=" then
;;   let oldTermOrLabel <- nil   { Substring left  of the merist "=". }
;;   let newTerm        <- nil   { Substring right of the merist "=". }
;;   
;;   split line along "=" into oldTermOrLabel and newTerm
;;   
;;   if memory contains oldTermOrLabel then
;;     replace oldTermOrLabel in memory by newTerm
;;     advance to the next command
;;   else if label with the name oldTermOrLabel exists then
;;     jump to the line with the label oldTermOrLabel
;;   else
;;     advance to the next command
;;   end if
;; 
;; { Substitute by constant and print or jump. }
;; else if line contains one "-" then
;;   let oldTermOrLabel <- nil   { Substring left  of the merist "-". }
;;   let newTerm        <- nil   { Substring right of the merist "-". }
;;   
;;   split line along "-" into oldTermOrLabel and newTerm
;;   
;;   if memory contains oldTermOrLabel then
;;     replace oldTermOrLabel in memory by newTerm
;;     print oldTermOrLabel
;;     advance to the next command
;;   else if label with the name oldTermOrLabel exists then
;;     jump to the line with the label oldTermOrLabel
;;   else
;;     advance to the next command
;;   end if
;; 
;; { Substitute by input or jump. }
;; else if line starts with ":" then
;;   let oldTermOrLabel <- line without ":" prefix
;;   
;;   if memory contains oldTermOrLabel then
;;     let newTerm <- request a line from the standard input conduit
;;     replace oldTermOrLabel in memory by newTerm
;;     advance to the next command
;;   else if label with the name oldTermOrLabel then
;;     jump to the line with the label oldTermOrLabel
;;   else
;;     advance to the next command
;;   end if
;; 
;; { Label definition. }
;; else
;;   if line contains two or more "=" or "-" then
;;     error: invalid label name
;;   else
;;     let labelName <- line
;;     
;;     if labelName has already been defined then
;;       error: Duplicate label name
;;     else
;;       associate labelName with line number in program
;;     end if
;;   end if
;; end if
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand has been developed in the programming
;; language Common Lisp, its operation a gestion of a twyfold
;; conformation, the inchoacy of which exerts on the ABCstr source code
;; a transformation into a sequence of representative command objects,
;; ere the actual interpretation effort is invested.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-25
;; 
;; Sources:
;;   [esolang:2024:ABCstr]
;;   The Esolang contributors, "ABCstr", September 27th, 2024
;;   URL: "https://esolangs.org/wiki/ABCstr"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency is
   edified upon a moutance amplecting zero or more entries, each such
   a jumelle of a key complying to the KEY-TYPE and an associated value
   compatible with the VALUE-TYPE, both governed by the generic sentinel
   ``*'' as the default configuration."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a singly linked list edified upon a
   moutenance tallying zero or more elements, each member subsuming into
   the ELEMENT-TYPE, for thilk governs the generic sentinel ``*'' as a
   default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (loop
            for    current-element of-type T in (the list candidate)
            always (typep current-element element-type))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable ABSstr program as a
   one-dimensional simple array comprehending zero or more ``Command''
   objects."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines a unidirectional mapping from a
   label name to its zero-based position into a parsed ABCstr program,
   its patefaction a hash table whose keys assume the simple string
   names, affiliatd with the ``fixnum'' indices."
  '(hash-table-of simple-string fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-truth-value (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\" and
   producing a veridicous Boolean truth value as its tantamount,
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the arithmetic operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-equals-one-p (number)
  "Determines whether the integer NUMBER is equal to the value one (1),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type integer number))
  (the boolean
    (convert-into-a-boolean-truth-value
      (= number 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either
   producing a fresh instance, upon the SOURCE's divergence from this
   specialized type, or, upon its conformity, responding with the
   unmodified SOURCE itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun count-the-character (source desideratum)
  "Returns the tally of the DESIDERATUM's occurrencies in the SOURCE
   string."
  (declare (type simple-string source))
  (declare (type standard-char desideratum))
  (the fixnum
    (count desideratum source :test #'char=)))

;;; -------------------------------------------------------

(defun count-the-equality-signs (source)
  "Returns the tally of the equality sign's (\"=\") occurrencies in the
   SOURCE string."
  (declare (type simple-string source))
  (the fixnum
    (count-the-character source #\=)))

;;; -------------------------------------------------------

(defun count-the-hyphens (source)
  "Returns the tally of hyphens (\"-\") in the SOURCE string."
  (declare (type simple-string source))
  (the fixnum
    (count-the-character source #\-)))

;;; -------------------------------------------------------

(defun count-the-colons (source)
  "Returns the tally of colons (\":\") in the SOURCE string."
  (declare (type simple-string source))
  (the fixnum
    (count-the-character source #\:)))

;;; -------------------------------------------------------

(defun split-the-string-at (source merist)
  "Splits the SOURCE string at all instances of the MERIST character
   and returns an ordered list of the resulting substrings."
  (declare (type simple-string source))
  (declare (type standard-char merist))
  (the (list-of simple-string)
    (loop
      for start-point
        of-type fixnum
        =       0
        then    (1+ end-point)
      for end-point
        of-type (or null fixnum)
        =       (position merist source
                  :start start-point
                  :test  #'char=)
      collect
        (convert-into-a-simple-string
          (subseq source start-point end-point))
      while end-point)))

;;; -------------------------------------------------------

(defun starts-with-a-colon-p (source)
  "Determines whether the SOURCE ostends as its first character a colon
   (\":\"), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-truth-value
      (string= source #\: :start1 0 :end1 (min 1 (length source))))))

;;; -------------------------------------------------------

(defun blank-line-p (source)
  "Determines whether the SOURCE represents the null string, or empty
   string, such does not entail any characters, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-truth-value
      (zerop
        (length source)))))

;;; -------------------------------------------------------

(defun read-a-line-from (source &optional (default NIL))
  "Queries the SOURCE for a line of characters and returns a simple
   string representation of the received content; or, upon its
   exhaustion, responds with the DEFAULT object, which resolves to
   ``NIL'' in its original configuration."
  (declare (type (or null (eql T) stream string) source))
  (declare (type (or null string)                default))
  (the (or null simple-string)
    (let ((consumed-line (read-line source NIL default)))
      (declare (type (or null string) consumed-line))
      (when consumed-line
        (convert-into-a-simple-string consumed-line)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ABCstr-Error (simple-error)
  ()
  (:documentation
    "The ``ABCstr-Error'' condition type serves as a firmament
     entreparted by all conditions ordained to the agency of a severe
     anomalous situation's communication whose etiology appertains to
     any stage of an ABCstr program's execution."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (ABCstr-Error)
  ()
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology emerges from
     the attempt to define a program label by a name already dedicated
     to this purpose at a prevenient location in the code."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' class serves as the substratum to all classes
   dedicated to the representation of ABCstr instructions.")

;;; -------------------------------------------------------

(defstruct (Substitute-Command
  (:include Command))
  "The ``Substitute-Command'' abstract class establishes a common
   foundry for all instructions assigned the dever of a string
   substitution, its diorism's amplection that of desideratum, that is,
   the string to substitute."
  (desideratum (error "No substitution desideratum has been specified.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Constant-Substitute-Command
  (:include Substitute-Command))
  "The ``Constant-Substitute-Command'' abstract class furnishes a
   firmament entreparted by all classes pursuing the representation of
   commands capacitated to substitute some content by another explicitly
   specified literal substring."
  (succedaneum (error "No substitution succedaneum has been specified.")
               :type      simple-string
               :read-only T))

;;; -------------------------------------------------------

(defstruct (Simple-Substitute-Command
  (:include     Constant-Substitute-Command)
  (:constructor make-a-simple-substitute-command (desideratum
                                                  succedaneum)))
  "The ``Simple-Substitute-Command'' class applies itself to the
   representation of an ABCstr substitution command which replaces a
   desideratum by a succedaneum, if possible.
   ---
   The appertaining ABCstr syntax constitutes the following:
     delendum=succedaneum
     ******** ***********")

;;; -------------------------------------------------------

(defstruct (Output-Substitute-Command
  (:include     Constant-Substitute-Command)
  (:constructor make-an-output-substitute-command (desideratum
                                                   succedaneum)))
  "The ``Output-Substitute-Command'' class applies itself to the
   representation of an ABCstr substitution command which issues, as an
   epiphenomenal action in a successful attempt's perclose, the just
   replaced content to the standard output conduit.
   ---
   The appertaining ABCstr syntax constitutes the following:
     delendum-succedaneum
     ******** ***********")

;;; -------------------------------------------------------

(defstruct (Input-Substitute-Command
  (:include     Substitute-Command)
  (:constructor make-an-input-substitute-command (desideratum)))
  "The ``Input-Substitute-Command'' class applies itself to the
   representation of an ABCstr substitution command which replaces an
   optated content by a user-provided input string.
   ---
   The appertaining ABCstr syntax constitutes the following:
     :delendum
      ********")

;;; -------------------------------------------------------

(defstruct (Label-Definition-Command
  (:include     Command)
  (:constructor make-a-label-definition-command (name)))
  "The ``Label-Definition-Command'' class applies itself to the
   representation of a behest airted at a label's declaration, its
   diorism the mandatory agnomination's bield.
   ---
   The appertaining ABCstr syntax constitutes the following:
     name
     ****"
  (name (error "No label name has been specified.")
        :type      simple-string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Command
  (:include     Command)
  (:constructor make-a-nop-command ()))
  "The ``NOP-Command'' class serves as a signification of an ABCstr
   line entailing no operative content.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-into-a-program (commands)
  "Creates and returns a fresh ``program'' amplecting the COMMANDS
   list's members."
  (declare (type (list-of Command) commands))
  (the program
    (coerce commands
      '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-line (source)
  "Parses the SOURCE line and returns a conable ``Command''
   representation thereof."
  (declare (type simple-string source))
  (symbol-macrolet
      ((line-contains-one-equality-sign-p
        (the boolean
          (number-equals-one-p
            (count-the-equality-signs source))))
       (line-contains-one-hyphen-p
        (the boolean
          (number-equals-one-p
            (count-the-hyphens source)))))
    (declare (type boolean line-contains-one-equality-sign-p))
    (declare (type boolean line-contains-one-hyphen-p))
    (the Command
      (cond
        ;; No-operation (NOP).
        ((blank-line-p source)
          (make-a-nop-command))
        
        ;; x = y
        (line-contains-one-equality-sign-p
          (apply #'make-a-simple-substitute-command
            (split-the-string-at source #\=)))
        
        ;; x - y
        (line-contains-one-hyphen-p
          (apply #'make-an-output-substitute-command
            (split-the-string-at source #\-)))
        
        ;; : x
        ((starts-with-a-colon-p source)
          (make-an-input-substitute-command
            (convert-into-a-simple-string
              (subseq source 1))))
        
        (T
          (make-a-label-definition-command source))))))

;;; -------------------------------------------------------

(defun parse-the-program (source)
  "Parses the piece of ABCstr SOURCE code and returns a ``program''
   representation of its line commands."
  (declare (type string source))
  (the program
    (assemble-into-a-program
      (with-input-from-string (input-stream source)
        (declare (type string-stream input-stream))
        (loop
          for current-line
            of-type (or null simple-string)
            =       (read-a-line-from input-stream)
          while current-line
          collect
            (parse-the-line current-line))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label registration operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-the-labels (program)
  "Collates the label definitions partaking of the ABCstr PROGRAM and
   returns a hash table mapping their names to their zero-based indices
   into the same."
  (declare (type program program))
  (let ((labels (make-hash-table :test #'equal)))
    (declare (type label-table labels))
    (loop
      for current-command     of-type Command across program
      and current-line-number of-type fixnum  from   0 by 1
      when (label-definition-command-p current-command) do
        (let ((label-name
                (label-definition-command-name current-command)))
          (declare (type simple-string label-name))
          (if (gethash label-name labels)
            (error 'Duplicate-Label-Error
              :format-control
                "A label with the name ~s has already been defined at ~
                 a prevenient position in the program."
              :format-arguments
                (list label-name))
            (setf (gethash label-name labels) current-line-number))))
    (the label-table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string substitution operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-substring-in (haystack needle)
  "Seeks the NEEDLE in the HAYSTACK and returns two values:
     (1) If the NEEDLE could be detected in the HAYSTACK, the zero-based
         position of the former's first character in the first
         occurrency; otherwise the ``NIL'' sentinel.
     (2) If the NEEDLE could be detected in the HAYSTACK, the zero-based
         position into the HAYSTACK immediately succeeding the former's
         first occurrency in the latter; otherwise the ``NIL''
         sentinel."
  (declare (type simple-string haystack))
  (declare (type simple-string needle))
  (let ((start-index (search needle haystack :test #'char=)))
    (declare (type (or null fixnum) start-index))
    (the (values (or null fixnum) (or null fixnum))
      (values
        start-index
        (and start-index
             (+ start-index (length needle)))))))

;;; -------------------------------------------------------

(defun substitute-the-tmema-by (source
                                start-index
                                end-index
                                new-content)
  "Replaces the tmema in the SOURCE demarcated by the inclusive
   START-INDEX and the exclusive END-INDEX by the NEW-CONTENT and
   returns a fresh string of the thus obtained result."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (declare (type fixnum        end-index))
  (declare (type simple-string new-content))
  (the simple-string
    (convert-into-a-simple-string
      (with-output-to-string (modified-source)
        (declare (type string-stream modified-source))
        (format modified-source "~a~a~a"
          (subseq source 0 start-index)
          new-content
          (subseq source end-index))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 3) +DEFAULT-MEMORY-STATE+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-MEMORY-STATE+ "abc"
  "The inicipial ABCstr memory string.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "No ABCstr program has been supplied.")
    :type          program
    :documentation "The ABCstr program to execute as a vector of
                    commands.")
   (labels
    :type          label-table
    :documentation "Maps the label names in the PROGRAM to their
                    zero-based position into the same.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the PROGRAM.")
   (memory
    :initform      +DEFAULT-MEMORY-STATE+
    :type          simple-string
    :documentation "The program memory as a string."))
  (:documentation
    "The ``Interpreter'' class constitutes that wike's pernor to whom
     the vouchsafement of actual efficacy to a parsed ABCstr program,
     in the plasmature of a command vector, is consigned."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Collates the labels partaking of the ABCstr program concredited to
   the INTERPRETER's castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program labels) interpreter
    (declare (type program     program))
    (declare (type label-table labels))
    (setf labels
      (register-the-labels program)))
  (values))

;;; -------------------------------------------------------

(defun make-an-interpreter-for (program)
  "Creates and returns a fresh ``Interpreter'' whose ordainment
   appertains to the ABCstr PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-to-the-next-command (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in the underlying program, if possible, and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (setf ip
      (min
        (1+ ip)
        (length program))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-the-label (interpreter name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the label
   amenable to the NAME; or, upon its disrespondency, moves the same
   ayond the desinent command in order to signify the PROGRAM's
   cessation, in any case returning no value."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string name))
  (with-slots (program labels ip) interpreter
    (declare (type program     program))
    (declare (type label-table labels))
    (declare (type fixnum      ip))
    (setf ip
      (or (gethash name labels)
          (1+ ip))))
  (values))

;;; -------------------------------------------------------

(defun substitute-or-jump (interpreter
                           desideratum
                           succedaneum
                           requests-succedaneum-from-input-p
                           prints-desideratum-on-success-p)
  "Determines whether the INTERPRETER's memory string comprehends the
   DESIDERATUM, on confirmation substituting its first occurrency either
   by the SUCCEDANEUM, if REQUESTS-SUCCEDANEUM-FROM-INPUT-P assumes the
   ``NIL'' state, or, for an activated flag, ignores the SUCCEDANEUM for
   a user-requested string obtained from the standard input conduit;
   contingently succeeds the substitution by a printing of the 
   DESIDERATUM, ere advancing to the next command in the program;
   otherwise, upon the DESIDERATUM's carency, relocates the instruction
   pointer (IP) to the label designated by the DESIDERATUM itself; in
   any case returns no value."
  (declare (type Interpreter   interpreter))
  (declare (type simple-string desideratum))
  (declare (type simple-string succedaneum))
  (declare (type boolean       requests-succedaneum-from-input-p))
  (declare (type boolean       prints-desideratum-on-success-p))
  (with-slots (memory) interpreter
    (declare (type simple-string memory))
    (multiple-value-bind (start-index end-index)
        (locate-the-substring-in memory desideratum)
      (declare (type (or null fixnum) start-index))
      (declare (type (or null fixnum) end-index))
      (cond
        ((and start-index end-index)
          (setf memory
            (substitute-the-tmema-by
              memory
              start-index
              end-index
              (if requests-succedaneum-from-input-p
                (prog1
                  (read-a-line-from NIL "")
                  (clear-input))
                succedaneum)))
          (when prints-desideratum-on-success-p
            (format T "~&~a" desideratum))
          (advance-to-the-next-command interpreter))
        (T
          (jump-to-the-label interpreter desideratum)))))
  (values))

;;; -------------------------------------------------------

(defgeneric process-the-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter)
            (command     Simple-Substitute-Command))
    (declare (type Interpreter               interpreter))
    (declare (type Simple-Substitute-Command command))
    (substitute-or-jump
      interpreter
      (constant-substitute-command-desideratum command)
      (constant-substitute-command-succedaneum command)
      NIL
      NIL)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Output-Substitute-Command))
    (declare (type Interpreter               interpreter))
    (declare (type Output-Substitute-Command command))
    (substitute-or-jump
      interpreter
      (constant-substitute-command-desideratum command)
      (constant-substitute-command-succedaneum command)
      NIL
      T)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Input-Substitute-Command))
    (declare (type Interpreter              interpreter))
    (declare (type Input-Substitute-Command command))
    (format T "~&>> ")
    (finish-output)
    (substitute-or-jump
      interpreter
      (input-substitute-command-desideratum command)
      ""
      T
      NIL)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     Label-Definition-Command))
    (declare (type Interpreter              interpreter))
    (declare (type Label-Definition-Command command)
             (ignore                        command))
    (advance-to-the-next-command interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (command     NOP-Command))
    (declare (type Interpreter interpreter))
    (declare (type NOP-Command command)
             (ignore           command))
    (advance-to-the-next-command interpreter)
    (values)))

;;; -------------------------------------------------------

(defun start-the-interpreter (interpreter)
  "Instigates the INTERPRETER's operations and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (loop while (< ip (length program)) do
      (process-the-command interpreter
        (aref program ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-abcstr-code (code)
  "Interprets the piece of ABCstr source CODE and returns no value."
  (declare (type string code))
  (start-the-interpreter
    (make-an-interpreter-for
      (parse-the-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!".
(interpret-the-abcstr-code
"a=Hello, World!
a
Hello, World!-a")

;;; -------------------------------------------------------

;; Cyclic tag interpreter.
(interpret-the-abcstr-code
"a=def
a
:f
f
b=gh
b
:h
h
-
g1
eg=g
gc=g
c=c
gc
d=de
-=-
eg
e;=;e
g0-g
g0
g1-g
-=-
;e
e0=0e
g1=g1
c=0c
-=-
e0
e1=1e
g1=g1
c=1c
-=-
d
c
e1
")
