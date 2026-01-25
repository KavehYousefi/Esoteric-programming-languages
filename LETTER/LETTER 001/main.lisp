;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "LETTER", invented by the Esolang user "A()" and presented
;; on December 7th, 2025, conceived as a derivation of Urban Mueller's
;; "brainfuck" whose paravaunt proprium, with all further aspects
;; ipsissima verba replications of the entheus' notions, appertains to
;; a deviation from the one-symbol instruction identifiers towards a
;; missive's verisimilitude in particular phrases, as well as the
;; program's ensconcement in a covenable salutation and a polite
;; valediction.
;; 
;; 
;; Concept
;; =======
;; The LETTER programming language establishes a syntactical
;; reformulation of brainfuck, its foundational instruction set's
;; one-symbol identifiers forming the subject of a missive's
;; simulacrum's administration; this plasmature concluded, in a sensible
;; concoction as empight in the topic, by two adscititious and
;; imperative members, the first forms the letter's "salutation" at
;; the program's inchoation, the latter its desistive formality in the
;; "valediction".
;; 
;; == LETTER: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; LETTER's kenspeckle physiognomy betokens its provenance in a
;; cambistry's application with the operation identifiers, transitioning
;; from the one-symbol originals to phrases whose acquaintance concurs
;; with one's expectant acquaintance in written communications; the
;; semblant's pleroma such as to impose both a dedicated salutation
;; phrase and a desition in a valediction.
;; 
;; A program's assemblage ensues from a catena of phrases, the merist
;; commorant atwixen any word twissel rendered into one or more
;; whitespaces.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of LETTER's recipiency does not elude brainfuck's
;; architecture, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; Begotten from a status of paregal conception in all but its
;; identifiers' semantical aspects to its brainfuck heritage, LETTER's
;; compass does neither actuate an ostention's commission designed with
;; supererogation with respect to its provenance, nor a curtailment in
;; the competences; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the LETTER programming language's
;; facilities, thilk in a manner or preponderance concomitantly concur
;; with the offerings of its brainfuck stock-father, wisting merely of
;; two ritualistic additaments concerning a program's inchoacy and
;; desition:
;; 
;;   ------------------------------------------------------------------
;;   Command                        | Effect
;;   -------------------------------+----------------------------------
;;   Dear, Reader                   | Starts the program.
;;                                  |----------------------------------
;;                                  | This instruction must per force
;;                                  | constitute the first one in the
;;                                  | program.
;;                                  |----------------------------------
;;                                  | If the program lacks this
;;                                  | operation, comprehends two or
;;                                  | more instances of the same, or
;;                                  | does not commence with thilk, an
;;                                  | error of the type
;;                                  | "InvalidSalutationError" will be
;;                                  | signaled.
;;   ..................................................................
;;   Love, Programer                | Terminates the program.
;;                                  |----------------------------------
;;                                  | This instruction must per force
;;                                  | constitute the desitive one in
;;                                  | the program.
;;                                  |----------------------------------
;;                                  | If the program lacks this
;;                                  | operation, comprehends two or
;;                                  | more instances of the same, or
;;                                  | does not conclude with thilk, an
;;                                  | error of the type
;;                                  | "InvalidValedictionError" will be
;;                                  | signaled.
;;   ..................................................................
;;   Very great news!               | Increments the current cell value
;;                                  | by one (1). If the new value
;;                                  | transgresses the inclusive upper
;;                                  | bourne of 255, the state wraps
;;                                  | around to the minimum of zero
;;                                  | (0).
;;   ..................................................................
;;   Unpleasent news.               | Decrements the current cell value
;;                                  | by one (1). If the new value
;;                                  | transgresses the inclusive lower
;;                                  | bourne of zero (0), the state
;;                                  | wraps around to the maximum of
;;                                  | 255.
;;   ..................................................................
;;   You might thinking,            | Translates the cell pointer one
;;                                  | step to the right.
;;   ..................................................................
;;   But,                           | Translates the cell pointer one
;;                                  | step to the left.
;;   ..................................................................
;;   I would like to hear from you! | Queries the standard input
;;                                  | conduit for a character and
;;                                  | stores its ASCII code in the
;;                                  | current cell.
;;   ..................................................................
;;   Well, that's that.             | Prints the character whose ASCII
;;                                  | code concurs with the current
;;                                  | cell value to the standard output
;;                                  | conduit.
;;   ..................................................................
;;   So hello!                      | If the current cell value equals
;;                                  | zero (0), moves the instruction
;;                                  | pointer (IP) forward to the
;;                                  | position immediately succeeding
;;                                  | the matching
;;                                  | "Let's move on parhaps?"
;;                                  | instruction; otherwise proceeds
;;                                  | as usual.
;;   ..................................................................
;;   Let's move on parhaps?         | If the current cell value does
;;                                  | not equal zero (0), moves the
;;                                  | instruction pointer (IP) back to
;;                                  | the position immediately
;;                                  | succeeding the matching
;;                                  | "So hello!" instruction;
;;                                  | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == LETTER AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation --- with twifold exemptions the supererogative
;; formalities incarnated in the bracketing salutation and valediction,
;; LETTER's patration in language twissel's replication homologates an
;; equiparation applying to the operative bailiwick:
;; 
;;   ------------------------------------------------------------------
;;   LETTER                         | brainfuck | Causatum
;;   -------------------------------+-----------+----------------------
;;   Very great news!               | +         | Increment
;;   ..................................................................
;;   Unpleasent news.               | -         | Decrement
;;   ..................................................................
;;   You might thinking,            | >         | Move to right cell
;;   ..................................................................
;;   But,                           | <         | Move to left cell
;;   ..................................................................
;;   I would like to hear from you! | ,         | Input
;;   ..................................................................
;;   Well, that's that.             | .         | Output
;;   ..................................................................
;;   So hello!                      | [         | Jump forward
;;   ..................................................................
;;   Let's move on parhaps?         | ]         | Jump back
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the LETTER source code
;; string's transcription into dedicated instruction representations
;; inwith whose potential an enhaused capacity for eath evaluation
;; wones, ere these entities ultimate vouchsafement of actual efficacy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-22
;; 
;; Sources:
;;   [esolang2025:LETTER]
;;   The Esolang contributors, "LETTER", December 7th, 2025
;;   URL: "https://esolangs.org/wiki/LETTER"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes the NAME's
   dation, and which acts as a pernor to the LAMBDA-LIST's ipsissima
   verba specifications as its personal formal parameters, concomitantly
   assigning the probed object to the CANDIDATE-NAME, evaluates the BODY
   forms, and construes the desinent form's primary return value as the
   docimasy's adjudgment, a \"generalized boolean\" truth value of
   \"true\" peracting a successful compatibility's assessment's
   signification, while a \"false\" response concludes in the
   candidate's rejection.
   ---
   The first BODY form, in the case of its resolution to a string
   object, is adhibited the role of a documentation string to the type
   definition, being, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
       ,(or (and (stringp (first body))
               (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency tallying zero or more entries, everichon member among
   these dimidiated into a key compliant with the KEY-TYPE and an allied
   value of the VALUE-TYPE, both governed by a configuration which
   assigns the generic sentinel ``*'' as the default state."
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
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*)
                                             (size         '*))
  "The ``list-of'' type defines a linked list comprehending zero or more
   members, each element partaking of the same complying with the
   ELEMENT-TYPE, the cardinality either limining an owelty to the SIZE,
   or, upon its omission, an arbitrary accompt, for both is specified
   the generic sentinel ``*'' as the default."
  (and
    (listp candidate)
    (or
      (eq size '*)
      (=  (length (the list candidate))
          size))
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type   '*)
                                        (value-type '*)
                                        (size       '*))
  "The ``association-list-of'' type defines an association list, or
   alist, the componency assigned to whom imposes a catena enumerating
   either a SIZE cardinality in membership or, upon this option's
   omission, an arbitrary accompt; natheless, each element subsumes into
   a species of cons cell, the sinistral compartment of which lends a
   commorancy to the key, compliant with the KEY-TYPE, the dextral to
   the affiliated value of the VALUE-TYPE, for both holds the default
   configuration involving the generic sentinel ``*''."
  `(list-of (cons ,key-type ,value-type) ,size))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized instructions
   partaking of a LETTER program, its compass amplecting such of
   epiphenomenal vallidom, as well as such twissel appertaining to a
   traditional missive's consuetude in its salutation and valediction."
  '(member
    :salutation
    :valediction
    :increment
    :decrement
    :move-right
    :move-left
    :input
    :output
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable LETTER program in the
   guise of a one-dimensional simple array comprehending zero or more
   ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype position-map ()
  "The ``position-map'' type defines zero or more associations atwixen
   positions or indices, realized as a hash table whose keys and values
   both partake of a subsumption into the ``fixnum'' type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value edified from an
   ogdoad of attiguous bits, thus constituting an incolant of the closed
   integer interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism's amplection enhalses, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition LETTER-Error (simple-error)
  ()
  (:documentation
    "The ``LETTER-Error'' condition type furnishes a common firmament
     dedicated to the apprizal about any anomalous circumstance emerging
     during a LETTER program's obtention, analyzation, and
     interpretation."))

;;; -------------------------------------------------------

(define-condition Invalid-Salutation-Error (LETTER-Error)
  ()
  (:documentation
    "The ``Invalid-Salutation-Error'' condition type serves in the
     apprizal about an anomalous circumstance whose etiology ensues
     from an invalid tally or distribution of salutation instructions,
     designated via the character sequence \"Dear, Reader\", in a
     LETTER program."))

;;; -------------------------------------------------------

(define-condition Invalid-Valediction-Error (LETTER-Error)
  ()
  (:documentation
    "The ``Invalid-Valediction-Error'' condition type serves in the
     apprizal about an anomalous circumstance whose etiology ensues
     from an invalid tally or distribution of valediction instructions,
     designated via the character sequence \"Love, Programer\", in a
     LETTER program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its semblance as a \"generalized boolean\"
   and returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' OBJECT a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' input, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either by the
   latter's conversion into the desiderated type, if not already
   subsumed into the simple variant, or, upon a compliance's existency,
   by the simple string SOURCE's delivery without further investments."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-value
      (string= source ""))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-a-program-from (instructions)
  "Creates and returns a fresh ``program'' whose operations are desumed
   from the INSTRUCTIONS list."
  (declare (type (list-of instruction *) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))

;;; -------------------------------------------------------

(defun locate-the-instruction-in (program desiderated-instruction)
  "Returns an ordered list of the zero-based positions into the PROGRAM
   occupied by the DESIDERATED-INSTRUCTION."
  (declare (type program     program))
  (declare (type instruction desiderated-instruction))
  (the (list-of fixnum *)
    (loop
      for current-instruction of-type instruction across program
      and current-position    of-type fixnum      from   0 by 1
      when (eq current-instruction desiderated-instruction)
        collect current-position)))

;;; -------------------------------------------------------

(defun locate-the-salutations-in (program)
  "Returns an ordered list of the zero-based positions into the PROGRAM
   occupied by the salutation \"Dear, Reader\"."
  (declare (type program program))
  (the (list-of fixnum *)
    (locate-the-instruction-in program :salutation)))

;;; -------------------------------------------------------

(defun locate-the-valedictions-in (program)
  "Returns an ordered list of the zero-based positions into the PROGRAM
   occupied by the valediction \"Love, Programer\"."
  (declare (type program program))
  (the (list-of fixnum *)
    (locate-the-instruction-in program :valediction)))

;;; -------------------------------------------------------

(defun validate-the-salutation (program)
  "Determines whether the PROGRAM's conformation anent the nomothesia's
   purview concerning the salutation phrase \"Dear, Reader\" adheres to
   the imposed covenant, upon its infracture signaling an error of the
   type ``Invalid-Salutation-Error''; otherwise simply returns no
   value."
  (declare (type program program))
  (let ((locations (locate-the-salutations-in program)))
    (declare (type (list-of fixnum *) locations))
    (cond
      ;; No salutation found?
      ((null locations)
        (error 'Invalid-Salutation-Error
          :format-control "The program is missing its salutation."))
      ;; Two or more salutations found?
      ((rest locations)
        (error 'Invalid-Salutation-Error
          :format-control "The program comprehends too many ~
                           salutations."))
      ;; The salutation does not occupy the first position?
      ((/= (first locations) 0)
        (error 'Invalid-Salutation-Error
          :format-control "The program does not commence with a ~
                           salutation."))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(defun validate-the-valediction (program)
  "Determines whether the PROGRAM's conformation anent the nomothesia's
   purview concerning the valediction phrase \"Love, Programer\" adheres
   to the imposed covenant, upon its infracture signaling an error of
   the type ``Invalid-Valediction-Error''; otherwise simply returns no
   value."
  (declare (type program program))
  (let ((locations (locate-the-valedictions-in program)))
    (declare (type (list-of fixnum *) locations))
    (cond
      ;; No valediction found?
      ((null locations)
        (error 'Invalid-Valediction-Error
          :format-control "The program is missing its valediction."))
      ;; Two or more valedictions found?
      ((rest locations)
        (error 'Invalid-Valediction-Error
          :format-control "The program comprehends too many ~
                           valedictions."))
      ;; The valediction does not occupy the desinent position?
      ((/= (first (nreverse locations))
           (1- (length program)))
        (error 'Invalid-Valediction-Error
          :format-control "The program does not conclude with a ~
                           valediction."))
      (T
        NIL)))
  (values))

;;; -------------------------------------------------------

(defun validate-the-program (program)
  "Determines whether the PROGRAM's conformation anent the nomothesia's
   purview concerning the salutation phrase \"Dear, Reader\" and the
   valediction phrase \"Love, Programer\" adheres to the imposed
   covenant, upon its infracture signaling either an error of the type
   ``Invalid-Salutation-Error'' or ``Invalid-Valediction-Error'';
   otherwise simply returns no value."
  (declare (type program program))
  (validate-the-salutation  program)
  (validate-the-valediction program)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No source has been specified for the lexer.")
    :type          simple-string
    :documentation "The piece of LETTER source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE string.")
   (character
    :type          character
    :documentation "The character at the current POSITION into the
                    SOURCE string."))
  (:documentation
    "The ``Lexer'' class is appportioned the wike of a lexical
     analyzer's furnishment, the same is ordained to the extraction and
     delivery of the ensconced tokens from a piece of LETTER source
     code string."))

;;; -------------------------------------------------------

(defmacro with-the-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots to local symbol macros,
   evaluates the BODY forms, and returns the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer)
                (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           (($source
              (the simple-string
                (slot-value ,evaluated-lexer 'source)))
            ($position
              (the fixnum
                (slot-value ,evaluated-lexer 'position)))
            ($character
              (the character
                (slot-value ,evaluated-lexer 'character)))
            ($more-characters-follow-p
              (the boolean
                (convert-into-a-boolean-value
                  (array-in-bounds-p $source $position)))))
         (declare (type simple-string $source)
                  (ignorable          $source))
         (declare (type fixnum        $position)
                  (ignorable          $position))
         (declare (type character     $character)
                  (ignorable          $character))
         (declare (type boolean       $more-characters-follow-p)
                  (ignorable          $more-characters-follow-p))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Stores the first character from the LEXER's source into the same, if
   possible, and returns no value."
  (declare (type Lexer lexer))
  (with-the-lexer (lexer)
    (when $more-characters-follow-p
      (setf $character
        (schar $source $position))))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-lexer-for (source)
  "Creates and returns a fresh ``Lexer'' dedicated to the piece of
   LETTER SOURCE code's lexical analyzation."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source
      (convert-into-a-simple-string source))))

;;; -------------------------------------------------------

(defun advance-to-the-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-the-lexer (lexer)
    (setf $position
      (min
        (1+ $position)
        (length $source)))
    (when $more-characters-follow-p
      (setf $character
        (schar $source $position))))
  (values))

;;; -------------------------------------------------------

(defun whitespace-character-follows-p (lexer)
  "Determines whether the current character into the LEXER's source
   represents a whitespace, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (with-the-lexer (lexer)
      (convert-into-a-boolean-value
        (and $more-characters-follow-p
             (whitespace-character-p $character))))))

;;; -------------------------------------------------------

(defun skip-attiguous-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a catena enumerating zero or more attiguous whitespaces, and returns
   no value."
  (declare (type Lexer lexer))
  (loop while (whitespace-character-follows-p lexer) do
    (advance-to-the-next-character lexer))
  (values))

;;; -------------------------------------------------------

(defun resides-outside-of-a-word-p (lexer)
  "Determines whether the current character into the LEXER's source
   represents a constituent admissible for a word, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (with-the-lexer (lexer)
      (convert-into-a-boolean-value
        (or (not $more-characters-follow-p)
            (whitespace-character-p $character))))))

;;; -------------------------------------------------------

(defun read-the-next-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   the ensuing word and returns a fresh simple string representation of
   its content."
  (declare (type Lexer lexer))
  (the simple-string
    (with-the-lexer (lexer)
      (convert-into-a-simple-string
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop until (resides-outside-of-a-word-p lexer) do
            (write-char $character identifier)
            (advance-to-the-next-character lexer)))))))

;;; -------------------------------------------------------

(defun request-the-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh empty simple string."
  (declare (type Lexer lexer))
  (the simple-string
    (with-the-lexer (lexer)
      (cond
        ((not $more-characters-follow-p)
          "")
        ((whitespace-character-follows-p lexer)
          (skip-attiguous-whitespaces lexer)
          (request-the-next-token     lexer))
        (T
          (read-the-next-word lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the phrase.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Phrase
  (:constructor initialize-a-new-phrase
    (tokens
     &aux (length (length tokens)))))
  "The ``Phrase'' class serves in the aggregation of a LETTER
   identifiers' constituent tokens into an aefauld complex, intended to
   filst in the matching process during the instruction extraction
   tier."
  (tokens (error "No tokens have been specified as the phrase's ~
                  constituents.")
          :type      (list-of simple-string *)
          :read-only T)
  (length (error "The phrase length has not been specified.")
          :type      fixnum
          :read-only T))

;;; -------------------------------------------------------

(defun assemble-a-phrase-from (&rest tokens)
  "Creates and returns a fresh ``Phrase'' composed of the TOKENS."
  (declare (type (list-of simple-string *) tokens))
  (the Phrase
    (initialize-a-new-phrase tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token buffer node.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Buffer-Node ()
  ((token
    :initarg       :token
    :initform      (error "No token has been specified for the ~
                           buffer node.")
    :reader        buffer-node-token
    :type          simple-string
    :documentation "The token maintained by this node.")
   (successor
    :initarg       :successor
    :initform      NIL
    :accessor      buffer-node-successor
    :type          (or null Buffer-Node)
    :documentation "A reference to an optional successor node."))
  (:documentation
    "The ``Buffer-Node'' class is intended as the pernor of that dever
     to ensconce a token as a constituent of the ``Token-Buffer'',
     its constitution, adhering to the singly linked node concept, an
     accompt of the token datum and an optional reference to the
     successor node."))

;;; -------------------------------------------------------

(defun create-a-new-buffer-node (token &optional (successor NIL))
  "Creates and returns a fresh ``Buffer-Node'' serving to ensconce the
   TOKEN, optionally complementing its castaldy by the reference to the
   SUCCESSOR node."
  (declare (type simple-string         token))
  (declare (type (or null Buffer-Node) successor))
  (the Buffer-Node
    (make-instance 'Buffer-Node :token token :successor successor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token buffer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Buffer) (values))
                load-the-next-token-into-the-buffer))

;;; -------------------------------------------------------

(defclass Token-Buffer ()
  ((lexer
    :initarg       :lexer
    :initform      (error "No lexer has been specified for the token ~
                           buffer.")
    :reader        token-buffer-lexer
    :type          Lexer
    :documentation "The entity responsible for the tokens' production.")
   (head
    :initform      NIL
    :accessor      token-buffer-head
    :type          (or null Buffer-Node)
    :documentation "The front node into the buffer.
                    ---
                    From its status as an actual node, rather than a
                    sentinel, ensues the contingency for its castaldy of
                    a token.")
   (tail
    :initform      NIL
    :accessor      token-buffer-tail
    :type          (or null Buffer-Node)
    :documentation "The desinent node into the buffer.
                    ---
                    From its status as an actual node, rather than a
                    sentinel, ensues the contingency for its castaldy of
                    a token.")
   (size
    :initform      0
    :accessor      token-buffer-size
    :type          fixnum
    :documentation "The tally of tokens concredited to this buffer's
                    castaldy.
                    ---
                    The HEAD and TAIL, representing actual nodes rather
                    than sentinels, if not ``NIL'', contribute to this
                    accompt.")
   (is-exhausted-p
    :initform      NIL
    :accessor      token-buffer-is-exhausted-p
    :type          boolean
    :documentation "Determines whether the underlying LEXER is
                    exhausted, that is to say, merely delivers empty
                    strings, thilk signify its source's end of file."))
  (:documentation
    "The ``Token-Buffer'' class furnishes a mechanism entalented with
     such a capacitation as to arrange its castaldy of tokens on express
     optation, the obtention the bailiwick of an internally managed
     ``Lexer'', following a queue's forbisen in the additament to the
     rear, and the removal from the front.
     ---
     This concrete implementation's firmament declaims itself a singly
     linked list's mimicry, both the head and tail nodes, the former the
     front, the latter the rear's representative, vouchsafed the
     capacity of actual data castaldy, rather than a sentinel's wike."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((buffer Token-Buffer) &key)
  "Queries the first token from the token BUFFER's underlying lexer,
   stores thilk in the BUFFER itself, and returns no value."
  (declare (type Token-Buffer buffer))
  (load-the-next-token-into-the-buffer buffer)
  (values))

;;; -------------------------------------------------------

(defun prepare-a-token-buffer-for (lexer)
  "Creates and returns a fresh ``Token-Buffer'' whose token obtention's
   provenance is accommodated by the LEXER."
  (declare (type Lexer lexer))
  (the Token-Buffer
    (make-instance 'Token-Buffer :lexer lexer)))

;;; -------------------------------------------------------

(defun token-buffer-is-empty-p (buffer)
  "Determines whether the token BUFFER is empty, that is, deprived of
   any tokens, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (the boolean
    (not (null
      (zerop
        (token-buffer-size buffer))))))

;;; -------------------------------------------------------

(defun append-the-token-to-the-buffer (buffer new-token)
  "Inserts the NEW-TOKEN at the BUFFER's rear and returns no value."
  (declare (type Token-Buffer  buffer))
  (declare (type simple-string new-token))
  (let ((new-node (create-a-new-buffer-node new-token)))
    (declare (type Buffer-node new-node))
    (if (token-buffer-is-empty-p buffer)
      (setf (token-buffer-head buffer)
            new-node)
      (setf (buffer-node-successor
              (token-buffer-tail buffer))
            new-node))
    (setf (token-buffer-tail buffer) new-node)
    (incf (token-buffer-size buffer)))
  (values))

;;; -------------------------------------------------------

(defun load-the-next-token-into-the-buffer (buffer)
  "Queries the next token from the token BUFFER's underlying lexer,
   stores thilk in the BUFFER, and returns no value."
  (declare (type Token-Buffer buffer))
  (unless (token-buffer-is-exhausted-p buffer)
    (let ((next-token
            (request-the-next-token
              (token-buffer-lexer buffer))))
      (declare (type simple-string next-token))
      (if (string-is-empty-p next-token)
        (setf (token-buffer-is-exhausted-p buffer) T)
        (append-the-token-to-the-buffer buffer next-token))))
  (values))

;;; -------------------------------------------------------

(defun ensure-a-minimum-token-buffer-size-of (buffer minimum-size)
  "Ascertains that the token BUFFER comprehends at least the
   MINIMUM-SIZE tally of elements by contingently requesting tokens from
   the underlying lexer and storing these in the BUFFER, until the
   carency has been ameliorated, and returns no value."
  (declare (type Token-Buffer buffer))
  (declare (type fixnum       minimum-size))
  (loop
    until (or (token-buffer-is-exhausted-p buffer)
              (>= (token-buffer-size buffer)
                  minimum-size))
    do    (load-the-next-token-into-the-buffer buffer))
  (values))

;;; -------------------------------------------------------

(defun eject-the-first-token-from-the-buffer (buffer)
  "Removes the front element from the token BUFFER and returns no
   value."
  (declare (type Token-Buffer buffer))
  (unless (token-buffer-is-empty-p buffer)
    (setf (token-buffer-head buffer)
      (buffer-node-successor
        (token-buffer-head buffer)))
    (decf (token-buffer-size buffer))
    (when (token-buffer-is-empty-p buffer)
      (setf (token-buffer-tail buffer) NIL)))
  (values))

;;; -------------------------------------------------------

(defun eject-the-tally-of-tokens-from-the-buffer (buffer
                                                  number-of-deletions)
  "Removes and discards a maxiimum NUMBER-OF-DELETIONS tally elements
   from the token BUFFER's front and returns no value."
  (declare (type Token-Buffer buffer))
  (declare (type fixnum       number-of-deletions))
  (loop repeat number-of-deletions do
    (eject-the-first-token-from-the-buffer buffer))
  (values))

;;; -------------------------------------------------------

(defmacro do-the-token-buffer-nodes ((current-node-variable buffer)
                                     &body body)
  "Evaluates the token BUFFER, iterates its entire nodes, binding during
   each cycle the current node to the CURRENT-NODE-VARIABLE, while
   evaluating the BODY forms, and finally returns no value."
  `(loop
     for ,current-node-variable
       of-type (or null Buffer-Node)
       =       (token-buffer-head     ,buffer)
       then    (buffer-node-successor ,current-node-variable)
     while current-node
       do ,@body
     finally
       (return (values))))

;;; -------------------------------------------------------

(defmethod print-object ((buffer Token-Buffer) (stream T))
  (declare (type Token-Buffer buffer))
  (declare (type stream       stream))
  (let ((first-token-p T))
    (declare (type boolean first-token-p))
    (format stream "[")
    (do-the-token-buffer-nodes (current-node buffer)
      (if first-token-p
        (setf first-token-p NIL)
        (format stream ", "))
      (format stream "~s"
        (buffer-node-token current-node)))
    (format stream "]"))
  (the Token-Buffer buffer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the phrase matching operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-the-token-buffer-for-the-phrase (buffer phrase)
  "Requests the requisite tally of tokens from the token BUFFER's
   underlying lexer in order to accommodate a sufficient amount of such
   for the PHRASE's equiparation with the BUFFER content, and returns
   no value."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (ensure-a-minimum-token-buffer-size-of buffer
    (phrase-length phrase))
  (values))

;;; -------------------------------------------------------

(defun token-buffer-starts-with-the-phrase-p (buffer phrase)
  "Determines whether the token BUFFER, in its contemporaneous state,
   comprehends the PHRASE's tokens as its front elements, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (the boolean
    (convert-into-a-boolean-value
      (and
        (>= (token-buffer-size buffer)
            (phrase-length     phrase))
        (loop
          for phrase-token
            of-type simple-string
            in      (phrase-tokens phrase)
          and buffer-node
            of-type Buffer-Node
            =       (token-buffer-head     buffer)
            then    (buffer-node-successor buffer-node)
          always
            (string= phrase-token
                     (buffer-node-token buffer-node)))))))

;;; -------------------------------------------------------

(defun match-the-phrase-against-the-token-buffer (buffer phrase)
  "Juxtaposes the PHRASE's tokens with the BUFFER's front elements, on
   confirmation removing the matched BUFFER tokens, while returning a
   ``boolean'' value of ``T''; accompassing no causatum, while
   responding with ``NIL''."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (prepare-the-token-buffer-for-the-phrase buffer phrase)
  (the boolean
    (when (token-buffer-starts-with-the-phrase-p buffer phrase)
      (eject-the-tally-of-tokens-from-the-buffer buffer
        (phrase-length phrase))
      T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the identifier table.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of Phrase instruction) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (list
    (cons (assemble-a-phrase-from "Dear," "Reader")
          :salutation)
    (cons (assemble-a-phrase-from "Love," "Programer")
          :valediction)
    (cons (assemble-a-phrase-from "Let's" "move" "on" "parhaps?")
          :jump-back)
    (cons (assemble-a-phrase-from "So" "hello!")
          :jump-forward)
    (cons (assemble-a-phrase-from "Very" "great" "news!")
          :increment)
    (cons (assemble-a-phrase-from "Unpleasent" "news.")
          :decrement)
    (cons (assemble-a-phrase-from "You" "might" "thinking,")
          :move-right)
    (cons (assemble-a-phrase-from "But,")
          :move-left)
    (cons (assemble-a-phrase-from "I" "would" "like" "to" "hear"
                                  "from" "you!")
          :input)
    (cons (assemble-a-phrase-from "Well," "that's" "that.")
          :output))
  "The ``+IDENTIFIERS+'' global constant defines an association atwixen
   phrases in representation of the recognzied LETTER identifiers and
   their equivalent ``instruction'' objects.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun detect-the-matching-phrase (buffer)
  "Returns the instruction associated with the phrase from the
   ``+IDENTIFIERS+'' matching the token BUFFER's front tokens; or, upon
   its disrespondency, responds with the ``NIL'' sentinel."
  (declare (type Token-Buffer buffer))
  (the (or null instruction)
    (cdr
      (assoc-if
        #'(lambda (probed-phrase)
            (declare (type Phrase probed-phrase))
            (the boolean
              (match-the-phrase-against-the-token-buffer
                buffer
                probed-phrase)))
        +IDENTIFIERS+))))

;;; -------------------------------------------------------

(defun extract-the-next-instruction (buffer)
  "Returns the instruction associated with the phrase from the
   ``+IDENTIFIERS+'' matching the token BUFFER's front tokens; or, upon
   its disrespondency, signals an error of an unspecified type."
  (declare (type Token-Buffer buffer))
  (the instruction
    (or (detect-the-matching-phrase buffer)
        (error "The token sequence ~a cannot be matched with any ~
                identifier."
          buffer))))

;;; -------------------------------------------------------

(defun extract-the-instructions (buffer)
  "Extracts from the token BUFFER the ensconced LETTER instructions and
   returns a fresh ``program'' comprehending these."
  (declare (type Token-Buffer buffer))
  (the program
    (assemble-a-program-from
      (loop
        until   (token-buffer-is-empty-p               buffer)
        collect (extract-the-next-instruction          buffer)
        do      (ensure-a-minimum-token-buffer-size-of buffer 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :accessor      jump-table-connections
    :type          position-map
    :documentation "Affiliates the forward and back jump instructions
                    in a bidirectional fashion per procurationem of
                    their zero-based indices into the underlying
                    program's instruction vector."))
  (:documentation
    "The ``Jump-Table'' class applied itself to the castaldy of the
     bidirectional vincula betwixt the jump points in a LETTER program
     by adminiculum of their zero-based positions into the program's
     instruction vector."))

;;; -------------------------------------------------------

(defun prepare-a-vacant-jump-table ()
  "Creates and returns a `Jump-Table'' whose state at this instant of
   inchoacy amounts to a plene vacancy."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Connects the jump points designated by the zero-based indices
   START-POINT and END-POINT in the jump TABLE and returns no value.
   ---
   Any extant entry whose key conflates with either the START-POINT, the
   END-POINT, or both, will be subjected to a tacit supersession by the
   new twain."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-slots (connections) table
    (declare (type position-map connections))
    (psetf
      (gethash start-point connections) end-point
      (gethash end-point   connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun locate-the-destination-jump-point (table point-of-departure)
  "Returns the zero-based position of the jump point coalized with the
   POINT-OF-DEPARTURE in the jump TABLE; or, upon its direspondency,
   signals an error of an unspecified type."
  (declare (type Jump-Table table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure
          (jump-table-connections table))
        (error "No destination exists for the jump point ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (program)
  "Builds and returns a fresh ``Jump-Table'' dedicated to the vincula's
   contexture betwixt the matching jump points in the LETTER PROGRAM,
   mediated by adminiculum of their zero-based positions inwith its
   instruction vector."
  (declare (type program program))
  (the Jump-Table
    (let ((jump-points  (prepare-a-vacant-jump-table))
          (start-points NIL))
      (declare (type Jump-Table       jump-points))
      (declare (type (list-of fixnum) start-points))
      (dotimes (current-position (length program))
        (case (aref program current-position)
          (:jump-forward
            (push current-position start-points))
          (:jump-back
            (if start-points
              (connect-the-jump-points jump-points
                (pop start-points)
                current-position)
              (error "An unmatched back jump point has been ~
                      detected.")))
          (otherwise
            NIL)))
      (if start-points
        (error "One or more unmatched forwad jump points have been ~
                detected.")
        jump-points))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape cell.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cell ()
  ((value
    :initarg       :value
    :initform      0
    :accessor      cell-value
    :type          octet
    :documentation "The unsigned octet datum concredited to the cell's
                    castaldy, and which forms its state.")
   (predecessor
    :initarg       :predecessor
    :initform      NIL
    :accessor      cell-predecessor
    :type          (or null Cell)
    :documentation "The optional cell immediately preceding this one.")
   (successor
    :initarg       :successor
    :initform      NIL
    :accessor      cell-successor
    :type          (or null Cell)
    :documentation "The optional cell immediately succeeding this
                    one."))
  (:documentation
    "The ``Cell'' class implements a cell into the memory ``Tape'',
     realized as doubly linked list node; as a corollary comprehending
     the unsigned byte-valued datum, as well as a jumelle of optional
     pointers to the predecessor and successor cells."))

;;; -------------------------------------------------------

(defun prepare-a-new-cell (&optional (predecessor NIL)
                                     (successor   NIL))
  "Creates and returns a fresh tape ``Cell'', assuming in its state of
   inchoacy a zero-valued integer datum, optionally delineated by the
   vincula to a PREDECESSOR and/or SUCCESSOR cell."
  (declare (type (or null Cell) predecessor))
  (declare (type (or null Cell) successor))
  (the Cell
    (make-instance 'Cell
      :value       0
      :predecessor predecessor
      :successor   successor)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((header
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The front node into the tape's doubly linked list,
                    serving as a sentinel, always inclavated at the
                    leftmost position in order to facilitate
                    insertions.")
   (trailer
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The desinent node into the tape's doubly linked
                    list, serving as a sentinel, always inclavated at
                    the rightmost position in order to facilitate
                    insertions.")
   (pointer
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "A reference to the current selected cell.
                    ---
                    The cell pointer, albeit motile in its capacitation,
                    will never assume neither the HEADER nor the
                    TRAILER sentinels."))
  (:documentation
    "The ``Tape'' class furnishes an implementation of a bilaterally
     infinite dispansion of unsigned byte-valued cells, operated upon by
     a mobile cell pointer which at any instant designates the currently
     active entity, the sole cell endowed with amenability to
     perquisitions and modulations.
     ---
     This tape implementation's substratum is accommodated by a doubly
     linked list, along both lateralities involving sentinel nodes."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Establishes the vincula atwixen the inchoate node, represented by the
   TAPE's pointer, the header and the trailer sentinels, and returns no
   value."
  (declare (type Tape tape))
  (with-slots (header trailer pointer) tape
    (declare (type Cell header))
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (psetf
      (cell-successor   header)  pointer
      (cell-predecessor pointer) header
      (cell-successor   pointer) trailer
      (cell-predecessor trailer) pointer))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose state of inchoacy
   incorporates an aefauld, zero-valued cell."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun insert-a-cell-atwixen (predecessor successor)
  "Inserts a fresh cell atwixen the PREDECESOR and SUCCESOR node, its
   state's configuration compliant with the default plasmature, and
   returns the thus yielded cell."
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (the Cell
    (let ((new-cell (prepare-a-new-cell predecessor successor)))
      (declare (type Cell new-cell))
      (psetf
        (cell-successor   predecessor) new-cell
        (cell-predecessor successor)   new-cell)
      new-cell)))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Relocates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (with-slots (header pointer) tape
    (declare (type Cell header))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-predecessor pointer) header)
        (insert-a-cell-atwixen header pointer)
        (cell-predecessor pointer))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Relocates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (with-slots (trailer pointer) tape
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-successor pointer) trailer)
        (insert-a-cell-atwixen pointer trailer)
        (cell-successor pointer))))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (cell-value
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping into the valid unsigned byte
   range of [0, 255], and returns no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf (cell-value pointer)
      (mod new-value 256)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-letter-program (program)
  "Executes the LETTER PROGRAM and returns no value."
  (declare (type program program))
  (validate-the-program program)
  (let ((ip         0)
        (jump-table (supputate-the-jump-table-for program))
        (tape       (prepare-a-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (declare (type Tape       tape))
    (loop while (< ip (length program)) do
      (case (aref program ip)
        (:salutation
          NIL)
        (:valediction
          NIL)
        (:increment
          (incf (current-cell-value tape)))
        (:decrement
          (decf (current-cell-value tape)))
        (:move-left
          (move-the-cell-pointer-left tape))
        (:move-right
          (move-the-cell-pointer-right tape))
        (:jump-forward
          (when (zerop (current-cell-value tape))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (:jump-back
          (unless (zerop (current-cell-value tape))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (:output
          (format T "~c"
            (code-char
              (current-cell-value tape))))
        (:input
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value tape)
            (char-code
              (read-char NIL NIL #\null)))
          (clear-input))
        (otherwise
          (error "The instruction ~a cannot be processed."
            (aref program ip))))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-letter-code (code)
  "Interprets the piece of LETTER source CODE and returns no value."
  (declare (type string code))
  (execute-the-letter-program
    (extract-the-instructions
      (prepare-a-token-buffer-for
        (prepare-a-lexer-for code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the converter from brainfuck to LETTER.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-brainfuck-code-into-letter
    (bf-code
     &optional (destination NIL))
  "Translates the brainfuck code BF-CODE into an equivalent LETTER
   source code and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the output."
  (declare (type string      bf-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for current-token of-type standard-char across bf-code do
        (format destination "~&~a"
          (case current-token
            (#\]       "Let's move on parhaps?")
            (#\[       "So hello!")
            (#\+       "Very great news!")
            (#\-       "Unpleasent news.")
            (#\>       "You might thinking,")
            (#\<       "But,")
            (#\,       "I would like to hear from you!")
            (#\.       "Well, that's that.")
            (otherwise ""))))
      (with-output-to-string (letter-code)
        (declare (type string-stream letter-code))
        (translate-the-brainfuck-code-into-letter
          bf-code
          letter-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-the-letter-code
  "Dear, Reader
   I would like to hear from you!
   Well, that's that.
   Love, Programer")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This implementation limns an enker owelty to the brainfuck code
;;   ,.[-->+[>>]<[.]<<]
(interpret-the-letter-code
  "Dear, Reader
   I would like to hear from you!
   Well, that's that.
   So hello!
   Unpleasent news.
   Unpleasent news.
   You might thinking,
   Very great news!
   So hello!
   You might thinking,
   You might thinking,
   Let's move on parhaps?
   But,
   So hello!
   Well, that's that.
   Let's move on parhaps?
   But,
   But,
   Let's move on parhaps?
   Love, Programer")
