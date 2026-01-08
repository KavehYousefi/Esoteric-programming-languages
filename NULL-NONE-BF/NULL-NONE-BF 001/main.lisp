;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; lanugage "NULL-NONE-BF", invented by the Esolang user "Xi-816" and
;; presented on September 2nd, 2023, conceived as a derivation of Urban
;; Mueller's "brainfuck", while, maugre its patration in the
;; quantitative and syntactical congruency with the entheus, deploys a
;; set of identifiers whose establishment's choice ensues from several
;; permutations partaken in by the constituents "NULL" and "NONE", these
;; instances alligated by a hyphen each parcel's interstice pursuing a
;; trisulc compound's plasmature into a singular composition.
;; 
;; 
;; Concept
;; =======
;; The NULL-NONE-BF programming language constitutes an equipollent to
;; its brainfuck entheus, the lealty's incarnation neither of the wite's
;; tholance to deviate from the basic tenets in the program execution,
;; neither from the data castaldy's mode and epiphenomena; however, the
;; diorism's contribution establishes a variation on the octuple
;; instruction names, the default one-symbol identifiers experiencing
;; a supersession through combinations of the token twissel "NULL" and
;; "NONE" into a treble unity, the adnascence such as to entertain
;; atwixen any constituent an aefauld hyphen's mediation.
;; 
;; == NULL-NONE-BF: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; NULL-NONE-BF's kenspeckle designment is edified upon the provision of
;; succedanea to brainfuck's single-character instruction identifiers
;; by combinations of the words "NULL" and "NONE" into trisulc
;; compounds, the sepiment atwixen each such intrined membership a
;; single hyphen; while the distinguishment of these composite tokens
;; themselves naits one or more whitespaces as the responsible merist.
;; 
;; As a fact of interesting vallidom, the diction so produced emerges
;; as an epiphenomenon of the same author's cognate, however matrix-
;; and stack-based, language "NULL-NONE NOTHING".
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of NULL-NONE-BF's recipiency does not elude brainfuck's
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
;; identifiers' semantical aspects to its brainfuck heritage,
;; NULL-NONE-BF's compass does neither actuate an ostention's commission
;; designed with supererogation with respect to its provenance, nor a
;; curtailment in the competences; in corollary, this cleronomy accounts
;; for an octuple contingency, amplecting in its compass the cell
;; pointer movement, basic arithmetics, input and output facilities, as
;; well as an aefauld construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the NULL-NONE-BF programming language's
;; facilities, thilk concomitantly concur with the offerings of its
;; brainfuck stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   NULL-NULL-NULL | Increments the current cell value by one (1). If
;;                  | the new state transgresses the upper march of
;;                  | 255, the value wraps around to the lower extremum
;;                  | of zero (0).
;;   ..................................................................
;;   NULL-NULL-NONE | Decrements the current cell value by one (1). If
;;                  | the new state transgresses the lower march of
;;                  | zero (0), the value wraps around to the upper
;;                  | extremum of 255.
;;   ..................................................................
;;   NULL-NONE-NULL | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   NULL-NONE-NONE | Translates the cell pointer one step to the
;;                  | right.
;;   ..................................................................
;;   NONE-NULL-NULL | Prints the character whose ASCII code corresponds
;;                  | to the current cell value to the standard output
;;                  | conduit.
;;   ..................................................................
;;   NONE-NULL-NONE | Queries the standard input conduit for a
;;                  | character and stores its ASCII code in the
;;                  | current cell.
;;   ..................................................................
;;   NONE-NONE-NULL | If the current cell value equals zero (0), moves
;;                  | the instruction pointer (IP) forward to the
;;                  | position immediately succeeding the matching
;;                  | "NONE-NONE-NONE" instruction; otherwise proceeds
;;                  | as usual.
;;   ..................................................................
;;   NONE-NONE-NONE | If the current cell value does not equal zero
;;                  | (0), moves the instruction pointer (IP) back to
;;                  | the position immediately succeeding the matching
;;                  | "NONE-NONE-NULL " instruction; otherwise proceeds
;;                  | as usual.
;;   ------------------------------------------------------------------
;; 
;; == NULL-NONE-BF AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation, NULL-NONE-BF's patration in language twissel's
;; replication homologates an equiparation applying to the operative
;; bailiwick:
;; 
;;   ----------------------------------------------------------
;;   NULL-NONE-BF   | brainfuck | Causatum
;;   ---------------+-----------+------------------------------
;;   NULL-NULL-NULL | +         | Increment the current cell.
;;   ..........................................................
;;   NULL-NULL-NONE | -         | Decrement the current cell.
;;   ..........................................................
;;   NULL-NONE-NULL | <         | Move the cell pointer left.
;;   ..........................................................
;;   NULL-NONE-NONE | >         | Move the cell pointer right.
;;   ..........................................................
;;   NONE-NULL-NULL | .         | Print the current cell.
;;   ..........................................................
;;   NONE-NULL-NONE | ,         | Input into the current cell.
;;   ..........................................................
;;   NONE-NONE-NULL | [         | Jump forward if zero.
;;   ..........................................................
;;   NONE-NONE-NONE | ]         | Jump back if not zero.
;;   ----------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the NULL-NONE-BF
;; source code string's transcription into a vector of dedicated
;; instruction objects, furnishing a parasceve to the actual execution
;; stage.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-06
;; 
;; Sources:
;;   [esolang:2023:NULL-NONE NOTHING]
;;   The Esolang contributors, "NULL-NONE NOTHING", September 3rd, 2023
;;   URL: "https://esolangs.org/wiki/NULL-NONE_NOTHING"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-derived-type
    (type-name (candidate-name &rest lambda-list) &body body)
  "Defines a derived type the provenance of whose agnomination wones in
   the TYPE-NAME, the LAMBDA-LIST in its ipissima verba fashion forming
   the formal parameter list, and whose fathomed object enjoys the
   CANDIDATE-NAME nevening's parcery, evaluates the BODY forms, and
   construes the desinent form's primary return value as the docimasy's
   conclusion, a \"generalized boolean\" value of \"true\" serving as
   the assessment's affirmation, while a \"false\" response conflates
   with its compatibility's rejection.
   ---
   The first BODY form, upon its resolution to a string, experiences a
   conspection as a documentation string to the derived type, and, as
   a consectary, is reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
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

(define-a-derived-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a linked list whose componency
   enumerates zero or more elements, everichon among these subsuming
   into the ELEMENT-TYPE, for thilk is imposed the comprehensive type
   ``T'' as the default configuration."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-a-derived-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose constitution
   founds upon a collection of zero or more entries, each key of which
   complies with the KEY-TYPE and answers to a value of the VALUE-TYPE,
   for both is assigned the comprehensive ``T'' as the default
   configuration."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and
          (typep current-key   key-type)
          (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variations on
   NULL-NONE-BF instructions."
  '(member
    :increment
    :decrement
    :move-left
    :move-right
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable NULL-NONE-BF program
   as a one-dimensional simple array comprehending zero or more
   ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping betwixt the
   jump points in a NULL-NONE-BF program, established per procurationem
   of the zero-based positions of the respective operations in the
   parsed instructions sequence, and realized by adminiculum of a hash
   table whose keys and values both assume ``finum'' representations of
   these indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   (8) accolent bits, and thus forming an incolant of the closed integer
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines the NULL-NONE-BF memory tape as a
   bilaterally infinite dispansion of unsigned byte-valued cells, its
   tangible entelechy that of a hash table whose signed integer keys
   ally with the notion of the cell indices, and whose affiliated
   ``octet'' values equiparate with the maintained states."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
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
   inwith whose compass are subsumed the traditional space, horizontal
   tab, vertical tabulation, form feed, carriage return, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the tokenizer.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-token (source start)
  "Proceeding from the START position into the SOURCE, searches for the
   nearest following token and returns two values:
     (1) If a token could be detected, the index of its first character
         in the SOURCE; otherwise, the length of the SOURCE itself.
     (2) If a token could be detected, the index immediately succeeding
         its desinent character in the SOURCE; otherwise, the length of
         the SOURCE itself.
   ---
   Please heed the contingency for the two return values' conflation,
   which, as a corollary, signifies the absence of any further tokens
   from the SOURCE."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let ((token-start-point
          (or (position-if-not #'whitespace-character-p source
                :start start)
              (length source))))
    (declare (type fixnum token-start-point))
    (the (values fixnum fixnum)
      (values
        token-start-point
        (or (position-if #'whitespace-character-p source
              :start token-start-point)
            (length source))))))

;;; -------------------------------------------------------

(defun extract-the-next-token (source start)
  "Proceeding from the START position into the SOURCE, searches for the
   nearest following token and returns two values:
     (1) A simple string representing the extracted token.
     (2) The position into the source immediately succeeding the thus
         extracted token.
   ---
   Please heed the contingency for an empty string's production in the
   first return value (1), which, as a corollary, signifies the absence
   of any further tokens from the SOURCE."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (multiple-value-bind (token-start-point token-end-point)
      (locate-the-next-token source start)
    (declare (type fixnum token-start-point))
    (declare (type fixnum token-end-point))
    (the (values simple-string fixnum)
      (values
        (subseq source token-start-point token-end-point)
        token-end-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro switch-on-the-string (key-form &rest clauses)
  "Evaluates the KEY-FORM, yielding the test key, and juxtaposes each
   clause among the CLAUSES, its conformation expected to produce a list
   comprehending one or more elements, the first component among these
   the clause key, mandated to represent to resolve to a string
   designator, the remaining items the clause body, with the test key,
   upon an owelty's confirmation evaluating the clause body and
   returning the desinent form's return values; otherwise, confronted
   with a complete failure in the matching process, responds with
   ``NIL''.
   ---
   The desinent CLAUSES option might bear a key amounting to the symbol
   ``T'' or ``otherwise'', in which circumstance's transpiration a
   default, or fallback, clause applies, its activation a sequela of
   all prevenient options' abortive exhaustion, while, anent the clause
   body, the acquainted nomothesy shall exercise its purview."
  (let ((test-key (gensym)))
    (declare (type symbol test-key))
    (flet ((signifies-the-default-case (clause-key)
            "Determines whether the CLAUSE-KEY signifies a default
             clause, introduced either via the keyword \"T\" or
             \"otherwise\", returning on confirmation a ``boolean''
             value of ``T'', otherwise ``NIL''."
            (declare (type T clause-key))
            (the boolean
              (convert-into-a-boolean-value
                (and (symbolp clause-key)
                     (or (eq clause-key 'otherwise)
                         (eq clause-key 'T)))))))
      `(let ((,test-key ,key-form))
         (declare (type string ,test-key))
         (cond
           ,@(loop for current-clause of-type list in clauses collect
               (destructuring-bind (clause-key &rest clause-forms)
                   current-clause
                 (declare (type T    clause-key))
                 (declare (type list clause-forms))
                 (if (signifies-the-default-case clause-key)
                   `(T
                      ,@clause-forms)
                   `((string= ,clause-key ,test-key)
                      ,@clause-forms)))))))))

;;; -------------------------------------------------------

(defun string-is-empty-p (subject)
  "Determines whether the SUBJECT represents an empty string, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string subject))
  (the boolean
    (convert-into-a-boolean-value
      (string= subject ""))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-as-a-null-none-bf-instruction (token)
  "Attents to parse the TOKEN as a NULL-NONE-BF instruction identifier,
   upon a successful equiparation responding with a connable
   ``instruction'' representation; otherwise returns ``NIL''."
  (declare (type simple-string token))
  (the (or null instruction)
    (switch-on-the-string token
      ("NULL-NULL-NULL" :increment)
      ("NULL-NULL-NONE" :decrement)
      ("NULL-NONE-NULL" :move-left)
      ("NULL-NONE-NONE" :move-right)
      ("NONE-NULL-NULL" :output)
      ("NONE-NULL-NONE" :input)
      ("NONE-NONE-NULL" :jump-forward)
      ("NONE-NONE-NONE" :jump-back)
      (otherwise        NIL))))

;;; -------------------------------------------------------

(defun parse-the-null-none-bf-program (code)
  "Parses the piece of NULL-NONE-BF source CODE and returns a
   ``program'' representation of the ensconced instructions."
  (declare (type simple-string code))
  (the program
    (coerce
      (loop
        with current-position    of-type fixnum                = 0
        with current-token       of-type simple-string         = ""
        with current-instruction of-type (or null instruction) = NIL
        do
          (multiple-value-setq (current-token current-position)
            (extract-the-next-token code current-position))
          (setf current-instruction
            (parse-as-a-null-none-bf-instruction current-token))
        until
          (string-is-empty-p current-token)
        when current-instruction
          collect current-instruction)
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-the-jump-table-for (program)
  "Engages in a collation of the NULL-NONE-BF PROGRAM's jump points and
   the jumelles' contexture, returning a ``jump-table'' ensconcement of
   these established vincula."
  (declare (type program program))
  (let ((connections         (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       connections))
    (declare (type (list-of fixnum) forward-jump-points))
    (the jump-table
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from   0 by 1
        
        if (eq current-instruction :jump-forward) do
          (push current-position forward-jump-points)
        else if (eq current-instruction :jump-back) do
          (if forward-jump-points
            (let ((start-point (pop forward-jump-points))
                  (end-point   current-position))
              (declare (type fixnum start-point))
              (declare (type fixnum end-point))
              (psetf
                (gethash start-point connections) end-point
                (gethash end-point   connections) start-point))
            (error "An unmatched back jump point has been detected."))
        end
        
        finally
          (if forward-jump-points
            (error "One or more unmatched forward jump points have ~
                    been detected.")
            (return connections))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the tape operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cell-value-at (tape index)
  "Returns the unsigned byte datum stored in the cell TAPE amenable to
   the INDEX."
  (declare (type tape    tape))
  (declare (type integer index))
  (the octet
    (gethash index tape 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value tape index)
  "Stores the NEW-VALUE in the TAPE cell amenable to the INDEX,
   contingently preceded by a wrapping of the state into the admissible
   unsigned byte interval of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type tape    tape))
  (declare (type integer index))
  (setf (gethash index tape)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-null-none-bf-program (program)
  "Executes the NULL-NONE-BF PROGRAM's instructions and returns no
   value."
  (declare (type program program))
  (let ((ip           0)
        (jump-points  (build-the-jump-table-for program))
        (tape         (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-points))
    (declare (type tape       tape))
    (declare (type integer    cell-pointer))
    (symbol-macrolet
        ((current-cell-value
          (the (or octet integer)
            (cell-value-at tape cell-pointer))))
      (declare (type (or octet integer) current-cell-value))
      (loop while (< ip (length program)) do
        (case (aref program ip)
          (:increment
            (incf current-cell-value))
          (:decrement
            (decf current-cell-value))
          (:move-left
            (decf cell-pointer))
          (:move-right
            (incf cell-pointer))
          (:output
            (format T "~c"
              (code-char current-cell-value)))
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf current-cell-value
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          (:jump-forward
            (when (zerop current-cell-value)
              (setf ip
                (gethash ip jump-points))))
          (:jump-back
            (unless (zerop current-cell-value)
              (setf ip
                (gethash ip jump-points))))
          (otherwise
            (error "The instruction ~s cannot be processed."
              (aref program ip))))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-null-none-bf-code (code)
  "Interprets the piece of NULL-NONE-BF source CODE and returns no
   value."
  (declare (type string code))
  (execute-the-null-none-bf-program
    (parse-the-null-none-bf-program
      (coerce code 'simple-string)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "Hello World!" to the standard output conduit.
(interpret-the-null-none-bf-code
  "NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NONE-NONE-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NONE-NONE-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NULL-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NONE-NULL NULL-NONE-NULL NULL-NONE-NULL NULL-NONE-NULL NULL-NULL-NONE NONE-NONE-NONE NULL-NONE-NONE NULL-NULL-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NONE-NONE NULL-NULL-NONE NULL-NONE-NONE NULL-NONE-NONE NULL-NULL-NULL NONE-NONE-NULL NULL-NONE-NULL NONE-NONE-NONE NULL-NONE-NULL NULL-NULL-NONE NONE-NONE-NONE NULL-NONE-NONE NULL-NONE-NONE NONE-NULL-NULL NULL-NONE-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NONE-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NONE-NULL-NULL NONE-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NONE-NULL-NULL NULL-NONE-NONE NULL-NONE-NONE NONE-NULL-NULL NULL-NONE-NULL NULL-NULL-NONE NONE-NULL-NULL NULL-NONE-NULL NONE-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NULL-NULL-NULL NONE-NULL-NULL NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NONE-NULL-NULL NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NULL-NULL-NONE NONE-NULL-NULL NULL-NONE-NONE NULL-NONE-NONE NULL-NULL-NULL NONE-NULL-NULL NULL-NONE-NONE NULL-NULL-NULL NULL-NULL-NULL NONE-NULL-NULL")

;;; -------------------------------------------------------

;; A repeating cat program which terminates on a "null character" input,
;; limining an owelty to the brainfuck code
;; 
;;   ,[.,]
(interpret-the-null-none-bf-code
  "NONE-NULL-NONE
   NONE-NONE-NULL
     NONE-NULL-NULL
     NONE-NULL-NONE
   NONE-NONE-NONE")
