;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Jp32", invented by the Esolang user "Juanp32" and presented
;; on February 9th, 2025, conceived as a derivation of Urban Mueller's
;; "brainfuck", the cleronomy appropriated by thilk such of a nearly
;; enker compass; merely abluding from its designment's provenance in
;; the bailiwick of its syntaxis, this constituting a cambistry peracted
;; on the original one-symbol instruction identifiers for an ogdoad of
;; expressions of a ludibund caract, ostending a carency of formality in
;; their underlying diction.
;; 
;; 
;; Concept
;; =======
;; The Jp32 programming language constitutes an equipollent to its
;; brainfuck entheus, the lealty's incarnation neither of the wite's
;; tholance to deviate from the basic tenets in the program execution,
;; neither from the data castaldy's mode and epiphenomena; however, the
;; diorism's contribution establishes a variation on the octuple
;; instruction names, the default one-symbol identifiers experiencing a
;; supersession by phrases of an informal tongue's declamation.
;; 
;; == JP32: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; A program's assemblage ensues from a catena of phrases, informal and
;; ludibund in their designment, the sepiment commorant atwixen any
;; twissel rendered into one or more whitespaces.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of Jp32's recipiency does not elude brainfuck's
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
;; identifiers' semantical aspects to its brainfuck heritage, Jp32's
;; compass does neither actuate an ostention's commission designed with
;; supererogation with respect to its provenance, nor a curtailment in
;; the competences; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the Jp32 programming language's
;; facilities, thilk concomitantly concur with the offerings of its
;; brainfuck stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command               | Effect
;;   ----------------------+-------------------------------------------
;;   ok guys go left       | Translates the cell pointer one step to
;;                         | the left.
;;   ..................................................................
;;   ok guys go right      | Translates the cell pointer one step to
;;                         | the right.
;;   ..................................................................
;;   im a dot lol          | Prints the character whose ASCII code
;;                         | corresponds to the current cell value to
;;                         | the standard output conduit.
;;   ..................................................................
;;   its a comma. thats it | Queries the standard input conduit for a
;;                         | character and stores its ASCII code in the
;;                         | current cell.
;;   ..................................................................
;;   left side of box      | If the current cell value equals zero (0),
;;                         | moves the instruction pointer (IP) forward
;;                         | to the position immediately succeeding the
;;                         | matching "right side of box" instruction;
;;                         | otherwise proceeds as usual.
;;   ..................................................................
;;   right side of box     | If the current cell value does not equal
;;                         | zero (0), moves the instruction pointer
;;                         | (IP) back to the position immediately
;;                         | succeeding the matching "left side of box"
;;                         | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   LINE FTW              | Decrements the current cell value by one
;;                         | (1). If the new state transgresses the
;;                         | lower march of zero (0), the value wraps
;;                         | around to the upper extremum of 255.
;;   ..................................................................
;;   thats a plus :)       | Increments the current cell value by one
;;                         | (1). If the new state transgresses the
;;                         | upper march of 255, the value wraps around
;;                         | to the lower extremum of zero (0).
;;   ------------------------------------------------------------------
;; 
;; == JP32 AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation, Jp32's patration in language twissel's replication
;; homologates an equiparation applying to the operative bailiwick:
;; 
;;   -----------------------------------------------------------------
;;   Jp32                  | brainfuck | Causatum
;;   ----------------------+-----------+------------------------------
;;   ok guys go left       | <         | Move the cell pointer left.
;;   .................................................................
;;   ok guys go right	     | >         | Move the cell pointer right.
;;   .................................................................
;;   im a dot lol          | .         | Print the current cell.
;;   .................................................................
;;   its a comma. thats it | ,         | Input into the current cell.
;;   .................................................................
;;   left side of box      | [         | Jump forward if zero.
;;   .................................................................
;;   right side of box     | ]         | Jump back if not zero.
;;   .................................................................
;;   LINE FTW              | -         | Decrement the current cell.
;;   .................................................................
;;   thats a plus :)       | +         | Increment the current cell.
;;   -----------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the Jp32 source code
;; string's transcription into dedicated instruction representations
;; inwith whose potential an enhaused capacity for eath evaluation
;; wones, ere these entities ultimate vouchsafement of actual efficacy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-19
;; 
;; Sources:
;;   [esolang2025:Jp32]
;;   The Esolang contributors, "Jp32", March 9th, 2025
;;   URL: "https://esolangs.org/wiki/Jp32"
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
                                   &optional (element-type '*))
  "The ``list-of'' type defines a linked list comprehending zero or more
   members, each element partaking of the same complying with the
   ELEMENT-TYPE, for thilk is specified the generic sentinel ``*'' as
   the default."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, whose componency's edification amplects zero or more entries,
   everichon among these a cons cell whose sinistral compartment lends
   a commorancy to the key, adhering to the KEY-TYPE, while the dextral
   moeity accommodates the associated value, subsuming into the
   VALUE-TYPE, for both is imposed the default configuration of the
   generic sentinel ``*''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype dynamic-string-vector ()
  "The ``dynamic-string-vector'' type defines an adjustable vector
   whose componency enumerates zero or more simple strings."
  '(vector simple-string *))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   Jp32 instructions."
  '(member
    :increment
    :decrement
    :move-left
    :move-right
    :jump-forward
    :jump-back
    :output
    :input))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Jp32 program as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype position-map ()
  "The ``position-map'' type defines zero or more associations betwixt
   positions or indices, realized as a hash table whose keys and values
   both partake of a subsumption into the ``fixnum'' type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   (8) attiguous bits, thus forming an incolant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, the
   circumference appropriated by the dispands across, among others, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either by returning a
   freshly created object of the optated type, upon the SOURCE's failure
   to comply with its stipulations, or, if the SOURCE already subsumes
   into the ``simple-string'' species, by delivering the SOURCE itself.
   ---
   The SOURCE will not be modulated in any case."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun character-is-a-whitespace-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, a diorism
   inwith whose bournes are ensconced the horizontal tab, the newline,
   or line feed, the vertical tabulation, the form feed, the carriage
   return, as well as the traditional space, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string splitter.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Splitter ()
  ((source
    :initarg       :source
    :initform      (error "Missing the source for the splitter.")
    :type          simple-string
    :documentation "The string to segregate into its tokens.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE string."))
  (:documentation
    "The ``Splitter'' class furnishes an entity whose capacitation
     appertains to the extraction of whitespace-separated tokens from
     an underlying source string."))

;;; -------------------------------------------------------

(defun skip-accolent-whitespaces (splitter)
  "Proceeding from the current position into the SPLITTER's source,
   skips a catena enumerating zero or more whitespace characters, and
   returns no value."
  (declare (type Splitter splitter))
  (with-slots (source position) splitter
    (declare (type simple-string source))
    (declare (type fixnum        position))
    (setf position
      (or (position-if-not #'character-is-a-whitespace-p source
            :start position)
          (length source))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((splitter Splitter) &key)
  "Skips any leading whitespaces at the inchoacy of the SPLITTER's
   source string and returns no value."
  (declare (type Splitter splitter))
  (skip-accolent-whitespaces splitter)
  (values))

;;; -------------------------------------------------------

(defun prepare-a-splitter-for (source)
  "Creates and returns a fresh ``Splitter'' dedicated to the SOURCE
   string's segregation into its tokens."
  (declare (type string source))
  (the Splitter
    (make-instance 'Splitter :source
      (convert-into-a-simple-string source))))

;;; -------------------------------------------------------

(defun more-tokens-follow-p (splitter)
  "Determines whether one or more tokens remain to be requested from the
   SPLITTER, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Splitter splitter))
  (the boolean
    (with-slots (source position) splitter
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (convert-into-a-boolean-value
        (< position
           (length source))))))

;;; -------------------------------------------------------

(defun request-the-next-token (splitter)
  "Returns the next token from the SPLITTER.
   ---
   Upon its source's exhaustion, the SPLITTER responds to any request
   with a fresh empty string."
  (declare (type Splitter splitter))
  (the simple-string
    (with-slots (source position) splitter
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (let ((end-position
              (or (position-if #'character-is-a-whitespace-p source
                    :start position)
                  (length source))))
        (declare (type fixnum end-position))
        (prog1
          (subseq source position end-position)
          (setf position end-position)
          (skip-accolent-whitespaces splitter))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-a-program-from (instructions)
  "Creates and returns a fresh ``program'' comprehending the
   INSTRUCTIONS in their specified order."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the phrase.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Phrase ()
  ((tokens
    :initarg       :tokens
    :initform      (error "No tokens have been specified for the ~
                           phrase.")
    :reader        phrase-tokens
    :type          (list-of simple-string)
    :documentation "An ordered list of the phrase's tokens.")
   (length
    :initform      0
    :type          fixnum
    :reader        phrase-length
    :documentation "The tally of tokens comprising the phrase."))
  (:documentation
    "The ``Phrase'' class serves in the alligation of an ordered
     sequence of words, the same form a sentential tmema."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((phrase Phrase) &key)
  "Tallies the number of tokens comprising the PHRASE, stores this
   accompt in the same, and returns no value."
  (declare (type Phrase phrase))
  (setf (slot-value phrase 'length)
    (length
      (slot-value phrase 'tokens)))
  (values))

;;; -------------------------------------------------------

(defun assemble-a-phrase (&rest tokens)
  "Creates and returns a fresh ``Phrase'' comprised of the TOKENS."
  (declare (type (list-of simple-string) tokens))
  (the Phrase
    (make-instance 'Phrase :tokens tokens)))

;;; -------------------------------------------------------

(defmethod print-object ((phrase Phrase) (stream T))
  (declare (type Phrase phrase))
  (declare (type stream stream))
  (format stream "~{~a~^ ~}"
    (phrase-tokens phrase))
  (the Phrase phrase))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token buffer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Buffer ()
  ((splitter
    :initarg       :splitter
    :initform      (error "No splitter has been specified for the ~
                           token buffer.")
    :reader        token-buffer-splitter
    :type          Splitter
    :documentation "The entity responsible for the token's purveyance.")
   (tokens
    :initform      (list NIL)
    :accessor      token-buffer-tokens
    :type          (list-of (or null simple-string))
    :documentation "A queue of tokens, admitting new members as its
                    rear, while the front elements of whom may be
                    juxtaposed with a phrase and, upon their owelty,
                    elided.
                    ---
                    This queue is realized as a linked list of the
                    hitherto requested tokens from the SPLITTER,
                    complying to the principles of \"tail-consing\" in
                    order to vouchsafe an efficient insertion at the
                    rear; for which please consult the TAIL slot further
                    alow.")
   (tail
    :initform      NIL
    :accessor      token-buffer-tail
    :type          (list-of (or null simple-string))
    :documentation "A reference to the desinent cons cell in the
                    TOKENS linked list, which please see aboon, naited
                    for efficient insertions at the rear.")
   (size
    :initform      0
    :accessor      token-buffer-size
    :type          fixnum
    :documentation "The tally of tokens concredited to this buffer's
                    castaldy."))
  (:documentation
    "The ``Token-Buffer'' class represents the wike's pernor which
     imposed upon its the furnishment of a token buffer as a first in,
     first out salvatory, alligated to a consanguinity with a
     traditional queue, the admission into whose amplection is peracted
     on the rear, while perquisitions and ejections are accompassed
     along the frontal laterality.
     ---
     Described enkerly in its attrectation, a token buffer pursues its
     telos, the collation of tokens for their equiparation with the
     recognized Jp32 identifier phrases, by adminiculum of a
     ``Splitter'', this serving as a provenance to its tokens'
     obtention. Upon an phrase's juxtaposition, the requisite tally of
     words are requested from the splitter and appended to the buffer;
     in the case of this storage front's owelty with the docimasy's
     subject the entirety of matched elements is ejected from these
     head positions, and the representative instruction's gendrure limns
     the consectary; upon a mismatch, the next Jp32 phrase's eligibility
     fathom is indagated in accord with the selfsame principle. A
     failure to accommodate any identification incites an alternative
     route, inwith the same's purview merely the first token serves as
     a delendum, forecause its agency as a prefixion has been attested
     as an impossible prerequisite, this modulation accoutring a
     parasceve for an iterum matching stage's entelechy."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((buffer Token-Buffer) &key)
  "Stores a reference to the token BUFFER's desinent cons cell in its
   tail slot and returns no value."
  (declare (type Token-Buffer buffer))
  (setf (token-buffer-tail buffer)
    (last
      (token-buffer-tokens buffer)))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-token-buffer-for (splitter)
  "Creates and returns a fresh ``Token-Buffer'' whose token obtention
   is assigned to the SPLITTER's bailiwick."
  (declare (type Splitter splitter))
  (the Token-Buffer
    (make-instance 'Token-Buffer :splitter splitter)))

;;; -------------------------------------------------------

(defun token-buffer-is-exhausted-p (buffer)
  "Determines whether the token BUFFER is exhausted, that is, it is
   deprived of any elements while the underlying splitter cannot furnish
   anymore tokens, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (the boolean
    (convert-into-a-boolean-value
      (and
        (null
          (rest
            (token-buffer-tokens buffer)))
        (not
          (more-tokens-follow-p
            (token-buffer-splitter buffer)))))))

;;; -------------------------------------------------------

(defun load-the-next-token-into-the-buffer (buffer)
  "Requests the next token from the token BUFFER's underlying splitter,
   appends the same to the BUFFER, and returns no value."
  (declare (type Token-Buffer buffer))
  (setf (rest (token-buffer-tail buffer))
    (list
      (request-the-next-token
        (token-buffer-splitter buffer))))
  (setf (token-buffer-tail buffer)
    (rest
      (token-buffer-tail buffer)))
  (incf (token-buffer-size buffer))
  (values))

;;; -------------------------------------------------------

(defun sufficient-tokens-exist-for-the-phrase-p (buffer phrase)
  "Determines whether the token BUFFER comprehends a sufficient tally of
   elements to match its state against the PHRASE's own tokens,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (the boolean
    (convert-into-a-boolean-value
      (>= (token-buffer-size buffer)
          (phrase-length     phrase)))))

;;; -------------------------------------------------------

(defun prepare-the-token-buffer-for-the-phrase (buffer phrase)
  "Loads a sufficient tally of tokens from the token BUFFER underlying
   splitter into the BUFFER, until its state ostends a capacitation to
   be equiparated with the PHRASE' own tokens, or until the splitter
   cannot furnish any more items, and returns no value."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (with-slots (splitter) buffer
    (declare (type Splitter splitter))
    (loop
      until
        (or (sufficient-tokens-exist-for-the-phrase-p buffer phrase)
            (not (more-tokens-follow-p splitter)))
      do
        (load-the-next-token-into-the-buffer buffer)))
  (values))

;;; -------------------------------------------------------

(defun token-buffer-matches-the-phrase-p (buffer phrase)
  "Determines whether the front elements of the token BUFFER's content
   replicate the PHRASE's tokens, upon necessity loading a requisite
   tally of items from the underlying splitter, and returns on
   conformation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (the boolean
    (convert-into-a-boolean-value
      (and
        (sufficient-tokens-exist-for-the-phrase-p buffer phrase)
        (loop
          for phrase-token
            of-type simple-string
            in      (phrase-tokens phrase)
          and remaining-buffer-tokens
            of-type (list-of simple-string)
            =       (rest (token-buffer-tokens buffer))
            then    (rest remaining-buffer-tokens)
          always
            (and
              remaining-buffer-tokens
              (string= phrase-token
                       (first remaining-buffer-tokens))))))))

;;; -------------------------------------------------------

(defun eject-the-first-token-from-the-buffer (buffer)
  "Removes the first element from the token BUFFER and returns no
   value."
  (declare (type Token-Buffer buffer))
  (setf (token-buffer-tokens buffer)
    (rest
      (token-buffer-tokens buffer)))
  (decf (token-buffer-size buffer))
  (values))

;;; -------------------------------------------------------

(defun excise-the-phrase-from-the-token-buffer (buffer phrase)
  "Removes a tally of elements from the token BUFFER's front equal to
   the accompt of tokens in the PHRASE and returns no value."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (loop repeat (phrase-length phrase) do
    (eject-the-first-token-from-the-buffer buffer))
  (values))

;;; -------------------------------------------------------

(defun match-the-phrase (buffer phrase)
  "Attempts to match the PHRASE's tokens against the token BUFFER's
   front elements, contingently preceded by a requsite tally of tokens'
   obtention from the underlying splitter ere the equiparation, on the
   owelty's affirmation removes the matching elements from the BUFFER,
   and returns a ``boolean'' value of ``T''; otherwise peracts no
   causatum, concomitantly responding with ``NIL''."
  (declare (type Token-Buffer buffer))
  (declare (type Phrase       phrase))
  (prepare-the-token-buffer-for-the-phrase buffer phrase)
  (the boolean
    (when (token-buffer-matches-the-phrase-p buffer phrase)
      (excise-the-phrase-from-the-token-buffer buffer phrase)
      T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the Jp32 identifier table.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of Phrase instruction)
               +JP32-KEYWORDS+))

;;; -------------------------------------------------------

(defparameter +JP32-KEYWORDS+
  (list
    (cons (assemble-a-phrase "ok" "guys" "go" "left")
          :move-left)
    (cons (assemble-a-phrase "ok" "guys" "go" "right")
          :move-right)
    (cons (assemble-a-phrase "im" "a" "dot" "lol")
          :output)
    (cons (assemble-a-phrase "its" "a" "comma." "thats" "it")
          :input)
    (cons (assemble-a-phrase "left" "side" "of" "box")
          :jump-forward)
    (cons (assemble-a-phrase "right" "side" "of" "box")
          :jump-back)
    (cons (assemble-a-phrase "LINE" "FTW")
          :decrement)
    (cons (assemble-a-phrase "thats" "a" "plus" ":)")
          :increment))
  "The ``+JP32-KEYWORDS+'' global constant coalizes the recognized Jp32
   keyword phrases with the connable instruction representations.")

;;; -------------------------------------------------------

(defun look-up-the-jp32-phrase (optated-instruction)
  "Returns the phrase associated with the OPTATED-INSTRUCTION; or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type instruction optated-instruction))
  (the Phrase
    (or
      (car
        (rassoc-if
          #'(lambda (probed-instruction)
              (declare (type instruction probed-instruction))
              (eq probed-instruction optated-instruction))
          +JP32-KEYWORDS+))
      (error
        "No phrase amenable to the instruction ~a could be retrieved."
          optated-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Jp32 code parser.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-next-instruction (buffer)
  "Attempts to parse the token BUFFER's front elements as a Jp32
   instruction, returning on confirmation the affiliated ``instruction''
   representation, while deleting the matched tokens from the BUFFER;
   otherwise merely the first element is ejected and the ``NIL''
   sentinel is produced."
  (declare (type Token-Buffer buffer))
  (the (or null instruction)
    (let ((matching-instruction
            (cdr
              (assoc-if
                #'(lambda (probed-phrase)
                    (declare (type Phrase probed-phrase))
                    (match-the-phrase buffer probed-phrase))
                +JP32-KEYWORDS+))))
      (declare (type (or null instruction) matching-instruction))
      (unless matching-instruction
        (eject-the-first-token-from-the-buffer buffer))
      matching-instruction)))

;;; -------------------------------------------------------

(defun extract-the-jp32-instructions (code)
  "Parses the piece of Jp32 source CODE and returns a one-dimensional
   simple array comprehending its extracted instructions."
  (declare (type string code))
  (the program
    (let ((buffer
            (prepare-a-token-buffer-for
              (prepare-a-splitter-for code))))
      (declare (type Token-Buffer buffer))
      (assemble-a-program-from
        (loop
          until (token-buffer-is-exhausted-p buffer)
          for current-instruction
            of-type (or null instruction)
            =       (parse-the-next-instruction buffer)
          when current-instruction
            collect current-instruction)))))



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
     bidirectional vincula betwixt the jump points in an Jp32 program by
     adminiculum of their zero-based positions into the program's
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
   contexture betwixt the matching jump points in the Jp32 PROGRAM,
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
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :accessor      tape-bits
    :type          unsigned-byte
    :documentation "Maintains the explicitly sojourned cells' unsigned
                    byte states in an integer-encoded bit sequence,
                    each accolent octuple of positions allying with one
                    cell, the least significant bit (LSB) locations
                    chosen such as to answer to the lowest cell index.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          integer
    :documentation "The current cell pointer position.")
   (smallest-accessed-cell-index
    :initform      0
    :accessor      tape-smallest-accessed-cell-index
    :type          integer
    :documentation "The minimum cell index traversed by the cell
                    POINTER during a program's execution."))
  (:documentation
    "The ``Tape'' class serves in a bidirectionally infinite, unsigned
     byte-valued cell catena's furnishment, thilk is founded upon an
     integer-encoded bit sequence as the data castaldy's mechanism."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'', empight at its inchoacy in the
   default state comprising merely zero-valued cells."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun locate-the-selected-tape-bits (tape)
  "Returns an implementation-dependent byte specifier which locates the
   eight (8) accolent bits comprising the TAPE's currently selected
   cell."
  (declare (type Tape tape))
  (the T
    (byte 8
      (* (- (tape-pointer                      tape)
            (tape-smallest-accessed-cell-index tape))
         8))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value commorant in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (ldb
      (locate-the-selected-tape-bits tape)
      (tape-bits                     tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by its wrapping into the admissible unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (locate-the-selected-tape-bits tape)
      (tape-bits                     tape))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step in a dextrosinistral
   airt and returns no value."
  (declare (type Tape tape))
  (with-slots (bits pointer smallest-accessed-cell-index) tape
    (declare (type unsigned-byte bits)
             (ignorable          bits))
    (declare (type integer       pointer))
    (declare (type integer       smallest-accessed-cell-index))
    (decf pointer)
    (when (< pointer smallest-accessed-cell-index)
      (psetf
        smallest-accessed-cell-index pointer
        bits                         (ash bits 8))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step in a sinistrodextral
   airt and returns no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-jp32-program (program)
  "Executes the Jp32 PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (supputate-the-jump-table-for program))
        (tape       (prepare-a-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (declare (type Tape       tape))
    (loop while (< ip (length program)) do
      (case (aref program ip)
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

(defun interpret-the-jp32-code (code)
  "Interprets the piece of Jp32 source CODE and returns no value."
  (declare (type string code))
  (execute-the-jp32-program
    (extract-the-jp32-instructions
      (convert-into-a-simple-string code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck code parser.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-the-brainfuck-instructions (code)
  "Parses the piece of brainfuck source CODE and returns a
   one-dimensional simple array ensconcing its instructions."
  (declare (type simple-string code))
  (the program
    (let ((optimized-code (convert-into-a-simple-string code)))
      (declare (type simple-string optimized-code))
      (assemble-a-program-from
        (loop for token of-type character across optimized-code append
          (case token
            (#\+       '(:increment))
            (#\-       '(:decrement))
            (#\<       '(:move-left))
            (#\>       '(:move-right))
            (#\,       '(:input))
            (#\.       '(:output))
            (#\[       '(:jump-forward))
            (#\]       '(:jump-back))
            (otherwise NIL)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck code generator.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-program-into-brainfuck (program
                                             &key (destination NIL))
  "Generates and returns for the PROGRAM the equivalent brainfuck code,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the output."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null simple-string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from   0 by 1
        do
          (format destination "~c"
            (case current-instruction
              (:increment    #\+)
              (:decrement    #\-)
              (:move-right   #\>)
              (:move-left    #\<)
              (:jump-forward #\[)
              (:jump-back    #\])
              (:output       #\.)
              (:input        #\,)
              (otherwise
                (error "The instruction ~a at the position ~d cannot ~
                        be translated into a brainfuck instruction."
                  current-instruction current-position)))))
      (convert-into-a-simple-string
        (with-output-to-string (brainfuck-code)
          (declare (type string-stream brainfuck-code))
          (translate-the-program-into-brainfuck program
            :destination brainfuck-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Jp32 code generator.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-program-into-jp32 (program
                                        &key (destination NIL))
  "Generates and returns for the PROGRAM the equivalent Jp32 code,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the output."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null simple-string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        do  (format destination "~&~a"
              (look-up-the-jp32-phrase current-instruction)))
      (convert-into-a-simple-string
        (with-output-to-string (jp32-code)
          (declare (type string-stream jp32-code))
          (translate-the-program-into-jp32 program
            :destination jp32-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the translation operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-jp32-code-into-brainfuck (jp32-code
                                               &key (destination NIL))
  "Translates the piece of JP32-CODE into its brainfuck equivalent,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION, the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the result."
  (declare (type string      jp32-code))
  (declare (type destination destination))
  (the (or null simple-string)
    (translate-the-program-into-brainfuck
      (extract-the-jp32-instructions jp32-code)
      :destination destination)))

;;; -------------------------------------------------------

(defun translate-the-brainfuck-code-into-jp32 (brainfuck-code
                                               &key (destination NIL))
  "Translates the piece of BRAINFUCK-CODE into its Jp32 equivalent,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION, the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the result."
  (declare (type simple-string brainfuck-code))
  (declare (type destination   destination))
  (the (or null simple-string)
    (translate-the-program-into-jp32
      (extract-the-brainfuck-instructions brainfuck-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
;; 
;; This limns an owelty to the brainfuck code:
;;   ,[.,]
(interpret-the-jp32-code
  "its a comma. thats it
   left side of box
   im a dot lol
   its a comma. thats it
   right side of box")

;;; -------------------------------------------------------

;; Print the message "Hello, World!" to the standard output.
(interpret-the-jp32-code
  "
  thats a plus :)
  left side of box
  LINE FTW
  LINE FTW
  ok guys go right
  LINE FTW
  left side of box
  ok guys go right
  ok guys go right
  thats a plus :)
  ok guys go right
  LINE FTW
  LINE FTW
  LINE FTW
  LINE FTW
  LINE FTW
  ok guys go left
  ok guys go left
  right side of box
  ok guys go left
  LINE FTW
  LINE FTW
  ok guys go left
  LINE FTW
  LINE FTW
  LINE FTW
  right side of box
  ok guys go right
  LINE FTW
  im a dot lol
  ok guys go right
  ok guys go right
  ok guys go right
  thats a plus :)
  im a dot lol
  ok guys go right
  ok guys go right
  im a dot lol
  im a dot lol
  thats a plus :)
  thats a plus :)
  thats a plus :)
  left side of box
  im a dot lol
  ok guys go right
  right side of box
  ok guys go left
  ok guys go left
  ok guys go left
  ok guys go left
  im a dot lol
  thats a plus :)
  thats a plus :)
  thats a plus :)
  im a dot lol
  LINE FTW
  LINE FTW
  LINE FTW
  LINE FTW
  LINE FTW
  LINE FTW
  im a dot lol
  ok guys go left
  ok guys go left
  LINE FTW
  im a dot lol
  ok guys go right
  ok guys go right
  ok guys go right
  ok guys go right
  thats a plus :)
  im a dot lol
  ")

;;; -------------------------------------------------------

;; Convert the "Hello, World!" from its brainfuck provenance to the
;; Jp32 equivalent and execute thilk.
(interpret-the-jp32-code
  (translate-the-brainfuck-code-into-jp32
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))
