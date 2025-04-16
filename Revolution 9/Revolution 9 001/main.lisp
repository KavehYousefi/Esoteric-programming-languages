;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Revolution 9", invented by the Esolang user
;; "188.26.204.119" and presented on April 13th, 2023, its dioristic
;; property's woning in the reformulation of Urban Mueller's "brainfuck"
;; in a diction desumed from and alluding to the song "Revolution 9" of
;; the rock band "The Beatles", the remaining propria account for
;; verbatim appropriation in its cleronomy's course.
;; 
;; 
;; Concept
;; =======
;; The Revolution 9 programming language represents a paregal to the
;; brainfuck programming language whose conflation propagates through
;; all aspects except for the synactical department, where the single
;; symbol behests experience an elevated nimiety by replication of
;; phrases from the The Beatles song "Revolution 9".
;; 
;; == REVOLUTION 9: AN ARTISTIC INTERPRETATION OF BRAINFUCK ==
;; The Revolution 9 programming language's agnomination constituting an
;; allusion to the epoynoums song of the rock band The Beatles alreadys
;; serves to bewray its kenspeckle designment, scilicet, the employment
;; of terms from the same musical provenance for the application of
;; brainfuck's octuple facilities.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE CATENA OF UNSIGNED BYTES ==
;; A further appropriation from its brainfuck stock-father, Revolution 9
;; deploys a bilateral infinite dispansion of unsigned byte-valued
;; cells, arranged seriatim, and operated upon a mobile cell pointer
;; which at any instant selects the currently active cell.
;; 
;; Each cell's capacity is measured at an aefauld unsigned byte value,
;; its contingency exhausted by the integral closed range of [0, 255].
;; Upon any of its bournes' transgression, the state wraps around along
;; the athwart extremum.
;; 
;; The cells' arrangement adights thilk in a bilaterally infinite
;; catena, upon which operates a mobile cell pointer, its dever the
;; designation at any point of the program of the currently active unit,
;; the sole entity endowed with amenability to perquisitions and
;; modulations. By its stillatim progressions along both airts, the
;; pointer may traverse any cell in the memory.
;; 
;; 
;; Instructions
;; ============
;; Revolution 9's appropriation of brainfuck's concept, exempted
;; therefrom merely the syntaxis, entalents the former with an
;; equipollence in its competences; in corollary, the octuple
;; instruction set retains its characteristics, however, while it
;; assumes a new guise.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a cursory mete of gnarity's
;; adhibition anent the operative warklumes offered by the language:
;; 
;;   ------------------------------------------------------------------
;;   Commands             | Effect
;;   ---------------------+--------------------------------------------
;;   It's alright         | Translates the cell pointer one step to the
;;                        | left.
;;   ..................................................................
;;   turn me on, dead man | Translates the cell pointer one step to the
;;                        | right.
;;   ..................................................................
;;   Number 9             | Increments the current cell value by one
;;                        | (1). If the new state transgresses the
;;                        | upper extremum of 255, the cell value wraps
;;                        | around to the minimum of zero (0).
;;   ..................................................................
;;   if you become naked  | Decrements the current cell value by one
;;                        | (1). If the new state transcends alow the
;;                        | lower extremum of zero (0), the cell value
;;                        | wraps around to the maximum of 255.
;;   ..................................................................
;;   The Beatles          | Prints the character whose ASCII code
;;                        | corresponds to the current cell value to
;;                        | the standard output.
;;   ..................................................................
;;   Paul is dead         | Queries the standard input for a character
;;                        | and stores its ASCII code in the current
;;                        | cell.
;;   ..................................................................
;;   Revolution 1         | If the current cell value equals zero (0),
;;                        | moves the instruction pointer (IP) forward
;;                        | to the position immediately succeeding the
;;                        | matching "Revolution 9" instruction;
;;                        | otherwise proceeds as usual.
;;   ..................................................................
;;   Revolution 9         | If the current cell value does not equal
;;                        | zero (0), moves the instruction pointer
;;                        | (IP) to the position immediately succeeding
;;                        | the matching "Revolution 1" instruction;
;;                        | otherwise proceeds as usual.
;;   ..................................................................
;; 
;; == REVOLUTION 9 AND BRAINFUCK ==
;; Revolution 9's status as a syntactical rejuvenation of brainfuck's
;; principles to patration serves as the provenance for the sole
;; divergence's emphasis, with the following tabular illustration
;; ordained to equiparate the two language's keywords:
;; 
;;   ---------------------------------
;;   Revolution 9         | brainfuck
;;   ---------------------+-----------
;;   It's alright         | <
;;   .................................
;;   turn me on, dead man | >
;;   .................................
;;   Number 9             | +
;;   .................................
;;   if you become naked  | -
;;   .................................
;;   The Beatles          | .
;;   .................................
;;   Paul is dead         | ,
;;   .................................
;;   Revolution 1         | [
;;   .................................
;;   Revolution 9         | ]
;;   .................................
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's implementation has been enacted in the programming
;; language Common Lisp, its kenspeckle effort's exercise harbored in
;; the lexical analyzation and parsing process, whence ensues a sequence
;; of instruction identifier symbols for the interpreter's procession.
;; 
;; == PARSING: AN APPROACH IN SEVERAL TIERS ==
;; The tiers, ligated into a linear system of interrelation, shall be
;; expounded in the following listing:
;; 
;;   (1) An identifier table is assembled which maps to the serelepes
;;       adight words and punctuation characters the representative
;;       instruction keyword symbols.
;;   
;;   (2) A tokenizer applies itself to the segregation of the Revolution
;;       9 source string into a stream of string-valued tokens.
;;   
;;   (3) A token buffer, superimposed on the tokenizer, is assigned to
;;       the tokenizer products' usufruct, querying and storing these
;;       in a queue as a consequence of necessity.
;;   
;;   (4) A parser iterates over all recognized Revolution 9 phrases,
;;       everichon from this set a list of strings, and matches these
;;       against the token buffer's front elements, the latter entity
;;       contingently loading additional resources from the underlying
;;       tokenizer.
;;   
;;       (4.1) If a match has occurred, the successfully equiparated
;;             strings from the token buffer's front are expunged, and
;;             a representative instruction identifier is collected.
;;       
;;       (4.2) Otherwise, merely the buffer's first element edifies the
;;             delenda's sole componency, concluding from the
;;             signification of the input source's current subsequence's
;;             ineligibility to deliver an operation at its inchoation.
;;   
;;   (5) The stage -> (4) operates in an iterance cycle, until both the
;;       token buffer's and the tokenizer's exhaustion signal the
;;       source code evaluation's patration. The instruction identifiers
;;       collated in the step -> (4.1) are subsequently returned as a
;;       more covenable Revolution 9 program representation for the
;;       succeeding interpretation process.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-13
;; 
;; Sources:
;;   [esolang2025Revolution9]
;;   The Esolang contributors, "Revolution 9", April 10th, 2025
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list whose conformation is edified
   upon zero or more objects entailed in a list, everichon of these
   members adhering to the ELEMENT-TYPE, for thilk holds the default
   of the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (every
                #'(lambda (current-element)
                    (declare (type T current-element))
                    (typep current-element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, each such a cons whose first
   constituent, or \"car\" entails an object of the KEY-TYPE, and whose
   dextral moeity, or \"cdr\" compartment, accommodated a commorancy to
   a value of the VALUE-TYPE, both defaulting to the generic sentinel
   ``*''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key from these adhering to the KEY-TYPE and
   ligated to a value of the VALUE-TYPE, for both is imposed the generic
   sentinel ``*'' as a default."
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
                (and
                  (or (eq    key-type      '*)
                      (typep current-key   key-type))
                  (or (eq    value-type    '*)
                      (typep current-value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   Revolution 9 instructions."
  '(member
    :move-left
    :move-right
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Revolution 9 program as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping betwixt Revolution 9
   phrases and representative instructions identifiers, baed upon an
   association list whose keys contribute a list of the phrase tokens,
   answering to ``instruction'' equivalencies."
  '(association-list-of (list-of simple-string) instruction))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirection association betwixt
   jump points in a Revolution 9, their mediation accomplished by the
   instructions' zero-based positions into the program's source code,
   and manifesting in a hash table whose keys and values both assume
   fixnum objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, and thus a commorant of the closed integral interval
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which entails, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\", and
   ensuing from this notion, produces a veridicous Boolean tantamount
   thereof, returning for a non-``NIL'' input a ``boolean'' value of
   ``T''; otherwise, for the ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '((("It's" "alright")                  . :move-left)
    (("turn" "me" "on" "," "dead" "man") . :move-right)
    (("Number" "9")                      . :increment)
    (("if" "you" "become" "naked")       . :decrement)
    (("The" "Beatles")                   . :output)
    (("Paul" "is" "dead")                . :input)
    (("Revolution" "1")                  . :jump-forward)
    (("Revolution" "9")                  . :jump-back))
  "Affiliates the recognized phrases with symbolic operation
   representatives.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' comprehending the
   INSTRUCTIONS."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of list operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-starts-with-p (source prefix)
  "Determines whether the SOURCE list commences with the string values
   specifeid in the PREFIX, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type (list-of string) source))
  (declare (type (list-of string) prefix))
  (the boolean
    (get-boolean-value-of
      (and
        (>= (length source)
            (length prefix))
        (every #'string= source prefix)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, including
   in this diorism the space, horizontal tab, and newline specimens,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Linefeed #\Newline #\Space #\Tab)
        :test #'char=))))

;;; -------------------------------------------------------

(defun punctuation-character-p (candidate)
  "Determines whether the CANDIDATE represents a punctuation character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate ",;:.?!" :test #'char=))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid consitutent for
   a word, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not
      (or (whitespace-character-p  candidate)
          (punctuation-character-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string tokenizer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tokenizer ()
  ((source
    :initarg       :source
    :initform      (error "Missing tokenizer source.")
    :reader        tokenizer-source
    :type          string
    :documentation "The piece of Revolution 9 source code to segregate
                    into tokens.")
   (position
    :initform      0
    :accessor      tokenizer-position
    :type          fixnum
    :documentation "The current position into the SOURCE."))
  (:documentation
    "The ``Tokenizer'' class serves in the furnishment of an entity
     capacitated to extract from a piece of Revolution 9 source code
     the entailed words and punctuation characters."))

;;; -------------------------------------------------------

(defun make-tokenizer (source)
  "Creates and returns a fresh ``Tokenizer'' whose devotation is airted
   towards the piece of Revolution 9 SOURCE code."
  (declare (type string source))
  (the Tokenizer
    (make-instance 'Tokenizer :source source)))

;;; -------------------------------------------------------

(defun get-source-length (tokenizer)
  "Returns the length of the TOKENIZER's source in characters."
  (declare (type Tokenizer tokenizer))
  (the fixnum
    (length
      (tokenizer-source tokenizer))))

;;; -------------------------------------------------------

(defun source-is-exhausted-p (tokenizer)
  "Determines whether the TOKENIZER's source is exhausted, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Tokenizer tokenizer))
  (the boolean
    (get-boolean-value-of
      (>= (tokenizer-position tokenizer)
          (get-source-length  tokenizer)))))

;;; -------------------------------------------------------

(defun get-current-character (tokenizer)
  "Returns the currently selected character in the TOKENIZER's source."
  (declare (type Tokenizer tokenizer))
  (the character
    (char
      (tokenizer-source   tokenizer)
      (tokenizer-position tokenizer))))

;;; -------------------------------------------------------

(defun skip-whitespaces (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   skips a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Tokenizer tokenizer))
  (setf (tokenizer-position tokenizer)
    (or (position-if-not #'whitespace-character-p
          (tokenizer-source tokenizer)
          :start (tokenizer-position tokenizer))
        (get-source-length tokenizer)))
  (values))

;;; -------------------------------------------------------

(defun locate-end-of-word (tokenizer)
  "Returns the position in the TOKENIZER's source immediately succeeding
   the termination of the nearest following word."
  (declare (type Tokenizer tokenizer))
  (the fixnum
    (or (position-if-not #'word-character-p
          (tokenizer-source tokenizer)
          :start (tokenizer-position tokenizer))
        (get-source-length tokenizer))))

;;; -------------------------------------------------------

(defun read-word (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   reads the nearest following word and returns thilk as a fresh
   string."
  (declare (type Tokenizer tokenizer))
  (let ((end-of-word (locate-end-of-word tokenizer)))
    (declare (type fixnum end-of-word))
    (the string
      (prog1
        (subseq
          (tokenizer-source   tokenizer)
          (tokenizer-position tokenizer)
          end-of-word)
        (setf (tokenizer-position tokenizer) end-of-word)))))

;;; -------------------------------------------------------

(defun consume-current-character (tokenizer)
  "Returns the TOKENIZER's currently selected character as a fresh
   string, while concomitantly advances to the next position in its
   source."
  (declare (type Tokenizer tokenizer))
  (the (string 1)
    (prog1
      (string
        (get-current-character tokenizer))
      (incf
        (tokenizer-position tokenizer)))))

;;; -------------------------------------------------------

(defun get-next-token (tokenizer)
  "Returns the next token from the TOKENIZER.
   ---
   Upon its source's exhaustion, the TOKENIZER responds to any request
   with a fresh empty string."
  (declare (type Tokenizer tokenizer))
  (the string
    (symbol-macrolet
        ((current-character
          (the character
            (get-current-character tokenizer))))
      (declare (type character current-character))
      (cond
        ((source-is-exhausted-p tokenizer)
          "")
        ((whitespace-character-p current-character)
          (skip-whitespaces tokenizer)
          (get-next-token   tokenizer))
        ((word-character-p current-character)
          (read-word tokenizer))
        (T
          (consume-current-character tokenizer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Buffer".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Buffer) boolean)
                load-next-token-into-buffer))

;;; -------------------------------------------------------

(defclass Token-Buffer ()
  ((head
    :initform      (cons "" NIL)
    :accessor      token-buffer-head
    :type          (list-of string)
    :documentation "The list of tokens, the first element of which
                    represents the head, intended to obviate a
                    veridically empty list, the same would invalidate
                    the TAIL pointer, and, as a corollary, the efficient
                    appendage principle for the list's rear.")
   (tail
    :initform      NIL
    :accessor      token-buffer-tail
    :type          (or null cons)
    :documentation "A reference to the underlying list's desinent cons
                    cell.")
   (size
    :initform      0
    :accessor      token-buffer-size
    :type          (integer 0 *)
    :documentation "The tally of currently maintained tokens.")
   (tokenizer
    :initarg       :tokenizer
    :initform      (error "Missing tokenizer.")
    :reader        token-buffer-tokenizer
    :type          Tokenizer
    :documentation "The provenance of the extracted tokens."))
  (:documentation
    "The ``Token-Buffer'' class is apportioned the dever of a progress
     token collation's castaldy, loading thilk from an underlying
     ``Tokenizer'' in response to a demand's emergency.
     ---
     The provenance of this requisitum's gendrure being the appendage
     of tokens to the rear rather than the front, the ``Token-Buffer''
     relies on the notion of \"tail-consing\", also realized in the
     consanguinous \"tconc\" diorism, where a separate \"tail pointer\"
     entertains the compernage of the list itself, the latter nevened
     the \"head\"."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((buffer Token-Buffer) &key)
  "Connects the token BUFFER's head and tail pointers, queries and
   stores the first token from the internally managed tokenizer, and
   returns no value."
  (declare (type Token-Buffer buffer))
  (setf (token-buffer-tail buffer)
    (last (token-buffer-head buffer)))
  (load-next-token-into-buffer buffer)
  (values))

;;; -------------------------------------------------------

(defun make-token-buffer (tokenizer)
  "Creates and returns a fresh ``Token-Buffer'' whose warklume for the
   segregation of the Revolution 9 source code is realized in the
   TOKENIZER."
  (declare (type Tokenizer tokenizer))
  (the Token-Buffer
    (make-instance 'Token-Buffer :tokenizer tokenizer)))

;;; -------------------------------------------------------

(defun token-buffer-is-empty-p (buffer)
  "Determines whether the token BUFFER is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (the boolean
    (null
      (rest (token-buffer-head buffer)))))

;;; -------------------------------------------------------

(defun add-token (buffer new-token)
  "Appends the NEW-TOKEN to the token BUFFER's rear and returns no
   value."
  (declare (type Token-Buffer buffer))
  (declare (type string       new-token))
  (let ((new-node (cons new-token NIL)))
    (declare (type (cons string null) new-node))
    (setf (rest (token-buffer-tail buffer)) new-node)
    (setf (token-buffer-tail buffer)
      (rest (token-buffer-tail buffer))))
  (incf (token-buffer-size buffer))
  (values))

;;; -------------------------------------------------------

(defun load-next-token-into-buffer (buffer)
  "Queries the next token from the BUFFER's tokenizer, appends thilk to
   the BUFFER, and returns no value."
  (declare (type Token-Buffer buffer))
  (the boolean
    (let ((next-token
            (get-next-token
              (token-buffer-tokenizer buffer))))
      (declare (type string next-token))
      (unless (zerop (length next-token))
        (add-token buffer next-token)
        T))))

;;; -------------------------------------------------------

(defun ensure-token-buffer-size (buffer required-size)
  "Ascertains the token BUFFER's tally of elements to at least equal the
   REQUIRED-SIZE by repeated queries and insertions of the underlying
   tokenizer's tokens into the same until the account is satisfied,
   finally returning no value."
  (declare (type Token-Buffer  buffer))
  (declare (type (integer 0 *) required-size))
  (loop while
    (and (< (token-buffer-size buffer)
            required-size)
         (load-next-token-into-buffer buffer)))
  (values))

;;; -------------------------------------------------------

(defun remove-token-at-front (buffer)
  "Removes the token at the token BUFFER's front and returns no value."
  (declare (type Token-Buffer buffer))
  (unless (token-buffer-is-empty-p buffer)
    (setf (token-buffer-head buffer)
      (rest
        (token-buffer-head buffer)))
    (decf (token-buffer-size buffer)))
  (ensure-token-buffer-size buffer 1)
  (values))

;;; -------------------------------------------------------

(defun remove-first-tokens (buffer number-of-deletions)
  "Removes the NUMBER-OF-DELETIONS tally of tokens at the token BUFFER's
   front and returns no value."
  (declare (type Token-Buffer  buffer))
  (declare (type (integer 0 *) number-of-deletions))
  (loop repeat number-of-deletions do
    (remove-token-at-front buffer))
  (values))

;;; -------------------------------------------------------

(defun get-buffered-tokens (buffer)
  "Returns a reference to the token BUFFER's currently maintained token
   objects."
  (declare (type Token-Buffer buffer))
  (the (list-of string)
    (rest
      (token-buffer-head buffer))))

;;; -------------------------------------------------------

(defun buffer-tokens-match-phrase-p (buffer probed-phrase)
  "Determines whether the token BUFFER's front tokens match those of the
   PROBED-PHRASE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer     buffer))
  (declare (type (list-of string) probed-phrase))
  (let ((phrase-length (length probed-phrase)))
    (declare (type (integer 0 *) phrase-length))
    (ensure-token-buffer-size buffer phrase-length)
    (the boolean
      (list-starts-with-p
        (get-buffered-tokens buffer)
        probed-phrase))))

;;; -------------------------------------------------------

(defmethod print-object ((buffer Token-Buffer) (stream T))
  (declare (type Token-Buffer buffer))
  (declare (type destination  stream))
  (format stream "(Token-Buffer~{ ~s~})"
    (get-buffered-tokens buffer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun probe-instruction (buffer)
  "Attempts to match the token BUFFER's foremost tokens against one of
   the admissible Revolution 9 phrases, on confirmation returning the
   representative instruction keyword, while deleting the matched
   token objects from the BUFFER; otherwise merely deletes the aefauld
   first token and returns ``NIL''."
  (the (or null instruction)
    (loop
      for (phrase . instruction)
        of-type ((list-of simple-string) . instruction)
        in      +IDENTIFIERS+
      when (buffer-tokens-match-phrase-p buffer phrase) do
        (remove-first-tokens buffer (length phrase))
        (return instruction)
      finally
        (remove-token-at-front buffer)
        (return NIL))))

;;; -------------------------------------------------------

(defun extract-instructions (buffer)
  "Extracts from the token BUFFER the ensconced Revolution 9
   instructions and returns a ``program'' representation thereof."
  (declare (type Token-Buffer buffer))
  (the program
    (make-program
      (loop until (token-buffer-is-empty-p buffer) append
        (let ((next-instruction (probe-instruction buffer)))
          (declare (type (or null instruction) next-instruction))
          (when next-instruction
            (list next-instruction)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-jump-table ()
  "Creates and returns a fresh and empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the jump instructions mediated by START-POINT and END-POINT
   in the JUMP-TABLE and returns no value.
   ---
   Any already extant entry amenable to either the START-POINT or the
   END-POINT will be tacitly superseded by this operation."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun get-destination-jump-point (jump-table point-of-departure)
  "Returns the position of the jump point opposite to the
   POINT-OF-DEPARTURE in the JUMP-TABLE, or, upon its disrespondency,
   signals an error of an unspecified type."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No jump destination associated with the position ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun build-jump-table-for (program)
  "Creates and returns a fresh ``jump-table'' dedicated to the bilateral
   ligation of the Revolution 9 PROGRAM's jump points by adminiculum of
   their zero-based position."
  (declare (type program program))
  (let ((jump-table          (prepare-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-instruction
        of-type instruction
        across  program
      and current-position
        of-type fixnum
        from    0
        by      1
      do
       (case current-instruction
         (:jump-forward
           (push current-position forward-jump-points))
         (:jump-back
           (if forward-jump-points
             (connect-jump-points jump-table
               (pop forward-jump-points)
               current-position)
             (error "Unmatched back jump point.")))
         (otherwise NIL))
      finally
        (when forward-jump-points
          (error "~d unmatched forward jump point~:p registered."
            (length forward-jump-points))))
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor prepare-memory ()))
  "The ``Memory'' class applies itself to the implementation of the
   Revolution 9 program memory, realized as a sparse vector of unsigned
   byte-valued cells amenable to signed integer indices, operated upon
   by a pointer"
  (cells   (make-hash-table :test #'eql)
           :type      (hash-table-of integer octet)
           :read-only T)
  (pointer 0
           :type      integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the byte value stored in the MEMORY's currently selected
   cell."
  (declare (type Memory memory))
  (the octet
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's currently selected cell,
   contingently preceded by a wrapping into the admissible byte range
   of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (program)
  "Executes the Revolution 9 PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (build-jump-table-for program))
        (memory     (prepare-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (symbol-macrolet
        ((program-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length program)))))
         (current-instruction
          (the instruction
            (aref program ip)))
         (current-cell-value
          (the (or octet integer)
            (current-cell-value memory))))
      (declare (type boolean            program-completed-p))
      (declare (type instruction        current-instruction))
      (declare (type (or octet integer) current-cell-value))
      (loop until program-completed-p do
        (case current-instruction
          (:increment
            (incf current-cell-value))
          
          (:decrement
            (decf current-cell-value))
          
          (:move-right
            (move-cell-pointer-right memory))
          
          (:move-left
            (move-cell-pointer-left memory))
          
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
                (get-destination-jump-point jump-table ip))))
          
          (:jump-back
            (unless (zerop current-cell-value)
              (setf ip
                (get-destination-jump-point jump-table ip))))
          
          (otherwise
            (error "Unrecognized instruction: ~s."
              current-instruction)))
        
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Revolution-9 (code)
  "Interprets the piece of Revolution 9 source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (extract-instructions
      (make-token-buffer
        (make-tokenizer code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Revolution-9-converter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-Revolution-9 (brainfuck-code
                                            &optional (destination NIL))
  "Translates the piece of BRAINFUCK-CODE into the tantamount
   Revolution 9 program, writes the result to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh string comprehending
   the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for current-brainfuck-token
          of-type character
          across  brainfuck-code
        do
          (format destination "~&~a"
            (case current-brainfuck-token
              (#\<       "It's alright")
              (#\>       "turn me on, dead man")
              (#\+       "Number 9")
              (#\-       "if you become naked")
              (#\.       "The Beatles")
              (#\,       "Paul is dead")
              (#\[       "Revolution 1")
              (#\]       "Revolution 9")
              (otherwise ""))))
      (with-output-to-string (revolution-9-code)
        (declare (type string-stream revolution-9-code))
        (translate-brainfuck-to-Revolution-9
          brainfuck-code
          revolution-9-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input:
(interpret-Revolution-9
  "
  Paul is dead
  Revolution 1
  The Beatles
  Paul is dead
  Revolution 9
  ")

;;; -------------------------------------------------------

;; Print the message "Hello, World!" to the standard output.
(interpret-Revolution-9
  "Number 9
   Revolution 1
   if you become naked
   if you become naked
   turn me on, dead man
   if you become naked
   Revolution 1
   turn me on, dead man
   turn me on, dead man
   Number 9
   turn me on, dead man
   if you become naked
   if you become naked
   if you become naked
   if you become naked
   if you become naked
   It's alright
   It's alright
   Revolution 9
   It's alright
   if you become naked
   if you become naked
   It's alright
   if you become naked
   if you become naked
   if you become naked
   Revolution 9
   turn me on, dead man
   if you become naked
   The Beatles
   turn me on, dead man
   turn me on, dead man
   turn me on, dead man
   Number 9
   The Beatles
   turn me on, dead man
   turn me on, dead man
   The Beatles
   The Beatles
   Number 9
   Number 9
   Number 9
   Revolution 1
   The Beatles
   turn me on, dead man
   Revolution 9
   It's alright
   It's alright
   It's alright
   It's alright
   The Beatles
   Number 9
   Number 9
   Number 9
   The Beatles
   if you become naked
   if you become naked
   if you become naked
   if you become naked
   if you become naked
   if you become naked
   The Beatles
   It's alright
   It's alright
   if you become naked
   The Beatles
   turn me on, dead man
   turn me on, dead man
   turn me on, dead man
   turn me on, dead man
   Number 9
   The Beatles")

;;; -------------------------------------------------------

;; Translate the "Hello, World!" program from brainfuck to Revolution 9
;; and execute thilk.
(interpret-Revolution-9
  (translate-brainfuck-to-Revolution-9
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]
     <<<<.+++.------.<<-.>>>>+."))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Revolution-9
  "
  Paul is dead
  The Beatles
  Revolution 1
  if you become naked
  if you become naked
  turn me on, dead man
  Number 9
  Revolution 1
  turn me on, dead man
  turn me on, dead man
  Revolution 9
  It's alright
  Revolution 1
  The Beatles
  Revolution 9
  It's alright
  It's alright
  Revolution 9
  ")
