;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Chicken you too beautiful", invented by the Esolang user
;; "None1" and presented on August 14th, 2023, the bourne of its
;; existency a reformulation of Urban Mueller's "brainfuck" language
;; in a syntactical guise which sings a herrying to a chicken and its
;; gnarity in many occupations.
;; 
;; 
;; Concept
;; =======
;; The "Chicken you too beautiful" programming language constitutes a
;; mimicry of brainfuck's haecceity manuducted approximately into a mete
;; of patration; its aefauld aspect of deviation's establishment
;; manifested in the naited operation tokens' designment, thilk, in lieu
;; of the abstractness' purview that extended across the entheus'
;; one-symbol identifiers' syntaxis, inlines towards a diction to whom
;; is ligated an alate animals' pulchritude's encomium.
;; 
;; == THE SYNTAXIS: A CHICKEN AND ITS TALENTS ==
;; "Chicken you too beautiful" employs a lexicon whose terms are desumed
;; from a provenance which applies itself to avaunt a chicken and its
;; capabilities' gamut, each operative token a word wisting as its
;; merist from the surrounding context of whitespaces' agency.
;; 
;; Unrecognized tokens enjoy the adhibition of tolerance and neglect
;; in a paregal mete.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE CATENA OF UNSIGNED BYTES ==
;; The department whose concern wones in the data castaldy relies upon
;; a bilaterally infinite dispansion of unsigned byte-valued cells, the
;; ordonnance referring to this notion that of a tape.
;; 
;; Each cell's capacity metes exactly eight bits' coefficiency, whence
;; ensues the consectary that assigns to a single salvatory the closed
;; integral interval of [0, 255], the minimum of zero (0) acting as the
;; inchoate state's accompt. Upon one of its bournes' transgression,
;; the state wraps around towards the athwart march, producing for an
;; incrementation beyond the upper extremum of 255 a resorting to the
;; minimum of zero (0), and, obverse in this airt, for the lower
;; threshold's violation, the restart at 255.
;; 
;; A dedicated cursor, the "cell pointer", is vouchsafed its woning on
;; this tape, empight at an incipial cell as its first location,
;; incumbent in the wike of the currently active unit's designation, to
;; whom is imparted the sole cynosure in terms of perquisitions and
;; modulations. Its motile nature homologates the pointer's stillatim
;; perambulation along both axes in order to modify this selection.
;; 
;; 
;; Instructions
;; ============
;; Its status as a paregal of brainfuck in any aspect, except for its
;; syntactical guise, invests "Chicken you too beautiful" with an
;; equipollence also resonanting in its instruction set, the same
;; enumerates the acquainted octuple warklumes.
;; 
;; == OVERVIEW ==
;; A requisite mete of nortelry, designed with syntomy in its telos,
;; concerning the language's operative competences shall be the alow
;; apercu's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   chicken    | Increments the current cell value by one (1). Upon
;;              | its upper bourne's of 255 transgression, the state
;;              | wraps around along the lower extremum of zero (0).
;;   ..................................................................
;;   you        | Decrements the current cell value by one (1). Upon
;;              | its lower bourne's of zero (0) transgression, the
;;              | state wraps around along the upper extremum of 255.
;;   ..................................................................
;;   too        | Translates the cell pointer one step in the sinistral
;;              | airt.
;;   ..................................................................
;;   beautiful  | Translates the cell pointer one step in the dextral
;;              | airt.
;;   ..................................................................
;;   sing       | Queries the standard input conduit for a character
;;              | and stores its ASCII code in the current cell.
;;   ..................................................................
;;   dance      | Prints the character whose ASCII code corresponds to
;;              | the current cell value to the standard output
;;              | conduit.
;;   ..................................................................
;;   rap        | If the current cell value equals zero (0), moves the
;;              | instruction pointer (IP) forward to the position
;;              | immediately succeeding the matching "basketball"
;;              | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   basketball | If the current cell value does not equal zero (0),
;;              | moves the instruction pointer (IP) back to the
;;              | position immediately succeeding the matching "rap"
;;              | instruction; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == CHICKEN YOU TOO BEAUTIFUL AND BRAINFUCK ==
;; Ensuing from a perfect equipollence's governance, an immediate and
;; unambiguous equiparation's adhibition may be adduced betwixt the
;; language "Chicken you too beautiful" and its brainfuck entheus:
;; 
;;   -------------------------------------
;;   Chicken you too beautiful | brainfuck
;;   --------------------------+----------
;;   chicken                   | +
;;   .....................................
;;   you                       | -
;;   .....................................
;;   too                       | <
;;   .....................................
;;   beautiful                 | >
;;   .....................................
;;   sing                      | ,
;;   .....................................
;;   dance                     | .
;;   .....................................
;;   rap                       | [
;;   .....................................
;;   basketball                | ]
;;   -------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, its most peisant telos, "Chicken
;; you too beautiful" programs' execution, proceeding by a prevenience
;; which transcripts each operative token into a connable instruction
;; representation.
;; 
;; A parergon to its paravaunt wike, the "Chicken you too beautiful"
;; programming language's interpretation, this project at hand intrines
;; aspects of an advenient hyle, airted at the respective gnarity's
;; integration:
;; 
;;   (1) Closure-based lexer:
;;       The firmament of the lexer's conformation ensues from a
;;       closure-based approach, binding the requisite state variables
;;       in such a manner as to confine their access to their dependent
;;       functions.
;;   
;;   (2) Accommodated hash table:
;;       Among the more sophisticated and demanding investments
;;       experiences a bespoke hash table solution its nemning, the same
;;       accompts in its basis for a static vector of "buckets" which
;;       store the key-value entries.
;;   
;;   (3) Expressive identifiers:
;;       The operations' agnomination proceeds from a peisant
;;       concentration on their communicative potential, accruing in
;;       this aspiration characters in proportion to their objective.
;; 
;; == (1) THE LEXER NAITS A CLOSURE-BASED SOLUTION ==
;; The lexical analyzer, lexer, in its dever's reification registers an
;; edification upon the functional notion of closures, rather than an
;; object-oriented approach.
;; 
;; In this telos pursuit, the pertinent state variables, exhausted
;; already in the "Chicken you too beautiful" source code and the
;; current position inwith thilk, lay their amplection around two
;; functions, these in an agency as the lexer's publicly available
;; interface, by which ensconcement the twissel's bindings are vouched
;; their visibility and continued existency at any instant of the
;; operations' invocation, while concomitantly retaining a tappished
;; status in their concealment from adscititious manipulations.
;; 
;; The jumelle of operative warklumes reliant in such a grade upon the
;; derne data comprises the recipient of the new "Chicken you too
;; beautiful" source code to evaluate, resetting the lexer's position
;; cursor as an epiphenomenal reaction; and the paravaunt instrument
;; for the next token's obtention from the thus installed provenance.
;; 
;; == (2) A CUSTOM HASH TABLE IS IMPLEMENTED ==
;; Several tasks partaking of this implementation wist of an associative
;; array, also nevened a "map" or a "dictionary", as a requisitum for
;; their efficient solution. Common Lisp's circumference admits manifold
;; facilities concerning this data structure, scilicet:
;; 
;;   (a) Hash table:
;;       Enunciated as "argumentum ad verecundiam", by authority of the
;;       Common Lisp standard, this species of associative structure
;;       does not mandate any intrinsic impositions besides the warrant
;;       of a key's alliance with at most one value. By this fact's
;;       virtue, the terminology involving a "hash" is eloigned from a
;;       veridicous euonym's stead; yet expected to arine on the notions
;;       of some hashed solution for reasons of sane feasibility.
;;       Natheless, a certain degree of performance in its conception
;;       ought to apply to the hash table, whose object-oriented
;;       fundament adheres to the tenet of ensconcing the majority among
;;       its configurations, such as its key selection predicate.
;;   
;;   (b) Association list:
;;       A popular tendence's recipient, the association list imposes
;;       a conformation based upon a simple linked list whose elements
;;       either appear in the form of conses, maintaining in the first
;;       moeity the key and in the second the affiliated value, or
;;       ``NIL'' instances.
;;       The presentation compliant with a plain list designment per
;;       force produces an assignment of the configurations, as the
;;       key comparison predicate, to the respective operations
;;       themselves.
;;   
;;   (c) Property list:
;;       Anent its designment invested with the least intricacy, the
;;       property list's eath conception limns a tantamount to its
;;       parvipotent status. The information, siclike to an association
;;       list, assumes a simple linked list's guise, with the entries,
;;       however, deprived of any further division other than each key's
;;       succession by its value.
;;       To this species of associative collection are vouchsafed very
;;       few configurations and contingencies; in particular, keys are
;;       expected to subsume into the ``symbol'' or ``keyword'' types
;;       only in order to guarantee the property list's successful
;;       application.
;; 
;; Maugre the versatility's incarnation as berined and elucidated aboon,
;; the fanding of the didascalic engagement's alliciency cheviced in a
;; bespoke hash table implementation's endeavor.
;; 
;; The apprehended solution entertains the principle of a vector of
;; "buckets", each such the actual salvatory to zero or more entries.
;; A key's numeric hash code generation furnishes the parasceve to its
;; modulation in such a manner as to mold into a zero-based index for
;; the underlying fixed-size array. Inwith this, a bucket, realized as
;; a singly linked list, stores those entries whose keys produce this
;; exact index. If desiderating an entry's obtention via its indicator,
;; the respective bucket's inquisition must be followed by a linear
;; search until either the matching entry with the matching key has been
;; discovered, or its absence can be avered.
;; 
;; == (3) IDENTIFIERS OSTEND SUMPTUOUS NORNINGS ==
;; A further prosthesis whose presence serves as a dioristic dation in
;; this implementation emerges in the form of the operation identifiers,
;; to whom a nomenclature's endowment with a nimiety's high mete is
;; apportioned; the causatum withal shall be engendered in an eeked
;; expressiveness thilk remains elusive to siccan syntactical
;; parceries whose dictionaries exhibit a proclivity towards stringent
;; syntomy.
;; 
;; A forbisen's adduction to the treatise produced ubi supra will be
;; a bespoke hash table implementation's bucket operation, to whom a
;; contingent entry's obtention by a desiderated key shall be assigned
;; as an onus:
;; 
;;   search-the-bucket-for-an-entry-with-the-key
;; 
;; A tongue whose lealty aligns with more widely dissipated consuetudes
;; would lief resort to a diction cognate with the alternative:
;; 
;;   get-bucket-entry-for-key
;; 
;; Please heed the installment of connectives and articles, for
;; instance, "an" and "the", in this project.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-09-09
;; 
;; Sources:
;;   [esolang2024Chickenyoutoobeautiful]
;;   The Esolang contributors, "Chicken you too beautiful",
;;     September 3rd, 2024
;;   URL: "https://esolangs.org/wiki/Chicken_you_too_beautiful"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*) (size '*))
  "The ``list-of'' type defines a list whose conformation admits zero
   or more elements of the ELEMENT-TYPE, thilk represented in its
   default by the generic sentinel ``*'', and whose cardinality, if
   specified, designates an equinumerant accompt to the SIZE, the same
   in its original state tolerates any quantity via the ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (eq size '*)
                (=  size (length (the list candidate))))
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   accompts for zero or more entries, everichon among this membership a
   twissel composite of a key compliant with the KEY-TYPE and an allied
   value whose species is imposed by the VALUE-TYPE, for both
   configurations holds the generic sentinel ``*'' as the default."
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

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized Chicken you too
   beautiful operations in a manner invested with abstract potence."
  '(member
    :increment
    :decrement
    :move-left
    :move-right
    :input
    :output
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Chicken you too beautiful
   program as a one-dimensional simple array of ``instruction''
   objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype entry ()
  "The ``entry'' type defines an key-value entry into a hash table as a
   cons, the sinistral compartment of which accommodates a woning to
   the key, and affiliates with the value in the dextral moeity."
  '(cons * *))

;;; -------------------------------------------------------

(deftype entry-list ()
  "The ``entry-list'' type defines a linked list of hash table entries,
   each such represented by an ``entry'' key-value cons."
  '(list-of entry))

;;; -------------------------------------------------------

(deftype entry-bucket-vector ()
  "The ``entry-bucket-vector'' type defines one-dimensional simple array
   of buckets intended for their deployment in a hash table, each such
   in a ``Entry-Bucket'' instance's guise."
  '(simple-array Entry-Bucket (*)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, and thus establishing a commorant of the closed
   integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-as-a-boolean-value (object)
  "Interprets the OBJECT in its facette as a \"generalized boolean\"
   designator and produces a veridicous Boolean paregal thereof,
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of equality predicate.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric both-objects-are-equal-p (first-object second-object)
  (:documentation
    "Determines whether the FIRST-OBJECT and the SECOND-OBJECT partake
     commit a relationship of equality, returning on confirmation a
     ``boolean'' value of ``T'', otherwise ``NIL''.")
  
  (:method ((first-object integer) (second-object integer))
    (declare (type integer first-object))
    (declare (type integer second-object))
    (the boolean
      (interpret-as-a-boolean-value
        (= first-object second-object))))
  
  (:method ((first-object string) (second-object string))
    (declare (type string first-object))
    (declare (type string second-object))
    (the boolean
      (interpret-as-a-boolean-value
        (string= first-object second-object)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table bucket.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Entry-Bucket
  (:constructor prepare-an-empty-entry-bucket ()))
  "The ``Entry-Bucket'' class establishes a bucket partaking of a
   hash table, represented by the ``HTable'' class, serving as a
   linked-list-based salvatory for zero or more entries whose keys
   produce the same numeric hash code.
   ---
   A bucket employs the generic function ``both-objects-are-equal-p''
   for its probed keys' equiparation."
  (entries NIL :type entry-list :read-only NIL))

;;; -------------------------------------------------------

(defun search-the-bucket-for-an-entry-with-the-key (bucket key)
  "Returns the entry affiliated with the KEY in the BUCKET; or, upon
   its absence, responds with the ``NIL'' sentinel."
  (declare (type Entry-Bucket bucket))
  (declare (type T            key))
  (the (or null entry)
    (assoc key
      (entry-bucket-entries bucket)
      :test #'both-objects-are-equal-p)))

;;; -------------------------------------------------------

(defun insert-an-entry-into-the-bucket (bucket new-key new-value)
  "Integrates the entry compact of the NEW-KEY and the NEW-VALUE into
   the BUCKET, either by a fresh entry's prepending to the underlying
   linked list, or, upon the NEW-KEY's presence inwith the same, by the
   modification of the extant entry's value compartment to hold the
   NEW-VALUE; in any case returning no value."
  (declare (type Entry-Bucket bucket))
  (declare (type T            new-key))
  (declare (type T            new-value))
  (let ((the-entry-with-the-key
          (search-the-bucket-for-an-entry-with-the-key bucket new-key)))
    (declare (type (or null entry) the-entry-with-the-key))
    (if the-entry-with-the-key
      (setf (cdr the-entry-with-the-key) new-value)
      (push
        (cons new-key new-value)
        (entry-bucket-entries bucket))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass HTable ()
  ((capacity
    :initarg       :capacity
    :initform      (error "No capacity specified.")
    :type          fixnum
    :documentation "The underlying BUCKETS vector's length.")
   (buckets
    :type          entry-bucket-vector
    :documentation "A vector of buckets, each comprehending zero or more
                    key-value entries."))
  (:documentation
    "The ``HTable'' class applies itself to the realization of a hash
     table, its firmament a fixed-size vector of \"buckets\", everichon
     among these a salvatory to zero or more key-value pairs.
     ---
     This hash table implementation employs the generic function
     ``both-objects-are-equal-p'' for its probed keys' equiparation."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((table HTable) &key)
  "Accommodates a vector of sufficiency in mickleness to satisfy the
   hash TABLE's capacity, all of its inchoate positions being filled
   with empty buckets, and returns no value."
  (declare (type HTable table))
  (with-slots (capacity buckets) table
    (declare (type fixnum              capacity))
    (declare (type entry-bucket-vector buckets))
    (setf buckets
      (map-into
        (make-array capacity
          :element-type    'entry-bucket
          :initial-element (prepare-an-empty-entry-bucket)
          :adjustable      NIL
          :fill-pointer    NIL)
        #'prepare-an-empty-entry-bucket)))
  (values))

;;; -------------------------------------------------------

(defun prepare-an-empty-htable (capacity)
  "Creates and returns an initially empty hash TABLE whose underlying
   bucket vector enumerates the CAPACITY of elements in size."
  (declare (type fixnum capacity))
  (the HTable
    (make-instance 'HTable :capacity capacity)))

;;; -------------------------------------------------------

(defun compute-the-bucket-index-for-the-key (table key)
  "Supputates and returns the zero-based index of the bucket responsible
   for the KEY's admission or recovery in the hash TABLE."
  (declare (type HTable table))
  (declare (type T      key))
  (the fixnum
    (mod
      (sxhash key)
      (slot-value table 'capacity))))

;;; -------------------------------------------------------

(defun find-the-bucket-containing-the-key (table key)
  "Returns the bucket responsible in the hash TABLE for the KEY's
   accommodation."
  (declare (type HTable table))
  (declare (type T      key))
  (the Entry-Bucket
    (with-slots (buckets) table
      (declare (type entry-bucket-vector buckets))
      (aref buckets
        (compute-the-bucket-index-for-the-key table key)))))

;;; -------------------------------------------------------

(defun insert-an-entry-into-the-htable (table new-key new-value)
  "Inserts an entry into the hash TABLE which conjoins the NEW-KEY with
   the NEW-VALUE and returns no value.
   ---
   Upon an entry's existence in the TABLE amenable to the NEW-KEY, the
   respective association will be tacitly superseded by one collocating
   the NEW-VALUE with the NEW-KEY."
  (declare (type HTable table))
  (declare (type T      new-key))
  (declare (type T      new-value))
  (insert-an-entry-into-the-bucket
    (find-the-bucket-containing-the-key table new-key)
    new-key
    new-value)
  (values))

;;; -------------------------------------------------------

(defun look-up-the-entry-for-the-key (table key)
  "Returns the entry associated in the hash TABLE with the KEY; or, upon
   its disrespondency, responds with the ``NIL'' sentinel."
  (declare (type HTable table))
  (declare (type T      key))
  (let ((the-bucket-for-the-key
          (find-the-bucket-containing-the-key table key)))
    (declare (type (or null Entry-Bucket) the-bucket-for-the-key))
    (the (or null entry)
      (and
        the-bucket-for-the-key
        (search-the-bucket-for-an-entry-with-the-key
          the-bucket-for-the-key
          key)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of character 6) +WHITESPACE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTERS+
  (mapcar #'code-char '(9 10 11 12 13 32))
  "Defines an unordered list comprehending the recognized whitespace
   characters.
   ---
   In a diction entalented with concrete expressive fortitude and
   compendiousness, the following sextuple membership is imputed among
   the whitespace species' terminology:
     --------------------------------------------------------
     ASCII code | Whitespace character name | Escape sequence
     -----------+---------------------------+----------------
     9          | Tab                       | \\t
     ........................................................
     10         | Newline, Line Feed        | \\n
     ........................................................
     11         | Vertical Tabulation       | \\v
     ........................................................
     12         | Form Feed                 | \\f
     ........................................................
     13         | Carriage Return           | \\r
     ........................................................
     32         | Space                     | (none)
     --------------------------------------------------------")

;;; -------------------------------------------------------

(defun the-character-is-a-whitespace (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   inwith whose diorism are amplected the space, horizontal tab, and
   newline entities, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   For this species' membership's definition, please consult the global
   constant +WHITESPACE-CHARACTERS+."
  (declare (type character candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (member candidate +WHITESPACE-CHARACTERS+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-start-of-the-next-word (source start)
  "Proceeding from the START position into the SOURCE, returns the index
   of the next following word's first character; or, upon its
   disrespondency, answers with the SOURCE length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if-not #'the-character-is-a-whitespace source
          :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-the-end-of-the-nearest-word (source start)
  "Proceeding from the START position into the SOURCE, returns the index
   immediately succeeding the next following word's desinent character;
   or, upon its disrespondency, answers with the SOURCE length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if #'the-character-is-a-whitespace source
          :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-the-bournes-of-the-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the next
   word and returns two values:
     (1) The index into the SOURCE of the detected word's first
         character; or, upon its disrespondency, the SOURCE length.
     (2) The index into the SOURCE immediately succeeding the detected
         word's desinent character; or, upon its disrespondency, the
         SOURCE length.
   ---
   Please heed that, in the special circumstance of no subsequent word's
   presence in the SOURCE commencing at the START position, this
   operation will yield a conflation of the word start and end indices
   with the SOURCE length, signifying beyond any ambiguity's puissance
   the SOURCE's exhaustion."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let ((word-start (locate-the-start-of-the-next-word source start)))
    (declare (type fixnum word-start))
    (the (values fixnum fixnum)
      (values
        word-start
        (locate-the-end-of-the-nearest-word source word-start)))))

;;; -------------------------------------------------------

(defun extract-the-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the next
   word and returns two values:
     (1) A fresh simple string comprehending the detected word's
         characters. Upon siccan word's absence, this object will
         constitute an empty string.
     (2) The index into the SOURCE immediately succeeding the detected
         word's desinent character; or, upon its disrespondency, the
         SOURCE length.
   ---
   Please heed that, in the special circumstance of no subsequent word's
   presence in the SOURCE commencing at the START position, this
   operation will yield a fresh empty string, signifying beyond any
   ambiguity's puissance the SOURCE's exhaustion."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (multiple-value-bind (word-start word-end)
      (locate-the-bournes-of-the-next-word source start)
    (declare (type fixnum word-start))
    (declare (type fixnum word-end))
    (the (values simple-string fixnum)
      (values
        (subseq source word-start word-end)
        word-end))))

;;; -------------------------------------------------------

(defun the-string-is-empty-p (candidate)
  "Determines whether the CANDIDATE represents an empty string,
   enumerating exactly zero constituents, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (zerop
        (length candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer (lexer).                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((source           "")
      (current-position 0))
  (declare (type simple-string source))
  (declare (type fixnum        current-position))
  
  (defun prepare-the-lexer-with-a-new-source (new-source)
    "Configures the lexer to operate on the NEW-SOURCE, resets its
     internal state, and returns no value."
    (declare (type string new-source))
    (psetf
      source           (coerce new-source 'simple-string)
      current-position 0)
    (values))
  
  (defun request-the-next-token ()
    "Obtains the next token from the lexer.
     ---
     Upon its source's exhaustion, the lexer responds to any request
     with a fresh empty string."
    (multiple-value-bind (next-token new-position)
        (extract-the-next-word source current-position)
      (declare (type simple-string next-token))
      (declare (type fixnum        new-position))
      (setf current-position new-position)
      (the simple-string next-token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type HTable +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (prepare-an-empty-htable 8)
  "Allies the recognized Chicken you too beautiful identifier name with
   representative instruction objects.")

;;; -------------------------------------------------------

(flet ((register-an-identifier (name instruction)
        "Associates the Chicken you too beautiful identifier NAME with
         the INSTRUCTION in the ``+IDENTIFIERS+'' table and returns no
         value."
        (declare (type simple-string name))
        (declare (type instruction   instruction))
        (insert-an-entry-into-the-htable +IDENTIFIERS+ name instruction)
        (values)))
  (register-an-identifier "chicken"    :increment)
  (register-an-identifier "you"        :decrement)
  (register-an-identifier "too"        :move-left)
  (register-an-identifier "beautiful"  :move-right)
  (register-an-identifier "sing"       :input)
  (register-an-identifier "dance"      :output)
  (register-an-identifier "rap"        :jump-forward)
  (register-an-identifier "basketball" :jump-back)
  (values))

;;; -------------------------------------------------------

(defun obtain-the-instruction-for-the-identifier (candidate)
  "Determines whether the CANDIDATE represents a Chicken you too
   beautiful identifier, returning on confirmation the affiliated
   instruction; otherwise responds with the ``NIL'' sentinel."
  (declare (type simple-string candidate))
  (the (or null instruction)
    (cdr
      (look-up-the-entry-for-the-key +IDENTIFIERS+ candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-string-as-a-program (code)
  "Parses the piece of Chicken you too beautiful source CODE and returns
   a connable ``program'' representation of its ensconced instructions."
  (declare (type string code))
  (prepare-the-lexer-with-a-new-source code)
  (the program
    (coerce
      (loop
        for the-current-token
          of-type simple-string
          =       (request-the-next-token)
        until
          (the-string-is-empty-p the-current-token)
        append
          (let ((the-possible-instruction
                  (obtain-the-instruction-for-the-identifier
                    the-current-token)))
            (declare (type (or null instruction)
                           the-possible-instruction))
            (and the-possible-instruction
                 (list the-possible-instruction))))
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initarg       :connections
    :type          HTable
    :documentation "A mapping betwixt jumelles of matching jump
                    instructions in a Chicken you too beautiful
                    program, mediated by adminiculum of their zero-based
                    positions into the source code."))
  (:documentation
    "The ``Jump-Table'' class serves in the castaldy of a Chicken you
     too beautiful program's jump points, mediated by adminiculum of
     their zero-based indices into the instruction vector."))

;;; -------------------------------------------------------

(defun prepare-an-empty-jump-table (capacity)
  "Creates and returns a fresh and initially empty ``Jump-Table'' whose
   underlying hash table employs the specified CAPACITY."
  (declare (type fixnum capacity))
  (the Jump-Table
    (make-instance 'Jump-Table :connections
      (prepare-an-empty-htable capacity))))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Establishes a bidirectional vinculum betwixt the jump START-POINT and
   END-POINT, stores this affiliation in the jump TABLE, and returns no
   value."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-slots (connections) table
    (declare (type HTable connections))
    (insert-an-entry-into-the-htable connections start-point end-point)
    (insert-an-entry-into-the-htable connections end-point start-point))
  (values))

;;; -------------------------------------------------------

(defun count-the-jump-points-in (the-program)
  "Returns the tally of forward or back jump instructions partaking in
   THE-PROGRAM."
  (declare (type program the-program))
  (the fixnum
    (count-if
      #'(lambda (the-probed-instruction)
          (declare (type instruction the-probed-instruction))
          (the boolean
            (interpret-as-a-boolean-value
              (or (eq the-probed-instruction :jump-forward)
                  (eq the-probed-instruction :jump-back)))))
      the-program)))

;;; -------------------------------------------------------

(defun construct-a-jump-table-for (the-program)
  "Creates and returns a fresh ``Jump-Table'' dedicated to the
   bidirectional ligation of the THE-PROGRAM's jump points."
  (declare (type program the-program))
  (let ((the-new-jump-table
          (prepare-an-empty-jump-table
            (count-the-jump-points-in the-program)))
        (a-stack-of-start-points
          NIL))
    (declare (type Jump-Table       the-new-jump-table))
    (declare (type (list-of fixnum) a-stack-of-start-points))
    (loop
      for current-instruction of-type instruction across the-program
      and current-position    of-type fixnum      from   0 by 1
      do
        (case current-instruction
          (:jump-forward
            (push current-position a-stack-of-start-points))
          (:jump-back
            (if a-stack-of-start-points
              (connect-the-jump-points the-new-jump-table
                (pop a-stack-of-start-points)
                current-position)
              (error "Unmatched back jump point detected.")))
          (otherwise
            NIL))
      finally
        (when a-stack-of-start-points
          (error "Unmatched forward jump point~p detected."
            (length a-stack-of-start-points))))
    (the Jump-Table the-new-jump-table)))

;;; -------------------------------------------------------

(defun locate-the-matching-jump-destination (table point-of-departure)
  "Returns the destination position affiliated with the jump point
   POINT-OF-DEPARTURE in the jump TABLE; or, upon its disrespondency,
   signals an error of an unspecified type."
  (declare (type Jump-Table table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (with-slots (connections) table
      (declare (type HTable connections))
      (or (cdr (look-up-the-entry-for-the-key
                 connections
                 point-of-departure))
          (error "No destination point exists for the jump point at ~
                  the position ~d."
            point-of-departure)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape cell.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape-Cell ()
  ((value
    :initform      0
    :accessor      tape-cell-value
    :type          octet
    :documentation "The unsigned byte value stored in this tape cell.")
   (previous
    :initarg       :previous
    :initform      NIL
    :accessor      tape-cell-previous
    :type          (or null Tape-Cell)
    :documentation "The contingent predecessor cell in the tape.")
   (next
    :initarg       :next
    :initform      NIL
    :accessor      tape-cell-next
    :type          (or null Tape-Cell)
    :documentation "The contingent successor cell in the tape."))
  (:documentation
    "The ``Tape-Cell'' class serves in the encapsulation of a tape
     cell's notion, thilk attains in its diorism an unsigned byte-valued
     state, as well as a jumelle of pointers, one dedicated to a
     vinculum's establishment with a potential predecessor cell along
     the tape, the other, airted at the obverse laterality, pertaining
     to a successor's contingency.
     ---
     Maintaining these aspects, the a tape cell limns a doubly linked
     node's simulacrum, intended for its participation in the ``Tape''
     class with its lealty to the doubly linked list tenets."))

;;; -------------------------------------------------------

(defun prepare-a-new-tape-cell (&optional (previous NIL) (next NIL))
  "Creates and returns a fresh and initially zero-valued ``Tape-Cell''
   whose vicinage is defined by the jumelle of an optional PREVIOUS
   cell and its NEXT counterpart."
  (declare (type (or null Tape-Cell) previous))
  (declare (type (or null Tape-Cell) next))
  (the Tape-Cell
    (make-instance 'Tape-Cell :previous previous :next next)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Tape Tape-Cell Tape-Cell) Tape-Cell)
                insert-a-new-tape-cell-betwixt))

;;; -------------------------------------------------------

(defclass Tape ()
  ((header
    :initform      (prepare-a-new-tape-cell NIL NIL)
    :type          Tape-Cell
    :documentation "The header sentinel, a special tape cell thilk,
                    deprived of useful data storage, applies itself to
                    the tape's sinistral bourne marker, always retaining
                    its position on the leftmost side of the explored
                    cell catena.")
   (trailer
    :initform      (prepare-a-new-tape-cell NIL NIL)
    :type          Tape-Cell
    :documentation "The trailer sentinel, a special tape cell thilk,
                    deprived of useful data storage, applies itself to
                    the tape's dextral bourne marker, always retaining
                    its position on the rightmost side of the explored
                    cell catena.")
   (pointer
    :type          Tape-Cell
    :documentation "Stores a reference to the currently selected tape
                    cell."))
  (:documentation
    "The ``Tape'' class serves in the program memory's furnishment,
     thilk adheres to the diorism of a bilaterally infinite dispansion
     of unsigned byte-valued cells, the entire ambit a mobile cell
     pointer's bailiwick, to whom the dever of the currently active
     unit's selection accompts for the sole parcery."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Connects the TAPE's header and trailer cells in a bilateral manner,
   empights the cell pointer on the former, and returns no value."
  (declare (type Tape tape))
  (with-slots (header trailer pointer) tape
    (declare (type Tape-Cell header))
    (declare (type Tape-Cell trailer))
    (declare (type Tape-Cell pointer))
    (psetf
      (tape-cell-next     header)  trailer
      (tape-cell-previous trailer) header)
    (setf pointer
      (insert-a-new-tape-cell-betwixt tape header trailer)))
  (values))

;;; -------------------------------------------------------

(defun prepare-a-new-tape ()
  "Creates and returns a fresh ``Tape'' whose cells all retain their
   pristine configuration."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun insert-a-new-tape-cell-betwixt (tape predecessor successor)
  "Creates a fresh tape cell in the TAPE betwixt the PREDECESSOR and the
   SUCCESSOR, inserts thilk, and returns the thus established new cell."
  (declare (type Tape      tape))
  (declare (ignore         tape))
  (declare (type Tape-Cell predecessor))
  (declare (type Tape-Cell successor))
  (the Tape-Cell
    (let ((the-new-cell
            (prepare-a-new-tape-cell predecessor successor)))
      (declare (type Tape-Cell the-new-cell))
      (psetf
        (tape-cell-next     predecessor) the-new-cell
        (tape-cell-previous successor)   the-new-cell)
      the-new-cell)))

;;; -------------------------------------------------------

(defun the-cell-pointer-occupies-the-leftmost-position (tape)
  "Determines whether the TAPE's cell pointer is located in the leftmost
   cell of the discovered dispansion, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (interpret-as-a-boolean-value
      (with-slots (pointer header) tape
        (declare (type Tape-Cell pointer))
        (declare (type Tape-Cell header))
        (eq (tape-cell-previous pointer)
            header)))))

;;; -------------------------------------------------------

(defun ensure-that-a-tape-cell-to-the-left-exists (tape)
  "Ensures the existency of a cell on the TAPE cell pointer's sinistral
   laterality, upon necessity creating and inserting such, and returns
   no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Tape-Cell pointer))
    (when (the-cell-pointer-occupies-the-leftmost-position tape)
      (insert-a-new-tape-cell-betwixt tape
        (tape-cell-previous pointer)
        pointer)))
  (values))

;;; -------------------------------------------------------

(defun the-cell-pointer-occupies-the-rightmost-position (tape)
  "Determines whether the TAPE's cell pointer is located in the
   rightmost cell of the discovered dispansion, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (interpret-as-a-boolean-value
      (with-slots (pointer trailer) tape
        (declare (type Tape-Cell pointer))
        (declare (type Tape-Cell trailer))
        (eq (tape-cell-next pointer)
            trailer)))))

;;; -------------------------------------------------------

(defun ensure-that-a-tape-cell-to-the-right-exists (tape)
  "Ensures the existency of a cell on the TAPE cell pointer's dextral
   laterality, upon necessity creating and inserting such, and returns
   no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Tape-Cell pointer))
    (when (the-cell-pointer-occupies-the-rightmost-position tape)
      (insert-a-new-tape-cell-betwixt tape pointer
        (tape-cell-next pointer))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step into the sinistral airt
   and returns no value."
  (declare (type Tape tape))
  (ensure-that-a-tape-cell-to-the-left-exists tape)
  (with-slots (pointer) tape
    (declare (type Tape-Cell pointer))
    (setf pointer
      (tape-cell-previous pointer)))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step into the dextral airt and
   returns no value."
  (declare (type Tape tape))
  (ensure-that-a-tape-cell-to-the-right-exists tape)
  (with-slots (pointer) tape
    (declare (type Tape-Cell pointer))
    (setf pointer
      (tape-cell-next pointer)))
  (values))

;;; -------------------------------------------------------

(defun the-current-cell-value (tape)
  "Returns the unsigned byte value defining the TAPE's currently
   selected cell's state."
  (declare (type Tape tape))
  (the octet
    (tape-cell-value
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun (setf the-current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell, preceded
   by a contingent wrapping of the same into the admissible unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-slots (pointer) tape
    (declare (type Tape-Cell pointer))
    (setf (tape-cell-value pointer)
      (mod new-value 256)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          program
    :documentation "The sequence of instructions to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position,
                    referencing the active instruction's zero-based
                    index into the PROGRAM's instruction sequence.")
   (jump-points
    :type          Jump-Table
    :documentation "Ligates the jump points inside of the PROGRAM.")
   (tape
    :initform      (prepare-a-new-tape)
    :type          Tape
    :documentation "The program memory as a bilaterally infinite catena
                    of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class naits its faculties in the pursuit of the
     telos in accompassing actual efficacy to a parsed Chicken you too
     beautiful program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Constructs for the Chicken you too beautiful program consigned to the
   INTERPRETER's castaldy a jump table which connects its jump points,
   stores thilk in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-points)
    (construct-a-jump-table-for
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun prepare-an-interpreter-for (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Chicken
   you too beautiful PROGRAM's evaluation."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun query-the-current-instruction (interpreter)
  "Returns the currently selected instruction in the INTERPRETER's
   maintained program."
  (declare (type Interpreter interpreter))
  (the instruction
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (aref program ip))))

;;; -------------------------------------------------------

(defun advance-to-the-next-instruction (interpreter)
  "Advances the INTERPRETER's internal instruction pointer (IP) to the
   subsequent instruction in its underlying program and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (when (array-in-bounds-p program ip)
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-the-opposite-point (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to
   contemporaneously reside on a jump point, relocates the same to the
   obverse instruction and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (jump-points ip) interpreter
    (declare (type Jump-Table jump-points))
    (declare (type fixnum     ip))
    (setf ip
      (locate-the-matching-jump-destination jump-points ip)))
  (values))

;;; -------------------------------------------------------

(defun the-program-has-been-completely-processed-p (interpreter)
  "Determines whether the program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun execute-the-program-of-the-interpreter (interpreter)
  "Executes the program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (tape) interpreter
    (declare  (type Tape tape))
    (loop
      until
        (the-program-has-been-completely-processed-p interpreter)
      do
        (case (query-the-current-instruction interpreter)
          (:increment
            (incf (the-current-cell-value tape)))
          
          (:decrement
            (decf (the-current-cell-value tape)))
          
          (:move-left
            (move-the-cell-pointer-left tape))
          
          (:move-right
            (move-the-cell-pointer-right tape))
          
          (:input
            (format *query-io* "~&>> ")
            (finish-output *query-io*)
            (setf (the-current-cell-value tape)
              (char-code
                (read-char *query-io* NIL #\Null)))
            (clear-input *query-io*))
          
          (:output
            (format *query-io* "~c"
              (code-char
                (the-current-cell-value tape))))
          
          (:jump-forward
            (when (zerop (the-current-cell-value tape))
              (jump-to-the-opposite-point interpreter)))
          
          (:jump-back
            (unless (zerop (the-current-cell-value tape))
              (jump-to-the-opposite-point interpreter)))
          
          (otherwise
            (error "The instruction ~s cannot be recognized."
              (query-the-current-instruction interpreter))))
        
        (advance-to-the-next-instruction interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-chicken-you-too-beautiful-code (code)
  "Interprets the piece of Chicken you too beautiful source CODE and
   returns no value."
  (declare (type string code))
  (execute-the-program-of-the-interpreter
    (prepare-an-interpreter-for
      (parse-the-string-as-a-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-chicken-you-too-beautiful-code
  "sing rap dance sing basketball")

;;; -------------------------------------------------------

;; Print the message "Hello World!" to the standard output conduit.
(interpret-the-chicken-you-too-beautiful-code
  "chicken chicken chicken chicken chicken chicken chicken chicken rap beautiful chicken chicken chicken chicken rap beautiful chicken chicken beautiful chicken chicken chicken beautiful chicken chicken chicken beautiful chicken too too too too you basketball beautiful chicken beautiful chicken beautiful you beautiful beautiful chicken rap too basketball too you basketball beautiful beautiful dance beautiful you you you dance chicken chicken chicken chicken chicken chicken chicken dance dance chicken chicken chicken dance beautiful beautiful dance too you dance too dance chicken chicken chicken dance you you you you you you dance you you you you you you you you dance beautiful beautiful chicken dance beautiful chicken chicken dance")

;;; -------------------------------------------------------

;; brainfuck interpreter, based upon "dbfi", a brainfuck
;; self-interpreter.
(interpret-the-chicken-you-too-beautiful-code
  "beautiful beautiful beautiful chicken rap rap you basketball beautiful beautiful rap you basketball chicken chicken beautiful chicken beautiful chicken chicken chicken chicken chicken chicken chicken rap too chicken chicken chicken chicken beautiful beautiful chicken chicken too you basketball chicken chicken beautiful beautiful chicken beautiful chicken beautiful chicken chicken chicken chicken chicken rap beautiful chicken chicken beautiful chicken chicken chicken chicken chicken chicken too too you basketball chicken beautiful beautiful beautiful sing too chicken chicken rap rap beautiful rap you beautiful beautiful basketball too rap beautiful beautiful basketball too too you basketball too rap too basketball too chicken beautiful beautiful rap beautiful basketball beautiful rap too chicken beautiful you rap rap too chicken beautiful you basketball beautiful basketball too rap rap rap you basketball too basketball chicken chicken too you rap too chicken chicken chicken chicken chicken chicken chicken chicken chicken beautiful rap too you beautiful you basketball beautiful beautiful basketball beautiful beautiful basketball basketball too too basketball too basketball too rap rap too basketball beautiful rap rap beautiful basketball beautiful beautiful rap beautiful beautiful basketball chicken rap too too basketball too rap too basketball too chicken beautiful beautiful you basketball beautiful rap beautiful basketball chicken rap you beautiful beautiful basketball too too too too rap rap too too basketball too rap too basketball chicken too too rap chicken beautiful chicken too too you rap beautiful you you beautiful chicken too too you rap beautiful chicken too rap beautiful beautiful chicken too too you basketball basketball basketball beautiful rap too chicken beautiful you basketball too basketball chicken chicken beautiful beautiful you you beautiful rap beautiful basketball beautiful beautiful rap beautiful beautiful basketball basketball too too rap beautiful beautiful chicken too rap rap too basketball too basketball beautiful rap rap too too basketball too rap too basketball chicken rap you too chicken beautiful beautiful you rap too too chicken beautiful chicken chicken beautiful you rap too you beautiful rap too too chicken beautiful beautiful you basketball basketball basketball too rap beautiful chicken too you basketball beautiful basketball beautiful rap beautiful basketball beautiful basketball beautiful rap beautiful beautiful basketball beautiful beautiful basketball too too rap beautiful beautiful chicken beautiful beautiful chicken beautiful beautiful basketball too too rap you beautiful beautiful beautiful beautiful beautiful beautiful beautiful beautiful basketball too too rap beautiful dance beautiful beautiful beautiful beautiful beautiful beautiful beautiful basketball too too rap beautiful you beautiful beautiful beautiful beautiful beautiful basketball too too rap beautiful sing beautiful beautiful beautiful basketball too too rap beautiful chicken beautiful basketball too too rap chicken too too basketball too basketball")
