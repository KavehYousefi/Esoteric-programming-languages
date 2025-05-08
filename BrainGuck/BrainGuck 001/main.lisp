;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BrainGuck", invented by the Esolang user "Periapsis" and
;; presented on August 1st, 2017, the commorancy of its haecceity an
;; extension of Urban Mueller's "brainfuck" by a quintuple operative
;; competences, its perimeter amplecting a complement in the jump-based
;; control flow conduction, the transfer of cell values via the cell
;; pointer, as well as a cell state resetting facility.
;; 
;; 
;; Concept
;; =======
;; The BrainGuck programming language's gendrure is vindicated by an
;; extension of the brainfuck programming language's octuple competences
;; to an additional control flow governance mechanism, the potential
;; for the copying of a cell's state to a temporary location in the
;; cell pointer, with the capacitation for future insertions at the
;; new pointer position, as well as a facility for resetting a cell to
;; its inchoate state of zero (0).
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE VALUES ==
;; BrainGuck's ipsissima verba appropriation of its entheus' designment
;; propages through the architecture, its reliance that upon a
;; bilaterally infinite dispansion of unsigned byte-valued cells.
;; 
;; == CELLS WRAP AROUND IN ORDER TO ENSURE THE RANGE [0, 255] ==
;; Upon any transgression of the thus established admissible capacity,
;; meted at [0, 255], the state wraps around to the athwart extremum,
;; commencing for a transcendence of the upper bourne of 255 to zero
;; (0), and for a descent alow that minimum to 255.
;; 
;; == A POINTER SELECTS THE CURRENTLY ACTIVE CELL ==
;; Inwith the tape a dedicated cursor, the "cell pointer", is admitted
;; its woning, selecting at any instant the currently active cell, this
;; constituting the sole unit entalented with amenability to the
;; state's perquisitions and modulations. The motile nature incorporated
;; in this pointer homologates stillatim translations along both axes
;; in order to alter the selection.
;; 
;; 
;; Instructions
;; ============
;; An enhancement of brainfuck, BrainGuck's instruction set rises beyond
;; the octuple cleronomy's appropriation in the introduction of a
;; quintuple polymechanies, airted at the transfers of cell values via
;; the cell pointer, the resetting of the currently selected unit's
;; state, as well as a complementary designment to the acquainted
;; jump-based control flow mechanism.
;; 
;; == OVERVIEW ==
;; The following apercu shall be employed with telos of a requisite
;; mete of gnarity's communication anent BrainGuck's account in
;; operations:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell's value by one (1). If the
;;           | new state transcedes the upper bourne of 255, the value
;;           | wraps around to the minimum of zero (0).
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   -       | Decrements the current cell's value by one (1). If the
;;           | new state transcedes the lower bourne of zero 90), the
;;           | value wraps around to the maximum of 255.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   <       | Translates the cell pointer oen step to the left.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   [       | If the current cell contains zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ..................................................................
;;   ]       | If the current cell does not contain zero (0), moves the
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation from
;;           | brainfuck's diorism.
;;   ==================================================================
;;   {       | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer (IP) forward to the
;;           | position immediately succeeding the matching "}" token;
;;           | otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a novelty of BrainGuck,
;;           | absent from its stock-father brainfuck.
;;   ..................................................................
;;   }       | If the current cell contains the value zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "{" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a novelty of BrainGuck,
;;           | absent from its stock-father brainfuck.
;;   ..................................................................
;;   ^       | Copies the current cell value into the cell pointer's
;;           | storage component, replacing the preveniently extant
;;           | datum in the pointer.
;;           |---------------------------------------------------------
;;           | This operation constitutes a novelty of BrainGuck,
;;           | absent from its stock-father brainfuck.
;;   ..................................................................
;;   v       | Copies the value stored in the cell pointer's storage
;;           | component into the current cell, thus superseding the
;;           | cell's state.
;;   ..................................................................
;;   0       | Resets the current cell's value to the inchoate state
;;           | of zero (0).
;;           |---------------------------------------------------------
;;           | This operation constitutes a novelty of BrainGuck,
;;           | absent from its stock-father brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been realized in the programming
;; language Common Lisp, operating in a kenspeckle mode by which the
;; source code string is directly transformed into a tantamount
;; Common Lisp program by adminiculum of macros.
;; 
;; The highest mete of conspectuity harbors its commorancy in the
;; jump instruction translations, appertain to the jumelles "[" and "]",
;; as well as "{" and "}", the same enjoy their causata's replication
;; via a "go to"-based approach, rendered as the ultimity of a Common
;; Lisp "tagbody" form that assigns to each twissel of matching forward
;; and back jump token a navigable label of unique identification.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-29
;; 
;; Sources:
;;   [esolang2023BrainGuck]
;;   The Esolang contributors, "BrainGuck", November 10th, 2023
;;   URL: "https://esolangs.org/wiki/BrainGuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT as a \"generalized boolean\" and returns an
   actual Boolean tantamount thereof, producing for a non-``NIL'' input
   a ``boolean'' value of ``T''; otherwise, for a ``NIL'' OBJECT,
   responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designates-any-type-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER admits any object by its
   conflation with the generic sentinel ``*'', returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))

;;; -------------------------------------------------------


(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is desumed from the
   TYPE-NAME and whose formal parameters' provenance ensues from the
   LAMBDA-LIST, the probed candidate receiving the norning communicated
   in the CANDIDATE-NAME, evaluates the BODY forms, with the desinent
   form's primary result construed as the docimasy's conclusion, a
   \"generalized boolean\" \"true\" value enjoying the interpretation
   as the candidate's acceptance, while a \"false\" sentinel amounts to
   the compatibility's rejection.
   ---
   The first body form, if representing a string object, is construed as
   the derived type's documentation string, and is consequently
   reappropriated for this purpose."
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
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, and as such a commorant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, its
   default registered as the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (designates-any-type-p element-type)
      (every
        #'(lambda (current-element)
            (declare (type T current-element))
            (typep current-element element-type))
        (the list candidate)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   '*)
                                                 (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, affiliating each key of the KEY-TYPE with a value
   adhering to the VALUE-TYPE, for both holds the generic sentinel ``*''
   as a default."
  (and
    (hash-table-p candidate)
    (or
      (and (designates-any-type-p key-type)
           (designates-any-type-p value-type))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and
            (or (designates-any-type-p key-type)
                (typep                 current-key key-type))
            (or (designates-any-type-p value-type)
                (typep                 current-value value-type)))))))

;;; -------------------------------------------------------

(deftype jump-label-twain ()
  "The ``jump-label-twain'' type defines a twissel of matching forward
   and back jump labels as symbols in a design amenable to the
   ``tagbody'' and ``prog'' forms' expectations for such control flow
   tags, ensconced in a cons cell, the first compartment of which bears
   the forward jump label name, while the second lends a commorancy to
   the back jump complement."
  '(cons symbol symbol))

;;; -------------------------------------------------------

(deftype jump-label-table ()
  "The ``jump-label-table'' type defines an association betwixt a
   matching jump label twain's numeric nesting level as the respective
   label identifiers, realized as a hash table whose keys maintain the
   level as a fixnum, while the affiliated value is represented by a
   cons of the symbolic forward and back jump label names."
  '(hash-table-of fixnum jump-label-twain))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type unsigned-byte *tape-bits*))
(declaim (type integer       *cell-pointer-position*))
(declaim (type octet         *cell-pointer-value*))
(declaim (type integer       *smallest-cell-index*))
(declaim (type octet         *current-byte-segment*))

;;; -------------------------------------------------------

(defparameter *tape-bits* 0
  "The program memory tape conceived as a bilaterally infinite
   dispansion of unsigned byte-valued cells, and implemented by
   adminiculum of a single integer-encoded bit sequence.
   ---
   Each eight consecutive bits coalesce to form one cell, with the
   left-most defined unit always residing at the lowest bits, commencing
   with the bit offset zero (0).")

(defparameter *cell-pointer-position* 0
  "The cell pointer's current position, that is, the signed integer
   cell index.")

(defparameter *cell-pointer-value* 0
  "The value stored in the cell pointer, copied from a desiderated cell
   and intended for future transfers.")

(defparameter *smallest-cell-index* 0
  "The smallest value assumed by the *CELL-POINTER-POSITION* during the
   course of a program, employed during the translation of the cell
   pointer index into a non-negative offset into the *TAPE-BITS*.")

;;; -------------------------------------------------------

(defun translate-cell-index-to-bit-offset ()
  "Returns the zero-based bit offset into the *TAPE-BITS* corresponding
   to the address of the first bit comprising the cell located at the
   *CELL-POINTER-POSITION*."
  (the (integer 0 *)
    (* (- *cell-pointer-position*
          *smallest-cell-index*)
       8)))

;;; -------------------------------------------------------

;; An accessor into the *TAPE-BITS* selecting the eight accolent bits
;; which correspond to the tape cell amenable to the current
;; *CELL-POINTER-POSITION*.
(define-symbol-macro *current-byte-segment*
  (the octet
    (ldb
      (byte 8
        (translate-cell-index-to-bit-offset))
      *tape-bits*)))

;;; -------------------------------------------------------

(defun prepare-memory-for-execution ()
  "Resets the program memory's state to its inchoate form and returns
   no value."
  (psetf
    *tape-bits*             0
    *cell-pointer-position* 0
    *cell-pointer-value*    0
    *smallest-cell-index*   0)
  (values))

;;; -------------------------------------------------------

(defun current-cell-value ()
  "Returns the unsigned byte value stored in the current cell."
  (the octet *current-byte-segment*))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value)
  "Stores the NEW-VALUE in the current cell, contingently preceded by
   its wrapping around to accommodate the admissible unsigned byte range
   of [0, 255], and returns no value."
  (declare (type integer new-value))
  (setf *current-byte-segment* (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p ()
  "Determines whether the current cell contains the value zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value)))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right ()
  "Translates the cell pointer one step to the right and returns no
   value."
  (incf *cell-pointer-position*)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left ()
  "Translates the cell pointer one step to the left and returns no
   value."
  (decf *cell-pointer-position*)
  (when (< *cell-pointer-position*
           *smallest-cell-index*)
    (psetf
      *smallest-cell-index* *cell-pointer-position*
      *tape-bits*           (ash *tape-bits* 8)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump label builder operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-symbol (format-control &rest format-arguments)
  "Creates and returns a fresh symbol whose agnomination derives from
   the FORMAT-CONTROL being rendered with the FORMAT-ARGUMENTS, akin to
   the ``format'' function."
  (declare (type string      format-control))
  (declare (type (list-of *) format-arguments))
  (the symbol
    (intern
      (apply #'format NIL format-control format-arguments))))

;;; -------------------------------------------------------

(declaim (type fixnum           *jump-if-zero-level-counter*))
(declaim (type (list-of fixnum) *jump-if-zero-level-matcher*))
(declaim (type jump-label-table *jump-if-zero-labels*))

(declaim (type fixnum           *jump-if-not-zero-level-counter*))
(declaim (type (list-of fixnum) *jump-if-not-zero-level-matcher*))
(declaim (type jump-label-table *jump-if-not-zero-labels*))

;;; -------------------------------------------------------

(defparameter *jump-if-zero-level-counter* 0
  "Generates a numeric identifier for each twissel of \"[\" and \"]\"
   instructions, this being a tantamount to their nesting level.")

(defparameter *jump-if-zero-level-matcher* NIL
  "A stack whose castaldy appertains to the \"[\" and \"]\"
   instructions' nesting levels, with items of higher recency woning
   nearer to the top.")

(defparameter *jump-if-zero-labels*
  (make-hash-table :test #'eql)
  "Associates the \"[\" and \"]\" instructions' nesting levels,
   apportioned via the *JUMP-IF-ZERO-LEVEL-COUNTER*, with a twain of
   forward and jump label names, defined as symbols and ensconced in
   a dedicated cons cell.")

(defparameter *jump-if-not-zero-level-counter* 0
  "Generates a numeric identifier for each twissel of \"{\" and \"}\"
   instructions, this being a tantamount to their nesting level.")

(defparameter *jump-if-not-zero-level-matcher* NIL
  "A stack whose castaldy appertains to the \"{\" and \"}\"
   instructions' nesting levels, with items of higher recency woning
   nearer to the top.")

(defparameter *jump-if-not-zero-labels*
  (make-hash-table :test #'eql)
  "Associates the \"{\" and \"}\" instructions' nesting levels,
   apportioned via the *JUMP-IF-NOT-ZERO-LEVEL-COUNTER*, with a twain of
   forward and jump label names, defined as symbols and ensconced in
   a dedicated cons cell.")

;;; -------------------------------------------------------

(defun prepare-jump-label-builder ()
  "Resets the jump label builder's state and returns no value."
  (psetf
    *jump-if-zero-level-counter* 0
    *jump-if-zero-level-matcher* NIL)
  (clrhash *jump-if-zero-labels*)
  (psetf
    *jump-if-not-zero-level-counter* 0
    *jump-if-not-zero-level-matcher* NIL)
  (clrhash *jump-if-not-zero-labels*)
  (values))

;;; -------------------------------------------------------

(defun build-jump-if-not-zero-labels (nesting-level)
  "Creates a twissel of interrelated jump labels, yclept by an
   automatically assigned name which incorporates the NESTING-LEVEL,
   and communicated via cons cell whose componency amplects the symbolic
   names."
  (declare (type fixnum nesting-level))
  (the jump-label-twain
    (cons
      (build-symbol "JUMP-FORWARD-IF-NOT-ZERO-~d" nesting-level)
      (build-symbol "JUMP-BACK-IF-ZERO-~d"        nesting-level))))

;;; -------------------------------------------------------

(defun memorize-jump-if-not-zero-labels (jump-labels)
  "Associates the current *JUMP-IF-NOT-ZERO-LEVEL-COUNTER* state with
   the JUMP-LABELS twain in the *JUMP-IF-NOT-ZERO-LABELS* table and
   returns no value."
  (declare (type jump-label-twain jump-labels))
  (setf (gethash *jump-if-not-zero-level-counter*
                 *jump-if-not-zero-labels*)
        jump-labels)
  (values))

;;; -------------------------------------------------------

(defun define-jump-if-not-zero-labels ()
  "Defines a twissel of symbolic jump labels, utilizing the
   *JUMP-IF-NOT-ZERO-LEVEL-COUNTER*'s contemporaneous state, stores the
   association betwixt the nesting level and the labels in the
   *JUMP-IF-NOT-ZERO-LABELS* table, updates the level, and returns the
   thus created label twain."
  (push *jump-if-not-zero-level-counter*
        *jump-if-not-zero-level-matcher*)
  (let ((jump-labels
          (build-jump-if-not-zero-labels
            *jump-if-not-zero-level-counter*)))
    (declare (type jump-label-twain jump-labels))
    (memorize-jump-if-not-zero-labels jump-labels)
    (incf *jump-if-not-zero-level-counter*)
    (the jump-label-twain jump-labels)))

;;; -------------------------------------------------------

(defun query-jump-if-not-zero-labels ()
  "Returns the symbolic forward and back jump label names associated
   with the currently active *JUMP-IF-NOT-ZERO-LEVEL-MATCHER* nesting
   level, or signals an error of an unspecified type upon its
   disrespondency."
  (the jump-label-twain
    (or
      (and
        *jump-if-not-zero-level-matcher*
        (gethash
          (pop *jump-if-not-zero-level-matcher*)
          *jump-if-not-zero-labels*))
      (error "Unmatched \"}\" instruction encountered."))))

;;; -------------------------------------------------------

(defun build-jump-if-zero-labels (nesting-level)
  "Creates a twissel of interrelated jump labels, yclept by an
   automatically assigned name which incorporates the NESTING-LEVEL,
   and communicated via cons cell whose componency amplects the symbolic
   names."
  (declare (type fixnum nesting-level))
  (the jump-label-twain
    (cons
      (build-symbol "JUMP-FORWARD-IF-ZERO-~d"  nesting-level)
      (build-symbol "JUMP-BACK-IF-NOT-ZERO-~d" nesting-level))))

;;; -------------------------------------------------------

(defun memorize-jump-if-zero-labels (jump-labels)
  "Associates the current *JUMP-IF-ZERO-LEVEL-COUNTER* state with
   the JUMP-LABELS twain in the *JUMP-IF-ZERO-LABELS* table and returns
   no value."
  (declare (type jump-label-twain jump-labels))
  (setf (gethash *jump-if-zero-level-counter*
                 *jump-if-zero-labels*)
        jump-labels)
  (values))

;;; -------------------------------------------------------

(defun define-jump-if-zero-labels ()
  "Defines a twissel of symbolic jump labels, utilizing the
   *JUMP-IF-ZERO-LEVEL-COUNTER*'s contemporaneous state, stores the
   association betwixt the nesting level and the labels in the
   *JUMP-IF-ZERO-LABELS* table, updates the level, and returns the thus
   created label twain."
  (push *jump-if-zero-level-counter*
        *jump-if-zero-level-matcher*)
  (let ((jump-labels
          (build-jump-if-zero-labels
            *jump-if-zero-level-counter*)))
    (declare (type jump-label-twain jump-labels))
    (memorize-jump-if-zero-labels jump-labels)
    (incf *jump-if-zero-level-counter*)
    (the jump-label-twain jump-labels)))

;;; -------------------------------------------------------

(defun query-jump-if-zero-labels ()
  "Returns the symbolic forward and back jump label names associated
   with the currently active *JUMP-IF-ZERO-LEVEL-MATCHER* nesting level,
   or signals an error of an unspecified type upon its disrespondency."
  (the jump-label-twain
    (or
      (and
        *jump-if-zero-level-matcher*
        (gethash
          (pop *jump-if-zero-level-matcher*)
          *jump-if-zero-labels*))
      (error "Unmatched \"]\" instruction encountered."))))

;;; -------------------------------------------------------

(defun check-if-all-jump-points-are-matched ()
  "Determines whether all \"[\" and \"{\" instructions enjoy their
   proper affiliations with corresponding \"]\" and \"}\" tokens,
   returning on confirmation no value; otherwise signals an error of an
   unspecified type."
  (cond
    (*jump-if-zero-level-matcher*
      (error "One or more unmatched \"[\" instructions exist."))
    (*jump-if-not-zero-level-matcher*
      (error "One or more unmatched \"{\" instructions exist."))
    (T
      NIL))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program builder.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of *)   *program-builder-head*))
(declaim (type (cons    * *) *program-builder-tail*))

;;; -------------------------------------------------------

(defparameter *program-builder-head*
  (list NIL)
  "A reference to the code builder list's front, which constitutes a
   tantamount to the entire list.")

(defparameter *program-builder-tail*
  (last *program-builder-head*)
  "A reference to the program builder list's desinent cons cell,
   employed for efficient insertions at the tail.")

;;; -------------------------------------------------------

(defun append-forms-to-program (&rest new-forms)
  "Inserts the NEW-FORMS at the program builder's rear and returns no
   value."
  (declare (type (list-of T) new-forms))
  (setf (rest *program-builder-tail*) new-forms)
  (setf *program-builder-tail*        (last *program-builder-tail*))
  (values))

;;; -------------------------------------------------------

(defun get-assembled-program ()
  "Returns a list comprehending all forms consigned to the program
   builder's castaldy."
  (the (list-of T)
    (rest *program-builder-head*)))

;;; -------------------------------------------------------

(defun clear-program-builder ()
  "Deprive the program builder from all forms and returns no value."
  (setf (rest *program-builder-head*) NIL)
  (setf *program-builder-tail*        (last *program-builder-head*))
  (values))

;;; -------------------------------------------------------

(defun transform-token (token)
  "Generates for the BrainGuck TOKEN a Common Lisp code tmema capable
   of its causata's replication, appends thilk to the program builder,
   and returns no value."
  (declare (type character token))
  (case token
    (#\+
      (append-forms-to-program
        '(incf (current-cell-value))))
    
    (#\-
      (append-forms-to-program
        '(decf (current-cell-value))))
    
    (#\>
      (append-forms-to-program
        '(move-cell-pointer-right)))
    
    (#\<
      (append-forms-to-program
        '(move-cell-pointer-left)))
    
    (#\.
      (append-forms-to-program
        '(format *standard-output* "~c"
           (code-char
             (current-cell-value)))))
    
    (#\,
      (append-forms-to-program
        '(when *displays-prompt-during-input-p*
           (format *standard-output* "~&>> "))
        '(finish-output)
        '(setf (current-cell-value)
           (char-code
             (read-char *standard-input* NIL #\Null)))
        '(clear-input)))
    
    (#\[
      (destructuring-bind (forward-jump-label . back-jump-label)
          (define-jump-if-zero-labels)
        (declare (type symbol forward-jump-label))
        (declare (type symbol back-jump-label))
        (append-forms-to-program
          forward-jump-label
          `(when (current-cell-contains-zero-p)
             (go ,back-jump-label)))))
    
    (#\]
      (destructuring-bind (forward-jump-label . back-jump-label)
          (query-jump-if-zero-labels)
        (declare (type symbol forward-jump-label))
        (declare (type symbol back-jump-label))
        (append-forms-to-program
          back-jump-label
          `(unless (current-cell-contains-zero-p)
             (go ,forward-jump-label)))))
    
    (#\{
      (destructuring-bind (forward-jump-label . back-jump-label)
          (define-jump-if-not-zero-labels)
        (declare (type symbol forward-jump-label))
        (declare (type symbol back-jump-label))
        (append-forms-to-program
          forward-jump-label
          `(unless (current-cell-contains-zero-p)
             (go ,back-jump-label)))))
    
    (#\}
      (destructuring-bind (forward-jump-label . back-jump-label)
          (query-jump-if-not-zero-labels)
        (declare (type symbol forward-jump-label))
        (declare (type symbol back-jump-label))
        (append-forms-to-program
          back-jump-label
          `(when (current-cell-contains-zero-p)
             (go ,forward-jump-label)))))
    
    (#\^
      (append-forms-to-program
        '(setf *cell-pointer-value* (current-cell-value))))
    
    (#\v
      (append-forms-to-program
        '(setf (current-cell-value) *cell-pointer-value*)))
    
    (#\0
      (append-forms-to-program
        '(setf (current-cell-value) 0)))
    
    (otherwise NIL))
  
  (values))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Assembles a Common Lisp program whose capacitation homologates its
   operation in the sense of the piece of BrainGuck source CODE and
   returns the resulting form."
  (declare (type string code))
  (the (list-of T)
    `(tagbody
       (prepare-memory-for-execution)
       (prepare-jump-label-builder)
       (clear-program-builder)
       ,@(loop
           for current-token of-type character across code do
             (transform-token current-token)
           finally
             (check-if-all-jump-points-are-matched)
             (return (get-assembled-program))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type boolean *displays-prompt-during-input-p*))

;;; -------------------------------------------------------

(defparameter *displays-prompt-during-input-p* T
  "A Boolean flag the same determines whether an input request, issued
   via the BrainGuck instruction \",\", shall accompass the printing of
   a prompt message to the standard output ere the data's reception.")

;;; -------------------------------------------------------

(defmacro evaluate-BrainGuck-code (code)
  "Assembles for the piece of BrainGuck source CODE a operatively
   paregal in a Common Lisp program, executes the same, and returns no
   value."
  (let ((evaluated-code (gensym)))
    (declare (type symbol evaluated-code))
    `(let ((,evaluated-code ,code))
       (declare (type string ,evaluated-code))
       (eval (parse-program ,evaluated-code))
       (values))))

;;; -------------------------------------------------------

(defun interpret-BrainGuck (code)
  "Interprets the piece ocde BrainGuck source CODE and returns no
   value."
  (declare (type string code))
  (evaluate-BrainGuck-code code)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!" to the standard output.
(interpret-BrainGuck
  "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-BrainGuck ",[.,]")

;;; -------------------------------------------------------

;; Perpetually repeating cat program.
(interpret-BrainGuck "{,.0}")

;;; -------------------------------------------------------

;; Reverse cat program which operates on three input characters, each
;; such expected to differ from the "null character".
;; 
;; The code seeks forward and backward utilizing the two jump-based
;; control flow mechanisms and employs the cell value transfers
;; instructions "^" and "v".
(interpret-BrainGuck
  ",>,>,>+<
   {<}^0{>}[>]v.
   [<]{<}^0{>}[>]v.
   [<]{<}^0{>}[>]v.")

;;; -------------------------------------------------------

;; Reverse cat program which operates on three input characters, each
;; such expected to differ from the "null character".
;; 
;; The code seeks forward and backward utilizing the two jump-based
;; control flow mechanisms and employs the cell value transfers
;; instructions "^" and "v".
(let ((*standard-input*                (make-string-input-stream "ABC"))
      (*displays-prompt-during-input-p* NIL))
  (declare (type boolean *displays-prompt-during-input-p*))
  (interpret-BrainGuck
    ",>,>,>+<
     {<}^0{>}[>]v.
     [<]{<}^0{>}[>]v.
     [<]{<}^0{>}[>]v."))
