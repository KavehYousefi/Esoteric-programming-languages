;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NULL-NONE NOTHING", invented by the Esolang user "Xi-816"
;; and presented on September 2nd, 2023, the tiver administered to its
;; caract the deployment of instruction identifiers as permutations, or
;; "combowords", the constituents of which are desumed from the tokens
;; "NULL", "NONE", and "NOTHING", alligated via hyphens, while operating
;; on a twifaced memory edified upon a 50x50 matrix in coefficiency with
;; a stack, both ordained to the castaldy of signed integer number.
;; 
;; 
;; Concept
;; =======
;; The NULL-NONE NOTHING programming language's diorism wones in a
;; twifold conceptions: Imprimis, a particular species of diction
;; applies to the operative warklumes' identification, the agnominations
;; a gendrure from coalescence of the treble constituents "NULL",
;; "NONE", and "NOTHING" into "combowords", ligated into a single word
;; via a hyphen in their interstitial locations.
;; 
;; In a secondary of the conspectuity's adhibition, the memory's
;; bifidate architecture ostends a matrix of 50 rows and 50 columns,
;; concredited to the castaldy of signed integer number; chevised in
;; this dever by a stack whose elements are desumed from the selfsame
;; vale of numeric entities.
;; 
;; == "NULL", "NONE", AND "NOTHING" ASSEMBLE IDENTIFIERS ==
;; Ostensibly an acolyte of the nihilistic conspection upon the world,
;; the NULL-NONE NOTHING programming language's syntactical designment
;; involves the parcery of twissels or trebles desumed from the set of
;; tokens "NULL", "NONE", and "NOTHING" into combowords, their ligation
;; an effort per procurationem of an aefauld hyphen atwixen each such
;; constituent.
;; 
;; == THE MEMORY: SIGNED INTEGERS IN A 50x50 MATRIX AND A STACK ==
;; A twifold componency's governail is empight in the language's memory
;; model; imprimis, a 50x50 matrix nuncupated to signed integer numbers
;; furnishes a two-dimensional data castaldy construct; further, as a
;; parhedral salvatory, a stack whose elements are desumed from the same
;; numeric realm applies itself to a furnishment.
;; 
;; The twissel's champarty along a well-defined interface elevates the
;; singular participants' conjoined faculties to a valorized potential.
;; 
;; 
;; Instructions
;; ============
;; A quantity of 14 (fourteen) members partakes in the language's
;; instruction set's exhaustion, the bailiwick tangent to this
;; membership's conjoined competences the memory matrix' navigation,
;; the stack composition's manipulation, as well as basic arithmetics
;; applicable on the former, a contexture's adhibition onto the twissel
;; with the telos of their intercourse, and, finally, a jumelle of
;; skipping operations located in the vale of control flow conduction.
;; 
;; == OVERVIEW ==
;; A cursory but sufficient parcery of nortelry's administration anent
;; the operative capacitations shall be concredited to the following
;; apercu's participation:
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ==================================================================
;;   MATRIX HANDLING
;;   ------------------------------------------------------------------
;;   NULL-NOTHING            | Moves the matrix pointer one step to the
;;                           | right.
;;                           |-----------------------------------------
;;                           | If the new position violates the matrix'
;;                           | admissible bournes, an error of the type
;;                           | "InvalidMatrixPositionError" will be
;;                           | signaled.
;;   ..................................................................
;;   NULL-NONE               | Moves the matrix pointer one step to the
;;                           | left.
;;                           |-----------------------------------------
;;                           | If the new position violates the matrix'
;;                           | admissible bournes, an error of the type
;;                           | "InvalidMatrixPositionError" will be
;;                           | signaled.
;;   ..................................................................
;;   NONE-NOTHING            | Moves the matrix pointer one step
;;                           | upwards.
;;                           |-----------------------------------------
;;                           | If the new position violates the matrix'
;;                           | admissible bournes, an error of the type
;;                           | "InvalidMatrixPositionError" will be
;;                           | signaled.
;;   ..................................................................
;;   NONE-NONE               | Moves the matrix pointer one step
;;                           | downwards.
;;                           |-----------------------------------------
;;                           | If the new position violates the matrix'
;;                           | admissible bournes, an error of the type
;;                           | "InvalidMatrixPositionError" will be
;;                           | signaled.
;;   ..................................................................
;;   NOTHING-NONE-NONE       | Increments the matrix' current cell
;;                           | value by one (1).
;;   ..................................................................
;;   NOTHING-NONE-NULL       | Decrements the matrix' current cell
;;                           | value by one (1).
;;   ==================================================================
;;   STACK MANIPULATION
;;   ------------------------------------------------------------------
;;   NULL-NULL-NOTHING       | Pops the stack's top element and
;;                           | discards the same.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, an error of
;;                           | the type "EmptyStackError" will be
;;                           | signaled.
;;   ..................................................................
;;   NULL-NULL-NONE          | Duplicates the stack's top element.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, an error of
;;                           | the type "EmptyStackError" will be
;;                           | signaled.
;;   ..................................................................
;;   NULL-NULL-NULL          | Swaps the positions of the stack's two
;;                           | top elements.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, or cannot
;;                           | accommodate at least two elements, an
;;                           | error of the type "EmptyStackError" will
;;                           | be signaled during the illicit access.
;;   ==================================================================
;;   MATRIX-STACK COMMERCE
;;   ------------------------------------------------------------------
;;   NOTHING-NULL-NONE       | Pushes the matrix' current cell value
;;                           | onto the stack.
;;   ..................................................................
;;   NULL-NULL-NULL-NULL     | Peeks without removing the stack's top
;;                           | element and copies it into the matrix'
;;                           | current cell.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, or cannot
;;                           | accommodate at least two elements, an
;;                           | error of the type "EmptyStackError" will
;;                           | be signaled during the illicit access.
;;   ==================================================================
;;   CONTROL FLOW CONDUCTION
;;   ------------------------------------------------------------------
;;   NOTHING-NULL-NULL       | If the current matrix cell value does
;;                           | not equal zero (0), peek without
;;                           | removing the top stack element, here
;;                           | nevened "n", and skip backward ayond the
;;                           | "n" nearest "NOTHING-NOTHING"
;;                           | instructions; otherwise proceed as
;;                           | usual.
;;                           |-----------------------------------------
;;                           | If the less than the required number of
;;                           | "NOTHING-NOTHING" tokens to skip precede
;;                           | in the program, the instruction pointer
;;                           | (IP) is empight on the program's first
;;                           | instruction.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, an error of
;;                           | the type "EmptyStackError" will be
;;                           | signaled.
;;   ..................................................................
;;   NOTHING-NOTHING-NOTHING | If the current matrix cell value does
;;                           | not equal zero (0), peek without
;;                           | removing the top stack element, here
;;                           | nevened "n", and skip forward ayond the
;;                           | "n" nearest "NOTHING-NOTHING"
;;                           | instructions; otherwise proceed as
;;                           | usual.
;;                           |-----------------------------------------
;;                           | If the less than the required number of
;;                           | "NOTHING-NOTHING" tokens to skip follow
;;                           | in the program, the instruction pointer
;;                           | (IP) is located beyond the program's
;;                           | desistive command, thus terminating the
;;                           | execution immediately.
;;                           |-----------------------------------------
;;                           | If the stack is empty at the instant of
;;                           | this operation's invocation, an error of
;;                           | the type "EmptyStackError" will be
;;                           | signaled.
;;   ..................................................................
;;   NOTHING-NOTHING         | Defines a "pseudo-label", naited only
;;                           | in the context of the skipping commands
;;                           | "NOTHING-NULL-NULL" and
;;                           | "NOTHING-NOTHING-NOTHING".
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation, a labor actuated in the Common
;; Lisp programming language, is entreparted into a twifold tier, the
;; inchoacy that of the language combowords' transcription into
;; representative instruction objects, thilk entertains a sequela
;; involving actual execution stage.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-04
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
;; -- Definition of the global variables and constants.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0 50) +MATRIX-WIDTH+))
(declaim (type (integer 0 50) +MATRIX-HEIGHT+))

;;; -------------------------------------------------------

(defconstant +MATRIX-WIDTH+ 50
  "The tally of columns comprising the memory matrix horizontal
   dispansion.")

(defconstant +MATRIX-HEIGHT+ 50
  "The tally of rows comprising the memory matrix vertical dispansion.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype cell-array ()
  "The ``cell-array'' type defines a matrix composed of two dimensions,
   the first, dispanding along the horizontal axis, conflating in its
   mickleness with the ``+MAXIMUM-WIDTH+'', the second, airted
   vertically, assumes its magnitude from the ``+MATRIX-HEIGHT+''
   constant, this two-dimensional reticulation's cells concredited with
   the castaldy of a signed integer number each."
  `(simple-array integer (,+MATRIX-WIDTH+ ,+MATRIX-HEIGHT+)))

;;; -------------------------------------------------------

(deftype column-index ()
  "The ``column-index'' type defines a zero-based subscript capacitated
   to address a matrix' column."
  `(integer 0 ,(1- +MATRIX-WIDTH+)))

;;; -------------------------------------------------------

(deftype row-index ()
  "The ``column-index'' type defines a zero-based subscript capacitated
   to address a matrix' row."
  `(integer 0 ,(1- +MATRIX-HEIGHT+)))

;;; -------------------------------------------------------

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

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variations on
   NULL-NONE NOTHING instructions."
  '(member
    :increment-pointer-x
    :decrement-pointer-x
    :increment-pointer-y
    :decrement-pointer-y
    :increment-cell
    :decrement-cell
    :copy-cell-to-stack
    :skip-forward
    :skip-backward
    :label
    :pop-from-stack
    :duplicate-stack
    :swap-stack
    :copy-stack-to-cell))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable NULL-NONE NOTHING program
   as a one-dimensional simple array comprehending zero or more
   ``instruction'' objects."
  '(simple-array instruction (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition NULL-NONE-NOTHING-Error (error)
  ()
  (:documentation
    "The ``NULL-NONE-NOTHING-Error'' condition type applies itself to
     a common substratum's vouchsafement upon thilk any condition shall
     be edified whose purpose assigns it to the handling of an anomalous
     circumstance in the course of a NULL-NONE NOTHING program's
     obtention, lexical analyzation, parsing, or execution."))

;;; -------------------------------------------------------

(define-condition Invalid-Matrix-Position-Error
  (NULL-NONE-NOTHING-Error)
  ((x
    :initarg       :x
    :initform      (error "Missing the offending x-coordinate.")
    :reader        invalid-matrix-position-error-x
    :type          fixnum
    :documentation "The x-coordinate of point whose transgression of
                    the memory matrix homologated marches is peccant
                    in this error's infliction.")
   (y
    :initarg       :y
    :initform      (error "Missing the offending y-coordinate.")
    :reader        invalid-matrix-position-error-y
    :type          fixnum
    :documentation "The y-coordinate of point whose transgression of
                    the memory matrix homologated marches is peccant
                    in this error's infliction."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Matrix-Position-Error condition))
      (declare (type stream                        stream))
      (format stream "The position (x=~d, y=~d) violates the ~
                      homologated matrix subscripts, both tolerating ~
                      integer values from the interval [0, 49]."
        (invalid-matrix-position-error-x condition)
        (invalid-matrix-position-error-y condition))))
  (:documentation
    "The ``Invalid-Matrix-Position-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is located in
     the attempt to access the memory matrix via a point whose x- and/or
     y-coordinate violates the admissible bournes."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (NULL-NONE-NOTHING-Error)
  ()
  (:report
    "Cannot peek into or pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology is located in the
     attempt to indagate or remove from an empty stack."))



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

(defun parse-as-a-command (token)
  "Interprets the TOKEN as an operative \"comboword\" and returns the
   affiliated ``instruction'' object."
  (declare (type simple-string token))
  (the instruction
    (switch-on-the-string token
      ("NULL-NOTHING"            :increment-pointer-x)
      ("NULL-NONE"               :decrement-pointer-x)
      ("NONE-NOTHING"            :increment-pointer-y)
      ("NONE-NONE"               :decrement-pointer-y)
      ("NOTHING-NONE-NONE"       :increment-cell)
      ("NOTHING-NONE-NULL"       :decrement-cell)
      ("NOTHING-NULL-NONE"       :copy-cell-to-stack)
      ("NOTHING-NULL-NULL"       :skip-backward)
      ("NOTHING-NOTHING-NOTHING" :skip-forward)
      ("NOTHING-NOTHING"         :label)
      ("NULL-NULL-NOTHING"       :pop-from-stack)
      ("NULL-NULL-NONE"          :duplicate-stack)
      ("NULL-NULL-NULL"          :swap-stack)
      ("NULL-NULL-NULL-NULL"     :copy-stack-to-cell)
      (otherwise
        (error "Unrecognized command token: ~s." token)))))

;;; -------------------------------------------------------

(defun parse-the-program (source)
  "Parses the piece of NULL-NONE NOTHING SOURCE code and returns a
   ``program'' representation of the ensconced instructions in their
   specified order."
  (declare (type simple-string source))
  (the program
    (coerce
      (loop
        with current-position of-type fixnum        = 0
        with current-token    of-type simple-string = ""
        do
          (multiple-value-setq (current-token current-position)
            (extract-the-next-token source current-position))
        if (string-is-empty-p current-token) do
          (loop-finish)
        else collect
          (parse-as-a-command current-token)
        end)
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory matrix.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Matrix ()
  ((cells
    :initform      (make-array
                     (list +MATRIX-HEIGHT+ +MATRIX-WIDTH+)
                     :element-type    'integer
                     :initial-element 0
                     :adjustable      NIL
                     :fill-pointer    NIL)
    :type          cell-array
    :documentation "A simple two-dimensional array of integer-valued
                    entries comprising the matrix.")
   (pointer-x
    :initform      0
    :type          column-index
    :documentation "The currently selected column's index into the
                    CELLS array.")
   (pointer-y
    :initform      0
    :type          row-index
    :documentation "The currently selected row's index into the CELLS
                    array."))
  (:documentation
    "The ``Matrix'' class furnishes an implementation of the
     NULL-NONE NOTHING program memory's matrix component, a
     two-dimensional reticulation of signed integer numbers, enumerating
     both along the horizontal and the vertical axes 50 (fifty)
     cells, its entire dispansion constituting the bailiwick of a mobile
     pointer commorant in two dimensions, thus specifying the currently
     selected column and row twain thilk serves in the distinguishment
     of the active cell, this forming the aefauld entity endowed with an
     amenabiilty to perquisitions and modulations."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-matrix ()
  "Creates and returns a fresh ``Matrix'' whose state at its inchoation
   assigns to each of its entries the default value zero (0)."
  (the Matrix
    (make-instance 'Matrix)))

;;; -------------------------------------------------------

(defun translate-the-cell-pointer-horizontally-by (matrix offset)
  "Determines whether the MATRIX' zero-based column index, ensuing from
   a modulation by the signed OFFSET, yet establishes a valid column
   designator for the MATRIX, on confirmation assigning the thus
   supputated new column index to the MATRIX's x-coordinate pointer;
   otherwise an error of the type ``Invalid-Matrix-Position-Error'' will
   be signaled."
  (declare (type Matrix matrix))
  (declare (type fixnum offset))
  (the column-index
    (with-slots (pointer-x pointer-y) matrix
      (declare (type column-index pointer-x))
      (declare (type row-index    pointer-y))
      (let ((new-x-coordinate (+ pointer-x offset)))
        (declare (type fixnum new-x-coordinate))
        (if (typep new-x-coordinate 'column-index)
          (setf pointer-x new-x-coordinate)
          (error 'Invalid-Matrix-Position-Error
            :x new-x-coordinate
            :y pointer-y))))))

;;; -------------------------------------------------------

(defun translate-the-cell-pointer-vertically-by (matrix offset)
  "Determines whether the MATRIX' zero-based row index, ensuing from a
   modulation by the signed OFFSET, yet establishes a valid row
   designator for the MATRIX, on confirmation assigning the thus
   supputated new row index to the MATRIX's y-coordinate pointer;
   otherwise an error of the type ``Invalid-Matrix-Position-Error'' will
   be signaled."
  (declare (type Matrix matrix))
  (declare (type fixnum offset))
  (the row-index
    (with-slots (pointer-x pointer-y) matrix
      (declare (type column-index pointer-x))
      (declare (type row-index    pointer-y))
      (let ((new-y-coordinate (+ pointer-y offset)))
        (declare (type fixnum new-y-coordinate))
        (if (typep new-y-coordinate 'row-index)
          (setf pointer-y new-y-coordinate)
          (error 'Invalid-Matrix-Position-Error
            :x pointer-x
            :y new-y-coordinate))))))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (matrix)
  "Translates the MATRIX' cell pointer one step to the left and returns
   no value."
  (declare (type Matrix matrix))
  (translate-the-cell-pointer-horizontally-by matrix -1)
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (matrix)
  "Translates the MATRIX' cell pointer one step to the right and returns
   no value."
  (declare (type Matrix matrix))
  (translate-the-cell-pointer-horizontally-by matrix +1)
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-up (matrix)
  "Translates the MATRIX' cell pointer one step upwards and returns no
   value."
  (declare (type Matrix matrix))
  (translate-the-cell-pointer-vertically-by matrix -1)
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-down (matrix)
  "Translates the MATRIX' cell pointer one step downwards and returns
   no value."
  (declare (type Matrix matrix))
  (translate-the-cell-pointer-vertically-by matrix +1)
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (matrix)
  "Returns the signed integer datum consigned to the currently selected
   MATRIX cell's castaldy."
  (declare (type Matrix matrix))
  (the integer
    (with-slots (cells pointer-x pointer-y) matrix
      (declare (type cell-array   cells))
      (declare (type column-index pointer-x))
      (declare (type row-index    pointer-y))
      (aref cells pointer-y pointer-x))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value matrix)
  "Stores the NEW-VALUE in the currently selected MATRIX cell and
   returns no value."
  (declare (type integer new-value))
  (declare (type Matrix  matrix))
  (with-slots (cells pointer-x pointer-y) matrix
    (declare (type cell-array   cells))
    (declare (type column-index pointer-x))
    (declare (type row-index    pointer-y))
    (setf (aref cells pointer-y pointer-x) new-value))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((matrix Matrix) (stream T))
  (declare (type Matrix matrix))
  (declare (type stream stream))
  (with-slots (cells) matrix
    (declare (type cell-array cells))
    (destructuring-bind (height width)
        (array-dimensions cells)
      (declare (type fixnum height))
      (declare (type fixnum width))
      (dotimes (y height)
        (declare (type fixnum y))
        (dotimes (x width)
          (declare (type fixnum x))
          (format stream "~&matrix[~d][~d] = ~d" x y
            (aref cells y x))))))
  (the Matrix matrix))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the skipping operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-forward-in-the-program (program
                                    start-point
                                    number-of-skips)
  "Proceeding from the inclusive START-POINT into the PROGRAM, avaunts
   forward beyond the NUMBER-OF-SKIPS instances of \"NOTHING-NOTHING\"
   pseudo-labels, ultimately returning the desinent retrieved label's
   inclusive position into the PROGRAM.
   ---
   Upon a carency in the available skip points, this function returns
   the length of the PROGRAM as a signification of the search's
   premature abortion."
  (declare (type program program))
  (declare (type fixnum  start-point))
  (declare (type integer number-of-skips))
  (the fixnum
    (if (plusp number-of-skips)
      (loop
        with number-of-remaining-skips
          of-type (integer 0 *)
          =       number-of-skips
        
        for current-position
          of-type fixnum
          from    start-point
          below   (length program)
        for current-instruction
          of-type instruction
          =       (aref program current-position)
        
        when (eq current-instruction :label) do
          (decf number-of-remaining-skips)
          (when (zerop number-of-remaining-skips)
            (return current-position))
        
        finally
          (return
            (length program)))
      start-point)))

;;; -------------------------------------------------------

(defun skip-backward-in-the-program (program
                                     start-point
                                     number-of-skips)
  "Proceeding from the inclusive START-POINT into the PROGRAM, recedes
   backwards beyond the NUMBER-OF-SKIPS instances of \"NOTHING-NOTHING\"
   pseudo-labels, ultimately returning the desinent retrieved label's
   inclusive position into the PROGRAM.
   ---
   Upon a carency in the available skip points, this function returns
   sentinel -1 as a signification of the search's premature abortion."
  (declare (type program program))
  (declare (type fixnum  start-point))
  (declare (type integer number-of-skips))
  (the fixnum
    (if (plusp number-of-skips)
      (loop
        with number-of-remaining-skips
          of-type (integer 0 *)
          =       number-of-skips
        
        for current-position
          of-type  fixnum
          downfrom start-point
          to       0
        for current-instruction
          of-type instruction
          =       (aref program current-position)
        
        when (eq current-instruction :label) do
          (decf number-of-remaining-skips)
          (when (zerop number-of-remaining-skips)
            (return current-position))
        
        finally
          (return -1))
      start-point)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing the interpreter's program.")
    :type          program
    :documentation "The NULL-NONE NOTHING to execute as a vector of
                    instructions.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The zero-based position of the current instruction
                    into the PROGRAM vector.")
   (matrix
    :initform      (prepare-a-pristine-matrix)
    :type          Matrix
    :documentation "A matrix comprehending 50 rows and 50 columns, with
                    a signed integer datum commorant in each cell, and
                    serving as one moeity of the memory's twissel.")
   (stack
    :initform      NIL
    :type          (list-of integer)
    :documentation "A stack of signed integer numbers, contributing the
                    second moeity of the memory's twissel.")
   (prints-matrix-p
    :initarg       :prints-matrix-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the MATRIX
                    shall be printed to the standard output conduit at
                    the execution's inchoacy and desinence.")
   (prints-stack-p
    :initarg       :prints-stack-p
    :initform      T
    :type          boolean
    :documentation "A Boolean flag which determines whether the STACK
                    shall be printed to the standard output conduit at
                    the execution's inchoacy and desinence."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of accompassing
     actual efficacy to a NULL-NONE NOTHING provided as a sequence of
     its comprising instructions."))

;;; -------------------------------------------------------

(defun furnish-an-interpreter-for (program
                                   &key (prints-matrix-p NIL)
                                        (prints-stack-p  T))
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   NULL-NONE NOTHING PROGRAM's execution, issuing, in dependence upon
   the PRINTS-MATRIX-P and PRINTS-STACK-P flags, the respective memory
   components to the standard output at the execution's inchoation and
   desinence."
  (declare (type program program))
  (declare (type boolean prints-matrix-p))
  (declare (type boolean prints-stack-p))
  (the Interpreter
    (make-instance 'Interpreter
      :program         program
      :prints-matrix-p prints-matrix-p
      :prints-stack-p  prints-stack-p)))

;;; -------------------------------------------------------

(defun check-if-the-stack-is-not-empty (interpreter)
  "Determines whether the INTERPRETER's stack is empty, signaling on
   confirmation an error of the type ``Empty-Stack-Error''; otherwise
   returns the probed stack."
  (declare (type Interpreter interpreter))
  (the (list-of integer)
    (with-slots (stack) interpreter
      (declare (type (list-of integer) stack))
      (or stack
          (error 'Empty-Stack-Error)))))

;;; -------------------------------------------------------

(defun push-onto-the-stack (interpreter new-element)
  "Inserts the NEW-ELEMENT at the INTERPRETER stack's top position and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-element))
  (push new-element
    (slot-value interpreter 'stack))
  (values))

;;; -------------------------------------------------------

(defun peek-into-the-stack (interpreter)
  "Returns without removing the top element commorant on the
   INTERPRETER's stack."
  (declare (type Interpreter interpreter))
  (check-if-the-stack-is-not-empty interpreter)
  (the integer
    (first
      (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun pop-from-the-stack (interpreter)
  "Pops and returns the top element commorant on the INTERPRETER's
   stack."
  (declare (type Interpreter interpreter))
  (check-if-the-stack-is-not-empty interpreter)
  (the integer
    (pop
      (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun duplicate-the-stack-top (interpreter)
  "Duplicates the top element commorant on the INTERPRETER's stack and
   returns no value."
  (declare (type Interpreter interpreter))
  (push-onto-the-stack interpreter
    (peek-into-the-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun swap-the-stack-top (interpreter)
  "Swaps the top elements commorant on the INTERPRETER's stack and
   returns no value."
  (declare (type Interpreter interpreter))
  (let ((erstwhile-top-element    (pop-from-the-stack interpreter))
        (erstwhile-second-element (pop-from-the-stack interpreter)))
    (declare (type integer erstwhile-top-element))
    (declare (type integer erstwhile-second-element))
    (push-onto-the-stack interpreter erstwhile-top-element)
    (push-onto-the-stack interpreter erstwhile-second-element))
  (values))

;;; -------------------------------------------------------

(defun program-is-exhausted-p (interpreter)
  "Determines whether the NULL-NONE NOTHING program consigned to the
   INTERPRETER's castaldy has been processed in its entirety, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun execute-the-current-instruction (interpreter)
  "Executes the INTERPRETER's currently selected instruction and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip matrix stack) interpreter
    (declare (type program           program))
    (declare (type fixnum            ip))
    (declare (type Matrix            matrix))
    (declare (type (list-of integer) stack))
    (let ((current-instruction (aref program ip)))
      (declare (type instruction current-instruction))
      (case current-instruction
        (:increment-pointer-x
          (move-the-cell-pointer-right matrix))
        
        (:decrement-pointer-x
          (move-the-cell-pointer-left matrix))
        
        (:increment-pointer-y
          (move-the-cell-pointer-down matrix))
        
        (:decrement-pointer-y
          (move-the-cell-pointer-up matrix))
        
        (:increment-cell
          (incf (current-cell-value matrix)))
        
        (:decrement-cell
          (decf (current-cell-value matrix)))
        
        (:copy-cell-to-stack
          (push-onto-the-stack interpreter
            (current-cell-value matrix)))
        
        (:skip-forward
          (unless (zerop (current-cell-value matrix))
            (setf ip
              (skip-forward-in-the-program program ip
                (peek-into-the-stack interpreter)))))
        
        (:skip-backward
          (unless (zerop (current-cell-value matrix))
            (setf ip
              (skip-backward-in-the-program program ip
                (peek-into-the-stack interpreter)))))
        
        (:label
          NIL)
        
        (:pop-from-stack
          (pop-from-the-stack interpreter))
        
        (:duplicate-stack
          (duplicate-the-stack-top interpreter))
        
        (:swap-stack
          (swap-the-stack-top interpreter))
        
        (:copy-stack-to-cell
          (setf (current-cell-value matrix)
            (peek-into-the-stack interpreter)))
        
        (otherwise
          (error "Unrecognized instruction: ~a."
            current-instruction)))))
  
  (values))

;;; -------------------------------------------------------

(defun print-the-stack (interpreter)
  "Prints the INTERPRETER's memory stack to the standard output and
   returns no value."
  (declare (type Interpreter interpreter))
  (format T "~&[top>~{ ~d~^ ~} <bottom]"
    (slot-value interpreter 'stack))
  (values))

;;; -------------------------------------------------------

(defun print-the-memory-if-necessary (interpreter)
  "Depending upon the INTERPRETER's configurations, prints the memory
   matrix and/or stack to the standard output conduit and returns no
   value."
  (declare (type Interpreter interpreter))
  (when (slot-value interpreter 'prints-matrix-p)
    (format T "~a"
      (slot-value interpreter 'matrix)))
  (when (slot-value interpreter 'prints-stack-p)
    (print-the-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun start-the-interpreter (interpreter)
  "Executes the NULL-NONE NOTHING program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (print-the-memory-if-necessary interpreter)
  (loop until (program-is-exhausted-p interpreter) do
    (execute-the-current-instruction interpreter)
    (incf (slot-value interpreter 'ip)))
  (print-the-memory-if-necessary interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-the-NULL-NONE-NOTHING-CODE
    (code
     &key (prints-matrix-p NIL)
          (prints-stack-p  T))
  "Interprets the piece of NULL-NONE NOTHING source CODE, contingently
   printing the memory matrix and stack, depending upon the
   PRINTS-MATRIX-P and PRINTS-STACK-P flag configurations, to the
   standard output conduit at the program's inchoation and desinence,
   and returns no value."
  (declare (type string  code))
  (declare (type boolean prints-matrix-p))
  (declare (type boolean prints-stack-p))
  (start-the-interpreter
    (furnish-an-interpreter-for
      (parse-the-program
        (coerce code 'simple-string))
      :prints-matrix-p prints-matrix-p
      :prints-stack-p  prints-stack-p))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increment the first matrix cell to a value of two (2), and transfer
;; the resulting state to the stack.
(interpret-the-NULL-NONE-NOTHING-code
  "NOTHING-NONE-NONE NOTHING-NONE-NONE
   NOTHING-NULL-NONE")

;;; -------------------------------------------------------

;; Skip the duplication instruction "NULL-NULL-NONE", thus preventing a
;; duplication of the stack top.
(interpret-the-null-none-nothing-code
  "NOTHING-NONE-NONE NOTHING-NONE-NONE
   NOTHING-NULL-NONE
   NOTHING-NOTHING-NOTHING
   NOTHING-NOTHING
   NULL-NULL-NONE
   NOTHING-NOTHING")
