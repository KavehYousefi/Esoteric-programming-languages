;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter as well as converters for the
;; esoteric programming language "Mierda", invented by the Esolang user
;; "Threesodas" and presented on April 24th, 2021, its haecceity an
;; appropriation of the foundational aspects of Urban Mueller's
;; "brainfuck", expressed, however, with a diction derived from a
;; Spanish transcription of the octuple instruction set's intended
;; causata.
;; 
;; 
;; Concept
;; =======
;; The Mierda programming language constitutes a trivial brainfuck
;; substitution, an exercise of cambistry for the stock-father's
;; single-character operative tokens for a Spanish ambient of parlance,
;; while retaining a lealty to all other bailiwick imposed by its
;; cleronomy.
;; 
;; == MIERDA: A SPANISH VARIATION ON EMOLUMENTAL OBSCENITY ==
;; The choice of "Mierda" as the language's agnomination registers a
;; gendrure from the brain*fuck* traditional, "mierda" translating to
;; the Spanish term for "shit", or, according to the protolog's
;; ascertainment, "fuck".
;; 
;; == MIERDA EMPLOYS A SPANISH DICTIONARY ==
;; Mierda employs the Spanish tongue for the operation consequences'
;; description, allying with everichon of the octuple membership a
;; single term or a twissel compound of word.
;; 
;; Any word in the program eloigned from epiphenomenal potential, either
;; as a singular unit or serelepes in a possible but abortive
;; composition with a valid Mierda keyword, enjoys a negligence
;; equipollent in its assay with its, ultimately otiose, tolerance.
;; 
;; == THE MEMORY: AN INFINITE DISPANSION OF UNSIGNED BYTES ==
;; Its perfect congruency with brainfuck in any mete of conspection
;; liberated from the syntactical guise entalents Mierda with the
;; brainfuck memory model.
;; 
;; This data castaldy appertains to a bilaterally infinite tape of
;; unsigned byte-valued cells, each such a salvatory with the capacity
;; of an integral number from the closed interval [0, 255].
;; 
;; Upon any of its two bournes' transgression, the cell automatically
;; wraps the value around to the athwart border.
;; 
;; A cell pointer governs the onus of the currently active unit's
;; designation, its status the aefauld member contemporaneously invested
;; with an amenability to perquisitions and modulations. The mobile
;; nature of this cursor homologates a stillatim locomotion across the
;; tape pursuing both airts.
;; 
;; 
;; Instructions
;; ============
;; A veridicous and leal descendant of brainfuck's, Mierda's instruction
;; set wists of no bona adventitia, its perimeter the eight competences
;; that enjoy their woning in the entheus' realm.
;; 
;; == OVERVIEW ==
;; The octuple operative warklumes shall be a tabulation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   Mas            | Increments the current cell value by one. If the
;;                  | new state transcends the upper extremum of 255,
;;                  | the cell wraps around to the minimum of zero (0).
;;   ..................................................................
;;   Menos          | Decrements the current cell value by one. If the
;;                  | new state transcends the lower extremum of zero
;;                  | (0), the cell wraps around to the maximum of 255.
;;   ..................................................................
;;   Derecha        | Translates the cell pointer one step to the
;;                  | right.
;;   ..................................................................
;;   Izquierda      | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   Decir          | Prints the character whose ASCII code corresponds
;;                  | to the current cell value to the standard output.
;;   ..................................................................
;;   Leer           | Queries the standard input for a character and
;;                  | stores its ASCII code in the current cell.
;;   ..................................................................
;;   Iniciar Bucle  | If the current cell value equals zero (0), moves
;;                  | the instruction pointer (IP) forward to the
;;                  | position immediately succeeding the matching
;;                  | "Terminar Bucle"; otherwise proceeds as usual.
;;   ..................................................................
;;   Terminar Bucle | If the current cell value does not equal zero
;;                  | (0), moves the instruction pointer (IP) back to
;;                  | the position immediately succeeding the matching
;;                  | "Iniciar Bucle"; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == MIERDA AND BRAINFUCK: AN EQUIPARATION ==
;; An equiparation betwixt Mierda, the represented brainfuck tokens, and
;; an English language transcription shall be the following table's
;; cynosure:
;; 
;;   ------------------------------------------------
;;   Mierda         | brainfuck | English translation
;;   ---------------+-----------+--------------------
;;   Mas            | +         | Plus
;;   ................................................
;;   Menos          | -         | Minus
;;   ................................................
;;   Derecha        | >         | Right
;;   ................................................
;;   Izquierda      | <         | Left
;;   ................................................
;;   Decir          | .         | Print, say
;;   ................................................
;;   Leer           | ,         | Read
;;   ................................................
;;   Iniciar Bucle  | [         | Start loop
;;   ................................................
;;   Terminar Bucle | ]         | End loop
;;   ------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The Mierda interpreter's manifestation is begotten in the programming
;; language Common Lisp, the operative patration a twifold stage that,
;; imprimis, transforms the source code string into a vector of
;; dedicated instruction representations, ere their actual evaluation.
;; 
;; A scion of supererogation, this file comprehends as a paravail
;; contribution converters betwixt Mierda and brainfuck along both
;; lateralities.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-19
;; 
;; Sources:
;;   [esolang2021Mierda]
;;   The Esolang contributors, "Mierda", April 24th, 2021
;;   URL: "https://esolangs.org/wiki/Mierda"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (type-name (candidate-name
                                         &rest lambda-list)
                              &body body)
  "Defines a derived type, nevened by the TYPE-NAME and relying on the
   LAMBDA-LIST to induce its formal parameters, the probed object being
   norned by the CANDIDATE-NAME, evaluates the BODY forms, the desinent
   form's primary result producing the docimasy's conclusion, with a
   \"generalized boolean\" value of \"true\" signifying the candidate's
   eligibility, while \"false\" serves to communicate a rejection.
   ---
   The first BODY form, if resolving to a string object, is administered
   the construe as the defined type's documentation string; as a
   consectary, thilk is reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            (format NIL "Defines the type ``~a''." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name)
                        (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list comprehending zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   thilk holds the default of the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (every
        #'(lambda (current-element)
            (declare (type T current-element))
            (typep current-element element-type))
        (the list candidate)))))

;;; -------------------------------------------------------

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   '*)
                                             (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each key among these complies with
   the KEY-TYPE and answers with a value of the VALUE-TYPE, for both
   holds the default of the generic sentinel ``*''."
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
          (or (eq    current-key   '*)
              (typep current-key   key-type)
          (or (eq    current-value '*)
              (typep current-value value-type)))))))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   Mierda instructions."
  '(member
    :increment
    :decrement
    :move-right
    :move-left
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an execute Mierda program as a
   one-dimensional simple array tallying zero or more ``instruction''
   members."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral mapping betwixt the
   jump forward and back points in a Mierda program by adminiculum of
   their zero-based positions inside of its instruction sequence
   representation, its incarnation produced by a hash table whose keys
   and values both assume these fixnum indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte object composed of eight
   accolent bits, and, as a corollary, commorant in the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass apportioned to whom enumerates, without the claim of its
   potential's exhaustion, the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean equivalency thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program creation operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' whose compass enumerates the
   INSTRUCTIONS."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   a set whose members encompass the space, horizontal tab, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Linefeed #\Newline #\Space #\Tab)
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro string-case (keyform &rest clauses)
  "Evaluates the KEYFORM, and searches for a inquires the CLAUSES,
   everichon of these a list composed of one or more elements, the
   incipient providing the key to juxtapose with the KEYFORM, the clause
   forms to execute upon an eligibility, selects the first covenable
   clause, evaluates its forms, and returns the desinent form's
   results.
   ---
   Each member entailed in the CLAUSES ought to comply to the following
   forbisen:
     ({clause-key} {clause-form-1} ... {clause-form-n})
   Where the CLAUSE-KEY must resolve to a string when evaluated, while
   the CLAUSE-FORM-1 through CLAUSE-FORM-N serves to establish the zero
   or more forms to evaluate upon this clause's activation.
   ---
   For the circumstance defined by a lacuna of eligible options, a
   default clause may be supplied:
     (otherwise {default-form-1} ... {default-form-n})
   or, alternative:
     (T         {default-form-1} ... {default-form-n})
   The covenant appertaining to the fallback specification, with the
   key equiparation exempted, concur perfectly: The zero or more forms
   {default-form-1} through {default-form-n} evaluate and produce in
   the desinent form this operation's result values."
  (let ((evaluated-keyform (gensym)))
    (declare (type symbol evaluated-keyform))
    (flet ((default-clause-key-p (clause-key)
            "Determines whether the CLAUSE-KEY signifies a default case
             by resolving to either of the respective sentinels
             ``otherwise'' or ``T'', returning on confirmation a
             ``boolean'' value of ``T'', otherwise ``NIL''."
            (declare (type (or symbol string) clause-key))
            (the boolean
              (get-boolean-value-of
                (and (symbolp clause-key)
                     (or (eq clause-key 'otherwise)
                         (eq clause-key 'T)))))))
      `(let ((,evaluated-keyform ,keyform))
         (declare (type T ,evaluated-keyform))
         (cond
           ,@(loop for clause of-type list in clauses collect
               (destructuring-bind (clause-key &rest clause-body)
                   clause
                 (declare (type (or symbol string) clause-key))
                 (declare (type list               clause-body))
                 (if (default-clause-key-p clause-key)
                   `(T
                      ,@clause-body)
                   `((string= ,evaluated-keyform ,clause-key)
                      ,@clause-body)))))))))

;;; -------------------------------------------------------

(defmacro do-string ((variable-name source) &body body)
  "Evaluates the SOURCE string, traverses its elements, commencing from
   the first and concluding with the desinent one, upon each cycle
   binding the current character to a variable stevened by the
   VARIABLE-NAME, while executing the BODY forms, and finally returns
   no value."
  (let ((evaluated-source (gensym)))
    (declare (type symbol evaluated-source))
    `(let ((,evaluated-source ,source))
       (declare (type string ,evaluated-source))
       (loop
         for ,variable-name
           of-type character
           across  ,evaluated-source
         do ,@body)
       (values))))

;;; -------------------------------------------------------

(defun locate-start-of-token (source start)
  "Proceeding from the START position into the SOURCE, locates the index
   of the next token's inchoation and returns this subscript."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-token (source start)
  "Proceeding from the START position into the SOURCE, locates the index
   succeeding the next token's desinent character and returns this
   subscript."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-token-bournes (source start)
  "Proceeding from the START position into the SOURCE, locates the
   indices of the next token's start and end and returns two values:
     (1) The index of the next token's first character.
     (2) The index immediately succeeding the next token's last
         character."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((token-start-point (locate-start-of-token source start)))
    (declare (type fixnum token-start-point))
    (the (values fixnum fixnum)
      (values token-start-point
        (locate-end-of-token source token-start-point)))))

;;; -------------------------------------------------------

(defun get-next-token (source start)
  "Proceeding from the START position into the SOURCE, locates the next
   token and returns two values:
     (1) A fresh string representation of the next token.
     (2) The position into the SOURCE immediately succeeding the
         detected token's desinent character."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (token-start-point token-end-point)
      (locate-token-bournes source start)
    (declare (type fixnum token-start-point))
    (declare (type fixnum token-end-point))
    (the (values string fixnum)
      (values
        (subseq source token-start-point token-end-point)
        token-end-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token buffer.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Buffer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :reader        token-buffer-source
    :type          string
    :documentation "The piece of Mierda source code to analyze.")
   (position
    :initform      0
    :accessor      token-buffer-position
    :type          fixnum
    :documentation "The current index into the SOURCE.")
   (previous-token
    :initform      NIL
    :accessor      previous-token
    :type          (or null string)
    :documentation "The token queried in the prevenient step.")
   (current-token
    :initform      NIL
    :accessor      current-token
    :type          (or null string)
    :documentation "The most recently acquired token."))
  (:documentation
    "The ``Token-Buffer'' class applies itself to the dever of the
     tokens' furnishment from a specified piece of Mierda source code
     defined in its original string form."))

;;; -------------------------------------------------------

(defun skip-whitespaces (buffer)
  "Relocates the token BUFFER's position cursor to the next
   non-whitespace character and returns no value."
  (declare (type Token-Buffer buffer))
  (setf (token-buffer-position buffer)
    (locate-start-of-token
      (token-buffer-source   buffer)
      (token-buffer-position buffer)))
  (values))

;;; -------------------------------------------------------

(defun read-first-token (buffer)
  "Consumes the next word from the token BUFFER's source, stores thilk
   in the BUFFER, and returns no value."
  (declare (type Token-Buffer buffer))
  (multiple-value-bind (next-token new-position)
      (get-next-token
        (token-buffer-source   buffer)
        (token-buffer-position buffer))
    (declare (type string next-token))
    (declare (type fixnum new-position))
    (psetf
      (current-token         buffer) next-token
      (token-buffer-position buffer) new-position))
  (skip-whitespaces buffer)
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((buffer Token-Buffer) &key)
  "Relocates the token BUFFER's position cursor to the first
   non-whitespace character in its source and returns no value."
  (declare (type Token-Buffer buffer))
  (read-first-token buffer)
  (values))

;;; -------------------------------------------------------

(defun read-next-token (buffer)
  "Consumes the next word from the token BUFFER's source, stores thilk
   in the BUFFER, and returns no value."
  (declare (type Token-Buffer buffer))
  (multiple-value-bind (next-token new-position)
      (get-next-token
        (token-buffer-source   buffer)
        (token-buffer-position buffer))
    (declare (type string next-token))
    (declare (type fixnum new-position))
    (shiftf
      (previous-token buffer)
      (current-token  buffer)
      next-token)
    (setf (token-buffer-position buffer) new-position))
  (skip-whitespaces buffer)
  (values))

;;; -------------------------------------------------------

(defun make-token-buffer (source)
  "Creates and returns a fresh ``Token-Buffer'' dedicated to the piece
   of Mierda SOURCE code's analyzation."
  (declare (type string source))
  (the Token-Buffer
    (make-instance 'Token-Buffer :source source)))

;;; -------------------------------------------------------

(defun token-buffer-is-exhausted-p (buffer)
  "Determines whether the token BUFFER's has processed its source in its
   entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (the boolean
    (get-boolean-value-of
      (zerop
        (length
          (current-token buffer))))))

;;; -------------------------------------------------------

(defun previous-token-exists-p (buffer)
  "Determines whether the token BUFFER is endowed with the castaldy of
   a token prevenient to the currently active one, returning on
   confirmation a ``boolean'' value of ``T'', therwise ``NIL''."
  (declare (type Token-Buffer buffer))
  (the boolean
    (get-boolean-value-of
      (previous-token buffer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-program (buffer)
  "Extracts from the token BUFFER the ensconced Mierda instructions and
   returns a fresh ``program'' representation thereof."
  (declare (type Token-Buffer buffer))
  (the program
    (make-program
      (loop until (token-buffer-is-exhausted-p buffer) append
        (prog1
          (string-case (current-token buffer)
            ("Mas"       (list :increment))
            ("Menos"     (list :decrement))
            ("Derecha"   (list :move-right))
            ("Izquierda" (list :move-left))
            ("Decir"     (list :output))
            ("Leer"      (list :input))
            ("Iniciar"   NIL)
            ("Terminar"  NIL)
            ("Bucle"
              (when (previous-token-exists-p buffer)
                (string-case (previous-token buffer)
                  ("Iniciar"  (list :jump-forward))
                  ("Terminar" (list :jump-back))
                  (otherwise  NIL))))
            (otherwise NIL))
          (read-next-token buffer))))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts from the piece of Mierda source CODE the ensconced
   instructions and returns a fresh ``program'' representation thereof."
  (declare (type string code))
  (the program
    (assemble-program
      (make-token-buffer code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the jump forward and back instructions, mediated by
   adminiculum of their START-POINT and END-POINT, in a bilateral
   fashion inside of the JUMP-TABLE and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point jump-table) end-point
    (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-jump-points (program jump-table
                              &aux (forward-jump-points NIL))
  "Extracts and connects the forward and back jump instructions
   commorant in the Mierda PROGRAM, registers these in the JUMP-TABLE,
   and returns the modified JUMP-TABLE.
   ---
   Please heed that any extant entry in the JUMP-TABLE amenable to a
   jump point's zero-based index into the PROGRAM will be tacitly
   superseded."
  (declare (type program          program))
  (declare (type jump-table       jump-table))
  (declare (type (list-of fixnum) forward-jump-points))
  (loop
    for current-instruction of-type instruction across program
    and current-position    of-type fixnum      from   0 by 1
    do
      (case current-instruction
        (:jump-forward
          (push current-position forward-jump-points))
        (:jump-back
          (if forward-jump-points
            (connect-jump-points jump-table
              (pop forward-jump-points)
              current-position)
            (error "Missing forward jump point for instruction at ~
                    index ~d."
              current-position)))
        (otherwise NIL))
    finally
      (when forward-jump-points
        (error "Unmatched forward jump instruction~p at program
                ~:*~[index~;index~:;indices~] ~{~d~^, ~}."
          (length forward-jump-points)
          forward-jump-points)))
  (the jump-table jump-table))

;;; -------------------------------------------------------

(defun build-jump-table-for (program)
  "Creates and returns a fresh ``jump-table'' dedicated to the
   ligation of the jump points in the Mierda PROGRAM."
  (declare (type program program))
  (the jump-table
    (supputate-jump-points program
      (make-hash-table :test #'eql))))

;;; -------------------------------------------------------

(defun get-jump-target (jump-table point-of-departure)
  "Returns the jump point obverse to the POINT-OF-DEPARTURE as defined
   in the JUMP-TABLE; or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No jump target associated with the point ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      tape-cells
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of unsigned byte-valued cells,
                    amenable to signed integer subscripts.")
   (pointer
    :initform      0
    :accessor      cell-pointer
    :type          integer
    :documentation "The current selected cell's index, which constitutes
                    a tantamount to the respective key into the CELLS
                    hash table."))
  (:documentation
    "The ``Tape'' class serves in the incarnation of a bilateral
     infinite tape of unsigned byte-valued cells, operated upon by a
     mobile cell pointer which at any instant designates the currently
     active unit."))

;;; -------------------------------------------------------

(defun make-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose cells in their entirety
   assumes the default state of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (gethash
      (cell-pointer tape)
      (tape-cells   tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a parasceve involving the value's adjustment
   by wrapping thilk into the valid unsigned byte range of [0, 255], and
   returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (gethash
      (cell-pointer tape)
      (tape-cells   tape)
      0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (program)
  "Executes the Mierda PROGRAM furnished as a sequence of instructions
   and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (build-jump-table-for program))
        (tape       (make-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Tape       tape))
    (symbol-macrolet
        ((current-instruction
          (the instruction
            (aref program ip)))
         (program-is-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length program)))))
         (current-cell-value
          (the integer
            (current-cell-value tape)))
         (cell-pointer
          (the integer
            (cell-pointer tape)))
         (jump-target-for-current-position
          (the fixnum
            (get-jump-target jump-table ip))))
      (declare (type instruction current-instruction))
      (declare (type boolean     program-is-completed-p))
      (declare (type integer     current-cell-value))
      (declare (type integer     cell-pointer))
      (declare (type fixnum      jump-target-for-current-position))
      (loop until program-is-completed-p do
        (case current-instruction
          (:increment
            (incf current-cell-value))
          (:decrement
            (decf current-cell-value))
          (:move-right
            (incf cell-pointer))
          (:move-left
            (decf cell-pointer))
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
              (setf ip jump-target-for-current-position)))
          (:jump-back
            (unless (zerop current-cell-value)
              (setf ip jump-target-for-current-position)))
          (otherwise
            (error "The instruction ~a at the position ~d cannot be ~
                    resolved."
              current-instruction ip)))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Mierda (code)
  "Interprets the piece of Mierda source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to Mierda.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-Mierda (brainfuck-code
                                    &optional (destination NIL))
  "Translates the BRAINFUCK-CODE into a tantamount Mierda program and
   writes the result to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise responds with a fresh string
   comprehending the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((first-instruction-p T))
        (declare (type boolean first-instruction-p))
        (flet ((write-Mierda-token (token)
                "Prints the TOKEN to the DESTINATION, contingently
                 preceded by a single space if not constituting the
                 inchoate output issuance, and returns no value."
                (declare (type simple-string token))
                (format destination "~:[ ~;~]~a"
                  first-instruction-p
                  token)
                (setf first-instruction-p NIL)
                (values)))
          (do-string (brainfuck-token brainfuck-code)
            (case brainfuck-token
              (#\+       (write-Mierda-token "Mas"))
              (#\-       (write-Mierda-token "Menos"))
              (#\>       (write-Mierda-token "Derecha"))
              (#\<       (write-Mierda-token "Izquierda"))
              (#\.       (write-Mierda-token "Decir"))
              (#\,       (write-Mierda-token "Leer"))
              (#\[       (write-Mierda-token "Iniciar Bucle"))
              (#\]       (write-Mierda-token "Terminar Bucle"))
              (otherwise NIL)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-Mierda brainfuck-code output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from Mierda to brainfuck.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-Mierda-to-brainfuck (mierda-code
                                    &optional (destination NIL))
  "Translates the MIERDA-CODE into a tantamount Mierda program and
   writes the result to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise responds with a fresh string
   comprehending the output."
  (declare (type string      mierda-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((instructions (extract-instructions mierda-code)))
        (declare (type program instructions))
        (loop
          for current-instruction
            of-type instruction
            across  instructions
          and current-position
            of-type fixnum
            from    0
            by      1
          do
            (case current-instruction
              (:increment    (format destination "+"))
              (:decrement    (format destination "-"))
              (:move-right   (format destination ">"))
              (:move-left    (format destination "<"))
              (:output       (format destination "."))
              (:input        (format destination ","))
              (:jump-forward (format destination "["))
              (:jump-back    (format destination "]"))
              (otherwise
                (error "Unrecognized instruction ~s at position ~d."
                  current-instruction current-position)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-Mierda-to-brainfuck mierda-code output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Mierda
  "Mas Iniciar Bucle Menos Menos Derecha Menos Iniciar Bucle Derecha Derecha Mas Derecha Menos Menos Menos Menos Menos Izquierda Izquierda Terminar Bucle Izquierda Menos Menos Izquierda Menos Menos Menos Terminar Bucle Derecha Menos Decir Derecha Derecha Derecha Mas Decir Derecha Derecha Decir Decir Mas Mas Mas Iniciar Bucle Decir Derecha Terminar Bucle Izquierda Izquierda Izquierda Izquierda Decir Mas Mas Mas Decir Menos Menos Menos Menos Menos Menos Decir Izquierda Izquierda Menos Decir Derecha Derecha Derecha Derecha Mas Decir")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-Mierda "Leer Iniciar Bucle Decir Leer Terminar Bucle")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Mierda
  "Leer Decir Iniciar Bucle Menos Menos Derecha Mas Iniciar Bucle Derecha Derecha Terminar Bucle Izquierda Iniciar Bucle Decir Terminar Bucle Izquierda Izquierda Terminar Bucle")
