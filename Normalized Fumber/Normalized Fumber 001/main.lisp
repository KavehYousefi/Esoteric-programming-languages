;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Normalized Fumber", invented by the Esolang user
;; "PaxtonPenguin" and presented on May 9th, 2024, the kenspeckle
;; signum acquired by this derivative of Urban Mueller's "brainfuck"
;; ostensible in a polymechany's introduction capacitating the immediate
;; setting of a memory cell's content to a character's ASCII code.
;; 
;; 
;; Concept
;; =======
;; The Normalized Fumber programming language constitutes an augmenting
;; derivation of brainfuck by an advenient instruction's dation, thilk
;; sets the current cell's value to a subsequent character's ASCII code.
;; 
;; == DIRECT CELL SETTING ==
;; In addition to the acquainted facilities, Normalized Fumber's imparts
;; to the stock-father's competences the direct equiparation of the
;; current cell's value with a succeeding character's ASCII code.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF BYTES + A CELL POINTER ==
;; Its provenance in brainfuck, combined with a general lealty to the
;; cleronomy, Fumber appropriates the bilaterally infinite dispansion
;; of unsigned byte-valued cells, operated by a cell pointer.
;; 
;; Each cell stores a scalar byte commorant in the closed interval
;; [0, 255]; guarding these marches, a violation of the upper bourne of
;; 255 instigates a wrapping around of the state to the minimum of
;; zero (0); siclike, the lower extremum's infraction eventuates a
;; cycling motion to the maximum of 255.
;; 
;; At any instant, a mobile cell pointer is ordained to the currently
;; active unit's selection, this specimen posing as the sole possible
;; target for perquisitions and modulations. A gradual progression
;; along both of the tape's axes homologates an alteration in this role.
;; 
;; 
;; Syntax
;; ======
;; From the conspectuity's application upon the syntatical language
;; aspect, a Fumber program's conformation accommodates an ennead of
;; niladic one-symbol instruction identifers, an aefauld operation
;; whose dependency upon a successive operand is registered, as well as
;; any other content as a commentary provision.
;; 
;; == THE "%" INSTRUCTION'S CONFORMATION ==
;; The kenspeckle circumstance involving the Fumber instruction "%",
;; its form's patration depending upon a single argument, shall be
;; attended to by the following enumeration of its stipulations that
;; govern the expected datum:
;; 
;;   (1) If the argument comprises a symbol affiliated with a Normalized
;;       Fumber identifier, the same must be embraced in double
;;       quotation marks.
;;   
;;   (2) If the argument comprises a whitespace character, in whose
;;       diorism are included the space, the horizontal tab, the
;;       newline, as well as the linefed and carriage return entities,
;;       this content must be ensconced in double quotation marks.
;;   
;;   (3) Any other character may either be stated without further
;;       adornment or, upon conflation with one's personal deliberation,
;;       remain in the amplectation of double quotation marks.
;;   
;;   (4) Quoted content must comprise exactly one symbol.
;; 
;; 
;; Instructions
;; ============
;; A enneadic cardinality's account governs the Fumber programming
;; language, a preponderance of eight members a dation received from
;; its brainfuck cleronomy; augmented, however, by a direct cell state
;; modulation's efficacy.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be realized in a cursory mete of
;; nortelry's dation concerning the language's operative potentials:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right.
;;           | 
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;           | 
;;   ..................................................................
;;   +       | Increments the current cell value by one (1). If the new
;;           | state transgresses the upper bourne of 255, the value
;;           | wraps around to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1). If the new
;;           | state descends below the lower bourne of zero (0), the
;;           | value wraps around to the maximum of 255.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token; otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" token; otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   %       | Consumes the next character in the program and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | Please heed the following regulations appertaining to
;;           | the argument:
;;           |   (1) Symbols reserved for Fumber or brainfuck
;;           |       operations must be ensconced in double quotation
;;           |       marks ('"').
;;           |   (2) Whitespaces must be ensconced in double quotation
;;           |       marks ('"').
;;           |   (3) Any other content may be optionally be presented
;;           |       in a raw format or ensconed in double quotation
;;           |       marks.
;;           |   (4) Content in double quotation marks must always
;;           |       amout to exactly one character.
;;           |---------------------------------------------------------
;;           | If no character follows, an error of the type
;;           | "MissingArgumentError" is signaled.
;;           |---------------------------------------------------------
;;           | If the argument represents a symbol reserved for a
;;           | Fumber or brainfuck behest, without amplecting double
;;           | quotation marks, an error of the type
;;           | "IllegalArgumentError" is signaled.
;;           |---------------------------------------------------------
;;           | If the argument represents a whitespace character,
;;           | without amplecting double quotation marks, an error of
;;           | the type "IllegalArgumentError" is signaled.
;;           |---------------------------------------------------------
;;           | If the argument is amplected by whitespaces, but either
;;           | empty or composed of more than exactly one character, an
;;           | error of the type "IllegalArgumentError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation ensues from the efforts actuated
;; in the programming language Common Lisp, edifying a more concinnous
;; model of the committed Fumber program upon the source string as a
;; vector of dedicated instruction objects.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-17
;; 
;; Sources:
;;   [esolang2024NormalizedFumber]
;;   The Esolang contributors, "Normalized Fumber", May 9th, 2024
;;   URL: "https://esolangs.org/wiki/Normalized_Fumber"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomation is desumed from the
   TYPE-NAME, its formal parameters from the LAMBDA-LIST, and which
   assigns the stevening of the CANDIDATE-NAME to the docimasy's
   subject, while evaluating the BODY forms, with the desinent form's
   primary result construed as the compatibility's conclusion, a
   \"generalized boolean\" value of \"true\" contributing a tantamount
   to the candidate's conformance, while a rejection's apprizal ensues
   from a \"false\" value.
   ---
   The first BODY form, upon its resolution to a string object, will be
   interpreted as a documentation string to the newly created type, and
   will subsequently be reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
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

;;; -------------------------------------------------------

(defun type-specifier-is-generic-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER designates the generic sentinel
   symbol ``*'', returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   '*)
                                              (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   is founded upon zero or more entries, each key therein conforming to
   the KEY-TYPE and answering with a value of the VALUE-TYPE, for both
   governs the generic sentinel ``*'' as a default."
  (and
    (hash-table-p candidate)
    (or
      (and (type-specifier-is-generic-p key-type)
           (type-specifier-is-generic-p value-type))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and
            (or (type-specifier-is-generic-p key-type)
                (typep current-key key-type))
            (or (type-specifier-is-generic-p value-type)
                (typep current-value value-type)))))))

;;; -------------------------------------------------------

(define-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (type-specifier-is-generic-p element-type)
      (loop
        for    current-element of-type T in (the list candidate)
        always (typep current-element element-type)))))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction'' type enumerates the recognized variation on
   Fumber operations."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back
    :set-to))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, and as such constitutes a commorant of the closed
   integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a command character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (find candidate "><+-.,[]%" :test #'char=)))

;;; -------------------------------------------------------

(defun simple-argument-character-p (candidate)
  "Determines whether the CANDIDATE represents a character admissive in
   its unquoted form as an argument to the Fumber \"%\" instruction,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (and (graphic-char-p candidate)
         (not (whitespace-character-p candidate))
         (not (command-character-p    candidate))
         (not (char=                  candidate #\")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-simple-string (source)
  "Converts the SOURCE into a simple string and returns thilk."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source-code*))
(declaim (type fixnum        *source-position*))
(declaim (type character     *source-character*))
(declaim (type boolean       *source-is-exhausted-p*))

;;; -------------------------------------------------------

(defparameter *source-code*
  "The piece of Normalized Fumber source code to analyze.")

(defparameter *source-position* 0
  "The current position into the *SOURCE-CODE*.")

(define-symbol-macro *source-character*
  (the character
    (schar *source-code* *source-position*)))

(define-symbol-macro *source-is-exhausted-p*
  (the boolean
    (not
      (array-in-bounds-p *source-code* *source-position*))))

;;; -------------------------------------------------------

(defun set-source-code (new-source-code)
  "Sets the *SOURCE-CODE* to the NEW-SOURCE-CODE, resets the lexical
   analyzer state to its pristine configurations, and returns no value."
  (declare (type string new-source-code))
  (psetf
    *source-code*     (convert-to-simple-string new-source-code)
    *source-position* 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' class serves in the ensconcement of a Normalized
   Fumber instruction's requisite information, offered in a Procrustean
   guise as a general compound of a type, an optional argument, and
   location specifiers concerning the provenance source code's occupied
   tmema."
  (type           (error "Missing instruction type.")
                  :type      instruction-type
                  :read-only T)
  (argument       NIL
                  :type      (or null character)
                  :read-only T)
  (start-position (error "Missing start position in source.")
                  :type      fixnum
                  :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing program instructions.")
    :reader        program-instructions
    :type          (simple-array Instruction (*))
    :documentation "The random-access sequence of instructions."))
  (:documentation
    "The ``Program'' class models an executable Normalized Fumber
     program as a one-dimensional simple array of ``Instruction''
     objects."))

;;; -------------------------------------------------------

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' from the list of
   INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the Program
    (make-instance 'Program :instructions
      (coerce instructions
        '(simple-array Instruction (*))))))

;;; -------------------------------------------------------

(defun index-is-valid-for-program-p (program probed-index)
  "Determines whether PROBED-INDEX designates a valid position into the
   PROGRAM, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  probed-index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (program-instructions program)
        probed-index))))

;;; -------------------------------------------------------

(defun get-instruction-at (program index)
  "Returns the instruction located at the zero-based INDEX into the
   PROGRAM's instructions."
  (declare (type Program program))
  (declare (type fixnum  index))
  (the Instruction
    (aref
      (program-instructions program)
      index)))

;;; -------------------------------------------------------

(defun get-program-size (program)
  "Returns the tally of instructions comprising the PROGRAM."
  (declare (type Program program))
  (the fixnum
    (length
      (program-instructions program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Normalized-Fumber-Error (error)
  ()
  (:documentation
    "The ``Normalized-Fumber-Error'' condition type serves in the
     communication of an anomalous situation whose etiology is ligated
     to any stage of a Normalized Fumber program's evaluation."))

;;; -------------------------------------------------------

(define-condition Illegal-Argument-Error
  (Normalized-Fumber-Error simple-error)
  ()
  (:documentation
    "The ``Illegal-Argument-Error'' condition type serves in the
     communication of an anomalous situation whose etiology derives
     from the attempt to pass an argument of an inconcinnous
     conformation to the Normalized Fumber \"%\" instruction."))

;;; -------------------------------------------------------

(define-condition Missing-Argument-Error
  (Normalized-Fumber-Error simple-error)
  ()
  (:documentation
    "The ``Missing-Argument-Error'' condition type serves in the
     communication of an anomalous situation whose etiology derives
     from the attempt to invoke the Normalized Fumber \"%\" instruction
     without any succeeding argument."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-illegal-argument-error (format-control
                                      &rest format-arguments)
  "Signals an error of the type ``Invalid-Argument-Error'', employing
   the FORMAT-CONTROL, formatted using the FORMAT-ARGUMENTS."
  (declare (type string      format-control))
  (declare (type (list-of *) format-arguments))
  (error 'Illegal-Argument-Error
    :format-control   format-control
    :format-arguments format-arguments))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-quoted-argument (argument start-position end-position)
  "Assesses the validity of the quoted ARGUMENT, on its affirmation
   returning the character ensconced by the same; otherwise employs the
   START-POSITION and END-POSITION in its intent to signal an error
   of the type ``Illegal-Argument-Error''."
  (declare (type string argument))
  (declare (type fixnum start-position))
  (declare (type fixnum end-position))
  (let ((argument-length (length argument)))
    (declare (type fixnum argument-length))
    (the character
      (case argument-length
        (0
          (signal-illegal-argument-error
            "The quoted argument, commencing at the position ~d ~
             and terminating at ~d, is empty."
            start-position end-position))
        (1
          (char argument 0))
        (otherwise
          (signal-illegal-argument-error
            "The quoted argument ~s, commencing at the position ~d ~
             and terminating at ~d, contains ~d characters."
            argument argument-length start-position end-position))))))

;;; -------------------------------------------------------

(defun read-quoted-argument ()
  "Proceeding from the *SOURCE-POSITION*, consumes a quoted argument to
   the impouted previent \"%\" instruction and three values:
     (1) The zero or more characters amplected by the string's double
         quotation marks.
     (2) The start position of the reading action, which concurs with
         the *SOURCE-POSITION* at the instant of this function's
         actuation.
     (3) The position into the *SOURCE-CODE* of the closing double
         quotationg mark character."
  (let ((start-position *source-position*))
    (declare (type fixnum start-position))
    (incf *source-position*)
    (the (values string fixnum fixnum)
      (values
        (with-output-to-string (quoted-argument)
          (declare (type string-stream quoted-argument))
          (loop do
            (cond
              (*source-is-exhausted-p*
                (signal-illegal-argument-error
                  "Unterminated quoted argument which started ~
                   at the position ~d and terminated at ~d."
                  start-position *source-position*))
              ((char= *source-character* #\")
                (incf *source-position*)
                (loop-finish))
              (T
                (write-char *source-character* quoted-argument)
                (incf *source-position*)))))
        start-position
        (1- *source-position*)))))

;;; -------------------------------------------------------

(defun read-set-to-instruction ()
  "Proceeding from the *SOURCE-POSITION*, evaluates a Normalized Fumber
   \"%\" operation invocation and returns a covenable ``Instruction''
   representation thereof."
  (let ((start-position *source-position*))
    (declare (type fixnum start-position))
    (incf *source-position*)
    (the Instruction
      (cond
        (*source-is-exhausted-p*
          (error 'Missing-Argument-Error
            :format-control
              "Expected an argument to the \"%\" instruction at ~
               position ~d, but encountered the source exhausted."
            :format-arguments
              (list *source-position*)))
        
        ((simple-argument-character-p *source-character*)
          (prog1
            (make-instruction
              :type           :set-to
              :argument       *source-character*
              :start-position start-position)
            (incf *source-position*)))
        
        ((char= *source-character* #\")
          (make-instruction
            :type :set-to
            :argument
              (multiple-value-call #'evaluate-quoted-argument
                (read-quoted-argument))
            :start-position start-position))
        
        ((command-character-p *source-character*)
          (signal-illegal-argument-error
            "The command identifier \"~c\", located ~ at position ~d,
             must be quoted when used as an argument."
            *source-character* *source-position*))
        
        (T
          (signal-illegal-argument-error
            "Invalid argument character \"~c\" at position ~
             ~d. Such content must usually be quoted."
            *source-character* *source-position*))))))

;;; -------------------------------------------------------

(defun read-niladic-instruction (instruction-type)
  "Creates and returns a fresh ``Instruction'' of the INSTRUCTION-TYPE,
   while concomitantly advancing to the next position into the
   *SOURCE-CODE*."
  (declare (type instruction-type instruction-type))
  (the Instruction
    (prog1
      (make-instruction
        :type           instruction-type
        :argument       NIL
        :start-position *source-position*)
      (incf *source-position*))))

;;; -------------------------------------------------------

(defun get-next-instruction ()
  "Attempts to obtain from the current *SOURCE-CHARACTER* and contingent
   subsequent content an instruction, returning on confirmation a
   covenable ``Instruction'' representation thereof; otherwise produces
   ``NIL''."
  (the (or null Instruction)
    (case *source-character*
      (#\>       (read-niladic-instruction :move-right))
      (#\<       (read-niladic-instruction :move-left))
      (#\+       (read-niladic-instruction :increment))
      (#\-       (read-niladic-instruction :decrement))
      (#\.       (read-niladic-instruction :output))
      (#\,       (read-niladic-instruction :input))
      (#\[       (read-niladic-instruction :jump-forward))
      (#\]       (read-niladic-instruction :jump-back))
      (#\%       (read-set-to-instruction))
      (otherwise (incf *source-position*)
                 NIL))))

;;; -------------------------------------------------------

(defun parse-instructions ()
  "Extracts the instructions entailed in the *SOURCE-CODE* and returns
   a ``Program'' representation thereof."
  (the Program
    (make-program
      (loop
        until *source-is-exhausted-p*
        for next-instruction
          of-type (or null Instruction)
          =       (get-next-instruction)
        when next-instruction
          collect next-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :reader        jump-table-connections
    :type          (hash-table-of fixnum fixnum)
    :documentation "Maps the forward and jump instructions in a
                    bidirectional fashion by mediation of their
                    zero-based indices into the parsed instruction
                    sequence."))
  (:documentation
    "The ``Jump-Table'' class establishes a bidirectional association
     betwixt the jump points in a Normalized Fumber program, mediated by
     adminiculum of their zero-based indices into the same."))

;;; -------------------------------------------------------

(defun prepare-empty-jump-table ()
  "Creates and returns an initially empty ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the START-POINT and END-POINT in the JUMP-TABLE and returns
   no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point
      (jump-table-connections jump-table))
      end-point
    (gethash end-point
      (jump-table-connections jump-table))
      start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table-for (program)
  "Creates and returns a fresh ``Jump-Table'' which maps the Normalized
   Fumber PROGRAM's jump points by adminiculum of their zero-based
   indices."
  (declare (type Program program))
  (let ((jump-table          (prepare-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-instruction
        of-type Instruction
        across  (program-instructions program)
      and current-position
        of-type fixnum
        from    0
        by      1
      do
        (case (instruction-type current-instruction)
          (:jump-forward
            (push current-position forward-jump-points))
          
          (:jump-back
            (if forward-jump-points
              (connect-jump-points jump-table
                (pop forward-jump-points)
                current-position)
              (error "Unmatched back jump point at position ~d."
                (instruction-start-position current-instruction))))
          
          (otherwise
            NIL))
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            (mapcar #'instruction-start-position forward-jump-points))))
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defun get-destination-jump-point (jump-table point-of-departure)
  "Returns the jump point opposite to the POINT-OF-DEPARTURE as
   established in the JUMP-TABLE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure
          (jump-table-connections jump-table))
        (error "No destination jump point associated with the ~
                position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :reader        tape-cells
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of unsigned byte-valued cells,
                    amenable to signed integer indices, and represented
                    by a hash table whose keys concur with the cell
                    indices, while the values designate the cell
                    states.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          integer
    :documentation "A mobile cursor which selects the currently active
                    cell."))
  (:documentation
    "The ``Tape'' class implements the Normalized Fumber program memory
     as a bilaterally infinite dispansion of unsigned byte-valued cells,
     the currently amenable unit among these selected by a cell
     pointer."))

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Creates and returns a fresh ``Tape''."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the TAPE's currently selected cell's value."
  (declare (type Tape tape))
  (the octet
    (gethash
      (tape-pointer tape)
      (tape-cells   tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by its wrapping in the admissible unsigned byte
   range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf (gethash
          (tape-pointer tape)
          (tape-cells   tape)
          0)
        (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :reader        interpreter-program
    :type          Program
    :documentation "The Normalized Fumber program instructions to
                    execute.")
   (ip
    :initform      0
    :accessor      interpreter-ip
    :type          fixnum
    :documentation "The current instruction pointer (IP) position.")
   (jump-table
    :accessor      interpreter-jump-table
    :type          Jump-Table
    :documentation "Maps the jump points in the PROGRAM by adminiculum
                    of their zero-based indices into the same.")
   (tape
    :initform      (prepare-pristine-tape)
    :reader        interpreter-tape
    :type          Tape
    :documentation "The program memory as a bidirectionally infinite
                    dispansion of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class applies itself to the administration of
     actual efficacy to a Normalized Fumber program delivered in a
     sequence of instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's jump table, stores the same in the
   INTERPRETER itself, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-jump-table interpreter)
    (build-jump-table-for
      (interpreter-program interpreter)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' instance dedicated to
   the Normalized Fumber PROGRAM's execution."
  (declare (type Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Evaluates the INSTRUCTION in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (case (instruction-type instruction)
    (:move-right
      (incf
        (tape-pointer
          (interpreter-tape interpreter))))
    (:move-left
      (decf
        (tape-pointer
          (interpreter-tape interpreter))))
    (:increment
      (incf
        (current-cell-value
          (interpreter-tape interpreter))))
    (:decrement
      (decf
        (current-cell-value
          (interpreter-tape interpreter))))
    (:output
      (format T "~c"
        (code-char
          (current-cell-value
            (interpreter-tape interpreter)))))
    (:input
      (format T "~&>> ")
      (finish-output)
      (setf
        (current-cell-value
          (interpreter-tape interpreter))
        (char-code
          (read-char NIL NIL #\Null)))
      (clear-input))
    (:jump-forward
      (when (zerop
              (current-cell-value
                (interpreter-tape interpreter)))
        (setf (interpreter-ip interpreter)
          (get-destination-jump-point
            (interpreter-jump-table interpreter)
            (interpreter-ip         interpreter)))))
    (:jump-back
      (unless (zerop
                (current-cell-value
                  (interpreter-tape interpreter)))
        (setf (interpreter-ip interpreter)
          (get-destination-jump-point
            (interpreter-jump-table interpreter)
            (interpreter-ip         interpreter)))))
    (:set-to
      (setf
        (current-cell-value
          (interpreter-tape interpreter))
        (char-code
          (instruction-argument instruction))))
    (otherwise
      (error "Unrecognized instruction: ~s." instruction)))
  (values))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (min
      (1+ (interpreter-ip interpreter))
      (get-program-size
        (interpreter-program interpreter))))
  (values))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the Normalized Fumber program consigned to the
   INTERPRETER's castaldy has been consumed in its entirety, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (index-is-valid-for-program-p
        (interpreter-program interpreter)
        (interpreter-ip      interpreter)))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction located under the INTERPRETER's instruction
   pointer (IP)."
  (declare (type Interpreter interpreter))
  (the Instruction
    (get-instruction-at
      (interpreter-program interpreter)
      (interpreter-ip      interpreter))))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the INTERPRETER's current instruction, advances its
   instruction pointer (IP), if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-completed-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-instruction-pointer interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Normalized-Fumber (code)
  "Interprets the piece of Normalized Fumber source CODE and returns no
   value."
  (declare (type string code))
  (set-source-code code)
  (execute-program
    (make-interpreter
      (parse-instructions)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Normalized-Fumber
  "%H.%e.%l.%l.%o.%\",\".%\" \".%W.%o.%r.%l.%d.")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-Normalized-Fumber ",[.,]")
