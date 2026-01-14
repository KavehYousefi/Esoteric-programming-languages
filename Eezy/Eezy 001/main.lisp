;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Eezy", invented by the Esolang user "ChuckEsoteric08" and
;; presented on September 5th, 2025, the woning of its caract the
;; vouchsafement of a Minsky machine's puissance via a language of eath
;; conception and implementation, thilk is edified upon two registers'
;; champarty, each such a scalar non-negative integer datum's salvatory.
;; 
;; 
;; Concept
;; =======
;; The Eezy programming language's dedication comprises such as to
;; furnish an eath warklume for the replication of a Minsky machine,
;; its firmament a jumelle of registers whose capacity amounts to a
;; sclar non-negative integer number of no natural upper bourne,
;; operated upon by a septuple accoutrement of instructions.
;; 
;; == THE MEMORY: A TWAIN OF NON-NEGATIVE INTEGER-VALUED REGISTERS ==
;; The data castaldy's accommodation tallies among a twifold register
;; set's wike, both equipotent and equipollent in their conception as a
;; scalar non-negative integer datum's commorancy.
;; 
;; At any instant, one specimen from this membership accompts for the
;; active unit, the aefauld entity entalented with an amenability for
;; perquisitions and modulations. The capacitation for motation inherent
;; in this pointer invests thilk with a homologation for its transition
;; from the active one to the compernage, and vice versa.
;; 
;; At its inchoacy entertaining the assignment of the default, and
;; minimum, state of zero (0), a register's responsiveness enhalses
;; a bourneless auctificial capacity; yet attended by a confinement in
;; the diminution's admissibility to a lower extremum of zero (0).
;; 
;; 
;; Instructions
;; ============
;; A septuple membership's purview manifests the Eezy programming
;; language's instruction set, the bailiwicks conjoined with this
;; governail's expression such as to dispand over basic arithmetics,
;; an output facility, as well as a label-based control flow duction
;; mechanism.
;; 
;; == OVERVIEW ==
;; The following tabular exposition's cynosure shall be realized in a
;; requisite mete of nortelry's adhibition concerning the language's
;; operative paraphernalia.
;; 
;; Please heed the amplection of succedaneous tmemata in a jumelle of
;; braces, "{" and "}", intended for their supersession by actual Eezy
;; code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ==================================================================
;;   REGISTER MODIFICATION
;;   ------------------------------------------------------------------
;;   +        | Increments the current register value by one (1).
;;   ..................................................................
;;   -        | If the current register value is greater than zero (0),
;;            | decrements the same by one (1); otherwise accompasses
;;            | no causatum.
;;   ..................................................................
;;   ~        | Transitions the register pointer to the next register
;;            | among the twain.
;;            |--------------------------------------------------------
;;            | If the register pointer contemporaneously references
;;            | the first member, the transition empights thilk on the
;;            | second, and vice versa.
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   .        | Prints the current register value either in its
;;            | verbatim numeric form, or as the character whose ASCII
;;            | code concurs with the current register value, depending
;;            | upon the concrete interpreter implementation.
;;   ==================================================================
;;   CONTROL FLOW NAVIGATION
;;   ------------------------------------------------------------------
;;   {index}  | Defines a label identified by the numeric {index}.
;;            |--------------------------------------------------------
;;            | The {index} must be a catena composed of one or more
;;            | attiguous asterisks ("*").
;;            |--------------------------------------------------------
;;            | If a label amenable to the {index} has already been
;;            | defined in a prevenient location of the program, an
;;            | error of the type "DuplicateLabelIndexError" is
;;            | signaled.
;;   ..................................................................
;;   0{index} | If the current register value equals zero (0),
;;            | relocates the instruction pointer (IP) to the position
;;            | of the label amenable to the {index}; otherwise
;;            | proceeds without any further causatum.
;;            |--------------------------------------------------------
;;            | The {index} must be a catena comprised of one or more
;;            | attiguous circumflex symbols ("^"), their tally
;;            | identifies the numeric label identification.
;;            |--------------------------------------------------------
;;            | If no label amenable to the {index} has been defined,
;;            | an error of the type "MissingLabelIndexError" will be
;;            | signaled.
;;   ..................................................................
;;   1{index} | If the current register value does not equal zero (0),
;;            | relocates the instruction pointer (IP) to the position
;;            | of the label amenable to the {index}; otherwise
;;            | proceeds without any further causatum.
;;            |--------------------------------------------------------
;;            | The {index} must be a catena comprised of one or more
;;            | attiguous circumflex symbols ("^"), their tally
;;            | identifies the numeric label identification.
;;            |--------------------------------------------------------
;;            | If no label amenable to the {index} has been defined,
;;            | an error of the type "MissingLabelIndexError" is
;;            | signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; Realized in the programming language Common Lisp, the interpreter at
;; hand applies itself to a parasceuastic investment, whence ensues from
;; the original Eezy source code string provenance a reformulation as a
;; vector of dedicated instruction paregals, the same impose the
;; ultimate currency of the execution tier's cynosure.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-13
;; 
;; Sources:
;;   [esolang:2025:Eezy]
;;   The Esolang contributors, "Eezy", September 5th, 2025
;;   URL: "https://esolangs.org/wiki/Eezy"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype operator ()
  "The ``operator'' type enumerates the recognized instruction
   identifiers."
  '(member
    :increment
    :decrement
    :swap
    :output
    :declare-label
    :jump-if-zero
    :jump-if-not-zero))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0)."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number greater than
   or equal to one (1)."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype operand ()
  "The ``operand'' type defines an instruction operand as either an
   unsigned integer number greater than or equal to one (1), or as the
   ``NIL'' sentinel, communicans in the argument's omission."
  '(or null positive-integer))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines an Eezy instruction as a cons cell,
   the compound's dimidiation such as to allocate to the operation
   identifier in the left moiety, represented by an ``operator'', the
   optional argument, in an ``operand'''s guise, an incolant of the
   dextral parcel."
  '(cons operator operand))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Eezy program adhering to
   its diorism as a one-dimensional simple array of ``instruction''
   objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency that enumerates zero or more entries, each key of which
   complies with the KEY-TYPE and alligates with a value of the
   VALUE-TYPE, for both governing the default configuration involving
   the generic sentinel ``*''."
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

(deftype label-registry ()
  "The ``label-registry'' type defines a mapping betwixt a positive
   label index and the zero-based position of the corresponding label
   definition instruction in the program, mediated by adminiculum of a
   hash table whose keys conform with the ``positive-integer'' species,
   answering to a ``fixnum'' position designator."
  '(hash-table-of positive-integer fixnum))

;;; -------------------------------------------------------

(deftype register-bank ()
  "The ``register-bank'' type defines the Eezy register twissel as a
   list composed of two non-negative integer numbers."
  '(cons non-negative-integer
         (cons non-negative-integer null)))

;;; -------------------------------------------------------

(deftype register-pointer ()
  "The ``register-pointer'' type defines a reference to the cons cell
   among the ``register-bank'' twissel which constitutes the currently
   selected member.
   ---
   In a concrete diction, it holds:
   
     (1) If the register pointer selects the first register, the
         reference amounts to the first cons cell, which conflates with
         the the entire two-element ``register-bank'' list's storage.
         Expressed as a type specifier:
         
           (cons non-negative-integer
                 (cons non-negative-integer null))
     
     (2) If the register pointer selects the second register, the
         reference amounts to the second cons cell, which designates the
         second node of the two-element ``register-bank'' list, that is,
         a cons cell maintaining the second element and a reference to
         the list terminator ``NIL''. Expressed as a type specifier:
         
           (cons non-negative-integer null)"
  '(or register-bank
       (cons non-negative-integer null)))

;;; -------------------------------------------------------

(deftype output-mode ()
  "The ``output-mode'' type enumerates the recognized variations on the
   format in accordance to whom output shall be issued to the standard
   output conduit."
  '(member :numeric-output :character-output))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Eezy-Error (error)
  ()
  (:documentation
    "The ``Eezy-Error'' condition type serves in the apprizal about an
     anomalous situation arising during the reception, evaluation, or
     execution of an Eezy program."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Index-Error (Eezy-Error)
  ((offending-index
    :initarg       :offending-index
    :initform      (error "No offending index has been communicated.")
    :reader        duplicate-label-index-error-offending-index
    :type          positive-integer
    :documentation "The label index whose attempted definition, maugre
                    its utilization at a prevenient location in the
                    program, has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Index-Error condition))
      (declare (type stream                      stream))
      (format stream "A label with the index ~d has already been ~
                      defined in a prevenient position."
        (duplicate-label-index-error-offending-index condition))))
  (:documentation
    "The ``Invalid-Label-Index-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is begotten by
     the attempt to define a label by an invalid index, such constitutes
     an integer number less than one (1)."))

;;; -------------------------------------------------------

(define-condition Invalid-Label-Index-Error (Eezy-Error)
  ((offending-index
    :initarg       :offending-index
    :initform      (error "No offending index has been communicated.")
    :reader        invalid-label-index-error-offending-index
    :type          integer
    :documentation "The label index whose attempted definition, maugre
                    its lack of covenableness with the positive integer
                    species, has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Label-Index-Error condition))
      (declare (type stream                    stream))
      (format stream "The value ~d constitutes an inadmissible index ~
                      for a label; such ought to represent an integer ~
                      number greater than or equal to one (1)."
        (invalid-label-index-error-offending-index condition))))
  (:documentation
    "The ``Invalid-Label-Index-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is begotten by
     the attempt to define a label by an invalid index, such constitutes
     an integer number less than one (1)."))

;;; -------------------------------------------------------

(define-condition Missing-Label-Index-Error (Eezy-Error)
  ((offending-index
    :initarg       :offending-index
    :initform      (error "No offending index has been communicated.")
    :reader        missing-label-index-error-offending-index
    :type          positive-integer
    :documentation "The label index whose request, maugre its
                    disrespondency, has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Label-Index-Error condition))
      (declare (type stream                    stream))
      (format stream "No label amenable to the index ~d could be ~
                      located."
        (missing-label-index-error-offending-index condition))))
  (:documentation
    "The ``Missing-Label-Index-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is begotten by
     the attempt to navigate to a label via an index not defined in the
     respective Eezy program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexer and parser.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source*))
(declaim (type fixnum        *current-position*))
(declaim (type character     *current-character*))
(declaim (type boolean       *more-characters-follow-p*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of Eezy source code to analyze and parse.")

(defparameter *current-position* 0
  "The current position into the ``*SOURCE*'' string.")

(define-symbol-macro *current-character*
  (the character
    (schar *source* *current-position*)))

(define-symbol-macro *more-characters-follow-p*
  (the boolean
    (not (null
      (< *current-position*
         (length *source*))))))

;;; -------------------------------------------------------

(defun current-character-equals-p (expected-character)
  "Determines whether the current character into the ``*SOURCE*'' equals
   the EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type standard-char expected-character))
  (the boolean
    (not (null
      (and *more-characters-follow-p*
           (char= *current-character* expected-character))))))

;;; -------------------------------------------------------

(defun current-character-is-a-whitespace-p ()
  "Determines whether the current ``*SOURCE*'' character represents a
   whitespace, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (and *more-characters-follow-p*
        (member *current-character*
          '(9 10 11 12 13 32) :key #'code-char :test #'char=))))))

;;; -------------------------------------------------------

(defun advance-to-the-next-character ()
  "Advances to the next character in the ``*SOURCE*'' and returns no
   value."
  (incf *current-position*)
  (values))

;;; -------------------------------------------------------

(defun skip-attiguous-whitespaces ()
  "Proceeding from the inclusive current position into the ``*SOURCE*'',
   skips a catena enumerating zero or more attiguous whitespace
   characters and returns no value."
  (loop while (current-character-is-a-whitespace-p) do
    (advance-to-the-next-character))
  (values))

;;; -------------------------------------------------------

(defun count-the-symbol (symbol-to-count)
  "Proceeding from the ``*CURRENT-POSITION*`` into the ``*SOURCE*'',
   tallies the occurrencies of the SYMBOL-TO-COUNT in immediate
   succession and returns this accompt."
  (declare (type standard-char symbol-to-count))
  (the non-negative-integer
    (loop
      while (current-character-equals-p symbol-to-count)
      do    (advance-to-the-next-character)
      count 1)))

;;; -------------------------------------------------------

(defun validate-the-label-index (index)
  "Determines whether the label INDEX represents a valid identifier,
   such subsumes into a positive integer number greater than or equal to
   one (1), returning on confirmation the unmodulated INDEX; otherwise
   signals an error of the type ``Invalid-Label-Index-Error''."
  (declare (type integer index))
  (the positive-integer
    (if (plusp index)
      index
      (error 'Invalid-Label-Index-Error :offending-index index))))

;;; -------------------------------------------------------

(defun extract-the-instructions (source)
  "Extracts from the piece of Eezy SOURCE code the ensconced
   instructions and returns a one-dimensional simple array comprehending
   thilk."
  (declare (type string source))
  (psetf
    *source*           (coerce source 'simple-string)
    *current-position* 0)
  (skip-attiguous-whitespaces)
  (the program
    (coerce
      (loop while *more-characters-follow-p* collect
        (prog1
          (case *current-character*
            (#\+
              (advance-to-the-next-character)
              (cons :increment NIL))
            (#\-
              (advance-to-the-next-character)
              (cons :decrement NIL))
            (#\~
              (advance-to-the-next-character)
              (cons :swap NIL))
            (#\.
              (advance-to-the-next-character)
              (cons :output NIL))
            (#\*
              (cons :declare-label
                (count-the-symbol #\*)))
            (#\0
              (advance-to-the-next-character)
              (cons :jump-if-zero
                (count-the-symbol #\^)))
            (#\1
              (advance-to-the-next-character)
              (cons :jump-if-not-zero
                (count-the-symbol #\^)))
            (otherwise
              (error "The character \"~c\" at the position ~d cannot be ~
                      interpreted as a command."
                *current-character* *current-position*)))
          (skip-attiguous-whitespaces)))
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label registry operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-the-program-labels (program)
  "Gathers the Eezy PROGRAM's label definitions and returns these as a
   mapping from their identifying indices to their zero-based positions
   into the PROGRAM's instruction sequence."
  (declare (type program program))
  (let ((labels (make-hash-table :test #'eql)))
    (declare (type label-registry labels))
    (loop
      for (current-operator . current-operand)
        of-type (operator operand)
        across  program
      and current-position
        of-type fixnum
        from    0
        by      1
      when (eq current-operator :declare-label) do
        (if (gethash current-operand labels)
          (error 'Duplicate-Label-Index-Error
            :offending-index current-operand)
          (setf (gethash current-operand labels)
                current-position)))
    (the label-registry labels)))

;;; -------------------------------------------------------

(defun locate-the-label (labels index)
  "Returns the zero-based index associated with the label definition
   amenable to the INDEX as consigned to the LABELS registry's castaldy;
   or, upon its disrespondency, an error of the type
   ``Missing-Label-Index-Error''."
  (declare (type label-registry   labels))
  (declare (type positive-integer index))
  (the fixnum
    (or (gethash index labels)
        (error 'Missing-Label-Index-Error :offending-index index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-eezy-program (program
                                 &key (output-mode :numeric-output))
  "Executes the Eezy PROGRAM, employing the OUTPUT-MODE for its
   communications' issuance on the standard output conduit, and returns
   no value."
  (declare (type program     program))
  (declare (type output-mode output-mode))
  (let ((ip     0)
        (labels (collect-the-program-labels program)))
    (declare (type fixnum         ip))
    (declare (type label-registry labels))
    
    (let* ((registers        (list 0 0))
           (register-pointer registers))
      (declare (type register-bank    registers))
      (declare (type register-pointer register-pointer))
      
      (symbol-macrolet
          ((current-instruction
            (the instruction
              (aref program ip)))
           (current-operator
            (the operator
              (car current-instruction)))
           (current-operand
            (the operand
              (cdr current-instruction)))
           (current-register
            (the non-negative-integer
              (car register-pointer))))
        (declare (type instruction          current-instruction))
        (declare (type non-negative-integer current-register))
        (declare (type operator             current-operator))
        (declare (type positive-integer     current-operand))
        
        (loop while (< ip (length program)) do
          (case current-operator
            (:increment
              (incf current-register))
            
            (:decrement
              (unless (zerop current-register)
                (decf current-register)))
            
            (:swap
              (setf register-pointer
                (or (cdr register-pointer)
                    registers)))
            
            (:output
              (case output-mode
                (:numeric-output
                  (format T "~&~d~%" current-register))
                (:character-output
                  (format T "~c"
                    (code-char current-register)))
                (otherwise
                  (error "The output mode ~s cannot be recognized."
                    output-mode))))
            
            (:declare-label
              NIL)
            
            (:jump-if-zero
              (when (zerop current-register)
                (setf ip
                  (locate-the-label labels current-operand))))
            
            (:jump-if-not-zero
              (unless (zerop current-register)
                (setf ip
                  (locate-the-label labels current-operand))))
            
            (otherwise
              (error "The operator ~s does not match any recognized ~
                      instruction identifier."
                current-operator)))
          
          (incf ip)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-eezy-code (code
                                &key (output-mode :numeric-output))
  "Interprets the piece of Eezy source CODE, employing the OUTPUT-MODE
   for its communications' issuance on the standard output conduit, and
   returns no value."
  (declare (type string      code))
  (declare (type output-mode output-mode))
  (execute-the-eezy-program
    (extract-the-instructions code)
    :output-mode output-mode)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate the incrementation, transition among, and decrementation
;; of the two registers.
(interpret-the-eezy-code "+++~++-~++")

;;; -------------------------------------------------------

;; Count down from inclusive ten (10) to inclusive one (1).
(interpret-the-eezy-code
  "++++++++++
   *
   .
   -
   0^^
   1^
   **"
  :output-mode :numeric-output)

;;; -------------------------------------------------------

;; Print the message "Hello, World!".
(interpret-the-eezy-code
  "
  ~+++++++++
  *
  ~++++++++
  ~-
  1^
  ~
  .
  +++++++++++++++++++++++++++++.
  +++++++..
  +++.
  -------------------------------------------------------------------.
  ------------.
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++.
  ++++++++++++++++++++++++.
  +++.
  ------.
  --------.
  -------------------------------------------------------------------.
  "
  :output-mode :character-output)
