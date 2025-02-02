;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Bitwise Scanner", invented by the Esolang user "A" and
;; presented on March 10th, 2019, its kenspeckle proprium commorant in
;; the indagation, or "scanning", and contingent manipulation of a
;; bit-valued tape by adminiculum of bit state negations, as well as
;; conditional execution constructs, and a traversal mechanism.
;; 
;; 
;; Concept
;; =======
;; The Bitwise Scanner programming language operates on a tape of bits,
;; homologating their state's negation during a "scanning", or
;; traversal, process from the lowest towards the highest position.
;; 
;; == THE MEMORY: A TAPE OF BITS ==
;; The program memory's incarnation constitutes an adherent, in its
;; most abstract firmament, as an ordered sequence of bits, these
;; components producing a tape's edification, amenable at least to
;; inquisitions proceeding from the the least significant bit (LSB)
;; towards the most significant one (MSB).
;; 
;; The concrete designment in the ultimity of an implementation applied
;; to the tape --- whether this being finite or without bournes along
;; the upper margin --- registers a gendrure of the interpreter
;; reification itself, rather than a consectary of the Bitwise Scanner
;; language's stipulation.
;; 
;; 
;; Instructions
;; ============
;; A quintuple cardinality exercises its purview over the language's
;; operative competences, in its compass the capacity for a bit state's
;; negation, a sequential scanning of the tape, condition execution
;; ensuing from the contemporaneously select bit's value, as well as a
;; traversal cessation behest.
;; 
;; == OVERVIEW ==
;; A cursory treatise on the offered instructions' avail shall be the
;; alow apercu's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   ~              | Flips the tape's currently selected bit.
;;   ..................................................................
;;   ( statements ) | If the tape's currently selected bit equals zero
;;     **********   | (0), executes the {statements} once; otherwise
;;                  | accompasses no causatum.
;;                  |--------------------------------------------------
;;                  | {statements} must be a sequence compact of zero
;;                  | or more instructions.
;;                  |--------------------------------------------------
;;                  | If not empight in a sequential scanning routine,
;;                  | demarcated by "[...]", the current bit position
;;                  | defaults to the least significant bit (LSB).
;;                  |--------------------------------------------------
;;                  | Please heed that the modification applied by this
;;                  | operation on the tape does not propage through
;;                  | subsequent conditional statements; that is,
;;                  | consequent "(...)" and "{...}" react as if the
;;                  | bit state prevenient to this operation still
;;                  | governs the process.
;;   ..................................................................
;;   { statements } | If the tape's currently selected bit equals one
;;     **********   | (1), executes the {statements} once; otherwise
;;                  | accompasses no causatum.
;;                  |--------------------------------------------------
;;                  | {statements} must be a sequence compact of zero
;;                  | or more instructions.
;;                  |--------------------------------------------------
;;                  | If not empight in a sequential scanning routine,
;;                  | demarcated by "[...]", the current bit position
;;                  | defaults to the least significant bit (LSB).
;;                  |--------------------------------------------------
;;                  | Please heed that the modification applied by this
;;                  | operation on the tape does not propage through
;;                  | subsequent conditional statements; that is,
;;                  | consequent "(...)" and "{...}" react as if the
;;                  | bit state prevenient to this operation still
;;                  | governs the process.
;;   ..................................................................
;;   [ statements ] | Iterates the tape's positions, commencing from
;;                  | the inclusive least significant bit (LSB) and
;;                  | advancing towards the most significant one (MSB),
;;                  | during each cycle executing the {statements}.
;;                  |--------------------------------------------------
;;                  | {statements} must be a sequence compact of zero
;;                  | or more instructions.
;;   ..................................................................
;;   X              | If encountered during the a scanning block,
;;                  | demarcated by "[" and "]", immediately terminates
;;                  | thilk; otherwise accompasses no causatum.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-29
;; 
;; Sources:
;;   [esolang2023StackBit]
;;   The Esolang contributors, "StackBit", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/StackBit"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype token-type ()
  "The ``token-type'' type enumerates the anticipated variation on token
   categories encountered during a Bitwise Scanner's program's lexical
   analyzation stage."
  '(member
    :left-parenthesis
    :right-parenthesis
    :left-brace
    :right-brace
    :left-bracket
    :right-bracket
    :flip-bit
    :stop
    :eof))

;;; -------------------------------------------------------

(deftype token ()
  "The ``token'' type defines a significant object extracted from a
   piece of Bitwise Scanner source code as a composite of a
   categorizing type and a detailing value, this complex being realized
   in a cons cells, the sinistral component of whom bears the type,
   in its compernage the dextral moiety with the character value."
  '(cons token-type character))

;;; -------------------------------------------------------

(deftype opcode ()
  "The ``opcode'' type enumerates the recognized variation on Bitwise
   Scanner command types."
  '(member
    :execute-if-zero
    :execute-if-one
    :query-tape
    :flip-bit
    :stop))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type defines a Bitwise Scanner instruction as a
   bipartite composite amplecting the operation type and an optional
   argument, its reification produced from a cons cell whose sinistral
   moeity carries the species as an ``opcode'', in  the compernage of
   its operand in the dextral component."
  '(cons opcode *))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, thilk
   defaults to the generic sentinel ``*''."
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

(deftype command-list ()
  "The ``command-list'' type defines an ordered list of Bitwise Scanner
   instructions, each represented by a ``command'' object."
  '(list-of command))

;;; -------------------------------------------------------

(deftype bit-index ()
  "The ``bit-index'' type defines a bit position designator as a
   non-negative integer number without upper bourne."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism of which encompasses, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its facette as a \"generalized boolean\"
   significant, whence is produced a veridicous Boolean tantamount,
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a fresh ``token'' as a consilience of the
   categorizing TYPE and the detailing VALUE."
  (declare (type token-type type))
  (declare (type T          value))
  (the token
    (cons type value)))

;;; -------------------------------------------------------

(defun get-token-type (token)
  "Returns the TOKEN's categorizing type."
  (declare (type token token))
  (the token-type
    (car token)))

;;; -------------------------------------------------------

(defun token-has-type-of-p (candidate expected-type)
  "Determines whether the CANDIDATE token conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type token      candidate))
  (declare (type token-type expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (get-token-type candidate) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE subsumes into the diorism
   appropriated by the whitespace entities, this membership's compass
   extending across the space, horizontal tab, linefeed, and newline
   specimens, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces, and returns the position into
   the SOURCE immediately succeeding the thus traversed tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for current-position
        of-type fixnum
        from    start
        below   (length source)
      while
        (whitespace-character-p
          (char source current-position))
      finally
        (return current-position))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character located at the POSITION into the SOURCE, or,
   upon its transcendence of the SOURCE's defined bournes, responds with
   the ``NIL'' value."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexical analyzer (lexer).                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-token (source position token-type)
  "Creates a fresh token composed of the TOKEN-TYPE and the character at
   the POSITION into the SOURCE and returns two values:
     (1) A fresh token begotten by the consilience of the specified
         TOKEN-TYPE with the character at the POSITION into the SOURCE.
     (2) The index succeeding the POSITION."
  (declare (type string     source))
  (declare (type fixnum     position))
  (declare (type token-type token-type))
  (the (values token fixnum)
    (values
      (cons token-type
        (char source position))
      (1+ position))))

;;; -------------------------------------------------------

(defun get-next-token (source start)
  "Proceeding from the START position into the SOURCE, extracts the
   next token and returns two values:
     (1) The thus detected token as a ``token'' object.
     (2) The position into the SOURCE immediately succeeding the
         extracted token's parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-character (get-character-at source start)))
    (declare (type (or null character) current-character))
    (the (values token fixnum)
      (cond
        ((not (array-in-bounds-p source start))
          (values (cons :eof #\Null) start))
        
        ((whitespace-character-p current-character)
          (get-next-token source
            (skip-whitespaces source start)))
        
        ((char= current-character #\()
          (read-token source start :left-parenthesis))
        
        ((char= current-character #\))
          (read-token source start :right-parenthesis))
        
        ((char= current-character #\{)
          (read-token source start :left-brace))
        
        ((char= current-character #\})
          (read-token source start :right-brace))
        
        ((char= current-character #\[)
          (read-token source start :left-bracket))
        
        ((char= current-character #\])
          (read-token source start :right-bracket))
        
        ((char= current-character #\~)
          (read-token source start :flip-bit))
        
        ((char= current-character #\X)
          (read-token source start :stop))
        
        (T
          (error "Unrecognized character \"~c\" at position ~d."
            current-character start))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-command (type &optional (argument NIL))
  "Creates and returns a fresh ``command'' composed of the mandatory
   TYPE and an optional ARGUMENT which defaults to ``NIL''."
  (declare (type opcode type))
  (declare (type T      argument))
  (the command
    (cons type argument)))

;;; -------------------------------------------------------

(defun get-command-type (command)
  "Returns the COMMAND's categorizing opcode."
  (declare (type command command))
  (the opcode
    (car command)))

;;; -------------------------------------------------------

(defun get-command-argument (command)
  "Returns the COMMAND's optional argument, which defaults to ``NIL''."
  (declare (type command command))
  (the T
    (cdr command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (string fixnum) command-list)
                parse-command-list))

;;; -------------------------------------------------------

(defun expect-token (source start expected-type)
  "Determines whether the token detected commencing from the START
   position into the SOURCE conforms to the EXPECTED-TYPE, on
   confirmation returning the position into the SOURCE following the
   token's occupied parcel; otherwise signals an error of an unspecified
   type."
  (declare (type string     source))
  (declare (type fixnum     start))
  (declare (type token-type expected-type))
  (multiple-value-bind (next-token new-position)
      (get-next-token source start)
    (declare (type token  next-token))
    (declare (type fixnum new-position))
    (the fixnum
      (or (and (token-has-type-of-p next-token expected-type)
               new-position)
          (error "Expected a token of the type ~s, but encountered ~s."
            expected-type next-token)))))

;;; -------------------------------------------------------

(defun parse-command (source start)
  "Proceeding from the START into the SOURCE, attempts to parse a
   Bitwise Scanner command, returning two values:
     (1) If an operation could be detected, a ``command'' encapsulation
         of the instruction type and its contingent argument; otherwise
         the ``NIL'' value.
     (2) If an operation could be detected, the index into the SOURCE
         immediately succeeding the extracted command; otherwise the
         START position itself."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (next-token new-position)
      (get-next-token source start)
    (declare (type token  next-token))
    (declare (type fixnum new-position))
    (the (values (or null command) fixnum)
      (case (get-token-type next-token)
        (:left-parenthesis
          (let ((body NIL))
            (declare (type command-list body))
            (multiple-value-setq (body new-position)
              (parse-command-list source new-position))
            (values
              (make-command :execute-if-zero body)
              (expect-token source new-position :right-parenthesis))))
        (:left-brace
          (let ((body NIL))
            (declare (type command-list body))
            (multiple-value-setq (body new-position)
              (parse-command-list source new-position))
            (values
              (make-command :execute-if-one body)
              (expect-token source new-position :right-brace))))
        (:left-bracket
          (let ((body NIL))
            (declare (type command-list body))
            (multiple-value-setq (body new-position)
              (parse-command-list source new-position))
            (values
              (make-command :query-tape body)
              (expect-token source new-position :right-bracket))))
        (:flip-bit
          (values (make-command :flip-bit) new-position))
        (:stop
          (values (make-command :stop) new-position))
        (otherwise
          (values NIL start))))))

;;; -------------------------------------------------------

(defun parse-command-list (source start)
  "Proceeding from the START position into the SOURCE, parses a sequence
   of zero or more Bitwise Scanner instructions and returns two values:
     (1) An ordered ``command-list'' comprehending zero or more
         extracted Bitwise Scanner instructions.
     (2) The position into the SOURCE immediately succeeding the
         detected commands."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values command-list fixnum)
    (loop
      with current-position of-type fixnum            = start
      with next-command     of-type (or null command) = NIL
      with new-position     of-type fixnum            = 0
      do
        (setf (values next-command new-position)
          (parse-command source current-position))
      while next-command
        collect
          (prog1 next-command
            (setf current-position new-position))
        into commands
      finally
        (return
          (values commands current-position)))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses a Bitwise Scanner program from the SOURCE and returns a
   ``command-list'' representation thereof."
  (declare (type string source))
  (multiple-value-bind (commands new-position)
      (parse-command-list source 0)
    (declare (type command-list commands))
    (declare (type fixnum       new-position))
    (expect-token source new-position :eof)
    (the command-list commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flip-bit (bit)
  "Returns a state obverse to the BIT."
  (declare (type bit bit))
  (the bit
    (logxor bit 1)))

;;; -------------------------------------------------------

(define-modify-macro flip-bit-in-place ()
  flip-bit
  "Modifies the place, expected to resolve to a ``bit'' value, by
   \"flipping\", or negating, its state, and returns the subsequently
   assumed bit.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ()
  (:documentation
    "The ``Tape'' interface applies itself to the definition of an
     ordered bit sequence's castaldy."))

;;; -------------------------------------------------------

(defgeneric get-bit-at (tape index)
  (:documentation
    "Returns the value of the bit stored in the TAPE at the zero-based
     INDEX.
     ---
     If the INDEX transcends the TAPE's proper bournes, an error of the
     type ``Invalid-Bit-Index-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric flip-bit-at (tape index)
  (:documentation
    "Flip the bit stored in the TAPE at the zero-based INDEX and returns
     no value.
     ---
     If the INDEX transcends the TAPE's proper bournes, an error of the
     type ``Invalid-Bit-Index-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric set-bit-at (tape index new-state)
  (:documentation
    "Stores the NEW-STATE in the TAPE cell amenable to the zero-based
     INDEX and returns no value.
     ---
     If the INDEX transcends the TAPE's proper bournes, an error of the
     type ``Invalid-Bit-Index-Error'' is signaled."))

;;; -------------------------------------------------------

(defgeneric has-bit-at-p (tape index)
  (:documentation
    "Determines whether the INDEX constitutes a valid position into the
     TAPE, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''.
     ---
     Upon a negative response, producing a ``NIL'' value, the TAPE may
     be construed to being of a finite conformation; its scanning, as a
     corollary, enjoys the homologation of immediate cessation at this
     point or any prevenient issuing such rejection."))

;;; -------------------------------------------------------

(defgeneric print-tape (tape destination)
  (:documentation
    "Prints the TAPE's state in a form covenable for human perquisition
     to the DESTINATION, returning for a non-``NIL'' DESTINATION the
     ``NIL'' value; otherwise, for a ``NIL'' sink, responds with a fresh
     string comprehending the result."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of error type "Invalid-Bit-Index-Error".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Bit-Index-Error (error)
  ((offended-tape
    :initarg       :offended-tape
    :initform      (error "Missing offended tape.")
    :reader        invalid-bit-index-error-offended-tape
    :documentation "The tape whose inquisition into the OFFENDING-INDEX
                    served as this erroneous circumstance's etiology.")
   (offending-index
    :initarg       :offending-index
    :initform      (error "Missing offending index.")
    :reader        invalid-bit-index-error-offending-index
    :documentation "The zero-based bit position whose access for reading
                    or modification has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Bit-Index-Error condition))
      (declare (type destination             stream))
      (format stream "The bit index ~d violates the bournes of the ~
                      tape ~s."
        (invalid-bit-index-error-offending-index condition)
        (invalid-bit-index-error-offended-tape   condition))))
  (:documentation
    "The ``Invalid-Bit-Index-Error'' type serves in the apprizal about
     an anomalous situation whose etiology wones in the trial to access
     or modify a binary tape's bit by mediation of an index transcended
     its proper bournes."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Infinite-Tape".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Infinite-Tape (Tape)
  ((bits
    :initform      #b0
    :type          unsigned-byte
    :documentation "The tape cells as an integer-encoded binary
                    sequence.")
   (maximum-index
    :initform      0
    :type          bit-index
    :documentation "The inclusive largest explicitly requested or
                    modulated index into the integer-encoded BITS."))
  (:documentation
    "The ``Infinite-Tape'' class furnishes a bit-valued tape endowed
     with an infinite dispansion athwart both axes, its foundry an
     unsigned byte value encoding the binary positions."))

;;; -------------------------------------------------------

(defun make-pristine-infinite-tape ()
  "Creates and returns a fresh ``Infinite-Tape'' whose entirety of cells
   at the incipiency assume the default zero-bit state."
  (the Infinite-Tape
    (make-instance 'Infinite-Tape)))

;;; -------------------------------------------------------

(defun make-infinite-tape-of (incipial-bits)
  "Creates and returns a fresh ``Infinite-Tape'' whose initial cell
   states are desumed from the binary vector INCIPIAL-BITS.
   ---
   Please heed that the bit vector enumeration principle, as promulgated
   via the INCIPIAL-BITS, applies an assignment of bit positions to the
   elements in a form ascending proportionally by the sinistrodextral
   procession of its items. As a forbisen, the bit vector, communicated
   as a literal:
     #*011
   replicates the bit structure
     bits[0] = 0  (least significant bit (LSB))
     bits[1] = 1
     bits[2] = 1  (most  significant bit (MSB))
   This design, which allots the leftmost position to the least
   significant bit (LSB), contrasts with the commonly preferred visual
   illustration of the least significat position being empight on the
   dextral laterality, ascending in potence towards the dextral bourne,
   where the most significant bit (MSB) wones. In a juxtaposing
   elucidation, it holds:
     Bit vector:     LSB ---> MSB
     Common visuals: MSB <--- LSB"
  (declare (type bit-vector incipial-bits))
  (let ((new-tape (make-pristine-infinite-tape)))
    (declare (type Infinite-Tape new-tape))
    (with-slots (bits maximum-index) new-tape
      (declare (type unsigned-byte bits))
      (declare (type bit-index     maximum-index))
      (loop
        for current-bit   of-type bit across incipial-bits
        and current-index of-type bit-index  from 0 by 1
        do  (psetf
              (ldb (byte 1 current-index) bits) current-bit
              maximum-index                     current-index)))
    (the Infinite-Tape new-tape)))

;;; -------------------------------------------------------

(defun update-maximum-index (tape probed-index)
  "Adjusts the TAPE's internally managed maximum bit index in response
   to the PROBED-INDEX and returns no value."
  (declare (type Infinite-Tape tape))
  (declare (type bit-index     probed-index))
  (with-slots (maximum-index) tape
    (declare (type bit-index maximum-index))
    (setf maximum-index
      (max probed-index maximum-index)))
  (values))

;;; -------------------------------------------------------

(defmethod get-bit-at ((tape Infinite-Tape) (index integer))
  (declare (type Infinite-Tape tape))
  (declare (type bit-index     index))
  (update-maximum-index tape index)
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (the bit
      (or (and (logbitp index bits) 1)
          0))))

;;; -------------------------------------------------------

(defmethod set-bit-at ((tape      Infinite-Tape)
                       (index     integer)
                       (new-state integer))
  (declare (type Infinite-Tape tape))
  (declare (type bit-index     index))
  (declare (type bit           new-state))
  (update-maximum-index tape index)
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (setf (ldb (byte 1 index) bits) new-state))
  (values))

;;; -------------------------------------------------------

(defmethod flip-bit-at ((tape Infinite-Tape) (index integer))
  (declare (type Infinite-Tape tape))
  (declare (type bit-index     index))
  (update-maximum-index tape index)
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (setf (ldb (byte 1 index) bits)
      (flip-bit-in-place
        (ldb (byte 1 index) bits))))
  (values))

;;; -------------------------------------------------------

(defmethod has-bit-at-p ((tape Infinite-Tape) (index integer))
  (declare (type Infinite-Tape tape)
           (ignore             tape))
  (declare (type bit-index     index)
           (ignore             index))
  (the boolean T))

;;; -------------------------------------------------------

(defmethod print-tape ((tape Infinite-Tape) (destination T))
  (declare (type Infinite-Tape tape))
  (declare (type destination   destination))
  (the (or null string)
    (if destination
      (with-slots (bits maximum-index) tape
        (declare (type unsigned-byte bits))
        (declare (type bit-index     maximum-index))
        (loop
          for current-bit-index
            of-type (or bit-index (integer -1 -1))
            from    maximum-index
            downto  0
          do
            (format destination "~d"
              (get-bit-at tape current-bit-index))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (print-tape tape output)))))

;;; -------------------------------------------------------

(defmethod print-object ((tape Infinite-Tape) (stream T))
  (declare (type Infinite-Tape tape))
  (declare (type destination   stream))
  (print-tape tape stream))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Finite-Tape".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Finite-Tape (Tape)
  ((capacity
    :initarg       :capacity
    :initform      (error "Missing tape capacity.")
    :type          (integer 0 *)
    :documentation "The tally of cells comprising the tape.")
   (bits
    :type          simple-bit-vector
    :documentation "A vector of CAPACITY size comprehending the bits."))
  (:documentation
    "The ``Finite-Tape'' class serves in the embodiment of a bit-valued
     tape upon whose componency is imposed a stringent capacity."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Finite-Tape) &key)
  "Initializes the TAPE's bit vector from its capacity and returns no
   value."
  (declare (type Finite-Tape tape))
  (setf (slot-value tape 'bits)
    (make-array
      (slot-value tape 'capacity)
      :element-type    'bit
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL))
  (values))

;;; -------------------------------------------------------

(defun make-pristine-finite-tape (capacity)
  "Creates and returns a fresh ``Finite-Tape'' measuring the CAPACITY
   tally of bits."
  (declare (type (integer 0 *) capacity))
  (the Finite-Tape
    (make-instance 'Finite-Tape :capacity capacity)))

;;; -------------------------------------------------------

(defun make-finite-tape-of (initial-bits)
  "Creates and returns a fresh ``Finite-Tape'' whose capacity and
   incipial state are derived from the vector of INITIAL-BITS."
  (declare (type bit-vector initial-bits))
  (let ((new-tape
          (make-pristine-finite-tape
            (length initial-bits))))
    (declare (type Finite-Tape new-tape))
    (with-slots ((tape-bits bits)) new-tape
      (declare (type simple-bit-vector tape-bits))
      (map-into tape-bits #'identity initial-bits))
    (the Finite-Tape new-tape)))

;;; -------------------------------------------------------

(defmethod has-bit-at-p (tape index)
  (declare (type Finite-Tape tape))
  (declare (type bit-index   index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (slot-value tape 'bits)
        index))))

;;; -------------------------------------------------------

(defun validate-tape-index (tape probed-index)
  "Determines whether the PROBED-INDEX constitutes a valid position into
   the TAPE's underlying bit vector, returning on confirmation the
   PROBED-INDEX itself; otherwise signals an error of the type
   ``Invalid-Bit-Index-Error''."
  (declare (type Finite-Tape tape))
  (declare (type bit-index   probed-index))
  (the bit-index
    (or (and (has-bit-at-p tape probed-index)
             probed-index)
        (error 'Invalid-Bit-Index-Error
          :offended-tape   tape
          :offending-index probed-index))))

;;; -------------------------------------------------------

(defmethod get-bit-at ((tape Finite-Tape) (index integer))
  (declare (type Finite-Tape tape))
  (declare (type bit-index   index))
  (with-slots (bits) tape
    (declare (type simple-bit-vector bits))
    (the bit
      (sbit bits
        (validate-tape-index tape index)))))

;;; -------------------------------------------------------

(defmethod set-bit-at ((tape      Finite-Tape)
                       (index     integer)
                       (new-state integer))
  (declare (type Finite-Tape tape))
  (declare (type bit-index   index))
  (declare (type bit         new-state))
  (with-slots (bits) tape
    (declare (type simple-bit-vector bits))
    (setf (sbit bits (validate-tape-index tape index)) new-state))
  (values))

;;; -------------------------------------------------------

(defmethod flip-bit-at ((tape  Finite-Tape) (index integer))
  (declare (type Finite-Tape tape))
  (declare (type bit-index   index))
  (with-slots (bits) tape
    (declare (type simple-bit-vector bits))
    (flip-bit-in-place
      (sbit bits
        (validate-tape-index tape index))))
  (values))

;;; -------------------------------------------------------

(defmethod print-tape ((tape Finite-Tape) (destination T))
  (declare (type Finite-Tape tape))
  (declare (type destination destination))
  (with-slots (capacity bits) tape
    (declare (type (integer 0 *)     capacity))
    (declare (type simple-bit-vector bits))
    (loop
      for current-index
        of-type (or bit-index (integer -1 -1))
        from    (1- capacity)
        downto  0
      do
        (format destination "~d"
          (sbit bits current-index)))))

;;; -------------------------------------------------------

(defmethod print-object ((tape Finite-Tape) (stream T))
  (declare (type Finite-Tape tape))
  (declare (type destination stream))
  (print-tape tape stream))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Bit-State".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Bit-State
  (:constructor make-bit-state (tape
                                &aux (index 0)
                                     (bit   (get-bit-at tape index)))))
  "The ``Bit-State'' class serves in the ensconcement of a bit tape in
   conjunction with a memorization of its contemporaneously selected
   bit, disencumbered from all modulations applied at a certain point
   in time, usually at a code block's inchoation."
  (tape  (error "Missing tape.") :type Tape    :read-only T)
  (index 0                       :type integer :read-only NIL)
  (bit   0                       :type bit     :read-only NIL))

;;; -------------------------------------------------------

(defun update-current-bit-with-tape (bit-state)
  "Updates the BIT-STATE's memorized bit value by acquisition of its
   tape's currently selected binary value and returns no value."
  (declare (type Bit-State bit-state))
  (setf (bit-state-bit bit-state)
    (get-bit-at
      (bit-state-tape  bit-state)
      (bit-state-index bit-state)))
  (values))

;;; -------------------------------------------------------

(defun update-tape-with-current-bit (bit-state)
  "Updates the BIT-STATE tape's currently selected bit by acquisition of
   the memorized bit and returns no value."
  (declare (type Bit-State bit-state))
  (set-bit-at
    (bit-state-tape  bit-state)
    (bit-state-index bit-state)
    (bit-state-bit   bit-state))
  (values))

;;; -------------------------------------------------------

(defun set-current-bit (bit-state new-state)
  "Stores the NEW-STATE in the BIT-STATE's currently selected tape cell
   and returns no valeu."
  (declare (type Bit-State bit-state))
  (declare (type bit       new-state))
  (set-bit-at
    (bit-state-tape  bit-state)
    (bit-state-index bit-state)
    new-state)
  (values))

;;; -------------------------------------------------------

(defun flip-current-bit (bit-state)
  "Flips the state of the BIT-STATE tape's currently selected cell and
   returns no value."
  (declare (type Bit-State bit-state))
  (flip-bit-at
    (bit-state-tape  bit-state)
    (bit-state-index bit-state))
  (values))

;;; -------------------------------------------------------

(defun current-bit-is-zero-p (bit-state)
  "Determines whether the BIT-STATE's memorized bit equals zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Bit-State bit-state))
  (the boolean
    (get-boolean-value-of
      (zerop
        (bit-state-bit bit-state)))))

;;; -------------------------------------------------------

(defun reset-bit-state-index (bit-state)
  "Resets the BIT-STATE's index cursor to the minimum position of zero
   (0) and returns no value."
  (declare (type Bit-State bit-state))
  (setf (bit-state-index bit-state) 0)
  (values))

;;; -------------------------------------------------------

(defun increment-bit-state-index (bit-state)
  "Increments the BIT-STATE's index cursor to the next position and
   returns no value."
  (declare (type Bit-State bit-state))
  (incf (bit-state-index bit-state))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition type "Stop-Condition".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Stop-Condition (condition)
  ()
  (:documentation
    "The ``Stop-Condition'' condition type encapsulates the request for
     a tape scanning operation's cessation."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-command (opcode argument bit-state)
  (:documentation
    "Evaluates the command defined by its OPCODE and the ARGUMENT,
     employing the BIT-STATE's context for its causata' gendrure, and
     returns no value."))

;;; -------------------------------------------------------

(defun execute-commands (commands bit-state)
  (declare (type command-list commands))
  (declare (type Bit-State    bit-state))
  (dolist (current-command commands)
    (declare (type command current-command))
    (process-command
      (get-command-type     current-command)
      (get-command-argument current-command)
      bit-state))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((opcode    (eql :execute-if-zero))
                            (body      list)
                            (bit-state Bit-State))
    (declare (type opcode       opcode)
             (ignore            opcode))
    (declare (type command-list body))
    (declare (type Bit-State    bit-state))
    (when (current-bit-is-zero-p bit-state)
      (execute-commands body bit-state))
    (values))

;;; -------------------------------------------------------

(defmethod process-command ((opcode    (eql :execute-if-one))
                            (body      list)
                            (bit-state Bit-State))
  (declare (type opcode       opcode)
           (ignore            opcode))
  (declare (type command-list body))
  (declare (type Bit-State    bit-state))
  (unless (current-bit-is-zero-p bit-state)
    (execute-commands body bit-state))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((opcode    (eql :flip-bit))
                            (argument  T)
                            (bit-state Bit-State))
  (declare (type opcode    opcode)
           (ignore         opcode))
  (declare (type T         argument)
           (ignore         argument))
  (declare (type Bit-State bit-state))
  (flip-current-bit bit-state)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((opcode    (eql :stop))
                            (argument  T)
                            (bit-state Bit-State))
  (declare (type opcode    opcode)
           (ignore         opcode))
  (declare (type T         argument)
           (ignore         argument))
  (declare (type Bit-State bit-state)
           (ignore         bit-state))
  (signal 'Stop-Condition)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((opcode    (eql :query-tape))
                            (body      list)
                            (bit-state Bit-State))
  (declare (type opcode       opcode)
           (ignore            opcode))
  (declare (type command-list body))
  (declare (type Bit-State    bit-state))
  (let ((prevenient-tape-index (bit-state-index bit-state)))
    (declare (type bit-index prevenient-tape-index))
    (reset-bit-state-index bit-state)
    (handler-case
      (loop
        while
          (has-bit-at-p
            (bit-state-tape  bit-state)
            (bit-state-index bit-state))
        do
          (update-current-bit-with-tape bit-state)
          (execute-commands             body bit-state)
          (increment-bit-state-index    bit-state))
      (Stop-Condition ()
        NIL))
    (setf (bit-state-index bit-state) prevenient-tape-index)
    (update-current-bit-with-tape bit-state))
  (values))

;;; -------------------------------------------------------

(defun execute-program (program
                        &optional (tape (make-pristine-infinite-tape)))
  "Executes the Bitwise Scanner PROGRAM, optionally employing a specific
   TAPE as its memory, concludes the execution with a printing of the
   TAPE to the standard output, and returns no value."
  (declare (type command-list program))
  (declare (type Tape         tape))
  (let ((bit-state (make-bit-state tape)))
    (declare (type Bit-State bit-state))
    (execute-commands program bit-state)
    (update-current-bit-with-tape bit-state))
  (terpri)
  (print-tape tape T)
  (values))

;;; -------------------------------------------------------

(defun interpret-Bitwise-Scanner
    (code
     &optional (tape (make-pristine-infinite-tape)))
  "Interprets the piece of Bitwise Scanner source CODE, optionally
   employing a specific TAPE as its memory, concludes the execution with
   a printing of the TAPE to the standard output, and returns no value."
  (declare (type string code))
  (declare (type Tape   tape))
  (execute-program
    (parse-program code)
    tape)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Incrementation procedure for the bits "000":
;;   000 -> 001
(interpret-Bitwise-Scanner
  "(~){[{~}(~X)]}"
  (make-infinite-tape-of #*000))

;;; -------------------------------------------------------

;; Incrementation procedure for the bits "001":
;;   001 -> 010
(interpret-Bitwise-Scanner
  "(~){[{~}(~X)]}"
  (make-infinite-tape-of #*100))

;;; -------------------------------------------------------

;; Incrementation procedure for the bits "010":
;;   010 -> 011
(interpret-Bitwise-Scanner
  "(~){[{~}(~X)]}"
  (make-finite-tape-of #*010))

;;; -------------------------------------------------------

;; Incrementation procedure for the bits "011":
;;   011 -> 100
(interpret-Bitwise-Scanner
  "(~){[{~}(~X)]}"
  (make-finite-tape-of #*110))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "111":
;;   111 -> 110
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-infinite-tape-of #*111))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "110":
;;   110 -> 101
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-finite-tape-of #*011))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "101":
;;   101 -> 100
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-finite-tape-of #*101))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "100":
;;   100 -> 011
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-finite-tape-of #*001))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "011":
;;   011 -> 010
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-finite-tape-of #*110))

;;; -------------------------------------------------------

;; Decrementation procedure for the bits "010":
;;   010 -> 001
(interpret-Bitwise-Scanner
  "{~}([(~){~X}])"
  (make-finite-tape-of #*010))
