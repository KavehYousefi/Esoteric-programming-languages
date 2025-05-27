;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "StackBit", invented by the Esolang user "ChuckEsoteric08"
;; and presented on May 21st, 2023, expressing its dioristic concept in
;; the manipulation of a stack twissel, both dedicated to the castaldy
;; of an arbitrary account of bits, while programs are capacitated to
;; helm their control flow via a jump-based navigation construct.
;; 
;; 
;; Concept
;; =======
;; The StackBit programming language constitutes a warklume for the
;; eath manipulation of bits, commorant in either of two stacks, among
;; which at any instant during the program one member poses the active
;; instance.
;; 
;; == STACKBIT: A BIT MANIPULATION LANGUAGE ==
;; StackBit's competences do not rise aboon the contingency for the
;; manipulation of bits, aided in this wike's fulfilment by a jump-based
;; goto facility.
;; 
;; == THE MEMORY: A JUMELLE OF STACKS ==
;; Two stacks, both dedicated to the maintenance of bits, partake of
;; a program's data handling.
;; 
;; Any given time, one of this jumelle is designated the active stack,
;; the sole instance entalented with an amenability to perquisitions and
;; modulations. A dedicated operation's dation capacitates the
;; alteration of the active member towards the resting compernage.
;; 
;; == MINIMIZED STACKBIT: EQUIPOLLENCE MOLDED INTO PARSIMONY ==
;; A "minimized" version of the StackBit language partakes of its
;; existency as an alternative to the original definition, its telos'
;; entelechy an amplification in the niggardliness of reserved operation
;; symbols, without a concomitant curtailment in the operative aspect
;; itself.
;; 
;; The warklume of this novel compendiousness appertains to the standard
;; instruction set's septuple membership's reduction to a quintuple
;; cardinality, realized in the aggregation of several commands into
;; a single behest entalented with a duality of epiphenomenal capacity.
;; 
;; Depending on the disderata that impose the ideated causata, the
;; ensuing minimized program may be of a dispansion reduced in its
;; mickleness or expanded therein.
;; 
;; 
;; Architecture
;; ============
;; StackBit's architectural conformation wists of a stack twissel, both
;; moieties specialized in their homologation to restrict the admission
;; of bits only, but unbridled in their capacity.
;; 
;; A trial to peek into or pop from an empty stack instigates the
;; signaling of an "EmptyStackError".
;; 
;; At any instant, merely one of the twain might be active, commencing
;; with the first specimen; a dedicated instruction permits the activity
;; flag's bartery in order to harness the twifold competences.
;; 
;; 
;; Data Types
;; ==========
;; StackBit's type system offers a singularity in its conformation,
;; bits, desumed from the integral set {0, 1}, edifying the aefauld
;; species as the data castaldy's currency.
;; 
;; 
;; Instructions
;; ============
;; While vouchsafed equipollence in their competences, the actual
;; designment of the StackBit instruction set bifurcates into the
;; standard rendition and the alternative minimized diorism, the former
;; of which tallies seven members, while the second, begotten from the
;; coalescence of heterogeneous duties, enumerates a quintuple
;; contingency, both specimens, natheless, resulting in the faculty of
;; bit manipulation and jump-based control flow navigation.
;; 
;; == OVERVIEW: STANDARD STACKBIT ==
;; The following apercu shall be vested with the dever of communicating
;; the requisite gnarity concerning the standard StackBit language's
;; operative competences.
;; 
;; Please heed that succedaneous fragments are designated by an
;; underlined designed composed of a catena of carets ("^"), and
;; intended for their supersession by actual StackBit code in the
;; ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command          | Effect
;;   -----------------+------------------------------------------------
;;   0                | Pushes the value zero (0) onto the active
;;                    | stack's top.
;;   ..................................................................
;;   1                | Pushes the value one (1) onto the active
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the active stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   *                | Pops and discards the active stack's top
;;                    | element.
;;                    |------------------------------------------------
;;                    | If the active stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   ~                | Switches to the other stack as the active
;;                    | instance.
;;   ..................................................................
;;   [ instructions ] | While the active stack's top element equals
;;     ^^^^^^^^^^^^   | one (1), executes the {instructions}.
;;                    |------------------------------------------------
;;                    | The {instructions} must be a sequence of zero
;;                    | or more commands.
;;                    |------------------------------------------------
;;                    | If the active stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   .                | Prints the active stack's top element to the
;;                    | standard output.
;;                    |------------------------------------------------
;;                    | This constitutes an optional command dedicated
;;                    | to debugging purposes.
;;                    |------------------------------------------------
;;                    | If the active stack is empty at the instant of
;;                    | this operation's invocation, no causatum is
;;                    | accompassed.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: MINIMIZED STACKBIT ==
;; The "Minimized StackBit" variant's instructions shall constitute the
;; following apercu's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Switches to the other stack as the active instance and
;;           | subsequently pushes the number zero (0) onto the active
;;           | stack's top.
;;   ..................................................................
;;   *       | Pops and discards the active stack's top element.
;;           |---------------------------------------------------------
;;           | If the active stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   [       | If the active stack's top element does not equal
;;           | one (1), relocates the instruction pointer to the
;;           | matching "|" token; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | If the active stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   |       | Designates the end of the loop started via the matching
;;           | "[" token; if the thus defined loop terminates, pushes
;;           | the number one (1) onto the active stack's top.
;;   ..................................................................
;;   .       | Prints the active stack's top element to the standard
;;           | output.
;;           |---------------------------------------------------------
;;           | This constitutes an optional command dedicated to
;;           | debugging purposes.
;;           |---------------------------------------------------------
;;           | If the active stack is empty at the instant of this
;;           | operation's invocation, no causatum is accompassed.
;;   ------------------------------------------------------------------
;; 
;; == EQUIPARATION: MINIMIZED AND STANDARD STACKBIT ==
;; The concept of "Minimized StackBit" as a compaction of the standard
;; StackBit's instruction set by a conjunction of several operations
;; into a single token serves as the provenance for a particular arsenal
;; of conspicuous symbol combinations in the derived language rendition,
;; the same shall be listed in the below equiparation:
;; 
;;   --------------------------
;;   Minimalization | Standard
;;   ---------------+----------
;;   >              | ~ 0
;;   ..........................
;;   >*             | ~
;;   ..........................
;;   >*>            | 0
;;   ..........................
;;   []             | NOP
;;   ..........................
;;   |              | ] 1
;;   ..........................
;;   |*             | ]
;;   ..........................
;;   [|             | 1
;;   --------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-05-09
;; 
;; Sources:
;;   [esolang2023StackBit]
;;   The Esolang contributors, "StackBit", December 16th, 2023
;;   URL: "https://esolangs.org/wiki/StackBit"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Exercises a conspection of the OBJECT's \"generalized boolean\"
   aspect and produces a veridicous Boolean paregal thereof, returning
   for a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-new-type (type-name (candidate-name &rest lambda-list)
                           &body body)
  "Defines a derived type whose agnomination shall be appropriated from
   the TYPE-NAME, its formal parameters constitute the ipsissima verba
   replication of the LAMBDA-LIST, and whose probed object may be
   delivered to adit under the CANDIDATE-NAME, the fathoming process
   evaluating the BODY forms, with the desinent form's primary result
   entrusted with the dever of the docimasy's consequent; a
   \"generalized boolean\" value of \"true\" serving to admit the
   candidate's compatibility with the imposed type covenant, while a
   \"false\" sentinel apprizes about its rejection.
   ---
   The first BODY form, if resolving to a string object, will be
   subjected to an interpretation as the type definition's documentation
   string, and will be appropriated for this purpose."
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

(defun specifier-matches-any-type-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER designates the generic
   sentinel ``*'', by which agency its status complies with any probed
   object, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (not (null
      (and (symbolp type-specifier)
           (eq      type-specifier '*))))))

;;; -------------------------------------------------------

(defun object-is-of-type-p (candidate expected-type)
  "Determines whether the CANDIDATE complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type T candidate))
  (declare (type T expected-type))
  (the boolean
    (get-boolean-value-of
      (or (specifier-matches-any-type-p expected-type)
          (typep                        candidate expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-new-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies to the ELEMENT-TYPE, its
   default stipulated as the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (specifier-matches-any-type-p element-type)
      (every
        #'(lambda (current-element)
            (declare (type T current-element))
            (object-is-of-type-p current-element element-type))
        (the list candidate)))))

;;; -------------------------------------------------------

(define-new-type hash-table-of (candidate
                                &optional (key-type   '*)
                                          (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which complies with the KEY-TYPE and is
   allied with a value of the VALUE-TYPE, for both holds the generic
   sentinel ``*'' as the default."
  (and
    (hash-table-p candidate)
    (or
      (and
        (specifier-matches-any-type-p key-type)
        (specifier-matches-any-type-p value-type))
      (loop
        for current-key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value current-value)
        always
          (and
            (object-is-of-type-p current-key   key-type)
            (object-is-of-type-p current-value value-type))))))

;;; -------------------------------------------------------

(deftype stackbit-instruction ()
  "The ``stackbit-instruction'' type enumerates the recognized
   variation on instructions partaking of the standard StackBit
   programming language's operative diorism."
  '(member
    :push-zero
    :push-one
    :switch-stack
    :jump-forward
    :jump-back
    :pop
    :print-top))

;;; -------------------------------------------------------

(deftype stackbit-program ()
  "The ``stackbit-program'' type defines an executable standard
   StackBit program as a one-dimensional simple array compact of zero
   or more ``stackbit-instruction'' objects."
  '(simple-array stackbit-instruction (*)))

;;; -------------------------------------------------------

(deftype jump-point-map ()
  "The ``jump-point-map'' type defines an association betwixt a
   zero-based instruction position inside of a StackBit program and a
   ``Jump-Point'' in terms of a hash table."
  '(hash-table-of fixnum Jump-Point))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which encompasses, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of StackBit program operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-stackbit-program (instructions)
  "Creates and returns a fresh ``stackbit-program'' from the list of
   INSTRUCTIONS."
  (declare (type (list-of stackbit-instruction) instructions))
  (the stackbit-program
    (coerce instructions
      '(simple-array stackbit-instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of standard StackBit parser.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-standard-stackbit-instruction (token position)
  "Parses the TOKEN, located at the POSITION into the communicated
   StackBit source code, as a standard StackBit behest and returns a
   potentially empty list of its ensconced instructions."
  (declare (type character token))
  (declare (type fixnum    position))
  (the (list-of stackbit-instruction)
    (case token
      (#\0
        (list :push-zero))
      (#\1
        (list :push-one))
      (#\~
        (list :switch-stack))
      (#\[
        (list :jump-forward))
      (#\]
        (list :jump-back))
      (#\*
        (list :pop))
      (#\.
        (list :print-top))
      ((#\Newline #\Space #\Tab)
        (list))
      (otherwise
        (error "The token \"~c\", encountered at position ~d, ~
                is invalid for a standard StackBit program."
          token position)))))

;;; -------------------------------------------------------

(defun parse-standard-stackbit-program (code)
  "Parses the piece of standard StackBit source CODE and returns a
   covenable ``stackbit-program'' representation thereof."
  (declare (type string code))
  (the stackbit-program
    (make-stackbit-program
      (loop
        for current-token    of-type character across code
        and current-position of-type fixnum    from   0 by 1
        append
          (parse-standard-stackbit-instruction
            current-token
            current-position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of minimized StackBit parser.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-minimized-stackbit-instruction (token position)
  "Parses the TOKEN, located at the POSITION into the communicated
   StackBit source code, as a minimized StackBit behest and returns a
   potentially empty list of its ensconced instructions."
  (declare (type character token))
  (declare (type fixnum    position))
  (the (list-of stackbit-instruction)
    (case token
      (#\>
        (list :switch-stack
              :push-zero))
      (#\*
        (list :pop))
      (#\[
        (list :jump-forward))
      (#\|
        (list :jump-back
              :push-one))
      (#\.
        (list :print-top))
      ((#\Newline #\Space #\Tab)
        (list))
      (otherwise
        (error "The token \"~c\", encountered at position ~d, ~
                is invalid for a minimized StackBit program."
          token position)))))

;;; -------------------------------------------------------

(defun parse-minimized-stackbit-program (code)
  "Parses the piece of minimized StackBit source CODE and returns a
   covenable ``stackbit-program'' representation thereof."
  (declare (type string code))
  (the stackbit-program
    (make-stackbit-program
      (loop
        for current-token    of-type character across code
        and current-position of-type fixnum    from   0 by 1
        append
          (parse-minimized-stackbit-instruction
            current-token
            current-position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump point.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Jump-Point
  "The ``Jump-Point'' class serves in the encapsulation of a StackBit
   jump point's pertinent data, entailing in this diorism the navigation
   direction's concrete species, the position of the destination jump
   point, as well as Boolean flag determining whether the jumelle of
   these posts define an empty iteration, destitute of any intermediate
   operation invocations."
  (type        (error "Missing jump point instruction.")
               :type      stackbit-instruction
               :read-only T)
  (destination (error "Missing jump destination.")
               :type      fixnum
               :read-only T)
  (is-empty-p  (error "Missing empty loop flag.")
               :type      boolean
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((jump-points
    :initform      (make-hash-table :test #'eql)
    :type          jump-point-map
    :documentation "Associates the zero-based jump instruction positions
                    in a StackBit program with representative
                    ``Jump-Point'' objects."))
  (:documentation
    "The ``Jump-Table'' class is apportioned the onus of a StackBit
     program's jump points' castaldy, represeting these by adminiculum
     of their zero-based positions into the instruction sequence,
     everichon among these ensconced in a ``Jump-Point'' instance."))

;;; -------------------------------------------------------

(defun prepare-empty-jump-table ()
  "Creates and returns an initially empty ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun store-jump-point (jump-table position jump-point)
  "Associates the zero-based instruction POSITION with the JUMP-POINT in
   the JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     position))
  (declare (type Jump-Point jump-point))
  (with-slots (jump-points) jump-table
    (declare (type jump-point-map jump-points))
    (setf (gethash position jump-points) jump-point))
  (values))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the START-POINT and END-POINT in bilateral fashion inside
   of the JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (let ((empty-loop-p
          (get-boolean-value-of
            (<= (- end-point start-point)
                1))))
    (declare (type boolean empty-loop-p))
    (store-jump-point jump-table start-point
      (make-jump-point
        :type        :jump-forward
        :destination end-point
        :is-empty-p  empty-loop-p))
    (store-jump-point jump-table end-point
      (make-jump-point
        :type        :jump-back
        :destination start-point
        :is-empty-p  empty-loop-p)))
  (values))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Creates and returns a fresh ``Jump-Table'' which connects the
   StackBit PROGRAM's jump points in a bidirectional manner by
   adminiculum of their zero-based indices into the supplied instruction
   list."
  (declare (type stackbit-program program))
  
  (let ((jump-table   (prepare-empty-jump-table))
        (start-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) start-points))
    
    (loop
      for current-instruction
        of-type stackbit-instruction
        across  program
      and current-position
        of-type fixnum
        from    0
        below   (length program)
      
      if (eq current-instruction :jump-forward) do
        (push current-position start-points)
      else if (eq current-instruction :jump-back) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            current-position)
          (error "Unmatched back jump point as instruction #~d."
            current-position))
      end
      
      finally
        (when start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points)
            start-points)))
    
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-point-at (jump-table position)
  "Returns the jump point associated with the zero-based POSITION in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     position))
  (the Jump-Point
    (with-slots (jump-points) jump-table
      (declare (type jump-point-map jump-points))
      (or (gethash position jump-points)
          (error "No jump point is associated with the position ~d."
            position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of bit stack.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Bit-Stack ()
  ((elements
    :initform      (make-array 0
                     :element-type    'bit
                     :initial-element 0
                     :adjustable      T
                     :fill-pointer    T)
    :accessor      bit-stack-elements
    :type          bit-vector
    :documentation "A hypothetically infinite collection of bits.")
   (next-stack
    :accessor      bit-stack-successor
    :type          Bit-Stack
    :documentation "The succeeding bit stack."))
  (:documentation
    "The ``Bit-Stack'' class furnishes an implementation of a stack
     dedicated to an arbitrary tally of bits' castaldy, edified upon a
     dynamic bit vector's manipulation."))

;;; -------------------------------------------------------

(defun prepare-empty-bit-stack ()
  "Creates and returns a fresh and initially empty ``Bit-Stack''."
  (the Bit-Stack
    (make-instance 'Bit-Stack)))

;;; -------------------------------------------------------

(defun bit-stack-is-empty-p (stack)
  "Determines whether the bit STACK is empty, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Bit-Stack stack))
  (the boolean
    (get-boolean-value-of
      (zerop
        (fill-pointer
          (bit-stack-elements stack))))))

;;; -------------------------------------------------------

(defun signal-error-if-stack-is-empty (stack)
  "Determines whether the bit STACK is empty, signaling on confirmation
   an error of the type ``Empty-Stack-Error''; otherwise returns no
   value without further epiphenomenal investment."
  (declare (type Bit-Stack stack))
  (when (bit-stack-is-empty-p stack)
    (error 'Empty-Stack-Error :offended-stack stack))
  (values))

;;; -------------------------------------------------------

(defun get-top-stack-index (stack)
  "Returns the index of the top element in the bit STACK."
  (declare (type Bit-Stack stack))
  (the fixnum
    (1-
      (fill-pointer
        (bit-stack-elements stack)))))

;;; -------------------------------------------------------

(defun push-bit (stack bit-value)
  "Pushes the BIT-VALUE onto the bit STACK and returns no value."
  (declare (type Bit-Stack stack))
  (declare (type bit       bit-value))
  (vector-push-extend bit-value
    (bit-stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun peek-bit (stack)
  "Returns without removing the STACK's top bit element; or, upon its
   vacancy, signals an error of the type ``Empty-Stack-Error''."
  (declare (type Bit-Stack stack))
  (signal-error-if-stack-is-empty stack)
  (the bit
    (with-slots (elements) stack
      (declare (type bit-vector elements))
      (bit elements
        (get-top-stack-index stack)))))

;;; -------------------------------------------------------

(defun pop-bit (stack)
  "Removes and returns the STACK's top bit element; or, upon its
   vacancy, signals an error of the type ``Empty-Stack-Error''."
  (declare (type Bit-Stack stack))
  (the bit
    (prog1
      (peek-bit stack)
      (decf
        (fill-pointer
          (bit-stack-elements stack))))))

;;; -------------------------------------------------------

(defmethod print-object ((stack Bit-Stack) (stream T))
  (declare (type Bit-Stack   stack))
  (declare (type destination stream))
  (with-slots (elements) stack
    (declare (type bit-vector elements))
    (loop
      initially
        (format stream "[Top>")
      for current-index
        of-type fixnum
        from    (get-top-stack-index stack)
        downto  0
      and has-previous-element-p
        of-type boolean
        =       T
        then    NIL
      do
        (format stream "~:[,~;~] ~d"
          has-previous-element-p
          (bit elements current-index))
      finally
        (format stream " <Bottom]"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition StackBit-Error (error)
  ()
  (:documentation
    "The ``StackBit-Error'' condition type serves as a substratum for
     all conditions pursuing the communication of anomalous situations
     arising during any stage of a StackBit program's processing."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (StackBit-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :documentation "The bit stack which at the instant of the indagation
                    or removal operation has been empty."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition)
               (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot peek into or pop from an empty stack.")))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology ensues from the trial
     to indagate or remove from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (real 0 *) *delay-in-milliseconds-after-printing*))

;;; -------------------------------------------------------

(defparameter *delay-in-milliseconds-after-printing* 0.5
  "The number of milliseconds to pause the program succeeding an
   output issued via the StackBit instruction \.\".")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Stackbit program.")
    :type          stackbit-program
    :documentation "The StackBit program to execute.")
   (jump-table
    :type          Jump-Table
    :documentation "Connects the forward and back jump points.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the PROGRAM.")
   (active-stack
    :type          Bit-Stack
    :documentation "The contemporaneously active stack, being either of
                    the FIRST-STACK or SECOND-STACK."))
  (:documentation
    "The ``Interpreter'' class is assigned the wike appertaining to a
     StackBit program's instruction list representation's governance,
     pursing the accompassing of actual efficacy to the static
     specification."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Builds and stores the jump table for the INTERPRETER's program,
   interconnects the stack twain, configures the initally active stack,
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program jump-table active-stack) interpreter
    (declare (type stackbit-program program))
    (declare (type Jump-Table       jump-table))
    (declare (type Bit-Stack        active-stack))
    (let ((first-stack  (prepare-empty-bit-stack))
          (second-stack (prepare-empty-bit-stack)))
      (declare (type Bit-Stack first-stack))
      (declare (type Bit-Stack second-stack))
      (psetf
        jump-table                         (build-jump-table program)
        (bit-stack-successor first-stack)  second-stack
        (bit-stack-successor second-stack) first-stack
        active-stack                       first-stack)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' devoted to the StackBit
   PROGRAM's execution."
  (declare (type stackbit-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun push-bit-onto-active-stack (interpreter bit-value)
  "Pushes the BIT-VALUE onto the INTERPRETER's active stack and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type bit         bit-value))
  (with-slots (active-stack) interpreter
    (declare (type Bit-Stack  active-stack))
    (push-bit active-stack bit-value))
  (values))

;;; -------------------------------------------------------

(defun peek-into-active-stack (interpreter)
  "Returns without removing the top element from the INTERPRETER's
   active stack."
  (declare (type Interpreter interpreter))
  (the bit
    (peek-bit
      (slot-value interpreter 'active-stack))))

;;; -------------------------------------------------------

(defun pop-from-active-stack (interpreter)
  "Pops the top element from the INTERPRETER's active stack and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (active-stack) interpreter
    (declare (type Bit-Stack  active-stack))
    (pop-bit active-stack))
  (values))

;;; -------------------------------------------------------

(defun active-stack-is-empty-p (interpreter)
  "Determines whether the INTERPRETER's active stack is empty, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (bit-stack-is-empty-p
      (slot-value interpreter 'active-stack))))

;;; -------------------------------------------------------

(defun switch-stack (interpreter)
  "Switches the INTERPRETER's active stack from the currently selected
   to its peer and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (active-stack) interpreter
    (declare (type Bit-Stack active-stack))
    (setf active-stack
      (bit-stack-successor active-stack)))
  (values))

;;; -------------------------------------------------------

(defun shall-execute-current-loop-p (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to currently
   reside on either a forward or back jump instruction, determines
   whether the thus established iterance compound shall be executed,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (jump-table ip active-stack) interpreter
      (declare (type Jump-Table jump-table))
      (declare (type fixnum     ip))
      (declare (type Bit-Stack  active-stack))
      (let ((jump-point (get-jump-point-at jump-table ip)))
        (declare (type Jump-point jump-point))
        (get-boolean-value-of
          (and
            (not   (jump-point-is-empty-p  jump-point))
            (plusp (peek-into-active-stack interpreter))))))))

;;; -------------------------------------------------------

(defun jump-to-opposite-loop-point (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to reside on
   either a forward or back jump instruction, relocates the same to the
   respective opposite point and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (jump-table ip) interpreter
    (declare (type Jump-Table jump-table))
    (declare (type fixnum     ip))
    (setf ip
      (jump-point-destination
        (get-jump-point-at jump-table ip))))
  (values))

;;; -------------------------------------------------------

(defun program-has-been-completed-p (interpreter)
  "Determines whether the StackBit program consigned to the
   INTERPRETER's has been executed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type stackbit-program program))
      (declare (type fixnum           ip))
      (get-boolean-value-of
        (>= ip
            (length program))))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (instruction (eql :push-zero)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (push-bit-onto-active-stack interpreter 0)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :push-one)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (push-bit-onto-active-stack interpreter 1)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :pop)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (pop-from-active-stack interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :switch-stack)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (switch-stack interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-forward)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (unless (shall-execute-current-loop-p interpreter)
      (jump-to-opposite-loop-point interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :jump-back)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (when (shall-execute-current-loop-p interpreter)
      (jump-to-opposite-loop-point interpreter))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :print-top)))
    (declare (type Interpreter          interpreter))
    (declare (type stackbit-instruction instruction)
             (ignore                    instruction))
    (if (active-stack-is-empty-p interpreter)
      (format *standard-output* "~&The active stack is empty.")
      (format *standard-output* "~&Top element: ~d"
        (peek-into-active-stack interpreter)))
    (finish-output *standard-output*)
    (sleep *delay-in-milliseconds-after-printing*)
    (values)))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the StackBit program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type stackbit-program program))
    (declare (type fixnum           ip))
    (loop until (program-has-been-completed-p interpreter) do
      (process-instruction interpreter
        (aref program ip))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-standard-stackbit (code)
  "Interprets the piece of standard StackBit source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-standard-stackbit-program code)))
  (values))

;;; -------------------------------------------------------

(defun interpret-minimized-stackbit (code)
  "Interprets the piece of minimized StackBit source CODE and returns no
   value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (parse-minimized-stackbit-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Skip an empty loop, which poses a tantamount to a no-operation (NOP).
(interpret-standard-stackbit "1[].")

;;; -------------------------------------------------------

;; "Counter" which iterates five (5) times, during each cycle printing
;; the active stack's top element.
(interpret-standard-stackbit "011111[.*]")

;;; -------------------------------------------------------

;; Minimized version of the aboon "counter" program, which iterates
;; five (5) times, during each cycle printing the active stack's top
;; element.
(interpret-minimized-stackbit ">*>
                               [|[|[|[|[|
                               [.*|")
