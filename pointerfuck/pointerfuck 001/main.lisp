;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "pointerfuck", invented by the Esolang user "Transoptimal"
;; and presented on December 27th, 2022, the diorism of this derivative
;; from Urban Mueller's "brainfuck" wones in the translation principles
;; of its tape-like memory's cell pointer, the same employs a nuncupated
;; "call stack", an adminiculum for the cell indices' memorization and
;; restoration, and cell value dereferencing, in which context the
;; pointer desumes its new location from its currently traversed cell.
;; 
;; 
;; Concept
;; =======
;; The pointerfuck programming language's contribution enumerates the
;; appropriation of one moiety from brainfuck's competences, tallying in
;; this compass the incrementation and deduction operations, and the
;; generalized input/output conduits, with a concomitant modulation
;; applied to the control flow mechanism, and a supersession of the cell
;; pointer relocation constructs by dereferecing in conjunction with a
;; "call stack".
;; 
;; == POINTERFUCK: BRAINFUCK WITH MODULATIONS AND ENHANCEMENTS ==
;; The various junctures and divergences of appertaining to pointerfuck
;; in respect to its brainfuck cleronomy shall be the following apercu's
;; material:
;; 
;;   ------------------------------------------------------------------
;;   Aspect           | brainfuck            | pointerfuck
;;   -----------------+----------------------+-------------------------
;;   Memory layout    | Infinite along       | Infinite along positive
;;                    | positive axis.       | axis.
;;   ..................................................................
;;   Cell values      | Unsigned octets.     | Unbounded signed
;;                    |                      | integers.
;;   ..................................................................
;;   Cell arithmetics | Incrementation and   | Incrementation and
;;                    | deduction with       | deduction without
;;                    | wrapping behavior.   | natural bournes.
;;   ..................................................................
;;   Cell pointer     | Direct translations. | Dereferencing via cell
;;                    |                      | values and coefficency
;;                    |                      | with a call stack.
;;   ..................................................................
;;   Input, output    | Character-based      | Implementation-
;;                    | standard input and   | dependent, operating on
;;                    | output, usually the  | signed integers and
;;                    | console.             | contingently characters.
;;   ..................................................................
;;   Control flow     | Loop until zero.     | Loop until zero or
;;                    |                      | negative.
;;   ------------------------------------------------------------------
;; 
;; == THE MEMORY: AN INFINITE VECTOR OF SIGNED INTEGERS ==
;; pointerfuck employs a unilaterally infinite dispansion of signed
;; integer cells, enumerated with subscripts commencing from inclusive
;; zero (0), and propagating with a bourneless extent along the positive
;; axis.
;; 
;; == EVERY MEMORY CELL COMPREHENDS A SCALAR SIGNED INTEGER ==
;; Every cell, at the inicipial program moment defaulting to a zero (0)
;; state, admits gradual incrementations and deductions, wisting of no
;; natural bournes for its sign nor its magnitude.
;; 
;; == THE CELL POINTER SELECTS THE ACTIVE CELL ==
;; At any instant in the program, the currently active cell, the sole
;; instance amenable to perquisitions and modifications, is designated
;; by a mobile cell pointer, or simply "pointer". Counterdistinguished
;; from brainfuck's immediate location manipulation, the cell pointer in
;; pointerfuck obeys a champarty of two concepts: (a) the call stack and
;; (b) cell value dereferencing.
;; 
;;   (a) CALL STACK:
;;       The call stack establishes the paravaunt wherewithal for the
;;       cell pointer's castaldy, its furnishmnt that of a stack of
;;       integers, namely, the cell pointer's position at the instant of
;;       the stack's reception. Adminicular as a memory for traversed
;;       cell indices, these insertions, or "push" operations, and
;;       the equipoise accommodated in the removals, or "pop" behests,
;;       may be harnessed for the memory position's redirections.
;;   
;;   (b) CELL VALUE DEREFERENCING:
;;       A epiphenomenal transpiration occurs during the cell pointer's
;;       allocation unto the stack, as, being its parhedral action, the
;;       pointer's assumes the current cell value as its new position.
;; 
;; == POINTERFUCK ASSUMES THE CELL ARITHMETICS VERBATIM ==
;; The aefauld constituents appropriated with a conceptual and
;; syntactical ipsissima verba lealty manifest in the basic cell
;; arithmetics by the value incrementation ("+") and deduction ("-")
;; means.
;; 
;; == INPUT AND OUTPUT ARE GENERALIZED AND IMPLEMENTATION-DEPENDENT ==
;; An incipient ostention of its derivation from brainfuck's tenets, the
;; input provenance and output sink do not necessarily engage in a
;; vinculum with the respective input and output conduits, but
;; homologate a far more liberal circumference.
;; 
;; The pointerfuck standard obviates any particular imposition regarding
;; its data's reception and commission, resigning these bailiwicks to
;; the concrete implementation's proclivities, simultaneously postuling
;; the following requisites:
;; 
;;   (a) The input facility must upon each request return a signed
;;       integer number. A response involving a non-positive value shall
;;       be construed as its exhaustion, akin to an end-of-file (EOF)
;;       sentinel.
;;   
;;   (b) The output facility must upon each behest consume a signed
;;       integer. Its response, if any, ought to be a personal
;;       deliberation.
;; 
;; The language protolog redes, without a nomism's cumbrance, these
;; solutions:
;; 
;;   (a) The input facility realizes a queue of numeric character codes,
;;       the indicial response of which resolves to zero (0) for its
;;       exhaustion's apprizal.
;;   
;;   (b) The output facility prints the Unicode character whose code
;;       point concurs with the committed signed integer argument. Upon
;;       a negative value's reception, no causatum is accompassed.
;; 
;; == CONTROL FLOW IS BASED UPON THE CURRENT CELL'S POSITIVITY ==
;; A consectary yielded by pointerfuck's extension of the available
;; cell state gamut from brainfuck's unsigned byte range to the entire
;; and extensive realm governing signed integers reverberates in the
;; aefauld control structure, the forward ("[") and back jump ("]")
;; instructions, the same no longer merely respond to the dichotomoy
;; betwixt a zero (0) and positive value in the currently selected cell,
;; but rather a non-positive or positive state.
;; 
;; 
;; Instructions
;; ============
;; Siclike to its parentage's componency, pointerfuck proffers an
;; octuple instruction set's cardinality, with a twissel of brainfuck's
;; operations, the cell arithmetics, appropriates verbatim, the cell
;; pointer relocations subordinated into a novelty approaoch availing
;; themselves with a call stack and cell value dereferencing, the input
;; and output conduits experiences a further abstraction, and the
;; control flow constructs accommodated to the now more extensive signed
;; integer range.
;; 
;; == OVERVIEW ==
;; The following tabular illustration shall be entrusted with a basic
;; mete of gnarity's communication with respect to the language's
;; operational features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one.
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;   ..................................................................
;;   ,       | Queries the input facility for a signed integer number
;;           | and stores the same in the current cell.
;;   ..................................................................
;;   .       | Issues the current cell's value to the output facility.
;;   ..................................................................
;;   [       | If the current cell value is not positive, moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | Moves the instruction pointer (IP) back to the position
;;           | of the matching "[" instruction.
;;   ..................................................................
;;   @       | Pushes the cell pointer's value unto the call stack,
;;           | and subsequently sets the cell pointer to the value
;;           | stored in the current cell, thus dereferencing this
;;           | cell.
;;   ..................................................................
;;   !       | If the call stack is not empty, pops its top element and
;;           | sets the cell pointer to this value; otherwise
;;           | immediately halts the program.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-09
;; 
;; Sources:
;;   [esolang2022pointerfuck]
;;   The Esolang contributors, "pointerfuck", December 28th, 2022
;;   URL: "https://esolangs.org/wiki/Pointerfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type macros.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type utilizing the ``deftype'' infrastructure in
   coefficiency with the ``satisfies'' predicate, agnominated via the
   TYPE-NAME and endowed with the LAMBDA-LIST for its formal parameters,
   the BODY forms are capacitated to access the scrutinized objects by
   the CANDIDATE-VARIABLE name, with the desinent BODY form's primary
   value providing the result of this docimasy, where a generalized
   boolean non-``NIL'' value confirms the candidate's covenableness,
   while ``NIL'' refutes this attribute.
   ---
   If the first BODY form constitutes a string, the same is construed as
   the documentation string and reappropriated for this purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements desumed from the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
  (flet ((matches-element-type-p (element)
          "Determines whether the ELEMENT conforms to the ELEMENT-TYPE,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type T element))
          (the boolean
            (not (null
              (typep element element-type))))))
    (and
      (listp candidate)
      (every #'matches-element-type-p
        (the list candidate)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (typep key   key-type)
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional affiliation betwixt
   forward and back jump instructions, mediated by the positions inside
   of the enclosing program."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype stack-of (&optional (element-type T))
  "The ``stack'' type defines a potentially infinite stack of elements
   complying to the ELEMENT-TYPE, the same defaults to the comprehensive
   ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), without any upper bourne, and thus a
   commorant in the integral interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the pointerfuck program memory as a
   sparse vector of signed integer-valued cells, amenable to
   non-negative integer indices, and realized by adminiculum of a hash
   table, the ``non-negative-integer'' keys of which relate to the cell
   indices, whereas the ``integer'' values store the cell states."
  '(hash-table-of non-negative-integer integer))

;;; -------------------------------------------------------

(deftype call-stack ()
  "The ``call-stack'' type defines a potentially infinite stack of
   instruction pointer (IP) positions, realized as a list of unsigned
   integer numbers admitting any magnitude, the foundry of which is
   provided by a list of the ``stack-of'' species."
  '(list-of non-negative-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-jump-table ()
  "Creates and returns an initially empty jump table."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Associates the jump START-POINT and the END-POINT in the JUMP-TABLE
   and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table (code)
  "Creates and returns a new jump table nuncupated to the connection
   betwixt the forward and jump points in the piece of pointerfuck
   source code."
  (declare (type string code))
  (let ((jump-table   (make-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table        jump-table))
    (declare (type (stack-of fixnum) start-points))
    (loop
      for token    of-type character across code
      for position of-type fixnum    from   0 by 1
      if (char= token #\[) do
        (push position start-points)
      else if (char= token #\]) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            position)
          (error "Unmatched \"]\ token at position ~d." position))
      end
      finally
        (when start-points
          (error "Unmatched \"[\" token~p at position~:p ~{~d~^, ~}."
            (length start-points)
            start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table jump-point)
  "Returns the opposite end point associated with the JUMP-POINT in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     jump-point))
  (the fixnum
    (or (gethash jump-point jump-table)
        (error "No opposite jump point associated with the position ~d."
          jump-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input facility.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input ()
  ()
  (:documentation
    "The ``Input'' interface is apportioned that dever to procure upon
     an inquisition a signed integer response."))

;;; -------------------------------------------------------

(defgeneric handle-input (input)
  (:documentation
    "Queries the INPUT for a signed integer number and returns the
     response."))

;;; -------------------------------------------------------

(defclass Console-Input (Input)
  ()
  (:documentation
    "The ``Console-Input'' class implements an ``Input'' medium whose
     foundation revolves around the inquisition of the standard input,
     commonly, but not imperatively, the console, for a signed integer
     number."))

;;; -------------------------------------------------------

(defun make-console-input ()
  "Creates and returns a new ``Console-Input'' object."
  (the Console-Input
    (make-instance 'Console-Input)))

;;; -------------------------------------------------------

(defmethod handle-input ((input Console-Input))
  "Queries the standard input conduit for a signed integer number and
   returns the same, or the default value of zero (0) as an end-of-file
   (EOF) sentinel in the case of the provenance's exhaustion, ignoring
   the INPUT."
  (declare (type Console-Input input))
  (declare (ignore             input))
  (format T "~&>> ")
  (finish-output)
  (the integer
    (prog1
      (parse-integer
        (read-line NIL NIL 0))
      (clear-input))))

;;; -------------------------------------------------------

(defclass Queued-Input (Input)
  ((elements
    :initarg       :elements
    :initform      (error "Missing elements.")
    :accessor      input-elements
    :type          (list-of integer)
    :documentation "A fixed queue of elements from whose front the
                    members are removed."))
  (:documentation
    "The ``Queued-Input'' class implements an input facility based upon
     a predefined sequence of signed integer numbers, governed by the
     queue's principles, removing and returning upon each request the
     front element, but yielding in the case of its exhaustion the zero
     (0) sentinel."))

;;; -------------------------------------------------------

(defun make-queued-input-from-numbers (&optional (elements NIL))
  "Creates and returns a new ``Queued-Input'' endowed with the ELEMENTS.
   ---
   Please note that the ELEMENTS are copied, not referenced, which
   obviates a propagation of the template's modifications in the
   maintained sequence."
  (declare (type (list-of integer) elements))
  (the Queued-Input
    (make-instance 'Queued-Input
      :elements (copy-list elements))))

;;; -------------------------------------------------------

(defun make-queued-input-from-text (input-message)
  "Creates and returns a new ``Queued-Input'' whose content is derived
   from the INPUT-MESSAGE's character codes."
  (declare (type string input-message))
  (the Queued-Input
    (make-instance 'Queued-Input
      :elements (map 'list #'char-code input-message))))

;;; -------------------------------------------------------

(defmethod handle-input ((input Queued-Input))
  "Removes and returns the next element from the queued INPUT's content,
   if the same is not empty, otherwise responds with the end-of-file
   (EOF) sentinel of zero (0)."
  (declare (type Queued-Input input))
  (the integer
    (or (and (input-elements input) (pop (input-elements input)))
        0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of output facility.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Output ()
  ()
  (:documentation
    "The ``Output'' interface specifies the foundry for all classes to
     whom the telos is apportioned to accommodate an output conduit for
     printing operations, the same accepts a signed integer number and
     produces in some fashion an epiphenomenon."))

;;; -------------------------------------------------------

(defgeneric handle-output (output argument)
  (:documentation
    "Commits the signed integer ARGUMENT to the OUTPUT instance and
     returns a result connable to this combination."))

;;; -------------------------------------------------------

(defclass Console-Output (Output)
  ()
  (:documentation
    "The ``Console-Output'' class implements an ``Output'' service whose
     dever is fulfilled by the issuance of the received integer argument
     to the standard output, commonly, but not imperatively, the console
     in the form of the corresponding ASCII
     character, if possible."))

;;; -------------------------------------------------------

(defun make-console-output ()
  "Creates and returns a new ``Console-Output'' object."
  (the Console-Output
    (make-instance 'Console-Output)))

;;; -------------------------------------------------------

(defmethod handle-output ((output Console-Output) (argument integer))
  "If the ARGUMENT constitutes a non-negative integer number, prints the
   character whose Unicode code point matches the same to the standard
   output, and returns no value, ignoring the OUTPUT."
  (declare (type Console-Output output))
  (declare (ignore              output))
  (declare (type integer        argument))
  (unless (minusp argument)
    (format T "~c"
      (code-char argument)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-pointerfuck (code
                              &key (input  (make-console-input))
                                   (output (make-console-output)))
  "Interprets the piece of pointerfuck source CODE, availing itself with
   the INPUT and OUTPUT conduits for the respective interaction
   channels, and returns no value."
  (declare (type string code))
  (declare (type Input  input))
  (declare (type Output output))
  
  (let ((ip           0)
        (jump-table   (build-jump-table code))
        (memory       (make-hash-table :test #'eql))
        (cell-pointer 0)
        (call-stack   NIL))
    (declare (type fixnum               ip))
    (declare (type jump-table           jump-table))
    (declare (type call-stack           call-stack))
    (declare (type memory               memory))
    (declare (type non-negative-integer cell-pointer))
    
    (loop while (array-in-bounds-p code ip) do
      (case (char code ip)
        (#\+
          (incf (gethash cell-pointer memory 0))
          (incf ip))
        
        (#\-
          (decf (gethash cell-pointer memory 0))
          (incf ip))
        
        (#\>
          (incf cell-pointer)
          (incf ip))
        
        (#\<
          (when (plusp cell-pointer)
            (decf cell-pointer))
          (incf ip))
        
        (#\,
          (setf (gethash cell-pointer memory 0)
            (handle-input input))
          (incf ip))
        
        (#\.
          (handle-output output
            (gethash cell-pointer memory 0))
          (incf ip))
        
        (#\[
          (unless (plusp (gethash cell-pointer memory 0))
            (setf ip
              (get-jump-destination jump-table ip)))
          (incf ip))
        
        (#\]
          (setf ip
            (get-jump-destination jump-table ip)))
        
        (#\@
          (push cell-pointer call-stack)
          (let ((current-cell-value (gethash cell-pointer memory 0)))
            (declare (type integer current-cell-value))
            (cond
              ((>= current-cell-value 0)
                (setf cell-pointer current-cell-value)
                (incf ip))
              (T
                (loop-finish)))))
        
        (#\!
          (cond
            (call-stack
              (setf cell-pointer (pop call-stack))
              (incf ip))
            (T
              (loop-finish))))
        
        (otherwise
          (incf ip)))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a user input of zero (0).
(interpret-pointerfuck ",[.,]")

;;; -------------------------------------------------------

;; Repeating cat program which, utilizing a queued input conduit,
;; replicates the message "Hello, World!" by its character codes'
;; mediation.
(interpret-pointerfuck ",[.,]" :input
  (make-queued-input-from-numbers
    '(72 101 108 108 111 44 32 87 111 114 108 100 33)))

;;; -------------------------------------------------------

;; Repeating cat program which, utilizing a queued input conduit,
;; founded upon a fixed character sequence, replicates the message
;; "Hello, World!" by its character codes' mediation.
(interpret-pointerfuck ",[.,]" :input
  (make-queued-input-from-text "Hello, World!"))

;;; -------------------------------------------------------

;; Variety of a one-time cat program which accepts an integer and prints
;; the Unicode character whose code point equals twice the input value.
;; 
;; An input of 33, for example, will incite the display of the character
;; whose code point equals 66 (= 33 * 2), that is, the Latin majuscule
;; "B".
(interpret-pointerfuck "+@,[-!+@++!-@]++@.")
