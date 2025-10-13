;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ICBINB", invented by the Esolang user "ArthroStar11" and
;; presented on November 9th, 2021, its conception's fons et origo a
;; derivation and extension of Urban Mueller's brainfuck, substituting
;; the memory tape by a stack of 32-bit signed integers and, paravaunt
;; in its poietic production, a treble ramosity applying to the
;; inherited octuple instruction symbols whence each member's
;; accompassed functionality depends the currently selected program mode
;; among the underlying options' triplet.
;; 
;; 
;; Concept
;; =======
;; The ICBINB programming language bears the indicium of a brainfuck
;; derivation to whom an elevated set of capacities, as a supererogation
;; from its cleronomy's austerity, by adminiculum of a trifurcating
;; program mode, thilk assigns to member of the octuple instruction
;; tokens the contingency for an epiphenomenal triad, enjoys its
;; vouchsafement.
;; 
;; == ICBINB: [I] [C]AN'T [B]ELIEVE [I]T'S [N]OT [B]RAINFUCK ==
;; The agnomination's dation which constitution the adhibition to the
;; "ICBINB" programming language already furnishes a bewrayment of its
;; foundational entheus, upon whose firmament a magnanimous array of
;; polymechanies have been edified: "brainfuck"; and which, in an
;; aiblins antiphrastic employment of the tongue, dispands into the
;; full form "*I* *C*an't *B*elieve *I*t's *N*ot *B*rainfuck".
;; 
;; == INSTRUCTION ENCODING: A VARIABLE OF SYMBOL AND MODE ==
;; ICBINB's restriction on an octuple set of symbols, desumed from
;; brainfuck's personal donat, with a concomitant multiplication of the
;; consequent faculties, registers its provenance in the champarty
;; betwixt a symbol and the currently active program mode.
;; 
;; The mode, encoded as an integral number from the range {0, 1, 2},
;; and commencing at the execution's inchoation with the state of zero
;; (0), accompts for assignment of an actual and particular competence
;; to the processed command identifier.
;; 
;; The dedicated operation "," homologates the transition from one
;; mode to the subsequent state, upon its desinent status' departure
;; wrapping around to iterum assume the minimum of zero (0).
;; 
;; == JUMP POINT ASSOCIATIONS REMAIN INTACT ==
;; The thoroughly dynamic haecceity imbuing ICBINB's deployment of a
;; program mode conditions a wayward service to the conspectuity's
;; application on a piece of source code in this language, a predicament
;; of unfortunate emergence with special consideration of the tokens
;; "[" and "]", the same, in the mode 0, appertain to the forward and
;; back jump motions, respectively.
;; 
;; A certain ambivalence in their construct, as concerns the matching
;; procedure's hyle. In a diction entalented with concreteness and
;; syntomy, such a jump instruction, once identified by its jumelle of
;; symbol and mode, is delivered to a memorization, invested with an
;; immunity to further modulations.
;; 
;; As a forbisen, this code tmema shall be adduced:
;; 
;;   , [ , [ , , ]
;;   
;;     ^   ^     ^
;;     |   |     |
;;     |   |     +--- (C) back jump point    (mode 1)
;;     |   |
;;     |   +--------- (B) character output   (mode 2)
;;     |
;;     +------------- (A) forward jump point (mode 1)
;; 
;; The affiliation betwixt the forward jump point (A) and the matching
;; back jump point (C) remains in a memorized state. In particular, if
;; a recession from the back jump point (C) to the forward post is
;; issued, the execution remembers the "[" token at the position (A) as
;; the target, rather than (B).
;; 
;; == THE MEMORY: A STACK OF 32-BIT SIGNED INTEGERS ==
;; The onus of the data castaldy in ICBINB constitutes a parcery to an
;; aefauld stack's involvement, thilk in its tolerance is restricted to
;; admission of 32-bit signed integer numbers; their tally's accompt
;; wisting of no upper bourne in the mickleness.
;; 
;; 
;; Instructions
;; ============
;; ICBINB's instruction set enumerates 22 members, the circumference of
;; this contingency amplects arithmetic, relational, and bitwise
;; operations, stack management, as well as control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a nortelry's communication
;; relating to the operative competences in a mete concurring with
;; sufficiency, the ordonnance of which is edified upon the modes as
;; the premier axis, into which are subsumed the octuple instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   ==================================================================
;;   MODE 0: ARITHMETICS
;;   ------------------------------------------------------------------
;;   ,       | Changes to the next program mode, wrapping around to the
;;           | first one if currently empight on the desinent member.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   mode <- (mode + 1) % 3
;;   ..................................................................
;;   +       | If the stack entails less than two elements, pushes the
;;           | the number 1 onto the stack. Otherwise, if two or more
;;           | members exist on the stack, pops the top element, here
;;           | nevened "a", and the new top element, "b", from the
;;           | stack, and pushes the sum of (b + a) onto the same.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   -       | If the stack entails less than two elements, pushes the
;;           | the number -1 onto the stack. Otherwise, if two or more
;;           | members exist on the stack, pops the top element, here
;;           | nevened "a", and the new top element, "b", from the
;;           | stack, and pushes the difference of (b - a) onto the
;;           | same.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   <       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b", and pushes the product of (b * a)
;;           | onto the same.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   >       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b", and pushes the quotient of (b / a)
;;           | onto the same.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   [       | Pops the top stack element, performs a logical bit shift
;;           | in the left direction by one position on the same, and
;;           | pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a        <- pop from stack
;;           |   let shiftedA <- a << 1
;;           |   push shiftedA onto the stack
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   ]       | Pops the top stack element, performs a logical bit shift
;;           | in the right direction by one position on the same, and
;;           | pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a        <- pop from stack
;;           |   let shiftedA <- a >> 1
;;           |   push shiftedA onto the stack
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   .       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b", and pushes the remainder of
;;           | (b % a) onto the same.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ==================================================================
;;   MODE 1: CONTROL FLOW
;;   ------------------------------------------------------------------
;;   ,       | Changes to the next program mode, wrapping around to the
;;           | first one if currently empight on the desinent member.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   mode <- (mode + 1) % 3
;;   ..................................................................
;;   +       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b"; if it holds (a > b), pushes the
;;           | number 1 onto the stack, otherwise the number 0.
;;   ..................................................................
;;   -       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b"; if it holds (a < b), pushes the
;;           | number 1 onto the stack, otherwise the number 0.
;;   ..................................................................
;;   <       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b", generates a signed integer random
;;           | number "r", where
;;           |   r = floor((x * a) + b)
;;           | with x itself constitutes an aleatorily chosen
;;           | floating-point number occupying the interval [0, 1), and
;;           | pushes the random value "r" onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a <- pop from stack
;;           |   let b <- pop from stack
;;           |   let x <- random floating-point number from [0, 1)
;;           |   let r <- floor((x * a) + b)
;;           |   push r onto the stack
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   >       | Pops the top stack element and pushes two copies of the
;;           | same onto the stack, effectively duplicating the top
;;           | item.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a <- pop from stack
;;           |   push a onto the stack
;;           |   push a onto the stack
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   [       | Pops the top stack element; if it equals zero (0), moves
;;           | the instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token;
;;           | otherwise, if the element does not equal zero (0),
;;           | advances as usual.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;           |---------------------------------------------------------
;;           | If no matching back jump point could be retrieved, an
;;           | error of the type "MissingJumpPointError" will be
;;           | signaled.
;;   ..................................................................
;;   ]       | Pops the top stack element; if it does not equal zero
;;           | (0), moves the instruction pointer (IP) back to the
;;           | position immediately succeeding the matching "[" token;
;;           | otherwise, if the element equals zero (0), advances as
;;           | usual.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;           |---------------------------------------------------------
;;           | If no matching forawrd jump point could be retrieved, an
;;           | error of the type "MissingJumpPointError" will be
;;           | signaled.
;;   ..................................................................
;;   -       | Pops the top stack element, here nevened "a", and the
;;           | new top element, "b"; if it holds (a = b), pushes the
;;           | number 1 onto the stack, otherwise the number 0.
;;   ==================================================================
;;   MODE 2: INPUT/OUTPUT
;;   ------------------------------------------------------------------
;;   ,       | Changes to the next program mode, wrapping around to the
;;           | first one if currently empight on the desinent member.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   mode <- (mode + 1) % 3
;;   ..................................................................
;;   +       | Pops the top stack element, here nevened "max", queries
;;           | the standard input conduit for a string, curtails the
;;           | input to a maximum of "max" characters by culling any
;;           | contingent supernumerary positions from its tail, and
;;           | pushes the thus prepared string's characters, from left
;;           | to right, onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let max   <- pop from stack
;;           |   let input <- query standard input for a string
;;           |   
;;           |   input <- input[1..max]
;;           |   
;;           |   for each character char in input do
;;           |     push char onto stack
;;           |   end for
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   -       | Pops the top stack element, here nevened "len", and
;;           | subsequently pops a tally of "len" elements from the
;;           | stack, while concomitantly printing the character whose
;;           | ASCII code corresponds to the just removed element to
;;           | the standard output conduit.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let len <- pop from stack
;;           |   
;;           |   repeat len times do
;;           |     let asciiCode <- pop from stack
;;           |     print character with ASCII code asciiCode
;;           |   end repeat
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ..................................................................
;;   <       | Pops the top stack element and prints thilk in its
;;           | verbatim numeric form to the standard output conduit.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   >       | Queries the standard input conduit for a signed or
;;           | unsigned integer number and pushes the same onto the
;;           | stack.
;;   ..................................................................
;;   [       | Pops the top stack element and prints the character
;;           | whose ASCII code corresponds to the same to the standard
;;           | output conduit.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" will
;;           | be signaled.
;;   ..................................................................
;;   ]       | Queries the standard input conduit for a character and
;;           | pushes its ASCII code onto the stack.
;;   ..................................................................
;;   .       | Pops the top stack element, here nevened "len", and
;;           | subsequently pops a tally of "len" elements from the
;;           | stack, while concomitantly printing the just removed
;;           | element in its verbatim numeric form to the standard
;;           | standard output conduit.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let len <- pop from stack
;;           |   
;;           |   repeat len times do
;;           |     let currentNumber <- pop from stack
;;           |     print currentNumber
;;           |   end repeat
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the sufficient number
;;           | of elements for this operation, an error of the type
;;           | "EmptyStackError" will be signaled at the respective
;;           | occasion.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an exercise in the
;; programming language Common Lisp, the execution process ensuing from
;; an immediate operation on the ICBINB source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-10-04
;; 
;; Sources:
;;   [esolang2021ICBINB]
;;   The Esolang contributors, "ICBINB", November 9th, 2021
;;   URL: "https://esolangs.org/wiki/ICBINB"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list whose componency enumerates an
   arbitrary accompt of members, each such compliant with the
   ELEMENT-TYPE, thilk defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency is
   comprised of zero or more entries, for everichon of these specifies
   the KEY-TYPE the mandate species of the key, and the VALUE-TYPE the
   affiliated value's subsumption, both defaulting to the generic
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
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program-mode ()
  "The ``program-mode'' type defines the conceivable program modes as
   an integral code in the closed interval [0, 2]."
  '(integer 0 2))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirection mapping betwixt an
   ICBINB program's jump points, this castaldy's manifestation that of
   a hash table whose keys and values both represent the zero-based
   fixnum indices of the respective jump points in the program's source
   code string."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype 32-bit-integer ()
  "The ``32-bit-integer'' type defines a signed integer number whose
   componency enumerates 32 bits, thus representing an occupant of the
   closed integral interval [-2^31, (2^31)-1], which, expanded, equals
   [-2,147,483,648; 2,147,483,647]."
  '(signed-byte 32))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type defines a monadic function which accepts
   one 32-bit signed integer numbers and responds with a single value
   desumed from the same realm."
  '(function (32-bit-integer) 32-bit-integer))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type defines a dyadic function which accepts
   two 32-bit signed integer numbers and responds with a single value
   desumed from the same realm."
  '(function (32-bit-integer 32-bit-integer) 32-bit-integer))

;;; -------------------------------------------------------

(deftype numeric-predicate ()
  "The ``numeric-predicate'' type defines a relationship betwixt to
   32-bit signed integer numbers in the guise of a dyadic function whose
   two arguments are desumed from the ``32-bit-integer'' species, and
   whose sole output produces a \"generalized boolean\" value, this
   being \"true\" for the predicate's satisfaction; otherwise amounting
   to \"false\"."
  '(function (32-bit-integer 32-bit-integer) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   concerned specimens enumerate, without an exhaustive potential's
   claim, the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its facette as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Creates and returns a fresh simple string representation of the
   SOURCE."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun ensure-the-maximum-string-length (source maximum-length)
  "Ensures that the SOURCE string does not surpass the MAXIMUM-LENGTH,
   on necessity returning a fresh variant of the SOURCE curtailed to the
   MAXIMUM-LENGTH; otherwise responds with the SOURCE itself."
  (declare (type simple-string  source))
  (declare (type 32-bit-integer maximum-length))
  (the simple-string
    (if (> (length source) maximum-length)
      (subseq source 0 maximum-length)
      source)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program mode operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-the-subsequent-program-mode (current-mode)
  "Returns the program mode succeeding the CURRENT-MODE, which may be
   the first member upon the desinent state's specification."
  (declare (type program-mode current-mode))
  (the program-mode
    (mod (1+ current-mode) 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of 32-bit signed integer arithmetic.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type 32-bit-integer +MINIMUM-32-BIT-INTEGER+))
(declaim (type 32-bit-integer +MAXIMUM-32-BIT-INTEGER+))

;;; -------------------------------------------------------

(defparameter +MINIMUM-32-BIT-INTEGER+
  (- (ash 1 31))
  "The inclusive smallest integer number representable with 32 bits.")

(defparameter +MAXIMUM-32-BIT-INTEGER+
  (1- (ash 1 31))
  "The inclusive largest integer number representable with 32 bits.")

;;; -------------------------------------------------------

(defun adjust-the-number-to-the-32-bit-integer-range (number)
  "Ensures that the NUMBER does not violate the 32-bit signed integer
   range by simulating an overflow, or wrapping, into the admissible
   interval, if necessary, returning the thus adjusted value."
  (declare (type integer number))
  (the 32-bit-integer
    (if (minusp number)
      (mod number (1- +MINIMUM-32-BIT-INTEGER+))
      (mod number (1+ +MAXIMUM-32-BIT-INTEGER+)))))

;;; -------------------------------------------------------

(defun truncate-integer-to-32-bits (number)
  "Ensures the commorance of the integer NUMBER in the 32-bit signed
   integer range by a contingent removal of excessive bits along its
   most significant portion and returns the potentially adjusted
   numeric value."
  (declare (type integer number))
  (the 32-bit-integer
    (ldb (byte 32 0) number)))

;;; -------------------------------------------------------

(defun add-32-bit-integers (augend addend)
  "Adds the ADDEND to the AUGEND and returns the resulting 32-bit signed
   integer sum."
  (declare (type 32-bit-integer augend))
  (declare (type 32-bit-integer addend))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (+ augend addend))))

;;; -------------------------------------------------------

(defun subtract-32-bit-integers (minuend subtrahend)
  "Subtracts the SUBTRAHEND from the MINUEND and returns the resulting
   32-bit signed integer difference."
  (declare (type 32-bit-integer minuend))
  (declare (type 32-bit-integer subtrahend))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (- minuend subtrahend))))

;;; -------------------------------------------------------

(defun multiply-32-bit-integers (multiplicand multiplier)
  "Multiplies the MULTIPLICAND by the MULTIPLIER and returns the
   resulting 32-bit signed integer product."
  (declare (type 32-bit-integer multiplicand))
  (declare (type 32-bit-integer multiplier))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (* multiplicand multiplier))))

;;; -------------------------------------------------------

(defun divide-32-bit-integers (dividend divisor)
  "Divides the DIVIDEND by the DIVISOR and returns the resulting 32-bit
   signed integer quotient."
  (declare (type 32-bit-integer dividend))
  (declare (type 32-bit-integer divisor))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (round dividend divisor))))

;;; -------------------------------------------------------

(defun remainder-of-32-bit-integers (dividend divisor)
  "Divides the DIVIDEND by the DIVISOR and returns the resulting 32-bit
   signed integer remainder."
  (declare (type 32-bit-integer dividend))
  (declare (type 32-bit-integer divisor))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (mod dividend divisor))))

;;; -------------------------------------------------------

(defun left-shift-32-bit-integer (number)
  "Performs a leftward bit shift of the NUMBER by a single position and
   returns the resulting 32-bit signed integer number."
  (declare (type 32-bit-integer number))
  (the 32-bit-integer
    (truncate-integer-to-32-bits
      (ash number 1))))

;;; -------------------------------------------------------

(defun right-shift-32-bit-integer (number)
  "Performs a rightward bit shift of the NUMBER by a single position and
   returns the resulting 32-bit signed integer number."
  (declare (type 32-bit-integer number))
  (the 32-bit-integer
    (truncate-integer-to-32-bits
      (ash number -1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of random number generation operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-a-random-integer (range offset)
  "Generates and returns a random 32-bit signed integer number specified
   by the RANGE and the OFFSET.
   ---
   The random number generation concept proceeds from the formula
     r = floor((x * range) + offset)
   where
     x: a random floating-point number in the interval [0.0, 1.0)."
  (declare (type 32-bit-integer range))
  (declare (type 32-bit-integer offset))
  (the 32-bit-integer
    (adjust-the-number-to-the-32-bit-integer-range
      (floor
        (+ (* (random 1.0) range) offset)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer stack.                            --  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Integer-Stack
  (:constructor prepare-an-empty-integer-stack ())
  (:copier      NIL))
  "The ``Integer-Stack'' class serves in the ICBINB memory stack's
   furnishment, the same accommodates a haft to an arbitrary accompt of
   32-bit signed integer numbers, based upon a simple list."
  (elements NIL :type (list-of 32-bit-integer) :read-only NIL))

;;; -------------------------------------------------------

(defun integer-stack-is-empty-p (stack)
  "Determines whether the integer STACK is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Integer-Stack stack))
  (the boolean
    (resolve-to-a-boolean-value
      (null
        (integer-stack-elements stack)))))

;;; -------------------------------------------------------

(defun check-if-the-integer-stack-is-empty (stack)
  "Determines whether the integer STACK is empty, on confirmation
   signaling an error of the type ``Empty-Stack-Error''; otherwise
   terminates in a normal fashion without returning a value."
  (declare (type Integer-Stack stack))
  (when (integer-stack-is-empty-p stack)
    (error 'Empty-Stack-Error :offended-stack stack))
  (values))

;;; -------------------------------------------------------

(defun integer-stack-contains-less-than-two-elements-p (stack)
  "Determines whether the integer STACK contains strictly less than two
   (2) elements, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Integer-Stack stack))
  (the boolean
    (null
      (second
        (integer-stack-elements stack)))))

;;; -------------------------------------------------------

(defun push-onto-the-integer-stack (stack new-element)
  "Pushes the NEW-ELEMENT onto the integer STACK's top position and
   returns no value."
  (declare (type Integer-Stack  stack))
  (declare (type 32-bit-integer new-element))
  (push new-element
    (integer-stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun pop-one-element-from-the-integer-stack (stack)
  "Removes and returns the top element from the integer STACK.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (check-if-the-integer-stack-is-empty stack)
  (the 32-bit-integer
    (pop
      (integer-stack-elements stack))))

;;; -------------------------------------------------------

(defun pop-two-elements-from-the-integer-stack (stack)
  "Removes the top element, norned here \"a\", from the integer STACK,
   and, subsequently, the new top element, \"b\", and returns two
   values:
     (1) The erstwhile top STACK element \"a\".
     (2) The erstwhile top STACK element \"b\".
   ---
   Limned in a more illustrative fashion, the stack
   
     |  a  | <- top of stack
     |  b  |
     |  c  |
     |  d  |
     | ... |
     +-----+
      stack
   
   will be reduced to
   
     |  c  | <- top of stack
     |  d  |
     | ... |
     +-----+
      stack
   
   while yielding the two values:
   
     (a b)
   ---
   If the STACK is rendered vacant during any of the two removal
   actions, an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (the (values 32-bit-integer 32-bit-integer)
    (values
      (pop-one-element-from-the-integer-stack stack)
      (pop-one-element-from-the-integer-stack stack))))

;;; -------------------------------------------------------

(defun duplicate-the-top-element-on-the-integer-stack (stack)
  "Duplicates the top integer STACK element by removing thilk and
   subsequently pushing it twice onto the STACK, and returns no value.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (let ((top-element (pop-one-element-from-the-integer-stack stack)))
    (declare (type 32-bit-integer top-element))
    (push-onto-the-integer-stack stack top-element)
    (push-onto-the-integer-stack stack top-element))
  (values))

;;; -------------------------------------------------------

(defun the-top-element-on-the-stack-equals-zero-p (stack)
  "Removes the integer STACK's top element and determines whether the
   same equals the number zero (0), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack stack))
  (the boolean
    (resolve-to-a-boolean-value
      (zerop
        (pop-one-element-from-the-integer-stack stack)))))

;;; -------------------------------------------------------

(defun apply-a-unary-operator-to-the-integer-stack (stack operator)
  "Pops one element from the integer STACK, applies the unary OPERATOR
   to thilk, pushes the result onto the STACK, and returns no value.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack  stack))
  (declare (type unary-operator operator))
  (push-onto-the-integer-stack stack
    (funcall operator
      (pop-one-element-from-the-integer-stack stack)))
  (values))

;;; -------------------------------------------------------

(defun apply-a-binary-operator-to-the-integer-stack (stack operator)
  "Pops two elements from the integer STACK, the original top item being
   nevened \"a\", the subsequently removed \"b\", invokes the binary
   OPERATOR with \"b\" as the left (first) and \"a\" as the right
   (second) operand, pushes the result onto the STACK, and returns no
   value.
   ---
   If the STACK is rendered vacant during any of the two removal
   actions, an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack   stack))
  (declare (type binary-operator operator))
  (multiple-value-bind (right-operand left-operand)
      (pop-two-elements-from-the-integer-stack stack)
    (declare (type 32-bit-integer right-operand))
    (declare (type 32-bit-integer left-operand))
    (push-onto-the-integer-stack stack
      (funcall operator left-operand right-operand)))
  (values))

;;; -------------------------------------------------------

(defun the-top-integer-stack-elements-satisfy-p (stack predicate)
  "Pops two elements from the integer STACK, the original top item being
   nevened \"a\", the subsequently removed \"b\", invokes the PREDICATE
   with \"a\" as the left (first) and \"b\" as the right (second)
   operand, and returns a ``boolean'' value of ``T'' upon the
   PREDICATE's satisfaction, otherwise ``NIL''.
   ---
   If the STACK is rendered vacant during any of the two removal
   actions, an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack     stack))
  (declare (type numeric-predicate predicate))
  (the boolean
    (resolve-to-a-boolean-value
      (multiple-value-bind (left-operand right-operand)
          (pop-two-elements-from-the-integer-stack stack)
        (declare (type 32-bit-integer left-operand))
        (declare (type 32-bit-integer right-operand))
        (funcall predicate left-operand right-operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-a-jump-table-for (source)
  "Creates and returns a fresh ``jump-table'' whose wike shall be the
   castaldy and alligation of the jump points extracted from the piece
   of ICBINB SOURCE code."
  (declare (type simple-string source))
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL)
        (mode                0))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (declare (type program-mode     mode))
    (symbol-macrolet
        ((mode-1-is-active-p
          (the boolean
            (resolve-to-a-boolean-value
              (= mode 1)))))
      (declare (type boolean mode-1-is-active-p))
      (loop
        for current-symbol   of-type character across source
        and current-position of-type fixnum    from 0 by 1
        do
          (case current-symbol
            (#\,
              (setf mode
                (get-the-subsequent-program-mode mode)))
            (#\[
              (when mode-1-is-active-p
                (push current-position forward-jump-points)))
            (#\]
              (when mode-1-is-active-p
                (if forward-jump-points
                  (let ((forward-jump-point (pop forward-jump-points))
                        (back-jump-point    current-position))
                    (declare (type fixnum forward-jump-point))
                    (declare (type fixnum back-jump-point))
                    (psetf
                      (gethash forward-jump-point jump-table)
                        back-jump-point
                      (gethash back-jump-point    jump-table)
                        forward-jump-point))
                  (error 'Missing-Jump-Point-Error
                    :program         source
                    :origin-position current-position
                    :origin-token    #\]))))
            (otherwise
              NIL))))
    (when forward-jump-points
      (error 'Missing-Jump-Point-Error
        :program         source
        :origin-position (pop forward-jump-points)
        :origin-token    #\[))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun locate-the-matching-back-jump-point (source
                                            jump-table
                                            start-point)
  "Returns the back jump point (\"]\") reached from the forward jump
   START-POINT (\"[\") in the ICBINB program's SOURCE, being among those
   connections governed by JUMP-TABLE.
   ---
   If no such association partakes of an existency, an error of the type
   ``Missing-Jump-Point-Error'' will be signaled."
  (declare (type simple-string source))
  (declare (type jump-table    jump-table))
  (declare (type fixnum        start-point))
  (the fixnum
    (or (gethash start-point jump-table)
        (error 'Missing-Jump-Point-Error
                 :program         source
                 :origin-position start-point
                 :origin-token    #\[))))

;;; -------------------------------------------------------

(defun locate-the-matching-forward-jump-point (source
                                               jump-table
                                               start-point)
  "Returns the forward jump point (\"[\") reached from the back jump
   START-POINT (\"]\") in the ICBINB program's SOURCE, being among those
   connections governed by JUMP-TABLE.
   ---
   If no such association partakes of an existency, an error of the type
   ``Missing-Jump-Point-Error'' will be signaled."
  (declare (type simple-string source))
  (declare (type jump-table    jump-table))
  (declare (type fixnum        start-point))
  (the fixnum
    (or (gethash start-point jump-table)
        (error 'Missing-Jump-Point-Error
                 :program         source
                 :origin-position start-point
                 :origin-token    #\]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-a-string (maximum-length)
  "Queries the standard input conduit for a string whose first
   MAXIMUM-LENGTH of characters will be retained and returns the same."
  (declare (type 32-bit-integer maximum-length))
  (format *query-io* "~&Please enter a string of at most ~d ~
                        characters length: "
    maximum-length)
  (finish-output *query-io*)
  (the simple-string
    (ensure-the-maximum-string-length
      (prog1
        (convert-into-a-simple-string
          (read-line *query-io* NIL ""))
        (clear-input *query-io*))
      maximum-length)))

;;; -------------------------------------------------------

(defun print-a-string-of-the-length (stack length)
  "Pops the LENGTH tally of elements from the integer STACK, upon each
   such item's removal printing the character whose ASCII code
   corresponds to the numeric value to the standard output conduit, and
   returns no value.
   ---
   If the STACK is rendered vacant during any of the removal actions,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack  stack))
  (declare (type 32-bit-integer length))
  (loop repeat length do
    (format *query-io* "~c"
      (code-char
        (pop-one-element-from-the-integer-stack stack))))
  (values))

;;; -------------------------------------------------------

(defun push-a-string-onto-the-integer-stack (stack string)
  "Pushes the STRING's characters, proceeding from the first to the last
   member thereof, onto the integer STACK, utilizing a conversion from
   the entities to their ASCII codes as a prevenient measure, and
   returns no value.
   ---
   Please heed that, as a consectary of the last-in first-out principle
   commorant in the stack abstract data type, the inserted string's
   character sequence will be effectively reversed, the desinent
   character being aligned at the STACK's new top."
  (declare (type Integer-Stack stack))
  (declare (type simple-string string))
  (loop for current-character of-type character across string do
    (push-onto-the-integer-stack stack
      (char-code current-character)))
  (values))

;;; -------------------------------------------------------

(defun query-for-an-integer ()
  "Queries the standard input conduit for an integer number and returns
   a 32-bit signed integer representation thereof."
  (let ((raw-input    "")
        (parsed-input NIL))
    (declare (type string            raw-input))
    (declare (type (or null integer) parsed-input))
    (loop do
      (format        *query-io* "~&Please enter an integer number: ")
      (finish-output *query-io*)
      (setf raw-input
        (read-line *query-io* NIL ""))
      (setf parsed-input
        (ignore-errors
          (parse-integer raw-input)))
      (if parsed-input
        (loop-finish)
        (format *query-io* "~&Invalid input.")))
    (the 32-bit-integer
      (adjust-the-number-to-the-32-bit-integer-range parsed-input))))

;;; -------------------------------------------------------

(defun print-an-integer-list-of-the-length (stack length)
  "Pops the LENGTH tally of elements from the integer STACK, upon each
   such item's removal printing the same in its verbatim numeric form
   to the standard output conduit, and returns no value.
   ---
   If the STACK is rendered vacant during any of the removal actions,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Integer-Stack  stack))
  (declare (type 32-bit-integer length))
  (loop repeat length do
    (format *query-io* "~&~d~%"
      (pop-one-element-from-the-integer-stack stack)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the condition types.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ICBINB-Error (error)
  ()
  (:documentation
    "The ``ICBINB-Error'' condition type serves as a firmament afforded
     to all condition types whose dever shall be realized in the
     apprizal about anomalous situations occurring during a piece of
     ICBINB source code's obtention, evaluation, or execution."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (ICBINB-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :type          Integer-Stack
    :documentation "The stack which, its status being that of a complete
                    vacancy, was requested to remove its top element."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot pop from an empty stack.")))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology emerges from the
     attempt to remove an element from an empty stack."))

;;; -------------------------------------------------------

(define-condition Missing-Jump-Point-Error (ICBINB-Error)
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        missing-jump-point-error-program
    :type          simple-string
    :documentation "The piece of ICBINB source code inside whose compass
                    the mismatch has occurred.")
   (origin-position
    :initarg       :origin-position
    :initform      (error "Missing origin position.")
    :type          fixnum
    :reader        missing-jump-point-error-origin-position
    :documentation "The zero-based index of the point of departure
                    whence the missing destination jump point's search
                    has been instigated.")
   (origin-token
    :initarg       :origin-token
    :initform      (error "Missing origin token.")
    :reader        missing-jump-point-error-origin-token
    :documentation "The symbol representing the jump instruction serving
                    as the point of departure whence the missing
                    destination jump point's search has been
                    instigated."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Jump-Point-Error condition))
      (declare (type destination              stream))
      (format stream "No matching jump point has been detected for ~
                      the instruction \"~c\", located at the position ~
                      ~d in the ICBINB program ~s."
        (missing-jump-point-error-origin-token    condition)
        (missing-jump-point-error-origin-position condition)
        (missing-jump-point-error-program         condition))))
  (:documentation
    "The ``Missing-Jump-Point-Error'' condition type serves in the
     apprizal about an abortive attempt to detect a destination jump
     points for either a forward or back jump instruction."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the "Interpreter" class.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "No ICBINB program has been assigned to ~
                           the interpreter.")
    :type          simple-string
    :documentation "The ICBINB source code to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the PROGRAM string.")
   (jump-table
    :type          jump-table
    :documentation "Connects the forward and back jump points in the
                    PROGRAM in a bidirection manner.")
   (mode
    :initform      0
    :type          program-mode
    :documentation "The currently active program mode as an integral
                    code in the closed interval [0, 2].")
   (stack
    :initform      (prepare-an-empty-integer-stack)
    :type          Integer-Stack
    :documentation "The program memory as a stack of 32-bit signed
                    integer numbers."))
  (:documentation
    "The ``Interpreter'' class is that wike parcery's recipient which
     entrusts thilk with the embuing of a piece of ICBINB source code
     with actual efficacy."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots to local symbol macros
   whose agnomination ensues from an eponymy with a prefixion of the
   \"$\" symbol, evaluates the BODY forms, and returns the desinent
   form's results.
   ---
   The following local symbol macrolet definitions' establishments
   exhaust the furnishment and correlate to the respective INTERPRETER
   slot names, complemented by their purpose:
     ------------------------------------------------------------------
     Symbol macro | Slot       | Role
     -------------+------------+---------------------------------------
     $program     | program    | The ICBINB source string to execute.
     ..................................................................
     $ip          | ip         | The instruction pointer position.
     ..................................................................
     $jump-table  | jump-table | The associated jump points.
     ..................................................................
     $mode        | mode       | The current program mode.
     ..................................................................
     $stack       | stack      | The memory's 32-bit integer stack.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (with-slots (($program program)
                    ($ip         ip)
                    ($jump-table jump-table)
                    ($mode       mode)
                    ($stack      stack))
           ,evaluated-interpreter
         (declare (type simple-string $program))
         (declare (ignorable          $program))
         (declare (type fixnum        $ip))
         (declare (ignorable          $ip))
         (declare (type jump-table    $jump-table))
         (declare (ignorable          $jump-table))
         (declare (type program-mode  $mode))
         (declare (ignorable          $mode))
         (declare (type Integer-Stack $stack))
         (declare (ignorable          $stack))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Supputates a jump table for the ICBINB program consigned to the
   INTERPRETER's castaldy, stores thilk in the INTERPRETER, initializes
   the random number generator, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $jump-table
      (build-a-jump-table-for $program)))
  (setf *random-state*
    (make-random-state T))
  (values))

;;; -------------------------------------------------------

(defun prepare-an-interpreter-for (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the ICBINB
   PROGRAM's execution."
  (declare (type simple-string program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-to-the-next-symbol (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (min
        (1+ $ip)
        (length $program))))
  (values))

;;; -------------------------------------------------------

(defun jump-forward (interpreter)
  "Expected to contemporaneously reside on a forward jump instruction
   (\"[\"), relocates the INTERPRETER's instruction pointer (IP) to the
   affiliated back jump point (\"]\") and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (locate-the-matching-back-jump-point $program $jump-table $ip)))
  (values))

;;; -------------------------------------------------------

(defun jump-back (interpreter)
  "Expected to contemporaneously reside on a back jump instruction
   (\"]\"), relocates the INTERPRETER's instruction pointer (IP) to the
   affiliated forward jump point (\"[\") and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $ip
      (locate-the-matching-forward-jump-point
        $program
        $jump-table
        $ip)))
  (values))

;;; -------------------------------------------------------

(defun transition-to-the-next-mode (interpreter)
  "Changes the INTERPRETER's program mode to the subsequent state,
   contingently wrapping around to the first one upon its desinent
   member's transgression, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $mode
      (mod (1+ $mode) 3)))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-on-the-symbol (symbol mode interpreter)
  (:documentation
    "Processes the SYMBOL in the INTERPRETER's context, imputing the
     program MODE as the current governor, and returns no value.")
  
  (:method ((symbol      character)
            (mode        integer)
            (interpreter Interpreter))
    "Reperesents a no-operation (NOP) by ignoring all of its inputs,
     scilicet, the SYMBOL, MODE, and INTERPRETER, accompassing no
     epiphenomena, and returning no value."
    (declare (type character    symbol))
    (declare (ignore            symbol))
    (declare (type program-mode mode))
    (declare (ignore            mode))
    (declare (type Interpreter  interpreter))
    (declare (ignore            interpreter))
    (values)))

;;; -------------------------------------------------------

(defmacro define-a-symbol-dispatch ((symbol mode) &body body)
  "Defines a implementation of the generic function
   ``dispatch-on-the-symbol'', the first formal parameter's agnomination
   is fixated as ``$symbol'' and specializes on an ``eql''-compliance
   with the character SYMBOL, its second argument being stevened
   ``$mode``, specialing on an ``eql''-equality with the integral MODE,
   and whose third and desinent argument is yclept ``$interpreter'',
   specialized on the ``Interpreter'' class, the function body receiving
   its forms from the BODY, while returning no value."
  `(defmethod dispatch-on-the-symbol
       (($symbol      (eql ,symbol))
        ($mode        (eql ,mode))
        ($interpreter Interpreter))
     (declare (type character    $symbol))
     (declare (ignorable         $symbol))
     (declare (type program-mode $mode))
     (declare (ignorable         $mode))
     (declare (type Interpreter  $interpreter))
     (declare (ignorable         $interpreter))
     (with-interpreter ($interpreter)
       ,@body)
     (values)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\, 0)
  (transition-to-the-next-mode $interpreter))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\+ 0)
  (if (integer-stack-contains-less-than-two-elements-p $stack)
    (push-onto-the-integer-stack $stack 1)
    (apply-a-binary-operator-to-the-integer-stack
      $stack
      #'add-32-bit-integers)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\- 0)
  (if (integer-stack-contains-less-than-two-elements-p $stack)
    (push-onto-the-integer-stack $stack 1)
    (apply-a-binary-operator-to-the-integer-stack
      $stack
      #'subtract-32-bit-integers)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\< 0)
  (apply-a-binary-operator-to-the-integer-stack
    $stack
    #'multiply-32-bit-integers))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\> 0)
  (apply-a-binary-operator-to-the-integer-stack
    $stack
    #'divide-32-bit-integers))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\[ 0)
  (apply-a-unary-operator-to-the-integer-stack
    $stack
    #'left-shift-32-bit-integer))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\] 0)
  (apply-a-unary-operator-to-the-integer-stack
    $stack
    #'right-shift-32-bit-integer))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\. 0)
  (apply-a-binary-operator-to-the-integer-stack
    $stack
    #'remainder-of-32-bit-integers))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\, 1)
  (transition-to-the-next-mode $interpreter))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\+ 1)
  (if (the-top-integer-stack-elements-satisfy-p $stack #'>)
    (push-onto-the-integer-stack $stack 1)
    (push-onto-the-integer-stack $stack 0)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\- 1)
  (if (the-top-integer-stack-elements-satisfy-p $stack #'<)
    (push-onto-the-integer-stack $stack 1)
    (push-onto-the-integer-stack $stack 0)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\< 1)
  (multiple-value-bind (range offset)
      (pop-two-elements-from-the-integer-stack $stack)
    (declare (type 32-bit-integer range))
    (declare (type 32-bit-integer offset))
    (push-onto-the-integer-stack $stack
      (generate-a-random-integer range offset))))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\> 1)
  (duplicate-the-top-element-on-the-integer-stack $stack))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\[ 1)
  (when (the-top-element-on-the-stack-equals-zero-p $stack)
    (jump-forward $interpreter)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\] 1)
  (unless (the-top-element-on-the-stack-equals-zero-p $stack)
    (jump-back $interpreter)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\. 1)
  (if (the-top-integer-stack-elements-satisfy-p $stack #'=)
    (push-onto-the-integer-stack $stack 1)
    (push-onto-the-integer-stack $stack 0)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\, 2)
  (transition-to-the-next-mode $interpreter))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\+ 2)
  (push-a-string-onto-the-integer-stack $stack
    (query-for-a-string
      (pop-one-element-from-the-integer-stack $stack))))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\- 2)
  (print-a-string-of-the-length $stack
    (pop-one-element-from-the-integer-stack $stack)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\< 2)
  (format *query-io* "~&~d~%"
    (pop-one-element-from-the-integer-stack $stack)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\> 2)
  (push-onto-the-integer-stack $stack
    (query-for-an-integer)))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\[ 2)
  (format *query-io* "~c"
    (code-char
      (pop-one-element-from-the-integer-stack $stack))))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\] 2)
  (format        *query-io* "~&Please enter a character: ")
  (finish-output *query-io*)
  (push-onto-the-integer-stack $stack
    (char-code
      (read-char *query-io* NIL #\Null)))
  (clear-input *query-io*)
  (values))

;;; -------------------------------------------------------

(define-a-symbol-dispatch (#\. 2)
  (print-an-integer-list-of-the-length $stack
    (pop-one-element-from-the-integer-stack $stack)))

;;; -------------------------------------------------------

(defun execution-has-completed-p (interpreter)
  "Determines whether the ICBINB program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-interpreter (interpreter)
      (not (array-in-bounds-p $program $ip)))))

;;; -------------------------------------------------------

(defun request-the-current-symbol (interpreter)
  "Returns the character located at the position of the INTERPRETER's
   instruction pointer (IP) in the ICBINB program consigned to its
   castaldy."
  (declare (type Interpreter interpreter))
  (the character
    (with-interpreter (interpreter)
      (schar $program $ip))))

;;; -------------------------------------------------------

(defun process-the-symbol (interpreter symbol)
  "Processes the SYMBOL in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type character   symbol))
  (with-interpreter (interpreter)
    (dispatch-on-the-symbol symbol $mode interpreter))
  (values))

;;; -------------------------------------------------------

(defun process-the-current-symbol (interpreter)
  "Evaluates the currently selected symbol in the ICBINB program
   consigned to the INTERPRETER's castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (process-the-symbol interpreter
    (request-the-current-symbol interpreter))
  (values))

;;; -------------------------------------------------------

(defun execute-the-program (interpreter)
  "Executes the ICBINB program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (loop until (execution-has-completed-p interpreter) do
      (process-the-current-symbol interpreter)
      (advance-to-the-next-symbol interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-ICBINB (code)
  "Interprets the piece of ICBINB source CODE and returns no value."
  (declare (type string code))
  (execute-the-program
    (prepare-an-interpreter-for
      (convert-into-a-simple-string code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time textual cat program.
(interpret-ICBINB ",,][")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-ICBINB ",,><")

;;; -------------------------------------------------------

;; Print the message "HI" to the standard output conduit.
(interpret-ICBINB "+[[[[[[+[[[+,>,[,++,,[")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-ICBINB ",,>,,>[>>,<,,],<")

;;; -------------------------------------------------------

;; Count down from inclusive 99 to inclusive zero (0), printing each
;; assumed value.
(interpret-ICBINB "+[[[[[[+[[[[[++[[+,>[,,+-,>>,<,,]")

;;; -------------------------------------------------------

;; Reverse cat program which accepts at most 16 characters.
(interpret-ICBINB "---+[[[[,,+,,>[>,[,,]")
