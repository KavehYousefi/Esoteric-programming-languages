;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "At some land", invented by the Esolang user
;; "ChuckEsoteric08" and presented in the year 2022, which operates on a
;; two-dimensional grid of commands in order to manipulate both a stack
;; and an integer-valued accumulator.
;; 
;; 
;; Concept
;; =======
;; An At some land program's conformation resolves to a two-dimensional
;; grid-like reticulation of single character identifiers, processed by
;; an instruction pointer (IP) whose advance, in its inchoation in
;; sinistrodextral airt, is amenable to redirections, terminating either
;; explicitly or on transgression of the grid's boundaries.
;; 
;; 
;; Architecture
;; ============
;; Two storage entities’ champarty contribute in the program memory’s
;; entirety, the first, more generously invested with prominence and
;; intricacy, constituted by the infinite integer stack, its complement
;; and compernage realized in the integer scalar accumulator.
;; 
;; A paravant significance’s appropriation is vindicated by stack
;; component’s deployment in arithmetics, input/output communication,
;; as well as one moeity of the conditional execution facility. This
;; last-in-first-out (LIFO) storage maintains an arbitrarily large
;; amount of unbounded signed integers, amenable to insertions and
;; deletions at the front, also known as its top, as well as instruments
;; capacitating its layout’s manipulation.
;; 
;; A rather paravail, yet potent companion, the accumulator permits the
;; rudimentary modifiers incrementation and decrementation, augmented in
;; its influence upon a program’s control flow by an alternative to the
;; stack’s conditional execution. The accumulator stores a single
;; unbounded signed integer.
;; 
;; 
;; Data Types
;; ==========
;; At some land's type system bifurcates into the paravant integer and
;; the subordinated character ilk, the latter of which is restricted in
;; its utility to the communication channels.
;; 
;; == INTEGERS ==
;; The chief significance is apportioned to unbounded signed integers,
;; the species that partakes of the stack as well as the accumulator.
;; 
;; == CHARACTERS ==
;; The currency of the bidirectional commerce betwixt the system and the
;; program, characters are introduced into the stack in their ASCII
;; code form, and transliterated in the athwart airt by decoding the
;; numeric value into a textual object.
;; 
;; 
;; Instructions
;; ============
;; The At some land programming language's instruction set tallies 31
;; members, their distribution across a variety of operational aspects
;; entalents the same with a variety of competences, including control
;; flow navigation, arithmetics, conditional execution, stack layout
;; rearrangement, and input/output facilities.
;; 
;; == COMMAND CATEGORIES ==
;; The mickleness commorant in the language permits its conceptually
;; segregation into several tiers of effectiveness:
;; 
;;   ------------------------------------------------------------------
;;   Aspect       | Commands | Effect
;;   -------------+----------+-----------------------------------------
;;   Control flow | >        | Move right.
;;                | <        | Move left.
;;                | v        | Move down.
;;                | ^        | Move up.
;;                | /        | Ascending mirror.
;;                | \        | Descending mirror.
;;                | @        | Terminate program.
;;   ..................................................................
;;   Arithmetics  | 0        | Push 0 to stack.
;;                | 1        | Push 1 to stack.
;;                | 2        | Push 2 to stack.
;;                | 3        | Push 3 to stack.
;;                | 4        | Push 4 to stack.
;;                | 5        | Push 5 to stack.
;;                | 6        | Push 6 to stack.
;;                | 7        | Push 7 to stack.
;;                | 8        | Push 8 to stack.
;;                | 9        | Push 9 to stack.
;;                | +        | Add stack top elements.
;;                | -        | Subtract stack top elements.
;;                | *        | Multiply stack top elements.
;;                | !        | Divide stack top elements.
;;                | i        | Increment accumulator.
;;                | d        | Decrement accumulator.
;;   ..................................................................
;;   Conditionals | ;        | Skip next command if stack top   = 0.
;;                | ,        | Skip next command if accumulator = 0.
;;   ..................................................................
;;   Stack layout | :        | Duplicate stack top.
;;                | %        | Swap stack top elements.
;;                | '        | Move stack top to bottom.
;;                | `        | Move stack bottom to top.
;;   ..................................................................
;;   Input/Output | ?        | Input character.
;;                | .        | Output character
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; A compendious apercu shall adhibit a basic nortelry about the
;; available operational warklooms, ere an epexegetical section endows
;; the reader with deeper apprehension.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Sets the instruction pointer's (IP) direction to right.
;;   ..................................................................
;;   <       | Sets the instruction pointer's (IP) direction to left.
;;   ..................................................................
;;   ^       | Sets the instruction pointer's (IP) direction to up.
;;   ..................................................................
;;   v       | Sets the instruction pointer's (IP) direction to down.
;;   ..................................................................
;;   /       | Mirrors the instruction pointer's (IP) direction using
;;           | an ascending mirror. Concretely, the following
;;           | consequences hold:
;;           | 
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   right             | up
;;           |   .................................
;;           |   left              | down
;;           |   .................................
;;           |   down              | left
;;           |   .................................
;;           |   up                | right
;;           |   ---------------------------------
;;   ..................................................................
;;   \       | Mirrors the instruction pointer's (IP) direction using a
;;           | descending mirror. Concretely, the following
;;           | consequences hold:
;;           | 
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   right             | down
;;           |   .................................
;;           |   left              | up
;;           |   .................................
;;           |   down              | right
;;           |   .................................
;;           |   up                | left
;;           |   ---------------------------------
;;   ..................................................................
;;   @       | Immediately terminates the program.
;;   ..................................................................
;;   0       | Pushes the number zero (0) unto the stack.
;;   ..................................................................
;;   1       | Pushes the number one (1) unto the stack.
;;   ..................................................................
;;   2       | Pushes the number two (2) unto the stack.
;;   ..................................................................
;;   3       | Pushes the number three (3) unto the stack.
;;   ..................................................................
;;   4       | Pushes the number four (4) unto the stack.
;;   ..................................................................
;;   5       | Pushes the number five (5) unto the stack.
;;   ..................................................................
;;   6       | Pushes the number six (6) unto the stack.
;;   ..................................................................
;;   7       | Pushes the number seven (7) unto the stack.
;;   ..................................................................
;;   8       | Pushes the number eight (8) unto the stack.
;;   ..................................................................
;;   9       | Pushes the number nine (9) unto the stack.
;;   ..................................................................
;;   i       | Increments the accumulator by one.
;;   ..................................................................
;;   d       | Decrements the accumulator by one.
;;   ..................................................................
;;   +       | Pops the top stack element, here designated as "a", and
;;           | the new top stack element, designated as "b", calculates
;;           | the sum
;;           |   c = b + a
;;           | and pushes this value "c" unto the stack.
;;           | An error of the type "InsufficientStackSizeError" is
;;           | signaled if the stack does not contain at least two
;;           | items.
;;   ..................................................................
;;   -       | Pops the top stack element, here designated as "a", and
;;           | the new top stack element, designated as "b", calculates
;;           | the difference
;;           |   c = b - a
;;           | and pushes this value "c" unto the stack.
;;           | An error of the type "InsufficientStackSizeError" is
;;           | signaled if the stack does not contain at least two
;;           | items.
;;   ..................................................................
;;   *       | Pops the top stack element, here designated as "a", and
;;           | the new top stack element, designated as "b", calculates
;;           | the product
;;           |   c = b * a
;;           | and pushes this value "c" unto the stack.
;;           | An error of the type "InsufficientStackSizeError" is
;;           | signaled if the stack does not contain at least two
;;           | items.
;;   ..................................................................
;;   !       | Pops the top stack element, here designated as "a", and
;;           | the new top stack element, designated as "b", calculates
;;           | the quotient
;;           |   c = b / a
;;           | and pushes this value "c" unto the stack.
;;           | An error of the type "InsufficientStackSizeError" is
;;           | signaled if the stack does not contain at least two
;;           | items.
;;           | An arithmetic error is signaled if the divisor "a"
;;           | amounts to zero (0).
;;   ..................................................................
;;   :       | Duplicates the top stack element, that is, pushes its
;;           | copy unto the stack.
;;           | An error of the type "EmptyStackError" is signaled if
;;           | the stack is empty.
;;   ..................................................................
;;   %       | Swaps the two top stack elements.
;;           | An error of the type "InsufficientStackSizeError" is
;;           | signaled if the stack does not contain at least two
;;           | items.
;;   ..................................................................
;;   '       | Moves the top stack element to the bottom of the stack.
;;           | An error of the type "EmptyStackError" is signaled if
;;           | the stack is empty.
;;   ..................................................................
;;   `       | Moves the bottom stack element to the top of the stack.
;;           | An error of the type "EmptyStackError" is signaled if
;;           | the stack is empty.
;;   ..................................................................
;;   ;       | If the top stack element equals zero, skips the next
;;           | command; aliter proceeds as usual.
;;           | An error of the type "EmptyStackError" is signaled if
;;           | the stack is empty.
;;   ..................................................................
;;   ,       | If the accumulator equals zero, skips the next command;
;;           | aliter proceeds as usual.
;;   ..................................................................
;;   ?       | Queries the standard input for an ASCII character and
;;           | pushes its ASCII code unto the stack.
;;   ..................................................................
;;   .       | Pops the top stack element and prints the character with
;;           | the corresponding ASCII code to the standard output.
;;           | An error of the type "EmptyStackError" is signaled if
;;           | the stack is empty.
;;   ------------------------------------------------------------------
;; 
;; == MIRROR PRINCIPLES ==
;; The mirror twain admitted to the instruction set permits the
;; two-dimensional instruction pointer redirection's enhancement in
;; regards of flexibility and compendiousness. Two variants of this
;; implement exist:
;; 
;;   (a) The "ascending" mirror, rendered as
;;         /
;;   
;;   (b) The "descending" mirror, displayed via
;;         \
;; 
;; The following table's onus shall be an illustrative presentation and
;; juxtaposition of these two entities, displaying the input direction
;; in the sinistral column, while the output airt is rendered
;; immediately aboon the graphical representation. Please note the
;; central alignment of the respective mirror in each picture.
;; 
;;   ------------------------------------------------------
;;   Input direction | Ascending mirror | Descending mirror
;;   ----------------+------------------+------------------
;;                   |        up        |       down
;;                   |------------------|------------------
;;                   |                  |
;;                   |        ^         |
;;   right           |        |         |
;;                   |    --> /         |    --> \
;;                   |                  |        |
;;                   |                  |        V
;;                   |                  |
;;   ................|..................|..................
;;                   |       down       |        up
;;                   |------------------|------------------
;;                   |                  |
;;                   |                  |        ^
;;   left            |                  |        |
;;                   |        / <--     |        \ <--
;;                   |        |         |
;;                   |        V         |
;;                   |                  |
;;   ................|..................|..................
;;                   |      right       |       left
;;                   |------------------|------------------
;;                   |                  |
;;                   |                  |
;;   up              |                  |
;;                   |        / -->     |    <-- \
;;                   |        ^         |        ^
;;                   |        |         |        |
;;                   |                  |
;;   ................|..................|..................
;;                   |       left       |      right
;;                   |------------------|------------------
;;                   |                  |
;;                   |                  |
;;   down            |        |         |        |
;;                   |        V         |        V
;;                   |    <-- /         |        \ -->
;;                   |                  | 
;;                   |                  |
;;   ------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Despite the lucidity in administrating explications to the
;; operational aspects, the original specification's lacunae of further
;; formalities and facts encumbers its state with the peccadillo of
;; ambiguity. A subset thereof shall be the following treatise's
;; cynosure.
;; 
;; == ARE NON-COMMAND CHARACTERS HOMOLOGATED? ==
;; No supererogation vouchsafes a compernage to the command identifiers'
;; listing in the context of homologation or interdiction. As a
;; corollary, the non-command tokens' status retains an appearance of
;; adumbration.
;; 
;; It has been adjudged that space characters as the aefauld items of a
;; filling nature may be inserted liberally, contributing no effect on
;; their own. Any other content shall incite an error of an unspecified
;; type.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter has been implemented in the programming language
;; Common Lisp; its foundry being accompanied by a superogatory text
;; program generator whose competence limns the production of a piece of
;; At some land code capable of printing a specified message to the
;; standard output.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-16
;; 
;; Sources:
;;   [esolang2023Atsomeland]
;;   The Esolang contributors, "At some land", 2023
;;   URL: "https://esolangs.org/wiki/At_some_land"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized At some land
   operations."
  '(member
    :move-right
    :move-left
    :move-up
    :move-down
    :ascending-mirror
    :descending-mirror
    :stop-program
    :push-0
    :push-1
    :push-2
    :push-3
    :push-4
    :push-5
    :push-6
    :push-7
    :push-8
    :push-9
    :add-stack
    :subtract-stack
    :multiply-stack
    :divide-stack
    :increment-accumulator
    :decrement-accumulator
    :duplicate-top
    :swap-top
    :move-top-to-bottom
    :move-bottom-to-top
    :skip-if-zero-stack
    :skip-if-zero-accumulator
    :input-character
    :output-character
    :nop))

;;; -------------------------------------------------------

(deftype mirror-placement ()
  "The ``mirror-placement'' type enumerates the recognized arrangements
   of a mirror, answering to the two command variants \"/\" (ascending
   mirror) and \"\\\" (descending mirror)."
  '(member :ascending :descending))

;;; -------------------------------------------------------

(deftype sparse-matrix-of (&optional (element-type T))
  "The ``sparse-matrix-of'' type defines a sparse two-dimensional array
   of elements which conform to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T'', with any entry amenable to a ``Location'', that
   is, a column-row twain of unbounded signed integers."
  `(hash-table-of Location ,element-type))

;;; -------------------------------------------------------

(deftype sparse-command-matrix ()
  "The ``sparse-command-matrix'' type defines a sparse two-dimensional
   array, or matrix, of ``command'' objects, by which attributes a code
   grid is delineated."
  '(sparse-matrix-of command))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines a non-negative integer,
   that is, an integral number in the range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts for traveling
   inside of a code grid's boundaries."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based stack compact of zero or more
   unbounded signed integers."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type defines a function that accepts two
   integer objects and responds with a single integer result.
   ---
   Intended for the particular employment in the context of stack
   arithmetics, the function signature thus conforms to:
     lambda (integer integer) => integer"
  '(function (integer integer) integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ATSML-Error (error)
  ()
  (:documentation
    "The ``ASTML-Error'' provides a common foundry for all error types
     appertaining to anomalies in the context of an At some land (ATSML)
     program's execution."))

;;; -------------------------------------------------------

(define-condition Stack-Error (ATSML-Error)
  ((offending-stack
    :initarg       :offending-stack
    :initform      (error "Offending stack is missing.")
    :reader        stack-error-offending-stack
    :type          stack
    :documentation ""))
  (:documentation
    "The ``Stack-Error'' type provides a common base for all error types
     invested with those anomalies occurring in the handling of stack
     objects."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Stack-Error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot perform the operation as it is empty.")))
  (:documentation
    "The ``Empty-Stack-Error'' condition serves to signal an anomalous
     situation in which an operation required an element from a stack
     which was at the instant of the operation's invocation destitute of
     any items."))

;;; -------------------------------------------------------

(define-condition Insufficient-Stack-Size-Error (Stack-Error)
  ((expected-size
    :initarg       :expected-size
    :initform      (error "Expected stack size missing.")
    :reader        insufficient-stack-size-error-expected-size
    :type          non-negative-integer
    :documentation ""))
  (:report
    (lambda (condition stream)
      (declare (type Insufficient-Stack-Size-Error condition))
      (declare (type destination                   stream))
      (format stream "Insufficient number of stack elements for the ~
                      operation. Expected ~d but encountered only ~d."
        (insufficient-stack-size-error-expected-size condition)
        (length
          (stack-error-offending-stack condition)))))
  (:documentation
    "The ``Insufficient-Stack-Size-Error'' condition serves to signal
     an anomalous situation in which an operation required a specific
     number of items from a stack which failed to contribute one or more
     such."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of location.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class represents a two-dimensional compound of
   column-row indices, both member of the unbounded integer range."
  (x 0 :type integer)
  (y 0 :type integer))

;;; -------------------------------------------------------

(defun translate-location (location direction)
  "Translates the LOCATION one step into the DIRECTION and returns no
   value."
  (declare (type Location  location))
  (declare (type direction direction))
  (case direction
    (:left     (decf (location-x location)))
    (:right    (incf (location-x location)))
    (:up       (decf (location-y location)))
    (:down     (incf (location-y location)))
    (otherwise (error "Invalid direction: ~s." direction)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of pointer.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pointer
  (:constructor make-pointer (&optional (x 0) (y 0)
                              &aux (location (make-location x y)))))
  "The ``Pointer'' class encapsulates the information requisite for
   maintaining an instruction pointer (IP) or program counter (IP)
   inside of a two-dimensional command grid, its establishment being
   compact of the signed integer coordinates and the current translation
   direction."
  (location  (error "Missing location.") :type Location)
  (direction :right                      :type direction))

;;; -------------------------------------------------------

(defun move-pointer (pointer)
  "Translates the POINTER one step into its current direction and
   returns no value."
  (declare (type Pointer pointer))
  (translate-location
    (pointer-location pointer)
    (pointer-direction pointer))
  (values))

;;; -------------------------------------------------------

(defun mirror-direction (direction mirror-placement)
  "Returns the direction resulting by the intersection of the input
   DIRECTION with a mirror arranged according to the MIRROR-PLACEMENT."
  (declare (type direction        direction))
  (declare (type mirror-placement mirror-placement))
  (the direction
    (case mirror-placement
      (:ascending
        (case direction
          (:right    :up)
          (:left     :down)
          (:down     :left)
          (:up       :right)
          (otherwise (error "Invalid direction: ~s." direction))))
      (:descending
        (case direction
          (:right    :down)
          (:left     :up)
          (:down     :right)
          (:up       :left)
          (otherwise (error "Invalid direction: ~s." direction))))
      (otherwise
        (error "Invalid mirror placement: ~s." mirror-placement)))))

;;; -------------------------------------------------------

(defun mirror-pointer (pointer mirror-placement)
  "Alters the POINTER's direction as the consequence of its intersection
   with a mirror arranged according to the MIRROR-PLACEMENT and returns
   no value."
  (declare (type Pointer          pointer))
  (declare (type mirror-placement mirror-placement))
  (setf (pointer-direction pointer)
    (mirror-direction
      (pointer-direction pointer)
      mirror-placement))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of command and identifier tables.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character command)   +COMMAND-TABLE+))
(declaim (type (hash-table-of command   character) +IDENTIFIER-TABLE+))

;;; -------------------------------------------------------

(defparameter +COMMAND-TABLE+
  (make-hash-table :test #'eq)
  "Associates the recognized command identifiers (tokens) with the
   representative ``command'' objects.")

(defparameter +IDENTIFIER-TABLE+
  (make-hash-table :test #'eq)
  "Associates the recognized ``command'' objects with the identifiers
   (tokens) which they represent.")

;;; -------------------------------------------------------

(flet ((register-command (identifier command)
        "Associates the command IDENTIFIER with the COMMAND object in a
         bidirectional airt and returns no value."
        (declare (type character identifier))
        (declare (type command   command))
        (setf (gethash identifier +COMMAND-TABLE+) command)
        (setf (gethash command    +IDENTIFIER-TABLE+) identifier)
        (values)))
  (register-command #\Space :nop)
  (register-command #\>     :move-right)
  (register-command #\<     :move-left)
  (register-command #\^     :move-up)
  (register-command #\v     :move-down)
  (register-command #\/     :ascending-mirror)
  (register-command #\\     :descending-mirror)
  (register-command #\0     :push-0)
  (register-command #\1     :push-1)
  (register-command #\2     :push-2)
  (register-command #\3     :push-3)
  (register-command #\4     :push-4)
  (register-command #\5     :push-5)
  (register-command #\6     :push-6)
  (register-command #\7     :push-7)
  (register-command #\8     :push-8)
  (register-command #\9     :push-9)
  (register-command #\+     :add-stack)
  (register-command #\-     :subtract-stack)
  (register-command #\*     :multiply-stack)
  (register-command #\!     :divide-stack)
  (register-command #\i     :increment-accumulator)
  (register-command #\d     :decrement-accumulator)
  (register-command #\:     :duplicate-top)
  (register-command #\%     :swap-top)
  (register-command #\;     :skip-if-zero-stack)
  (register-command #\,     :skip-if-zero-accumulator)
  (register-command #\'     :move-top-to-bottom)
  (register-command #\`     :move-bottom-to-top)
  (register-command #\?     :input-character)
  (register-command #\.     :output-character)
  (register-command #\@     :stop-program)
  (values))

;;; -------------------------------------------------------

(defun get-command (identifier)
  "Returns the command object corresponding to the IDENTIFIER, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type character identifier))
  (the command
    (or (gethash identifier +COMMAND-TABLE+)
        (error "Invalid command identifier: \"~c\"." identifier))))

;;; -------------------------------------------------------

(defun get-command-identifier (command)
  "Returns the identifier or token corresponding to the command, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type command command))
  (the character
    (or (gethash command +IDENTIFIER-TABLE+)
        (error "Invalid command: ~s." command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code grid.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((width
    :initarg       :width
    :initform      0
    :type          non-negative-integer
    :documentation "The number of columns.")
   (height
    :initarg       :height
    :initform      0
    :type          non-negative-integer
    :documentation "The number of rows.")
   (cells
    :initarg       :cells
    :initform      (make-hash-table :test #'equalp)
    :type          sparse-command-matrix
    :documentation "A sparse two-dimensional array of commands."))
  (:documentation
    "The ``Grid'' class models a two-dimensional grid, or matrix, of
     At some land commands, amenable to column and row designators for
     their access."))

;;; -------------------------------------------------------

(defun valid-grid-location-p (grid probed-location)
  "Determines whether the PROBED-LOCATION resides inside of the GRID's
   boundaries, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location probed-location))
  (the boolean
    (not (null
      (and
        (>= (location-x probed-location) 0)
        (<  (location-x probed-location) (slot-value grid 'width))
        (>= (location-y probed-location) 0)
        (<  (location-y probed-location) (slot-value grid 'height)))))))

;;; -------------------------------------------------------

(defun grid-cell-at (grid location)
  "Returns the command at the LOCATION into the GRID, or signals an
   error of an unspecified type if the same violates the valid
   boundaries."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the command
    (if (valid-grid-location-p grid location)
      (gethash location (slot-value grid 'cells) :nop)
      (error "The location ~a violates the grid bounds." location))))

;;; -------------------------------------------------------

(defun build-code-grid (code)
  "Generates and returns for the piece of At some land source CODE a
   command ``Grid'' representation."
  (declare (type string code))
  (let ((code-grid (make-instance 'Grid))
        (x         0)
        (y         0))
    (declare (type Grid                 code-grid))
    (declare (type non-negative-integer x))
    (declare (type non-negative-integer y))
    (with-slots (cells width height) code-grid
      (declare (type sparse-command-matrix cells))
      (declare (type non-negative-integer  width))
      (declare (type non-negative-integer  height))
      (loop for token of-type character across code do
        ;; Ensure that the grid height is greater than zero (0).
        (setf height (max height 1))
        (case token
          (#\Newline
            (incf height)
            (incf y 1)
            (setf x 0))
          (otherwise
            (setf (gethash (make-location x y) cells)
                  (get-command token))
            (setf width (max width (1+ x)))
            ;; Ensure that the grid height is greater than zero (0).
            ;(setf height (max height 1))
            (incf x 1)))))
    (the Grid code-grid)))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-slots (width height) grid
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (format stream "~&~d x ~d grid:" width height)
    (dotimes (y height)
      (declare (type non-negative-integer y))
      (format stream "~&")
      (dotimes (x width)
        (declare (type non-negative-integer x))
        (format stream "~c"
          (get-command-identifier
            (grid-cell-at grid
              (make-location x y))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          Grid
    :documentation "The two-dimensional grid of commands to evaluate.")
   (pointer
    :initarg       :pointer
    :initform      (make-pointer 0 0)
    :accessor      program-pointer
    :type          Pointer
    :documentation "The instruction pointer (IP) whose perambulation
                    along the PROGRAM grid accompasses the code's
                    execution.")
   (stack
    :initarg       :stack
    :initform      NIL
    :type          stack
    :documentation "A stack of signed integer numbers.")
   (accumulator
    :initarg       :accumulator
    :initform      0
    :accessor      accumulator
    :type          integer
    :documentation "The signed integer accumulator.")
   (program-halted-p
    :initarg       :program-halted-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the instruction POINER (IP) has
                    encountered a halting command, \"@\", thus
                    explicitly being mandated to terminate the
                    program."))
  (:documentation
    "The ``Interpreter'' class' constitutes the induction of effect to
     an At some land command grid."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' which operates on the
   PROGRAM grid."
  (declare (type Grid program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun signal-empty-stack-error (interpreter)
  "Signals an error of the type ``Empty-Stack-Error'', conveying the
   INTERPRETER's stack as the offending entity."
  (declare (type Interpreter interpreter))
  (error 'Empty-Stack-Error
    :offending-stack (slot-value interpreter 'stack)))

;;; -------------------------------------------------------

(defun signal-insufficient-stack-size-error (interpreter
                                             expected-stack-size)
  "Signals an error of the type ``Insufficient-Stack-Size-Error'',
   communicating the INTERPRETER's stack in conjunction with the
   EXPECTED-STACK-SIZE as the offending pieces of information."
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer expected-stack-size))
  (error 'Empty-Stack-Error
    :offending-stack     (slot-value interpreter 'stack)
    :expected-stack-size expected-stack-size))

;;; -------------------------------------------------------

(defun check-if-stack-is-empty (interpreter)
  "Determines whether the INTERPRETER's stack is empty, on confirmation
   signaling an error of the type ``Empty-Stack-Error'', otherwise
   simply returning no value."
  (declare (type Interpreter interpreter))
  (unless (slot-value interpreter 'stack)
    (signal-empty-stack-error interpreter))
  (values))

;;; -------------------------------------------------------

(defun check-stack-size (interpreter expected-stack-size)
  "Determines whether the INTERPRETER's stack contains less than the
   EXPECTED-STACK-SIZE tally of elements, on confirmation signaling an
   error of the type ``Insufficient-Stack-Size-Error'', otherwise simply
   returning no value."
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer expected-stack-size))
  (when (< (length (slot-value interpreter 'stack))
           expected-stack-size)
    (signal-insufficient-stack-size-error interpreter
      expected-stack-size))
  (values))

;;; -------------------------------------------------------

(defun pointer-out-of-grid-p (interpreter)
  "Determines whether the INTERPRETER's instruction pointer (IP) has
   transgressed its command grid's boundaries, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (valid-grid-location-p
        (slot-value interpreter 'program)
        (pointer-location
          (slot-value interpreter 'pointer))))))

;;; -------------------------------------------------------

(defun program-finished-p (interpreter)
  "Determines whether the INTERPRETER's program has finished its
   execution, either by encountering an explicit halting operation
   (\"@\") or by transgressing the grid's boundaries, in any case
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (or (slot-value            interpreter 'program-halted-p)
        (pointer-out-of-grid-p interpreter))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the command in the INTERPRETER's program located under its
   instruction pointer (IP) position."
  (declare (type Interpreter interpreter))
  (the command
    (grid-cell-at
      (slot-value interpreter 'program)
      (pointer-location
        (slot-value interpreter 'pointer)))))

;;; -------------------------------------------------------

(defun advance-pointer (interpreter)
  "Translates the INTERPRETER's instruction pointer (IP) one step into
   its current direction and returns no value, or signals an error of an
   unspecified type upon the pointer's violation of the valid bounds."
  (declare (type Interpreter interpreter))
  (move-pointer
    (slot-value interpreter 'pointer))
  (values))

;;; -------------------------------------------------------

(defun set-direction (interpreter new-direction)
  "Changes the INTERPRETER's instruction pointer (IP) direction to the
   NEW-DIRECTION and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type direction   new-direction))
  (setf (pointer-direction (slot-value interpreter 'pointer))
        new-direction)
  (values))

;;; -------------------------------------------------------

(defun push-to-stack (interpreter new-element)
  "Pushes the NEW-ELEMENT unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-element))
  (push new-element
    (slot-value interpreter 'stack))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Removes and returns the INTERPRETER's top stack element.
   ---
   An error of the type ``Empty-Stack-Error'' is signaled if the stack
   is empty during the instant of this operation's invocation."
  (declare (type Interpreter interpreter))
  (check-if-stack-is-empty interpreter)
  (the integer
    (pop (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun get-first-stack-element (interpreter)
  "Returns the INTERPRETER's top stack element, or signals an error of
   the type ``Empty-Stack-Error'' if the same is empty."
  (declare (type Interpreter interpreter))
  (check-if-stack-is-empty interpreter)
  (the integer
    (first (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun apply-to-stack (interpreter binary-operator)
  "Pops the INTERPRETER's top stack element, \"a\", the new top stack
   element \"b\", induces \"b\" and \"a\" in this order into the
   BINARY-OPERATOR function, pushes the result unto the stack, and
   returns no value.
   ---
   The BINARY-OPERATOR must be a function that accepts two integer
   inputs and returns a single integer result, thus complying to the
   signature:
     lambda (integer integer) => integer
   ---
   This operation's complete working is a tantamount of the following
   pseudocode:
     let a <- interpreter.stack.pop()
     let b <- interpreter.stack.pop()
     let c <- call binaryOperator(b, a)
     interpreter.stack.push(c)
     return no value
   ---
   An error of the type ``Insufficient-Stack-Size-Error'' is signaled if
   the stack does not entail at least two elements."
  (declare (type Interpreter     interpreter))
  (declare (type binary-operator binary-operator))
  (check-stack-size interpreter 2)
  (let ((a (pop-from-stack interpreter))
        (b (pop-from-stack interpreter)))
    (declare (type integer a))
    (declare (type integer b))
    (push-to-stack interpreter
      (funcall binary-operator b a)))
  (values))

;;; -------------------------------------------------------

(defun duplicate-top-of-stack (interpreter)
  "Duplicates the INTERPRETER's top stack element and returns no value.
   ---
   An error of the type ``Empty-Stack-Error'' is signaled if the stack
   is empty during the instant of this operation's invocation."
  (declare (type Interpreter interpreter))
  (check-if-stack-is-empty interpreter)
  (push
    (first (slot-value interpreter 'stack))
    (slot-value interpreter 'stack))
  (values))

;;; -------------------------------------------------------

(defun swap-top-of-stack (interpreter)
  "Exchanges the position of the INTERPRETER's two top stack elements
   and returns no value.
   ---
   An error of the type ``Insufficient-Stack-Size-Error'' is signaled if
   the stack does not entail at least two elements."
  (declare (type Interpreter interpreter))
  (check-stack-size interpreter 2)
  (rotatef
    (first  (slot-value interpreter 'stack))
    (second (slot-value interpreter 'stack)))
  (values))

;;; -------------------------------------------------------

(defun move-top-stack-element-to-bottom (interpreter)
  "Relocates the element at the top of the INTERPRETER's stack to its
   bottom and returns no value.
   ---
   An error of the type ``Empty-Stack-Error'' is signaled if the stack
   is empty during the instant of this operation's invocation."
  (declare (type Interpreter interpreter))
  (let ((top-element (pop-from-stack interpreter)))
    (declare (type integer top-element))
    (with-slots (stack) interpreter
      (declare (type stack stack))
      (setf stack
        (append stack
          (list top-element)))))
  (values))

;;; -------------------------------------------------------

(defun move-bottom-stack-element-to-top (interpreter)
  "Relocates the element at the bottom of the INTERPRETER's stack to its
   top and returns no value.
   ---
   An error of the type ``Empty-Stack-Error'' is signaled if the stack
   is empty during the instant of this operation's invocation."
  (declare (type Interpreter interpreter))
  (check-if-stack-is-empty interpreter)
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (let ((bottom-element (first (last stack))))
      (declare (type integer bottom-element))
      (setf stack (butlast stack))
      (push bottom-element stack)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the program stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (program-finished-p interpreter) do
    (case (get-current-instruction interpreter)
      (:move-right
        (set-direction   interpreter :right)
        (advance-pointer interpreter))
      
      (:move-left
        (set-direction   interpreter :left)
        (advance-pointer interpreter))
      
      (:move-up
        (set-direction   interpreter :up)
        (advance-pointer interpreter))
      
      (:move-down
        (set-direction   interpreter :down)
        (advance-pointer interpreter))
      
      (:ascending-mirror
        (mirror-pointer
          (program-pointer interpreter)
          :ascending)
        (advance-pointer interpreter))
      
      (:descending-mirror
        (mirror-pointer
          (program-pointer interpreter)
          :descending)
        (advance-pointer interpreter))
      
      (:stop-program
        (setf (slot-value interpreter 'program-halted-p) T))
      
      (:push-0
        (push-to-stack interpreter 0)
        (advance-pointer interpreter))
      
      (:push-1
        (push-to-stack interpreter 1)
        (advance-pointer interpreter))
      
      (:push-2
        (push-to-stack interpreter 2)
        (advance-pointer interpreter))
      
      (:push-3
        (push-to-stack interpreter 3)
        (advance-pointer interpreter))
      
      (:push-4
        (push-to-stack interpreter 4)
        (advance-pointer interpreter))
      
      (:push-5
        (push-to-stack interpreter 5)
        (advance-pointer interpreter))
      
      (:push-6
        (push-to-stack interpreter 6)
        (advance-pointer interpreter))
      
      (:push-7
        (push-to-stack interpreter 7)
        (advance-pointer interpreter))
      
      (:push-8
        (push-to-stack interpreter 8)
        (advance-pointer interpreter))
      
      (:push-9
        (push-to-stack interpreter 9)
        (advance-pointer interpreter))
      
      (:add-stack
        (apply-to-stack interpreter #'+)
        (advance-pointer interpreter))
      
      (:subtract-stack
        (apply-to-stack interpreter #'-)
        (advance-pointer interpreter))
      
      (:multiply-stack
        (apply-to-stack interpreter #'*)
        (advance-pointer interpreter))
      
      (:divide-stack
        (apply-to-stack interpreter #'round)
        (advance-pointer interpreter))
      
      (:increment-accumulator
        (incf (accumulator interpreter))
        (advance-pointer interpreter))
      
      (:decrement-accumulator
        (decf (accumulator interpreter))
        (advance-pointer interpreter))
      
      (:duplicate-top
        (duplicate-top-of-stack interpreter)
        (advance-pointer        interpreter))
      
      (:swap-top
        (swap-top-of-stack interpreter)
        (advance-pointer   interpreter))
      
      (:move-top-to-bottom
        (move-top-stack-element-to-bottom interpreter)
        (advance-pointer interpreter))
      
      (:move-bottom-to-top
        (move-bottom-stack-element-to-top interpreter)
        (advance-pointer interpreter))
      
      (:skip-if-zero-stack
        (when (zerop (get-first-stack-element interpreter))
          (advance-pointer interpreter))
        (advance-pointer interpreter))
      
      (:skip-if-zero-accumulator
        (when (zerop (accumulator interpreter))
          (advance-pointer interpreter))
        (advance-pointer interpreter))
      
      (:input-character
        (format T "~&Please input an ASCII character: ")
        (push-to-stack interpreter
          (char-code
            (read-char *standard-input* NIL 0)))
        (clear-input *standard-input*)
        (advance-pointer interpreter))
      
      (:output-character
        (check-if-stack-is-empty interpreter)
        (format T "~c"
          (code-char
            (pop-from-stack interpreter)))
        (advance-pointer interpreter))
      
      (:nop
        (advance-pointer interpreter))
      
      (otherwise
        (error "Invalid instruction ~a at position ~a."
          (get-current-instruction interpreter)
          (pointer-location
            (program-pointer interpreter))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-At-some-land (code)
  "Interprets the piece of At some land source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (build-code-grid code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun join-character-lines (&rest lines)
  "Creates and returns a new string by joining the LINES with a newline
   each.
   ---
   This operation pursues to supply a commodity for eath At some land
   source code generation, permitting each line's specification as an
   aefauld string, provided to this function in a variadic list, such
   that the conjoining newlines are inserted automatically."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type non-negative-integer +CONTENT-AREA-WIDTH+))

;;; -------------------------------------------------------

(defparameter +CONTENT-AREA-WIDTH+ 60
  "The number of characters that comprise a text printer program's
   middle section, responsible for both the arithmetics that replicate
   a character's ASCII code in the memory and printing the same.")

;;; -------------------------------------------------------

(defun generate-number-replication-code (number
                                         &key (destination NIL))
  "Generates a series of At some land stack push and addition operations
   capable of replicating the NUMBER in the program memory and writes
   the thus produced program segment to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type non-negative-integer number))
  (declare (type destination          destination))
  (the (or null string)
    (if destination
      (let ((remaining-number number))
        (declare (type integer remaining-number))
        (loop
          initially
            (format destination "0")
          
          while (plusp remaining-number)
          
          for divisor
            of-type (integer 0 9)
            from   9
            downto 1
          
          for multiple
            of-type non-negative-integer
            =       (floor remaining-number divisor)
          
          when (plusp multiple) do
            (format destination "~v@{~d+~:*~}" multiple divisor)
            (decf remaining-number (* multiple divisor))
          
          finally
            (unless (zerop remaining-number)
              (error "Unable to replicate the number ~d. Result: ~d."
                number remaining-number))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-number-replication-code number :destination output)))))

;;; -------------------------------------------------------

(defun generate-text-program (message
                              &key (destination NIL))
  "Generates an At some land program capable of printing the MESSAGE to
   the standard output and writes the result to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   responding with a fresh string comprehending the code."
  (declare (type string message))
  (the (or null string)
    (if destination
      (flet ((write-inner-print-line (character)
              "Writes to the DESTINATION the At some land code capable
               of replicating the CHARACTER's ASCII code in the program
               memory, prefixed by a dextral instruction pointer (IP)
               redirection, and terminated by a downward movement, and
               returns no value."
              (format destination "~&>~va.v"
                +CONTENT-AREA-WIDTH+
                (generate-number-replication-code
                  (char-code character)
                  :destination NIL))
              (values))
             
             (write-terminal-print-line (character)
              "Writes to the DESTINATION the At some land code capable
               of replicating the CHARACTER's ASCII code in the program
               memory, prefixed by a dextral instruction pointer (IP)
               redirection, and terminated by a halt instruction, and
               returns no value."
              (format destination "~&>~va.@"
                +CONTENT-AREA-WIDTH+
                (generate-number-replication-code
                  (char-code character)
                  :destination NIL))
              (values))
             
             (write-carriage-return-line ()
              "Writes to the DESTINATION a line, consumed in
               dextrosinistral airt, redirects the instruction pointer
               (IP) to the left and finally downward, and returns no
               value."
              (format destination "~&v~va <"
                +CONTENT-AREA-WIDTH+ #\Space)
              (values)))
        
        (let ((ultimate-character-position (1- (length message))))
          (declare (type fixnum ultimate-character-position))
          (loop
            for character-position
              of-type fixnum
              from    0
              to      ultimate-character-position
            for penultimate-character-p
              of-type boolean
              =       (not (null
                        (>= character-position
                            ultimate-character-position)))
            do
              (cond
                (penultimate-character-p
                  (write-terminal-print-line
                    (char message character-position)))
                (T
                  (write-inner-print-line
                    (char message character-position))
                  (write-carriage-return-line))))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program message :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output.
(interpret-At-some-land
  ">09+9+9+9+9+9+9+9+          .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+2+  .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+  .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+  .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+3+.v
v                            <
>09+9+9+9+8+                .v
v                            <
>09+9+9+5+                  .v
v                            <
>09+9+9+9+9+9+9+9+9+6+      .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+3+.v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+6+.v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+9+  .v
v                            <
>09+9+9+9+9+9+9+9+9+9+9+1+  .v
v                            <
>09+9+9+6+                  .@")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-At-some-land
  (join-character-lines
    ">?.v"
    "^  <"))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-At-some-land
  (join-character-lines
    "?68*-;v68*.@"
    "     >v"
    "      6"
    "      8"
    "      *"
    "      1"
    "      +"
    "      ."
    "     ^<"))

;;; -------------------------------------------------------

;; Generate the At some land program for printing the message
;; "Hello, World!" and interpret it.
;; 
;; The text generator program produces the following code:
;; 
;;   >09+9+9+9+9+9+9+9+                                           .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+2+                                   .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+                                   .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+                                   .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+3+                                 .v
;;   v                                                             <
;;   >09+9+9+9+8+                                                 .v
;;   v                                                             <
;;   >09+9+9+5+                                                   .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+6+                                       .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+3+                                 .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+6+                                 .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+9+                                   .v
;;   v                                                             <
;;   >09+9+9+9+9+9+9+9+9+9+9+1+                                   .v
;;   v                                                             <
;;   >09+9+9+6+                                                   .@
(interpret-At-some-land
  (generate-text-program "Hello, World!"))
