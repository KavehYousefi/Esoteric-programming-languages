;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; lanugage "sb", invented by the Esolang user "C0ffee" and presented
;; on March 14th, 2025, its kenspeckle caract's ponibility being a
;; corollary of a memory whose dichotomy enumerates a stack and an
;; infinite vector, for both the castaldy of integer number, strings,
;; and arrays registers their involvement's telos.
;; 
;; 
;; Concept
;; =======
;; The sb programming language's firmament is realized in the
;; manipulation of integer numbers, strings, and arrays comprehending
;; all three species of participants by adminiculum of a memory compact
;; of a stack and an infinite vector.
;; 
;; == DATA TYPES: INTEGERS, STRINGS, AND ARRAYS ==
;; A relative magnanimity's ostention patefies in the sb data types'
;; taxonomy, enlisting a trisulc ramosity into integer numbers, strings,
;; and arrays, to latter of whom none of these elements encumbers a
;; forinsecal audition.
;; 
;; == INTEGERS MAY ASSUME ANY SIGN AND MAGNITUDE ==
;; The vale of admissible integers does not wist of natural mears
;; concerning the polarity or magnitude.
;; 
;; == STRINGS: CATENAE OF ZERO OR MORE CHARACTERS ==
;; Ensconced in a jumelle of double quotation marks ('"'), a string
;; literal's designment declaims its edification upon zero or more
;; characters.
;; 
;; == ARRAYS: VECTORS OF INTEGERS, STRINGS, AND ARRAYS ==
;; Inwith the array diorism wones the contingency for integers, strings,
;; and nested arrays' castaldy, their amenability for adits founded upon
;; a zero-based subscript enumeration principle.
;; 
;; == THE MEMORY: A TWAIN OF STACK AND RANDOM-ACCESS TAPE ==
;; A twyfold capacity serves in the sb memory architecture's patration,
;; its componency's paravaunt membership that of a stack, operating in
;; champarty with a tape-like vector, both species of salvatory
;; concredited with the castaldy of integer numbers, strings, and arrays
;; whose admission's compass dispands athwart the trisulc data types.
;; 
;; == THE STACK: A LAST IN, FIRST OUT STORAGE ==
;; The most data management agent most peisant in its significant is
;; realized in the stack, a last in, first out salvatory for objects.
;; 
;; == THE TAPE: A RANDOM-ACCESS STORAGE ==
;; The memory's parergon, molded into the tape or vector plasmature,
;; serves in a random-access data conditory's furnishment, its cells'
;; amenability ensuing from signed integer indices which wist of no
;; natural bournes in their mickleness.
;; 
;; 
;; Instructions
;; ============
;; sb's instruction set enumerates a membership of 26 avails, this
;; cardinality's coefficiency admitting such warklumes as the insertion
;; of literal data objects, unary, binary, and relational operators,
;; array handling, the castaldy of the memory's stack and tape
;; components, the input and output intercourse, dynamic execution
;; provisions, as well as an iterance construct.
;; 
;; Any token to whom no epiphenomenal causatum's parcery ligates an
;; enhausing proprium the participation, the tacit agency of a
;; no-operation, or NOP, in lieu of such utible nature limns the chosen
;; succedaneum.
;; 
;; == SB'S COMMANDS MAY BE SUBSUMED INTO TEN CATEGORIES ==
;; A decimal accompt of tiers' participation enstates the sb language's
;; operative bailiwicks:
;; 
;;   (a) Literal data statements
;;   
;;   (b) Unary operators
;;   
;;   (c) Binary operators
;;   
;;   (d) Relational operators
;;   
;;   (e) Array attrectation
;;   
;;   (f) Stack management
;;   
;;   (g) Tape castaldy
;;   
;;   (h) Input/Output commerce
;;   
;;   (i) Dynamic execution
;;   
;;   (j) Control flow duction
;; 
;; == OVERVIEW ==
;; The following apercu's cynosure shall be established in a nait mete
;; of nortelry's vouchsafement anent the language's operative warklumes.
;; 
;; Please heed the demarcation of succedaneous tmemata by underlines of
;; caret ("^") symbols, the occupied spatiality intended for its
;; supersession by actual sb code in the program's perclose.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   LITERAL DATA STATEMENT
;;   ------------------------------------------------------------------
;;   digits  | Pushes the integer number compact of the decimal digits
;;   ^^^^^^  | {digits} the memory stack.
;;           |---------------------------------------------------------
;;           | {digits} must constitute a catena amplecting one or more
;;           | decimal digits.
;;   ..................................................................
;;   'char   | Pushes the character {char} onto the memory stack.
;;    ^^^^   |---------------------------------------------------------
;;           | {char} must constitute a character.
;;           |---------------------------------------------------------
;;           | If {char}'s carency has been detected, an error of the
;;           | type "MalformedLiteralError" is signaled.
;;   ..................................................................
;;   "chars" | Pushes the string compact of the characters {chars} onto
;;    ^^^^^  | the memory stack.
;;           |---------------------------------------------------------
;;           | {chars} must constitute a catena amplecting zero or more
;;           | characters.
;;           |---------------------------------------------------------
;;           | If no terminating double quotation mark ('"') can be
;;           | detected, an error of the type "MalformedLiteralError"
;;           | is signaled.
;;   ==================================================================
;;   UNARY OPERATORS
;;   ------------------------------------------------------------------
;;   _       | Pops the memory stack's top element, expected to produce
;;           | an integer number, and pushes its negated value on the
;;           | stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- -a
;;           |   memoryStack.push(b)
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   !       | Pops the memory stack's top element, construes thilk as
;;           | a Boolean truth value, and pushes the negated
;;           | signification as integer number, in compliance with the
;;           | following nomothesy, onto the stack:
;;           | 
;;           |   (a) If the subject represents a Boolean "true" value,
;;           |       that is, any object except for an integer equal to
;;           |       zero (0), an integer number of zero (0) is
;;           |       furnished.
;;           |   
;;           |   (b) If the subject repreesents a Boolean "false"
;;           |       value, that is, an integer number equal to
;;           |       zero (0), an integer number of one (1) is
;;           |       furnished.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ==================================================================
;;   BINARY OPERATORS
;;   ------------------------------------------------------------------
;;   +       | Pops the memory stack's top element, here norned "a",
;;           | expected to represent an integer number, pops the new
;;           | top element, "b", siclike of an imperative numerical
;;           | composition, and pushes the result of the addition
;;           | (b + a) onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- b + a
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   -       | Pops the memory stack's top element, here norned "a",
;;           | expected to represent an integer number, pops the new
;;           | top element, "b", siclike of an imperative numerical
;;           | composition, and pushes the result of the subtraction
;;           | (b - a) onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- b - a
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   *       | Pops the memory stack's top element, here norned "a",
;;           | expected to represent an integer number, pops the new
;;           | top element, "b", siclike of an imperative numerical
;;           | composition, and pushes the result of the multiplication
;;           | (b * a) onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- b * a
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   /       | Pops the memory stack's top element, here norned "a",
;;           | expected to represent an integer number, pops the new
;;           | top element, "b", siclike of an imperative numerical
;;           | composition, and pushes the result of the integer
;;           | division truncate(b / a) onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- truncate(b / a)
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   %       | Pops the memory stack's top element, here norned "a",
;;           | expected to represent an integer number, pops the new
;;           | top element, "b", siclike of an imperative numerical
;;           | composition, and pushes the remainder of the integer
;;           | division (b % a), that is, (b modulo a), onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- b modulo a
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   &       | Pops the memory stack's top element, here norned "a",
;;           | pops the new top element, "b", and pushes the result of
;;           | a concatenation in compliance with the following
;;           | nomothesy to the memory stack:
;;           | 
;;           |   (a) If a and b are both arrays, a fresh array is
;;           |       produced by appending to a's contents the elements
;;           |       commorant in b.
;;           |   
;;           |   (b) If a is an array, and b represents a non-array,
;;           |       a fresh array is produced by appending to a's
;;           |       contents the object b.
;;           |   
;;           |   (c) If a represents a non-array, and b is an array,
;;           |       a fresh array is produced by prepending to b's
;;           |       content the object a.
;;           |   
;;           |   (d) In any other case, a fresh string is produced by
;;           |       concatenating a and b in this exact order.
;;           |---------------------------------------------------------
;;           | Neither a nor b will be destructively modified by this
;;           | operation.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- nil
;;           |   
;;           |   if (a is an array) and (b is an array) then
;;           |     c <- copy a
;;           |     c.insertAtRear(b.elements)
;;           |   else if (a is an array) and (b is not an array) then
;;           |     c <- copy a
;;           |     c.insertAtRear(b)
;;           |   else if (a is not an array) and (b is an array) then
;;           |     c <- copy b
;;           |     c.insertAtFront(a)
;;           |   else
;;           |     c <- concatenate a and b into a string
;;           |   end if
;;           |   
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ==================================================================
;;   RELATIONAL OPERATORS
;;   ------------------------------------------------------------------
;;   =       | Pops the memory stack's top element, here norned "a",
;;           | pops the new top element, "b", determines whether a is
;;           | equal b, and pushes an integer number signifying the
;;           | relationship's satisfaction:
;;           | 
;;           |   (a) If a and b differ in their types, always pushes
;;           |       the negative discriminator zero (0).
;;           |   
;;           |   (b) If a and b both represent integer numbers, pushes
;;           |       the integer number one (1) if a is numerically
;;           |       equal to b; otherwise pushes zero (0).
;;           |   
;;           |   (c) If a and b both represent strings, pushes the
;;           |       integer number one (1) if a is lexicographically
;;           |       equal to b, respecting the characters' cases;
;;           |       otherwise pushes zero (0).
;;           |   
;;           |   (d) If a and b both represent arrays, pushes the
;;           |       integer number one (1) if the arrays are of equal
;;           |       length and all elements in a are equal to those in
;;           |       b at the corresponding position; otherwise pushes
;;           | zero (0).
;;   ..................................................................
;;   <       | Pops the memory stack's top element, here norned "a",
;;           | pops the new top element, "b", determines whether a is
;;           | strictly less than b, and pushes an integer number
;;           | signifying the relationship's satisfaction:
;;           | 
;;           |   (a) If a and b differ in their types, always pushes
;;           |       the negative discriminator zero (0).
;;           |   
;;           |   (b) If a and b both represent integer numbers, pushes
;;           |       the integer number one (1) if a is strictly less
;;           |       than b; otherwise pushes zero (0).
;;           |   
;;           |   (c) If a and b both represent strings, pushes the
;;           |       integer number one (1) if a is strictly less
;;           |       than b in a lexicographic manner; otherwise pushes
;;           |       zero (0).
;;           |   
;;           |   (d) If a and b both represent arrays, pushes the
;;           |       integer number one (1) if the arrays are of equal
;;           |       length and all elements in a are strictly less
;;           |       than those in b at the corresponding position;
;;           |       otherwise pushes zero (0).
;;   ..................................................................
;;   ?       | Pops the memory stack's three top elements, here norned,
;;           | in the exact order of their removals, "a", "b", and "c";
;;           | if c represents a Boolean truth value of "true", pushes
;;           | b onto the stack top; otherwise pushes a.
;;           |---------------------------------------------------------
;;           | The following nomothesy governs the admission of an
;;           | object as a Boolean "true" value:
;;           | 
;;           |   (a) An integer value not equal to zero (0) is
;;           |       considered "true"; a zero-valued number produces
;;           |       "false".
;;           |   
;;           |   (b) A string object always produces a "true" response.
;;           |   
;;           |   (c) An array object always produces a "true" response.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()
;;           |   let c <- memoryStack.pop()
;;           |   
;;           |   if c is true then
;;           |     memoryStack.push(b)
;;           |   else
;;           |     memoryStack.push(a)
;;           |   end if
;;   ==================================================================
;;   ARRAY ATTRECTATION
;;   ------------------------------------------------------------------
;;   .       | Pushes a fresh empty array onto the memory stack.
;;   ..................................................................
;;   ,       | Pops the memory stack's top element, expecting thilk to
;;           | represent an array, and pushes the array's length onto
;;           | the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a <- memoryStack.pop()   {Expects an array.}
;;           |   let b <- a.length
;;           |   memoryStack.push(b)
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;           |---------------------------------------------------------
;;           | If the acquired memory stack element does not represent
;;           | an array, an error of the type "InvalidObjectError" is
;;           | signaled.
;;   ..................................................................
;;   @       | Pops the memory stack's top element, here nevened "a",
;;           | pops the new top element, "b", expecting the latter to
;;           | represent an array, and pushes the array element b[a]
;;           | onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()   {Expects an array.}
;;           |   let c <- b[a]
;;           |   memoryStack.push(c)
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;           |---------------------------------------------------------
;;           | If a does not designate a valid index into the array b,
;;           | an error of the type "InvalidArrayIndexError" is
;;           | signaled.
;;   ==================================================================
;;   STACK MANAGEMENT
;;   ------------------------------------------------------------------
;;   \       | Pops and discards the memory stack's top element.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   :       | Duplicates the memory stack's top element.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   ~       | Swaps the memory stack's two top stack elements.
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ==================================================================
;;   TAPE CASTALDY
;;   ------------------------------------------------------------------
;;   (       | Pops the memory stack's top element, here nevened "a",
;;           | pops the new top element, "b", expecting the latter to
;;           | represents an integer number, and stores the object a in
;;           | the memory tape cell amenable to the index b.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- memoryStack.pop()
;;           |   let b <- memoryStack.pop()   {Expects an integer.}
;;           |   memoryTape[b] <- a
;;           |---------------------------------------------------------
;;           | If the memory stack cannot accommodate at least two
;;           | elements, the illicit removal attempt will instigate an
;;           | "EmptyStackError".
;;   ..................................................................
;;   )       | Pops the memory stack's top element, here nevened "a",
;;           | and expected to represents an integer number, and pushes
;;           | the memory tape cell value stored at the index a onto
;;           | the stack.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ==================================================================
;;   INPUT COMMERCE
;;   ------------------------------------------------------------------
;;   $       | Queries the standard input conduit for a line of input
;;           | and pushes the thus received response as a string onto
;;           | the memory stack.
;;   ==================================================================
;;   DYNAMIC EXECUTION
;;   ------------------------------------------------------------------
;;   ;       | Pops the memory stack's top element, expecting thilk to
;;           | represent a string, and evaluates and executes the
;;           | incorporated characters as an sb program.
;;           |---------------------------------------------------------
;;           | This command effectively capacitates the execution of a
;;           | sub-program, the desition of whom concurs with its
;;           | personal contents' exhaustion.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   {       | Peeks without removing the memory stack's top element;
;;           | while the same represents a Boolean truth value of
;;           | "true", repeatedly executes the commands atwixen this
;;           | "{" and the concluding "}" command. If the subject of
;;           | the docimasy produces a Boolean "false" response, the
;;           | instruction pointer (IP) is relocated ayond the "}"
;;           | demarcation.
;;           |---------------------------------------------------------
;;           | The following nomothesy governs the admission of an
;;           | object as a Boolean "true" value:
;;           | 
;;           |   (a) An integer value not equal to zero (0) is
;;           |       considered "true"; a zero-valued number produces
;;           |       "false".
;;           |   
;;           |   (b) A string object always produces a "true" response.
;;           |   
;;           |   (c) An array object always produces a "true" response.
;;           |---------------------------------------------------------
;;           | The stack's top element is perquired upon each inchoacy
;;           | of this iterance construct.
;;           |---------------------------------------------------------
;;           | If the memory stack is empty at the instant of this
;;           | operation's invocation, an error of the type
;;           | "EmptyStackError" is signaled.
;;   ..................................................................
;;   }       | Relocates the instruction pointer (IP) to the matching
;;           | "{" token.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This sb interpreter at hand has been implemented in the programming
;; language Common Lisp, its operations' patefaction a twyfold tiers'
;; champarty: Imprimis, a reformulation of the ensconced commands in a
;; more conable class plasmature transpires; ere in the next stage the
;; actual execution, adhibited upon these dedicated entities, furnishes
;; the interpretation's patration.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-06-18
;; 
;; Sources:
;;   [esolang:2025:sb]
;;   The Esolang contributors, "sb", April 4th, 2025
;;   URL: "https://esolangs.org/wiki/Sb"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list whose componency ensues
   from an ordered catena enumerating zero or more elements, each member
   among these complies to the ELEMENT-TYPE, the default configuration
   associated with whom imposes the generic sentinel ``*''."
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
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entires, everichon among these a twissel
   compound of a key complying with the KEY-TYPE and an affiliated
   value subsumed into the VALUE-TYPE, for both constitutes the generic
   sentinel ``*'' the default configuration."
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
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-kind ()
  "The ``command-kind'' type enumerates the recognized variation on sb
   command identifiers."
  '(member
    ;; Literals.
    :push-character
    :push-integer
    :push-string
    ;; Unary operators.
    :negate
    :logical-not
    ;; Binary operators.
    :add
    :subtract
    :multiply
    :divide
    :remainder
    :concatenate
    ;; Relational operators.
    :equals
    :less-than
    :ternary-conditional
    ;; Array handling.
    :push-empty-array
    :push-array-length
    :push-array-element
    ;; Memory stack management.
    :pop
    :duplicate
    :swap
    ;; Memory tape handling.
    :write-to-tape
    :load-from-tape
    ;; Input and output.
    :input-line
    ;; Dynamic execution.
    :evaluate
    ;; Control flow duction.
    :start-while-loop
    :end-while-loop
    ;; Non-operations.
    :nop))

;;; -------------------------------------------------------

(deftype command-vector ()
  "The ``command-vector'' type defines an ordered, random-access
   catena of parsed sb commands as a one-dimensional simple array
   amplecting ``Command'' instances, enumerating commencing with the
   inclusive subscript zero (0)."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype loop-table ()
  "The ``loop-table'' type defines a bidirectional association atwixen
   the \"while\" loop end points partaking in an sb program by
   adminiculum of their parsed command representations' zero-based
   indices, manifesting in a hash table whose keys and values both
   assume ``fixnum'' position designators."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype program-stack ()
  "The ``program-stack'' type defines a last-in-first-out conditory
   concredited with the castaldy of both an sb execution's main program
   and the various sub-programs, the top element's wike the currently
   active execution context's furnishment."
  '(list-of Program))

;;; -------------------------------------------------------

(deftype memory-stack ()
  "The ``memory-stack'' type defines the sb program memory's stack
   component as a last in, first out salvatory of integers numbers,
   strings, and arrays themselves composed of the trisulc contingency;
   its realization a linked list compact of ``Sb-Object'' instances."
  '(list-of Sb-Object))

;;; -------------------------------------------------------

(deftype memory-tape ()
  "The ``memory-tape'' defines the sb program memory's tape component
   as a vector admitting in its circumference integer numbers, strings,
   as well as arrays themselves composed of the trisulc contingency;
   these elements' amenability edified upon subscripts desumes from the
   signed integer range; the patefaction of such data structure enstated
   by a sparse representation in a hash table, the keys of whom furnish
   the integer indices, while the values resolve to ``Sb-Object''
   instances."
  '(hash-table-of integer Sb-Object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean paregal thereof, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, produces ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the iterance operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro while (antecedent &body body)
  "Repeatedly executes the BODY forms while the ANTECEDENT produces a
   \"generalized boolean\" value of \"true\", and finally returns the
   ``NIL'' value."
  `(the null
     (loop while ,antecedent do
       (progn ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Sb-Object classes.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Sb-Object)
  "The ``Sb-Object'' interface serves in a common firmament's
   instatement for those classes to whom an sb data object's
   representation constitutes the paravaunt telos.")

;;; -------------------------------------------------------

(defstruct (Sb-Array
  (:include     Sb-Object)
  (:constructor make-an-empty-sb-array ())
  (:constructor make-an-sb-array-from (elements)))
  "The ``Sb-Array'' class applies itself to the encapsulation of a
   one-dimensional, ordered, random-access salvatory in an sb-conformant
   plasmature."
  (elements (make-array 0
              :element-type     'Sb-Object
              :initial-contents NIL
              :adjustable       NIL
              :fill-pointer     NIL)
            :type      (simple-array Sb-Object (*))
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Sb-Integer
  (:include     Sb-Object)
  (:constructor make-an-sb-integer (&optional (value 0))))
  "The ``Sb-Integer'' class applies itself to the encapsulation of a
   signed or unsigned integral datum in an sb-conformant plasmature."
  (value (error "No value has been specified.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Sb-Null
  (:include     Sb-Object)
  (:constructor make-an-sb-null ()))
  "The ``Sb-Null'' class serves as the delegate of an absent or
   undefined ``Sb-Object'' instance, or \"null\" sentinel.")

;;; -------------------------------------------------------

(defstruct (Sb-String
  (:include     Sb-Object)
  (:constructor make-an-sb-string (&optional (content "")))
  (:constructor format-into-an-sb-string
    (format-control
     &rest format-arguments
     &aux  (content
             (apply #'format NIL format-control format-arguments)))))
  "The ``Sb-String'' class applies itself to the encapsulation of a
   character sequence in an sb-conformant plasmature."
  (content (error "No content has been specified.")
           :type      string
           :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Sb-Object global constants.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Sb-Null    +SB-NULL+))
(declaim (type Sb-Integer +SB-INTEGER-ZERO+))
(declaim (type Sb-Integer +SB-INTEGER-ONE+))
(declaim (type Sb-String  +SB-EMPTY-STRING+))

;;; -------------------------------------------------------

(defparameter +SB-NULL+
  (make-an-sb-null)
  "The global singleton instance of the ``Sb-Null'' object, its dever
   the communication of a datum's carency.")

(defparameter +SB-INTEGER-ZERO+
  (make-an-sb-integer 0)
  "A global instance of the integer value zero (0) ensconced in an
   ``Sb-Integer'' object.")

(defparameter +SB-INTEGER-ONE+
  (make-an-sb-integer 1)
  "A global instance of the integer value one (1) ensconced in an
   ``Sb-Integer'' object.")

(defparameter +SB-EMPTY-STRING+
  (make-an-sb-string "")
  "A global instance of the empty or null string, '\"', ensconced in an
   ``Sb-String'' object.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical sb object operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sb-object-is-true-p (object)
  "Determines whether the sb OBJECT corresponds to a Boolean truth value
   of \"true\", returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Sb-Object object))
  (the boolean
    (not
      (and
        (sb-integer-p object)
        (zerop (sb-integer-value object))))))

;;; -------------------------------------------------------

(defun convert-into-a-boolean-sb-integer (object)
  "Returns a globally shared``Sb-Integer'' which encodes the OBJECT's
   truth value, a \"generalized boolean\" input of \"true\"
   corresponding to a one-valued (1) response, while a \"false\"
   sentinel produces a zero (0) instance."
  (declare (type T object))
  (the Sb-Integer
    (if object
      +SB-INTEGER-ONE+
      +SB-INTEGER-ZERO+)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sb array operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tally-the-sb-array-length (array)
  "Returns the tally of elements to whom a commorancy in the sb ARRAY
   is vouchsafed."
  (declare (type Sb-Array array))
  (the Sb-Integer
    (make-an-sb-integer
      (length
        (sb-array-elements array)))))

;;; -------------------------------------------------------

(defun prepend-to-the-sb-array (array new-element)
  "Inserts the NEW-ELEMENT at the sb ARRAY's front position and returns
   a fresh ``Sb-Array'' entailing the result.
   ---
   The input ARRAY is not subjected to any alterations."
  (declare (type Sb-Array  array))
  (declare (type Sb-Object new-element))
  (the Sb-Array
    (make-an-sb-array-from
      (concatenate
        '(simple-array Sb-Object (*))
        (list              new-element)
        (sb-array-elements array)))))

;;; -------------------------------------------------------

(defun append-to-the-sb-array (array new-element)
  "Inserts the NEW-ELEMENT at the sb ARRAY's rear position and returns
   a fresh ``Sb-Array'' entailing the result.
   ---
   The input ARRAY is not subjected to any alterations."
  (declare (type Sb-Array  array))
  (declare (type Sb-Object new-element))
  (the Sb-Array
    (make-an-sb-array-from
      (concatenate
        '(simple-array Sb-Object (*))
        (sb-array-elements array)
        (list              new-element)))))

;;; -------------------------------------------------------

(defun concatenate-the-sb-arrays (first-array second-array)
  "Appends the SECOND-ARRAY's elements to the FIRST-ARRAY's contents and
   returns a fresh ``Sb-Array'' entailing the conjoined items.
   ---
   Neither the FIRST-ARRAY nor the SECOND-ARRAY are subjected to any
   alterations."
  (declare (type Sb-Array first-array))
  (declare (type Sb-Array second-array))
  (the Sb-Array
    (make-an-sb-array-from
      (concatenate
        '(simple-array Sb-Object (*))
        (sb-array-elements first-array)
        (sb-array-elements second-array)))))

;;; -------------------------------------------------------

(defun retrieve-the-sb-array-element-at (array index)
  "Returns the sb ARRAY empight at the zero-based INDEX; or, upon the
   subscript's transgression of the admissible bournes, signals an
   error of the type ``Invalid-Array-Index-Error''."
  (declare (type Sb-Array array))
  (declare (type fixnum   index))
  (the Sb-Object
    (if (array-in-bounds-p (sb-array-elements array) index)
      (aref (sb-array-elements array) index)
      (error 'Invalid-Array-Index-Error :offending-index index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sb integer operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-the-sb-integers (operator first-number second-number)
  "Applies the OPERATOR to the FIRST-NUMBER and SECOND-NUMBER and
   returns a fresh ``Sb-Integer'' encompassing the result."
  (declare (type (function (integer integer) integer) operator))
  (declare (type Sb-Integer                           first-number))
  (declare (type Sb-Integer                           second-number))
  (the Sb-Integer
    (make-an-sb-integer
      (funcall operator
        (sb-integer-value first-number)
        (sb-integer-value second-number)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sb object relational operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric sb-objects-equal-p (first-object second-object)
  (:documentation
    "Determines whether the FIRST-OBJECT and SECOND-OBJECT may be
     considered as tantamounts, returning on confirmation a ``boolean''
     value of ``T'', otherwise ``NIL''.")
  
  (:method ((first-object Sb-Object) (second-object Sb-Object))
    (declare (type Sb-Object first-object))
    (declare (ignore         first-object))
    (declare (type Sb-Object second-object))
    (declare (ignore         second-object))
    (the boolean NIL))
  
  (:method ((first-object Sb-Array) (second-object Sb-Array))
    (declare (type Sb-Array first-object))
    (declare (type Sb-Array second-object))
    (the boolean
      (and
        (= (tally-the-sb-array-length first-object)
           (tally-the-sb-array-length second-object))
        (every
          #'sb-objects-equal-p
          (sb-array-elements first-object)
          (sb-array-elements second-object)))))
  
  (:method ((first-object Sb-Integer) (second-object Sb-Integer))
    (declare (type Sb-Integer first-object))
    (declare (type Sb-Integer second-object))
    (the boolean
      (convert-into-a-boolean-value
        (= (sb-integer-value first-object)
           (sb-integer-value second-object)))))
  
  (:method ((first-object Sb-String) (second-object Sb-String))
    (declare (type Sb-String first-object))
    (declare (type Sb-String second-object))
    (the boolean
      (convert-into-a-boolean-value
        (string=
          (sb-string-content first-object)
          (sb-string-content second-object))))))

;;; -------------------------------------------------------

(defgeneric sb-object-is-less-than-p (first-object second-object)
  (:documentation
    "Determines whether the FIRST-OBJECT may be considered as strictly
     less than the SECOND-OBJECT, complying with a sensible and
     covenable species of nomothesia, returning on confirmation a
     ``boolean'' value of ``T'', otherwise ``NIL''.")
  
  (:method ((first-object Sb-Object) (second-object Sb-Object))
    (declare (type Sb-Object first-object))
    (declare (ignore         first-object))
    (declare (type Sb-Object second-object))
    (declare (ignore         second-object))
    (the boolean NIL))
  
  (:method ((first-object Sb-Array) (second-object Sb-Array))
    (declare (type Sb-Array first-object))
    (declare (type Sb-Array second-object))
    (the boolean
      (and
        (= (tally-the-sb-array-length first-object)
           (tally-the-sb-array-length second-object))
        (every
          #'sb-object-is-less-than-p
          (sb-array-elements first-object)
          (sb-array-elements second-object)))))
  
  (:method ((first-object Sb-Integer) (second-object Sb-Integer))
    (declare (type Sb-Integer first-object))
    (declare (type Sb-Integer second-object))
    (the boolean
      (convert-into-a-boolean-value
        (< (sb-integer-value first-object)
           (sb-integer-value second-object)))))
  
  (:method ((first-object Sb-String) (second-object Sb-String))
    (declare (type Sb-String first-object))
    (declare (type Sb-String second-object))
    (the boolean
      (convert-into-a-boolean-value
        (string<
          (sb-string-content first-object)
          (sb-string-content second-object))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sb object printing operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((object Sb-Array) (destination T))
  (declare (type Sb-Array object))
  (declare (type stream   destination))
  (loop
    initially
      (format destination "(Sb-Array [")
    
    for current-element
      of-type Sb-Object
      across  (sb-array-elements object)
    and prevenient-element-exists-p
      of-type boolean
      =       NIL
      then    T
    do
      (format destination "~@[, ~*~]~a"
        prevenient-element-exists-p
        current-element)
    
    finally
      (format destination "])"))
  (the Sb-Array object))

;;; -------------------------------------------------------

(defmethod print-object ((object Sb-Integer) (destination T))
  (declare (type Sb-Integer object))
  (declare (type stream     destination))
  (format destination "(Sb-Integer ~d)"
    (sb-integer-value object))
  (the Sb-Integer object))

;;; -------------------------------------------------------

(defmethod print-object ((object Sb-Null) (destination T))
  (declare (type Sb-Null object))
  (declare (type stream  destination))
  (format destination "(Sb-Null)")
  (the Sb-Null object))

;;; -------------------------------------------------------

(defmethod print-object ((object Sb-String) (destination T))
  (declare (type Sb-String object))
  (declare (type stream    destination))
  (format destination "(Sb-String ~s)"
    (sb-string-content object))
  (the Sb-String object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the binary operation interface.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-the-binary-operator (operator
                                       left-operand
                                       right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and RIGHT-OPERAND
     in this exact order and returns a conable ``Sb-Object'' result."))

;;; -------------------------------------------------------

(defmacro define-a-binary-operation
    ((operator left-operand-type right-operand-type)
     &body body)
  "Establishes an implementation of the generic function
   ``apply-the-binary-operator'', the first formal parameter's
   agnomination is automatically generated, dispatching on an
   ``eql''-equiparation with the OPERATOR, the second assumes the fixed
   stevening ``$left'', specializing on the LEFT-OPERAND-TYPE, while
   the third and final's acquisition limns a twissel of the identifier
   ``$right'' in conjunction with the RIGHT-OPERAND-TYPE; executes the
   BODY forms, and returns the desinent form's results."
  `(defmethod apply-the-binary-operator
       (($operator (eql ,operator))
        ($left     ,left-operand-type)
        ($right    ,right-operand-type))
     (declare (type keyword             $operator))
     (declare (ignorable                $operator))
     (declare (type ,left-operand-type  $left))
     (declare (ignorable                $left))
     (declare (type ,right-operand-type $right))
     (declare (ignorable                $right))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary arithmetics.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-binary-operation (:add Sb-Integer Sb-Integer)
  (the Sb-Integer
    (combine-the-sb-integers #'+ $left $right)))

;;; -------------------------------------------------------

(define-a-binary-operation (:subtract Sb-Integer Sb-Integer)
  (the Sb-Integer
    (combine-the-sb-integers #'- $left $right)))

;;; -------------------------------------------------------

(define-a-binary-operation (:multiply Sb-Integer Sb-Integer)
  (the Sb-Integer
    (combine-the-sb-integers #'* $left $right)))

;;; -------------------------------------------------------

(define-a-binary-operation (:divide Sb-Integer Sb-Integer)
  (the Sb-Integer
    (combine-the-sb-integers #'truncate $left $right)))

;;; -------------------------------------------------------

(define-a-binary-operation (:remainder Sb-Integer Sb-Integer)
  (the Sb-Integer
    (combine-the-sb-integers #'rem $left $right)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary concatenation.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-binary-operation (:concatenate Sb-Integer Sb-Integer)
  (the Sb-String
    (format-into-an-sb-string "~d~d"
      (sb-integer-value $left)
      (sb-integer-value $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Integer Sb-Null)
  (the Sb-String
    (format-into-an-sb-string "~d"
      (sb-integer-value $left))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Integer Sb-String)
  (the Sb-String
    (format-into-an-sb-string "~d~a"
      (sb-integer-value  $left)
      (sb-string-content $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Null Sb-Integer)
  (the Sb-String
    (format-into-an-sb-string "~d"
      (sb-integer-value $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Null Sb-Null)
  (the Sb-String +SB-EMPTY-STRING+))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Null Sb-String)
  (the Sb-String
    (format-into-an-sb-string "~a"
      (sb-string-content $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-String Sb-Integer)
  (the Sb-String
    (format-into-an-sb-string "~a~d"
      (sb-string-content $left)
      (sb-integer-value  $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-String Sb-Null)
  (the Sb-String
    (format-into-an-sb-string "~a"
      (sb-string-content $left))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-String Sb-String)
  (the Sb-String
    (format-into-an-sb-string "~a~a"
      (sb-string-content $left)
      (sb-string-content $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Array Sb-Object)
  (the Sb-Array
    (append-to-the-sb-array $left $right)))

;;; -------------------------------------------------------

(define-a-binary-operation (:concatenate Sb-Object Sb-Array)
  (the Sb-Array
    (prepend-to-the-sb-array $right $left)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary comparisons.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-binary-operation (:equals Sb-Object Sb-Object)
  (the Sb-Integer
    (convert-into-a-boolean-sb-integer
      (sb-objects-equal-p $left $right))))

;;; -------------------------------------------------------

(define-a-binary-operation (:less-than Sb-Object Sb-Object)
  (the Sb-Integer
    (convert-into-a-boolean-sb-integer
      (sb-object-is-less-than-p $left $right))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Sb-Error (error)
  ()
  (:documentation
    "The ``Sb-Error'' condition type serves to enstate a entreparted
     foundry for all conditions appertaining to an sb program's
     reception, lexical evaluation, parsing, and execution."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Sb-Error)
  ()
  (:report "You cannot peek into or pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the
     representation of an anomalous situation whose etiology is located
     in the attempt to peek into or pop from an empty stack."))

;;; -------------------------------------------------------

(define-condition Malformed-Literal-Error (Sb-Error)
  ((data-type
    :initarg       :data-type
    :initform      (error "No literal type has been specified.")
    :reader        malformed-literal-error-data-type
    :type          simple-string
    :documentation "A descriptive string designating the literal datum's
                    type.")
   (start-position
    :initarg       :start-position
    :initform      (error "No start position has been specified.")
    :reader        malformed-literal-error-start-position
    :type          fixnum
    :documentation "The zero-based start position of the malformed
                    literal datum in the underlying sb source code
                    string."))
  (:report
    (lambda (condition destination)
      (declare (type Malformed-Literal-Error condition))
      (declare (type stream                   destination))
      (format destination
        "The ~a literal, commencing at the position ~d, has not been ~
         properly terminated."
        (malformed-literal-error-data-type      condition)
        (malformed-literal-error-start-position condition))))
  (:documentation
    "The ``Malformed-Literal-Error'' condition type serves in the
     apprizal about an sb literal datum statement being deprehended in
     an anomalous conformation, typically incomplete in its syntactical
     designment."))

;;; -------------------------------------------------------

(define-condition Invalid-Array-Index-Error (Sb-Error)
  ((offending-index
    :initarg       :offending-index
    :initform      (error "No offending index has been specified.")
    :reader        invalid-array-index-error-offending-index
    :type          fixnum
    :documentation "The invalid index attempted for an sb array's
                    indagation or modification."))
  (:report
    (lambda (condition destination)
      (declare (type Invalid-Array-Index-Error condition))
      (declare (type stream                    destination))
      (format destination
        "The subscript ~d does not specify a valid index into the ~
         array."
        (invalid-array-index-error-offending-index condition))))
  (:documentation
    "The ``Invalid-Array-Index-Error'' condition type serves in the
     apprizal about an illicit access attempt airted the perquisition
     or modification of an sb array object via an invalid subscript."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global operand constants.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Sb-Null +NO-OPERAND+))

;;; -------------------------------------------------------

(defparameter +NO-OPERAND+ +SB-NULL+
  "A sentinel which signifies an operand's lacuna.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the "Command" class.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-a-command (kind
                                &optional (operand +NO-OPERAND+))))
  "The ``Command'' class serves in the encapsulation of an sb command,
   its diorism's foundry a jumelle of the categorizing kind and an
   optional operand."
  (kind    (error "No command kind has been specified.")
           :type      command-kind
           :read-only T)
  (operand +NO-OPERAND+
           :type      Sb-Object
           :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command definition operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-niladic-command
    (variable-name
     kind
     causatum-description
     &optional (identifier NIL identifier-supplied-p))
  "Defines a global constant, the agnomination apportioned to whom
   ensues from the VARIABLE-NAME, its bound value a fresh ``Command''
   instance, conjoining it this plasmature the KIND, but no operand,
   and assigning to the thus established constant a documentation string
   composed of a standard text, assembled from the CAUSATUM-DESCRIPTION
   and the IDENTIFIER.
   ---
   The gendrure of the command subclass' documentation ensues from an
   assemblage which embeds the CAUSATUM-DESCRIPTION and IDENTIFIER in
   the forbisen
     \"Represents the sb {CAUSATUM-DESCRIPTION} command {IDENTIFIER}.\""
  `(progn
     (declaim (type Command ,variable-name))
     (defparameter ,variable-name
       (make-a-command ,kind)
       ,(format NIL "Represents the sb ~a command~@[ ~*\"~a\"~]."
          causatum-description
          identifier-supplied-p
          identifier))
     (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global command constants.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unary operators.
(define-a-niladic-command +NEGATE-COMMAND+
  :negate
  "negation" "_")
(define-a-niladic-command +LOGICAL-NOT-COMMAND+
  :logical-not
  "logical negation" "!")

;; Binary operators.
(define-a-niladic-command +ADD-COMMAND+
  :add
  "addition" "+")
(define-a-niladic-command +SUBTRACT-COMMAND+
  :subtract
  "subtraction" "-")
(define-a-niladic-command +MULTIPLY-COMMAND+
  :multiply
  "multiplication" "*")
(define-a-niladic-command +DIVIDE-COMMAND+
  :divide
  "division" "/")
(define-a-niladic-command +REMAINDER-COMMAND+
  :remainder
  "remainder supputation" "%")
(define-a-niladic-command +CONCATENATE-COMMAND+
  :concatenate
  "string and array concatenation" "&")

;; Relational operators.
(define-a-niladic-command +EQUALS-COMMAND+
  :equals
  "equality check" "=")
(define-a-niladic-command +LESS-THAN-COMMAND+
  :less-than
  "less than check" "<")
(define-a-niladic-command +TERNARY-CONDITIONAL-COMMAND+
  :ternary-conditional
  "ternary conditional" "?")

;; Array handling.
(define-a-niladic-command +PUSH-EMPTY-ARRAY-COMMAND+
  :push-empty-array
  "empty array push" ".")
(define-a-niladic-command +PUSH-ARRAY-ELEMENT-COMMAND+
  :push-array-element
  "array element push" "@")
(define-a-niladic-command +PUSH-ARRAY-LENGTH-COMMAND+
  :push-array-length
  "array length push" ",")

;; Stack management.
(define-a-niladic-command +POP-COMMAND+
  :pop
  "stack pop" "\\")
(define-a-niladic-command +DUPLICATE-COMMAND+
  :duplicate
  "stack duplication" ":")
(define-a-niladic-command +SWAP-COMMAND+
  :swap
  "stack top element exchange" "~")

;; Tape management.
(define-a-niladic-command +WRITE-TO-TAPE-COMMAND+
  :write-to-tape
  "stack-to-memory transfer" "(")
(define-a-niladic-command +LOAD-FROM-TAPE-COMMAND+
  :load-from-tape
  "memory-to-stack transfer" ")")

;; Input and output.
(define-a-niladic-command +INPUT-LINE-COMMAND+
  :input-line
  "line input" "$")

;; Dynamic code execution.
(define-a-niladic-command +EVALUATE-COMMAND+
  :evaluate
  "dynamic code evaluation" ";")

;; Control flow duction.
(define-a-niladic-command +START-WHILE-LOOP-COMMAND+
  :start-while-loop
  "while loop commencement" "{")
(define-a-niladic-command +END-WHILE-LOOP-COMMAND+
  :end-while-loop
  "while loop end" "{")

;; Non-operations.
(define-a-niladic-command +NOP-COMMAND+
  :nop
  "no-operation (NOP)")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the "Lexer" class.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No source has been communicated.")
    :type          simple-string
    :documentation "The piece of sb source code to analyze.")
   (current-position
    :initform      0
    :type          fixnum
    :documentation "The zero-based index designating the currently
                    selected character in the SOURCE string.")
   (current-character
    :initform      #\Null
    :type          character
    :documentation "The character at the CURRENT-POSITION into the
                    SOURCE string.")
   (exhausted-p
    :type          boolean
    :documentation "A Boolean flag which is \"true\" if the SOURCE has
                    been processed in its entirety, otherwise
                    \"false\"."))
  (:documentation
    "The ``Lexer'' class applies itself to a lexical analyzer's
     accoutrement, the bailiwick of whose assignment enstates the
     morcellation of a piece of sb source code into its pertinent
     constituents, the commands."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the lexer operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-the-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots to local symbol macros,
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   The following correpondences will be enstated atwixen the local
   symbol macros and their represented ``Lexer'' slot names.
     ---------------------------------------
     Symbol macro       | Slot name
     -------------------+-------------------
     $source            | source
     .......................................
     $current-position  | current-position
     .......................................
     $current-character | current-character
     .......................................
     $exhausted-p       | exhausted-p
     ---------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (with-slots (($source            source)
                    ($current-position  current-position)
                    ($current-character current-character)
                    ($exhausted-p       exhausted-p))
           ,evaluated-lexer
         (declare (type simple-string $source))
         (declare (ignorable          $source))
         (declare (type fixnum        $current-position))
         (declare (ignorable          $current-position))
         (declare (type character     $current-character))
         (declare (ignorable          $current-character))
         (declare (type boolean       $exhausted-p))
         (declare (ignorable          $exhausted-p))
         (flet
             ((current-character-equals-p (expected-character)
               "Determines whether the LEXER's current character equals
                the EXPECTED-CHARACTER, returning on confirmation a
                ``boolean'' value of ``T'', otherwise ``NIL''."
               (declare (type character expected-character))
               (the boolean
                 (convert-into-a-boolean-value
                   (and
                     (not $exhausted-p)
                     (char= $current-character expected-character)))))
              (current-character-satisfies-p (predicate)
               "Determines whether the LEXER's current character
                satisfies the PREDICATE, returning on confirmation a
                ``boolean'' value of ``T'', otherwise ``NIL''.
                ---
                The PREDICATE ought to enstate a monadic function, its
                sole argument a character to probe, its return value a
                \"generalized boolean\" signification of the input's
                compatibility with the antecedent's stipulations."
               (declare (type (function (character) *) predicate))
               (the boolean
                 (convert-into-a-boolean-value
                   (and
                     (not $exhausted-p)
                     (funcall predicate $current-character))))))
           ,@body)))))

;;; -------------------------------------------------------

(defun update-the-lexer-state (lexer)
  "Updates the LEXER's current character and exhaustion flag in response
   to the currently occupied position into its source string and returns
   no value."
  (declare (type Lexer lexer))
  (with-the-lexer (lexer)
    (if (array-in-bounds-p $source $current-position)
      (psetf
        $current-character (schar $source $current-position)
        $exhausted-p       NIL)
      (setf $exhausted-p T)))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's current character and exhaustion flag and
   returns no value."
  (declare (type Lexer lexer))
  (update-the-lexer-state lexer)
  (values))

;;; -------------------------------------------------------

(defun prepare-a-lexer-for (source)
  "Creates and returns a fresh ``Lexer'' dedicated to the sb SOURCE
   string's morcellation into its commands."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer
      :source (coerce source 'simple-string))))

;;; -------------------------------------------------------

(defun advance-to-the-next-character (lexer)
  "Advances the LEXER's position cursor to the next character in its
   underlying source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-the-lexer (lexer)
    (incf $current-position)
    (update-the-lexer-state lexer))
  (values))

;;; -------------------------------------------------------

(defun read-a-character-literal (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a single character literal and returns a ``:character''
   command representation thereof."
  (declare (type Lexer lexer))
  (the Command
    (make-a-command :push-character
      (with-the-lexer (lexer)
        (advance-to-the-next-character lexer)
        (if $exhausted-p
          (error 'Malformed-Literal-Error
            :data-type      "character"
            :start-position (1- $current-position))
          (make-an-sb-string
            (prog1
              (string $current-character)
              (advance-to-the-next-character lexer))))))))

;;; -------------------------------------------------------

(defun read-an-integer-literal (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an unsigned integer literal and returns an ``:integer''
   command encapsulation thereof."
  (declare (type Lexer lexer))
  (the Command
    (make-a-command :push-integer
      (with-the-lexer (lexer)
        (make-an-sb-integer
          (parse-integer $source
            :start $current-position
            :end
              (progn
                (while (current-character-satisfies-p #'digit-char-p)
                  (advance-to-the-next-character lexer))
                $current-position)))))))

;;; -------------------------------------------------------

(defun read-a-string-literal (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes a string literal and returns a ``:string'' command
   encapsulation of its intext."
  (declare (type Lexer lexer))
  (the Command
    (make-a-command :push-string
      (with-the-lexer (lexer)
        (make-an-sb-string
          (with-output-to-string (intext)
            (declare (type string-stream intext))
            (loop
              with start-point
                of-type fixnum
                =       (prog1 $current-position
                          (advance-to-the-next-character lexer))
              if $exhausted-p do
                (error 'Malformed-Literal-Error
                  :data-type      "string"
                  :start-position start-point)
              else if (current-character-equals-p #\") do
                (advance-to-the-next-character lexer)
                (loop-finish)
              else do
                (write-char $current-character intext)
                (advance-to-the-next-character lexer)
              end)))))))

;;; -------------------------------------------------------

(defun read-a-niladic-command (lexer command)
  "Consumes the LEXER's current character, assembles and returns a fresh
   command via a ligation of the COMMAND-KIND with the extracted symbol
   as its value, and advances to the next character in the LEXER's
   source, if possible."
  (declare (type Lexer   lexer))
  (declare (type Command command))
  (the Command
    (prog1 command
      (advance-to-the-next-character lexer))))

;;; -------------------------------------------------------

(defun extract-the-next-command (lexer)
  "Returns the next command from the LEXER.
   ---
   Upon its underlying source's exhaustion, the LEXER responds to any
   further request with the ``NIL'' sentinel."
  (declare (type Lexer lexer))
  (the (or null Command)
    (with-the-lexer (lexer)
      (cond
        ($exhausted-p
          NIL)
        
        ;; Literals.
        ((current-character-satisfies-p #'digit-char-p)
          (read-an-integer-literal lexer))
        ((current-character-equals-p #\")
          (read-a-string-literal lexer))
        ((current-character-equals-p #\')
          (read-a-character-literal lexer))
        
        ;; Unary operators.
        ((current-character-equals-p #\_)
          (read-a-niladic-command lexer +NEGATE-COMMAND+))
        ((current-character-equals-p #\!)
          (read-a-niladic-command lexer +LOGICAL-NOT-COMMAND+))
        
        ;; Binary operators.
        ((current-character-equals-p #\+)
          (read-a-niladic-command lexer +ADD-COMMAND+))
        ((current-character-equals-p #\-)
          (read-a-niladic-command lexer +SUBTRACT-COMMAND+))
        ((current-character-equals-p #\*)
          (read-a-niladic-command lexer +MULTIPLY-COMMAND+))
        ((current-character-equals-p #\/)
          (read-a-niladic-command lexer +DIVIDE-COMMAND+))
        ((current-character-equals-p #\%)
          (read-a-niladic-command lexer +REMAINDER-COMMAND+))
        ((current-character-equals-p #\&)
          (read-a-niladic-command lexer +CONCATENATE-COMMAND+))
        ((current-character-equals-p #\=)
          (read-a-niladic-command lexer +EQUALS-COMMAND+))
        ((current-character-equals-p #\<)
          (read-a-niladic-command lexer +LESS-THAN-COMMAND+))
        
        ;; Ternary operators.
        ((current-character-equals-p #\?)
          (read-a-niladic-command lexer +TERNARY-CONDITIONAL-COMMAND+))
        
        ;; Stack management.
        ((current-character-equals-p #\;)
          (read-a-niladic-command lexer +EVALUATE-COMMAND+))
        ((current-character-equals-p #\\)
          (read-a-niladic-command lexer +POP-COMMAND+))
        ((current-character-equals-p #\:)
          (read-a-niladic-command lexer +DUPLICATE-COMMAND+))
        ((current-character-equals-p #\~)
          (read-a-niladic-command lexer +SWAP-COMMAND+))
        
        ;; Array handling.
        ((current-character-equals-p #\.)
          (read-a-niladic-command lexer +PUSH-EMPTY-ARRAY-COMMAND+))
        ((current-character-equals-p #\@)
          (read-a-niladic-command lexer +PUSH-ARRAY-ELEMENT-COMMAND+))
        ((current-character-equals-p #\,)
          (read-a-niladic-command lexer +PUSH-ARRAY-LENGTH-COMMAND+))
        
        ;; Memory management.
        ((current-character-equals-p #\()
          (read-a-niladic-command lexer +WRITE-TO-TAPE-COMMAND+))
        ((current-character-equals-p #\))
          (read-a-niladic-command lexer +LOAD-FROM-TAPE-COMMAND+))
        
        ;; Input and output.
        ((current-character-equals-p #\$)
          (read-a-niladic-command lexer +INPUT-LINE-COMMAND+))
        
        ;; Control flow.
        ((current-character-equals-p #\{)
          (read-a-niladic-command lexer +START-WHILE-LOOP-COMMAND+))
        ((current-character-equals-p #\})
          (read-a-niladic-command lexer +END-WHILE-LOOP-COMMAND+))
        
        (T
          (read-a-niladic-command lexer +NOP-COMMAND+))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Program".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((commands
    :initarg       :commands
    :initform      (error "No command vector has been specified.")
    :type          command-vector
    :documentation "A vector comprehending the commands.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as the
                    zero-based index of the active element into the
                    COMMANDS vector.")
   (loop-table
    :initform      (make-hash-table :test #'eql)
    :type          loop-table
    :documentation "Ligates the loop boundaries' representative
                    commands in a bilateral fashion."))
  (:documentation
    "The ``Program'' class applies itself to the encapsulation of an
     sb program or subprogram, its diorism' foundry an ordered sequence
     of its ensconced commands, an instruction pointer (IP) concredited
     their the current member's selection, and a mapping atwixen its
     loop end points."))

;;; -------------------------------------------------------

(defun build-the-loop-table-for (program)
  "Suppuates the jump table for the sb PROGRAM, the same enstates the
   iterance bourne twissels' vincula by adminiculum of their
   representative commands' zero-based indices, stores these connections
   in the PROGRAM, and returns no value."
  (declare (type Program program))
  (with-slots (commands loop-table) program
    (declare (type command-vector commands))
    (declare (type loop-table     loop-table))
    (loop
      initially
        (clrhash loop-table)
      
      with start-indices   of-type (list-of fixnum) =      NIL
      
      for  current-command of-type Command          across commands
      and  current-index   of-type fixnum           from   0
      
      if (eq (command-kind current-command) :start-while-loop) do
        (push current-index start-indices)
      else if (eq (command-kind current-command) :end-while-loop) do
        (if start-indices
          (let ((start-index (pop start-indices))
                (end-index   current-index))
            (declare (type fixnum start-index))
            (declare (type fixnum end-index))
            (psetf
              (gethash start-index loop-table) end-index
              (gethash end-index   loop-table) start-index))
          (error
            (error "An unmatched loop termination point has been ~
                    detected.")))
      end
      
      finally
        (when start-indices
          (error "One or more unmatched loop start points have been ~
                  detected."))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((program Program) &key)
  "Constructs and stores the loop table for the PROGRAM and returns no
   value."
  (declare (type Program program))
  (build-the-loop-table-for program)
  (values))

;;; -------------------------------------------------------

(defun program-is-complete-p (program)
  "Determines whether the PROGRAM's instruction pointer (IP) has
   transitioned ayond the desinent command, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (the boolean
    (with-slots (commands ip) program
      (declare (type command-vector commands))
      (declare (type fixnum         ip))
      (not (array-in-bounds-p commands ip)))))

;;; -------------------------------------------------------

(defun request-the-current-program-command (program)
  "Returns the PROGRAM's currently selected command."
  (declare (type Program program))
  (the Command
    (with-slots (commands ip) program
      (declare (type command-vector commands))
      (declare (type fixnum         ip))
      (aref commands ip))))

;;; -------------------------------------------------------

(defun get-the-program-size (program)
  "Returns the tally of commands comprising the PROGRAM."
  (declare (type Program program))
  (the fixnum
    (length
      (slot-value program 'commands))))

;;; -------------------------------------------------------

(defun advance-the-program-ip (program)
  "Advances the PROGRAM's instruction pointer (IP) to the next command,
   if possible, and returns no value."
  (declare (type Program program))
  (with-slots (ip) program
    (declare (type fixnum ip))
    (setf ip
      (min
        (1+ ip)
        (get-the-program-size program))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-the-opposite-program-loop-point (program)
  "Expecting the PROGRAM's instruction pointer (IP) to currently reside
   on a \"while\" iterance command, relocates the cursor to the opposite
   loop end point and returns no value."
  (declare (type Program program))
  (with-slots (ip loop-table) program
    (setf ip
      (or (gethash ip loop-table)
          (error
            "No loop end point is associated with the position ~d."
            ip))))
  (values))

;;; -------------------------------------------------------

(defun reset-the-program (program)
  "Restores the PROGRAM to its state of inchoacy and returns no value."
  (declare (type Program program))
  (setf (slot-value program 'ip) 0)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((program Program) (destination T))
  (declare (type Program program))
  (declare (type stream  destination))
  (with-slots (commands) program
    (declare (type command-vector commands))
    (loop
      initially
        (format destination "~&Program with ~d commands:"
          (get-the-program-size program))
      for current-command of-type Command across commands
      for command-index   of-type fixnum  from   0
      do
        (format destination "~&~2t~d | ~a"
          command-index current-command)))
  (the Program program))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program parser.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-sb-program (source)
  "Parses the piece of sb SOURCE code and returns a fresh covenable
   ``Program'' representation thereof."
  (declare (type string source))
  (the Program
    (loop
      with lexer
        of-type Lexer
        =       (prepare-a-lexer-for source)
      
      for current-command
        of-type (or null Command)
        =       (extract-the-next-command lexer)
      
      while current-command
        collect current-command into commands
      
      finally
        (return
          (make-instance 'Program :commands
            (coerce commands
              '(simple-array Command (*))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Interpreter".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((programs
    :initarg       :programs
    :type          program-stack
    :documentation "A stack concredited the subprograms' castaldy, the
                    topmost element representative of the currently
                    processed one.")
   (stack
    :initform      NIL
    :type          memory-stack
    :documentation "The sb program memory's stack component.")
   (tape
    :initform      (make-hash-table :test #'eql)
    :type          memory-tape
    :documentation "The sb program memory's tape component."))
  (:documentation
    "The ``Interpreter'' class is apportioned the wike of an sb
     program's execution, accompassing actual efficacy to a enstated
     command sequence consigned to its castaldy."))

;;; -------------------------------------------------------

(defmacro with-the-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots to local symbol macros,
   executes the BODY forms, and returns the desinent form's results.
   ---
   The associations atwixen the thus established symbol macros and the
   representative ``Interpreter'' slot names amplect the following:
     -------------------------
     Symbol macro | Slot name
     -------------+-----------
     $programs    | programs
     .........................
     $stack       | stack
     .........................
     $tape        | tape
     -------------------------
   ---
   As parhedral adminicula, two adscititious local symbol macro bindings
   serve in the establishment of the avails' pleroma:
     ------------------------------------------------------------------
     Symbol macro     | Role
     -----------------+------------------------------------------------
     $current-program | The current program as the first element of
                      | the ``program'' slot.
     ..................................................................
     $current-command | The currently selected command among the
                      | ``$current-program'''s members.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (with-slots (($programs programs)
                    ($stack    stack)
                    ($tape     tape))
           ,evaluated-interpreter
         (declare (type program-stack $programs))
         (declare (ignorable          $programs))
         (declare (type memory-stack  $stack))
         (declare (ignorable          $stack))
         (declare (type memory-tape   $tape))
         (declare (ignorable          $tape))
         (symbol-macrolet
             (($current-program
                (the Program
                  (first $programs)))
              ($current-command
                (the Command
                  (request-the-current-program-command
                    $current-program))))
           (declare (type Program $current-program))
           (declare (ignorable    $current-program))
           (declare (type Command $current-command))
           (declare (ignorable    $current-command))
           ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter constructors.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-an-interpreter-for (main-program)
  "Returns a fresh ``Interpreter'' dedicated to the MAIN-PROGRAM's
   execution."
  (declare (type Program main-program))
  (the Interpreter
    (make-instance 'Interpreter :programs (list main-program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program castaldy operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun advance-the-current-program (interpreter)
  "Advances the instruction pointer (IP) operating on the INTERPRETER's
   currently executed program to the next position in the same and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (advance-the-program-ip $current-program))
  (values))

;;; -------------------------------------------------------

(defun executes-the-main-program-p (interpreter)
  "Determines whether the INTERPRETER's currently executes program
   represents the main specimen, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (convert-into-a-boolean-value
      (with-the-interpreter (interpreter)
        (<= (length $programs) 1)))))

;;; -------------------------------------------------------

(defun current-program-is-complete-p (interpreter)
  "Determines whether the INTERPRETER's currently executed program has
   been completed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-the-interpreter (interpreter)
      (program-is-complete-p $current-program))))

;;; -------------------------------------------------------

(defun main-program-is-complete-p (interpreter)
  "Determines whether the INTERPRETER's main, or top-level, program has
   been completed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (and
      (executes-the-main-program-p   interpreter)
      (current-program-is-complete-p interpreter))))

;;; -------------------------------------------------------

(defun terminate-the-current-program (interpreter)
  "Concludes the INTERPRETER's currently executed program, concomitantly
   restoring its state to the inchoate configuration, changes to the
   preveniently operative program, and returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    ;(reset-the-program $current-program)
    (pop               $programs))
  (values))

;;; -------------------------------------------------------

(defun start-a-new-program (interpreter new-program)
  "Adds the NEW-PROGRAM to the INTERPRETER's sub-program stack,
   designates the same as the new active instance, and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Program     new-program))
  (with-the-interpreter (interpreter)
    (push new-program $programs))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the stack attrectation operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push-onto-the-stack (interpreter new-element)
  "Pushes the NEW-ELEMENT onto the INTERPRETER's memory stack and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Sb-Object   new-element))
  (with-the-interpreter (interpreter)
    (push new-element $stack))
  (values))

;;; -------------------------------------------------------

(defun peek-into-the-stack (interpreter)
  "Returns without removing the element at the INTERPRETER's memory
   stack's top position.
   ---
   If the stack is empty at the instant of this operation's invocation,
   an abortive error of the type ``Empty-Stack-Error'' will be
   signaled."
  (declare (type Interpreter interpreter))
  (the Sb-Object
    (with-the-interpreter (interpreter)
      (or (first $stack)
          (error 'Empty-Stack-Error)))))

;;; -------------------------------------------------------

(defun pop-from-the-stack (interpreter)
  "Removes and returns the top element from the INTERPRETER's memory
   stack.
   ---
   If the stack is empty at the instant of this operation's invocation,
   an abortive error of the type ``Empty-Stack-Error'' will be
   signaled."
  (declare (type Interpreter interpreter))
  (the Sb-Object
    (with-the-interpreter (interpreter)
      (if $stack
        (pop $stack)
        (error "You cannot peek into or pop from an empty stack.")))))

;;; -------------------------------------------------------

(defun pop-two-elements-from-the-stack (interpreter)
  "Removes the twissel of top elements from the INTERPRETER's memory
   stack and returns these as two values:
     (1) The erstwhile top stack element.
     (2) The erstwhile element immediately alow the top position."
  (declare (type Interpreter interpreter))
  (the (values Sb-Object Sb-Object)
    (values
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter))))

;;; -------------------------------------------------------

(defun pop-three-elements-from-the-stack (interpreter)
  "Removes the trisulk of top elements from the INTERPRETER's memory
   stack and returns these as three values:
     (1) The erstwhile top stack element.
     (2) The element erstwhile immediately alow the top position.
     (3) The element erstwhile empight two positions alow the top."
  (declare (type Interpreter interpreter))
  (the (values Sb-Object Sb-Object Sb-Object)
    (values
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter))))

;;; -------------------------------------------------------

(defun print-the-stack (interpreter)
  "Prints the INTERPRETER memory stack's contents to the standard
   output conduit and returns no value."
  (declare (type Interpreter interpreter))
  (format T "~&Stack contents (from top to bottom):")
  (with-the-interpreter (interpreter)
    (dolist (current-element $stack)
      (declare (type Sb-Object current-element))
      (format T "~&~2t~a" current-element)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory attrectation operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tape-at (interpreter address)
  "Returns the object stored in the INTERPRETER's memory tape in the
   cell amenable to the ADDRESS."
  (declare (type Interpreter interpreter))
  (declare (type integer     address))
  (the Sb-Object
    (with-the-interpreter (interpreter)
      (gethash address $tape +SB-NULL+))))

;;; -------------------------------------------------------

(defun (setf tape-at) (new-value interpreter address)
  "Stores the NEW-VALUE in the INTERPRETER's memory tape cell at the
   specified ADDRESS and returns no value."
  (declare (type Sb-Object   new-value))
  (declare (type Interpreter interpreter))
  (declare (type integer     address))
  (with-the-interpreter (interpreter)
    (setf (gethash address $tape +SB-NULL+) new-value))
  (values))

;;; -------------------------------------------------------

(defun determine-the-tape-bounding-indices (interpreter)
  "Determines whether the INTERPRETER memory tape's bounding indices
   and returns two values:
     (1) The inclusive minimum index explicitly set in the tape.
     (2) The inclusive maximum index explicitly set in the tape."
  (declare (type Interpreter interpreter))
  (the (values integer integer)
    (with-the-interpreter (interpreter)
      (loop
        for current-index
          of-type integer
          being the hash-keys in $tape
        minimize current-index into minimum-index
        maximize current-index into maximum-index
        finally
          (return
            (values minimum-index maximum-index))))))

;;; -------------------------------------------------------

(defun print-the-tape (interpreter)
  "Prints the INTERPRETER memory tape's contents to the standard output
   conduit and returns no value."
  (declare (type Interpreter interpreter))
  (multiple-value-bind (minimum-index maximum-index)
      (determine-the-tape-bounding-indices interpreter)
    (declare (type integer minimum-index))
    (declare (type integer maximum-index))
    (loop
      initially
        (format T "~&Tape contents:")
      for current-index
        of-type integer
        from    minimum-index
        to      maximum-index
      do
        (format T "~&~2tTape[~d] = ~a"
          current-index
          (tape-at interpreter current-index))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command processing operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dispatch-on-the-command-kind (kind command interpreter)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER, dispatcing on the
     COMMAND's kind, and returns no value."))

;;; -------------------------------------------------------

(defmacro define-a-command-dispatch (command-kinds &body body)
  "Defines one or more implementations of the generic function
   ``dispatch-on-the-command-kind'', each one for any member of the
   COMMAND-KINDS, the method's first formal parameter's agnomination
   generated in an automatic fashion, dispatching on an
   ``eql''-conformity with the respective ``command-kind'', the second
   yclept in a fixed manner as ``$command'', specializing on the
   ``Command'' class, while the third and ultimate appropriates the
   name ``$interpreter'', concomitantly specializing on the
   ``Interpreter'' class; the implementation forms desumed, in an
   entreparted mode, from the BODY, with all methods fashioned according
   to this ilk returning no value.
   ---
   The COMMAND-KINDS must either be a single ``command-kind'' keyword
   symbol or a list comprehending zero or more such objects."
  (let ((command-kind-name (gensym)))
    (declare (type symbol command-kind-name))
    `(progn
       ,@(loop
           for command-kind
             of-type command-kind
             in      (if (listp command-kinds)
                       command-kinds
                       (list command-kinds))
           collect
             `(defmethod dispatch-on-the-command-kind
                  ((,command-kind-name (eql ,command-kind))
                   ($command           Command)
                   ($interpreter       Interpreter))
                (declare (type command-kind ,command-kind-name))
                (declare (ignore            ,command-kind-name))
                (declare (type Command      $command))
                (declare (ignorable         $command))
                (declare (type Interpreter  $interpreter))
                (declare (ignorable         $interpreter))
                (with-the-interpreter ($interpreter)
                  ,@body)
                (values))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the literal data operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch (:push-character
                            :push-integer
                            :push-string)
  (push-onto-the-stack $interpreter
    (command-operand $command))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the unary operators.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :negate
  (push-onto-the-stack $interpreter
    (make-an-sb-integer
      (- (sb-integer-value
           (pop-from-the-stack $interpreter)))))
  (advance-the-current-program $interpreter))

;;; -------------------------------------------------------

(define-a-command-dispatch :logical-not
  (push-onto-the-stack $interpreter
    (if (sb-object-is-true-p (pop-from-the-stack $interpreter))
      +SB-INTEGER-ZERO+
      +SB-INTEGER-ONE+))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary operators.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch (:add
                            :subtract
                            :multiply
                            :divide
                            :remainder
                            :equals
                            :less-than)
  (multiple-value-bind (right-operand left-operand)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type Sb-Object left-operand))
    (declare (type Sb-Object right-operand))
    (push-onto-the-stack $interpreter
      (apply-the-binary-operator
        (command-kind $command)
        left-operand
        right-operand)))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the relational operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :ternary-conditional
  (multiple-value-bind (alternative consequent antecedent)
      (pop-three-elements-from-the-stack $interpreter)
    (declare (type Sb-Object alternative))
    (declare (type Sb-Object consequent))
    (declare (type Sb-Object antecedent))
    (push-onto-the-stack $interpreter
      (if (sb-object-is-true-p antecedent)
        consequent
        alternative)))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the stack attrectation operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :pop
  (pop-from-the-stack          $interpreter)
  (advance-the-current-program $interpreter))

;;; -------------------------------------------------------

(define-a-command-dispatch :duplicate
  (let ((top-element (pop-from-the-stack $interpreter)))
    (declare (type Sb-Object top-element))
    (push-onto-the-stack $interpreter top-element)
    (push-onto-the-stack $interpreter top-element))
  (advance-the-current-program $interpreter))

;;; -------------------------------------------------------

(define-a-command-dispatch :swap
  (multiple-value-bind (top-element next-lower-element)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type Sb-Object top-element))
    (declare (type Sb-Object next-lower-element))
    (push-onto-the-stack $interpreter top-element)
    (push-onto-the-stack $interpreter next-lower-element))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the tape attrectation operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :write-to-tape
  (multiple-value-bind (new-value address)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type Sb-Object new-value))
    (declare (type Sb-Object address))
    (setf (tape-at $interpreter (sb-integer-value address))
          new-value))
  (advance-the-current-program $interpreter))

;;; -------------------------------------------------------

(define-a-command-dispatch :load-from-tape
  (push-onto-the-stack $interpreter
    (tape-at $interpreter
      (sb-integer-value
        (pop-from-the-stack $interpreter))))
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input communication operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :input-line
  (format T "~&Please input a line: ")
  (force-output)
  (push-onto-the-stack $interpreter
    (make-an-sb-string
      (read-line NIL NIL "")))
  (clear-input)
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the dynamic execution operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :evaluate
  (advance-the-current-program $interpreter)
  (start-a-new-program $interpreter
    (parse-the-sb-program
      (sb-string-content
        (pop-from-the-stack $interpreter)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the control flow duction operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :start-while-loop
  (let ((guard (peek-into-the-stack $interpreter)))
    (declare (type Sb-Object guard))
    (unless (sb-object-is-true-p guard)
      (jump-to-the-opposite-program-loop-point $current-program)))
  (advance-the-current-program $interpreter))

;;; -------------------------------------------------------

(define-a-command-dispatch :end-while-loop
  (jump-to-the-opposite-program-loop-point $current-program))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of non-operative commands.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-command-dispatch :nop
  (advance-the-current-program $interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter execution operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-the-command (interpreter command)
  "Evaluates the COMMAND in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (dispatch-on-the-command-kind
    (command-kind command)
    command
    interpreter)
  (values))

;;; -------------------------------------------------------

(defun process-the-current-command (interpreter)
  "Evaluates the currently selected command in the INTERPRETER's
   contemporaneously executed program and returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (process-the-command interpreter $current-command))
  (values))

;;; -------------------------------------------------------

(defun launch-the-interpreter (interpreter)
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (loop do
      (cond
        ;; Main program has been completed?
        ((main-program-is-complete-p interpreter)
          (loop-finish))
        
        ;; Sub-program has been completed?
        ;; => Change to next higher program.
        ((current-program-is-complete-p interpreter)
          (terminate-the-current-program interpreter))
        
        ;; Current program still possedes commands?
        (T
          (process-the-current-command interpreter)))))
  (print-the-stack interpreter)
  (print-the-tape  interpreter)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the entry operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-sb-code (code)
  "Interprets the piece of sb source CODE and returns no value."
  (declare (type string code))
  (launch-the-interpreter
    (make-an-interpreter-for
      (parse-the-sb-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the message "Hello, world!" on the memory stack.
(interpret-the-sb-code "\"Hello, world!\"")

;;; -------------------------------------------------------

;; Factorial: Query the standard input conduit for an integer number
;; and replicate on the stack top its factorial.
(interpret-the-sb-code "1$;{:0~(*0)1-}\\")

;;; -------------------------------------------------------

;; Echo: Query the standard input conduit for a line and push thilk as
;; a string onto the stack.
(interpret-the-sb-code "$")

;;; -------------------------------------------------------

;; Read a line from the standard input and execute thilk as a
;; sub-program.
(interpret-the-sb-code "$;")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Please heed that, as a consectary of the sb language's carency in an
;; output operation, the memory stack will be surfeited with an infinite
;; accompt of ones (1), while no printing transpires; hence, mimicking
;; an infinite loop.
(interpret-the-sb-code "1$;={1}\\0")
