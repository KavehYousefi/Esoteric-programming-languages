;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Phoneboard", presented by the Esolang user "Jan Gamecuber"
;; in the year 2022, and based upon two integer stacks whose indagation
;; and manipulation apply by adminiculum of single-character
;; instructions borrowed from the restricted keyboards built into common
;; telephones.
;; 
;; 
;; Concepts
;; ========
;; Phoneboard describes a stack-based programming language with a syntax
;; derived from the symbols on a telephone keyboard. Two stacks offer
;; their availability to the programmer, with exactly one at any time
;; representing the active party.
;; 
;; == PROGRAMS THAT MIMIC TELEPHONE SYMBOLS ==
;; Manifesting the kenspeckle property of Phoneboard, the syntactical
;; elements are constrained to the acquainted symbols of telephone key
;; pads, that is, the twelve-members set
;; 
;;   --------------------------------------------
;;   Category        | Tally of members | Members
;;   ----------------+------------------+--------
;;   Decimal digits  | 10               | 0
;;                   |                  | 1
;;                   |                  | 2
;;                   |                  | 3
;;                   |                  | 4
;;                   |                  | 5
;;                   |                  | 6
;;                   |                  | 7
;;                   |                  | 8
;;                   |                  | 9
;;   ............................................
;;   Special symbols | 2                | #
;;                   |                  | *
;;   --------------------------------------------
;; 
;; == TWO STACKS THAT CONJOIN INTO CHAMPARTY ==
;; A second dioristic element of Phoneboard --- maugre its subordinate
;; significance to the overall operations --- occupies its woning in the
;; deployment of two stacks.
;; 
;; Both moieties may store an arbitrary tally of unbounded integers,
;; with one member of the twain at any instant constituting the active
;; unit, amenable to indagations and modifications. The responsible
;; participant may be switched by the programmer in concord with his own
;; deliberations.
;; 
;; == INSTRUCTIONS THAT CALCULATE AND LOOP ==
;; Phoneboard's instruction set attends to a rather complete ilk of
;; service, offering operations to insert into the stack and modify its
;; order, perform addition and subtraction, receive Unicode character
;; input, commit output across the repertoire, and helm the control flow
;; via a single iteration construct based upon the equality of the two
;; top stack items.
;; 
;; 
;; Architecture
;; ============
;; Phoneboard programs operate on two unbounded stacks of arbitrary
;; integer numbers, one of each at any instant is designated the active,
;; and thus amenable, instance.
;; 
;; 
;; Data Types
;; ==========
;; Phoneboard's type system relates to a bivial subject, with the
;; integers' appropriation of a paravaunt echolon being a concomitant to
;; the puisne Unicode characters along the communication channels.
;; 
;; == INTEGERS: THE FOUNDATIONAL INGREDIENTS ==
;; Their commorancy in the program's exclusive storage units, the stack
;; twain, serves to invest the integer type with a superior role. These
;; numeric objects may assume any sign and magnitude, thus covering the
;; range [-infinity, +infinity].
;; 
;; == CHARACTERS: THE CURRENCY OF INTERACTION ==
;; The character type participates solely on the language's interaction
;; boundary, subscribing to the Unicode standard, and representing user
;; inputs as well as system outputs.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical vista, a Phoneboard program consists of zero or
;; more one-character instruction identifiers, optionally embraced by
;; zero or more whitespaces.
;; 
;; == INSTRUCTIONS ==
;; All twelve instruction are represented by single characters inspired
;; by telephone key pads, and thus spanning the decimal digits "0" to
;; inclusive "9", as well the hash sign "#" and the asterisk "*".
;; 
;; Instructions may be optionally surrounded by zero or more
;; whitespaces.
;; 
;; == WHITESPACES ==
;; Whitespaces, a term which comprehends the space, horizontal tab, and
;; the newline character, may be liberally inserted betwixt and around
;; instructions.
;; 
;; == COMMENTS ==
;; The language in its current iteration lacks any facilities for
;; commentary purposes.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation describes
;; the Phoneboard syntaxis:
;; 
;;   program     := { whitespaces , command } ;
;;   command     := "0" | "1" | "2" | "3" | "4" | "5"
;;               |  "6" | "7" | "8" | "9" | "#" | "*"
;;               ;
;;   whitespaces := { whitespace } ;
;;   whitespace  := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Phoneboard's instruction set tallies twelve members, the entirety of
;; which depend on the stack for indagation and/or manipulation.
;; 
;; == INSTRUCTION CATEGORIES ==
;; The operations' competences embrace a rather magnanimous field:
;; 
;;   (a) Stack logistics
;;       A preponderance of the circumference relates to the explicit
;;       and direct manipulation of the stack content and layout,
;;       including the contingency of pushing elements, swapping the
;;       top items, and rotating the structure. A kenspeckle attribute
;;       of the language, two stacks participate in parallel, with any
;;       of the twain being at a particular time selectable as the
;;       amenable instance.
;;   (b) Arithmetics
;;       Instructions exist for the basic arithmetic operations of
;;       addition and subtraction.
;;   (c) Input and output
;;       Communication channels for the receipt of character input and
;;       the display of the same class of output are provided.
;;   (d) Conditionals
;;       A single iteration instruction, based upon an equality
;;       predicate with regard to the current stack's top elements,
;;       permits conditional and repeated execution.
;; 
;; == OVERVIEW ==
;; The apercu alow shall endow the reader with a cursory nortelry anenst
;; the language's faculties:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Pops the top element "a" from the stack, then pops the
;;           | new top element "b", and pushes the difference
;;           |   a - b
;;           | unto the stack.
;;   ..................................................................
;;   1       | Pushes the value 1 unto the current stack.
;;   ..................................................................
;;   2       | Pushes the value 2 unto the current stack.
;;   ..................................................................
;;   3       | Pops the top element "a" from the stack, then pops the
;;           | new top element "b", and pushes the sum
;;           |   a + b
;;           | unto the stack.
;;   ..................................................................
;;   4       | Switches the current stack: If first stack is currently
;;           | active, the second one is selected; otherwise the first
;;           | stack is activated.
;;   ..................................................................
;;   5       | Queries the user for a Unicode character input and
;;           | pushes its code point unto the current stack.
;;   ..................................................................
;;   6       | Performs the "ROTCCW" operation on the current stack,
;;           | that is, applies a counterclockwise rotation unto the
;;           | three topmost elements following this mode:
;;           |   (a) The original top element is shifted to the third
;;           |       location from the top.
;;           |   (b) The original second element is relocated to the
;;           |       stack top.
;;           |   (c) The original third element moves to the second
;;           |       position from the top.
;;           | An illustrative diagram shall educate in a more lucid
;;           | fashion, juxtaposing the original stack state on the
;;           | sinistral side to the resulting state on the right:
;;           | 
;;           |   [ top    ]    [ second ]
;;           |   [ second ] => [ third  ]
;;           |   [ third  ]    [ top    ]
;;   ..................................................................
;;   7       | Swaps the current stack's two top elements.
;;   ..................................................................
;;   8       | Duplicates the current stack's top element.
;;   ..................................................................
;;   9       | Pops the current stack's top element and prints to the
;;           | standard output the Unicode character corresponding to
;;           | the numeric code point.
;;   ..................................................................
;;   #       | Designates the start of a "while" loop which repeats
;;           | as long as the current stack's top two elements are
;;           | equal. To do so, pops the top element "a" and the new
;;           | top element "b". While it holds
;;           |   a = b
;;           | the code section betwixt this "#" instance and the
;;           | matching "*" instruction is repeated.
;;           | Upon the predicate's failure, the instruction pointer
;;           | moves to the position immediately following the closing
;;           | "*" loop terminator.
;;   ..................................................................
;;   *       | Demarcates the end of the current "while" loop,
;;           | instigated via the "#" command, which please see above.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Phoneboard's protologue, albeit being rather explicit in most
;; regards, yet is inflicted with a few ambivalencies, a subset of the
;; most conspicuous shall be the following treatise's subject.
;; 
;; == WHAT EFFECT DOES THE COMMAND "6" EXERCISE? ==
;; The command associated with the token "6" is elucidated with the
;; single-verb description "ROTCCW" only. Given the stack context, this
;; might be a hint to a ROTation in CounterClockWise orientation.
;; 
;; It is, however, not clear in which aspect the rotation may traverse
;; widdershins, nor which portion of the stack ought to respond to this
;; request.
;; 
;; Founded upon the negated rotation of the siclike stack-based Forth
;; programming language, realized in the instruction "-rot", it has been
;; reckoned meet to expect the three topmost items
;; 
;;   [top, second, third]
;; 
;; to restructure into
;; 
;;   [second, third, top]
;; 
;; == DOES THE ITERATION CONSTRUCT CONSUME STACK ELEMENTS? ==
;; The single iteration construct, demarcated by the instruction twain
;; "#" and "*", decides upon each execution by mediation of the two top
;; stack items' equality. While several operations are explicitly stated
;; by the original specification to consume, that is, pop the required
;; stack elements, it is not known whether the principle applies to
;; the conditional facility.
;; 
;; The dubiosity is enhanced by the gnarity about some commands being
;; incompatible with such deductions from the storage: The top element
;; swapping, via "7", for instance, does with certitude abstain from
;; deletions; the same rationale applies to the duplication using the
;; "8" action.
;; 
;; It has been adjudged as tenable to concede into the assumption that
;; each checking of the loop predicate by the "#" command concomitantly
;; removes the tested top stack elements pair.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-02-01
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Phoneboard"
;;   -> "https://wiki.laptop.org/go/Forth_stack_operators"
;;       o Explanation of the "-rot" operation, which might translate to
;;         Phoneboard's "ROTCCW" command.
;;   -> "https://docs.oracle.com/cd/E19253-01/816-1177-10/fthtools.html"
;;       o Explanation of the "-rot" operation, which might translate to
;;         Phoneboard's "ROTCCW" command.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized instruction variants."
  '(member
    :subtract
    :push-1
    :push-2
    :add
    :swap-stacks
    :input-character
    :rotccw
    :swap-top-elements
    :duplicate-top-elements
    :output-character
    :start-loop
    :end-loop))

;;; -------------------------------------------------------

(deftype phoneboard-program ()
  "The ``phonenboard-program'' type defines an executable Phoneboard
   program as either a vector of zero or more commands, or, as a
   specialization, a one-dimensional simple array of such."
  '(or (vector       command  *)
       (simple-array command (*))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a jump table which maps the positions
   of loop start instructions in a Phoneboard instruction sequence to
   the corresponding iteration end instruction locations, and vice
   versa, manifesting in the form of a hash table that associates fixnum
   keys to values of the same set."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based stack of zero or more integer
   numbers."
  '(list-of integer))

;;; -------------------------------------------------------

(deftype stack-array ()
  "The ``stack-array'' type defines a one-dimensional simple array of
   zero or more ``stack'' objects."
  '(simple-array stack (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Phoneboard CODE the incorporated
   instructions and returns these as a one-dimensional simple array of
   ``command'' objects."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (flet ((collect-instruction (instruction)
            (declare (type command instruction))
            (push instruction instructions)
            (values)))
      (loop
        for token    of-type character across code
        and position of-type fixnum    from   0
        do
          (case token
            (#\0 (collect-instruction :subtract))
            (#\1 (collect-instruction :push-1))
            (#\2 (collect-instruction :push-2))
            (#\3 (collect-instruction :add))
            (#\4 (collect-instruction :swap-stacks))
            (#\5 (collect-instruction :input-character))
            (#\6 (collect-instruction :rotccw))
            (#\7 (collect-instruction :swap-top-elements))
            (#\8 (collect-instruction :duplicate-top-elements))
            (#\9 (collect-instruction :output-character))
            (#\# (collect-instruction :start-loop))
            (#\* (collect-instruction :end-loop))
            ((#\Space #\Tab #\Newline) NIL)
            (otherwise
              (error "Invalid character \"~c\" at position ~d."
                token position)))))
    (the phoneboard-program
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Stack-Set".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Stack-Set
  (:constructor make-stack-set
    (&optional (number-of-stacks 2)
     &aux      (stacks
                 (make-array number-of-stacks
                   :element-type    'stack
                   :initial-element NIL)))))
  "The ``Stack-Set'' maintains a sequence of one or more stacks,
   amenable to their manipulation as well as ordered exchange.
   ---
   The stack set maintains a fixed-length tally of theoretical
   infinite-capacity stacks in a ring-like structure."
  (number-of-stacks 2   :type (integer 1 *))
  (cursor           0   :type fixnum)
  (stacks           NIL :type stack-array))

;;; -------------------------------------------------------

(defun stack-set-current-stack (stack-set)
  "Returns the currently active stack from the STACK-SET."
  (declare (type Stack-Set stack-set))
  (the stack
    (aref (stack-set-stacks stack-set)
          (stack-set-cursor stack-set))))

;;; -------------------------------------------------------

(defun (setf stack-set-current-stack) (new-stack stack-set)
  "Replaces the currently active STACK-SET stack's content with the
   NEW-STACK and returns the NEW-STACK."
  (declare (type stack     new-stack))
  (declare (type Stack-Set stack-set))
  (setf (aref (stack-set-stacks stack-set)
              (stack-set-cursor stack-set))
        new-stack)
  (the stack
    (stack-set-current-stack stack-set)))

;;; -------------------------------------------------------

(defun stack-set-push (stack-set new-element)
  "Pushes the NEW-ELEMENT unto the STACK-SET's current stack's top and
   returns the modified STACK-SET."
  (declare (type Stack-Set stack-set))
  (declare (type integer   new-element))
  (push new-element (stack-set-current-stack stack-set))
  (the Stack-Set stack-set))

;;; -------------------------------------------------------

(defun stack-set-pop (stack-set)
  "Pops and returns the top element from the STACK-SET's current stack."
  (declare (type Stack-Set stack-set))
  (the integer
    (pop (stack-set-current-stack stack-set))))

;;; -------------------------------------------------------

(defun stack-set-rotccw (stack-set)
  "Rotates the three top elements in STACK-SET's current stack in a
   counterclockwise rotation, that is, shifts the original top element
   into the third place, the second element into the top, and the
   erstwhile third element into the second-highest location, and returns
   the modified STACK-SET.
   ---
   With respect to a visual illustration, the following transpires:
   Given an original current stack state
   
      ________
     | top    |
     | second |
     | third  |
     | ...... |
      --------
   
   a new state of
   
      ________
     | second |
     | third  |
     | top    |
     | ...... |
      --------
   
   is assumed.
   ---
   This function mimics the Forth programming language operation
   \"-rot\"."
  (declare (type Stack-Set stack-set))
  (rotatef
    (first  (stack-set-current-stack stack-set))
    (second (stack-set-current-stack stack-set)))
  (rotatef
    (first  (stack-set-current-stack stack-set))
    (second (stack-set-current-stack stack-set)))
  (the Stack-set stack-set))

;;; -------------------------------------------------------

(defun stack-set-swap-top-elements (stack-set)
  "Exchanges the positions of the two topmost elements on the
   STACK-SET's current set and returns no value."
  (declare (type Stack-Set stack-set))
  (rotatef (first  (stack-set-current-stack stack-set))
           (second (stack-set-current-stack stack-set)))
  (the Stack-Set stack-set))

;;; -------------------------------------------------------

(defun stack-set-duplicate-top-elements (stack-set)
  "Duplicates the top element of the STACK-SET's current stack and
   returns the modified STACK-SET."
  (declare (type Stack-Set stack-set))
  (push (first  (stack-set-current-stack stack-set))
        (stack-set-current-stack stack-set))
  (the Stack-Set stack-set))

;;; -------------------------------------------------------

(defun stack-set-top-elements-equal-p (stack-set)
  "Pops the two top elements from the STACK-SET's current stack and
   checks their equality, returning a ``boolean'' value of ``T'' if the
   twain matches, otherwise ``NIL''."
  (declare (type Stack-Set stack-set))
  (the boolean
    (not (null
      (= (pop (stack-set-current-stack stack-set))
         (pop (stack-set-current-stack stack-set)))))))

;;; -------------------------------------------------------

(defun stack-set-swap-stacks (stack-set)
  "Selects the next stack from the STACK-SET as the currently active one
   and returns the modified STACK-SET.
   ---
   If ordered to transition from the desinent stack, the cursor returns
   to the first one."
  (declare (type Stack-Set stack-set))
  (setf (stack-set-cursor stack-set)
    (mod (1+ (stack-set-cursor stack-set))
         (stack-set-number-of-stacks stack-set)))
  (setf (stack-set-current-stack stack-set)
    (aref (stack-set-stacks stack-set)
          (stack-set-cursor stack-set)))
  (the Stack-Set stack-set))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Creates and returns for the INSTRUCTIONS a jump table which
   associates each loop start position with the affiliated end location,
   and vice versa."
  (declare (type phoneboard-program instructions))
  (let ((jump-table  (make-hash-table :test #'eql))
        (loop-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) loop-starts))
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0 by 1
      do
        (case instruction
          (:start-loop
            (push position loop-starts))
          (:end-loop
            (if loop-starts
              (let ((start-position (pop loop-starts))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (setf (gethash start-position jump-table) end-position)
                (setf (gethash end-position jump-table) start-position))
              (error "Missing loop start (\"#\") at position ~d."
                position)))
          (otherwise
            NIL)))
    (when loop-starts
      (error "Missing loop end (\"*\") at positions ~{~d~^, ~}."
        loop-starts))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Executes the Phoneboard INSTRUCTIONS and returns no value."
  (declare (type phoneboard-program instructions))
  (when (plusp (length instructions))
    (let ((ip         0)
          (jump-table (build-jump-table instructions))
          (stacks     (make-stack-set 2)))
      (declare (type fixnum     ip))
      (declare (type jump-table jump-table))
      (declare (type Stack-Set  stacks))
      
      (flet
          ((has-more-instructions-p ()
            "Checks whether one or more instructions in the INSRUCTIONS
             sequence remain to be processed, returning on confirmation
             a ``boolean'' value of ``T'', otherwise ``NIL''."
            (the boolean
              (not (null
                (array-in-bounds-p instructions ip)))))
           
           (current-instruction ()
            "Returns the currently selected instruction, located at the
             instruction pointer IP."
            (the command
              (aref instructions ip)))
           
           (jump-out-of-loop ()
            "Expecting the instruction pointer IP to be located at the
             start of a loop, moves the same past the corresponding end
             position and returns no value."
            (setf ip (1+ (gethash ip jump-table)))
            (values))
           
           (jump-to-start-of-loop ()
            "Expecting the instruction pointer IP to be located at the
             end of a loop, moves the same to the corresponding start
             position and returns no value."
            (setf ip (gethash ip jump-table))
            (values)))
        
        (loop while (has-more-instructions-p) do
          (case (current-instruction)
            ((NIL)
              (loop-finish))
            
            (:subtract
              (let ((minuend    (stack-set-pop stacks))
                    (subtrahend (stack-set-pop stacks)))
                (declare (type integer minuend))
                (declare (type integer subtrahend))
                (stack-set-push stacks (- minuend subtrahend))))
            
            (:push-1
              (stack-set-push stacks 1))
            
            (:push-2
              (stack-set-push stacks 2))
            
            (:add
              (let ((augend (stack-set-pop stacks))
                    (addend (stack-set-pop stacks)))
                (declare (type integer augend))
                (declare (type integer addend))
                (stack-set-push stacks (+ augend addend))))
            
            (:swap-stacks
              (stack-set-swap-stacks stacks))
            
            (:input-character
              (format T "~&Please input a Unicode character: ")
              (stack-set-push stacks (char-code (read-char)))
              (clear-input))
            
            (:rotccw
              (stack-set-rotccw stacks))
            
            (:swap-top-elements
              (stack-set-swap-top-elements stacks))
            
            (:duplicate-top-elements
              (stack-set-duplicate-top-elements stacks))
            
            (:output-character
              (write-char
                (code-char
                  (stack-set-pop stacks))))
            
            (:start-loop
              (unless (stack-set-top-elements-equal-p stacks)
                (jump-out-of-loop)))
            
            (:end-loop
              (jump-to-start-of-loop))
            
            (otherwise
              (error "Unrecognized instruction ~s at position ~d."
                (current-instruction) ip)))
          
          (incf ip)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Phoneboard (code)
  "Interprets the piece of Phoneboard CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Phoneboard "59")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Phoneboard "11#5911*")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; A pseudocode description, accompanied by the active stack's state
;; succeeding the respective action, is adduced. Please note that the
;; stack representation limns the data structure in sinistrodextral
;; airt, that is, the leftmost entry relates to the top stack item,
;; proceeding towards the rightmost entry that is tantamount to the
;; stack bottom. The design thus resolves to
;; 
;;   [stackTop, stackTop-1, ..., stackBottom]
;; 
;; The tabular exposition can be extracted in the following:
;; 
;;   ------------------------------------------------------------------
;;   Pseudocode action               | Current stack state after action
;;   --------------------------------+---------------------------------
;;   generate number 49              | [49]
;;   duplicate 49                    | [49, 49]
;;   duplicate 49                    | [49, 49, 49]
;;   input                           | [input, 49, 49, 49]
;;   duplicate input                 | [input, input, 49, 49, 49]
;;   output    input                 | [input, 49, 49, 49]
;;   start loop                      | [49, 49]
;;     output    "1" (code point 49) | [49]
;;     duplicate 49                  | [49, 49]
;;     duplicate 49                  | [49, 49, 49]
;;     duplicate 49                  | [49, 49, 49, 49]
;;   end loop                        | [49, 49, 49, 49]
;;   ------------------------------------------------------------------
(interpret-Phoneboard
  "
  223 223 3 223 3 223 3 223 3 223 3 223 3 223 3 223 3 223 3 223 3 223 3 120 3
  8
  8
  5
  8
  9
  #
  9
  8
  8
  8
  *
  ")
