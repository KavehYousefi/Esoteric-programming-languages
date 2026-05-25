;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "1><>", also nevened "1Fish", invented by the Esolang user
;; "EvyLah" and presented on December 23rd, 2023, itself a derivation of
;; the user "Harpyon"'s "><>" ("Fish"), the two-dimensional
;; conformation's designment experiences a curtailment to an aefauld
;; dimense, operating like its entheus on a stack of real-valued number.
;; 
;; 
;; Concept
;; =======
;; The 1><> programming language's firmament entertains its edification
;; upon the two-dimensional specimen ><>, whose haecceities' retention
;; appertains, in a paravaunt but not exclusive application of one's
;; conspectuity, to the single-symbol operation identifiers and the
;; stack-based memory model.
;; 
;; == 1><>: ONE-DIMENSIONAL ><> ==
;; The 1><> programming language's fons et origo is begotten from a
;; derivation of the specimen ><>, having adhibited a curtailment upon
;; its axes to a one-dimensional creation, whence several diorisms'
;; adjustments entertain their gendrure, limning the lacrymatory
;; semblant of a piscine perambulator whose motations' compass does not
;; enjoy the enatation from a directrix' stricture.
;; 
;; == THE MEMORY: A STACK OF INTEGERS AND FLOATING-POINT NUMBERS ==
;; The data castaldy's exclusive conduction constitutes the parcery of
;; a stack, the currencies of its deliberation integral and
;; floating-point numbers upon whom no impounding nomothesy tholes a
;; cumbrance anent either the sign or the mickleness.
;; 
;; == INPUT IS PROVIDED THROUGH THE COMMAND LINE ==
;; 1><>'s mode of input receptions, the tokens of this intercourse
;; being enstated their manifestation in characters, proceeds by
;; adminiculum of the command line, its signification's parasceve the
;; "-i" option identifier, whence, succeeded by an aefauld space, a
;; catena of zero or more characters ought to be ostended.
;; 
;; The input tmema, constitutes an optional affair, whose absence does
;; not instigate an erroneous circumstance.
;; 
;; The command line's conformation's forbisen, when limned in its
;; enker fashion, resolves to the following:
;; 
;;   <interpreterPath> <sourceFile> [-i <inputs>]
;; 
;; where:
;;   <interpreterPath> signifies the interpreter's executable program
;;                     path on the operating system.
;;   
;;   <sourceFile>      designates the 1Fish program file to load and
;;                     execute in the interpreter's context.
;;   
;;   <inputs>          represents a sequence of zero or more characters
;;                     which, requested seriatim, produce the character
;;                     inputs for the 1><> "i" operation.
;; 
;; == 1><> SOURCE FILES ARE RECOGNIZED BY THE EXTENSION "1f" ==
;; A 1><> fish source file's indicium is patefied in the extension
;; "1f".
;; 
;; 
;; Instructions
;; ============
;; A cardinality of 34 members serves in the mickleness' exhaustion
;; thilk limns the 1><> instruction set's mears, its bailiwicks siccan
;; magnanimity as to enhalse numerical and arithmetic operations,
;; equiparation operators, stack attrectation, and several control flow
;; duction mechanisms.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be realized in a requisite mete
;; of gnarity's adhibition concerning the language's operative avails.
;; 
;; Please heed the demarcation of succedaneous tmemata by catenae of
;; tilde symbols ("~"), their occurrencies intended for their
;; substitution by actual 1><> code in the ultimate program.
;; 
;;   ==================================================================
;;   NUMERICAL OPERATIONS
;;   ------------------------------------------------------------------
;;   0            | Pushes the number zero (0) onto the stack.
;;   ..................................................................
;;   1            | Pushes the number one (1) onto the stack.
;;   ..................................................................
;;   2            | Pushes the number two (2) onto the stack.
;;   ..................................................................
;;   3            | Pushes the number three (3) onto the stack.
;;   ..................................................................
;;   4            | Pushes the number four (4) onto the stack.
;;   ..................................................................
;;   5            | Pushes the number five (5) onto the stack.
;;   ..................................................................
;;   6            | Pushes the number six (6) onto the stack.
;;   ..................................................................
;;   7            | Pushes the number seven (7) onto the stack.
;;   ..................................................................
;;   8            | Pushes the number eight (8) onto the stack.
;;   ..................................................................
;;   9            | Pushes the number nine (9) onto the stack.
;;   ==================================================================
;;   ARITHMETIC OPERATIONS
;;   ------------------------------------------------------------------
;;   +            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the sum of
;;                | y + x, and pushes the result onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y + x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   -            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the difference
;;                | of y - x, and pushes the result onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y - x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   *            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the product of
;;                | y * x, and pushes the result onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y * x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   /            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the quotient
;;                | of y / x, and pushes the result onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y / x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   |            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the integral
;;                | quotient of floor(y / x), and pushes the result
;;                | onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- floor(y / x)
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   ^            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the power of
;;                | the base y raised to the exponent x, and pushes the
;;                | result onto the stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y^x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   %            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", supputates the remainder
;;                | of y modulo x, and pushes the result onto the
;;                | stack.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   let result <- y modulo x
;;                |   stack.push(result)
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ==================================================================
;;   RELATIONAL OPERATIONS
;;   ------------------------------------------------------------------
;;   <            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", and juxtaposes x and y:
;;                | If it holds, y < x, pushes the number one (1) onto
;;                | the stack; otherwise pushes zero (0).
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   if y < x then
;;                |     stack.push(1)
;;                |   else
;;                |     stack.push(0)
;;                |   end if
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   =            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", and juxtaposes x and y:
;;                | If it holds, y = x, pushes the number one (1) onto
;;                | the stack; otherwise pushes zero (0).
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   if y = x then
;;                |     stack.push(1)
;;                |   else
;;                |     stack.push(0)
;;                |   end if
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   >            | Pops the top stack element, here yclept "x", and
;;                | the new top element, "y", and juxtaposes x and y:
;;                | If it holds, y > x, pushes the number one (1) onto
;;                | the stack; otherwise pushes zero (0).
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   let y <- stack.pop()
;;                |   if y > x then
;;                |     stack.push(1)
;;                |   else
;;                |     stack.push(0)
;;                |   end if
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ==================================================================
;;   STACK HANDLING OPERATIONS
;;   ------------------------------------------------------------------
;;   l            | Pushes the tally of elements comprising the stack
;;                | as a number onto the stack itself.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   stack.push(stack.getSize())
;;   ..................................................................
;;   d            | Duplicates the top stack element.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operation's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   D            | Duplicates the entire stack's content by inserting
;;                | a copy of the contemporaneously extant elements
;;                | aboon themselves.
;;                |----------------------------------------------------
;;                | For an empty stack this operation does not
;;                | accompass any causatum.
;;   ..................................................................
;;   s            | Swaps the two top stack elements' positions.
;;                |----------------------------------------------------
;;                | If the stack cannot accommodate at least two
;;                | elements at the instant of this operation's
;;                | invocation, an error of the type "EmptyStackError"
;;                | is signaled during the illicit access attempt.
;;   ..................................................................
;;   r            | Reverses the order of the stack elements.
;;   ..................................................................
;;   q            | Pops and discards the top stack element.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operation's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   c            | Clears the entire stack.
;;   ==================================================================
;;   INPUT AND OUTPUT OPERATIONS
;;   ------------------------------------------------------------------
;;   o            | Pops the top stack element, rounds it to the
;;                | nearest integer number, and prints the character
;;                | whose Unicode code point conflates with this
;;                | integral datum to the standard output conduit.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operation's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   n            | Pops the top stack element and prints thilk in its
;;                | verbatim numeric form to the standard output
;;                | conduit.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operation's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   i            | Queries the standard input conduit for a character
;;                | and pushes its Unicode code point onto the stack.
;;                | If no input is supplied, the value zero (0) is
;;                | instead inserted in the stack.
;;   ..................................................................
;;   "            | Commences the "string mode": Succeeding this
;;                | token, and perpetuating until the next '"' symbol
;;                | has been encountered, pushes the Unicode code point
;;                | of any amplected character onto the stack, except
;;                | for the bracing '"' entities.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   { Skip the instigating '"' symbol. }
;;                |   ip <- ip + 1
;;                |   repeat until code[ip] = '"" do
;;                |     stack.push(codePointOf(code[ip]))
;;                |     ip <- ip + 1
;;                |   end repeat
;;                |   { Skip the terminal '"' symbol. }
;;                |   ip <- ip + 1
;;                |----------------------------------------------------
;;                | If the program is exhausted without a terminating
;;                | '"' character's reception, an error of the type
;;                | "UnterminatedStringError" is signaled.
;;   ==================================================================
;;   CONTROL FLOW OPERATIONS
;;   ------------------------------------------------------------------
;;   {statements} | If the stack is empty or its top element equals
;;    ~~~~~~~~~~  | zero (0), the same being removed during this
;;                | indagation, moves the instruction pointer (IP) to
;;                | the terminal "}" token, thus skipping the body of
;;                | {statements}; otherwise evaluates these
;;                | {statements} once and advances as usual.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   if stack.isEmpty() or (stack.pop() = 0) then
;;                |     ip <- position of desitive "}" token
;;                |   else
;;                |     ip <- ip + 1
;;                |   end if
;;                |----------------------------------------------------
;;                | {statements} must be a sequence of zero or more
;;                | characters.
;;   ..................................................................
;;   (statements) | Repeatedly executes the body of {statements} until
;;    ~~~~~~~~~~  | the stack is empty, thus forming an iterance.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   repeat until stack.isEmpty() do
;;                |     execute the statements
;;                |   end repeat
;;                |----------------------------------------------------
;;                | {statements} must be a sequence of zero or more
;;                | characters.
;;   ..................................................................
;;   j            | Pops the top stack element, here norned "x", and
;;                | relocates the instruction pointer (IP) to the
;;                | position (x - 1) in the 1><> program, effectively
;;                | preparing a processing commencing with inclusive
;;                | the symbol at the position x.
;;                |----------------------------------------------------
;;                | In a pseudocode diction, it holds:
;;                |   let x <- stack.pop()
;;                |   ip    <- x - 1
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operation's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's patefaction constitutes a gendrure from the
;; Common Lisp programming language's involvement, its operations' mode
;; a per saltum evaluation of the 1><> source code's symbols.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-05-08
;; 
;; Sources:
;;   [esolang:2023:1Fish]
;;   The Esolang contributors, "1Fish", December 27th, 2023
;;   URL: "https://esolangs.org/wiki/1Fish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type founded upon a predicate's satisfaction, its
   agnominations constituting the TYPE-NAME's dation, its formal
   parameters the LAMBDA-LIST's ipsissima verba appropriation, and whose
   docimasy's subject is nevened by the CANDIDATE-NAME, the BODY forms
   enjoying an adit to the same during their evaluation, with the
   desinent form's primary return value being administered the construe
   as the assessment's perclose, a \"generalized boolean\" value of
   \"true\" signifying the probed object's compatibility with the
   BODY form's established antecedent, whereas a \"false\" response
   serves to reject the candidate.
   ---
   The first BODY form, upon its resolution to a string, is construed
   as the type definition's documentation, being reapproriated for this
   purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-bespoke-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   admits zero or more members, each such specimen a twissel compact of
   a key compliant with the KEY-TYPE and a value establishing a
   compatibility with the VALUE-TYPE, both are being governed by the
   generic sentinel ``*'' as a default configuration."
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
          (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype iterance-table ()
  "The ``iterance-table'' type defines abidirectional affiliations
   atwixen jump points in a 1><> program, the cirmcumference of which
   enhalses both conditional execution and iterance constructs, the
   mapping's entelechy an obtainal from a hash table's services, thilk
   associates with the zero-based ``fixnum'' indices of one end point
   its opposite moeity, cognate anent the type's diorism, in the source
   code."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(define-a-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a linked list compact of zero or more
   elements, each member inwith the same an adherent of the
   ELEMENT-TYPE, for which governs the generic sentinel ``*'' the
   default configuration."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype fixnum-stack ()
  "The ``fixnum-stack'' type defines a linked-list-based stack, the
   tokens of whose currency are realized in ``fixnum'' objects, their
   wike most commonly appertaining to the castaldy of zero-based
   indices."
  '(list-of fixnum))

;;; -------------------------------------------------------

(deftype numeric-stack ()
  "The ``numeric-stack'' type defines a linked-list-based stack, the
   tokens of whose currency are molded into real-valued numbers,
   disencumbered from any imposition concerning their polarity or
   mickleness."
  '(list-of real))

;;; -------------------------------------------------------

(deftype numeric-list ()
  "The ``numeric-stack'' type defines an ordered linked list, the
   tokens of whose currency are molded into real-valued numbers,
   disencumbered from any imposition concerning their polarity or
   mickleness."
  '(list-of real))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type defines an arithmetic dyadic function
   which accepts a twissel of real-valued operands and produces a single
   result desumed from the selfsame realm."
  '(function (real real) real))

;;; -------------------------------------------------------

(deftype relational-operator ()
  "The ``relational-operator'' type defines a relational dyadic function
   which accepts a twissel of real-valued operands and produces a
   \"generalized boolean\" response, the conflation of whom with a
   \"true\" value serves to signify the represented relation's
   satisfaction, and which for a \"false\" response imputes an
   incompatibility."
  '(function (real real) *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition 1Fish-Error (error)
  ()
  (:documentation
    "The ``1Fish-Error'' condition type accoutres the firmament
     entreparted by all conditions serving in the ministry of an
     analous circumstance's apprizal concerning any stage in a 1><>
     program's execution."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (1Fish-Error)
  ()
  (:report "You cannot peek into or pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology is engendered in an
     attempt to peek into or pop from an empty stack."))

;;; -------------------------------------------------------

(define-condition Unterminated-String-Error (1Fish-Error)
  ((start-point
    :initarg       :start-point
    :initform      (error "No start point has been communicated.")
    :reader        unterminated-string-error-start-point
    :type          fixnum
    :documentation "The zero-based index of the '\"' token which
                    instigated the malformed string mode."))
  (:report
    (lambda (condition stream)
      (declare (type Unterminated-String-Error condition))
      (declare (type stream                    stream))
      (format stream "An unterminated string sequence, commencing at ~
                      the position ~d, has been encountered."
        (unterminated-string-error-start-point condition))))
  (:documentation
    "The ``Unterminated-String-Error'' condition type serves in the
     apprizal about an anomalous circumstance whose etiology is
     engendered during the \"string mode\"'s actuation, commencing with
     a double quotation mark, '\"', but tholing a carency in its
     terminating paregal."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean paregal thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space character,
   inwith whose diorism partake the space and horizontal tab, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (find (char-code candidate) '(9 32) :test #'=))))

;;; -------------------------------------------------------

(defun locate-the-next-space (source start)
  "Proceeding from the inclusive START point into the SOURCE, locates
   the nearest following space or horizontal tab character, and either
   returns its index, or, upon its carency, responds with the ``NIL''
   sentinel."
  (declare (type string source))
  (declare (type fixnum start))
  (the (or null fixnum)
    (position-if #'space-character-p source :start start)))

;;; -------------------------------------------------------

(defun locate-the-next-non-space (source start)
  "Proceeding from the inclusive START point into the SOURCE, locates
   the nearest following character not subsuming into the space or
   horizontal tab species, and either returns its index, or, upon its
   carency, responds with the ``NIL'' sentinel."
  (declare (type string source))
  (declare (type fixnum start))
  (the (or null fixnum)
    (position-if-not #'space-character-p source :start start)))

;;; -------------------------------------------------------

(defun locate-the-next-quote (source start)
  "Proceeding from the inclusive START point into the SOURCE, locates
   the nearest following double quotation mark character, and either
   returns its index, or, upon its carency, responds with the ``NIL''
   sentinel."
  (declare (type string source))
  (declare (type fixnum start))
  (the (or null fixnum)
    (position #\" source :start start :test #'char=)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE string, either
   by its own delivery, upon its compliance with the ``simple-string''
   species, or, in any other case, by a fresh simple string's
   production."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the iterance table builder.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complect-the-jump-points (table code start-token end-token)
  "Alligates the jump points represented by the START-TOKEN as the
   signification of the orgin and the END-TOKEN as the terminal entity,
   stores their zero-based positions in the iterance TABLE, and returns
   the modified TABLE."
  (declare (type iterance-table table))
  (declare (type simple-string  code))
  (declare (type standard-char  start-token))
  (declare (type standard-char  end-token))
  (let ((start-points NIL))
    (declare (type fixnum-stack start-points))
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from 0 below (length code)
      if (char= current-token start-token) do
        (push current-position start-points)
      else if (char= current-token end-token) do
        (if start-points
          (let ((start-point current-position)
               (end-point    (pop start-points)))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf
              (gethash start-point table) end-point
              (gethash end-point   table) start-point))
          (error "The \"~c\" token at the position ~d does not match ~
                  a prevenient \"~c\" instruction."
            start-token current-position end-token))
      end
      finally
        (when start-points
          (let ((number-of-mismatches (length start-points)))
            (declare (type fixnum number-of-mismatches))
            (setf start-points (nreverse start-points))
            (error "The \"~c\" instruction~p at the position~:p ~
                    ~{~d~^, ~} thole~p a carency in the matching end ~
                    token \"~c\"."
              start-token
              number-of-mismatches
              start-points
              number-of-mismatches
              end-token)))))
  (the iterance-table table))

;;; -------------------------------------------------------

(defun build-the-iterance-table-for (code)
  "Creates and returns a fresh ``iterance-table'' which "
  (declare (type simple-string code))
  (the iterance-table
    (complect-the-jump-points
      (complect-the-jump-points
        (make-hash-table :test #'eql)
        code #\{ #\})
      code #\( #\))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the interface "Input-Source".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Source ()
  ()
  (:documentation
    "The ``Input-Source'' interfaces serves to enstate a firmament
     entreparted by all classes to whom the dever of character input
     requests' handling incarnates a parcery."))

;;; -------------------------------------------------------

(defgeneric request-the-next-input (source)
  (:documentation
    "Returns the next character from the input SOURCE; or, upon its
     exhaustion, produces the ``NIL'' sentinel."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Fixed-Input-Source".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Fixed-Input-Source (Input-Source)
  ((characters
    :initarg       :characters
    :initform      ""
    :type          simple-string
    :documentation "The input source's available character sequence.")
   (pointer
    :initform      0
    :type          fixnum
    :documentation "The zero-based index into the SOURCE string
                    designating the next character to return."))
  (:documentation
    "The ``Fixed-Input-Source'' class accoutres a provenance for input
     characters ensuing from a fixed string's content."))

;;; -------------------------------------------------------

(defun make-a-fixed-input-source (characters)
  "Creates and returns a fresh ``Fixed-Input-Source'' whose inputs are
   desumed from the CHARACTERS."
  (declare (type string characters))
  (the Fixed-Input-Source
    (make-instance 'Fixed-Input-Source
      :characters (convert-into-a-simple-string characters))))

;;; -------------------------------------------------------

(defmethod request-the-next-input ((source Fixed-Input-Source))
  (declare (type Fixed-Input-Source source))
  (the (or null character)
    (with-slots (characters pointer) source
      (declare (type simple-string characters))
      (declare (type fixnum        pointer))
      (when (< pointer (length characters))
        (prog1
          (schar characters pointer)
          (incf  pointer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Interactive-Input-Source".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interactive-Input-Source (Input-Source)
  ((displays-prompt-p
    :initarg       :displays-prompt-p
    :initform      T
    :accessor      interactive-input-source-displays-prompt-p
    :type          boolean
    :documentation "A Boolean flag which determines whether, as a
                    parasceve to an input character request, a prompt
                    message concurring with the forbisen \">> \" shall
                    be ostended on the standard output conduit."))
  (:documentation
    "The ``Interactive-Input-Source'' class serves in a character input
     construct's vouchsafement whose provenance concurs with the
     standard input, represented by the Common Lisp ``*query-io*''
     infrastructure."))

;;; -------------------------------------------------------

(defun make-an-interactive-input-source (&key (displays-prompt-p T))
  "Creates and returns a fresh ``Interactive-Input-Source'', the
   consuetude of whom anent a prompt message's display is edified upon
   the DISPLAYS-PROMPT-P flag's configuration."
  (declare (type boolean displays-prompt-p))
  (the Interactive-Input-Source
    (make-instance 'Interactive-Input-Source
      :displays-prompt-p displays-prompt-p)))

;;; -------------------------------------------------------

(defmethod request-the-next-input ((source Interactive-Input-Source))
  (declare (type Interactive-Input-Source source))
  (when (slot-value source 'displays-prompt-p)
    (format *query-io* "~&>> ")
    (force-output *query-io*))
    (the (or null character)
      (prog1
        (read-char *query-io* NIL NIL)
        (clear-input *query-io*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Interpreter".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Input-Source +DEFAULT-INPUT-SOURCE+))

;;; -------------------------------------------------------

(defconstant +DEFAULT-INPUT-SOURCE+
  (make-an-interactive-input-source)
  "The default provenance responsible for a 1><> program's character
   input obtention.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((code
    :initarg       :code
    :initform      (error "No 1><> program has been communicated.")
    :type          simple-string
    :documentation "The piece of 1><> source code to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the 1><> CODE>")
   (iterance-table
    :type          iterance-table
    :documentation "Alligates the matching end points of both the
                    conditional construct \"{...}\" and the iterance
                    mechanism \"(...)\".")
   (stack
    :initform      NIL
    :type          numeric-stack
    :documentation "The program memory as a stack of real-valued
                    numbers.")
   (input-source
    :initarg       :input-source
    :initform      +DEFAULT-INPUT-SOURCE+
    :type          Input-Source
    :documentation "The provenance of the input characters' reception."))
  (:documentation
    "The ``Interpreter'' class is apportioned the dever of a context's
     accoutrement for a 1><> program's execution."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the constructors.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Configures the iterance table for 1><> program consigned to the
   INTERPRETER's castaldy, stores thilk in the INTERPRETER, and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (code iterance-table) interpreter
    (declare (type simple-string  code))
    (declare (type iterance-table iterance-table))
    (setf iterance-table
      (build-the-iterance-table-for code)))
  (values))

;;; -------------------------------------------------------

(defun make-an-interpreter-for
    (code
     &key (input-source +DEFAULT-INPUT-SOURCE+))
  "Creates and returns a fresh ``Interpreter'' nuncupated to the 1><>
   source CODE's evaluation, and whose character input's reception
   ensues from the INPUT-SOURCE's services."
  (declare (type string       code))
  (declare (type Input-Source input-source))
  (the Interpreter
    (make-instance 'Interpreter
      :code         (convert-into-a-simple-string code)
      :input-source input-source)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the navigational operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun program-is-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's instruction pointer (IP) has
   peragrated ayond the underlying source string's admissible bournes,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (code ip) interpreter
      (declare (type simple-string code))
      (declare (type fixnum        ip))
      (not (array-in-bounds-p code ip)))))

;;; -------------------------------------------------------

(defun request-the-current-symbol (interpreter)
  "Returns the character empight at the INTERPRETER's contemporaneous
   instruction pointer (IP) position in the underlying source string."
  (declare (type Interpreter interpreter))
  (the character
    (with-slots (code ip) interpreter
      (declare (type simple-string code))
      (declare (type fixnum        ip))
      (schar code ip))))

;;; -------------------------------------------------------

(defun advance-to-the-next-symbol (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   character in its underlying source string and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (code ip) interpreter
    (declare (type simple-string code))
    (declare (type fixnum        ip))
    (setf ip
      (min (1+ ip) (length code))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-the-opposite-iterance-point (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to the position
   obverse to its currently occupied conditional or iterative end point
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip iterance-table) interpreter
    (declare (type fixnum         ip))
    (declare (type iterance-table iterance-table))
    (setf ip
      (gethash ip iterance-table)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the stack handling operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack-is-empty-p (interpreter)
  "Determines whether the INTERPRETER's stack is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (null
      (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun tally-the-elements-on-the-stack (interpreter)
  "Returns the tally of elements comprising the INTERPRETER's stack."
  (declare (type Interpreter interpreter))
  (the (integer 0 *)
    (length
      (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun push-onto-the-stack (interpreter new-element)
  "Pushes the NEW-ELEMENT onto the INTERPRETER's top stack position and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type real        new-element))
  (with-slots (stack) interpreter
    (declare (type numeric-stack stack))
    (push new-element stack))
  (values))

;;; -------------------------------------------------------

(defun pop-one-element-from-the-stack (interpreter)
  "Removes and returns the top element from the INTERPRETER's stack."
  (declare (type Interpreter interpreter))
  (the real
    (with-slots (stack) interpreter
      (declare (type numeric-stack stack))
      (or (pop stack)
          (error 'Empty-Stack-Error)))))

;;; -------------------------------------------------------

(defun pop-two-elements-from-the-stack (interpreter)
  "Removes the two top elements from the INTERPRETER's stack and returns
   these as two values:
     (1) The erstwhile top stack element.
     (2) The erstwhile element immediately alow the top stack element."
  (declare (type Interpreter interpreter))
  (the (values real real)
    (with-slots (stack) interpreter
      (declare (type numeric-stack stack))
      (values
        (pop-one-element-from-the-stack interpreter)
        (pop-one-element-from-the-stack interpreter)))))

;;; -------------------------------------------------------

(defun clear-the-stack (interpreter)
  "Removes all elements from the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'stack) NIL)
  (values))

;;; -------------------------------------------------------

(defun reverse-the-stack (interpreter)
  "Reverses the order governing the INTERPRETER stack's elements and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type numeric-stack stack))
    (setf stack (nreverse stack)))
  (values))

;;; -------------------------------------------------------

(defun duplicate-the-top-stack-element (interpreter)
  "Duplicates the INTERPRETER's top stack element and returns no value."
  (declare (type Interpreter interpreter))
  (let ((top-element (pop-one-element-from-the-stack interpreter)))
    (declare (type real top-element))
    (push-onto-the-stack interpreter top-element)
    (push-onto-the-stack interpreter top-element))
  (values))

;;; -------------------------------------------------------

(defun duplicate-the-stack (interpreter)
  "Duplicates the INTERPRETER's stack in its entirety and returns no
   value."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type numeric-stack stack))
    (setf stack
      (append stack stack)))
  (values))

;;; -------------------------------------------------------

(defun swap-the-two-top-stack-elements (interpreter)
  "Exchanges the positions of the INTERPRETER stack's two top elements
   and returns no value."
  (declare (type Interpreter interpreter))
  (multiple-value-bind (top-element next-lower-element)
      (pop-two-elements-from-the-stack interpreter)
    (declare (type real top-element))
    (declare (type real next-lower-element))
    (push-onto-the-stack interpreter top-element)
    (push-onto-the-stack interpreter next-lower-element))
  (values))

;;; -------------------------------------------------------

(defun peract-a-binary-operation-on-the-stack (interpreter operator)
  "Pops the two top elements from the INTERPRETER's stack, the top item
   being norned \"x\", its successor immediately alow \"y\", invokes the
   binary OPERATOR with \"y\" and \"x\" in this exact order, pushes the
   result onto the stack, and returns no value."
  (declare (type Interpreter     interpreter))
  (declare (type binary-operator operator))
  (with-slots (stack) interpreter
    (declare (type numeric-stack stack))
    (multiple-value-bind (x y)
        (pop-two-elements-from-the-stack interpreter)
      (declare (type real x))
      (declare (type real y))
      (push-onto-the-stack interpreter
        (funcall operator y x))))
  (values))

;;; -------------------------------------------------------

(defun peract-a-relational-operation-on-the-stack (interpreter operator)
  "Pops the two top elements from the INTERPRETER's stack, the top item
   being norned \"x\", its successor immediately alow \"y\", invokes the
   binary relational OPERATOR with \"y\" and \"x\" in this exact order,
   pushes either the value one (1) onto the stack, upon the represented
   relation's affirmation, or zero (0) in any other case, and returns no
   value."
  (declare (type Interpreter         interpreter))
  (declare (type relational-operator operator))
  (with-slots (stack) interpreter
    (declare (type numeric-stack stack))
    (multiple-value-bind (x y)
        (pop-two-elements-from-the-stack interpreter)
      (declare (type real x))
      (declare (type real y))
      (push-onto-the-stack interpreter
        (if (funcall operator y x)
          1
          0))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the 1><> instruction processing interface.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-the-symbol (interpreter symbol)
  (:documentation
    "Evaluates the SYMBOL in the INTERPRETER's context and returns no
     value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> arithmetic instructions.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\+)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'+)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\-)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'-)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\*)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'*)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\/)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'/)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\|)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'floor)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\^)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'expt)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\%)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-binary-operation-on-the-stack interpreter #'mod)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> comparison instructions.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\<)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-relational-operation-on-the-stack interpreter #'<)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\=)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-relational-operation-on-the-stack interpreter #'=)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\>)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (peract-a-relational-operation-on-the-stack interpreter #'>)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> stack handling instructions.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\l)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (push-onto-the-stack interpreter
    (tally-the-elements-on-the-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\d)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (duplicate-the-top-stack-element interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\D)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (duplicate-the-stack interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\s)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (swap-the-two-top-stack-elements interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\r)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (reverse-the-stack interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\q)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (pop-one-element-from-the-stack interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\c)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (clear-the-stack interpreter)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> input and output instructions.   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\o)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (format *query-io* "~c"
    (code-char
      (round
        (pop-one-element-from-the-stack interpreter))))
  (force-output *query-io*)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\n)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (format *query-io* "~&~a~%"
    (pop-one-element-from-the-stack interpreter))
  (force-output *query-io*)
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\i)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (with-slots (input-source) interpreter
    (declare (type Input-Source input-source))
    (let ((next-input (request-the-next-input input-source)))
      (declare (type (or null character) next-input))
      (push-onto-the-stack interpreter
        (if next-input
          (char-code next-input)
          0))))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\")))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (advance-to-the-next-symbol interpreter)
  (let ((start-point (slot-value interpreter 'ip)))
    (declare (type fixnum start-point))
    (symbol-macrolet
        ((current-symbol
          (the character
            (request-the-current-symbol interpreter))))
      (declare (type character current-symbol))
      (loop do
        (cond
          ((program-is-exhausted-p interpreter)
            (error 'Unterminated-String-Error :start-point start-point))
          ((char= current-symbol #\")
            (loop-finish))
          (T
            (push-onto-the-stack interpreter
              (char-code current-symbol))
            (advance-to-the-next-symbol interpreter))))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> navigational instructions.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\j)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (with-slots (ip) interpreter
    (declare (type fixnum ip))
    (setf ip
      (round
        (1- (pop-one-element-from-the-stack interpreter)))))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\{)))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (when (or (stack-is-empty-p interpreter)
            (zerop (pop-one-element-from-the-stack interpreter)))
    (jump-to-the-opposite-iterance-point interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\})))
  (declare (type Interpreter   interpreter))
  (declare (ignore             interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\()))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (when (stack-is-empty-p interpreter)
    (jump-to-the-opposite-iterance-point interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      (eql #\))))
  (declare (type Interpreter   interpreter))
  (declare (type standard-char symbol))
  (declare (ignore             symbol))
  (unless (stack-is-empty-p interpreter)
    (jump-to-the-opposite-iterance-point interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-the-symbol ((interpreter Interpreter)
                               (symbol      character))
  (declare (type Interpreter interpreter))
  (declare (type character   symbol))
  (when (digit-char-p symbol)
    (push-onto-the-stack interpreter
      (digit-char-p symbol)))
  (values))

;;; -------------------------------------------------------

(defun process-the-current-symbol (interpreter)
  "Processes the INTERPRETER's currently traversed symbol and returns
   no value."
  (declare (type Interpreter interpreter))
  (process-the-symbol interpreter
    (request-the-current-symbol interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the executional operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun launch-the-interpreter (interpreter)
  "Executes the 1><> program concredited to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-exhausted-p interpreter) do
    (process-the-current-symbol interpreter)
    (advance-to-the-next-symbol interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Program-Parameters".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Program-Parameters
  "The ``Program-Parameters'' class accoutres an aggregation of a
   1><> interpreter's command line configurations, the compass
   vouchsafed to whose diorism enumerates an optional file name and a
   siclike not imperative input string."
  (file-name "" :type string :read-only NIL)
  (input     "" :type string :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command line parser.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the inclusive START position into the SOURCE, skips
   a catena of zero or more attiguous spaces or horizontal tabs and
   returns the position into the SOURCE immediately succeeding the thus
   demarcated tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (locate-the-next-non-space source start)
        (length source))))

;;; -------------------------------------------------------

(defun read-a-quoted-word (source start)
  "Proceeding from the inclusive START point into the SOURCE, reads a
   word whose mears are imposed by a jumelle of double quotation marks
   and returns two values:
     (1) A fresh string compact of the characters atwixen the amplecting
         quotation marks, exclusing these symbols themselves.
     (2) The position into the SOURCE immediately succeeding the
         desitive quotation mark."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (let* ((start-point (1+ start))
           (end-point   (locate-the-next-quote source start-point)))
      (declare (type fixnum           start-point))
      (declare (type (or null fixnum) end-point))
      (if end-point
        (values
          (subseq source start-point end-point)
          (1+ end-point))
        (error "The quoted word commencing at the position ~d has not ~
                been terminated."
          start)))))

;;; -------------------------------------------------------

(defun read-an-unquoted-word (source start)
  "Proceeding from the inclusive START point into the SOURCE, reads a
   word concluding in a space, horizontal tab, or the SOURCE's desition,
   and returns two values:
     (1) A fresh string compact of the word's character.
     (2) The position into the SOURCE immediately succeeding the
         extracted word's desinent character."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (let ((end-point
            (or (locate-the-next-space source start)
                (length source))))
      (declare (type (or null fixnum) end-point))
      (values
        (subseq source start end-point)
        end-point))))

;;; -------------------------------------------------------

(defun read-the-next-word (source start)
  "Proceeding from the inclusive START point into the SOURCE, skips the
   contingent prevenience of spaces, extracts the nearest following
   word, either quoted or without a double quotation mark's concomitant
   ensconcement, and returns two values:
     (1) If a word compact of at least one character could be detected,
         a fresh string comprehending the same; otherwise the ``NIL''
         sentinel.
     (2) If a word compact of at least one character could be detected,
         the position into the SOURCE immediately succeeding the tmema
         occupied by the token; otherwise the START index itself."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (or null string) fixnum)
    (let ((start-point (skip-spaces source start)))
      (declare (type fixnum start-point))
      (cond
        ((not (array-in-bounds-p source start))
          (values NIL start-point))
        ((string= source #\" :start1 start-point :end1 (1+ start-point))
          (read-a-quoted-word source start-point))
        (T
          (read-an-unquoted-word source start-point))))))

;;; -------------------------------------------------------

(defun parse-the-command-line (parameters source)
  "Parses the SOURCE, representative of a command line behest, stores
   its extracted configurations in the program PARAMETERS object, and
   returns the same."
  (declare (type Program-Parameters parameters))
  (declare (type string             source))
  (let ((current-position 0))
    (declare (type fixnum current-position))
    ;; Extract the contingent file name.
    (multiple-value-bind (current-word new-position)
        (read-the-next-word source 0)
      (declare (type (or null string) current-word))
      (declare (type fixnum           new-position))
      (psetf
        (program-parameters-file-name parameters)
          current-word
        current-position
          (skip-spaces source new-position)))
    ;; Extract the contingent input parameters, introduced via the "-i"
    ;; command line argument.
    (multiple-value-bind (current-word new-position)
        (read-the-next-word source current-position)
      (declare (type (or null string) current-word))
      (declare (type fixnum           new-position))
      (cond
        ((null current-word)
          NIL)
        ((string= current-word "-i")
          (when (array-in-bounds-p source (1+ new-position))
            (setf (program-parameters-input parameters)
              (subseq source
                (1+ new-position)))))
        (T
          (error "The parameter string ~s ostends invalid content ~
                  inchoating at the position ~d."
            source current-position)))))
  (the Program-Parameters parameters))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the source file reading operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-the-1Fish-source-file (source)
  "Loads the 1><> SOURCE code file and returns a fresh simple string
   comprehending its content."
  (declare (type (or pathname stream string) source))
  (the simple-string
    (with-open-file (source-stream source
                     :direction         :input
                     :element-type      'character
                     :if-does-not-exist :error)
      (declare (type file-stream source-stream))
      (let ((file-content (make-string (file-length source-stream))))
        (declare (type string file-content))
        (read-sequence file-content source-stream)
        file-content))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the 1><> interpretation operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-1Fish-code
    (code
     &key (input-source +DEFAULT-INPUT-SOURCE+))
  "Interprets the piece of 1><> source CODE, naiting the INPUT-SOURCE
   for its contingent character input requests, and returns no value."
  (declare (type string       code))
  (declare (type Input-Source input-source))
  (launch-the-interpreter
    (make-an-interpreter-for code
      :input-source input-source))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-1Fish-command-line (command)
  "Parses the COMMAND line, loads the referenced 1><> source file,
   interprets thilk, and returns no value."
  (declare (type string command))
  (let ((parameters
          (parse-the-command-line
            (make-program-parameters)
            command)))
    (declare (type Program-Parameters parameters))
    (launch-the-interpreter
      (make-an-interpreter-for
        (load-the-1Fish-source-file
          (program-parameters-file-name parameters))
        :input-source
          (make-a-fixed-input-source
            (program-parameters-input parameters)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, world!".
(interpret-the-1Fish-code "\"Hello, world!\"r(o)")

;;; -------------------------------------------------------

;; Print the message "Hello, world!", its provenance an external file.
;; 
;; Please heed that the file "helloWorld.1f" must be accessible under
;; the communicated path.
(interpret-the-1Fish-command-line "./resources/helloWorld.1f")

;;; -------------------------------------------------------

;; Repeating character-based cat program which terminates on a
;; "null character" (ASCII code: 0) input.
(interpret-the-1Fish-code "i(d{oid}q)"
  :input-source (make-a-fixed-input-source "Sparrows are cute."))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-1Fish-code "i86*-{1(n1)}0n")
