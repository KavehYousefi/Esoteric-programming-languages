;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Bleph!", invented by the Esolang user "A()" and presented
;; on June 21st, 2025, its particular caract's commorancy a bespoke
;; character repertoire's deployment, in lieu of the nomothesy installed
;; in the ASCII standard, as the vehicle to the realization of the
;; commerce along the input and output conduits, while operating,
;; naiting a discordant set of instruction tokens, on a stack
;; concredited with the castaldy of signed integer numbers.
;; 
;; 
;; Concept
;; =======
;; The Bleph! programming language's foundry is established upon the
;; notion of an operative warklume vouchsafed with a cadeau widdershins
;; to any vetust eidolon anent its syntaxis; operating on a stack of
;; signed integer elements, while deploying a character set restricted
;; to 80 ASCII character, to whom an etypic assignment of character
;; codes as to the entheus is apportioned.
;; 
;; == "BLEPH!": YOUR UTTERANCE UPON THE CODE'S CONSPECTION ==
;; The substratumg serving in the "Bleph!" programming language's
;; agnomination constitutes an allusion to the inodiating designment
;; commorant in its syntaxis.
;; 
;; == A BESPOKE CHARACTER REPERTOIRE GOVERNS BLEPH! ==
;; Among those aspects inwith whose compass the Bleph! programming
;; language abludes from ingrained acquaintances appertains to the
;; underlying character repertoire and its encoding, thilk, in lieu of
;; a reliance upon the ASCII mapping atwixen a symbol and its numeric
;; identification relays this wike to a personal choice, incorporating
;; as a concomitant a curtailment's adhibition to 80 members.
;; 
;; The following tabular exposition's dation shall be satisfies in the
;; recognized character set's membership, its unambiguously affiliated
;; Bleph! character code, and an equiparation with the competing ASCII
;; encoding notion:
;; 
;;   ------------------------------------
;;   Character | Bleph! code | ASCII code
;;   ----------+-------------+-----------
;;   (space)   | 0           | 32        
;;   ....................................
;;   a         | 1           | 97        
;;   ....................................
;;   b         | 2           | 98        
;;   ....................................
;;   c         | 3           | 99        
;;   ....................................
;;   d         | 4           | 100       
;;   ....................................
;;   e         | 5           | 101       
;;   ....................................
;;   f         | 6           | 102       
;;   ....................................
;;   g         | 7           | 103       
;;   ....................................
;;   h         | 8           | 104       
;;   ....................................
;;   i         | 9           | 105       
;;   ....................................
;;   j         | 10          | 106       
;;   ....................................
;;   k         | 11          | 107       
;;   ....................................
;;   l         | 12          | 108       
;;   ....................................
;;   m         | 13          | 109       
;;   ....................................
;;   n         | 14          | 110       
;;   ....................................
;;   o         | 15          | 111       
;;   ....................................
;;   p         | 16          | 112       
;;   ....................................
;;   q         | 17          | 113       
;;   ....................................
;;   r         | 18          | 114       
;;   ....................................
;;   s         | 19          | 115       
;;   ....................................
;;   t         | 20          | 116       
;;   ....................................
;;   u         | 21          | 117       
;;   ....................................
;;   v         | 22          | 118       
;;   ....................................
;;   w         | 23          | 119       
;;   ....................................
;;   x         | 24          | 120       
;;   ....................................
;;   y         | 25          | 121       
;;   ....................................
;;   z         | 26          | 122       
;;   ....................................
;;   A         | 27          | 65        
;;   ....................................
;;   B         | 28          | 66        
;;   ....................................
;;   C         | 29          | 67        
;;   ....................................
;;   D         | 30          | 68        
;;   ....................................
;;   E         | 31          | 69        
;;   ....................................
;;   F         | 32          | 70        
;;   ....................................
;;   G         | 33          | 71        
;;   ....................................
;;   H         | 34          | 72        
;;   ....................................
;;   I         | 35          | 73        
;;   ....................................
;;   J         | 36          | 74        
;;   ....................................
;;   K         | 37          | 75        
;;   ....................................
;;   L         | 38          | 76        
;;   ....................................
;;   M         | 39          | 77        
;;   ....................................
;;   N         | 40          | 78        
;;   ....................................
;;   O         | 41          | 79        
;;   ....................................
;;   P         | 42          | 80        
;;   ....................................
;;   Q         | 43          | 81        
;;   ....................................
;;   R         | 44          | 82        
;;   ....................................
;;   S         | 45          | 83        
;;   ....................................
;;   T         | 46          | 84        
;;   ....................................
;;   U         | 47          | 85        
;;   ....................................
;;   V         | 48          | 86        
;;   ....................................
;;   W         | 49          | 87        
;;   ....................................
;;   X         | 50          | 88        
;;   ....................................
;;   Y         | 51          | 89        
;;   ....................................
;;   Z         | 52          | 90        
;;   ....................................
;;   !         | 53          | 33        
;;   ....................................
;;   ?         | 54          | 63        
;;   ....................................
;;   @         | 55          | 64        
;;   ....................................
;;   #         | 56          | 35        
;;   ....................................
;;   $         | 57          | 36        
;;   ....................................
;;   %         | 58          | 37        
;;   ....................................
;;   ^         | 59          | 94        
;;   ....................................
;;   *         | 60          | 42        
;;   ....................................
;;   &         | 61          | 38        
;;   ....................................
;;   (         | 62          | 40        
;;   ....................................
;;   )         | 63          | 41        
;;   ....................................
;;   _         | 64          | 95        
;;   ....................................
;;   +         | 65          | 43        
;;   ....................................
;;   -         | 66          | 45        
;;   ....................................
;;   [         | 67          | 91        
;;   ....................................
;;   ]         | 68          | 93        
;;   ....................................
;;   :         | 69          | 58        
;;   ....................................
;;   0         | 70          | 48        
;;   ....................................
;;   1         | 71          | 49        
;;   ....................................
;;   2         | 72          | 50        
;;   ....................................
;;   3         | 73          | 51        
;;   ....................................
;;   4         | 74          | 52        
;;   ....................................
;;   5         | 75          | 53        
;;   ....................................
;;   6         | 76          | 54        
;;   ....................................
;;   7         | 77          | 55        
;;   ....................................
;;   8         | 78          | 56        
;;   ....................................
;;   9         | 79          | 57        
;;   ------------------------------------
;; 
;; == THE MEMORY: A STACK OF INTEGER NUMBERS ==
;; The data castaldy's attrectation constitutes a stack's bailiwick,
;; inwith whose governail merely integer numbers, however, disencumbered
;; from any sign or magnitude's imposition, are subsumed.
;; 
;; At the program's inchoation empight in a state of vacancy, operations
;; exist for the insertion at both of its extrema, as well as such for
;; deletions, and a feelefold accompt regarding sophisticated
;; perquisitions and modulations.
;; 
;; Every attempt at requesting a stack element, regardless of a peeking
;; or deleting intention, will instigate an abortive "EmptyStackError"'s
;; infliction.
;; 
;; 
;; Instructions
;; ============
;; The Bleph! instruction set enumerates a contingency of 14 members,
;; the bailiwick's occupation such as to preside over the stack
;; management, basic arithmetic commorant in the unary and binary realm,
;; input and output facilities, as well as an aefauld conditional
;; iterance construct.
;; 
;; Any content disencumbered from the ligation with an operative dever
;; shall be a tolerance's recipient in a mete that is equipensated with
;; perfect neglect.
;; 
;; == OVERVIEW ==
;; A dation of a sufficient nortelry's ilk anent the operative
;; paraphernalia shall constitute the following apercu's telos.
;; 
;; Please heed the demarcation of succedaneous tmemata by an underlining
;; catena composed of asterisks ("*"), intended for their supersession
;; by actual Bleph! code in the ultimate program.
;; 
;;   ==================================================================
;;   Command      | Effect
;;   ==================================================================
;;   STACK MANAGEMENT
;;   ------------------------------------------------------------------
;;   ^            | Inserts (pushes) a new zero-valued element (0) at
;;                | the stack's top position.
;;   ..................................................................
;;   v            | Inserts a new zero-valued element (0) at the
;;                | stack's bottom position.
;;   ..................................................................
;;   :            | Duplicates the top stack element.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   s            | Swaps the positions of the stack's two top
;;                | elements.
;;                |----------------------------------------------------
;;                | If the stack cannot furnish at least two elements,
;;                | an error of the type "InsufficientStackSizeError"
;;                | is signaled.
;;   ..................................................................
;;   ?            | Selects in an aleatory fashion one among the
;;                | stack's two top elements, relocates the same to the
;;                | top position, if not already empight there, and
;;                | removes the other, not selected item.
;;                |----------------------------------------------------
;;                | If the stack cannot furnish at least two elements,
;;                | an error of the type "InsufficientStackSizeError"
;;                | is signaled.
;;   ==================================================================
;;   UNARY ARITHMETICS
;;   ------------------------------------------------------------------
;;   _            | If the top stack element equals one (1), increments
;;                | the same by one (1); otherwise decrements it by
;;                | the same amount.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   +            | Increments the top stack element by one (1).
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   -            | Decrements the top stack element by one (1).
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ==================================================================
;;   BINARY ARITHMETICS
;;   ------------------------------------------------------------------
;;   !            | Pops the top stack element, here nevened "a", and
;;                | the new top stack element, "b", supputates the sum
;;                | "c" as:
;;                |   c = a + b,
;;                | and pushes "c" onto the stack.
;;                |----------------------------------------------------
;;                | If the stack cannot furnish at least two elements,
;;                | an error of the type "InsufficientStackSizeError"
;;                | is signaled.
;;   ..................................................................
;;   @            | Pops the top stack element, here nevened "a", and
;;                | the new top stack element, "b", supputates the
;;                | difference "c" as:
;;                |   c = a - b,
;;                | and pushes "c" onto the stack.
;;                |----------------------------------------------------
;;                | If the stack cannot furnish at least two elements,
;;                | an error of the type "InsufficientStackSizeError"
;;                | is signaled.
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   #            | Queries the standard input conduit for a character
;;                | and sets the top stack element to its Bleph!
;;                | character code.
;;                |----------------------------------------------------
;;                | If the committed character does not partake of an
;;                | existency in the Bleph! character repertoire, the
;;                | inquisition perpetuates until an admissible input's
;;                | provision.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   O            | Pops the top stack element and prints it in its
;;                | numeric form to the standard output conduit.
;;                |----------------------------------------------------
;;                | The output is empight at the start of a line of its
;;                | own, and terminates in a single newline character.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ..................................................................
;;   ~            | Pops the top stack element and prints the character
;;                | whose Bleph! character code concurs with the same.
;;                |----------------------------------------------------
;;                | If the top stack element's value does not
;;                | correspond to any Bleph! character, an error of the
;;                | type "InvalidCharacterCode" is signaled.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of this
;;                | operations's invocation, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ==================================================================
;;   CONTROL FLOW
;;   ------------------------------------------------------------------
;;   [ commands ] | Executes the {commands} while the top stack element
;;     ********   | does not equal zero (0).
;;                |----------------------------------------------------
;;                | Upon each cycle's inchoation, the element at the
;;                | stack's top position is peeked without removal.
;;                |----------------------------------------------------
;;                | {commands} must be an ordered sequence
;;                | comprehending zero or more commands.
;;                |----------------------------------------------------
;;                | If the stack is empty at the instant of the top
;;                | element's perquisition, an error of the type
;;                | "EmptyStackError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort exercised in
;; the programming language Common Lisp, its working adhibited with
;; immediacy on the source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-13
;; 
;; Sources:
;;   [esolang2025:Bleph!]
;;   The Esolang contributors, "Bleph!", June 23rd, 2025
;;   URL: "https://esolangs.org/wiki/Bleph!"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is desumed from the
   TYPE-NAME and whose formal parameters constitute an ipsissima verba
   appropriation from the LAMBDA-LIST, the docimasy's subject being the
   CANDIDATE-NAME as the nevening's recipient, evaluates the BODY forms
   as the antecedent, deriving its conclusion concerning the type
   compatibility from the desinent form's primary result, with a
   \"generalized boolean\" value of \"true\" furnishing a confirmation's
   tantamount, while a \"false\" response signifies a mismatch.
   ---
   Upon the first BODY form's resolution to a string literal, the same
   is construed as the derived type's documentation string, and is
   administered a reappropriation for this purpose."
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

(define-a-bespoke-type hash-table-of (candidate
                                      &optional (key-type   '*)
                                                (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, these being defines in terms of a
   key adhering to the KEY-TYPE and an allied value subsuming into the
   VALUE-TYPE, for both governs the generic sentinel ``*'' the default
   configuration."
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

(define-a-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a linked list of zero or more elements'
   participation, for everichon inwith this salvatory holds an adherence
   to the ELEMENT-TYPE as the sole imposition, defaulting to the generic
   sentinel ``*''."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype iteration-table ()
  "The ``iteration-table'' type defines a bidirectional association
   betwixt an iterance construct's start and end points, the vincula's
   signification a gendrure begotten by a hash table's state as the
   accoucheuse, the mode of its castaldy the representation of both
   extrema by their zero-based ``fixnum'' indices into the Bleph!
   program."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the Bleph! character repertoire.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 80) +BLEPH!-CHARACTER-REPERTOIRE+))

;;; -------------------------------------------------------

(defconstant +BLEPH!-CHARACTER-REPERTOIRE+
  (coerce
    (format NIL " ~
                 abcdefghijklmnopqrstuvwxyz~
                 ABCDEFGHIJKLMNOPQRSTUVWXYZ~
                 !?@#$%^*&()_+-[]:~
                 0123456789")
    'simple-base-string)
  "The ``+BLEPH!-CHARACTER-REPERTOIRE+'' global constant defines the
   recognized Bleph! character repertoire, amenable by its natural
   arrangement's adminiculum to zero-based subscripts concurring with
   their bespoke character codes.")

;;; -------------------------------------------------------

(defun look-up-the-Bleph!-character (code)
  "Returns the character endowed with an amenability to the zero-based
   character CODE according to the Bleph! repertoire; or signals an
   error of the type ``Invalid-Character-Code-Error'' upon its
   disrespondency."
  (declare (type integer code))
  (the standard-char
    (if (array-in-bounds-p +BLEPH!-CHARACTER-REPERTOIRE+ code)
      (schar +BLEPH!-CHARACTER-REPERTOIRE+ code)
      (error 'Invalid-Character-Code-Error :offending-code code))))

;;; -------------------------------------------------------

(defun look-up-the-Bleph!-character-code (character)
  "Returns the zero-based character code of the CHARACTER according to
   the Bleph! repertoire; or signals an error of the type
   ``Invalid-Character-Error''."
  (declare (type character character))
  (the (integer 0 79)
    (or (position character +BLEPH!-CHARACTER-REPERTOIRE+ :test #'char=)
        (error 'Invalid-Character-Error
          :offending-character character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the iteration table operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-iteration-table ()
  "Returns a fresh ``iteration-table'' whose state at its inchoacy
   ostends a complete vacancy."
  (the iteration-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-the-iteration-posts (table start-point end-point)
  "Connects the iteration START-POINT and END-POINT in a bidirectional
   fashion, stores these vincula in the iteration TABLE, and returns no
   value."
  (declare (type iteration-table table))
  (declare (type fixnum          start-point))
  (declare (type fixnum          end-point))
  (psetf
    (gethash start-point table) end-point
    (gethash end-point   table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-the-iteration-table-for (program)
  "Creates and returns a fresh ``Iteration-table'' concredited with the
   castaldy of the Bleph! PROGRAM's iteration points by adminiculum of
   their zero-based positions."
  (declare (type simple-string program))
  (let ((connections  (prepare-an-empty-iteration-table))
        (start-points NIL))
    (declare (type iteration-table  connections))
    (declare (type (list-of fixnum) start-points))
    (the iteration-table
      (loop
        for current-token    of-type character across program
        and current-position of-type fixnum    from   0 by 1
        do
          (case current-token
            (#\[
              (push current-position start-points))
            (#\]
              (if start-points
                (connect-the-iteration-posts connections
                  (pop start-points)
                  current-position)
                (error "Unmatched \"]\" instruction at position ~d."
                  current-position)))
            (otherwise
              NIL))
        finally
          (if start-points
            (error "Unmatched \"[\" instruction~p at position~:p ~
                    ~{~d~^, ~}."
              (length start-points)
              start-points)
            (return connections))))))

;;; -------------------------------------------------------

(defun locate-the-opposite-iteration-post (table point-of-departure)
  "Returns the position of the end point associated with the
   POINT-OF-DEPARTURE in the iteration TABLE; or, upon its
   disrespondency, signals an error of an unspecified type."
  (declare (type iteration-table table))
  (declare (type fixnum          point-of-departure))
  (the fixnum
    (or (gethash point-of-departure table)
        (error "No opposite iteration post exists for the position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Stack".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Stack
  (:constructor prepare-an-empty-stack ()))
  "The ``Stack'' class is apportioned the dever of a salvatory's
   furnishment, nuncupated to an arbitary accompt of signed integer
   elements' castaldy."
  (elements NIL :type (list-of integer) :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-stack ((stack) &body body)
  "Evaluates the STACK, binds its slot ``elements'' to the local symbol
   macro ``$elements'', evaluates the BODY forms, and returns the
   desinent form's results."
  (let ((evaluated-stack (gensym)))
    (declare (type symbol evaluated-stack))
    `(let ((,evaluated-stack ,stack))
       (declare (type Stack ,evaluated-stack))
       (symbol-macrolet
           (($elements
              (the (list-of integer)
                (stack-elements ,evaluated-stack))))
         (declare (type (list-of integer) $elements))
         (declare (ignorable              $elements))
         ,@body))))

;;; -------------------------------------------------------

(defun validate-that-the-stack-is-not-empty (stack)
  "Determines whether the STACK is empty, on confirmation signaling an
   error of the type ``Empty-Stack-Error''; otherwise accompasses no
   further causatum, and returns no value."
  (declare (type Stack stack))
  (with-stack (stack)
    (unless $elements
      (error 'Empty-Stack-Error)))
  (values))

;;; -------------------------------------------------------

(defun validate-at-least-two-elements-on-the-stack (stack)
  "Determines whether the STACK comprehends less than two elements,
   on confirmation signaling an error of the type
   ``Insufficient-Stack-Size-Error''; otherwise accompasses no further
   causatum, and returns no value."
  (declare (type Stack stack))
  (with-stack (stack)
    (let ((actual-stack-size (length $elements)))
      (declare (type (integer 0 *) actual-stack-size))
      (unless (>= actual-stack-size 2)
        (error 'Insufficient-Stack-Size-Error
          :minimum-size 2
          :actual-size  actual-stack-size))))
  (values))

;;; -------------------------------------------------------

(defun top-stack-element (stack)
  "Returns without removing the STACK's top element."
  (declare (type Stack stack))
  (validate-that-the-stack-is-not-empty stack)
  (the integer
    (with-stack (stack)
      (first $elements))))

;;; -------------------------------------------------------

(defun (setf top-stack-element) (new-value stack)
  "Replaces the STACK's top element by the NEW-VALUE and returns no
   value."
  (declare (type integer new-value))
  (declare (type Stack   stack))
  (validate-that-the-stack-is-not-empty stack)
  (with-stack (stack)
    (setf (first $elements) new-value))
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-stack (stack)
  "Removes and returns the STACK's top element."
  (declare (type Stack stack))
  (validate-that-the-stack-is-not-empty stack)
  (the integer
    (with-stack (stack)
      (pop $elements))))

;;; -------------------------------------------------------

(defun select-a-random-element-from-the-stack (stack)
  "Selects in an aleatory mode one element among the STACK's two top
   stack positions and relocates thilk to the STACK's top, while
   removing and discarding the other candidate, and returns no value."
  (declare (type Stack stack))
  (validate-at-least-two-elements-on-the-stack stack)
  (with-stack (stack)
    (when (zerop (random 2))
      (rotatef
        (first  $elements)
        (second $elements)))
    (pop $elements))
  (values))

;;; -------------------------------------------------------

(defun insert-at-the-stack-top (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's top position and returns no
   value."
  (declare (type Stack   stack))
  (declare (type integer new-element))
  (with-stack (stack)
    (push new-element $elements))
  (values))

;;; -------------------------------------------------------

(defun insert-at-the-stack-bottom (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's bottom position and returns no
   value."
  (declare (type Stack   stack))
  (declare (type integer new-element))
  (with-stack (stack)
    (setf $elements (nreverse $elements))
    (push new-element $elements)
    (setf $elements (nreverse $elements)))
  (values))

;;; -------------------------------------------------------

(defun duplicate-the-top-stack-element (stack)
  "Duplicates the STACK's top element and returns no value."
  (declare (type Stack stack))
  (validate-that-the-stack-is-not-empty stack)
  (with-stack (stack)
    (push (first $elements) $elements))
  (values))

;;; -------------------------------------------------------

(defun swap-the-top-stack-elements (stack)
  "Duplicates the STACK's top element and returns no value."
  (declare (type Stack stack))
  (validate-at-least-two-elements-on-the-stack stack)
  (with-stack (stack)
    (rotatef
      (first  $elements)
      (second $elements)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Bleph!-Error (error)
  ()
  (:documentation
    "The ``Bleph!-Error'' condition type's bailiwick is realized in a
     firmament's inclavation whence ensue all condition types to whom
     the communication of anomalous circumstances in a Bleph! program's
     diadrom are assigned."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Bleph!-Error)
  ()
  (:report
    "You cannot peek into or pop from an empty stack.")
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology emerges from the
     illicit attempt to engage in a perquisition into or removal from a
     stack in the state of vacancy."))

;;; -------------------------------------------------------

(define-condition Insufficient-Stack-Size-Error (Bleph!-Error)
  ((minimum-size
    :initarg       :minimum-size
    :initform      (error "Missing the minimum stack size.")
    :reader        insufficient-stack-size-error-minimum-size
    :type          (integer 0 *)
    :documentation "The minimum tally of elements required for the
                    pernicious operation.")
   (actual-size
    :initarg       :actual-size
    :initform      (error "Missing the actual stack size.")
    :reader        insufficient-stack-size-error-actual-size
    :type          (integer 0 *)
    :documentation "The actual, insufficient tally of elements
                    concredited to the stack's castaldy."))
  (:report
    (lambda (condition stream)
      (declare (type Insufficient-Stack-Size-Error condition))
      (declare (type stream                        stream))
      (format stream "The stack requires at least ~d element~:p, ~
                      but comprehends only ~d."
        (insufficient-stack-size-error-minimum-size condition)
        (insufficient-stack-size-error-actual-size  condition))))
  (:documentation
    "The ``Insufficient-Stack-Size-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology emerges from
     the illicit attempt to request a tally of elements from the stack
     the same is inflicted with a carency in the requisitum's
     provision."))

;;; -------------------------------------------------------

(define-condition Invalid-Character-Code-Error (Bleph!-Error)
  ((offending-code
    :initarg       :offending-code
    :initform      (error "Missing the offending character code.")
    :reader        invalid-character-code-error-offending-code
    :type          integer
    :documentation "The integer number thilk does not map to any
                    Bleph! character."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Character-Code-Error condition))
      (declare (type stream                       stream))
      (format stream "The value ~d does not represent a valid Bleph!
                      character code."
        (invalid-character-code-error-offending-code condition))))
  (:documentation
    "The ``Invalid-Character-Code-Error'' condition type serves in the
     apprizal about an anomalous situtation whose etiology emerges from
     the illicit attempt to request a character by an integral code
     not specified in the Bleph! character repertoire."))

;;; -------------------------------------------------------

(define-condition Invalid-Character-Error (Bleph!-Error)
  ((offending-character
    :initarg       :offending-character
    :initform      (error "Missing the offending character.")
    :reader        invalid-character-error-offending-character
    :type          character
    :documentation "The character whose existency does not partake of
                    the Bleph! repertoire's diorism."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Character-Error condition))
      (declare (type stream                  stream))
      (format stream "The character \"~c\" does not partake of the ~
                      valid Bleph! character repertoire."
        (invalid-character-error-offending-character condition))))
  (:documentation
    "The ``Invalid-Character-Code-Error'' condition type serves in the
     apprizal about an anomalous situtation whose etiology emerges from
     the illicit attempt to request the character code of a symbol not
     specified in the Bleph! character repertoire."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input and output operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-a-character ()
  "Repeatedly queries the standard input conduit until an admissible
   Bleph! symbol has been committed, and finally returns its numeric
   character code."
  (the (integer 0 79)
    (let ((original-input NIL)
          (parsed-input   NIL))
      (declare (type (or null character) original-input))
      (declare (type (or null integer)   parsed-input))
      (loop do
        (format T "~&>> ")
        (finish-output)
        (setf original-input
          (read-char NIL NIL #\Space))
        (clear-input)
        ;; Interpret the received input character.
        (handler-case
            (setf parsed-input
              (look-up-the-Bleph!-character-code original-input))
          (Invalid-Character-Error (error)
            (format T "~&~a" error)
            (finish-output)
            (setf parsed-input NIL))
          (error ()
            (format T "~&The input \"~c\" cannot be resolved."
              original-input)
            (setf parsed-input NIL)
            (finish-output)))
        ;; Terminate the loop if the input character could be
        ;; translated into its Bleph! character code.
        (when parsed-input
          (return parsed-input))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Bleph!
    (code
     &aux (simplified-code (coerce code 'simple-string)))
  "Interprets the piece of Bleph! source CODE and returns no value."
  (declare (type string code))
  (let ((ip              0)
        (iteration-table (supputate-the-iteration-table-for
                           simplified-code))
        (stack           (prepare-an-empty-stack)))
    (declare (type simple-string simplified-code))
    (declare (type Stack         stack))
    (loop while (< ip (length simplified-code)) do
      (case (schar simplified-code ip)
        (#\_
          (if (= (top-stack-element stack) 1)
            (incf (top-stack-element stack))
            (decf (top-stack-element stack))))
        
        (#\?
          (select-a-random-element-from-the-stack stack))
        
        (#\+
          (incf (top-stack-element stack)))
        
        (#\-
          (decf (top-stack-element stack)))
        
        (#\O
          (format T "~&~d~%"
            (pop-from-the-stack stack)))
        
        (#\~
          (format T "~c"
            (look-up-the-Bleph!-character
              (pop-from-the-stack stack))))
        
        (#\[
          (when (zerop (top-stack-element stack))
            (setf ip
              (locate-the-opposite-iteration-post iteration-table ip))))
        
        (#\]
          (setf ip
            (locate-the-opposite-iteration-post iteration-table ip))
          (decf ip))
        
        (#\#
          (setf (top-stack-element stack)
            (query-for-a-character)))
        
        (#\^
          (insert-at-the-stack-top stack 0))
        
        (#\v
          (insert-at-the-stack-bottom stack 0))
        
        (#\!
          (let ((augend (pop-from-the-stack stack))
                (addend (pop-from-the-stack stack)))
            (declare (type integer augend))
            (declare (type integer addend))
            (insert-at-the-stack-top stack
              (+ augend addend))))
        
        (#\@
          (let ((minuend    (pop-from-the-stack stack))
                (subtrahend (pop-from-the-stack stack)))
            (declare (type integer minuend))
            (declare (type integer subtrahend))
            (insert-at-the-stack-top stack
              (- minuend subtrahend))))
        
        (#\:
          (duplicate-the-top-stack-element stack))
        
        (#\s
          (swap-the-top-stack-elements stack))
        
        (otherwise
          NIL))
      
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-Bleph! "^#[:~#:]")

;;; -------------------------------------------------------

;; Print the message "Hello world!".
(interpret-Bleph!
  "^++++++++++++++++++++++++++++++++++
   :~[-]
   +++++
   :~[-]
   ++++++++++++
   :~:~[-]
   +++++++++++++++
   :~[-]
   :~[-]
   +++++++++++++++++++++++
   :~[-]
   +++++++++++++++
   :~[-]
   ++++++++++++++++++
   :~[-]
   ++++++++++++
   :~[-]
   ++++
   :~[-]+++++++++++++++++++++++++++++++++++++++++++++++++++++
   :~[-]")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The following pseudocode tmema serves in the underlying concept's
;; elucidation:
;; 
;;   insert an element at the stack's top position
;;   let input <- query for a Bleph! character and obtains its code
;;   { Reduce the "0" (= 70) or "1" (= 71) character code to the }
;;   { number 0 or 1.                                            }
;;   input <- input - 70
;;   set the top stack element to input
;;   
;;   while top stack element != 0 do
;;     duplicate the top stack element
;;     pop the top stack element and print it in its numeric form
;;   end while
(interpret-Bleph!
  "^#
   -----------------------------------
   -----------------------------------
   [:O]")

;;; -------------------------------------------------------

;; A very boring guessing game.
;; 
;; Randomly generates on the stack a value of zero (0) or one (1),
;; queries the standard input conduit for a bit value, that is, either
;; the decimal digit "0" or "1", and prints upon an owelty's governail
;; betwixt the two stack elements the number "0", otherwise signifies
;; the mismatch via "-1".
(interpret-Bleph!
  "^+
   ^?
   ^#----------------------------------------------------------------------
   @:O")

;;; -------------------------------------------------------

;; Addition: Query a twissel of characters from the standard input, sum
;; the same, push the result onto the stack, and print thilk in its
;; numeric form.
(interpret-Bleph! "^#^#!O")

;;; -------------------------------------------------------

;; Subtraction: Query a twissel of characters from the standard input,
;; supputate their difference, push the result onto the stack, and
;; print thilk in its numeric form.
;; 
;; Please heed how the swapping operation "s" ascertains that the first
;; input serves as the minuend and second as the subtrahend.
(interpret-Bleph! "^#^#s@O")
