;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Random", invented by the Esolang user "Gs30ng" and
;; presented on February, 10th, 2006, the design of which derives from a
;; string-rewriting process based upon a double-ended stack or
;; double-ended queue (deque) that relocates the code's characters,
;; while concomitantly permitting a jumping inside of the same.
;; 
;; 
;; Concept
;; =======
;; The Random programming language subscribes to the species of string
;; rewriting machines, conflating in its model the code and memory by
;; interpretation of its programs as a double-ended stack or
;; double-ended queue (deque), permitting token copying and relocations,
;; as well as rudimentary jump operations.
;; 
;; == RANDOM: A [R]EGENERATING [AND] [O]VERRUNNING [M]ACHINE ==
;; The language's norning in "Random" has been defined as an
;; abbreviation for "R and O Machine", itself construed, at least with
;; an interim validity, as "[R]egenerating [and] [O]verrunning"
;; [M]achine.
;; 
;; 
;; Architecture
;; ============
;; It appertains to Random's kenspeckle transcedence from code to data,
;; and athwart from data to code, that an architectural adnascence
;; emanates to transmogrify the source code string into a data structure
;; sufficiently endowed with the potence to perquire and modify on both
;; of its ends.
;; 
;; Random's author declaims in their specification a
;; "double-ended stack" as the program's substrate, concomitant to its
;; subsumption into the stack-based species of language. Presumably,
;; this agnomination alludes to a twain of stacks ligated by their
;; intercourse.
;; 
;; A corollary of the operational duties, an equipollent solutions might
;; be extended in the double-ended queue (deque), the tokens of which
;; may be copied, relocated, or removed with all ease levied against an
;; implementation.
;; 
;; 
;; Data Type
;; =========
;; Random wists of a single type only: the currency of its code, namely,
;; characters, operating as bivial tokens; imprimis, of course, in the
;; agency occupied by the program source's communication. In a second,
;; but in no mete parhedral, avenue, their rearrangement in order to
;; realize the string rewriting capabilities enumerates their particular
;; potence.
;; 
;; 
;; Syntax
;; ======
;; The syntactical aspect of Random ostends a liberal admission of any
;; character that adumbrates a quintuple tokens' particular dedication
;; to special causatum, whereas all further entities reply in siclike
;; manner.
;; 
;; == COMMENTS ==
;; An ultimity begotten by all characters' amenability to an operational
;; wike, no provision for comments are incorporated into the current
;; language iteration.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) description shall
;; elucidate the syntaxis in greater detail:
;; 
;;   program          := { command } ;
;;   command          := moveToBack
;;                    |  moveToFront
;;                    |  output
;;                    |  jumpForward
;;                    |  jumpBack
;;                    |  copyCharToBack
;;                    ;
;;   output           := "><" ;
;;   moveToBack       := ">" ;
;;   moveToFront      := "<" ;
;;   jumpForward      := "[" ;
;;   jumpBack         := "]" ;
;;   copyCharToBack   := character - commandCharacter ;
;;   commandCharacter := "<" | ">" | "[" | "]" ;
;; 
;; 
;; Instructions
;; ============
;; Random's instruction set actually amplects all communicable
;; characters, forecause, in addition to the dedicated command tokens
;; for relocating the source code items, removing and printing them, and
;; the forward and backward jump facilities, any remaining content
;; incites the append of itself to the code's rear position.
;; 
;; == OVERVIEW ==
;; An apercu shall now administer a requisite mete of nortelry
;; concerning Random's operational aspect.
;; 
;; Please heed that placeholder sections are underlined with asterisks
;; ("*"), thus rendered as in dearth of supersession by actual code,
;; while all surrounding content must be assumed ipsissima verba.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   <       | Removes (dequeues) an element from the back of the code,
;;           | and pushes it to the front of the code.
;;   ..................................................................
;;   >       | Removes (pops) an element from the front of the code,
;;           | and pushes it onto the back of the code.
;;   ..................................................................
;;   ><      | Removes (dequeues) an element from the back of the code,
;;           | and prints it.
;;   ..................................................................
;;   [       | If the front and the back element are equal, jump
;;           | forward to the matching "]". Otherwise proceed as usual.
;;           | If no matching "]" exists, the program is terminated
;;           | with immediacy.
;;           | A twain of collaborating "[" and "]" commands emulate a
;;           | loop of the type
;;           | 
;;           |   while frontElement != backElement do
;;           |     ...
;;           |   end while
;;   ..................................................................
;;   ]       | If the front and the back element are different, jump
;;           | back to the matching "[". Otherwise proceed as usual.
;;           | If no matching "[" exists, the program is terminated
;;           | with immediacy.
;;           | A twain of collaborating "[" and "]" commands emulate a
;;           | loop of the type
;;           | 
;;           |   while frontElement != backElement do
;;           |     ...
;;           |   end while
;;   ..................................................................
;;   c       | Any non-command character {c} is copied, without its
;;   *       | original's removal, to the rear of the code.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Random's exhibition in terms of paragraphs and examples conditions a
;; rather eath apprehension of its elements; natheless, a few inroads of
;; incertitude's presence vindicate further disquisitions.
;; 
;; == WHAT SHALL A "DOUBLE-ENDED STACK" DENOTE? ==
;; The Random language's author relates to the underlying basis as a
;; "double-ended stack" --- a term extant, yet daimen to be employed or
;; defined.
;; 
;; The term's eisegesis begets at least two conjectures:
;; 
;;   (1) The term refers to a twain of stacks operating in champarty as
;;       close compernage.
;;   (2) The term is tantamount to a poecilonym for the double-ended
;;       queue (deque).
;; 
;; It has been adjudged to attend to the first (1) interpretation, as
;; the protolog relates of the language's potence to simulate two
;; stacks' coefficacy. The implementation, however, proceeds by an
;; equipollent data structure, the deque, as a compatible program
;; conformation.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp is founded upon the molding
;; of the Random source code into a double-ended queue (deque), the
;; elements of which constitute characters only.
;; 
;; == DEQUE: A DOUBLY LINKED LIST ==
;; The deque implementation adheres closely to the solution adduced by
;; Michael T. Goodrich et al. [goodrich2014datastructure6th] for the
;; doubly linked list, whose concinnity with the intended purpose
;; homologates its appropriation (see [goodrich2014datastructure6th],
;; page 250).
;; 
;; == THE REQUIREMENTS BEGET THE DATA STRUCTURES ==
;; The deque's vindication ensues from the consectaries having their
;; woning accommodated in the Random language's requirements. A
;; corroboration of these sensibility shall be derived from the
;; vinculums between the language facilities and the underlying code
;; model's expected deportment in response to the same.
;; 
;; The sinistral column ("Task") lists the general abilities levied
;; against the program's architectural design, while the dextral
;; compernage ("Data str.") juxtaposes the data structures sufficiently
;; competent for their chevisance.
;; 
;;   ------------------------------------------------------------------
;;   Task                                                | Data str.
;;   ----------------------------------------------------+-------------
;;   The current element must be copied to the end of    | queue
;;   the code.                                           | 
;;   ..................................................................
;;   The element at the rear of the code must be moved   | stack, deque
;;   (removed) and inserted at its front (pushed).       | 
;;   ..................................................................
;;   The element at the front of the code must be moved  | queue, stack
;;   (removed) and inserted at its rear (enqueued).      | 
;;   ..................................................................
;;   The element at the rear of the code must be removed | deque
;;   for printing.                                       | 
;;   ..................................................................
;;   All elements must be traversed from the current     | list
;;   location to the rear in order to find a jump end    | 
;;   point.                                              | 
;;   ..................................................................
;;   All elements must be traversed from the current     | list
;;   location to the front in order to find a jump start | 
;;   point.                                              | 
;;   ------------------------------------------------------------------
;; 
;; The tabular exposition very patently limns a picture of the deque's
;; congruence with all expectations, namely that of the queue, stack,
;; and the very liberally endowed list.
;; 
;; == RANDOM'S DEQUE: AN ENHANCED SPECIES ==
;; Maugre its intended lealty to the authors' principles, both in
;; nominal and conceptual notions, several accommodations have been
;; adhibited in a pursuit to align the deque interface with both the
;; Random language's kenspeckle requisitums and Common Lisp's
;; consuetudes:
;; 
;;   (1) Counterdistinguished from the expected deque interface, which
;;       trades in elements for return values, this implementation
;;       preponderantly responds with direct node references, so as to
;;       capacitate their indagation during a Random program's
;;       execution.
;;   (2) Operations advenient to the slim deque interface design have
;;       been imported in an effort to attend to Random's necessities,
;;       such as the relocation of a front node to the rear, or the
;;       athwart modification.
;;   (3) The deque interface and extended operations follow the rules of
;;       agnomination acquainted to the Common Lisp realm, that is, in
;;       particular, the connection of separated words in a name via
;;       hyphens ("-"), and a prefixing of operation identifiers with
;;       the associated class, such as "deque-" and "dlnode-".
;; 
;; A twain of listings shall now be produced, the first educating about
;; the deque's standard interface operations, the second administering
;; gnarity concerning the forinsecal members attending to Random's
;; dioristic attributes.
;; 
;; The deque interface standard mandates the following specimens:
;; 
;;   ------------------------------------------------------------------
;;   Standard operation | Effect
;;   -------------------+----------------------------------------------
;;   deque-size         | Determines the number of elements.
;;   ..................................................................
;;   deque-empty-p      | Determines whether the deque is empty.
;;   ..................................................................
;;   deque-first        | Returns the front node.
;;   ..................................................................
;;   deque-last         | Returns the rear node.
;;   ..................................................................
;;   deque-add-first    | Inserts an element at the front.
;;   ..................................................................
;;   deque-add-last     | Inserts an element at the rear.
;;   ..................................................................
;;   deque-remove-first | Removes and returns the front node.
;;   ..................................................................
;;   deque-remove-last  | Removes and returns the rear node.
;;   ------------------------------------------------------------------
;; 
;; The scions of Random's influence embrace these additions:
;; 
;;   ------------------------------------------------------------------
;;   Advenient operation      | Effect
;;   -------------------------+----------------------------------------
;;   deque-node-after         | Returns a node's successor, or ``NIL''
;;                            | if none such exists.
;;   ..................................................................
;;   deque-node-before        | Returns a node's predecessor, or
;;                            | ``NIL'' if none such exists.
;;   ..................................................................
;;   deque-move-first-to-last | Moves the front element to the rear.
;;   ..................................................................
;;   deque-move-last-to-first | Moves the rear element to the front.
;;   ..................................................................
;;   deque-ends-equal-p       | Determines whether the front and rear
;;                            | elements (characters) are equal.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-03
;; 
;; Sources:
;;   [esolang2016Random]
;;   The Esolang contributors, "Random", May 2nd, 2016
;;   URL: "https://esolangs.org/wiki/Random"
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures and Algorithms in Java", sixth edition, 2014
;;   Notes:
;;     - Pages 135--137: Implementation of a doubly linked list.
;;     - Pages 248--251: Description of the double-ended queue (deque);
;;                       reference to doubly linked list.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "DLNode".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (DLNode
  (:constructor make-dlnode (element previous next)))
  "The ``DLNode'' class implements a doubly linked node, fit for being
   naited in a doubly linked list realization of the deque data
   structure."
  (element  NIL :type (or null character))
  (previous NIL :type (or null DLNode))
  (next     NIL :type (or null DLNode)))

;;; -------------------------------------------------------

(defmethod print-object ((node DLNode) stream)
  (declare (type DLNode      node))
  (declare (type destination stream))
  (format stream "(DLNode ~s)"
    (dlnode-element node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Deque".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Deque ()
  ((header
    :initarg       :header
    :initform      NIL
    :accessor      deque-header
    :type          (or null DLNode)
    :documentation "The header sentinel.
                    ---
                    The header precedes all other nodes, but does not
                    contribute to the deque's size.")
   (trailer
    :initarg       :trailer
    :initform      NIL
    :accessor      deque-trailer
    :type          (or null DLNode)
    :documentation "The trailer sentinel.
                    ---
                    The trailer succeeds all other nodes, but does not
                    contribute to the deque's size.")
   (size
    :initarg       :size
    :initform      0
    :type          (integer 0 *)
    :documentation "The number of elements in the deque.
                    ---
                    The HEADER and TRAILER, carrying no elements, do not
                    contribute to this tally."))
  (:documentation
    "The ``Deque'' class represents a double-ended queue, or abbreviated
     \"deque\", for storing character elements, founded upon a doubly
     linked list of nodes.
     ---
     Accommodated to the Random programming language's haecceity, a vast
     preponderance among the deque's operations return direct references
     to the employed nodes, these being instances of the ``DLNode''
     class."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((deque Deque) &key)
  "Defines the DEQUE's header and trailer nodes, connects them, and
   returns the modified DEQUE."
  (declare (type Deque deque))
  (with-slots (header trailer) deque
    (declare (type (or null DLNode) header))
    (declare (type (or null DLNode) trailer))
    (setf header  (make-dlnode NIL NIL    NIL))
    (setf trailer (make-dlnode NIL header NIL))
    (setf (dlnode-next header) trailer))
  (the Deque deque))

;;; -------------------------------------------------------

(defun make-deque ()
  "Creates and returns an empty ``Deque''."
  (the Deque
    (make-instance 'Deque)))

;;; -------------------------------------------------------

(defun deque-size (deque)
  "Returns the number of elements stored in the DEQUE."
  (declare (type Deque deque))
  (the (integer 0 *)
    (slot-value deque 'size)))

;;; -------------------------------------------------------

(defun deque-empty-p (deque)
  "Determines whether the DEQUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Deque deque))
  (the boolean
    (not (null
      (zerop (deque-size deque))))))

;;; -------------------------------------------------------

(defun deque-first (deque)
  "Returns the node at the DEQUE's front, or ``NIL'' if the same is
   empty."
  (declare (type Deque deque))
  (the (or null DLNode)
    (unless (deque-empty-p deque)
      (dlnode-next
        (deque-header deque)))))

;;; -------------------------------------------------------

(defun deque-last (deque)
  "Returns the node at the DEQUE's rear, or ``NIL'' if the same is
   empty."
  (declare (type Deque deque))
  (the (or null DLNode)
    (unless (deque-empty-p deque)
      (dlnode-previous
        (deque-trailer deque)))))

;;; -------------------------------------------------------

(defun deque-add-between (deque new-element predecessor successor)
  "Creates a new node containing the NEW-ELEMENT betwixt the PREDECESSOR
   and the SUCCESSOR nodes, inserts it into the DEQUE, and returns the
   thus produced node."
  (declare (type Deque     deque))
  (declare (type character new-element))
  (declare (type DLNode    predecessor))
  (declare (type DLNode    successor))
  (let ((new-node (make-dlnode new-element predecessor successor)))
    (declare (type DLNode new-node))
    (setf (dlnode-next     predecessor) new-node)
    (setf (dlnode-previous successor)   new-node)
    (incf (slot-value deque 'size))
    (the DLNode new-node)))

;;; -------------------------------------------------------

(defun deque-add-first (deque new-element)
  "Creates a new node storing the NEW-ELEMENT, inserts it at the DEQUE's
   front, and returns the thus produced node."
  (declare (type Deque     deque))
  (declare (type character new-element))
  (the DLNode
    (deque-add-between deque new-element
      (deque-header deque)
      (dlnode-next (deque-header deque)))))

;;; -------------------------------------------------------

(defun deque-add-last (deque new-element)
  "Creates a new node storing the NEW-ELEMENT, inserts it at the DEQUE's
   rear, and returns the thus produced node."
  (declare (type Deque     deque))
  (declare (type character new-element))
  (the DLNode
    (deque-add-between deque new-element
      (dlnode-previous (deque-trailer deque))
      (deque-trailer deque))))

;;; -------------------------------------------------------

(defun deque-remove-node (deque node)
  "Removes and returns the NODE from the DEQUE."
  (declare (type Deque  deque))
  (declare (type DLNode node))
  (let ((predecessor (dlnode-previous node))
        (successor   (dlnode-next     node)))
    (declare (type DLNode predecessor))
    (declare (type DLNode successor))
    (setf (dlnode-next     predecessor) successor)
    (setf (dlnode-previous successor)   predecessor)
    (decf (slot-value deque 'size))
    (the DLNode node)))

;;; -------------------------------------------------------

(defun deque-remove-first (deque)
  "Removes and returns the node at the DEQUE's front, or responds with
   ``NIL'' if the same is empty."
  (declare (type Deque deque))
  (the (or null DLNode)
    (unless (deque-empty-p deque)
      (deque-remove-node deque
        (dlnode-next
          (deque-header deque))))))

;;; -------------------------------------------------------

(defun deque-remove-last (deque)
  "Removes and returns the node at the DEQUE's rear, or responds with
   ``NIL'' if the same is empty."
  (declare (type Deque deque))
  (the (or null DLNode)
    (unless (deque-empty-p deque)
      (deque-remove-node deque
        (dlnode-previous
          (deque-trailer deque))))))

;;; -------------------------------------------------------

(defun deque-move-first-to-last (deque)
  "Moves the node at the DEQUE's front to the rear and returns no
   value.
   ---
   This operation does not belong to the foundational members of the
   deque abstract data structure."
  (declare (type Deque deque))
  (deque-add-last deque
    (dlnode-element
      (deque-remove-first deque)))
  (values))

;;; -------------------------------------------------------

(defun deque-move-last-to-first (deque)
  "Moves the node at the DEQUE's rear to the front and returns no
   value.
   ---
   This operation does not belong to the foundational members of the
   deque abstract data structure."
  (declare (type Deque deque))
  (deque-add-first deque
    (dlnode-element
      (deque-remove-last deque)))
  (values))

;;; -------------------------------------------------------

(defun deque-ends-equal-p (deque)
  "Determines whether the DEQUE's front and rear elements are equal,
   returning on confirmation a ``boolean'' value of ``T'', otherwise ---
   including for an empty DEQUE --- ``NIL''.
   ---
   This operation does not belong to the foundational members of the
   deque abstract data structure."
  (declare (type Deque deque))
  (unless (deque-empty-p deque)
    (let ((first-node (deque-first deque))
          (last-node  (deque-last  deque)))
      (declare (type DLNode first-node))
      (declare (type DLNode last-node))
      (the boolean
        (char= (dlnode-element first-node)
               (dlnode-element last-node))))))

;;; -------------------------------------------------------

(defun deque-node-after (deque node)
  "Returns the successor of the NODE in the DEQUE, or ``NIL'' if the
   same is located at the rear.
   ---
   This operation does not belong to the foundational members of the
   deque abstract data structure."
  (declare (type Deque  deque))
  (declare (type DLNode node))
  (let ((successor (dlnode-next node)))
    (declare (type (or null DLNode) successor))
    (the (or null DLNode)
      (when (and successor
                 (not (eq successor (deque-trailer deque))))
        successor))))

;;; -------------------------------------------------------

(defun deque-node-before (deque node)
  "Returns the predecessor of the NODE in the DEQUE, or ``NIL'' if the
   same is located at the front.
   ---
   This operation does not belong to the foundational members of the
   deque abstract data structure."
  (declare (type Deque  deque))
  (declare (type DLNode node))
  (let ((predecessor (dlnode-previous node)))
    (declare (type (or null DLNode) predecessor))
    (the (or null DLNode)
      (when (and predecessor
                 (not (eq predecessor (deque-header deque))))
        predecessor))))

;;; -------------------------------------------------------

(defmethod print-object ((deque Deque) stream)
  (declare (type Deque       deque))
  (declare (type destination stream))
  (format stream "(Deque")
  (loop
    for current-node
      of-type DLNode
      =       (dlnode-next (deque-header deque))
      then    (dlnode-next current-node)
    until (eq current-node (deque-trailer deque))
    do
      (format stream " ~s"
        (dlnode-element current-node)))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-Random-code (code)
  "Transforms the piece of Random source CODE from its string form into
   a deque representation and returns the result."
  (declare (type string code))
  (let ((deque (make-deque)))
    (declare (type Deque deque))
    (loop for token of-type character across code do
      (deque-add-last deque token))
    (the Deque deque)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-jump-end-point (deque start)
  "Proceeding from the START node's successor in the DEQUE, searches
   forward for the matching jump end point (\"]\"), upon success
   returning the comprehending node; otherwise, if no such match can be
   detected, responds with ``NIL''."
  (declare (type Deque  deque))
  (declare (type DLNode start))
  (the (or null DLNode)
    (loop
      with level of-type fixnum = 0
      
      for current-node
        of-type (or null DLNode)
        =       (deque-node-after deque start)
        then    (deque-node-after deque current-node)
      
      do
        (cond
          ((null current-node)
            (return NIL))
          
          ((char= (dlnode-element current-node) #\])
            (if (zerop level)
              (return current-node)
              (decf level)))
          
          ((char= (dlnode-element current-node) #\[)
            (incf level))
          
          (T
            NIL)))))

;;; -------------------------------------------------------

(defun find-jump-start-point (deque end)
  "Proceeding from the END node's predecessor in the DEQUE, searches
   backward for the matching jump start point (\"[\"), upon success
   returning the comprehending node; otherwise, if no such match can be
   detected, responds with ``NIL''."
  (declare (type Deque  deque))
  (declare (type DLNode end))
  (the (or null DLNode)
    (loop
      with level of-type fixnum = 0
      
      for current-node
        of-type (or null DLNode)
        =       (deque-node-before deque end)
        then    (deque-node-before deque current-node)
      
      do
        (cond
          ((null current-node)
            (return NIL))
          
          ((char= (dlnode-element current-node) #\[)
            (if (zerop level)
              (return current-node)
              (decf level)))
          
          ((char= (dlnode-element current-node) #\])
            (incf level))
          
          (T
            NIL)))))

;;; -------------------------------------------------------

(defun matches-token-p (deque start expected-characters)
  "Determines whether, proceeding from the START node in the DEQUE, the
   characters stored in the accolent nodes replicate the
   EXPECTED-CHARACTERS, returning on confirmation a ``boolean'' value of
   ``T'', otherwise responding with ``NIL''."
  (declare (type Deque  deque))
  (declare (type DLNode start))
  (declare (type string expected-characters))
  (the boolean
    (loop
      for expected-character
        of-type character
        across  expected-characters
      for current-node
        of-type (or null DLNode)
        =       start
        then    (deque-node-after deque current-node)
      always
        (and current-node
             (char= (dlnode-element current-node)
                    expected-character)))))

;;; -------------------------------------------------------

(defun process-code-deque (deque)
  "Processes the DEQUE representing a Random program and returns no
   value."
  (declare (type Deque deque))
  (let ((current-node (deque-first deque)))
    (declare (type (or null DLNode) current-node))
    (flet ((advance ()
            "Moves the CURRENT-NODE to its successor node, updates the
             CURRENT-NODE, and returns no value."
            (setf current-node
              (deque-node-after deque current-node))
            (values)))
      
      (loop while current-node do
        (cond
          ;; Dequeue from rear and print.
          ((matches-token-p deque current-node "><")
            (let ((last-character (deque-remove-last deque)))
              (declare (type DLNode last-character))
              (format T "~c"
                (dlnode-element last-character)))
            (advance)
            (advance))
          
          ;; Move last element to front.
          ((char= (dlnode-element current-node) #\<)
            (deque-move-last-to-first deque)
            (advance))
          
          ;; Move first element to rear.
          ((char= (dlnode-element current-node) #\>)
            (deque-move-first-to-last deque)
            (advance))
          
          ;; Loop while front and rear elements differ.
          ;; => Jump forward to "]" (or end of program) if front = rear.
          ((char= (dlnode-element current-node) #\[)
            (if (deque-ends-equal-p deque)
              (let ((end-point
                      (find-jump-end-point deque current-node)))
                (declare (type (or null DLNode) end-point))
                (if end-point
                  (setf current-node end-point)
                  (loop-finish)))
              (advance)))
          
          ;; Loop while front and rear elements differ.
          ;; => Jump back to "[" (or end of program) if front = rear.
          ((char= (dlnode-element current-node) #\])
            (if (deque-ends-equal-p deque)
              (advance)
              (let ((start-point
                      (find-jump-start-point deque current-node)))
                (declare (type (or null DLNode) start-point))
                (if start-point
                  (setf current-node start-point)
                  (loop-finish)))))
          
          ;; Append the current character to the rear.
          (T
            (deque-add-last deque
              (dlnode-element current-node))
            (advance))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Random (code)
  "Interprets the piece of Random source CODE and returns no value."
  (declare (type string code))
  (process-code-deque
    (parse-Random-code code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Random "!dlroW ,olleH[><]><")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-Random "!dlroW ,olleH><><><><><><><><><><><><><")

;;; -------------------------------------------------------

;; Countdown from 9 to 0.
(interpret-Random "0123456789[><]><")
