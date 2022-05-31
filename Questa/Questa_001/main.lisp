;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; The "Questa" constitutes an esoteric data structure, invented by the
;; Esolang user "ChuckEsoteric08", and kenspeckle in its peculiar
;; combination of the queue and stack capabilities.
;; 
;; Concept
;; =======
;; The Questa, whose agnomination originates as a portmanteau from its
;; twofold heritage, the QUEue and STAck, is defined as an esoteric
;; data structure, combining in its nature select portions borrowed from
;; its parentage's attributes. While able only to receive new elements
;; at its front, like the stack, both the removal at the same laterality
;; as well as at its rear, in the latter following the queue principle,
;; are admitted their homologation. In addition, a novel faculty permits
;; the exchange of the front and bottom items.
;; 
;; == CONTEXT ==
;; A comprehension regarding the Questa is facilitated by the gnarity of
;; its kindred, which appertains to its heritage from the stack and
;; queue inspiration, and, to some degree, a juxtaposition to the
;; conceptually kindred deque.
;; 
;; == STACK ==
;; The abstract data type stack describes a last-in first-out (LIFO)
;; type of storage, receiving at its top elements and removing these at
;; the same location. As a corollary, the last item inserted will be the
;; first item removed, with the oldest object reserved as the final
;; output.
;; 
;; Two foundational operations are defined on the stack:
;;   
;;   push(x)
;;     Inserts the element {x} at the topmost position of the stack.
;;   
;;   pop()
;;     Removes and returns the topmost element of the stack.
;;     An error occurs if this operation is applied to an empty stack.
;; 
;; == QUEUE ==
;; The diorism of the queue abstract data type entails a first-in
;; first-out (FIFO) storage type, receiving at its rear new elements,
;; while removing items from the front. The operation set includes two
;; members:
;;   
;;   enqueue(x)
;;     Inserts the element {x} at the rear position of the queue.
;;   
;;   dequeue()
;;     Removes and returns the element at the front of the queue.
;;     An error occurs if this operation is applied to an empty queue.
;; 
;; == DEQUE ==
;; Developed as a combination of these two emolumental storage types,
;; the deque, or Double-Ended QUEue, unites stack and queue operations
;; into a new entity. Yet, developed beyond an amalgam, the deque's
;; potentials elevate it to a more precious rank than a mere union of
;; its parental capabilities, introducing further operations. This
;; concrete augmentation entails, among other aspects, an adscititious,
;; while particularly utile, facility frequently adhibited to both
;; parents without comprising an aspect of their haecceity: the
;; inquisition without concomitant deletion at one end, also known as
;; "peeking".
;; 
;; The following overview enumerates the official deque operations.
;; Please note that the terminology varies betwixt sources.
;;   
;;   Operation   | Donor  | Effect
;;   ------------+--------+--------------------------------------------
;;   addFirst    | stack  | Inserts an element at the front.
;;               |        | Corresponds to the stack operation PUSH.
;;   ..................................................................
;;   addLast     | queue  | Inserts an element at the rear.
;;               |        | Corresponds to the queue operation ENQUEUE.
;;   ..................................................................
;;   getFirst    | stack, | Returns without removing the element at
;;               | queue  | the front.
;;               |        | Corresponds to the optional stack and queue
;;               |        | operations PEEK.
;;   ..................................................................
;;   getLast     | none   | Returns without removing the element at the
;;               |        | rear.
;;               |        | Is an original operation.
;;   ..................................................................
;;   removeFirst | stack, | Removes and returns the element at the
;;               | queue  | front.
;;               |        | Corresponds to the stack operation POP and
;;               |        | the queue operation DEQUEUE.
;;   ..................................................................
;;   removeLast  | none   | Removes and returns the element at the
;;               |        | rear.
;;               |        | Is an original operation.
;; 
;; Oftentimes, further functionality, such as the checking for the
;; collection's size or its vacancy, exists as an optional inclusion,
;; but shall not be a digressing object of our disquisitions.
;; 
;; 
;; Operations
;; ==========
;; The Questa interface, as established by most data structures' wont,
;; subscribes to a thoroughly minimalistic circumference, impelled by
;; requisites rather than comprehensiveness or exhaustion. Four
;; operations are defined, three of which legatees to the stack and
;; queue cleronomy, and a single member eluding the donorship.
;; 
;; == OVERVIEW ==
;; The following tabular apercu shall describe the four basic Questa
;; operations, in juxtaposition to the inspiring data structure.
;;   
;;   Operation | Donor   | Description
;;   ----------+---------+---------------------------------------------
;;   PUSH      | stack   | Inserts an element at the front and returns
;;             |         | no value.
;;   ..................................................................
;;   POP1      | stack   | Removes and returns the element at the
;;             |         | front.
;;   ..................................................................
;;   POP2      | deque   | Removes and returns the element at the rear.
;;   ..................................................................
;;   SWAP      | none    | Swaps the front and rear elements and returns
;;             |         | a Boolean value which resolves to true upon
;;             |         | the operation's success, otherwise to false.
;; 
;; == PUSH ==
;; Inserts an element at the front of the Questa, preceding any extant
;; items.
;; 
;; Signature:
;;   PUSH
;; 
;; Interface:
;;   PUSH (element : any) : void
;; 
;; Description:
;;   The {element} will be inserted at the first position of the Questa,
;;   preceding any previous content, if extant, and returns no value.
;; 
;; Side effects:
;;   - A new element will be inserted into the Questa.
;; 
;; Exceptional situations:
;;   - Implementation-dependent and non-canonical, if the Questa imposes
;;     a maximum capacity, an error of the type "FullQuesta" is
;;     signaled.
;;   - Implementation-dependent and non-canonical, if the Questa imposes
;;     type constraints, and the {element} does not conform to the
;;     required type, an error of the type "InvalidElement" is signaled.
;; 
;; Notes:
;;   - This operation concords with the stack data structure's PUSH and
;;     the deque's ADD_FIRST facilities.
;; 
;; == POP1 ==
;; Removes and returns the first or top element in the Questa.
;; 
;; Signature:
;;   POP1
;; 
;; Interface:
;;   POP1 () : any
;; 
;; Description:
;;   Removes the element at the front of the Questa and returns it. If
;;   applied to a singleton Questa, the storage will subsequently be
;;   empty. If applied to an already empty Questa, an error will be
;;   signaled.
;; 
;; Side effects:
;;   - The Questa will be reduced by the removed element.
;;   - A singleton Questa will become empty.
;; 
;; Exceptional situations:
;;   - Upon attempting to remove an element from an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation concords with the stack data structure's POP, the
;;     queue's DEQUEUE, and the deque's REMOVE_FIRST facilities.
;; 
;; == POP2 ==
;; Removes and returns the last or bottom element in the Questa.
;; 
;; Signature:
;;   POP2
;; 
;; Interface:
;;   POP2 () : any
;; 
;; Description:
;;   Removes the element at the rear of the Questa and returns it. If
;;   applied to a singleton Questa, the storage will subsequently be
;;   empty. If applied to an already empty Questa, an error will be
;;   signaled.
;; 
;; Side effects:
;;   - The Questa will be reduced by the removed element.
;;   - A singleton Questa will become empty.
;; 
;; Exceptional situations:
;;   - Upon attempting to remove an element from an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation concords with the deque's REMOVE_LAST facilty.
;; 
;; == SWAP ==
;; Changes the position of the first and the last element in the Questa.
;; 
;; Signature:
;;   SWAP
;; 
;; Interface:
;;   SWAP () : boolean
;; 
;; Description:
;;   If not empty, the first and last element of the Questa exchange
;;   their positions, finally a Boolean return value of true will be
;;   produced. In the case of a singleton Questa no effectual
;;   modifications apply, however, returning a Boolean value of true.
;;   Applied to an empty storage, no manipulation is exercised, and the
;;   operation returns a Boolean false indicator.
;; 
;; Side effects:
;;   - The first and last element will be exchanged.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; Notes:
;;   - Among the quadruple this operation establishes the sole original
;;     member, not according to the queue and stack cleronomy.
;; 
;; 
;; Questa-Completeness
;; ===================
;; Being a device for the practical realization of programs, the
;; presence and incorporation of the Questa serves to signify a
;; particular characteristic of a reliant environment. A programming
;; language is called "Questa-complete" if three criteria are satisfied:
;;   
;;   (1) The language utilizes the Questa data structure.
;;       While the Questa data structure's employment constitutes a
;;       prerequisite to Questa-completeness, its concrete mete of
;;       integration does not bear any weight in the qualification.
;;       Whether this collection appropriates an integral or an
;;       incidental role eludes the accreditation.
;;   
;;   (2) The data storage is unbounded.
;;       An at least theoretical amenability to an arbitrary tally of
;;       elements in reception must be avered. Practicability, such as
;;       the impositions of the system resources, may, of course,
;;       infringe on the veridical reification without corrupting the
;;       notional integrity.
;;   
;;   (3) All four basic commands are integrated.
;;       The language must accommodate facilities to reference the
;;       quadruple command set composed of
;;         - PUSH
;;         - POP1
;;         - POP2
;;         - SWAP.
;;       The inclusion must perforce obey to the specified
;;       characteristics with respect to inputs, outputs, and side
;;       effects; a nominal reproduction, that is, a mandating of these
;;       exact operation names, does not pertain to the circumference.
;;       Adscititious operations, in addition to this nucleus, may be
;;       included and do not inflict a disqualification in this regard.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The exactitude frequently yielded by the fermating influence of
;; senescence yet eloigned from its fairly recent geniture, some
;; dubiosity incurs upon certain of the Questa's aspects, a select of
;; thereof shall be discussed in the following.
;; 
;; == HOW DO QUESTA OPERATIONS REACT UPON VACANCY? ==
;; The content or wantage relating to a Questa participates in the
;; nature of three of its commands: POP1, POP2, and SWAP. The original
;; specification does not indulge too meticulously in a treatise
;; concerning the emptiness' effect on such an attempted modification.
;; Two competing modes may apply in these circumstances:
;;   
;;   (a) An error is signaled.
;;   (b) A sentinel value is returned.
;;   (c) No information is delivered.
;; 
;; The desinent proposal (c) would be peccant in its refusal to convey
;; significant intelligence to the programmer or user with respect to
;; the data management's state. In spite of the second choice's (b)
;; proffered convenience, a facilitated distinguishment in such
;; exceptional cases, as offered by an error signaling (a), constitutes
;; a more professional option.
;; 
;; 
;; Implementation
;; ==============
;; The realization chosen in this program relies on the singly linked
;; list structure connate to Common Lisp, alleviating the inefficiency
;; inherent to insertions at the rear by adminicle of a tail pointer.
;; 
;; == LISP'S LINKED LIST PROVIDES A STACK ==
;; Common Lisp's foundational data structure, the list, which
;; simultaneously partakes of an agency as its programs'
;; representation --- a fact known as "homoiconicity" ---, redes the
;; same as a natural solution to the implementation of the Questa. The
;; Common Lisp structure constitutes a singly linked list, compact of
;; nodes called "cons cells" or simply "conses"; each such unit stores
;; beside its datum a reference to its successor cons, with the ultimate
;; one pointing to the ``NIL'' object, so as to designate the list's
;; conclusion.
;; 
;; == LISTS ARE EFFICIENT STACKS, BUT INEFFICIENT QUEUES ==
;; The innate efficiency at manipulating and querying the list's first
;; element, or "head", permits its employment as a stack. Concomitantly,
;; however, operations on the list's rear, maugre their existence, are
;; encumbered with deficiencies in the same regard. Indagations and, in
;; particular, insertions at the tail conflict with expectencies levied
;; at queues and portions of the deque characteristics.
;; 
;; == TAIL-CONSING: EFFICIENT QUEUE CHARACTERISTICS FOR LISTS ==
;; An idee recue, justified satisfactorily in its advocacy, relies on
;; tail-consing as an augmentation to the bare list handling in order to
;; access and modify the desinent node. A list's ultimate cons is
;; maintained separately, acting as a reference, not a copy, of the list
;; tail. Examinations into and manipulations of this cons thus
;; reverberate verbatim and with immediate effect in the ensconcing data
;; structure. If we, for instance, create a new cons and set it as the
;; tail-cons' successor, the appertaining list automatically receives
;; the additional appendage. Likewise, querying the object stored in the
;; tail-cons delivers the list's tail item. Operations applying to the
;; list head retain their independence from this solution.
;; 
;; A pertinent fact in the context of tail-consing, a tail-pointer can
;; only refer to a non-empty list, as the ``NIL'' object, perfectly
;; identical to the vacant list structure, lacks any conses to be
;; referrable. As a corollary, an "empty" list relying on tail-consing
;; must provide at least one element at the time of the tail-pointer's
;; administration. Commonly, a "dummy node" or "dummy cons" may be
;; utilized in order to ascertain that, at any instant, the tail-pointer
;; avers validity. A cons of this ilk may contain any object, the same
;; is neglected and excluded during operations, but constantly retained
;; for this exact purpose. 
;; 
;; A principal realization of a list, structured in a suitable manner
;; for the use of tail-consing, is exposed in the following Common Lisp
;; code snippet:
;;   
;;   (let ((my-list (cons 'head NIL)))
;;     (declare (type list my-list))
;;     
;;     (let ((tail-pointer my-list))
;;       (declare (type (or cons list) tail-pointer))
;;       
;;       ...))
;; 
;; == TAIL-CONSING IN THE QUESTA ==
;; The notions inherent to tail-consing as applied to the Questa shall
;; be subjected to further elucidations in the following pseudocode
;; form:
;;   
;;   procedure Questa.initialize (questa : Questa)
;;     questa.elements <- ("head" . NIL)
;;     questa.tail     <- NIL
;;   end procedure
;;   
;;   function Questa.push (questa : Questa, newElement : any)
;;     if questa.isEmpty () then
;;       { The "newElement" must be stored into a cons for insertion. }
;;       let newTailCons <- (newElement . NIL)
;;       make newTailCons the successor node of questa.tail
;;       { Make "newTailCons" the new "questa.tail". }
;;       questa.tail <- successor node of questa.tail
;;     else
;;       prepend newElement to questa.elements
;;       { Make the dummy node the first cons again. }
;;       swap first and second element of questa.elements
;;     end if
;;     
;;     return true
;;   end function
;; 
;; 
;; Appendix A: Extension by Additional Operations
;; ==============================================
;; Despite the virtue commorant in the Questa's modestly invested
;; standard interface, pragmatism, entailed and exemplified in the two
;; incipient programming languages based upon this data structure,
;; "Quests" and "Miw", redes the fixation of further operations.
;; 
;; The following considerations apply with respect to the proposed
;; extensions:
;;   - Both "Quests" and "Miw" require a modification of the top and
;;     bottom elements.
;;   - The Questa's vacancy, as explicated and attested in the standard
;;     operations' treatise, nearly consistently designates a special
;;     case. If the same embraces a critical moment, signified by an
;;     error elicitation, the current Questa state is esteemed as an
;;     important piece of intelligence for consideration ere the fatal
;;     operation proceeds to its invocation.
;;   - Similar dations regarding the capabilities are applied to several
;;     abstract data structures as well, for instance, the "size()" and
;;     "isEmpty()" accessors for the stack, queue, and dequeue, albeit
;;     not mandated by the object's ultimate haecceity.
;;   - A lack of symmetry inflicts upon the potential of inquisitions
;;     into the Questa's top or bottom element a predicament. Forecause
;;     no operations exist to return either end element without its
;;     removal, the only standard method for such a task would include
;;     the deletion and subsequent insertion of the desiderated item at
;;     the previous position in the Questa. However, this capability
;;     merely exists for the top location, as the PUSH operation adds an
;;     object to the front, responding to POP1 which removes the same;
;;     no analogous insertion solution is reserved for the POP2
;;     operation, deleting the bottom.
;; 
;; == OVERVIEW ==
;; The following table shall offer a summary concerning the recommended
;; adscititious operations:
;;   
;;   Operation | Donor  | Description
;;   ----------+--------+-----------------------------------------------
;;   PEEK1     | deque  | Returns without removing the first element.
;;   ..................................................................
;;   PEEK2     | deque  | Returns without removing the last element.
;;   ..................................................................
;;   SET1      | none   | Replaces the first element and returns the
;;             |        | dislodged one.
;;   ..................................................................
;;   SET2      | none   | Replaces the last element and returns the
;;             |        | dislodged one.
;;   ..................................................................
;;   CLEAR     | none   | Removes all elements and returns no value.
;;   ..................................................................
;;   SIZE      | stack, | Returns the number of elements.
;;             | queue, |
;;             | deque  |
;;   ..................................................................
;;   IS_EMPTY  | stack, | Checks whether the Questa is empty, returning
;;             | queue, | a Boolean value of true upon vacancy,
;;             | deque  | otherwise false.
;; 
;; == PEEK1 ==
;; Returns the first or top element of the Questa.
;; 
;; Signature:
;;   PEEK1
;; 
;; Interface:
;;   PEEK1 () : any
;; 
;; Description:
;;   Returns without removing the first or top element of the Questa.
;;   If the Questa is empty, an error is signaled.
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - Upon attempting to peek an element from an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation represents a non-standard recommendation.
;;   - If the Questa contains a single element, the effect of this
;;     operation is paregal to that of PEEK2.
;; 
;; == PEEK2 ==
;; Returns the last or bottom element of the Questa.
;; 
;; Signature:
;;   PEEK2
;; 
;; Interface:
;;   PEEK2 () : any
;; 
;; Description:
;;   Returns without removing the last or bottom element of the Questa.
;;   If the Questa is empty, an error is signaled.
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - Upon attempting to peek an element from an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation represents a non-standard recommendation.
;;   - If the Questa contains a single element, the effect of this
;;     operation is paregal to that of PEEK1.
;; 
;; == SET1 ==
;; Replaces the first or top element by a new value.
;; 
;; Signature:
;;   SET1
;; 
;; Interface:
;;   SET1 (newElement : any) : any
;; 
;; Description:
;;   Replaces the first or top element of the Questa by the {newElement}
;;   and returns the replaced value. If the Questa is empty, an error
;;   is signaled.
;; 
;; Side effects:
;;   - The first or top element is replaced.
;; 
;; Exceptional situations:
;;   - Upon attempting to replace an element of an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation represents a non-standard recommendation.
;;   - If the Questa contains a single element, the effect of this
;;     operation is paregal to that of SET2.
;; 
;; == SET2 ==
;; Replaces the last or bottom element by a new value.
;; 
;; Signature:
;;   SET2
;; 
;; Interface:
;;   SET2 (newElement : any) : any
;; 
;; Description:
;;   Replaces the last or bottom element of the Questa by the
;;   {newElement} and returns the replaced value. If the Questa is
;;   empty, an error is signaled.
;; 
;; Side effects:
;;   - The last or bottom element is replaced.
;; 
;; Exceptional situations:
;;   - Upon attempting to replace an element of an empty Questa, an
;;     error of the type "EmptyQuesta" is signaled.
;; 
;; Notes:
;;   - This operation represents a non-standard recommendation.
;;   - If the Questa contains a single element, the effect of this
;;     operation is paregal to that of SET1.
;; 
;; == CLEAR ==
;; Removes all elements.
;; 
;; Signature:
;;   CLEAR ()
;; 
;; Interface:
;;   CLEAR () : void
;; 
;; Description:
;;   Removes all elements from the Questa and returns no value.
;; 
;; Side effects:
;;   - The Questa will be cleared.
;;   - The Questa, if not already vacant, will become empty.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == SIZE ==
;; Returns the number of elements in the Questa.
;; 
;; Signature:
;;   SIZE
;; 
;; Interface:
;;   SIZE () : unsigned_integer
;; 
;; Description:
;;   Returns the number of elements maintained by the Questa as a
;;   non-negative integer value.
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == IS_EMPTY ==
;; Checks whether the Questa is empty.
;; 
;; Signature:
;;   IS_EMPTY
;; 
;; Interface:
;;   IS_EMPTY () : boolean
;; 
;; Description:
;;   Checks whether the Questa is empty, that is, contains exactly zero
;;   elements. Upon confirmation, a Boolean value of true is returned,
;;   otherwise false.
;; 
;; Side effects:
;;   - None.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; 
;; Glossary
;; ========
;; The obtention of an elevated apprehension concerning the Questa and
;; the topics in its adjacency may be attained by an explication of its
;; basic materials and terminology, a service proffered in the current
;; section.
;; 
;; ADT
;;   [Abbreviation] See -> ABSTRACT DATA TYPE.
;; 
;; abstract data type
;;   [Noun] Abbreviated "ADT". A description of a -> DATA STRUCTURE
;;          using a mathematical formulation which comprehends the
;;          expected operations in conjunction with its parameters and
;;          type information. Roughly corresponds to an interface or
;;          protocol deployed in some programming languages.
;; 
;; cons
;;   [Noun] In the Lisp programming language family, an object compact
;;          of two sections, very similar to a tuple, and capable of
;;          storing in any compartment an arbitrary datum. In the
;;          context of the list representation, the cons partakes of a
;;          node's agency, harboring in its first moeity, also known as
;;          the "car", the node data, while its second half, the "cdr",
;;          maintains a pointer to the next cons, or the ``NIL''
;;          sentinel if located at the list tail.
;; 
;; data structure
;;   [Noun] An object or format intended to store data in a manner which
;;          grants efficiency for its members' query and manipulation,
;;          while concomitantly conforming to a particular organization
;;          principle. Its logical representation usually manifests in
;;          an -> ABSTRACT DATA TYPE.
;; 
;; deque
;;   [Noun] A data structure produced by the combination of the -> STACK
;;          and -> QUEUE characteristics, which may insert and remove
;;          elements at both its front and rear.
;; 
;; queue
;;   [Noun] A data structure capable of storing elements at its rear and
;;          removing such from its front.
;; 
;; singleton
;;   [Adjective] (Of a collection) Composed of a single element only.
;;               Thus, for instance, a singleton Questa comprehends one
;;               and only one item.
;; 
;; stack
;;   [Noun] A data structure capable of inserting elements at its front
;;          and removing such at the same location.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-29
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Questa"
;;   -> "https://stackoverflow.com/questions/17731670/common-lisp-first-returns-first-but-last-returns-a-list-of-last-huh"
;;       o Discusses the peculiarity of the ``last'' function which, in
;;         contrast to ``first'', returns a cons, not the element stored
;;         therein.
;;       o Demonstrates how the reference to the last cons, returned by
;;         the function ``last'', may be utilized to append an element to
;;         the end of a list, similar to the technique of tail-consing.
;;         - For this please consult the direct answer
;;           -> "https://stackoverflow.com/a/17732792".
;;   -- Michael T. Goodrich, Roberto Tamassia,
;;      "Data Structures and Algorithms in Java", 4th Edition
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, as a forbisen, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype tail-pointer ()
  "The ``tail-pointer'' type defines a cons suitable as a tracking
   implement for efficient insertions at the tail of a list."
  '(cons T (or null (cons T null))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Questa".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Questa ()
  ((elements
    :initarg       :elements
    :initform      (cons 'head NIL)
    :type          list
    :documentation "The list of elements. Necessarily never empty, as
                    the TAIL pointer must reference its desinent cons,
                    the first and initial element constitutes a 'dummy'
                    cell with the value ('head . NIL).")
   (tail
    :initarg       :tail
    :initform      NIL
    :type          (or null tail-pointer)
    :documentation "A pointer to the last cons of the ELEMENTS, employed
                    as a warkloom for efficient insertions at the
                    Questa's rear."))
  (:documentation
    "The ``Questa'' class models the esoteric data structure 'Questa'
     (QUEue and STAck), a storage similar to the deque, but amenable to
     insertions only at its front.
     ---
     Please note, if intent on this class' maintenance, that the first
     ELEMENTS list cons always constitutes a 'dummy' node, installed in
     order to ascertain, with its 'cdr' (pointer) moeity, a source for
     the tail-cons (please see the TAIL slot) to possess a reference for
     queries and insertion at the ELEMENTS' tail. The dummy node does
     neither contribute to the size of the Questa, nor does it store
     any element in its 'car' place; in consectary, even clearing the
     ensconcing data structure does not eradicate this particular
     cons."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Empty-Questa-Error".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Questa-Error (error)
  ((questa
    :initarg       :questa
    :initform      NIL
    :reader        empty-questa-error-questa
    :type          (or null Questa)
    :documentation "The offended Questa.")
   (cause
    :initarg       :cause
    :initform      NIL
    :reader        empty-questa-error-cause
    :type          T
    :documentation "An optional message to prepend to the overall
                    error report."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Questa-Error condition))
      (declare (type destination        stream))
      (format stream "~@[~a ~]The Questa is empty."
        (empty-questa-error-cause condition))))
  (:documentation
    "Signals that an operation, which relies on at least one element's
     presence in the Questa, has been attempted on an empty instance."))

;;; -------------------------------------------------------

(defun signal-empty-questa-error (questa &optional (cause NIL))
  "Signals an ``Empty-Questa-Error'' involving the QUESTA in conjunction
   with an optional CAUSE, which defaults to ``NIL''."
  (declare (type Questa questa))
  (declare (type T      cause))
  (error 'Empty-Questa-Error :questa questa :cause cause))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of function prototypes.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard operations.
(declaim (ftype (function (Questa T) (values))      questa-push))
(declaim (ftype (function (Questa)   T)             questa-pop1))
(declaim (ftype (function (Questa)   T)             questa-pop2))
(declaim (ftype (function (Questa)   boolean)       questa-swap))

;; Extended, non-standard operations.
(declaim (ftype (function (Questa)   T)             questa-peek1))
(declaim (ftype (function (Questa)   T)             questa-peek2))
(declaim (ftype (function (Questa T) T)             questa-set1))
(declaim (ftype (function (Questa T) T)             questa-set2))
(declaim (ftype (function (Questa)   (values))      questa-clear))
(declaim (ftype (function (Questa)   boolean)       questa-is-empty))
(declaim (ftype (function (Questa)   (integer 0 *)) questa-size))

;; Additional operations.
(declaim (ftype (function (Questa)   list)          questa-element-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of generic functions.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((questa Questa) &key)
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (setf tail elements))
  (the Questa questa))

;;; -------------------------------------------------------

(defmethod print-object ((questa Questa) stream)
  (declare (type Questa      questa))
  (declare (type destination stream))
  (format stream "Questa(~{~a~^, ~})"
    (rest (slot-value questa 'elements))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of constructors.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-questa ()
  "Creates and returns an empty Questa."
  (the Questa (make-instance 'Questa)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun questa-push (questa new-element)
  "Adds the NEW-ELEMENT to the front of the QUESTA and returns no value.
   ---
   This function represents the Questa operation 'PUSH'."
  (declare (type Questa questa))
  (declare (type T      new-element))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (cond
      ((questa-is-empty questa)
        (setf (cdr tail) (cons new-element NIL))
        (setf tail       (cdr tail)))
      (T
        (push new-element elements)
        (rotatef (first elements) (second elements)))))
  (values))

;;; -------------------------------------------------------

(defun questa-pop1 (questa)
  "If the QUESTA is not empty, removes and returns the element at its
   front, otherwise signals an ``Empty-Questa-Error''.
   ---
   This function represents the Questa operation 'POP1'."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa "Cannot pop the top element.")
        (prog1
          ;; As the first item of the ELEMENTS always constitutes a
          ;; "dummy", the true front element is represented by the
          ;; second in its collection.
          (second elements)
          ;; Swap the head and the second element in order to be able to
          ;; use the built-in ``pop'' function on the latter, ultimately
          ;; rendering the head again the first element.
          (rotatef (first elements) (second elements))
          ;; Pop the second element, which has temporarily been moved to
          ;; the front of the ELEMENTS for this exact reason.
          (pop elements)
          ;; Did we just remove the TAIL pointer?
          ;; => Redirect it to the ELEMENTS.
          (when (= (length elements) 1)
            (setf tail elements)))))))

;;; -------------------------------------------------------

(defun questa-pop2 (questa)
  "If the QUESTA is not empty, removes and returns the element at its
   rear, otherwise signals an ``Empty-Questa-Error''."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot pop the bottom element.")
        (prog1
          (car tail)
          (setf elements (nbutlast elements))
          (setf tail     (last elements)))))))

;;; -------------------------------------------------------

(defun questa-swap (questa)
  "Exchanges the position of the first and the last element in the
   QUESTA, returning a ``boolean'' value of ``T'' if the QUESTA contains
   at least one element and thus responds to this operation, otherwise
   ``NIL''."
  (declare (type Questa questa))
  (the boolean
    (when (> (questa-size questa) 1)
      (with-slots (elements tail) questa
        (declare (type list         elements))
        (declare (type tail-pointer tail))
        (rotatef (second elements) (car tail)))
      T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of extended operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun questa-peek1 (questa)
  "If the QUESTA is not empty, returns without removing the element at
   its front, otherwise signals an ``Empty-Questa-Error''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements) questa
    (declare (type list elements))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot peek the top element.")
        (second elements)))))

;;; -------------------------------------------------------

(defun questa-peek2 (questa)
  "If the QUESTA is not empty, returns without removing the element at
   its end, otherwise signals an ``Empty-Questa-Error''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (tail) questa
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot peek the bottom element.")
        (car tail)))))

;;; -------------------------------------------------------

(defun questa-set1 (questa new-top)
  "Replaces the first QUESTA element with the NEW-TOP and returns the
   replaced element.
   ---
   An ``Empty-Questa-Error'' is signaled if the QUESTA is empty.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (declare (type T      new-top))
  (the T
    (if (questa-is-empty questa)
      (signal-empty-questa-error questa "Cannot set the top element.")
      (with-slots (elements) questa
        (declare (type list elements))
        (prog1
          (second elements)
          (setf (car (rest elements)) new-top))))))

;;; -------------------------------------------------------

(defun questa-set2 (questa new-bottom)
  "Replaces the first QUESTA element with the NEW-BOTTOM and returns the
   replaced element.
   ---
   An ``Empty-Questa-Error'' is signaled if the QUESTA is empty.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (declare (type T      new-bottom))
  (the T
    (if (questa-is-empty questa)
      (signal-empty-questa-error questa
        "Cannot set the bottom element.")
      (with-slots (tail) questa
        (declare (type tail-pointer tail))
        (the T
          (prog1
            (car tail)
            (setf (car tail) new-bottom)))))))

;;; -------------------------------------------------------

(defun questa-clear (questa)
  "Removes all elements from the QUESTA and returns no value.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (setf (cdr elements) NIL)
    (setf tail          elements))
  (values))

;;; -------------------------------------------------------

(defun questa-size (questa)
  "Returns the number of elements stored in the QUESTA.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements) questa
    (declare (type list elements))
    (the (integer 0 *)
      (1- (length elements)))))

;;; -------------------------------------------------------

(defun questa-is-empty (questa)
  "Checks whether the QUESTA is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type list         elements))
    (declare (type tail-pointer tail))
    (the boolean
      (not (null (eq elements tail))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of adscititious operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun questa-element-list (questa)
  "Returns a list containing the elements of the QUESTA.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (the list
    (copy-list
      (rest (slot-value questa 'elements)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test "POP1" and "POP2".
(let ((questa (make-questa)))
  (declare (type Questa questa))
  
  (questa-push questa 'one)
  (questa-push questa 'two)
  (questa-push questa 'three)
  (questa-push questa 'four)
  (questa-push questa 'five)
  (print questa)
  (print (questa-pop1 questa))
  (print questa)
  (print (questa-pop2 questa))
  (print questa))

;;; -------------------------------------------------------

;; Test "SWAP".
(let ((questa (make-questa)))
  (declare (type Questa questa))
  
  (print (questa-swap questa))
  (questa-push questa 'one)
  (print (questa-swap questa))
  (questa-push questa 'two)
  (print questa)
  (print (questa-swap questa))
  (print questa)
  (questa-push questa 'three))

;;; -------------------------------------------------------

;; Test "POP1" in conjunction with an empty Questa.
(let ((questa (make-questa)))
  (declare (type Questa questa))
  
  (questa-push questa "birds")
  (questa-push questa "loves")
  (questa-push questa "everyone")
  
  ;; Pop and print all top elements.
  (loop repeat (questa-size questa) do
    (print (questa-pop1 questa)))
  
  ;; This should signal an "Empty-Questa-Error".
  (questa-pop1 questa))

;;; -------------------------------------------------------

;; Test "POP1" in conjunction with an empty Questa and error handling.
(let ((questa (make-questa)))
  (declare (type Questa questa))
  
  (questa-push questa "birds")
  (questa-push questa "loves")
  (questa-push questa "everyone")
  
  ;; Pop and print all top elements.
  (loop repeat (questa-size questa) do
    (print (questa-pop1 questa)))
  
  ;; Handle the expected "Empty-Questa-Error".
  (handler-case
    (questa-pop1 questa)
    (Empty-Questa-Error (my-error)
      (format T "~&The empty Questa '~s' has signaled an error."
        (empty-questa-error-questa my-error)))))

;;; -------------------------------------------------------

;; Test the non-standard operations "SET1" and "SET2".
(let ((questa (make-questa)))
  (declare (type Questa questa))
  
  ;; Insert the content ("cats", "loves", "noone").
  (questa-push questa "noone")
  (questa-push questa "loves")
  (questa-push questa "cats")
  (print questa)
  
  ;; Retrieve the first element without removing.
  (print (questa-peek1 questa))
  ;; Retrieve the last element without removing.
  (print (questa-peek2 questa))
  
  ;; Reorder to ("noone", "loves", "cats").
  (questa-swap questa)
  (print questa)
  
  ;; Replace the first element "noone" by "everyone".
  (questa-set1 questa "everyone")
  (print questa)
  
  ;; Replace the last element "cats" by "birds", yielding
  ;; ("everyone", "loves", "birds").
  (questa-set2 questa "birds")
  (print questa))
