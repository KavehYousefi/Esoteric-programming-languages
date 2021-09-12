;; Date: 2021-09-12
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Kolmogorov"
;;   -> "http://www.99-bottles-of-beer.net/language-kolmogorov-2000.html"
;;       o An implementation of the program "99 bottles of beer" in the
;;         Kolmogorov programming language.
;;       o Note that the source code, according to the posting author,
;;         seems to have been distorted: Each occurrence of the byte
;;         value ``\0'' was substituted by the phrase "{CODE}".
;;   -> "https://ruslanspivak.com/lsbasi-part1/"
;;       o Describes the implementation of an interpreter for the
;;         programming language Pascal in Python.
;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype byte-value ()
  "The ``byte-value'' type describes a non-negative integer in the
   range [0, 255].
   ---
   Any data stored in the Kolmogorov programming language must adhere to
   this range or be adjusted to fit into the same."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype address ()
  "The ``address'' type describes an index from one node to another
   along a directed edge, whose maintained value identifies with this
   address.
   ---
   As with all other objects in the Kolmogorov programming language, an
   address must be in the ``byte-value'' range [0, 255]; additionally,
   a special sentinel value exists to permit a node to address itself,
   denoted with the asterisk '*' in the language and associated with
   the keyword symbol ``:active-node'' in this type. With each address
   being representative of a directed edge from one node to another,
   the former commonly designated as the edge's \"origin\" and the
   latter as its \"destination\", the ``:active-node'' specifier can be
   regarded as a loop, also known as a self-loop, leading from a node
   into itself."
  '(or (eql :active-node)
       byte-value))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type describes an object of the ``hash-table''
   type whose entries, if extant at all, exclusively consist of keys of
   the KEY-TYPE and values of the VALUE-TYPE, both of which default to
   the comprehensive ``T'' type."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for    key of-type T
                     being the hash-keys in (the hash-table object)
              using  (hash-value value)
              always (and (typep key key-type)
                          (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (element-type)
  "The ``list-of'' type describes an object of the ``list'' type which
   consists of zero or more elements, each exclusively of the
   ELEMENT-TYPE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((value
    :initarg       :value
    :initform      0
    :type          byte-value
    :documentation "The byte value stored in this node.")
   (destinations
    :initarg       :destinations
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of address Node)
    :documentation "A mapping of outgoing edge values (addresses) to
                    neighbor ``Node'' objects.
                    ---
                    Each entry key is defined by a byte value, that is,
                    an integer in the range [0, 255], which maps to the
                    destination ``Node'' reached by following this edge.
                    The special entry keyword key ``:active-node''
                    refers to the ensconcing ``Node'' itself.")
   (origins
    :initarg       :origins
    :initform      (make-hash-table :test #'eql)
    :accessor      node-origins
    :type          (hash-table-of address Node)
    :documentation "A mapping of incoming edge values (addresses) to
                    neighbor ``Node'' objects.
                    ---
                    Each entry key is defined by a byte value, that is,
                    an integer in the range [0, 255], which maps to the
                    origin ``Node'' reached by following this edge.
                    The special entry keyword key ``:active-node''
                    refers to the ensconcing ``Node'' itself."))
  (:documentation
    "The ``Node'' class represents a vertex or node in the Kolmogorov
     graph, storing an element as well outgoing and incoming edges.
     ---
     As edges in a Kolmogorov graph, based upon its directed variant,
     designate nothing apart from addresses, unique among the incoming
     edges and separately unique among those outgoing, both types of
     connections are implemented as integer-Node mappings. Conceptually,
     such a mapping associates a byte-valued edge address with the
     ``Node'' object opposite to the enveloping ``Node''. The special
     keyword symbol ``:active-node'' refers to this node itself."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((node Node) &key)
  (with-slots (destinations) node
    (declare (type (hash-table-of address Node) destinations))
    (setf (gethash :active-node destinations) node)))

;;; -------------------------------------------------------

(defun make-node (value)
  "Creates and returns a ``Node'' storing the VALUE."
  (declare (type byte-value value))
  (the Node (make-instance 'Node :value value)))

;;; -------------------------------------------------------

(defun node-value (node)
  "Returns the ``byte-value'' stored in the NODE."
  (declare (type Node node))
  (the byte-value (slot-value node 'value)))

;;; -------------------------------------------------------

(defun (setf node-value) (new-value node)
  "Sets the NODE's value to the NEW-VALUE and returns the, contingently
   adjusted to the valid ``byte-value'' range, updated NODE value.
   ---
   Being restricted to byte values, the NEW-VALUE is automatically
   clamped to the valid range if transgressing its bounds. A NEW-VALUE
   of less than zero will be converted to zero, a value greater than
   255 will be substituted by this upper bound."
  (declare (type integer new-value))
  (declare (type Node    node))
  (with-slots (value) node
    (declare (type byte-value value))
    (setf value (max 0 (min 255 new-value)))
    (the byte-value value)))

;;; -------------------------------------------------------

(defun node-get-destination (node address)
  "Returns the ``Node'' reached by following the edge identified by the
   ADDRESS, starting from this NODE as its origin, or ``NIL'' if no such
   edge could be found."
  (declare (type Node node))
  (declare (type address address))
  (with-slots (destinations) node
    (declare (type (hash-table-of address Node) destinations))
    (the (or null Node) (gethash address destinations))))

;;; -------------------------------------------------------

(defun node-set-destination (node new-destination address)
  "Sets the NEW-DESTINATION node reached by an edge with the ADDRESS
   value leaving from the NODE, and returns the modified NODE.
   ---
   If an edge amenable to the ADDRESS exists among the outgoing edges
   of the NODE, its association in the latter will be overridden,
   otherwise a new entry will be created. In any case, the
   NEW-DESTINATION node will not be modified, consigning this task to
   the programmer's own efforts."
  (declare (type Node    node))
  (declare (type Node    new-destination))
  (declare (type address address))
  (with-slots (destinations) node
    (declare (type (hash-table-of address Node) destinations))
    (setf (gethash address destinations) new-destination))
  (the Node node))

;;; -------------------------------------------------------

(defun node-remove-destination (node address)
  "Removes from the NODE the outgoing edge identified by the ADDRESS and
   returns a generalized boolean which resolves to true if the edge
   could be found and removed, otherwise yielding ``NIL''."
  (declare (type Node    node))
  (declare (type address address))
  (with-slots (destinations) node
    (declare (type (hash-table-of address Node) destinations))
    (remhash address destinations)))

;;; -------------------------------------------------------

(defun node-set-origin (destination origin address)
  "Sets the DESTINATION's, associated with the incoming edge amenable to
   the ADDRESS and issuing from the ORIGIN node, and returns the
   modified DESTINATION.
   ---
   If an edge amenable to the ADDRESS exists among the incoming edges
   of the DESTINATION, its association in the latter will be overridden,
   otherwise a new entry will be created. In any case, the ORIGIN node
   will not be modified, consigning this task to the programmer's own
   efforts."
  (declare (type Node    destination))
  (declare (type Node    origin))
  (declare (type address address))
  (with-slots (origins) destination
    (declare (type (hash-table-of address Node) origin))
    (setf (gethash address origins) origin))
  (the Node destination))

;;; -------------------------------------------------------

(defun node-remove-origin (destination address)
  "Diassociates the edge amenable to the ADDRESS from the incoming edges
   of the DESTINATION node, and returns a ``boolean'' value of ``T''
   upon success or ``NIL'' if the ADDRESS is not in use."
  (declare (type Node    destination))
  (declare (type address address))
  (with-slots (origins) destination
    (declare (type (hash-table-of address Node) origins))
    (the boolean (not (null (remhash address origins))))))

;;; -------------------------------------------------------

(defun node-connect-to (origin destination edge-value)
  "Connects the ORIGIN to the DESTINATION node by a new edge storing the
   EDGE-VALUE, and returns a ``boolean'' result equal to ``T'' if the
   connection could be established, otherwise ``NIL''.
   ---
   Upon consideration of joining the ORIGIN and DESTINATION nodes, three
   cases ought to be discriminated:
     (1) The ORIGIN does not yet use the EDGE-VALUE.
         - In this case, the nodes can be immediately connected.
     (2) The ORIGIN is already connected to a destination using the
         EDGE-VALUE, but the current destination does not equal the
         desired DESTINATION.
         - In this case, the extant edge betwixt the ORIGIN and its
           current destination must be removed on both sides.
         - Subsequently, the ORIGIN and DESTINATION can be joined.
     (3) The ORIGIN is already connected to this exact DESTINATION using
         the EDGE-VALUE.
         - In this case, any further action would be redundant, thus
           nothing ought to be done.
   
   A pseudocode representation of this process is the following:
     
     let currentNeighborOrigin = origin.getDestination (edgeValue)
     
     if currentNeighborOrigin = null then
       graph.connect (origin, destination, edgeValue)
       return true
     else if currentNeighborOrigin != destination then
       origin.removeEdge (edgeValue)
       graph.connect (origin, destination, edgeValue)
       return true
     else
       return false
    end if"
  (declare (type Node    origin))
  (declare (type Node    destination))
  (declare (type address edge-value))
  (flet ((connect-nodes ()
           "Connects the ORIGIN to the DESTINATION using the EDGE-VALUE,
            and vice versa."
           (node-set-destination origin      destination edge-value)
           (node-set-origin      destination origin      edge-value)
           (values)))
    ;; CURRENT-NEIGHBOR-OF-ORIGIN: The current node reached by leaving
    ;;   the ORIGIN with an edge of the address EDGE-VALUE. It may be
    ;;   the ``NIL''.
    (let ((current-neighbor-of-origin
            (node-get-destination origin edge-value)))
      (declare (type (or null Node) current-neighbor-of-origin))
      (the boolean
        (cond
          ;; No edge with EDGE-VALUE leaving from the ORIGIN?
          ;; => Simply connect ORIGIN and DESTINATION.
          ((null current-neighbor-of-origin)
            (connect-nodes)
            T)
          ;; Extant edge with EDGE-VALUE leaving from ORIGIN, but not
          ;; leading into the DESTINATION?
          ;; => Remove the edge with the EDGE-VALUE betwixt the ORIGIN and
          ;;    its current neighbor, then connect the ORIGIN with the
          ;;    DESTINATION.
          ((not (eq current-neighbor-of-origin destination))
            (node-remove-destination origin                     edge-value)
            (node-remove-origin      current-neighbor-of-origin edge-value)
            (connect-nodes)
            T)
          ;; Extant edge with EDGE-VALUE leaving from the ORIGIN and
          ;; leading into the DESTINATION?
          ;; => Redundant operation; do nothing.
          (T
            NIL))))))

;;; -------------------------------------------------------

(defun node-disconnect (node)
  "Removes all references to this NODE from its incoming and outgoing
   edges, clears its own connections, and returns the modified NODE."
  (declare (type Node node))
  (with-slots (origins) node
    ;; Remove all edges pointing into the NODE.
    (maphash
      #'(lambda (address origin)
          (declare (type address address))
          (declare (type Node    origin))
          (node-remove-destination origin address))
      origins)
    ;; Remove all outgoing edges.
    (clrhash origins))
  (with-slots (destinations) node
    ;; Remove all edges pointing away from the NODE.
    (maphash
      #'(lambda (address destination)
          (declare (type address address))
          (declare (type Node    destination))
          (node-remove-origin destination address))
      destinations)
    (clrhash destinations))
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node node))
  (declare (type (or null (eql T) stream string) stream))
  (with-slots (value) node
    (format stream "Node(~a)" value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Graph".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Graph ()
  ((active-node
    :initarg       :active-node
    :initform      (make-node 0)
    :type          Node
    :documentation "The active node, represented in the Kolmogorov
                    programming languages by the asterisk '*', and in
                    this implementation by the keyword symbol
                    ``:active-node''."))
  (:documentation
    "The ``Graph'' class represents the Kolmogorov graph, maintaining
     the active node.
     ---
     The Kolmogorov graph manifests as a directed graph, permitting
     self-loops and parallel or multiple edges."))

;;; -------------------------------------------------------

(defun make-graph ()
  "Creates and returns a new ``Graph'' initially containing only the
   active node."
  (the Graph (make-instance 'Graph)))

;;; -------------------------------------------------------

(defun graph-get-node-at (graph address)
  "Returns the ``Node'' accessible from the active node by following
   the edge holding the ADDRESS, or ``NIL'' if no such edge exists."
  (declare (type Graph   graph))
  (declare (type address address))
  (with-slots (active-node) graph
    (the (or null Node) (node-get-destination active-node address))))

;;; -------------------------------------------------------

(defun throw-missing-edge-error (edge-value &optional prefix)
  "Signals a ``simple-error'' apprizing about the EDGE-VALUE being
   undefined as an outgoing edge address for the active node, optionally
   prepending the PREFIX object, which will be displayed in text form,
   to the error message."
  (declare (type address edge-value))
  (declare (type T       prefix))
  (error "~:[Invalid edge:~;~:*~a~] No edge with value ~a leaves from the active node."
    prefix
    edge-value))

;;; -------------------------------------------------------

(defmethod print-object ((graph Graph) stream)
  (declare (type Graph                           graph))
  (declare (type (or null (eql T) stream string) stream))
  (with-slots (active-node) graph
    (format stream "Graph(active=~a)" active-node)))

;;; -------------------------------------------------------

(defun graph-assign (graph destination-node-value edge-value)
  "Creates a new node storing the DESTINATION-NODE-VALUE, and connects
   it with a new edge, leaving from the GRAPH's active node towards
   the newly created vertex and storing the EDGE-VALUE, finally
   returning the GRAPH.
   ---
   If completed successfully, the EDGE-VALUE will act as an address from
   the active node to the new node."
  (declare (type Graph      graph))
  (declare (type byte-value destination-node-value))
  (declare (type byte-value edge-value))
  (with-slots (active-node) graph
    (declare (type Node active-node))
    (let ((destination-node (make-node destination-node-value)))
      (declare (type Node destination-node))
      (node-connect-to active-node destination-node edge-value)))
  (the Graph graph))

;;; -------------------------------------------------------

(defun graph-join (graph from-address to-address edge-value)
  "Connects the two neighboring vertices of the GRAPH's active node,
   one reached by the outgoing edge with the value FROM-ADDRESS, the
   other reached by the outgoing edge with the value TO-ADDRESS by a new
   edge extending from the former to the latter, storing itself the
   EDGE-VALUE, and returns the GRAPH.
   ---
   If any of the two addresses FROM-ADDRESS and TO-ADDRESS cannot be
   detected among the active node's outgoing edges, a ``simple-error''
   is signaled."
  (declare (type Graph      graph))
  (declare (type address    from-address))
  (declare (type address    to-address))
  (declare (type byte-value edge-value))
  (let ((origin-node      (graph-get-node-at graph from-address))
        (destination-node (graph-get-node-at graph to-address)))
    (declare (type (or null Node) origin-node))
    (declare (type (or null Node) destination-node))
    (cond
      ((null origin-node)
        (throw-missing-edge-error from-address
          "Cannot find the origin node."))
      ((null destination-node)
        (throw-missing-edge-error to-address
          "Cannot find the destination node."))
      (T
        (node-connect-to origin-node destination-node edge-value))))
  (the Graph graph))

;;; -------------------------------------------------------

(defun graph-remove-node (graph address)
  "Removes from the GRAPH the node reached from the active node by
   following the edge with the ADDRESS, and return the modified GRAPH.
   ---
   An error occurs if the ADDRESS refers to the active node, that is,
   equals ``:active-node''.
   ---
   An error occurs if no edge with the ADDRESS enumerates among the
   outgoing edges of the active node."
  (declare (type Graph   graph))
  (declare (type address address))
  (with-slots (active-node) graph
    (declare (type Node active-node))
    (when (eql address :active-node)
      (error "Remove node: Cannot remove the active node."))
    (let ((node-to-remove (node-get-destination active-node address)))
      (declare (type (or null Node) node-to-remove))
      (unless node-to-remove
        (throw-missing-edge-error address "Cannot remove the node."))
      ;; Removes all references (edges) of the NODE-TO-REMOVE as a
      ;; destination and removes its own outgoing edges.
      (node-disconnect node-to-remove)))
  (the Graph graph))

;;; -------------------------------------------------------

(defun graph-remove-edge (graph edge-value)
  "Removes from the GRAPH's active node the outgoing edge storing the
   EDGE-VALUE, returning the GRAPH.
   ---
   If no outgoing edge from the active node exists being associated with
   the EDGE-VALUE, a ``simple-error'' is signaled."
  (declare (type Graph      graph))
  (declare (type byte-value edge-value))
  (with-slots (active-node) graph
    (declare (type Node active-node))
    (let ((destination (node-get-destination active-node edge-value)))
      (declare (type (or null Node) destination))
      (unless destination
        (throw-missing-edge-error edge-value "Cannot remove the edge."))
      (node-remove-origin      destination edge-value)
      (node-remove-destination active-node edge-value)))
  (the Graph graph))

;;; -------------------------------------------------------

(defun graph-seek (graph address)
  "Changes the active node of the GRAPH to the neighbor node accessible
   by the outgoing edge amenable to the ADDRESS, and returns the
   modified GRAPH.
   ---
   If the active node does not contain an outgoing edge with the
   ADDRESS, a ``simple-error'' is signaled."
  (declare (type Graph   graph))
  (declare (type address address))
  (with-slots (active-node) graph
    (declare (type Node active-node))
    (let ((new-active-node (node-get-destination active-node address)))
      (declare (type (or null Node) new-active-node))
      (unless new-active-node
        (throw-missing-edge-error address "Cannot move to this node."))
      (setf active-node new-active-node)))
  (the Graph graph))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token ()
  ((type
    :initarg       :type
    :initform      NIL
    :accessor      token-type
    :type          (or null keyword)
    :documentation "The type of this token.")
   (value
    :initarg       :value
    :initform      NIL
    :accessor      token-value
    :type          T
    :documentation "The value of this token."))
  (:documentation
    "The ``Token'' class represents a detected constituent of the
     Kolmogorov source code."))

;;; -------------------------------------------------------

(defun make-token (type value)
  (declare (type (or null keyword) type))
  (declare (type T                 value))
  (the Token (make-instance 'Token :type type :value value)))

;;; -------------------------------------------------------

(defun token-is-of-type (token expected-type)
  "Checks whether the TOKEN is of the EXPECTED-TYPE, returning a
   ``boolean'' value equal to ``T'' on parity and ``NIL'' otherwise."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (eq (slot-value token 'type) expected-type))

;;; -------------------------------------------------------

(defmethod print-object ((token Token) stream)
  (declare (type Token                           token))
  (declare (type (or null (eql T) stream string) stream))
  (with-slots (type value) token
    (format stream "Token(~s, ~s)" type value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "The lexer expects a source.")
    :type          string
    :documentation "The string containing the Kolmogorov source code.")
   (source-end
    :initarg       :source-end
    :initform      0
    :type          fixnum
    :documentation "The last position in the SOURCE. It is stored in
                    order to facilitate the checking of the
                    CURRENT-POSITION for the end of the SOURCE.")
   (current-position
    :initarg       :current-position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE. If its
                    progress unto or beyond the SOURCE-END, its advance
                    ceases.")
   (current-character
    :initarg       :current-character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the CURRENT-POSITION in the
                    SOURCE. It amounts to ``NIL'' if the position has
                    advanced to or beyond the SOURCE-END."))
  (:documentation
    "The ``Lexer'' class analyzes a source code of the Kolmogorov
     programming language and produces a series of tokens representative
     of the recognized constituents."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source source-end current-position current-character)
      lexer
    (cond
      ((plusp (length source))
        (setf source-end        (1- (length source)))
        (setf current-position  0)
        (setf current-character (char source current-position)))
      (T
        (setf source-end        0)
        (setf current-position  0)
        (setf current-character NIL)))))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' operating on the SOURCE string."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  (declare (type Lexer lexer))
  (with-slots (source source-end current-position current-character)
      lexer
    (declare (type string              source))
    (declare (type fixnum              source-end))
    (declare (type fixnum              current-position))
    (declare (type (or null character) current-character))
    (cond
      ((< current-position source-end)
        (incf current-position)
        (setf current-character (char source current-position)))
      (T
        (setf current-character NIL))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER constitutes a whitespace character,
   returning a ``boolean'' result which assumes ``T'' if the CHARACTER
   accounts for a whitespace, otherwise equaling ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character
        '(#\Linefeed #\Newline #\Return #\Space #\Tab)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current LEXER position, reads zero or more
   whitespaces, stopping if either a non-whitespace character or the
   end of the source has been encountered, and returns the LEXER."
  (declare (type Lexer lexer))
  (with-slots (current-character) lexer
    (declare (type (or null character) current-character))
    (loop
      while (and current-character
                 (whitespace-character-p current-character))
      do    (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current LEXER position, skips the comment, moving
   the LEXER to the first position following the comment or the end of
   the LEXER source.
   ---
   An error occurs if the LEXER source is exhausted before a closing
   quotation mark has been found."
  (declare (type Lexer lexer))
  (lexer-advance lexer)
  (with-slots (current-character) lexer
    (declare (type (or null character) current-character))
    (loop do
      (case current-character
        ((NIL) (error "Unclosed comment at position ~a. EOF reached."
                 (slot-value lexer 'current-position)))
        (#\"   (loop-finish))
        (#\\   (lexer-advance lexer)
               (lexer-advance lexer))
        (T     (lexer-advance lexer)))))
  (lexer-advance lexer)
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-byte-value (lexer)
  "Starting at the current LEXER position, reads and returns a
   ``byte-value'', positioning the LEXER to the first position following
   the value.
   ---
   An error is signaled if the detected number is not a valid byte
   value."
  (declare (type Lexer lexer))
  (with-slots (current-character) lexer
    (declare (type (or null character) current-character))
    (let ((byte-value
            (parse-integer
              (with-output-to-string (digits)
                (declare (type string-stream digits))
                (loop
                  while (and current-character
                             (digit-char-p current-character))
                  do    (write-char current-character digits)
                        (lexer-advance lexer))))))
      (unless (<= 0 byte-value 255)
        (error "Invalid byte value: ~a." byte-value))
      (the byte-value byte-value))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current LEXER position, reads and returns an
   identifier, positioning the LEXER immediately after the end of the
   identifier character.
   ---
   An error occurs if the identifier does not denote a valid command
   name."
  (declare (type Lexer lexer))
  (with-slots (current-character) lexer
    (declare (type (or null character) current-character))
    (the Token
      (prog1
        (case current-character
          (#\a (make-token :small-a    "a"))
          (#\i (make-token :small-i    "i"))
          (#\j (make-token :small-j    "j"))
          (#\o (make-token :small-o    "o"))
          (#\p (make-token :small-p    "p"))
          (#\r (make-token :small-r    "r"))
          (#\R (make-token :capital-r  "R"))
          (#\s (make-token :small-s    "s"))
          (T   (error "Lexer error: Unexpected character ~s at ~
                       position ~a."
                 current-character
                 (slot-value lexer 'current-position))))
        (lexer-advance lexer)))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next ``Token'' from the LEXER.
   ---
   If the LEXER is exhausted, a token of the type ``:eof'', being a
   representative of the end of the file, will be continuously
   delivered."
  (declare (type Lexer lexer))
  (with-slots (current-character) lexer
    (cond
      ((null current-character)
        (make-token :eof NIL))
      
      ((whitespace-character-p current-character)
        (lexer-skip-whitespaces lexer)
        (lexer-get-next-token lexer))
      
      ((char= current-character #\")
        (lexer-skip-comment lexer)
        (lexer-get-next-token lexer))
      
      ((char= current-character #\\)
        (lexer-advance lexer)
        (let ((byte-value (lexer-read-byte-value lexer)))
          (make-token :byte-value byte-value)))
      
      ((char= current-character #\*)
        (lexer-advance lexer)
        (make-token :asterisk "*"))
      
      ((char= current-character #\+)
        (lexer-advance lexer)
        (make-token :plus "+"))
      
      ((char= current-character #\-)
        (lexer-advance lexer)
        (make-token :minus "-"))
      
      ((char= current-character #\[)
        (lexer-advance lexer)
        (make-token :left-bracket "["))
      
      ((char= current-character #\])
        (lexer-advance lexer)
        (make-token :right-bracket "]"))
      
      ((char= current-character #\{)
        (lexer-advance lexer)
        (make-token :left-brace "{"))
      
      ((char= current-character #\})
        (lexer-advance lexer)
        (make-token :right-brace "}"))
      
      ((alpha-char-p current-character)
        (lexer-read-identifier lexer))
      
      (T
        (error "Invalid character: '~a'." current-character)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST).                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tree ()
  "The ``tree'' type describes a node, tree, or subtree in an abstract
   syntax tree (AST) of a parsed Kolmogorov code.
   ---
   For the sake of simplicity, this implementation does not discriminate
   betwixt the various parse tree nodes by manifestation in classes and
   subclasses, instead representing each such entity by a property list,
   the entry of which always associates the ``:node-type'' key with an
   identifier for the represented node type, while any further key-value
   pair stores the node-specific properties in a fashion akin to class
   slots. The ``assign'' command (``a''), for example, is realized in
   the following property list form:
     (:node-type         :assign
      :destination-value <destination-value>
      :edge-value        <edge-value>)
   ---
   Ensuing from this prototypical approach, the obtained flexibility is
   defrayed with a considerable detriment in expressive power."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (length (the list object)))
            (loop
              for    key of-type T in (the list object) by #'cddr
              always (keywordp key)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(defun make-tree (node-type &rest fields)
  "Creates and returns a parse ``tree'' of the NODE-TYPE and its
   required FIELDS.
   ---
   According to the property list requirements, the FIELDS must form a
   even-sized list, associating with each keyword-typed key an arbitrary
   value."
  (declare (type keyword node-type))
  (declare (type list    fields))
  (the tree (append (list :node-type node-type) fields)))

;;; -------------------------------------------------------

(defun tree-get-property (tree attribute-name &optional (default NIL))
  "Returns the TREE attribute associated with the ATTRIBUTE-NAME or
   the DEFAULT value, if the ATTRIBUTE-NAME could not be found."
  (declare (type tree    tree))
  (declare (type keyword attribute-name))
  (declare (type T       default))
  (the T (getf tree attribute-name default)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "The parser expects a lexer.")
    :type          Lexer
    :documentation "Provides the tokens from which to assemble an
                    abstract syntax tree (AST).")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The current token obtained by the last query of the
                    LEXER."))
  (:documentation
    "The ``Parser'' class assembles from a list of tokens, provided by
     the maintained ``Lexer'' a parse tree for a Kolmogorov program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (setf current-token (lexer-get-next-token lexer))))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'', obtaining its tokens from the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token is of the
   EXPECTED-TOKEN-TYPE, upon confirmation querying and storing the next
   token from the PARSER's lexer, before returning the PARSER itself,
   otherwise throwing an error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (cond
      ((token-is-of-type current-token expected-token-type)
        (setf current-token (lexer-get-next-token lexer)))
      (T
        (error "Expected a token of the type ~s, but received ~
                the token ~s."
               expected-token-type current-token))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-program (parser)
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((statements (parser-parse-statements parser)))
      (declare (type (list-of tree) statements))
      (let ((program-node
              (make-tree :program :statements statements)))
        (unless (token-is-of-type current-token :eof)
          (error "No EOF: ~s." current-token))
        (the tree program-node)))))

;;; -------------------------------------------------------

(defun parser-parse-expression (parser)
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (cond
      ((token-is-of-type current-token :asterisk)
        (parser-eat parser :asterisk)
        (make-tree :address :address :active-node))
      
      ((token-is-of-type current-token :byte-value)
        (let ((byte-value (token-value current-token)))
          (prog1
            (make-tree :byte-value :value byte-value)
            (parser-eat parser :byte-value))))
      
      ((token-is-of-type current-token :small-i)
        (parser-eat parser :small-i)
        (make-tree :input))
      
      ((token-is-of-type current-token :minus)
        (parser-eat parser :minus)
        (let* ((minuend    (parser-parse-expression parser))
               (subtrahend (parser-parse-expression parser)))
          (declare (type tree minuend))
          (declare (type tree subtrahend))
          (make-tree :subtraction :minuend minuend :subtrahend subtrahend)))
      
      ((token-is-of-type current-token :small-p)
        (parser-eat parser :small-p)
        (make-tree :peek :address (parser-parse-expression parser)))
      
      ((token-is-of-type current-token :plus)
        (parser-eat parser :plus)
        (let* ((augend (parser-parse-expression parser))
               (addend (parser-parse-expression parser)))
          (declare (type tree augend))
          (declare (type tree addend))
          (make-tree :addition :augend augend :addend addend)))
      
      (T
        (error "Invalid expression token: ~a." current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((tree NIL))
      (declare (type (or null tree) tree))
      (cond
        ((null current-token)
          (make-tree :no-op))
        ((token-is-of-type current-token :eof)
          (make-tree :no-op))
        
        ((token-is-of-type current-token :small-a)
          (parser-eat parser :small-a)
          (let* ((destination-value (parser-parse-expression parser))
                 (edge-value        (parser-parse-expression parser)))
            (declare (type tree destination-value))
            (declare (type tree edge-value))
            (setf tree (make-tree :assign
                                  :destination-value destination-value
                                  :edge-value        edge-value))))
        
        ((token-is-of-type current-token :small-j)
          (parser-eat parser :small-j)
          (let* ((origin      (parser-parse-expression parser))
                 (destination (parser-parse-expression parser))
                 (edge-value  (parser-parse-expression parser)))
            (declare (type tree origin))
            (declare (type tree destination))
            (declare (type tree edge-value))
            (setf tree (make-tree :join
                                  :origin      origin
                                  :destination destination
                                  :edge-value  edge-value))))
        
        ((token-is-of-type current-token :small-o)
          (parser-eat parser :small-o)
          (let ((argument (parser-parse-expression parser)))
            (declare (type tree argument))
            (setf tree (make-tree :output :argument argument))))
        
        ((token-is-of-type current-token :capital-r)
          (parser-eat parser :capital-r)
          (let ((address (parser-parse-expression parser)))
            (declare (type tree address))
            (setf tree (make-tree :remove-node :address address))))
        
        ((token-is-of-type current-token :small-r)
          (parser-eat parser :small-r)
          (let ((edge-value (parser-parse-expression parser)))
            (declare (type tree edge-value))
            (setf tree (make-tree :remove-edge :edge-value edge-value))))
        
        ((token-is-of-type current-token :small-s)
          (parser-eat parser :small-s)
          (let ((destination (parser-parse-expression parser)))
            (declare (type tree destination))
            (setf tree (make-tree :seek :destination destination))))
        
        ((token-is-of-type current-token :left-bracket)
          (parser-eat parser :left-bracket)
          (let ((test-node (parser-parse-expression parser)))
            (declare (type tree test-node))
            (let ((body (parser-parse-statements parser)))
              (declare (type (list-of tree) body))
              (parser-eat parser :right-bracket)
              (setf tree (make-tree :node-loop :test-node test-node :body body)))))
        
        ((token-is-of-type current-token :left-brace)
          (parser-eat parser :left-brace)
          (let* ((test-edge (parser-parse-expression parser))
                 (body      (parser-parse-statements parser)))
            (declare (type tree           test-edge))
            (declare (type (list-of tree) body))
            (parser-eat parser :right-brace)
            (setf tree (make-tree :edge-loop :test-edge test-edge :body body))))
        
        ((member (token-type current-token)
                 '(:asterisk :byte-value :small-i :minus :small-p :plus)
                 :test #'eq)
          (setf tree (parser-parse-expression parser)))
        
        (T
          (setf tree (make-tree :no-op))))
      (the tree tree))))

;;; -------------------------------------------------------

(defun parser-parse-statements (parser)
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((statements NIL))
      (declare (type (list-of tree) statements))
      (loop do
        (cond
          ((null current-token)
            (loop-finish))
          ((token-is-of-type current-token :eof)
            (loop-finish))
          ((token-is-of-type current-token :right-bracket)
            (loop-finish))
          ((token-is-of-type current-token :right-brace)
            (loop-finish))
          (T
            (push (parser-parse-statement parser) statements))))
      (the (list-of tree) (nreverse statements)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  (declare (type Parser parser))
  (the tree (parser-parse-program parser)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass AST-Visitor ()
  ()
  (:documentation
    "The ``AST-Visitor'' interface describes an entity which processes
     an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defgeneric ast-visitor-visit (visitor node-type tree)
  (:documentation
    "Processes the abstract syntax TREE node of the NODE-TYPE by the
     VISITOR, returning a result suitable for the respective
     NODE-TYPE."))

;;; -------------------------------------------------------

(defun dispatch-visitor (visitor tree)
  "Analyzes the abstract syntax TREE node and invokes the matching
   ``ast-visitor-visit'' method on the VISITOR, the TREE, and the
   node type contained in the latter, returning the method's result."
  (declare (type AST-Visitor visitor))
  (declare (type tree        tree))
  (funcall #'ast-visitor-visit
           visitor
           (tree-get-property tree :node-type)
           tree))

;;; -------------------------------------------------------

(defclass Interpreter (AST-Visitor)
  ((graph
    :initarg       :graph
    :initform      (make-graph)
    :type          Graph
    :documentation "The Kolmogorov graph being operated upon."))
  (:documentation
    "The ``Interpreter'' class implements an ``AST-Visitor'' for
     processing an abstract syntax tree (AST) and applying its commands
     on the internally maintained Kolmogorov graph."))

;;; -------------------------------------------------------

(defun make-interpreter ()
  "Creates and returns a new ``Interpreter''."
  (the Interpreter (make-instance 'Interpreter)))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :program))
                              tree)
  (let ((statements (tree-get-property tree :statements)))
    (declare (type (list-of tree) statements))
    (dolist (statement statements)
      (declare (type tree statement))
      (dispatch-visitor interpreter statement))))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :addition))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let* ((augend-tree (tree-get-property tree :augend))
         (addend-tree (tree-get-property tree :addend)))
    (declare (type tree augend-tree addend-tree))
    (let* ((augend-address (dispatch-visitor interpreter augend-tree))
           (addend-value   (dispatch-visitor interpreter addend-tree)))
      (declare (type address    augend-address))
      (declare (type byte-value addend-value))
      (with-slots (graph) interpreter
        (let ((augend-node (graph-get-node-at graph augend-address)))
          (declare (type Node augend-node))
          (incf (node-value augend-node) addend-value)))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :address))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (the address (tree-get-property tree :address)))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :assign))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((destination-tree (tree-get-property tree :destination-value))
        (edge-value-tree  (tree-get-property tree :edge-value)))
    (declare (type tree destination-tree))
    (declare (type tree edge-value-tree))
    (let ((destination-value (dispatch-visitor interpreter destination-tree))
          (edge-value        (dispatch-visitor interpreter edge-value-tree)))
      (declare (type byte-value destination-value))
      (declare (type byte-value edge-value))
      (with-slots (graph) interpreter
        (graph-assign graph destination-value edge-value))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :byte-value))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (the address (tree-get-property tree :value)))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :edge-loop))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((test-edge-tree (tree-get-property tree :test-edge))
        (body           (tree-get-property tree :body)))
    (declare (type tree           test-edge-tree))
    (declare (type (list-of tree) body))
    (let ((test-edge-value (dispatch-visitor interpreter test-edge-tree)))
      (declare (type byte-value test-edge-value))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (loop while (graph-get-node-at graph test-edge-value) do
          (dolist (statement body)
            (dispatch-visitor interpreter statement))))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :input))
                              tree)
  (declare (ignore interpreter))
  (declare (ignore node-type))
  (declare (ignore tree))
  (loop do
    (format T "~&Please enter a character: ")
    (let ((user-input (read-line)))
      (declare (type (or null string) user-input))
      (when (and user-input (plusp (length user-input)))
        (the byte-value (return (char-code (char user-input 0))))))))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :join))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((origin-tree      (tree-get-property tree :origin))
        (destination-tree (tree-get-property tree :destination))
        (edge-value-tree  (tree-get-property tree :edge-value)))
    (declare (type tree origin-tree))
    (declare (type tree destination-tree))
    (declare (type tree edge-value-tree))
    (let ((origin-address      (dispatch-visitor interpreter origin-tree))
          (destination-address (dispatch-visitor interpreter destination-tree))
          (edge-value          (dispatch-visitor interpreter edge-value-tree)))
      (declare (type address    origin-address))
      (declare (type address    destination-address))
      (declare (type byte-value edge-value))
      (with-slots (graph) interpreter
        (graph-join graph origin-address destination-address edge-value))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :node-loop))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((test-node-tree (tree-get-property tree :test-node))
        (body-tree      (tree-get-property tree :body)))
    (declare (type tree           test-node-tree))
    (declare (type (list-of tree) body-tree))
    (let ((test-node (dispatch-visitor interpreter test-node-tree)))
      (declare (type address test-node))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (loop until (zerop (node-value (graph-get-node-at graph test-node))) do
          (dolist (statement body-tree)
            (dispatch-visitor interpreter statement))))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :output))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((address-tree (tree-get-property tree :argument)))
    (declare (type tree address-tree))
    (let ((address (dispatch-visitor interpreter address-tree)))
      (declare (type address address))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (write-char
          (code-char (node-value (graph-get-node-at graph address))))))))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :peek))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((address-tree (tree-get-property tree :address)))
    (declare (type tree address-tree))
    (let ((address (dispatch-visitor interpreter address-tree)))
      (declare (type address address))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (the byte-value (node-value (graph-get-node-at graph address)))))))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :remove-edge))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((edge-value-tree (tree-get-property tree :edge-value)))
    (declare (type tree edge-value-tree))
    (let ((edge-value (dispatch-visitor interpreter edge-value-tree)))
      (declare (type byte-value edge-value))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (graph-remove-edge graph edge-value))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :remove-node))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((address-tree (tree-get-property tree :address)))
    (declare (type tree address-tree))
    (let ((address (dispatch-visitor interpreter address-tree)))
      (declare (type address address))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (graph-remove-node graph address))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :seek))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((destination-tree (tree-get-property tree :destination)))
    (declare (type tree destination-tree))
    (let ((destination (dispatch-visitor interpreter destination-tree)))
      (declare (type address destination))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (graph-seek graph destination))))
  (values))

;;; -------------------------------------------------------

(defmethod ast-visitor-visit ((interpreter Interpreter)
                              (node-type   (eql :subtraction))
                              tree)
  (declare (type Interpreter interpreter))
  (declare (ignore           node-type))
  (declare (type tree        tree))
  (let ((minuend-tree    (tree-get-property tree :minuend))
        (subtrahend-tree (tree-get-property tree :subtrahend)))
    (declare (type tree minuend-tree))
    (declare (type tree subtrahend-tree))
    (let ((minuend    (dispatch-visitor interpreter minuend-tree))
          (subtrahend (dispatch-visitor interpreter subtrahend-tree)))
      (declare (type address     minuend))
      (declare (type byte-value subtrahend))
      (with-slots (graph) interpreter
        (declare (type Graph graph))
        (decf (node-value (graph-get-node-at graph minuend)) subtrahend)
        (node-value (graph-get-node-at graph minuend))))))

;;; -------------------------------------------------------

(defun execute-kolmogorov-code (code)
  (declare (type string code))
  (let ((lexer (make-lexer code)))
    (declare (type Lexer lexer))
    (let ((parser (make-parser lexer)))
      (declare (type Parser parser))
      (let ((ast (parser-parse parser)))
        (declare (type tree ast))
        (let ((interpreter (make-interpreter)))
          (declare (type Interpreter interpreter))
          (dispatch-visitor interpreter ast))))))

;;; -------------------------------------------------------

(defun read-source-file (file-path)
  "Reads the file located under the FILE-PATH and returns its content as
   a ``simple-string''."
  (declare (type (or pathname stream string) file-path))
  (with-open-file (input file-path :direction :input)
    (let ((file-size (file-length input)))
      (declare (type (or null fixnum) file-size))
      (let ((file-content (make-array file-size :element-type 'character)))
        (declare (type simple-string file-content))
        (read-sequence file-content input)
        (the simple-string file-content)))))

;;; -------------------------------------------------------

(defun execute-kolmogorov-file (file-path)
  (declare (type (or pathname stream string) file-path))
  (execute-kolmogorov-code (read-source-file file-path)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node loop.
(execute-kolmogorov-code "+*\\9
                          [*
                            +*\\48
                            o*
                            -*\\49
                          ]
                          ")

;;; -------------------------------------------------------

;; Edge loop.
(execute-kolmogorov-code "a\\5\\1
                          a\\60\\0
                          j\\0\\1\\1
                          s\\0
                          [\\1
                            o*
                            {\\0
                              s\\0
                            }
                            ap\\1\\2
                            [\\2
                              a\\60\\0
                              j\\0\\1\\1
                              a\\0\\2 \"exit the loop\"
                            ]
                            -\\1\\1 \"subtract one from the counter node\"
                          ]
                          ")

;;; -------------------------------------------------------

;; 99 bottles.
(execute-kolmogorov-file
  "Kolmogorov/resources/99-bottles-of-beer.kolmogorov")

;;; -------------------------------------------------------

;; Cat program: copies a character from the standard input to the
;; standard output.
;; Note that this is an infinite loop, hence, the program must be
;; aborted irregularly.
(execute-kolmogorov-code
  "a\\1\\1 \"Create a dummy node at edge address 1 with a non-zero value for infinite repetitions.\"
   [\\1    \"Ensure an infinite loop by referring to the constant non-zero dummy node at edge 1.\"
     -*p*  \"Set the active node to zero by deducing from it its own value.\"
     +*i   \"Read a character from the user and store its ASCII code in the active node.\"
     o*    \"Output the active node's byte value, which contains the input ASCII character code.\"
   ]")
