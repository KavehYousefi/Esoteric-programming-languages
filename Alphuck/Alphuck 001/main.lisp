;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Alphuck", invented by the Esolang user "68.189.222.97" and
;; presented on September 6th, 2014, conceived as a derivative specimen
;; of Urban Mueller's "brainfuck" whose aefauld parcel of bona
;; adventitia amounts to the employment of alphabetic characters in lieu
;; of the original octuple's symbolic donet.
;; 
;; 
;; Concept
;; =======
;; The Alphuck programming language's diorism resolves to the verbatim
;; and consummate appropriation of its brainfuck cleronomy's, the
;; perimeter of which lays its amplectation across all aspects of the
;; architecture, type system, and operative competences, yet
;; concomitantly assigning a novel set of identifiers to the octuple
;; instruction set desumed from this legacy.
;; 
;; 
;; Instructions
;; ============
;; A syntactically reformulating ectype of brainfuck, Alphuck's
;; acquisition enhalses the entheus' entirety, enumerating the octuple
;; facilities that lend the same a Turing-complete status.
;; 
;; == OVERVIEW ==
;; The eight behests to the programmer's avail shall now be a cursory
;; species of nortelry's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   a       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ">".
;;   ..................................................................
;;   c       | Translates the cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "<".
;;   ..................................................................
;;   e       | Increments the current cell value by one (1).
;;           | If the new cell value transcends the upper march of 255,
;;           | its state is reset to the minimum of zero (0).
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "+".
;;   ..................................................................
;;   i       | Decrements the current cell value by one (1).
;;           | If the new cell value violates the lower march of zero
;;           | (0), its state wraps around to the maximum of 255.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "-".
;;   ..................................................................
;;   o       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ",".
;;   ..................................................................
;;   j       | Prints the character whose ASCII code equals the current
;;           | cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ".".
;;   ..................................................................
;;   p       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "s" command.
;;           | Otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "[".
;;   ..................................................................
;;   s       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "p" command.
;;           | Otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "]".
;;   ------------------------------------------------------------------
;; 
;; == EQUIPARATION OF ALPHUCK AND BRAINFUCK ==
;; The ligature of consanguinity imparts to Alphuck and brainfuck the
;; potential of an equiparation in consummate patration, the same shall
;; now be illustrated:
;; 
;;   -------------------
;;   Alphuck | brainfuck
;;   --------+----------
;;   a       | >
;;   ...................
;;   c       | <
;;   ...................
;;   e       | +
;;   ...................
;;   i       | -
;;   ...................
;;   j       | .
;;   ...................
;;   o       | ,
;;   ...................
;;   p       | [
;;   ...................
;;   s       | ]
;;   -------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, with a dioristic focus on the deployment of an
;; accommodated hash table in lieu of the language's autochthonous
;; specimen.
;; 
;; The thus produced associative data structure, founded upon a vector
;; of entries, complies with the concepts of "open addressing" and
;; "linear probing" for key collision resolutions, and ensues from the
;; elucidations of Ben Hoyt's [hoyt2021howimplhashtable] implementation
;; in the C programming language.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-11-23
;; 
;; Sources:
;;   [esolang2023Alphuck]
;;   The Esolang contributors, "Alphuck", November 21st, 2023
;;   URL: "https://esolangs.org/wiki/Alphuck"
;;   
;;   [hoyt2021howimplhashtable]
;;   Ben Hoyt, "How to implement a hash table (in C)", March 2021
;;   URL: "https://benhoyt.com/writings/hash-table-in-c/"
;;   Notes:
;;     - The appertaining GitHub repository can be found under:
;;       -> "https://github.com/benhoyt/ht/blob/master/ht.c".
;;   
;;   [nystrom2021craftintIII20ht]
;;   Robert Nystrom,
;;     "Crafting Interpreters", 2021, chapter "III.20 Hash Tables"
;;   URL: "https://craftinginterpreters.com/hash-tables.html"
;;   Notes:
;;     - Describes the implementation of hash tables.
;;   
;;   [wikipedia2023fowernollvohash]
;;   The Wikipedia contributors, "Fowler-Noll-Vo hash function",
;;     October 3rd, 2023
;;   URL: "https://en.wikipedia.org/wiki/
;;         Fowler%E2%80%93Noll%E2%80%93Vo_hash_function"
;;   Notes:
;;     - Describes the Fowler-Noll-Vo hash function.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type (type-name
                                  (candidate-name &rest lambda-list)
                                  &body body)
  "Defines a derived type by exploiting the ``deftype'' infrastructure
   in conjunction with the ``satisfies'' specifier in announcing a new
   species of the TYPE-NAME, the parameters of which issue from the
   LAMBDA-LIST, and whose forms are contributed through the BODY,
   granted an adit to the probed object by its agnomination via the
   CANDIDATE-NAME, and expected to return a generalized Boolean \"true\"
   value for the candidate's eligibility, otherwise ``NIL''.
   ---
   If the first BODY form constitutes a string, this element is
   construed as a documentation string for the derived type, removed,
   and reappropriated for the selfsame purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name (,@lambda-list)
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Defines the type ``~s''." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))

;;; -------------------------------------------------------

(define-predicated-type stack-of (candidate &optional (element-type '*))
  "The ``stack-of'' type defines a list-based stack compact of zero or
   more elements, each member of which conforms to the ELEMENT-TYPE,
   assuming as its default form the generic sentinel ``*''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, and thus spanning the closed integral range of
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype uint64 ()
  "The ``uint64'' type defines an unsigned byte composed of 64 accolent
   bits."
  '(unsigned-byte 64))

;;; -------------------------------------------------------

(deftype size ()
  "The ``size'' type defines a non-negative integer greater than or
   equal to zero (0), but without any upper bourne's imposition, thus
   spanning the closed integral range of [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype entry-vector ()
  "The ``entry-vector'' type defines a one-dimensional simple array
   composed of zero or more ``HTable-Entry'' objects or ``NIL'' value
   occurrences, compatible with the ``HTable'' class' diorism."
  '(simple-array (or null HTable-Entry) (*)))

;;; -------------------------------------------------------

(deftype hash-function ()
  "The ``hash-function'' type defines a hash function as a unary
   function which, provided with a key of any species, responds with an
   unsigned 64-bit hash value.
   ---
   The thus represented function conforms to the signature:
     lambda (key) => uint64"
  '(function (*) uint64))

;;; -------------------------------------------------------

(deftype comparator ()
  "The ``comparator'' type defines a function responsible for the
   determination of two probed objects, one imposing the key of an
   extant entry in the hash table, the other a candidate to match
   against, anenst their equality, realized as a binary function of any
   input which responds with a generalized boolean, the value of which
   must assume a non-``NIL'' form for the test subjects' equality,
   otherwise the ``NIL'' datum."
  '(function (* *) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   comprehending, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of FVN-1a hash function.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type uint64 +FVN-64-BIT-OFFSET-BIAS+))
(declaim (type uint64 +FVN-64-BIT-PRIME+))

;;; -------------------------------------------------------

(defparameter +FVN-64-BIT-OFFSET-BIAS+ 14695981039346656037
  "Defines the initial hash value for the calculation of the hash in the
   64-bit hash function.")

(defparameter +FVN-64-BIT-PRIME+ 1099511628211
  "Defines the prime number involved in the iterative calculation of the
   64-bit hash value.")

;;; -------------------------------------------------------

(defgeneric compute-fvn-1a-hash (key)
  (:documentation
    "Supputates and returns for the KEY a 64-bit unsigned hash value.")
  
  (:method ((key integer))
    (declare (type integer key))
    (let ((hash           +FVN-64-BIT-OFFSET-BIAS+)
          (number-of-bits (integer-length key)))
      (declare (type uint64 hash))
      (declare (type size   number-of-bits))
      (loop
        for bit-position
          of-type size
          from    0
          below   number-of-bits
          by      8
        for current-byte
          of-type octet
          =       (ldb (byte 8 bit-position) key)
        do
          (setf hash
            (ldb (byte 64 0)
              (* (logxor hash current-byte)
                 +FVN-64-BIT-PRIME+))))
      (the uint64 hash)))
  
  (:method ((key string))
    (declare (type string key))
    (let ((hash +FVN-64-BIT-OFFSET-BIAS+))
      (declare (type uint64 hash))
      (loop for character of-type character across key do
        (setf hash
          (ldb (byte 64 0)
            (* (logxor hash (char-code character))
               +FVN-64-BIT-PRIME+))))
      (the uint64 hash)))
  
  (:method ((key T))
    (declare (type T key))
    (the uint64
      (compute-fvn-1a-hash
        (sxhash key)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "HTable-Entry".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct HTable-Entry
  "The ``HTable-Entry'' class encapsulates an entry for a hash table of
   the species ``HTable'', compact of a key and a value, the former
   moeity of which may not be modified."
  (key   (error "Missing key.")   :type T :read-only T)
  (value (error "Missing value.") :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of entry vector operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-entry-vector (capacity)
  "Creates and returns a new ``entry-vector'' enumerating the CAPACITY
   tally of initially ``NIL''-valued slots."
  (declare (type size capacity))
  (the entry-vector
    (make-array capacity
      :element-type    '(or null HTable-Entry)
      :initial-element NIL
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "HTable".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass HTable ()
  ((entries
    :initform      (make-entry-vector 0)
    :type          entry-vector
    :documentation "A vector which stores the key-value pairs,
                    maintaining ``NIL'' values as sentinels for vacant
                    slots, the same enumerate a tally of CAPACITY
                    members.")
   (capacity
    :initarg       :capacity
    :type          size
    :documentation "The number of slots, and thus the maximum number of
                    elements currently admissive, to the hash table, in
                    this being tantamount to the ENTRIES' length.")
   (size
    :initform      0
    :type          size
    :documentation "The actual number of entries currently stored in the
                    hash table, which is tantamount to the tally of
                    non-``NIL'' slots in the ENTRIES vector.")
   (hash-function
    :initarg       :hash-function
    :initform      (error "Missing hash function.")
    :type          hash-function
    :documentation "The function responsible for the generation of
                    hashes for a key to insert.")
   (key-comparator
    :initarg       :key-comparator
    :initform      (error "Missing comparator.")
    :type          comparator
    :documentation "The function responsible for testing a key in the
                    table and an input key for equality."))
  (:documentation
    "The ``HTable'' class models a simple hash table of niggardliness in
     competences, founded upon open addressing in conjunction with
     linear probing for its collision resolutions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((table HTable) &key)
  "Sets the hash TABLE's entries to a new entry vector whose size equals
   the initial capacity and returns no value.."
  (declare (type HTable table))
  (with-slots (entries capacity) table
    (declare (type entry-vector entries))
    (declare (type size         capacity))
    (setf entries
      (make-entry-vector capacity)))
  (values))

;;; -------------------------------------------------------

(defun make-htable (&key (initial-capacity 15)
                         (hash-function    #'compute-fvn-1a-hash)
                         (key-comparator   #'eql))
  "Creates and returns a new ``HTable'' instance, optionally configured
   with the INITIAL-CAPACITY, a bespoke HASH-FUNCTION, and a
   KEY-COMPARATOR."
  (the HTable
    (make-instance 'HTable
      :capacity       initial-capacity
      :hash-function  hash-function
      :key-comparator key-comparator)))

;;; -------------------------------------------------------

(defun htable-size (table)
  "Returns the number of entries in the hash TABLE."
  (declare (type HTable table))
  (the size
    (slot-value table 'size)))

;;; -------------------------------------------------------

(defun htable-get-entry-at (table index)
  "Returns the hash TABLE entry located at the INDEX, or responds with
   ``NIL'' if no such is set."
  (declare (type HTable table))
  (declare (type size   index))
  (the (or null HTable-Entry)
    (aref (slot-value table 'entries) index)))

;;; -------------------------------------------------------

(defun htable-set-entry-at (table index new-entry)
  "Stores the NEW-ENTRY in the hash TABLE at the specified INDEX and
   returns no value."
  (declare (type HTable       table))
  (declare (type size         index))
  (declare (type HTable-Entry new-entry))
  (setf (aref (slot-value table 'entries) index)
        new-entry)
  (values))

;;; -------------------------------------------------------

(defun htable-entry-empty-p (table index)
  "Determines whether the hash TABLE slot at the INDEX is vacant, which
   means that it contains the ``NIL'' value instead of an
   ``HTable-Entry'' instance, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type HTable table))
  (declare (type size   index))
  (the boolean
    (null (aref (slot-value table 'entries) index))))

;;; -------------------------------------------------------

(defun htable-entry-key-matches-p (table index probed-key)
  "Determines whether the hash TABLE entry at the INDEX exists and
   matches the PROBED-KEY, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type HTable table))
  (declare (type size   index))
  (declare (type T      probed-key))
  (the boolean
    (unless (htable-entry-empty-p table index)
      (not (null
        (funcall
          (slot-value table 'key-comparator)
          (htable-entry-key
            (htable-get-entry-at table index))
          probed-key))))))

;;; -------------------------------------------------------

(defun htable-get-index-for-key (table key)
  "Returns the index into the hash TABLE's entries corresponding with
   the KEY."
  (declare (type HTable table))
  (declare (type T      key))
  (let ((hash (funcall (slot-value table 'hash-function) key)))
    (declare (type uint64 hash))
    (the size
      (logand hash
        (1- (slot-value table 'capacity))))))

;;; -------------------------------------------------------

(defun htable-store-entry (table key value)
  "Expecting the hash TABLE's capacity to suffice for a continent
   extension, stores the KEY-VALUE pair in the same, either updating an
   extant entry's value, or creating a new one, and returns no value."
  (declare (type HTable table))
  (declare (type T      key))
  (declare (type T      value))
  ;; Employ linear probing for finding either extant or a vacant slot
  ;; for the new KEY-VALUE twain.
  (loop
    for index
      of-type size
      =       (htable-get-index-for-key table key)
      then    (mod (1+ index)
                (slot-value table 'capacity))
    
    until (htable-entry-empty-p table index)
    
    ;; Entry found with this KEY?
    ;; => Supersede its value with the new VALUE.
    when (htable-entry-key-matches-p table index key) do
      (setf (htable-entry-value
              (htable-get-entry-at table index))
            value)
      (return NIL)
    
    ;; KEY not yet present in the TABLE?
    ;; => Insert a new slot and increment the TABLE size.
    finally
      (htable-set-entry-at table index
        (make-htable-entry :key key :value value))
      (incf (slot-value table 'size)))
  (values))

;;; -------------------------------------------------------

(defun htable-expand-if-necessary (table)
  "Determines whether the hash TABLE vindicates an expansion of its
   underlying array, on confirmation accompassing the same, in any case
   returning no value."
  (declare (type HTable table))
  (when (>= (slot-value table 'size)
            (/ (slot-value table 'capacity) 2))
    (let ((old-entries (slot-value table 'entries)))
      (declare (type entry-vector old-entries))
      (let ((new-capacity (* (slot-value table 'capacity) 2)))
        (declare (type size new-capacity))
        (setf (slot-value table 'entries)
              (make-entry-vector new-capacity))
        (setf (slot-value table 'capacity) new-capacity)
        (setf (slot-value table 'size)     0))
      (loop
        for    entry of-type (or null HTable-Entry) across old-entries
        unless (null entry)
        do     (htable-store-entry table
                 (htable-entry-key   entry)
                 (htable-entry-value entry)))))
  (values))

;;; -------------------------------------------------------

(defun htable-put (table key value)
  "Stores the KEY-VALUE pair in the TABLE and returns no value."
  (declare (type HTable table))
  (declare (type T      key))
  (declare (type T      value))
  (htable-expand-if-necessary table)
  (htable-store-entry         table key value)
  (values))

;;; -------------------------------------------------------

(defun htable-get (table key &optional (default NIL))
  "Searches for the entry associated with the KEY in the TABLE and
   returns two values:
     (1) If the KEY could be detected, the value associated with the
         same, otherwise the DEFAULT object.
     (2) If the KEY could be detected, a ``boolean'' value of ``T'',
         otherwise ``NIL''."
  (declare (type HTable table))
  (declare (type T      key))
  (declare (type T      default))
  (the (values T boolean)
    (loop
      for index
        of-type size
        =       (htable-get-index-for-key table key)
        then    (mod (1+ index)
                  (slot-value table 'capacity))
      
      until (htable-entry-empty-p table index)
      
      when (htable-entry-key-matches-p table index key) do
        (return
          (values
            (htable-entry-value
              (htable-get-entry-at table index))
            T))
      
      finally
        (return
          (values default NIL)))))

;;; -------------------------------------------------------

(defmethod print-object ((table HTable) stream)
  (declare (type HTable      table))
  (declare (type destination stream))
  (format stream "~&HTable of ~d entr~:@p (capacity: ~d)"
    (slot-value table 'size)
    (slot-value table 'capacity))
  (loop
    for entry
      of-type (or null HTable-Entry)
      across  (slot-value table 'entries)
    when entry do
      (format stream "~&~2t~s => ~s"
        (htable-entry-key   entry)
        (htable-entry-value entry))))

;;; -------------------------------------------------------

(define-predicated-type htable-of (candidate
                                   &optional (key-type   '*)
                                             (value-type '*))
  "The ``htable-of'' type defines a hash table of the type ``HTable''
   composed of zero or more entries, each key of which conforms to the
   KEY-TYPE and associates with a value of the VALUE-TYPE, both
   defaulting to the generic sentinel ``*''."
  (and
    (typep candidate 'HTable)
    (loop
      for entry
        of-type (or null HTable-Entry)
        across  (slot-value candidate 'entries)
      always
        (or
          (null entry)
          (and
            (typep (htable-entry-key   entry) key-type)
            (typep (htable-entry-value entry) value-type))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-htable)
    :type          (htable-of integer octet)
    :documentation "A sparse vector of cells, infinite in their
                    dispansion along both axes, and each such with the
                    capacity for a scalar unsigned byte value's
                    storage.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The motile cell pointer, responsible for the
                    selection of the currently active cell among the
                    CELLS vector by storing its address (index)."))
  (:documentation
    "The ``Memory'' class serves in the modeling of the Alphuck program
     memory as a bilaterally infinite tape of unsigned byte-valued
     cells, the currently amenable instance among which is at any time
     designated by a motile cell pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type (htable-of integer octet) cells))
    (declare (type integer                   pointer))
    (the octet
      (htable-get cells pointer 0))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type (htable-of integer octet) cells))
    (declare (type integer                   pointer))
    (htable-put cells pointer
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one, contingently
   wrapping around its state upon the upper bourne's transcendence to
   the minimum of zero (0), and returns no value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell value by one, contingently
   wrapping around its state upon the lower bourne's transgression to
   the maximum of 255, and returns no value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and
   returns no value."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Jump-Table" class.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-htable)
    :type          (htable-of fixnum fixnum)
    :documentation "Maintains a bidirection association of jump forward
                    and back points, represented by the positions in the
                    executed Alphuck program."))
  (:documentation
    "The ``Jump-Table'' class applies itself to the castaldy of the jump
     points' vincula in an Alphuck program."))

;;; -------------------------------------------------------

(defun make-empty-jump-table ()
  "Creates and returns an empty ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun jump-table-connect (table first-end-point second-end-point)
  "Connects the FIRST-END-POINT and the SECOND-END-POINT in a
   bidirectional fashion, stores the new connection in the jump TABLE,
   and returns no value."
  (declare (type Jump-Table table))
  (declare (type fixnum     first-end-point))
  (declare (type fixnum     second-end-point))
  (with-slots (connections) table
    (declare (type (htable-of fixnum fixnum) connections))
    (htable-put connections first-end-point  second-end-point)
    (htable-put connections second-end-point first-end-point))
  (values))

;;; -------------------------------------------------------

(defun compute-jump-table (code)
  "Generates and returns a jump table for the piece of Alphuck CODE,
   connecting its jump points."
  (declare (type string code))
  (let ((jump-table          (make-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table        jump-table))
    (declare (type (stack-of fixnum) forward-jump-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      if (char= token #\p) do
        (push position forward-jump-points)
      else if (char= token #\s) do
        (if forward-jump-points
          (jump-table-connect jump-table
            (pop forward-jump-points)
            position)
          (error "Unmatched back jump point at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            (nreverse forward-jump-points))))
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defun jump-table-get-destination (table start-point)
  "Returns the opposite end point associated with the START-POINT in the
   jump TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type Jump-table table))
  (declare (type fixnum     start-point))
  (with-slots (connections) table
    (declare (type (htable-of fixnum fixnum) connections))
    (multiple-value-bind (destination contains-connection-p)
        (htable-get connections start-point)
      (declare (type (or null fixnum) destination))
      (declare (type boolean          contains-connection-p))
      (the fixnum
        (if contains-connection-p
          destination
          (error "No end point is associated with the position ~d."
            start-point))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          string
    :documentation "The piece of Alphuck source code to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position into
                    the SOURCE.")
   (jump-table
    :initform      (make-empty-jump-table)
    :type          Jump-Table
    :documentation "A bidirectional mapping of the jump end points
                    commorant in the SOURCE code.")
   (memory
    :initform      (make-memory)
    :documentation "The infinite tape of unsigned byte-valued cells,
                    operated upon by a cell pointer which selects the
                    currently amenable instance."))
  (:documentation
    "The ``Interpreter'' class serves as an entity responsible to
     accompass the operations of a piece of Alphuck source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Supputates the jump table for the INTERPRETER's Alphuck program and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-table)
    (compute-jump-table
      (slot-value interpreter 'source)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (source)
  "Creates and returns a new ``Interpreter'' which serves to evaluate
   the Alphuck SOURCE code."
  (declare (type string source))
  (the Interpreter
    (make-instance 'Interpreter :source source)))

;;; -------------------------------------------------------

(defun interpreter-finished-p (interpreter)
  "Determines whether the INTERPRETER's internally managed Alphuck
   program is exhausted, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (array-in-bounds-p
        (slot-value interpreter 'source)
        (slot-value interpreter 'ip)))))

;;; -------------------------------------------------------

(defun interpreter-get-current-token (interpreter)
  "Returns the current Alphuck source code token maintained by the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (the character
    (aref
      (slot-value interpreter 'source)
      (slot-value interpreter 'ip))))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its Alphuck program and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'ip)
    (min (1+ (slot-value interpreter 'ip))
      (length (slot-value interpreter 'source))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-jump (interpreter)
  "Expecting to reside at a jump instruction, relocates the
   INTERPRETER's instruction pointer (IP) to the opposite end point in
   its Alphuck program and returns no value, othrwise signals an error
   of an unspecified type."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'ip)
    (jump-table-get-destination
      (slot-value interpreter 'jump-table)
      (slot-value interpreter 'ip)))
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-process-token (interpreter token)
  (:documentation
    "Processes the Alphuck source code TOKEN in the INTERPRETER's
     context and returns no value.")
  
  (:method ((interpreter Interpreter) (token (eql #\a)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (memory-move-right memory))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\c)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (memory-move-left memory))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\e)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (memory-increment memory))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\i)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (memory-decrement memory))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\j)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (write-char
        (code-char
          (memory-current-cell memory))))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\o)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (format T "~&>> ")
    (finish-output)
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (setf (memory-current-cell memory)
        (char-code
          (read-char))))
    (clear-input)
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\p)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (when (zerop (memory-current-cell memory))
        (interpreter-jump interpreter)))
    (values))
  
  (:method ((interpreter Interpreter) (token (eql #\s)))
    (declare (type Interpreter interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (with-slots (memory) interpreter
      (declare (type Memory memory))
      (unless (zerop (memory-current-cell memory))
        (interpreter-jump interpreter)))
    (values))
  
  (:method ((interpreter Interpreter) (token T))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type character   token))
    (declare (ignore           token))
    (values)))

;;; -------------------------------------------------------

(defun interpreter-execute (interpreter)
  "Executes the Alphuck program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-finished-p interpreter) do
    (interpreter-process-token interpreter
      (interpreter-get-current-token interpreter))
    (interpreter-advance interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Alphuck (code)
  "Interpret the piece of Alphuck source CODE and returns no value."
  (declare (type string code))
  (interpreter-execute
    (make-interpreter code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Alphuck
  "eeeeeeeepaeeeepaeeaeeeaeeeaeccccisaea
   eaiaaepcscisaajaiiijeeeeeeejjeeejaajcijcjeeej
   iiiiiijiiiiiiiijaaejaeej")

;;; -------------------------------------------------------

;; Repeating cat program which terminates for a user input equal to the
;; "null character".
(interpret-Alphuck "opjos")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Alphuck "ojpiiaepaascpjsccs")
