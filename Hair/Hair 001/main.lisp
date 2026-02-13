;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Hair", invented by the Esolang user "A()" and presented on
;; December 31st, 2025, its dioristic acquisition such as to consign a
;; program's syntactical guise to the concord with symbols thilk pursue
;; coiffures' semblances, while operating on a memory twyforked into an
;; infinite tape and a scalar accumulator, both of which apply
;; themselves to the unsigned byte species' castaldy.
;; 
;; 
;; Concept
;; =======
;; The Hair programming language is founded upon the mimicry of hair
;; styles in its syntaxis' mold, the warklume of this art's pursuit a
;; particular assemblage anent its chosen instruction identifiers
;; repertoire.
;; 
;; == PROGRAMS LIMN COIFFURES' SEMBLANCES ==
;; The claviger to the language's kenspeckle phenotype, in its pursuit
;; of coiffures' replication, constitutes the set of instruction
;; identifiers, each an aefauld symbol's coalition with an operative
;; warkloom.
;; 
;; Characters eloigned from an epiphenomenal vallidom entertain a
;; tolerance thilk equiparates to their neglect.
;; 
;; == THE MEMORY: A TWISSEL OF TAPE AND ACCUMULATOR ==
;; The program memory limns a twyforked genesis, upon its firmament a
;; bilaterally infinite tape of unsigned byte-valued cells engages in a
;; champarty with a scalar accumulator whose capacity metes a single
;; such unit's semblant.
;; 
;; == THE TAPE: AN INFINITE CATENA OF BYTES ==
;; The paravaunt salvatory, concomitant a superior grade of capacity's
;; pernor, ostends itself in the tape as a bilaterally infinite
;; mountenance of unsigned byte-valued cells.
;; 
;; Each constituent admission supputates a scalar integral object
;; desumed from the closed integral interval [0, 255]. Upon the upper
;; extremum's transgression, the state wraps around to the minimum of
;; 0 (zero); siclike, a descent alow the smaller mear instigates a
;; resort to the maximum of 255.
;; 
;; Operating upon this catena, a "cell pointer", at the program's
;; inchoacy empight on a first unit, selects at any instant the
;; currently active cell, thilk designates the exclusive recipiency of
;; amenability to perquisitions and modulations. Its capacitation
;; concerning motation induces a periegesis' homologation athwart the
;; tape's entirety in a per gradus locomation.
;; 
;; == THE ACCUMULATOR: A BYTE-SIZED PARERGON ==
;; A perhedral rank of significance's recipient, the accumulator's
;; furnishment does not levitate aboon a scalar unsigned byte datum's
;; castaldy. This incolant mimics a tape cell's haecceity in its
;; aspects' entire compass: Impounded to the integral range [0, 255],
;; a peragration along the upper march concludes with a return to the
;; minimum of zero (0); while the overthwart case of this bourne's
;; violation inflicts a revolution to the higher extremum of 255.
;; 
;; 
;; Instructions
;; ============
;; The Hair programming language's operative compass wists of a
;; duodecimal cardinality, its bailiwicks decussate the tape and
;; accumulator management, basic arithmetics, input and output commerce,
;; as well as an unconditional control flow duction mechanism.
;; 
;; == OVERVIEW ==
;; The following apercu's cynosure shall be edified upon the dever of a
;; requisite moutance of gnarity's communication concerning the
;; operative warklumes:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   (       | Translates the tape cell pointer a random accompt of
;;           | steps, which is never less than one (1), in the dextral
;;           | direction.
;;   ..................................................................
;;   )       | Translates the tape cell pointer a random accompt of
;;           | steps, which is never less than one (1), in the
;;           | sinistral direction.
;;   ..................................................................
;;   /       | Increments the current tape cell's value by one (1).
;;   ..................................................................
;;   \       | Decrements the current tape cell's value by one (1).
;;   ..................................................................
;;   {       | Increments the accumulator's value by one (1).
;;   ..................................................................
;;   }       | Decrements the accumulator's value by one (1).
;;   ..................................................................
;;   |       | Copies the accumulator value into the current tape cell.
;;   ..................................................................
;;   -       | Copies the current tape cell's value into the
;;           | accumulator.
;;   ..................................................................
;;   [       | If the current tape cell value does not equal the
;;           | accumulator's state, selects in an aleatory manner a
;;           | succeeding back jump point "]", and relocates the
;;           | instruction pointer (IP) to the position immediately
;;           | succeeding the same; otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | Upon each evaluation, even with the control flow having
;;           | returned from a preveniently sojourned "]" position, a
;;           | new random destination is chosen for subsequent
;;           | navigation.
;;   ..................................................................
;;   ]       | Relocates the instruction pointer (IP) to the "["
;;           | instruction whence it emerged.
;;   ..................................................................
;;   =       | Prints the character whose ASCII code concurs with the
;;           | current tape cell's value to the standard output
;;           | conduit.
;;   ..................................................................
;;   ~       | Queries the standard input conduit for an character and
;;           | stores its ASCII code in the current tape cell.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Implementation
;; ==============
;; This interpreter's implementation represents an endeavor in the
;; programming language Common Lisp, the Hair source code's execution
;; principle a per saltum evaluation adhibited onto its characters.
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-02-11
;; 
;; Sources:
;;   [esolang:2025:Hair]
;;   The Esolang contributors, "Hair", December 31st, 2025
;;   URL: "https://esolangs.org/wiki/Hair"
;;   
;;   [goodrich:2006:datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 120--127 describe the doubly linked list.
;;       o The page 120 presents an implementation of a doubly linked
;;         list node in Java, norned "DNode".
;;       o The pages 125--127 present an implementation of a doubly
;;         linked list in Java.
;;     
;;     - The pages 213--216 describe the double-ended queue, or deque,
;;       abstract data type (ADT).
;;       o The pages 215--216 present a partial implementation in Java
;;         utilizing a doubly linked list.
;;     
;;     - The pages 231-241 describe the node list abstract data type
;;       (ADT).
;;       o This data type utilizes the notion of "positions" in order
;;         to furnish an abstraction of nodes for its elements' access.
;;       o The pages 234--235 describe an interface for the node list
;;         ADT, nevened "PositionList".
;;       o The page 235 mentions the equivalency of the node list
;;         operations and the deque counterparts.
;;       o The pages 236--241 present an implementation of the node list
;;         ADT via a doubly linked list, the product being yclept the
;;         "NodePositionList".
;;   
;;   [goodrich:2014:datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 132--137 describe the concept and an implementation
;;       of the doubly linked list in the Java programming language.
;;       o The pages 135--137 furnish the implementation.
;;     
;;     - The pages 248--251 describe the concept and implementation of
;;       the double-ended queue, or deque, abstract data type (ADT).
;;       o The pages 250--251 describe the implementation of a deque via
;;         a doubly linked list.
;;     
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a linked list whose componency ensues
   from an arbitrary mountenance of members, each such adhering to the
   ELEMENT-TYPE, for thilk is imposed the default configuration
   involving the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each such a twissel of a key, complying with the
   KEY-TYPE, and an associated value of the VALUE-TYPE, for both species
   is imposed the default configuration assigning the generic sentinel
   ``*''."
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

(deftype position-vector ()
  "The ``position-vector'' type defines an adjustable vector of indices,
   everichon among these members a ``fixnum'' position designator into
   the ensconcing Hair program code."
  '(vector fixnum *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a unilateral association atwixen
   from a forward jump point to zero or more back jump instructions,
   both species mediated per procurationem of their zero-based indices
   into the ensconcing Hair source code string."
  '(hash-table-of fixnum position-vector))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   (8) attiguous bits, thus engendering a plasmature which complies to
   the closed integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table generator.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-a-dynamic-position-vector ()
  "Creates and returns an initially vacant, adjustable vector of
   indices, represented by ``fixnum'' values."
  (the position-vector
    (make-array 0
      :element-type    'fixnum
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))

;;; -------------------------------------------------------

(defun register-the-back-jump-point (forward-jump-points
                                     back-jump-point)
  "Adds the BACK-JUMP-POINT index as a destination to all of the
   FORWARD-JUMP-POINTS and returns no value."
  (declare (type (list-of position-vector) forward-jump-points))
  (declare (type fixnum                    back-jump-point))
  (dolist (forward-jump-point forward-jump-points)
    (declare (type position-vector forward-jump-point))
    (vector-push-extend back-jump-point forward-jump-point))
  (values))

;;; -------------------------------------------------------

(defun determine-whether-all-jump-points-are-matched (jump-table)
  "Determines whether the JUMP-TABLE's forward jump points, represented
   by its keys, affiliate with at least one back jump position,
   returning on confirmation the unmodified JUMP-TABLE itself;
   otherwise, an error of an unspecified type is signaled."
  (declare (type jump-table jump-table))
  (the jump-table
    (loop
      for forward-jump-point
        of-type fixnum
        being the hash-keys in jump-table
      using
        (hash-value back-jump-points)
      
      when (zerop (fill-pointer back-jump-points)) do
        (error "The forward jump point (\"[\") at the position ~d ~
                lacks any succeeding back jump point (\"[\")."
          forward-jump-point)
      end
      
      finally
        (return jump-table))))

;;; -------------------------------------------------------

(defun contex-the-jump-points (source)
  "Establishes the vincula from the forward jump points to the back jump
   instructions in the piece of Hair SOURCE code and returns a fresh
   jump table concredited with their castaldy, mapping the \"[\" token's
   zero-based indices to a dynamic vector of all succeeding \"]\"
   instructions' positions."
  (declare (type string source))
  (the jump-table
    (loop
      with jump-table
        of-type jump-table
        =       (make-hash-table :test #'eql)
      with forward-jump-points
        of-type (list-of position-vector)
        =       NIL
      
      for current-token
        of-type character
        across  source
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (char= current-token #\[) do
        (let ((new-forward-jump-point
                (prepare-a-dynamic-position-vector)))
          (declare (type position-vector new-forward-jump-point))
          (setf (gethash current-position jump-table)
                new-forward-jump-point)
          (push new-forward-jump-point forward-jump-points))
      else if (char= current-token #\]) do
        (if forward-jump-points
          (register-the-back-jump-point
            forward-jump-points
            current-position)
          (error "The back jump instruction \"]\" at the position ~d ~
                  does not affiliate with any prevenient \"[\" token."
            current-position))
      end
      
      finally
        (return
          (determine-whether-all-jump-points-are-matched jump-table)))))

;;; -------------------------------------------------------

(defun look-up-the-jump-destinations-for (jump-table forward-jump-point)
  "Request the back jump points affiliated with the FORWARD-JUMP-POINT
   in the JUMP-TABLE and returns two values:
     (1) An adjustable vector comprehending the back jump points as
         ``fixnum'' objects.
     (2) The tally of back jump points partaking of the aboon mentioned
         vector (see -> (1))."
  (declare (type jump-table jump-table))
  (declare (type fixnum     forward-jump-point))
  (the (values position-vector fixnum)
    (let ((destinations
            (or (gethash forward-jump-point jump-table)
                (error "No back jump points are associated with the ~
                        position ~d."
                  forward-jump-point))))
      (declare (type position-vector destinations))
      (values destinations
        (fill-pointer destinations)))))

;;; -------------------------------------------------------

(defun select-a-random-back-jump-point (jump-table forward-jump-point)
  "Selects and returns in an aleatory fashion one of the back jump
   points allied with the FORWARD-JUMP-POINT in the JUMP-TABLE."
  (declare (type jump-table jump-table))
  (declare (type fixnum     forward-jump-point))
  (the fixnum
    (multiple-value-bind (available-destinations number-of-destinations)
        (look-up-the-jump-destinations-for
          jump-table
          forward-jump-point)
      (declare (type position-vector available-destinations))
      (declare (type fixnum          number-of-destinations))
      (aref available-destinations
        (random number-of-destinations)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory's tape cell.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor prepare-a-fresh-cell (predecessor successor)))
  "The ``Cell'' class applies itself to the encapsulation of a tape
   cell's notion as a doubly linked list node, the designment of its
   diorism a tripartite componency enumerating the ensconced unsigned
   byte datum, as well as an optional pointer to the predecessor and
   an equivalent reference to the successor cells."
  (value       0   :type octet          :read-only NIL)
  (predecessor NIL :type (or null Cell) :read-only NIL)
  (successor   NIL :type (or null Cell) :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory's tape.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((header
    :initform      (prepare-a-fresh-cell NIL NIL)
    :type          Cell
    :documentation "The header sentinel, which serves as the constant
                    leftmost node in the doubly linked list, so as to
                    filsen in the insertion of new cells.")
   (trailer
    :initform      (prepare-a-fresh-cell NIL NIL)
    :type          Cell
    :documentation "The trailer sentinel, which serves as the constant
                    rightmost node in the doubly linked lsit, so as to
                    filsen in the insertion of new cells.")
   (pointer
    :initform      (prepare-a-fresh-cell NIL NIL)
    :type          Cell
    :documentation "The cell pointer, thilk references the currently
                    active node in the doubly linked list."))
  (:documentation
    "The ``Tape'' class furnishes the Hair memory's moeity entalented
     with a paravaunt mete of potency, its conformation such of a catena
     whose mountenance does not wist of any natural bournes along both
     lateralities, and inwith whose compass each cell bears a capacity
     of a single unsigned byte scalar, a specialized integral object
     desumed from the closed interval [0, 255], from the membership
     edified by this infinite multitude a cell pointer, motatorious in
     its haecceity, acquires the wike of the contemporaneously selected
     unit's designation, the aefauld specimen at any instant invested
     with an amenability to perquisitions and modulations in its state.
     ---
     This particular tape implementation is established upon a doubly
     linked list's firmament, its contextured nodes nevened, via a
     veridicous euonymy, the \"cells\" in this circumstance."))

;;; ------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Establishes the contexture atwixen the TAPE's header, trailer, and
   pointer cells, and returns no value."
  (declare (type Tape tape))
  (with-slots (header trailer pointer) tape
    (declare (type Cell header))
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (psetf
      (cell-successor   header)  pointer
      (cell-predecessor pointer) header
      (cell-successor   pointer) trailer
      (cell-predecessor trailer) pointer))
  (values))

;;; ------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'', at its inchoacy empight in the
   default state of zero-valued cells."
  (the Tape
    (make-instance 'Tape)))

;;; ------------------------------------------------------

(defun insert-a-new-cell-atwixen (predecessor successor)
  "Inserts a fresh cell atwixen the PREDECESSOR and SUCCESSOR units,
   alligates their vincula, and returns the thus produced new cell."
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (let ((new-cell (prepare-a-fresh-cell predecessor successor)))
    (declare (type Cell new-cell))
    (psetf
      (cell-successor   predecessor) new-cell
      (cell-predecessor successor)   new-cell)
    (the Cell new-cell)))

;;; ------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step in the sinistral airt
   and returns no value."
  (declare (type Tape tape))
  (with-slots (header pointer) tape
    (declare (type Cell header))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-predecessor pointer) header)
        (insert-a-new-cell-atwixen header pointer)
        (cell-predecessor pointer))))
  (values))

;;; ------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step in the dextral airt and
   returns no value."
  (declare (type Tape tape))
  (with-slots (trailer pointer) tape
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-successor pointer) trailer)
        (insert-a-new-cell-atwixen pointer trailer)
        (cell-successor pointer))))
  (values))

;;; ------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the value stored in the TAPE's currently selected cell."
  (declare (type Tape tape))
  (the octet
    (cell-value
      (slot-value tape 'pointer))))

;;; ------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceding the transfer by a wrapping of the value into
   the unsigned byte range [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf (cell-value pointer)
      (mod new-value 256)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory's accumulator.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Accumulator ()
  ((state
    :initform      0
    :type          octet
    :documentation "The contemporaneous accumulator state."))
  (:documentation
    "The ``Accumulator'' class serves in the implementation of the Hair
     memory's accumulator component, a parhedral salvatory nuncupated
     to the castaldy of a scalar unsigned byte value, thilk limns an
     incolant of the closed integer interval [0, 255]."))

;;; ------------------------------------------------------

(defun prepare-a-pristine-accumulator ()
  "Creates and returns a fresh ``Accumulator'', at its inchoacy empight
   in the default state of entailing a zero-valued datum."
  (the Accumulator
    (make-instance 'Accumulator)))

;;; ------------------------------------------------------

(defun accumulator-state (accumulator)
  "Returns the ACCUMULATOR's contemporaneous byte state."
  (declare (type Accumulator accumulator))
  (the octet
    (slot-value accumulator 'state)))

;;; ------------------------------------------------------

(defun (setf accumulator-state) (new-state accumulator)
  "Stores the NEW-STATE in the ACCUMULATOR, contingently preceding the
   transaction by a wrapping of the input into the unsigned byte range
   [0, 255], and returns no value."
  (declare (type integer     new-state))
  (declare (type Accumulator accumulator))
  (setf (slot-value accumulator 'state)
    (mod new-state 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the random number generator.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 1 *) *maximum-random-integer*))

;;; -------------------------------------------------------

(defparameter *maximum-random-integer* 100
  "The ``*maximum-random-integer*'' global variable specifies the
   inclusive largest positive integer number whose random selection is
   administered a homologation.")

;;; -------------------------------------------------------

(defun select-a-random-positive-integer ()
  "Returns an aleatorily chosen positive integer number greater than or
   equal to one (1)."
  (the (integer 1 *)
    (1+ (random *maximum-random-integer*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Hair interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *random-state* (make-random-state T))

;;; -------------------------------------------------------

(defun interpret-the-hair-code (code)
  "Interprets the piece of Hair source CODE and returns no value."
  (declare (type string code))
  (let ((ip                  0)
        (jump-table          (contex-the-jump-points code))
        (forward-jump-points NIL)
        (tape                (prepare-a-pristine-tape))
        (accumulator         (prepare-a-pristine-accumulator)))
    (declare (type fixnum           ip))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (declare (type Tape             tape))
    (declare (type Accumulator      accumulator))
    
    (loop while (array-in-bounds-p code ip) do
      (case (char code ip)
        (#\(
          (loop repeat (select-a-random-positive-integer) do
            (move-the-cell-pointer-right tape)))
        
        (#\)
          (loop repeat (select-a-random-positive-integer) do
            (move-the-cell-pointer-left tape)))
        
        (#\/
          (incf (current-cell-value tape)))
        
        (#\\
          (decf (current-cell-value tape)))
        
        (#\{
          (incf (accumulator-state accumulator)))
        
        (#\}
          (decf (accumulator-state accumulator)))
        
        (#\|
          (setf (current-cell-value tape)
            (accumulator-state accumulator)))
        
        (#\-
          (setf (accumulator-state accumulator)
            (current-cell-value tape)))
        
        (#\[
          (push ip forward-jump-points)
          (when (/= (current-cell-value tape)
                    (accumulator-state  accumulator))
            (pop forward-jump-points)
            (setf ip (select-a-random-back-jump-point jump-table ip))))
        
        (#\]
          (if forward-jump-points
            (setf ip (1- (pop forward-jump-points)))
            (error "No forward jump point exists for the \"]\" at the ~
                    position ~d to return to."
              ip)))
        
        (#\=
          (format *query-io* "~c"
            (code-char
              (current-cell-value tape))))
        
        (#\~
          (format        *query-io* "~&>> ")
          (finish-output *query-io*)
          (setf (current-cell-value tape)
            (char-code
              (read-char *query-io* NIL #\Null)))
          (clear-input *query-io*))
        
        
        (otherwise
          NIL))
      
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output conduit.
(interpret-the-hair-code
  "/////////////////////////////////////////////////////////////////////
   ///=
   
   (////////////////////////////////////////////////////////////////////
   /////////////////////////////////=
   
   (////////////////////////////////////////////////////////////////////
   ////////////////////////////////////////==
   
   (////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////=
   
   (////////////////////////////////////////////=
   
   (////////////////////////////////=
   
   (////////////////////////////////////////////////////////////////////
   ///////////////////=
   
   (////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////=
   
   (////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////=
   
   (////////////////////////////////////////////////////////////////////
   ////////////////////////////////////////=
   
   (////////////////////////////////////////////////////////////////////
   ////////////////////////////////=
   
   (/////////////////////////////////=")

;;; -------------------------------------------------------

;; A repeating cat program whose perpetuation follows an olamic
;; principle, detached from the input's choice.
(interpret-the-hair-code "~-[=~-]")

;;; -------------------------------------------------------

;; Randomly either select the number zero (0) or one (1) and print
;; thilk.
(interpret-the-hair-code
  "////////////////////////////////////////////////[]/[]=")

;;; -------------------------------------------------------

;; Randomly generate and print a catena compact of zero (0) and one (1)
;; bits, its length itself subject to an aleatory choice comprehending
;; one or more elements.
(interpret-the-hair-code
  "[////////////////////////////////////////////////[]/[]=)]=")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-hair-code
  "~{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{[=]=")
