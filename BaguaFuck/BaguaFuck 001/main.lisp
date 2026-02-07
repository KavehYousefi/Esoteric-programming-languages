;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BaguaFuck", siclike current under the alternative
;; agnomination "☰☱☲☳☴☵☶☷Fuck", invented by the Esolang user
;; Steve Abel, also known by the account name "PrySigneToFry", and
;; presented on May 5th, 2024, the diorism's adhibition such as to
;; produce a derivation from Urban Mueller's "brainfuck" in a mete of
;; lealty thilk only divorces anenst the syntactical hyle, where the
;; novel language peracts a cambistry of the original ASCII symbols for
;; the ogdoad of Chinese names engaged in an affiliation with the Taoist
;; "bagua".
;; 
;; 
;; Concept
;; =======
;; The BaguaFuck programming language establishes a tantamount of
;; brainfuck, the congruency's merist solely emerges in the diction,
;; whose parcery of ASCII identifiers is cauped for the ogdoad of
;; Chinese names nevening the Taoist "bagua" concepts.
;; 
;; == BAGUAFUCK = BAGUA + BRAINFUCK ==
;; A zealot in its lealty to brainfuck's original nomothesia,
;; BaguaFuck's aefauld dioristic deviation wones in the instruction
;; identifiers' supersession by the Chinese names corresponding to the
;; trigrams of the Taoist "bagua", the concrete glyph set chosen from
;; the simplified variant of the tongue.
;; 
;; == THE ARCHITECTURE: AN INFINITE DISPANSION OF UNSIGNED BYTES ==
;; Pursuing the ejusdem generic nomothesia in nearly all aspects of its
;; brainfuck cleronomy, BaguaFuck's memory model complies to the
;; selfsame rules, the data castaldy's consignment limns a bilaterally
;; dispansion of cells, adight in a catena of units whose entreparted
;; dimensum is delineated by a scalar unsigned byte datum.
;; 
;; Occupying the closed integer interval of [0, 255], upon any bournes'
;; transgression a cell's state wraps around from the violated to the
;; overthwart extremum.
;; 
;; Operating upon this conformation, a mobile cell pointer homologates
;; a stillatim traversal athwart the tape's entirety.
;; 
;; 
;; Instructions
;; ============
;; BaguaFuck's instruction set constitutes a conceptual ipsissima verba
;; appropriation from its brainfuck entheus, the octuple identifiers,
;; however, experience a cambistry's actuation for terms desumed from
;; the Chinese tongue.
;; 
;; == OVERVIEW ==
;; The following apercu's wike shall be satisfied in the BaguaFuck
;; operations' ostention:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   乾       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   兑       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   离       | Increments the current cell value by one (1). Upon the
;;           | the upper mere's transgression, which is meted at 255,
;;           | the cell state wraps around to the lower extremum of
;;           | zero (0).
;;   ..................................................................
;;   震       | Decrements the current cell value by one (1). Upon the
;;           | lower mere's transgression, which is meted at zero (0),
;;           | the state wraps around to the upper extremum of 255.
;;   ..................................................................
;;   巽       | Prints the character whose ASCII code concurs with the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   坎       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ..................................................................
;;   艮       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "坤" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   坤       | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "艮" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BAGUAFUCK, BAGUA, AND BRAINFUCK ==
;; The bagua's paravaunt involvement in the language's donat shall be
;; manifested into the following tabular tendance, the Chinese names
;; partaking of an equiparation with the representative "I Ching"
;; trigram and the brainfuck equivalency.
;; 
;; Please heed that those Chinese glyphs to whom BaguaFuck assigns the
;; simplified notation shall be ostended in their traditional
;; designment's compernage by mediation of a parenthesized prosthesis.
;; 
;;   ----------------------------------------------------------
;;   Name      | Name    | Trigram | brainfuck | Causatum
;;   (Chinese) | (Latin) |         | symbol    |
;;   ----------+---------+---------+-----------+---------------
;;   乾         | qián    | ☰       | >         | Move right
;;   ..........................................................
;;   兑 (兌)     | duì     | ☱       | <         | Move left
;;   ..........................................................
;;   离 (離)     | lí      | ☲       | +         | Increment
;;   ..........................................................
;;   震         | zhèn    | ☳       | -         | Decrement
;;   ..........................................................
;;   巽         | xùn     | ☴       | .         | Output
;;   ..........................................................
;;   坎         | kǎn     | ☵       | ,         | Input
;;   ..........................................................
;;   艮         | gèn     | ☶       | [         | Jump forward
;;   ..........................................................
;;   坤         | kūn     | ☷       | ]         | Jump back
;;   ----------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter, implemented in the programming language Common
;; Lisp, accompasses its telos' stipulations by a conversion of the
;; source code string into a dimerous compound of the character and the
;; contingent jump destination index, the latter airted at the twain of
;; control flow behests, "艮" and "坤", ere the tokens' procession in
;; order for the program to execute.
;; 
;; The interpretation process as an entire notion bewrays its complex
;; designment in a treble tier architecture:
;; 
;;   (1) CONVERSION INTO SYMBOL-JUMP-POINT TUPLES
;;       Each character partaking of the BaguaFuck source string
;;       experiences a transformation into a tuple of particular
;;       componency, the sinistral compartment of which applies itself
;;       to the character's castaldy, irregardless of its actual
;;       efficacy; while the dextral moeity's dever is realized in the
;;       specification of a contingent jump destination, in the case of
;;       a forward ("艮") or back jump ("坤") instruction, intended for
;;       a subsequent definition in the following step (2).
;;       
;;       The jump destination parcel either designates a ``NIL''
;;       sentinel, the same delineates any tuple's incipial
;;       configuration, or, at the latter stage of the jump points'
;;       contexture, the zero-based integral index into the collated
;;       tuple sequence.
;;       
;;       This tier's produce, as a consectary of its investments,
;;       ostends a vector of symbol-jump-point tuples.
;;   
;;   (2) JUMP POINT CONTEXTURE
;;       Each symbol-jump-point tuple affiliated with a forward or back
;;       jump instruction enjoys the conspection of its vinculum to the
;;       respective matching opposite destination point. To this end,
;;       a forward jump point ("艮") receives the zero-based index of
;;       its corresponding back jump tuple ("坤") into the vector, and
;;       vice versa.
;;   
;;   (3) TUPLE VECTOR EXECUTION
;;       Its preservation of the original BaguaFuck characters, the
;;       compass of whose fixature does not wist of the non-operative
;;       constituents' amission's wite, homologates the expected
;;       causata's replication by a gradual procession athwart the
;;       tuples' ensconced symbols.
;;       
;;       The twifold compound's haecceity kithes its status as a
;;       polymechany in the jump instructions, where the dextral tuple
;;       moeity furnishes the destination point's index for a contingent
;;       navigation.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen:2013:lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-31
;; 
;; Sources:
;;   [christensen:2013:lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang:2024:BaguaFuck]
;;   The Esolang contributors, "BaguaFuck", June 6th, 2024
;;   URL: "https://esolangs.org/wiki/BaguaFuck"
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
;;   [risingmoontaichi:2026:bagua]
;;   Rising Moon Tai Chi, "The Bagua", 2026
;;   URL: "https://www.risingmoontaichi.net/the-bagua"
;;   Notes:
;;     - Elucidates the concept of "bagua" and its deployment in Taoism,
;;       also yclept Daoism.
;;   
;;   [wiktionary:2025:bagua]
;;   The Wiktionary contributors, "bagua", November 1st, 2025, at 05:16
;;   URL: "https://en.wiktionary.org/wiki/bagua"
;;   Notes:
;;     - Defines the term "bagua".
;;     - Enumerates the trigrams and their Chinese names in their
;;       traditional and simplified forms.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which, among others, enhalses the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type defines a command in the BaguaFuck programming
   language as a composite of a operative or non-operative character and
   an optional jump destination, its manifestation that of a cons cell
   whose dimidiation assigns to the sinistral moiety the character,
   while the dextral tmema serves in either a ``fixnum'' position
   designator's castaldy, or the ``NIL'' sentinel's absence
   communication."
  '(cons character (or null fixnum)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable BaguaFuck program as a
   one-dimensional simply array composed of zero or more ``command''
   members."
  '(simple-array command (*)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value edified from an
   ogdoad of attiguous bits, thus constituting an incolant of the closed
   integer interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the BaguaFuck program parser.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-baguafuck-code (code)
  "Parses the piece of BaguaFuck source CODE and returns a covenable
   ``program'' representation."
  (declare (type string code))
  (the program
    (map '(simple-array command (*))
      #'(lambda (token)
          (declare (type character token))
          (cons token NIL))
      code)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump point contexture operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contex-the-jump-points (program)
  "Connects the jump command in the BaguaFuck PROGRAM by adminiculum of
   their zero-based indices into the same and returns no value."
  (declare (type program program))
  (let ((forward-jump-points
          (make-array 0
            :element-type    'fixnum
            :initial-element 0
            :adjustable      T
            :fill-pointer    T)))
    (declare (type (vector fixnum *) forward-jump-points))
    (macrolet
        ((memorize-the-current-position ()
          "Pushes the CURRENT-POSITION onto the FORWARD-JUMP-POINTS
           stack's top location and returns no value."
          `(progn
             (vector-push-extend current-position forward-jump-points)
             (values)))
         (pop-the-current-position ()
          "Removes and returns the top element from the
           FORWARD-JUMP-POINTS stack."
          `(the fixnum
             (if (plusp (length forward-jump-points))
               (prog1
                 (aref forward-jump-points
                   (1- (fill-pointer forward-jump-points)))
                 (decf (fill-pointer forward-jump-points)))
               (error "You cannot pop the top element of an empty ~
                       jump point stack."))))
         (command-at (index)
          "Returns the place designating the command at the INDEX into
           the PROGRAM vector."
          `(the command
             (aref program ,index))))
      (loop
        for current-command  of-type Command across program
        for current-position of-type fixnum  from 0 by 1
        do
          (case (car current-command)
            (#\艮
              (memorize-the-current-position))
            (#\坤
              (let ((start-point (pop-the-current-position))
                    (end-point   current-position))
                (declare (type fixnum start-point))
                (declare (type fixnum end-point))
                (psetf
                  (cdr (command-at start-point)) end-point
                  (cdr (command-at end-point))   start-point)))
            (otherwise
              NIL))
        finally
          (when (plusp (length forward-jump-points))
            (error "One or more unmatched forward jump points ~
                    exist.")))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape cell.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor prepare-a-new-cell (&optional (predecessor NIL)
                                              (successor   NIL))))
  "The ``Cell'' class implements a cell into the memory ``Tape'',
   realized as doubly linked list node; as a corollary comprehending
   the unsigned byte-valued datum, as well as a jumelle of optional
   pointers to the predecessor and successor cells."
  (value       0    :type octet           :read-only NIL)
  (predecessor NIL  :type (or null Cell)  :read-only NIL)
  (successor   NIL  :type (or null Cell)  :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((header
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The front node into the tape's doubly linked list,
                    serving as a sentinel, always inclavated at the
                    leftmost position in order to facilitate
                    insertions.")
   (trailer
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "The desinent node into the tape's doubly linked
                    list, serving as a sentinel, always inclavated at
                    the rightmost position in order to facilitate
                    insertions.")
   (pointer
    :initform      (prepare-a-new-cell NIL NIL)
    :type          Cell
    :documentation "A reference to the currently selected cell.
                    ---
                    The cell pointer, albeit motile in its capacitation,
                    will never assume neither the HEADER nor the
                    TRAILER sentinels."))
  (:documentation
    "The ``Tape'' class furnishes an implementation of a bilaterally
     infinite dispansion of unsigned byte-valued cells, operated upon by
     a mobile cell pointer which at any instant designates the currently
     active entity, the sole cell endowed with amenability to
     perquisitions and modulations.
     ---
     This tape implementation's substratum is accommodated by a doubly
     linked list, along both lateralities involving sentinel nodes."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Establishes the vincula atwixen the inchoate node, represented by the
   TAPE's pointer, the header and the trailer sentinels, and returns no
   value."
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

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'' whose state of inchoacy
   incorporates an aefauld, zero-valued cell."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun insert-a-cell-atwixen (predecessor successor)
  "Inserts a fresh cell atwixen the PREDECESSOR and SUCCESSOR nodes, its
   state's configuration compliant with the default plasmature, and
   returns the thus yielded cell."
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (the Cell
    (let ((new-cell (prepare-a-new-cell predecessor successor)))
      (declare (type Cell new-cell))
      (psetf
        (cell-successor   predecessor) new-cell
        (cell-predecessor successor)   new-cell)
      new-cell)))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Relocates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (with-slots (header pointer) tape
    (declare (type Cell header))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-predecessor pointer) header)
        (insert-a-cell-atwixen header pointer)
        (cell-predecessor pointer))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Relocates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (with-slots (trailer pointer) tape
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (setf pointer
      (if (eq (cell-successor pointer) trailer)
        (insert-a-cell-atwixen pointer trailer)
        (cell-successor pointer))))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (cell-value
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping into the valid unsigned byte
   range of [0, 255], and returns no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf (cell-value pointer)
      (mod new-value 256)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the BaguaFuck interpreter.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-baguafuck-program (program)
  "Executes the BaguaFuck PROGRAM and returns no value."
  (declare (type program program))
  (contex-the-jump-points program)
  (let ((ip   0)
        (tape (prepare-a-pristine-tape)))
    (declare (type fixnum ip))
    (declare (type Tape   tape))
    (loop while (< ip (length program)) do
      (destructuring-bind (token . jump-destination)
          (aref program ip)
        (declare (type character        token))
        (declare (type (or null fixnum) jump-destination))
        (declare (ignorable             jump-destination))
        (case token
          (#\乾
            (move-the-cell-pointer-right tape))
          
          (#\兑
            (move-the-cell-pointer-left tape))
          
          (#\离
            (incf (current-cell-value tape)))
          
          (#\震
            (decf (current-cell-value tape)))
          
          (#\巽
            (format *query-io* "~c"
              (code-char
                (current-cell-value tape))))
          
          (#\坎
            (format *query-io* "~&>> ")
            (finish-output *query-io*)
            (setf (current-cell-value tape)
              (char-code
                (read-char *query-io* NIL #\Null)))
            (clear-input *query-io*))
          
          (#\艮
            (when (zerop (current-cell-value tape))
              (setf ip jump-destination)))
          
          (#\坤
            (unless (zerop (current-cell-value tape))
              (setf ip jump-destination)))
          
          (otherwise
            NIL))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-baguafuck-code (code)
  "Interprets the piece of BaguaFuck source CODE and returns no value."
  (declare (type string code))
  (execute-the-baguafuck-program
    (parse-the-baguafuck-code code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the identifier tables.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string      8) +BAGUAFUCK-IDENTIFIERS+))
(declaim (type (simple-base-string 8) +BRAINFUCK-IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +BAGUAFUCK-IDENTIFIERS+
  "乾兑离震巽坎艮坤"
  "The ``+BAGUAFUCK-IDENTIFIERS+'' global constant maps the BaguaFuck
   identifiers by adminiculum of their zero-based indices into the
   string, and through a stringent lealty's compliance in the canonical
   order, to the tantamount brainfuck symbols.")

(defparameter +BRAINFUCK-IDENTIFIERS+
  (coerce "><+-.,[]" 'simple-base-string)
  "The ``+BRAINFUCK-IDENTIFIERS+'' global constant maps the brainfuck
   identifiers by adminiculum fo their zero-based indices into the
   string, and through a stringent lealty's compliance in the canonical
   order, to the tantamount BaguaFuck symbols.")

;;; -------------------------------------------------------

(defun look-up-the-baguafuck-symbol (brainfuck-symbol)
  "Returns the BaguaFuck identifier corresponding to the
   BRAINFUCK-SYMBOL, or responds with ``NIL'' upon an owelty's carency."
  (declare (type character brainfuck-symbol))
  (the (or null character)
    (let ((opcode
            (position brainfuck-symbol +BRAINFUCK-IDENTIFIERS+
              :test #'char=)))
      (declare (type (or null fixnum) opcode))
      (and opcode
           (schar +BAGUAFUCK-IDENTIFIERS+ opcode)))))

;;; -------------------------------------------------------

(defun look-up-the-brainfuck-symbol (baguafuck-symbol)
  "Returns the brainuck identifier corresponding to the
   BAGUAFUCK-SYMBOL, or responds with ``NIL'' upon an owelty's carency."
  (declare (type character baguafuck-symbol))
  (the (or null standard-char)
    (let ((opcode
            (position baguafuck-symbol +BAGUAFUCK-IDENTIFIERS+
              :test #'char=)))
      (declare (type (or null fixnum) opcode))
      (and opcode
           (schar +BRAINFUCK-IDENTIFIERS+ opcode)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the converter from brainfuck to BaguaFuck. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-the-brainfuck-code-into-baguafuck
    (brainfuck-code
     &optional (destination NIL))
  "Transcripts the piece of BRAINFUCK-CODE into an equivalent BaguaFuck
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' object; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for brainfuck-symbol
          of-type character
          across  brainfuck-code
        for baguafuck-symbol
          of-type (or null character)
          =       (look-up-the-baguafuck-symbol brainfuck-symbol)
        when baguafuck-symbol do
          (format destination "~c" baguafuck-symbol))
      (with-output-to-string (baguafuck-code)
        (declare (type string-stream baguafuck-code))
        (convert-the-brainfuck-code-into-baguafuck
          brainfuck-code
          baguafuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the converter from BaguaFuck to brainfuck. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-the-baguafuck-code-to-brainfuck
    (baguafuck-code
     &optional (destination NIL))
  "Transcripts the piece of BAGUAFUCK-CODE into an equivalent brainfuck
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' object; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the result."
  (declare (type string      baguafuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for baguafuck-symbol
          of-type character
          across  baguafuck-code
        for brainfuck-symbol
          of-type (or null standard-char)
          =       (look-up-the-brainfuck-symbol baguafuck-symbol)
        when brainfuck-symbol do
          (format destination "~c" brainfuck-symbol))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (convert-the-baguafuck-code-to-brainfuck
          baguafuck-code
          brainfuck-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!", concluded by a linebreak.
(interpret-the-baguafuck-code
  "离离离离离离艮乾离离离离离离离离离
   离离离兑震坤乾巽兑离离离离离离离艮
   乾离离离离兑震坤乾离巽离离离离离离
   离巽巽离离离巽兑离离离离离离艮乾震
   震震震震震震震震震震兑震坤乾震巽震
   震震震震震震震震震震震巽兑离离离离
   离离离离离离离艮乾离离离离离兑震坤
   乾巽兑离离离艮乾离离离离离离离离兑
   震坤乾巽离离离巽震震震震震震巽震震
   震震震震震震巽兑离离离离离离艮乾震
   震震震震震震震震震震兑震坤乾震巽乾
   离离离离离离离离离离离离离巽兑兑兑")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-baguafuck-code "坎艮巽坎坤")
