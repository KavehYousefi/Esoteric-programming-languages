;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "searchfuck", invented by the Esolang user "CappyIsCrappy"
;; and presented on May 25th, 2022, its dioristic contribution a new
;; guise for Urban Mueller's "brainfuck" programming language, expressed
;; in popular term sougth after with the Google search machine.
;; 
;; 
;; Concept
;; =======
;; The searchfuck programming language, subsumed into the species of
;; trivial brainfuck substitutions, their paravaunt discrepancy to the
;; entheus the application of a different dictionary to relate to the
;; octuple operative features, educed its kenspeckle designment from a
;; set of popular search terms committed to the Google search engine.
;; 
;; == THE MEMORY: AN INFINITE DISPANSION OF UNSIGNED BYTES ==
;; Appropriated from its brainfuck heritage, searchfuck deploys a memory
;; defined in terms of a bilaterally infinite tape of unsigned byte
;; values, each such ensconced in a cell.
;; 
;; The cell policy's foundry appertains to a wrapping mode, where a
;; transgression along any of the integral range's inclusive bournes,
;; measuring the interval [0, 255], instigates a return from the
;; obverse post.
;; 
;; Empight upon the tape, a dedicated cursor, the "cell pointer", serves
;; in the designation of the currently active cell, the same at any
;; instant is entalented with the exclusive amenability to perquisitions
;; and modulations. This cursor's motile nature capacitates its gradual
;; traversal athwart both axes in order to alter the selection.
;; 
;; 
;; Syntax
;; ======
;; Its kenspeckle indicium, the syntactical reformulation of brainfuck's
;; instruction set, imposes a renovated species of nomothesia, the
;; stipulation the requisites of whitespaces betwixt tokens as a
;; segregating agent.
;; 
;; 
;; Instructions
;; ============
;; searchfuck's cleronomy perpetuates into the operative aspects'
;; bailiwick, the language limining an equipollent of its entheus in
;; cardinality and functionality, enumerating an octuple membership
;; which permits the memory's perquisition and modulation, its cell
;; pointer's translation, input and output issuance, as well as a
;; jump-based control flow governance mechanism.
;; 
;; == OVERVIEW ==
;; A cursory mete of nortelry's adhibition shall comprise the following
;; apercu's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command      | Effect
;;   -------------+----------------------------------------------------
;;   youtube      | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   facebook     | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   whatsapp web | Increments the current cell value by one (1). If
;;                | the new state transcends the admissible upper
;;                | bourne of 255, the value wraps around to the lower
;;                | extremum of zero (0).
;;   ..................................................................
;;   google       | Decrements the current cell value by one (1). If
;;                | the new state violates the lower march of zero (0),
;;                | the value wraps around to the upper extremum of
;;                | 255.
;;   ..................................................................
;;   gmail        | Prints the character whose ASCII code corresponds
;;                | to the current cell value to the standard output.
;;   ..................................................................
;;   amazon       | Queries the standard input for a character and
;;                | stores its ASCII code in the current cell.
;;   ..................................................................
;;   translate    | If the current cell contains zero (0), moves the
;;                | instruction pointer (IP) forward to the position
;;                | immediately succeeding the matching "traductor"
;;                | instruction; otherwise advances as usual.
;;   ..................................................................
;;   traductor    | If the current cel does not contain (zero), moves
;;                | the instruction pointer (IP) back to the position
;;                | immediately succeeding the matching "translate"
;;                | instruction; otherwise advances as usual.
;;   ------------------------------------------------------------------
;; 
;; == SEARCHFUCK AND BRAINFUCK ==
;; Founded upon the two languages' perfect conflation, a contraposition
;; anent the sole signum, the instructions' diction, shall be adduced:
;; 
;;   ------------------------
;;   searchfuck   | brainfuck
;;   -------------+----------
;;   youtube      | >
;;   ........................
;;   facebook     | <
;;   ........................
;;   whatsapp web | +
;;   ........................
;;   google       | -
;;   ........................
;;   gmail        | .
;;   ........................
;;   amazon       | ,
;;   ........................
;;   translate    | [
;;   ........................
;;   traductor    | ]
;;   ------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an exercise in Common
;; Lisp, prevenient to the execution stage being apposted an intermede
;; that produces from the source code string a vector of representative
;; instruction objects.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-18
;; 
;; Sources:
;;   [esolang2023searchfuck]
;;   The Esolang contribtors, "searchfuck", December 8th, 2023
;;   URL: "https://esolangs.org/wiki/Searchfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each siccan an adherent of the ELEMENT-TYPE, for which is
   specified the generic sentinel ``*'' as a default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (and (symbolp element-type)
                   (eq      element-type '*))
              (every
                #'(lambda (current-element)
                    (declare (type T current-element))
                    (typep current-element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each such a composition of a key conforming to the
   KEY-TYPE and an associated value of the VALUE-TYPE, for both holds
   the default of the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (or
              (and (symbolp key-type)
                   (eq      key-type   '*)
                   (symbolp value-type)
                   (eq      value-type '*))
              (loop
                for current-key
                  of-type T
                  being the hash-keys in (the hash-table candidate)
                using
                  (hash-value current-value)
                always
                  (and (typep current-key   key-type)
                       (typep current-value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   searchfuck operations."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable searchfuck program as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirection mapping betwixt jump
   points in a searchfuck program, mediated by adminiculum of their
   zero-based indices into the instruction vector yielded during the
   source code's parsing stratum, and manifested in a hash table whose
   fixnum keys and values contribute the jump positions."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bit, thus forming a occupant of the closed integral range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   amplectation of this diorism amounting to, among others, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, returns ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   inside whose circumference are amplected the space, horizontal tab,
   and newline specimens, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-is-empty-p (candidate)
  "Determines whether the CANDIDATE represents an empty string, that is,
   one of the length zero, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type string candidate))
  (the boolean
    (get-boolean-value-of
      (zerop
        (length candidate)))))

;;; -------------------------------------------------------

(defun default-clause-key-p (clause-key)
  "Determines whether the CLAUSE-KEY designates the default or fallback
   clause key, signified either by the symbol \"T\" or \"otherwise\",
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type (or string symbol) clause-key))
  (the boolean
    (get-boolean-value-of
      (and (symbolp clause-key)
           (or (eq clause-key 'T)
               (eq clause-key 'otherwise))))))

;;; -------------------------------------------------------

(defmacro string-case (key-form &rest clauses)
  "Evaluates the KEY-FORM, expecting to yield a string object, probes
   the CLAUSES, each such a list composed of one or more forms, the
   first contributing the clause key to match against the evaluated
   KEY-FORM, the optional remaining comprising the consequent actions'
   forms, and returns the first matching clause's desinent form's
   results.
   ---
   Every member of the CLAUSES list ought to establish a list with at
   least one element, the first among these construed as the clause key,
   the following the clause forms. The clause key must either yield a
   string object intended for its equiparation with the KEY-FORM, or,
   alternatively and at the desinent position of the CLAUSES, either of
   the sentinel symbols ``T'' or ``otherwise'', thilk serve in the
   signification of the default or fallback selection, matching in the
   case of any prevenience's failure."
  (let ((evaluated-subject (gensym)))
    (declare (type symbol evaluated-subject))
    `(let ((,evaluated-subject ,key-form))
       (declare (type string ,evaluated-subject))
       (cond
         ,@(loop
             for current-clause
               of-type list
               in      clauses
             collect
               (destructuring-bind (clause-key &rest clause-forms)
                   current-clause
                 (declare (type (or string symbol) clause-key))
                 (declare (type list               clause-forms))
                 (if (default-clause-key-p clause-key)
                   `(T
                      ,@clause-forms)
                   `((string= ,clause-key ,evaluated-subject)
                      ,@clause-forms))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns from the list of INSTRUCTIONS a fresh ``program''
   representation."
  (declare (type list instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string tokenizer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tokenizer
  (:constructor make-tokenizer (source)))
  "The ``Tokenizer'' class establishes an entity entalented with that
   dever which appertains to the segregation of a piece of searchfuck
   source code, committed in a string format, into a stream of words,
   their sepiments specified in whitespace characters."
  (source   (error "No source for the tokenizer specified.")
            :type      string
            :read-only T)
  (position 0
            :type      fixnum
            :read-only NIL))

;;; -------------------------------------------------------

(defun get-source-length (tokenizer)
  "Returns the tally of characters comprising the TOKENIZER's source."
  (declare (type Tokenizer tokenizer))
  (the fixnum
    (length
      (tokenizer-source tokenizer))))

;;; -------------------------------------------------------

(defun source-is-exhausted-p (tokenizer)
  "Determines whether the TOKENIZER's source has been traversed in its
   entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tokenizer tokenizer))
  (the boolean
    (get-boolean-value-of
      (>= (tokenizer-position tokenizer)
          (get-source-length  tokenizer)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   skips a sequence of zero or more accolent whitespaces and returns no
   value."
  (declare (type Tokenizer tokenizer))
  (setf (tokenizer-position tokenizer)
    (or (position-if-not #'whitespace-character-p
          (tokenizer-source tokenizer)
          :start (tokenizer-position tokenizer))
        (get-source-length tokenizer)))
  (values))

;;; -------------------------------------------------------

(defun move-to-end-of-current-word (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source, and
   expecting to be commorant inwith a token's marches, moves the
   TOKENIZER's position cursor to the index immediately succeeding the
   contemporaneously processed word's occupied tmema and returns this
   new position."
  (declare (type Tokenizer tokenizer))
  (setf (tokenizer-position tokenizer)
    (or (position-if #'whitespace-character-p
          (tokenizer-source tokenizer)
          :start (tokenizer-position tokenizer))
        (get-source-length tokenizer)))
  (the fixnum
    (tokenizer-position tokenizer)))

;;; -------------------------------------------------------

(defun get-next-word (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   locates the nearest following word and returns thilk as a fresh
   string.
   ---
   Upon its source's exhaustion, the TOKENIZER responds to any request
   with a fresh empty string."
  (declare (type Tokenizer tokenizer))
  (skip-whitespaces tokenizer)
  (the string
    (or (and (source-is-exhausted-p tokenizer)
             "")
        (subseq
          (tokenizer-source            tokenizer)
          (tokenizer-position          tokenizer)
          (move-to-end-of-current-word tokenizer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser
    (tokenizer
     &aux (current-word
            (get-next-word tokenizer)))))
  "The ``Parser'' class furnishes an entity invested with the capacity
   to assemble a searchfuck program from a stream of its source code's
   words."
  (tokenizer    (error "No tokenizer specified for the parser.")
                :type      Tokenizer
                :read-only T)
  (current-word (error "No current word commorant in the parser.")
                :type      string
                :read-only NIL))

;;; -------------------------------------------------------

(defun load-next-word (parser)
  "Queries the next word from the PARSER's underlying tokenizer, stores
   thilk in the PARSER, and returns this new word."
  (declare (type Parser parser))
  (setf (parser-current-word parser)
    (get-next-word
      (parser-tokenizer parser)))
  (the string
    (parser-current-word parser)))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Parses a searchfuck operation in the PARSER's context and either
   an ``instruction'' representation thereof, or, upon its
   disrespondency, responds with ``NIL''."
  (declare (type Parser parser))
  (the (or null instruction)
    (string-case (parser-current-word parser)
      ("youtube"
        (prog1 :move-right
          (load-next-word parser)))
      ("facebook"
        (prog1 :move-left
          (load-next-word parser)))
      ("whatsapp"
        (and (string= (load-next-word parser) "web")
             :increment))
      ("google"
        (prog1 :decrement
          (load-next-word parser)))
      ("gmail"
        (prog1 :output
          (load-next-word parser)))
      ("amazon"
        (prog1 :input
          (load-next-word parser)))
      ("translate"
        (prog1 :jump-forward
          (load-next-word parser)))
      ("traductor"
        (prog1 :jump-back
          (load-next-word parser)))
      (otherwise
        (load-next-word parser)
        NIL))))

;;; -------------------------------------------------------

(defun all-words-have-been-processed-p (parser)
  "Determines whether all tokens from the PARSER's underlying tokenizer
   have been consumed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Parser parser))
  (the boolean
    (string-is-empty-p
      (parser-current-word parser))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a searchfuck program utilizing the PARSER's context and
   returns a ``program'' representation thereof."
  (declare (type Parser parser))
  (the program
    (make-program
      (loop
        until
          (all-words-have-been-processed-p parser)
        for current-instruction
          of-type (or null instruction)
          =       (parse-instruction parser)
        when current-instruction
          collect current-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-jump-table ()
  "Creates and returns a fresh, initially vacant ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun get-destination-jump-point (jump-table point-of-departure)
  "Returns the zero-based jump endpoint affiliated with the
   POINT-OF-DEPARTURE in the JUMP-TABLE, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No destination associated with the jump point ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the START-POINT and the END-POINT in a bilateral fashion in
   the JUMP-TABLE and returns no value.
   ---
   Any entry whose key concurs with either the START-POINT or END-POINT
   will be tacitly overwritten by the new pairing."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point jump-table) end-point
    (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table-for (program)
  "Generates and returns a fresh ``jump-table'' which ligates the
   corresponding jump end points of the searchfuck PROGRAM by
   adminiculum of their zero-based indices into the same."
  (declare (type program program))
  (let ((jump-table          (prepare-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for current-instruction
        of-type instruction
        across  program
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (eq current-instruction :jump-forward) do
        (push current-position forward-jump-points)
      else if (eq current-instruction :jump-back) do
        (if forward-jump-points
          (connect-jump-points jump-table
            (pop forward-jump-points)
            current-position)
          (error "Unmatched back jump point at position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor prepare-program-memory ()))
  "The ``Memory'' class applies itself to the implementation of the
   searchfuck program memory, its diorism conflating with a bilaterally
   infinite dispansion of unsigned byte-valued cells, operated upon by
   a mobile cell poiner which at any instant designates the currently
   amenable unit among these."
  (bits                    #b0 :type (unsigned-byte *) :read-only NIL)
  (pointer                 0   :type (integer * *)     :read-only NIL)
  (smallest-selected-index 0   :type (integer * 0)     :read-only NIL)
  (largest-selected-index  0   :type (integer 0 *)     :read-only NIL))

;;; -------------------------------------------------------

(defun translate-cell-index-to-bit-offset (memory cell-index)
  "Returns for the signed integer CELL-INDEX the corresponding
   non-negative offset into the MEMORY's integer-encoded bit sequence
   that designates the affiliated cell's inchoation."
  (declare (type Memory  memory))
  (declare (type integer cell-index))
  (the (integer 0 *)
    (* (- cell-index
          (memory-smallest-selected-index memory))
       8)))

;;; -------------------------------------------------------

(defun get-byte-location-for-cell-at (memory cell-index)
  "Returns a byte location, compact of the twissel indicating the
   non-negative offset into the MEMORY's bit sequence and the extent,
   always resolving to an octuple dispansion, which in champarty
   designate the MEMORY cell amenable to the signed integer CELL-INDEX.
   ---
   Please heed the implementation-dependent nature of the byte specifier
   type in the Common Lisp standard."
  (declare (type Memory  memory))
  (declare (type integer cell-index))
  (the T
    (byte 8
      (translate-cell-index-to-bit-offset memory cell-index))))

;;; -------------------------------------------------------

(defun get-cell-value-at (memory index)
  "Returns the unsigned byte value stored in the MEMORY cell amenable
   to the signed integer INDEX."
  (declare (type Memory memory))
  (the octet
    (ldb
      (get-byte-location-for-cell-at memory index)
      (memory-bits                   memory))))

;;; -------------------------------------------------------

(defun translate-cell-pointer-to-bit-offset (memory)
  "Returns for the MEMORY's cell pointer the corresponding non-negative
   offset into the MEMORY's integer-encoded bit sequence that designates
   the affiliated cell's inchoation."
  (declare (type Memory  memory))
  (the (integer 0 *)
    (translate-cell-index-to-bit-offset memory
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun get-byte-location-for-current-cell (memory)
  "Returns a byte location, compact of the twissel indicating the
   non-negative offset into the MEMORY's bit sequence and the extent,
   always resolving to an octuple dispansion, which in champarty
   designate the MEMORY cell pointer's selected cell.
   ---
   Please heed the implementation-dependent nature of the byte specifier
   type in the Common Lisp standard."
  (declare (type Memory memory))
  (the T
    (byte 8
      (translate-cell-pointer-to-bit-offset memory))))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the unsigned byte value stored in the MEMORY's currently
   selected cell."
  (declare (type Memory memory))
  (the octet
    (ldb
      (get-byte-location-for-current-cell memory)
      (memory-bits                        memory))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's currently selected cell,
   contingently preceded by a wrapping of its state into the admissible
   unsigned byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (ldb
      (get-byte-location-for-current-cell memory)
      (memory-bits                        memory))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun update-bounding-cell-indices (memory)
  "Updates the MEMORY's smallest and largest selected cell index
   designators in response to the contemporaneous cell pointer position
   and returns no value."
  (declare (type Memory memory))
  (psetf
    (memory-smallest-selected-index memory)
      (min (memory-smallest-selected-index memory)
        (memory-pointer memory))
    (memory-largest-selected-index memory)
      (max (memory-largest-selected-index memory)
        (memory-pointer memory)))
  (values))

;;; -------------------------------------------------------

(defun shift-memory-bits-if-necessary (memory)
  "Determines whether the MEMORY's cell pointer has been translated
   beyond the smallest cell index yet traversed, on confirmation
   shifting its bits dextrally by eight places, in any case returning
   no value."
  (declare (type Memory memory))
  (when (< (memory-pointer                 memory)
           (memory-smallest-selected-index memory))
    (setf (memory-bits memory)
      (ash (memory-bits memory) 8)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (shift-memory-bits-if-necessary memory)
  (update-bounding-cell-indices   memory)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (update-bounding-cell-indices memory)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (loop
    initially
      (format stream "Memory:")
    for current-cell-index
      of-type integer
      from    (memory-smallest-selected-index memory)
      to      (memory-largest-selected-index  memory)
    do
      (format stream "~&~2t[~d] = ~d" current-cell-index
        (get-cell-value-at memory current-cell-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-searchfuck-program (program)
  "Interprets the parsed searchfuck PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (build-jump-table-for program))
        (memory     (prepare-program-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    
    (symbol-macrolet
        ((program-is-complete-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length program)))))
         (current-instruction
          (the instruction
            (aref program ip)))
         (current-cell-value
          (the (or octet integer)
            (current-cell-value memory))))
      (declare (type boolean            program-is-complete-p))
      (declare (type instruction        current-instruction))
      (declare (type (or octet integer) current-cell-value))
      
      (loop until program-is-complete-p do
        (case current-instruction
          (:move-right
            (move-cell-pointer-right memory))
          
          (:move-left
            (move-cell-pointer-left memory))
          
          (:increment
            (incf current-cell-value))
          
          (:decrement
            (decf current-cell-value))
          
          (:output
            (format T "~c"
              (code-char current-cell-value)))
          
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf current-cell-value
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          
          (:jump-forward
            (when (zerop current-cell-value)
              (setf ip
                (get-destination-jump-point jump-table ip))))
          
          (:jump-back
            (unless (zerop current-cell-value)
              (setf ip
                (get-destination-jump-point jump-table ip))))
          
          (otherwise
            (error "Invalid instruction ~s at position ~d."
              current-instruction ip)))
        
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-searchfuck (code)
  "Interprets the piece of searchfuck source CODE and returns no value."
  (declare (type string code))
  (interpret-searchfuck-program
    (parse-program
      (make-parser
        (make-tokenizer code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to searchfuck.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-searchfuck (brainfuck-code
                                          &key (destination NIL))
  "Translate the piece of BRAINFUCK-CODE to an equivalent searchfuck
   program and writes the resulting code to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a
   ``NIL'' DESTINATION, responds with a fresh string comprehending the
   output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for current-brainfuck-token
          of-type character
          across  brainfuck-code
        do
          (format destination "~&~a"
            (case current-brainfuck-token
              (#\>       "youtube")
              (#\<       "facebook")
              (#\+       "whatsapp web")
              (#\-       "google")
              (#\.       "gmail")
              (#\,       "amazon")
              (#\[       "translate")
              (#\]       "traductor")
              (otherwise ""))))
      (with-output-to-string (searchfuck-code)
        (declare (type string-stream searchfuck-code))
        (translate-brainfuck-to-searchfuck brainfuck-code
          :destination searchfuck-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output.
(interpret-searchfuck
  "
  whatsapp web
  translate
  google
  google
  youtube
  google
  translate
  youtube
  youtube
  whatsapp web
  youtube
  google
  google
  google
  google
  google
  facebook
  facebook
  traductor
  facebook
  google
  google
  facebook
  google
  google
  google
  traductor
  youtube
  google
  gmail
  youtube
  youtube
  youtube
  whatsapp web
  gmail
  youtube
  youtube
  gmail
  gmail
  whatsapp web
  whatsapp web
  whatsapp web
  translate
  gmail
  youtube
  traductor
  facebook
  facebook
  facebook
  facebook
  gmail
  whatsapp web
  whatsapp web
  whatsapp web
  gmail
  google
  google
  google
  google
  google
  google
  gmail
  facebook
  facebook
  google
  gmail
  youtube
  youtube
  youtube
  youtube
  whatsapp web
  gmail
  ")

;;; -------------------------------------------------------

;; Convert a brainfuck program which prints the message "Hello, World!"
;; into the corresponding searchfuck tantamount and execute the latter.
(interpret-searchfuck
  (translate-brainfuck-to-searchfuck
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-searchfuck "amazon translate gmail amazon traductor")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-searchfuck
  "
  amazon
  gmail
  translate
  google
  google
  youtube
  whatsapp web
  translate
  youtube
  youtube
  traductor
  facebook
  translate
  gmail
  traductor
  facebook
  facebook
  traductor
  ")
