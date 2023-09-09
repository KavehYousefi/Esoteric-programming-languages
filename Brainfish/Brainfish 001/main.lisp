;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainfish", invented by the Esolang user "Cinnamony" and
;; presented on June 19th, 2023, the kenspeckle property of which is
;; realized in the consilience of two extant language species, Urban
;; Mueller's "brainfuck" and Jonathan Todd Skinner's "Deadfish", the
;; conformity of which is expressed in single-token commands, while
;; their tryst begets an infinite tape of unbounded integer cells,
;; operated upon in a repeatedly executing interpretation loop.
;; 
;; 
;; Concept
;; =======
;; The Brainfish programming language's haecceity is designed as the
;; ultimity of two distinct entities' synartesis: that of brainfuck and
;; Deadfish --- a conjugality that informs all aspects of the progeny,
;; syntaxis, instructions, and architecture.
;; 
;; == INTERPRETATION PROCEEDS IN CYCLES ==
;; The interpreter moils in an iteration, querying at each cycle's
;; inchoation the standard input for a sequence of commands, the same
;; are evaluated, until a completely empty response terminates the
;; session.
;; 
;; 
;; Architecture
;; ============
;; The brainfuck and Deafish champarty's exhibition from the operational
;; and syntactical departments perpetuates in the architectural
;; concepts, as Brainfish appropriates the former's infinite tape of
;; cells --- yet admitted to an arbitrary integer range ---, compounded
;; with the cell pointer, and offers it to both parties' imperatives'
;; avail.
;; 
;; == THE MEMORY: AN INFINITE EXPANSE OF CELLS ==
;; The brainfuck tape's infinite extent of cells, linear in their
;; ordonnance, along both axes governs the overall componency.
;; 
;; == THE CELLS: SIGNED INTEGER STORAGES ==
;; While lealty to the scalar constraint locates every cell partially
;; in brainfuck's realm, the otherward heritage, that of Deadfish,
;; liberates its circumference from the traditional unsigned byte range
;; to an arbitrary signed integer space.
;; 
;; A corollary yielded by this novel influence, the same parentage's
;; diorism that mandates the accumulator's relapse from the sentinels
;; -1 and 256 governs the cell's haecceity.
;; 
;; Any cell assumes, at the program's inchoation, the default value of
;; zero (0).
;; 
;; == THE CELL POINTER: A MOBILE CELL SELECTOR ==
;; Again desumed from brainfuck, a cell pointer maintains at any instant
;; during the program's execution the currently active unit, whose
;; amenability to perquisitions and alterations capacitates the data
;; castaldy.
;; 
;; The cell pointer's responsiveness to certain commands homologates its
;; gradual sinistral and dextral locomotion along the infinite tape.
;; 
;; 
;; Data Types
;; ==========
;; Brainfish's type system bifurcates into the species of unbounded
;; signed integers, paravaunt in their significance by bestraddling all
;; aspects, from arithmetics, through input and output facilities, to
;; control flow, and the parhedral ASCII character type, the same
;; operates exclusively along the communication conduits.
;; 
;; 
;; Instructions
;; ============
;; A usufructuary from a bivial reception avenue, brainfuck's and
;; Deadfish's, Brainfish's cleronomy with respect to its operational
;; warklumes enhalse in their amplectation a decimal account of
;; paraphernalia, the slight preponderance, a sextuple, the former
;; parentage's dation, whereas the latter quadruple ensues from the
;; Deadfish lineage.
;; 
;; Non-command tokens do not inflict the program with an error, instead
;; issuing a newline output.
;; 
;; == OVERVIEW ==
;; The wike of a cursory nortelry's adhibition with respect to the
;; granted commands shall be the following table's onus:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   >       | Moves the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer forward to the position immediately
;;           | succeeding the matching "]" command. Otherwise proceeds
;;           | as usual.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer back to the position immediately
;;           | succeeding the matching "[" command. Otherwise proceeds
;;           | as usual.
;;           |---------------------------------------------------------
;;           | This command contributes a verbatim appropriation from
;;           | brainfuck.
;;   ..................................................................
;;   i       | Increments the current cell value by one (1).
;;           | If the new value equals 256 following this command's
;;           | invocation, resets the cell to zero (0).
;;           |---------------------------------------------------------
;;           | This command contributes a modified appropriation from
;;           | Deadfish.
;;   ..................................................................
;;   d       | Decrements the current cell value by one (1).
;;           |---------------------------------------------------------
;;           | If the new value equals -1 following this command's
;;           | invocation, resets the cell to zero (0).
;;           |---------------------------------------------------------
;;           | This command contributes a modified appropriation from
;;           | Deadfish.
;;   ..................................................................
;;   s       | Squares the current cell value.
;;           |---------------------------------------------------------
;;           | If the new value equals 256 following this command's
;;           | invocation, resets the cell to zero (0).
;;           |---------------------------------------------------------
;;           | This command contributes a modified appropriation from
;;           | Deadfish.
;;   ..................................................................
;;   o       | Prints the numeric value stored in the current cell in
;;           | its verbatim form to the standard output.
;;           |---------------------------------------------------------
;;           | This command contributes a modified appropriation from
;;           | Deadfish.
;;   ..................................................................
;; 
;; == BRAINFISH, BRAINFUCK, AND DEADFISH ==
;; The vinculum betwixt Brainfish and its parentage, fulfilled by the
;; nuptials of brainfuck and Deadfish, shall now be a juxtaposing
;; equiparation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Provenance | Apostille
;;   --------+------------+--------------------------------------------
;;   ,       | brainfuck  | -
;;   ..................................................................
;;   .       | brainfuck  | -
;;   ..................................................................
;;   <       | brainfuck  | -
;;   ..................................................................
;;   >       | brainfuck  | -
;;   ..................................................................
;;   [       | brainfuck  | -
;;   ..................................................................
;;   ]       | brainfuck  | -
;;   ..................................................................
;;   d       | Deadfish   | Operates on the current cell, as is the
;;           |            | wont with brainfuck, in lieu of Deadfish's,
;;           |            | here missing, accumulator.
;;   ..................................................................
;;   i       | Deadfish   | Operates on the current cell, as is the
;;           |            | wont with brainfuck, in lieu of Deadfish's,
;;           |            | here missing, accumulator.
;;   ..................................................................
;;   o       | Deadfish   | Operates on the current cell, as is the
;;           |            | wont with brainfuck, in lieu of Deadfish's,
;;           |            | here missing, accumulator.
;;   ..................................................................
;;   s       | Deadfish   | Operates on the current cell, as is the
;;           |            | wont with brainfuck, in lieu of Deadfish's,
;;           |            | here missing, accumulator.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Brainfish, as a consequence of its bivial descendancy, is encumbered
;; with several elements of ambivalent characteristics, the most peisant
;; members therefrom shall be extricated for further elucidation.
;; 
;; == WHICH OUTPUT OPERATION(S) EXIST? ==
;; In regards to the output facilities, Brainfish's protolog tholes from
;; an antilogy, on one hand stating that, besides the tokens "[", "]",
;; "<", ">", and ",", no operations from brainfuck are emulated, which
;; also ostracizes the output mechanism ".", while Deadfish's complete
;; roster, including, of course, its output facility "o", participates;
;; on the other hand, the example twain exposes both printing commands,
;; "." and "o".
;; 
;; This antinome's provenance retains its status as a commorant to
;; caligation, but might very well emanate from a lapsus on the
;; language's original author.
;; 
;; It has been adjudged, in the face of the examples' requisita and the
;; impotence inflicted upon by any of the two alternatives' lacunae, to
;; resort to both participants' admission as a tenable imputation. The
;; "." command from brainfuck, hence, outputs a character, whereas
;; Deadfish's "o" displays the verbatim numeric cell form.
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in the programming language
;; Common Lisp, employing a rather peculiar approach in its castaldy of
;; an operation table, the same affiliates with the recognized command
;; tokens a function, the "handler", that accompasses the expected
;; response to its invocation; reserving a dedicated instance for such
;; characters abstinent from any causata.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-28
;; 
;; Sources:
;;   [esolang2023Brainfish]
;;   The Esolang contributors, "Brainfish<' June 19th, 2023
;;   URL: "https://esolangs.org/wiki/Brainfish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, the keys of which conform to the KEY-TYPE and associate
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   which conform to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump positions
   in a piece of Brainfish code to the respective back jump locations,
   and vice versa, the same's reification limned by a hash table from
   fixnum keys to fixnum values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse vector of cells, amenable to
   integer indices, whose capacity each is quantified by a scalar
   integer datum."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype operator ()
  "The ``operator'' type defines a function intended to imbue a token in
   the Brainfish programming language with a computation effect, reified
   in the form of a function which embraces a ``Context'' object to
   perquire and manipulate, returning no value."
  '(function (Context) (values)))

;;; -------------------------------------------------------

(deftype operator-table ()
  "The ``operator-table'' maps functional objects to command tokens, its
   incarnation chosen as a hash table that affiliates characters with
   ``operator'' instances."
  '(hash-table-of character operator))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (code)
  "Generates and returns for the piece of Brainfish source CODE a jump
   table which associates the forward jump positions into the same with
   that of thei matching back jumps, and vice versa."
  (declare (type string code))
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type character across code
      and position of-type fixnum    from   0 by 1
      if (char= command #\[) do
        (push position forward-jump-points)
      else if (char= command #\]) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched back jump point at position ~d." position))
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun jump-table-get-destination (jump-table position)
  "Returns the jump point opposite to the POSITION in the JUMP-TABLE, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     position))
  (the fixnum
    (or (gethash position jump-table)
        (error "No destination associated with the jump position ~d."
          position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class occupies the wike of a Brainfish's program
   memory, the design of which designs the same as a bilaterally
   infinite account of integer-valued cells, its currently active
   instance designated by a mobile pointer."
  (cells   (make-hash-table :test #'eql) :type cell-map)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the current MEMORY cell value."
  (declare (type Memory memory))
  (the integer
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, respecting the
   peculiarity commorant in Deadfish to reset a value of -1 or 256 by
   the default of zero (0), and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    (if (or (= new-value -1) (= new-value 256))
      0
      new-value))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the current MEMORY cell by one and returns no value.
   ---
   Please note that, in concord with Deadfish's dioristic behavior, the
   acquisition of a value equal to -1 or 256 will reset the cell to the
   default value of zero (0)."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the current MEMORY cell by one and returns no value.
   ---
   Please note that, in concord with Deadfish's dioristic behavior, the
   acquisition of a value equal to -1 or 256 will reset the cell to the
   default value of zero (0)."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-square (memory)
  "Squares the MEMORY's current cell value and returns no value.
   ---
   Please note that, in concord with Deadfish's dioristic behavior, the
   acquisition of a value equal to -1 or 256 will reset the cell to the
   default value of zero (0)."
  (declare (type Memory memory))
  (symbol-macrolet ((current-cell
                      (the integer
                        (memory-current-cell memory))))
    (setf current-cell
          (* current-cell current-cell)))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns no
   value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns no
   value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Context".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Context
  (:constructor make-context
                  (code
                   &aux (jump-table (compute-jump-table code)))))
  "The ``Context'' class encapsulates the state information requisite
   for the operation of an interpreter."
  (code       (error "Missing code.")       :type string)
  (ip         0                             :type fixnum)
  (jump-table (error "Missing jump table.") :type jump-table)
  (memory     (make-memory)                 :type Memory))

;;; -------------------------------------------------------

(defun context-current-token (context)
  "Returns the character located at the CONTEXT's instruction pointer
   (IP) position, or ``NIL'' if the same has transcended its code's
   bournes."
  (declare (type Context context))
  (the (or null character)
    (char (context-code context)
      (context-ip context))))

;;; -------------------------------------------------------

(defun context-exhausted-p (context)
  "Determines whether the CONTEXT's instruction pointer (IP) has passed
   its code's desinence, signifying the Brainfish program's termination,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Context context))
  (the boolean
    (not (null
      (>= (context-ip context)
          (length (context-code context)))))))

;;; -------------------------------------------------------

(defun context-set-code (context new-code)
  "Sets the CONTEXT's code to the NEW-CODE, resets its state, and
   returns no value."
  (declare (type Context context))
  (declare (type string  new-code))
  (setf (context-code       context) new-code)
  (setf (context-jump-table context)
        (compute-jump-table new-code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of operator table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type operator-table +OPERATORS+))
(declaim (type operator       +NOP-OPERATOR+))

;;; -------------------------------------------------------

(defparameter +OPERATORS+
  (make-hash-table :test #'eql)
  "Associates with command tokens the functions responsible for their
   evaluation.")

(defparameter +NOP-OPERATOR+
  #'(lambda (context)
      (declare (type Context context))
      (declare (ignore       context))
      (terpri))
  "A function which handles tokens not affiliated with any effect.")

;;; -------------------------------------------------------

(flet ((register-command (identifier operator)
        "Associates the OPERATOR with the IDENTIFIER in the +OPERATORS+
         table, supplanted any previous extant correspondence for the
         latter, and returns no value."
        (declare (type character identifier))
        (declare (type operator  operator))
        (setf (gethash identifier +OPERATORS+) operator)
        (values)))
  
  (register-command #\>
    #'(lambda (context)
        (declare (type Context context))
        (memory-move-right
          (context-memory context))
        (values)))
  
  (register-command #\<
    #'(lambda (context)
        (declare (type Context context))
        (memory-move-left
          (context-memory context))
        (values)))
  
  (register-command #\,
    #'(lambda (context)
        (declare (type Context context))
        (format T "~&>> ")
        (force-output)
        (setf (memory-current-cell (context-memory context))
              (char-code (read-char)))
        (clear-input)
        (values)))
  
  (register-command #\.
    #'(lambda (context)
        (declare (type Context context))
        (format T "~c"
          (code-char
            (memory-current-cell
              (context-memory context))))
        (values)))
  
  (register-command #\[
    #'(lambda (context)
        (declare (type Context context))
        (when (zerop (memory-current-cell
                       (context-memory context)))
          (setf (context-ip context)
            (jump-table-get-destination
              (context-jump-table context)
              (context-ip         context))))
        (values)))
  
  (register-command #\]
    #'(lambda (context)
        (declare (type Context context))
        (unless (zerop (memory-current-cell
                         (context-memory context)))
          (setf (context-ip context)
            (jump-table-get-destination
              (context-jump-table context)
              (context-ip         context))))
        (values)))
  
  (register-command #\d
    #'(lambda (context)
        (declare (type Context context))
        (memory-decrement
          (context-memory context))
        (values)))
  
  (register-command #\i
    #'(lambda (context)
        (declare (type Context context))
        (memory-increment
          (context-memory context))
        (values)))
  
  (register-command #\s
    #'(lambda (context)
        (declare (type Context context))
        (memory-square
          (context-memory context))
        (values)))
  
  (register-command #\o
    #'(lambda (context)
        (declare (type Context context))
        (format T " ~d"
          (memory-current-cell
            (context-memory context)))
        (values)))
  
  (values))

;;; -------------------------------------------------------

(defun get-operator (identifier)
  "Returns the operator associated with the IDENTIFIER."
  (declare (type character identifier))
  (the function
    (gethash identifier +OPERATORS+ +NOP-OPERATOR+)))

;;; -------------------------------------------------------

(defun apply-operator (operator context)
  "Applies the OPERATOR to the CONTEXT and returns no value."
  (declare (type operator operator))
  (declare (type Context  context))
  (funcall operator context)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-context (context)
  "Interprets the Brainfish program governed by the CONTEXT's castaldy
   and returns no value."
  (declare (type Context context))
  (loop until (context-exhausted-p context) do
    (apply-operator
      (get-operator
        (context-current-token context))
      context)
    (incf (context-ip context)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Brainfish (&optional (initial-code ""))
  "Launches the Brainfish interpreter, optionally executing the
   INITIAL-CODE, and repeatedly quering for input to evaluated, until a
   completely empty line is committed, finally returning no value.
   ---
   Please heed that for the interpreter to cease, the input line must
   be utterly empty, that is, devoid even of spaces."
  (declare (type string initial-code))
  (let ((context (make-context initial-code)))
    (declare (type Context context))
    (loop
      initially
        (interpret-context context)
      for input-code
        of-type string
        =       (progn
                  (format T "~&>> ")
                  (finish-output)
                  (read-line NIL NIL ""))
      while
        (plusp (length input-code))
      do
        (clear-input)
        (context-set-code context input-code)
        (interpret-context context)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hi".
(interpret-Brainfish ">>>iiiiiiiiii[<<<iiiii[>ii<d][d]>[d>i<]>>d]<iiii.i.")

;;; -------------------------------------------------------

;; One-time cat program which accepts a character, but prints its ASCII
;; code.
(interpret-Brainfish ",o")

;;; -------------------------------------------------------

;; One-time cat program which accepts a character and prints the same.
(interpret-Brainfish ",.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on an input of the
;; null character.
(interpret-Brainfish "i[,.]")
