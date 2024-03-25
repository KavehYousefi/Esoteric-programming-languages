;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "tinyBF", invented by the Esolang user "Bataais" and
;; presented on November 10th, 2014, its haecceity founded upon a
;; more compendious variation on Urban Mueller's "brainfuck", the
;; curtailed syntaxis begotten by a "direction" switch which assigns to
;; many instruction tokens two discrepant causata.
;; 
;; 
;; Concept
;; =======
;; The tinyBF programming language commits to a curtailment of
;; brainfuck's octuple instruction set to a sextuple reformulation,
;; its novel compendiousness' provenance a "direction" mode, capable of
;; an identifier's twyforked interpretation in obverse causalities.
;; 
;; == THE DIRECTION DETERMINES THE OPERATION ==
;; A bifurcation of a preponderance among its operation token's causata
;; governs the language, conjoined in the token's potential for
;; discrepancy with a "direction": a state either "positive" or
;; "negative".
;; 
;; Assuming the former as its default, a command token, upon its
;; collision with the program evaluating routine, generates the
;; respective choice from its two selections. The direction switch
;; command, its signification begotten by the "=" character, imposes the
;; aefauld operative symbol embued a confluence of both responses to a
;; singular effect, namely the direction's vault-face.
;; 
;; Please heed that the terminology of "direction" does not propagate
;; into the realm of the program code's procession, and as such the
;; lexical analyzer or code execution facility always retains the
;; sinistrodextral ilk of airt.
;; 
;; == PARTICULAR FORBISENS REPLICATE LACKING INSTRUCTIONS ==
;; The quadruple instruction set offered by tinyBF, a triad among which
;; engages in brainfuck's sextuple equivalency's verbatim mimicry, also
;; accommodates the direction switch as its desinent member; by this
;; diorism, ultimately, the input and output facilities are excluded
;; from an immediate acquistion.
;; 
;; In lieu of the traditional one-symbol identifiers, several special
;; cases limn the lacunae's melioration.
;; 
;; The otioseness commorant in a twifold direction switching, via "==",
;; has been reapproriated as a sentinel of the output operation,
;; legitimate in both modes.
;; 
;; The input conduit's involvement ensues from the lost sensibility of
;; empty loops in brainfuck, that is, the character sequence "[]", such
;; reverberates in the positive tinyBF direction as "|=|", and in the
;; negative by "=|=|"; the respective cases instead poll the input
;; channel.
;; 
;; == THE MEMORY: A TAPE OF INFINITE SIGNED INTEGERS ==
;; A counterdistinguished from its predecessor's common restriction to
;; unsigned bytes, tinyBF employs a bilaterally infinite tape of signed
;; integers, obeying to no natural bournes along either axis.
;; 
;; A cell pointer, empight on the first cell at the program's
;; inchoation, designates at any point in the program the currently
;; active unit, amenable to perquisitions and modulations. The motile
;; nature inherent to the pointer capacitates its gradual translation
;; along the tape in both airts.
;; 
;; 
;; Instructions
;; ============
;; The tinyBF programming language's competences replicates its
;; brainfuck entheus' equinumerant simulacrum via a quadruple of
;; symbol in champarty with a mode, stevened the "direction".
;; 
;; == OVERVIEW ==
;; The following tabulation's dever shall be assumed in order to convey
;; a basic nortelry anenst the language's operative features:
;; 
;;   ------------------------------------------------------------------
;;   Command |              Effect when direction is ...
;;           | positive                   | negative
;;   --------+----------------------------+----------------------------
;;   =       | Switches the direction.    | Switches the direction.
;;   ..................................................................
;;   +       | Increments the current     | Decrements the current cell
;;           | cell value by one.         | value by one.
;;   ..................................................................
;;   >       | Moves the cell pointer one | Moves the cell pointer one
;;           | step to the right.         | step to the left.
;;   ..................................................................
;;   |       | If the current cell value  | If the current cell value
;;           | equals zero (0), moves the | does not equal zero (0),
;;           | instruction pointer (IP)   | moves the instruction
;;           | forward to the position    | pointer (IP) back to the
;;           | immediately succeeding the | position immediately
;;           | matching "|" token in a    | succeeding the matching "|"
;;           | negative context.          | token in a positive
;;           | Otherwise proceeds as      | context. Otherwise proceeds
;;           | usual.                     | as usual.
;;   ..................................................................
;;   ==      | Prints the character whose | Prints the character whose
;;           | ASCII code corresponds to  | ASCII code corresponds to
;;           | the current cell value to  | the current cell value to
;;           | the standard output.       | the standard output.
;;   ..................................................................
;;   |=|     | Queries the standard input | None. Please see the "=|=|"
;;           | for a character and stores | command immediately below.
;;           | its ASCII code in the      | 
;;           | current cell.              | 
;;   ..................................................................
;;   =|=|    | None. Please see the "|=|" | Queries the standard input
;;           | command immediately aboon. | for a character and stores
;;           |                            | its ASCII code in the
;;           |                            | current cell.
;;   ------------------------------------------------------------------
;; 
;; A rearrangement of the instruction table, with a cynosure's
;; adhibition to the telos, disranking the signification to a paravail
;; echolon, shall be provided alow:
;; 
;;   ------------------------------------------------------------------
;;   Effect       |       Direction 
;;                | Positive  | Negative
;;   -------------+-----------+----------------------------------------
;;   Switch       | =         | =
;;   ..................................................................
;;   Increment    | +         | none
;;   ..................................................................
;;   Decrement    | none      | +
;;   ..................................................................
;;   Move right   | >         | none
;;   ..................................................................
;;   Move left    | none      | >
;;   ..................................................................
;;   Jump forward | |         | none
;;   ..................................................................
;;   Jump back    | none      | |
;;   ..................................................................
;;   Output       | ==        | ==
;;   ..................................................................
;;   Input        | |=|       | =|=|
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation has materialized in the programming language
;; Common Lisp, employing a prevenient parsing stage, which transcripts
;; the recognized operative symbols to instruction objects, to the
;; ultimity designated by the interpretation effort.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-25
;; 
;; Sources:
;;   [esolang2022tinyBF]
;;   The Esolang contributors, "tinyBF", January 19th, 2022
;;   URL: "https://esolangs.org/wiki/TinyBF"
;;   
;;   [esolang2022tinyBFDiscussion]
;;   The Esolang contributors, "Talk:TinyBF", January 19th, 2022
;;   URL: "https://esolangs.org/wiki/Talk:TinyBF"
;;   Notes:
;;     - Talk page regarding "tinyBF", comprehending further
;;       intelligence and elucidations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized tinyBF instruction
   variants."
  '(member
    :switch-direction
    :increment
    :decrement
    :move-right
    :move-left
    :input
    :output
    :jump-forward
    :jump-back
    :nop
    :eof))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized selection of airts
   into which the instruction pointer may navigate."
  '(member :positive :negative))

;;; -------------------------------------------------------

(deftype stack-of (&key (element-type T))
  "The ``stack-of'' type defines a list-based stack whose componency
   enumerates an arbitrary tally of elements, each such a member of the
   ELEMENT-TYPE, defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&key (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   tallies an arbitrary cardinality in membership of entries, complying
   in their keys to the KEY-TYPE and in their values to the VALUE-TYPE,
   for both holds the default comprehensive ``T''."
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

(deftype program ()
  "The ``program'' type defines a tinyBF program as a one-dimensinoal
   simple array of ``command'' objects."
  '(simple-array command (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping betwixt forward and back
   jump points in a tinyBF program, mediated by the positions into its
   operation sequence."
  '(hash-table-of :key-type fixnum :value-type fixnum))

;;; -------------------------------------------------------

(deftype cell-vector ()
  "The ``cell-vector'' type defines a sparse vector of integer-valued
   cells, amenable to unbounded signed integer indices, and entalented
   with siccan capacity as to render an equiponderant estate to the
   subscripts' range, realized as a hash table compact of signed integer
   keys and values."
  '(hash-table-of :key-type T :value-type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-opposite-direction (current-direction)
  (:documentation
    "Returns the direction obverse to the CURRENT-DIRECTION.")
  
  (:method ((current-direction (eql :positive)))
    (declare (type direction current-direction))
    (the  direction :negative))
  
  (:method ((current-direction (eql :negative)))
    (declare (type direction current-direction))
    (the  direction :positive)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a command identifier or
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "=+>|" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-non-command-content (source)
  "Returns a new simple string which represents the SOURCE purged of any
   non-operative content."
  (declare (type string source))
  (the simple-string
    (coerce
      (remove-if-not #'command-character-p source)
      'simple-string)))

;;; -------------------------------------------------------

(defun token-starts-at-p (source start expected-token)
  "Determines whether, proceeding from the START position into the
   SOURCE, the subsequent characters replicate the EXPECTED-TOKEN,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (declare (type simple-string expected-token))
  (the boolean
    (not (null
      (string= source expected-token
        :start1 start
        :end1   (min (+ start (length expected-token))
                     (length source)))))))

;;; -------------------------------------------------------

(defun get-next-token (source start direction)
  "Proceeding from the START position into the SOURCE, and the imputed
   DIRECTION, extracts the token commencing at this location and returns
   two values:
     (1) The token commencing at the START position into the SOURCE.
     (2) The position into the SOURCE immediately succeeding that parcel
         occupied by the extracted token."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (declare (type direction     direction))
  (the (values command fixnum)
    (cond
      ;; End of SOURCE.
      ((not (array-in-bounds-p source start))
        (values :eof start))
      
      ;; Output.
      ((token-starts-at-p source start "==")
        (values :output (+ start 2)))
      
      ;; Input in positive direction.
      ((and (eq direction :positive)
            (token-starts-at-p source start "|=|"))
        (values :input (+ start 3)))
      
      ;; Input in negative direction.
      ((and (eq direction :negative)
            (token-starts-at-p source start "=|=|"))
        (values :input (+ start 4)))
      
      ;; Switch direction.
      ((token-starts-at-p source start "=")
        (values :switch-direction (1+ start)))
      
      ;; Increment current cell.
      ((and (eq direction :positive)
            (token-starts-at-p source start "+"))
        (values :increment (1+ start)))
      
      ;; Decrement current cell.
      ((and (eq direction :negative)
            (token-starts-at-p source start "+"))
        (values :decrement (1+ start)))
      
      ;; Move cell pointer right.
      ((and (eq direction :positive)
            (token-starts-at-p source start ">"))
        (values :move-right (1+ start)))
      
      ;; Move cell pointer left.
      ((and (eq direction :negative)
            (token-starts-at-p source start ">"))
        (values :move-left (1+ start)))
      
      ;; Jump forward if zero.
      ((and (eq direction :positive)
            (token-starts-at-p source start "|"))
        (values :jump-forward (1+ start)))
      
      ;; Jump back if zero.
      ((and (eq direction :negative)
            (token-starts-at-p source start "|"))
        (values :jump-back (1+ start)))
      
      ;; No command token.
      (T
        (values :nop (1+ start))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (source)
  "Extracts from the piece of tinyBF SOURCE code a sequence of its
   instructions and returns these in a one-dimensional array of
   ``command'' objects."
  (declare (type simple-string source))
  (let ((position  0)
        (direction :positive))
    (declare (type fixnum    position))
    (declare (type direction direction))
    (the program
      (coerce
        (loop while (< position (length source)) collect
          (multiple-value-bind (command new-position)
              (get-next-token source position direction)
            (declare (type command command))
            (declare (type fixnum  new-position))
            (setf position new-position)
            (when (eq command :switch-direction)
              (setf direction
                (get-opposite-direction direction)))
            command))
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (program)
  "Supputates and returns for the tinyBF program a jump table which maps
   its forward and back jump commands in a bidirectional mode by their
   positions' adminiculum."
  (declare (type program program))
  (let ((jump-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table                      jump-table))
    (declare (type (stack-of :element-type fixnum) start-points))
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0 by 1
      if (eq command :jump-forward) do
        (push position start-points)
      else if (eq command :jump-back) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf (gethash start-point jump-table) end-point
                   (gethash end-point   jump-table) start-point))
          (error "No jump start instruction associated with the ~
                  end point at position ~d."
            position))
      end
      finally
        (when start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements the tinyBF program memory as a sparse
   vector of signed integer-valued cells, operated upon by a cell
   pointer to whom the dever of the currently active unit's castaldy is
   attributed."
  (cells   (make-hash-table :test #'eql)
           :type      cell-vector
           :read-only T)
  (pointer 0
           :type      integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    new-value)
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory)
  "Increments the MEMORY's current cell value by one and returns no
   value."
  (declare (type Memory memory))
  (incf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory)
  "Decrements the MEMORY's current cell value by one and returns no
   value."
  (declare (type Memory memory))
  (decf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (memory)
  "Determines whether the MEMORY's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop
        (current-cell-value memory))))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (program
                        &key (input          NIL)
                             (shows-prompt-p T))
  "Executes the tinyBF PROGRAM, utilizing the optional INPUT conduit and
   the SHOWS-PROMPT-P flag which determines the display of an input
   query message, and returns no value."
  (declare (type program          program))
  (declare (type (or null stream) input))
  (declare (type boolean          shows-prompt-p))
  (let ((ip         0)
        (jump-table (compute-jump-table program))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length program)) do
      (case (aref program ip)
        (:eof
          (loop-finish))
        (:switch-direction
          NIL)
        (:increment
          (increment-current-cell memory))
        (:decrement
          (decrement-current-cell memory))
        (:move-right
          (move-cell-pointer-right memory))
        (:move-left
          (move-cell-pointer-left memory))
        (:input
          (when shows-prompt-p
            (format T "~&>> "))
          (finish-output)
          (setf (current-cell-value memory)
            (char-code
              (read-char input NIL #\Null)))
          (clear-output))
        (:output
          (write-char
            (code-char
              (current-cell-value memory))))
        (:jump-forward
          (when (current-cell-contains-zero-p memory)
            (setf ip
              (gethash ip jump-table))))
        (:jump-back
          (unless (current-cell-contains-zero-p memory)
            (setf ip
              (gethash ip jump-table))))
        (:nop
          NIL)
        (otherwise
          (error "Unrecognized command: ~s."
            (aref program ip))))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-tinyBF (code
                         &key (input          NIL)
                              (shows-prompt-p T))
  "Interprets the piece of tinyBF source CODE, utilizing the optional
   INPUT conduit and the SHOWS-PROMPT-P flag which determines the
   display of an input query message, and returns no value."
  (declare (type string           code))
  (declare (type (or null stream) input))
  (declare (type boolean          shows-prompt-p))
  (execute-program
    (extract-commands
      (remove-non-command-content code))
    :input          input
    :shows-prompt-p shows-prompt-p)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-tinyBF
  "++++++++|>++++|>++>+++>+++>+=>>>>+|=>+>+>=+=>>+|=>|>+|=>>==>=+++==
   =+++++++====+++==>>===>+==>===+++===++++++==++++++++===>>+==>++==")

;;; -------------------------------------------------------

;; Reverse input.
(with-open-stream (input (make-string-input-stream "!dlroW olleH"))
  (declare (type string-stream input))
  (interpret-tinyBF "+|>|=|=|>+=|+===>+|"
    :input          input
    :shows-prompt-p NIL))
