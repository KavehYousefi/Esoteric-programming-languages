;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Malbrain", invented by the Esolang user "Doomguy0505" and
;; presented on November 14th, 2007, the kenspeckle attribute of which
;; wones in its expression of Urban Mueller's "brainfuck" in a twissel
;; of commands, the same decode the octuple competences of its entheus
;; with the conformance to a mode curtailed in its variety, but defrayed
;; by a magnitude's amplification in the code's mickleness.
;; 
;; 
;; Concept
;; =======
;; The Malbrain programming language furnishes an paregal to its
;; inspiration, brainfuck, reducing the eight instructions to a mere
;; two, defined in terms of transitions from one brainfuck command to
;; the next, and the selected one's execution.
;; 
;; == MALBRAIN PROGRAMS: BRAINFUCK COMMANDS SELECTION AND EXECUTION ==
;; The sole Malbrain instruction tokens, ">", and ".", concord with the
;; following agencies:
;; 
;;   - The transitioning from the current brainfuck command to the next
;;     one, according to a predefined scheme that commences with the
;;     dextral cell pointer translation operation, ">", and terminates
;;     in the back jump "]", repeating in a siclike mode.
;;   - The execution of the current selected brainfuck command.
;; 
;; == THE BRAINFUCK COMMAND TRANSITIONS FOLLOW A SPECIFIC ORDER ==
;; The brainfuck commands arrangement's regulation proceeds from an
;; established enumeration, its transition inchoating with the ">"
;; token, concluding in "]", and repeating accordingly in a cyclic
;; manner, internally indexed starting with zero (0) through seven (7):
;; 
;;   --------------------------------------------------------
;;   No. | Current brainfuck command | Next brainfuck command
;;   ----+---------------------------+-----------------------
;;   0   | >                         | <
;;   ........................................................
;;   1   | <                         | +
;;   ........................................................
;;   2   | +                         | -
;;   ........................................................
;;   3   | -                         | .
;;   ........................................................
;;   4   | .                         | ,
;;   ........................................................
;;   5   | ,                         | [
;;   ........................................................
;;   6   | [                         | ]
;;   ........................................................
;;   7   | ]                         | >
;;   --------------------------------------------------------
;; 
;; 
;; Instructions
;; ============
;; The governance of an equipollent capacity identifies Malbrain in
;; equiparation to its brainfuck cleronomy, reducing the stock-father's
;; octuple circumference to a quart of its cardinality.
;; 
;; A syntactical vinculum to the provenance, any non-command token in
;; Malbrain experiences a mete of patience that renders such content
;; tantamount to mere descants.
;; 
;; == MALBRAIN COMMAND OVERVIEW ==
;; Malbrain's operative twain shall be the following apercu's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Selects the next brainfuck command from the list,
;;           | wrapping around to the incipient member upon the
;;           | desinence's transgression.
;;           | As a curtailed purlicue, the following internections
;;           | govern the process:
;;           | 
;;           |   --------------------------------------------------
;;           |   Current brainfuck command | Next brainfuck command
;;           |   --------------------------+-----------------------
;;           |   >                         | <
;;           |   ..................................................
;;           |   <                         | +
;;           |   ..................................................
;;           |   +                         | -
;;           |   ..................................................
;;           |   -                         | .
;;           |   ..................................................
;;           |   .                         | ,
;;           |   ..................................................
;;           |   ,                         | [
;;           |   ..................................................
;;           |   [                         | ]
;;           |   ..................................................
;;           |   ]                         | >
;;           |   --------------------------------------------------
;;   ..................................................................
;;   .       | Executes the currently selected brainfuck command.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK COMMAND OVERVIEW ==
;; The octuple brainfuck instruction set, which constitutes the
;; effective telos inherent to any Malbrain program's aspirations, shall
;; be a cursory species of nurture's material:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the memory cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   <       | Translates the memory cell pointer oen step to the left.
;;   ..................................................................
;;   +       | Increments the current cell value by one.
;;           | If the new value transgresses the upper march of 255,
;;           | the cell state wraps around to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;           | If the new value descends below the lower march of zero
;;           | (0), the cell state wraps around to the maximum of 255.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code equals the current
;;           | cell value to the standard output.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Interpreter
;; ===========
;; This interpreter, as well as its converters betwixt Malbrain and
;; brainfuck, have been implemented in the programming language
;; Common Lisp.
;; 
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-18
;; 
;; Sources:
;;   [esolang2014Malbrain]
;;   The Esolang contributors, "Malbrain", July 13th, 2014
;;   URL: "https://esolangs.org/wiki/Malbrain"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype association-list-of (&optional (indicator-type '*)
                                        (value-type     '*))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each indicator, or key, of
   which conforms to the INDICATOR-TYPE and associates with a value of
   the VALUE-TYPE, both defaulting to the generic ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for entry
                of-type T
                in      (the list candidate)
              always
                (typep entry
                  `(cons ,indicator-type ,value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   which conform to the ELEMENT-TYPE, defaulting to the generic ``*''
   sentinel."
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

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic ``*'' sentinel."
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

(deftype brainfuck-command ()
  "The ``brainfuck-command'' type enumerates the recognized brainfuck
   instructions."
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

(deftype brainfuck-command-table ()
  "The ``brainfuck-command-table'' type defines a mapping of brainfuck
   command tokens to representative command objects, implemented as an
   association list, or alist, which maps the command characters to
   ``brainfuck-command'' objects."
  '(association-list-of character brainfuck-command))

;;; -------------------------------------------------------

(deftype brainfuck-command-entry ()
  "The ``brainfuck-command-entry'' type defines an entry into a
   ``brainfuck-command-table'', realized as a cons whose sinistral
   compartment lends horborage to a ``character'', while its dextral
   moeity is desumed from the ``brainfuck-command'' species."
  '(cons character brainfuck-command))

;;; -------------------------------------------------------

(deftype brainfuck-program ()
  "The ``brainfuck-program'' type defines an executable brainfuck
   program as a vector of zero or more ``brainfuck-command'' instances."
  '(vector brainfuck-command *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, as a corollary, a commorant of the closed integral
   range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   amplecting, without the subject's exhaustion, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of brainfuck command table.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type brainfuck-command-table +BRAINFUCK-IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +BRAINFUCK-IDENTIFIERS+
  '((#\> . :move-right)
    (#\< . :move-left)
    (#\+ . :increment)
    (#\- . :decrement)
    (#\. . :output)
    (#\, . :input)
    (#\[ . :jump-forward)
    (#\] . :jump-back))
  "Associates the recognized brainfuck command tokens with their
   representative ``brainfuck-command'' objects.")

;;; -------------------------------------------------------

(defun get-brainfuck-command-entry (token)
  "Returns the entry in the +BRAINFUCK-IDENTIFIERS+ table corresponding
   to the TOKEN, or ``NIL'' upon its disrespondency."
  (declare (type character token))
  (the (or null brainfuck-command-entry)
    (assoc token +BRAINFUCK-IDENTIFIERS+ :test #'char=)))

;;; -------------------------------------------------------

(defun brainfuck-command-token-p (token)
  "Determines whether the TOKEN represents a brainfuck command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (get-brainfuck-command-entry token)))))

;;; -------------------------------------------------------

(defun get-brainfuck-command (token)
  "Returns the brainfuck command associated with the TOKEN, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type character token))
  (let ((command-entry (get-brainfuck-command-entry token)))
    (declare (type (or null brainfuck-command-entry) command-entry))
    (the brainfuck-command
      (if command-entry
        (cdr command-entry)
        (error "The character \"~c\" does not represent a ~
                brainfuck command."
          token)))))

;;; -------------------------------------------------------

(defun get-brainfuck-command-token (command)
  "Returns the character which corresponds to the brainfuck COMMAND, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type brainfuck-command command))
  (let ((command-entry
          (rassoc command +BRAINFUCK-IDENTIFIERS+ :test #'eq)))
    (declare (type (or null brainfuck-command-entry) command-entry))
    (if command-entry
      (car command-entry)
      (error "No brainfuck command: ~s." command))))

;;; -------------------------------------------------------

(defun get-next-brainfuck-command (current-command)
  "Returns the brainfuck command succeeding the CURRENT-COMMAND as
   specified by the Malbrain arrangement."
  (declare (type brainfuck-command current-command))
  (the brainfuck-command
    (case current-command
      (:move-right   :move-left)
      (:move-left    :increment)
      (:increment    :decrement)
      (:decrement    :output)
      (:output       :input)
      (:input        :jump-forward)
      (:jump-forward :jump-back)
      (:jump-back    :move-right)
      (otherwise     (error "Unrecognized command: ~s."
                       current-command)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the "Malbrain-Decoder" interface.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Malbrain-Decoder ()
  ()
  (:documentation
    "The ``Malbrain-Decoder'' interface serves as a common foundry for
     all entities dedicated to the conversion of a piece of Malbrain
     source code into some representation of the equivalent brainfuck
     program."))

;;; -------------------------------------------------------

(defgeneric start-decoding (decoder)
  (:documentation
    "Prepares the DECODER for the subsequent process defined by repeated
     requests for brainfuck character consumptions, and returns no
     value."))

;;; -------------------------------------------------------

(defgeneric decode-command-token (decoder token)
  (:documentation
    "Consumes the brainfuck command TOKEN in the DECODER's context and
     returns no value."))

;;; -------------------------------------------------------

(defgeneric decode-nop-token (decoder token)
  (:documentation
    "Consumes the non-brainfuck command TOKEN in the DECODER's context
     and returns no value."))

;;; -------------------------------------------------------

(defgeneric finish-decoding (decoder)
  (:documentation
    "Concludes the DECODER's investement and returns a value fitten for
     the same.
     ---
     Please heed that DECODER is not expected to be vouch for its iterum
     amenability concerning the returned object for subsequent
     invocations, or any other means. If, a forbisen being adduced, the
     DECODER responds with a string representation of a brainfuck
     program, this string is homologated its irreversible destruction,
     and a second request to the ``finish-decoding'' operation may
     respond in any fashion, including an undefined behavior or an error
     signaling."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Code-String-Decoder".                    -- ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Code-String-Decoder (Malbrain-Decoder)
  ((buffer
    :initform      (make-string-output-stream)
    :type          string-stream
    :documentation "Collects the brainfuck command, depending upon the
                    IGNORE-COMMENTS-P flag's state also embracing
                    non-command content.")
   (ignore-comments-p
    :initarg       :ignore-comments-p
    :initform      T
    :type          boolean
    :documentation "Determines whether non-command tokens shall be
                    omitted, that is, not induced into BUFFER's
                    brainfuck source code."))
  (:documentation
    "The ``Code-String-Decoder'' class furnishes a Malbrain decoder
     accommodated for the production of the equivalent brainfuck program
     in the form of a string comprehending the incorporated commands,
     contingently embracing in the acquisition the commentary segments
     ostended by the provenance."))

;;; -------------------------------------------------------

(defun make-code-string-decoder (&key (ignore-comments-p T))
  "Creates and returns a new ``Code-String-Decoder'' which converts a
   Malbrain program into the equivalent brainfuck code in string form,
   contingently, depending upon the IGNORE-COMMENTS-P flag, omitting all
   non-brainfuck command tokens."
  (the Code-String-Decoder
    (make-instance 'Code-String-Decoder
      :ignore-comments-p ignore-comments-p)))

;;; -------------------------------------------------------

(defmethod start-decoding ((decoder Code-String-Decoder))
  (declare (type Code-String-Decoder decoder))
  (finish-output
    (slot-value decoder 'buffer))
  (values))

;;; -------------------------------------------------------

(defmethod decode-command-token ((decoder Code-String-Decoder)
                                 (token   character))
  (declare (type Code-String-Decoder decoder))
  (declare (type character           token))
  (write-char token
    (slot-value decoder 'buffer))
  (values))

;;; -------------------------------------------------------

(defmethod decode-nop-token ((decoder Code-String-Decoder)
                             (token   character))
  (declare (type Code-String-Decoder decoder))
  (declare (type character           token))
  (unless (slot-value decoder 'ignore-comments-p)
    (write-char token
      (slot-value decoder 'buffer)))
  (values))

;;; -------------------------------------------------------

(defmethod finish-decoding ((decoder Code-String-Decoder))
  (declare (type Code-String-Decoder decoder))
  (the string
    (get-output-stream-string
      (slot-value decoder 'buffer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command-Sequence-Decoder".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command-Sequence-Decoder (Malbrain-Decoder)
  ((commands
    :initform      NIL
    :type          (list-of brainfuck-command)
    :documentation "A stack of the gathered ``brainfuck-command''
                    objects, the same ought to be reversed ere its
                    ultimate transcription into a ``brainfuck-program''
                    command vector."))
  (:documentation
    "The ``Command-Sequence-Decoder'' class contributes a Malbrain
     decoder whose dedication apportions it the wike of producing an
     equivalency in the brainfuck language by a vector of
     ``brainfuck-command'' objects, that is, a ``brainfuck-program''
     composition."))

;;; -------------------------------------------------------

(defun make-command-sequence-decoder ()
  "Creates and returns a new ``Command-Sequence-Decoder''."
  (the Command-Sequence-Decoder
    (make-instance 'Command-Sequence-Decoder)))

;;; -------------------------------------------------------

(defmethod start-decoding ((decoder Command-Sequence-Decoder))
  (declare (type Command-Sequence-Decoder decoder))
  (setf (slot-value decoder 'commands) NIL)
  (values))

;;; -------------------------------------------------------

(defmethod decode-command-token ((decoder Command-Sequence-Decoder)
                                 (token   character))
  (declare (type Command-Sequence-Decoder decoder))
  (declare (type character                token))
  (push
    (get-brainfuck-command token)
    (slot-value decoder 'commands))
  (values))

;;; -------------------------------------------------------

(defmethod decode-nop-token ((decoder Command-Sequence-Decoder)
                             (token   character))
  (declare (type Command-Sequence-Decoder decoder))
  (declare (ignore                        decoder))
  (declare (type character                token))
  (declare (ignore                        token))
  (values))

;;; -------------------------------------------------------

(defmethod finish-decoding ((decoder Command-Sequence-Decoder))
  (declare (type Command-Sequence-Decoder decoder))
  (the brainfuck-program
    (coerce
      (nreverse
        (slot-value decoder 'commands))
      '(simple-array brainfuck-command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Malbrain decoding operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Code-String-Decoder
               +DEFAULT-BRAINFUCK-CODE-DECODER+))
(declaim (type Command-Sequence-Decoder
               +DEFAULT-BRAINFUCK-PROGRAM-DECODER+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-BRAINFUCK-CODE-DECODER+
  (make-code-string-decoder)
  "Furnishes the default Malbrain decoder intended to generate an
   equivalent brainfuck source code in string form.")

(defparameter +DEFAULT-BRAINFUCK-PROGRAM-DECODER+
  (make-command-sequence-decoder)
  "Furnishes the default Malbrain decoder intended to generate an
   equivalent brainfuck command sequence as a ``brainfuck-program''.")

;;; -------------------------------------------------------

(defun decode-malbrain-code (decoder malbrain-code)
  "Decodes the MALBRAIN-CODE utilizing the DECODER and returns the
   latter's output."
  (declare (type Malbrain-Decoder decoder))
  (declare (type string           malbrain-code))
  (start-decoding decoder)
  (let ((current-brainfuck-command :move-right))
    (declare (type brainfuck-command current-brainfuck-command))
    (loop
      for malbrain-token of-type character across malbrain-code
      if (char= malbrain-token #\>) do
        (setf current-brainfuck-command
          (get-next-brainfuck-command current-brainfuck-command))
      else if (char= malbrain-token #\.) do
        (decode-command-token decoder
          (get-brainfuck-command-token current-brainfuck-command))
      else do
        (decode-nop-token decoder malbrain-token)
      end))
  (the T
    (finish-decoding decoder)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of fixnum fixnum)
    :documentation "Associates the positions of the forward jump and
                    back jump commands, desumed from a specific
                    brainfuck program, in a bilateral fashion."))
  (:documentation
    "The ``Jump-Table'' class serves the wike of the internections'
     establishment betwixt the forward and back jump commands in a
     brainfuck program, delegated to the unambiguous indices into the
     selfsame instruction sequence."))

;;; -------------------------------------------------------

(defun make-empty-jump-table ()
  "Creates and returns an empty ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table first-end-point second-end-point)
  "Associates the Jump points FIRST-END-POINT and SECOND-END-POINT in a
   symmetrical fashion in the JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     first-end-point))
  (declare (type fixnum     second-end-point))
  (with-slots (connections) jump-table
    (declare (type (hash-table-of fixnum fixnum) connections))
    (setf (gethash first-end-point  connections) second-end-point)
    (setf (gethash second-end-point connections) first-end-point))
  (values))

;;; -------------------------------------------------------

(defun get-jump-target (jump-table source-position)
  "Returns the index of the jump end point united in a vinculum with the
   SOURCE-POSITION in the JUMP-TABLE, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source-position))
  (with-slots (connections) jump-table
    (declare (type (hash-table-of fixnum fixnum) connections))
    (the fixnum
      (or (gethash source-position connections)
          (error "No jump target associated with the position ~d."
            source-position)))))

;;; -------------------------------------------------------

(defun build-jump-table (brainfuck-program)
  "Supputates and returns for the BRAINFUCK-PROGRAM as jump table which
   connects its forward and back jump commands by mediation of their
   locations in the program's instruction sequence."
  (declare (type brainfuck-program brainfuck-program))
  (let ((jump-table          (make-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type brainfuck-command across brainfuck-program
      for position of-type fixnum            from   0 by 1
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (connect-jump-points jump-table start-point end-point))
          (error "Unmatched back jump point at position ~d." position))
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at ~
                  position~:p ~{~d~^, ~}."
            (length   forward-jump-points)
            (nreverse forward-jump-points))))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of unsigned byte-valued cells,
                    realized as a hash table, the keys of which answer
                    to the cell indices, engaged in internection with
                    the cell values as the associated entry objects.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, manifesting as the key (index)
                    into the CELLS table's active entry."))
  (:documentation
    "The ``Memory'' class implements the Malbrain program memory, a
     verbatim appropriate of brainfucks, by its furnishment of a
     bilaterally infinite dispansion of unsigned byte-valued cells, the
     currently active one, amenable to all operations, designated by a
     mobile cell pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns an empty ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the octet stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer octet) cells))
    (declare (type integer                       pointer))
    (the octet
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceding the transfer by a wrapping around of the object in order to
   accommodate the valid unsigned byte range [0, 255], and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer octet) cells))
    (declare (type integer                       pointer))
    (setf (gethash pointer cells 0)
          (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck interpreter.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainfuck-program (program)
  "Interprets the brainfuck PROGRAM and returns no value."
  (declare (type brainfuck-program program))
  (let ((ip         0)
        (jump-table (build-jump-table program))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length program)) do
      (let ((current-command (aref program ip)))
        (declare (type brainfuck-command current-command))
        (case current-command
          (:move-right
            (move-cell-pointer-right memory))
          (:move-left
            (move-cell-pointer-left memory))
          (:increment
            (incf (current-cell-value memory)))
          (:decrement
            (decf (current-cell-value memory)))
          (:output
            (write-char
              (code-char
                (current-cell-value memory))))
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value memory)
              (char-code
                (read-char)))
            (clear-input))
          (:jump-forward
            (when (zerop (current-cell-value memory))
              (setf ip
                (get-jump-target jump-table ip))))
          (:jump-back
            (unless (zerop (current-cell-value memory))
              (setf ip
                (get-jump-target jump-table ip))))
          (otherwise
            (error "Invalid brainfuck command: ~s."
              current-command))))
      (incf ip)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Malbrain interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Malbrain (malbrain-code)
  "Interprets the piece of MALBRAIN-CODE and returns no value."
  (interpret-brainfuck-program
    (decode-malbrain-code
      +DEFAULT-BRAINFUCK-PROGRAM-DECODER+
      malbrain-code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck encoder.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-Malbrain-transition (start-command target-command
                                  malbrain-destination)
  "Writes to the MALBRAIN-DESTINATION the sequence of Malbrain state
   transitions and the concluding execution instruction requisite for
   moving from the brainfuck START-COMMAND to the TARGET-COMMAND and
   accompassing the same, and returns no value."
  (declare (type brainfuck-command start-command))
  (declare (type brainfuck-command target-command))
  (declare (type destination       malbrain-destination))
  (loop
    for current-command
      of-type brainfuck-command
      =       start-command
      then    (get-next-brainfuck-command current-command)
    while
      (not (eq current-command target-command))
    do
      (format malbrain-destination ">")
    finally
      (format malbrain-destination "."))
  (values))

;;; -------------------------------------------------------

(defun encode-brainfuck (brainfuck-code
                         &key (destination       NIL)
                              (ignore-comments-p T))
  "Generates for the piece of BRAINFUCK-CODE the equivalent Malbrain
   program, writes the same to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responds with a
   fresh string comprehending the result.
   ---
   If the IGNORE-COMMENTS-P flag is set to the Boolean ``T'' value, the
   commentary tokens in the BRAINFUCK-CODE are omitted during the
   transcription; otherwise these are appropriated ipsissima verba in
   the DESTINATION."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (declare (type boolean     ignore-comments-p))
  (the (or null string)
    (if destination
      (let ((current-brainfuck-command :move-right))
        (declare (type brainfuck-command current-brainfuck-command))
        (loop
          for token of-type character across brainfuck-code
          if (brainfuck-command-token-p token) do
            (let ((new-command (get-brainfuck-command token)))
              (declare (type brainfuck-command new-command))
              (write-Malbrain-transition
                current-brainfuck-command new-command destination)
              (setf current-brainfuck-command new-command))
          else if (not ignore-comments-p) do
            (format destination "~c" token)
          end))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (encode-brainfuck brainfuck-code
          :destination       output
          :ignore-comments-p ignore-comments-p)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Malbrain
  ".>>.........>>>>.>>>.>........>>>>>>.>>>.>>>>.>>.>>>.>>>>.
   >>.......>>>>.>>>.>....>>>>>>.>>>.>>>>.>>.>.>>.>>>>>>.......
   >>..>>>>>>...>>.>>>>...>>........>>>>.>>>.>....>>>>>>.>>>.>>>>.
   >>.>>>.>>>>...>>..........>>>>.>>>.>.........>>>>>>.>>>.>>>>.>>.
   >>...>.>>>>>....>>>.>>>>>>...>>.>>>>>>>......>.>>>>>>>........>.
   >>>>..>>.>>.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Malbrain ">>>>>.>.>>>>>>.>>.>>>>>.>>>>.>>>>>>.>>.")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Malbrain
  ">>>>>.>>>>>>>.>>.>>>>>..>>>>>.>>.>>>>.>>..>>>>>>>.>>.>>>>>.>>>>>>.>>>.>>..>>>>>>.")

;;; -------------------------------------------------------

;; Generate the brainfuck code equivalent to this "Hello World!"
;; program in Malbrain and return the resulting string.
(decode-malbrain-code
  +DEFAULT-BRAINFUCK-CODE-DECODER+
  ".>>.........>>>>.>>>.>........>>>>>>.>>>.>>>>.>>.>>>.>>>>.
   >>.......>>>>.>>>.>....>>>>>>.>>>.>>>>.>>.>.>>.>>>>>>.......
   >>..>>>>>>...>>.>>>>...>>........>>>>.>>>.>....>>>>>>.>>>.>>>>.
   >>.>>>.>>>>...>>..........>>>>.>>>.>.........>>>>>>.>>>.>>>>.>>.
   >>...>.>>>>>....>>>.>>>>>>...>>.>>>>>>>......>.>>>>>>>........>.
   >>>>..>>.>>.")

;;; -------------------------------------------------------

;; Generate the brainfuck code equivalent to this repeating cat program
;; in Malbrain and return the resulting string.
(decode-malbrain-code
  +DEFAULT-BRAINFUCK-CODE-DECODER+
  ">>>>>.>.>>>>>>.>>.>>>>>.>>>>.>>>>>>.>>.")

;;; -------------------------------------------------------

;; Generate the brainfuck code equivalent to this repeating cat program
;; in Malbrain and return the resulting string, retaining all commentary
;; characters in the process.
(decode-malbrain-code
  (make-code-string-decoder :ignore-comments-p NIL)
  "input:        >>>>>.
jump_forward: >.
output:       >>>>>>.
jump_forward: >>.
decrement:    >>>>>.
jump_back:    >>>>.
input:        >>>>>>.
jump_back:    >>.")

;;; -------------------------------------------------------

;; Generate the executable brainfuck command sequence equivalent to this
;; repeating cat program in Malbrain and return the resulting string.
(decode-malbrain-code
  +DEFAULT-BRAINFUCK-PROGRAM-DECODER+
  ">>>>>.>.>>>>>>.>>.>>>>>.>>>>.>>>>>>.>>.")

;;; -------------------------------------------------------

;; Generate the Malbrain program for the repeating cat program stated in
;; brainfuck and execute the same.
(interpret-Malbrain
  (encode-brainfuck ",[.,]"))

;;; -------------------------------------------------------

;; Generate the Malbrain program for the repeating cat program stated in
;; brainfuck and return it as a string.
(encode-brainfuck ",[.,]")

;;; -------------------------------------------------------

;; Generate the Malbrain program for the repeating cat program stated in
;; brainfuck and return it as a string, retaining all comments.
(encode-brainfuck
  "input:        ,
   jump_forward: [
   output:       .
   iniput:       ,
   jump_back:    ]"
  :ignore-comments-p NIL)
