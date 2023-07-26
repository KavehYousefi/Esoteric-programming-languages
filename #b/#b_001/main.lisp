;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "#b", also nevened "Sharp flat", created by the Esolang user
;; "None1" and presented in the year 2023, the design of which relates
;; it to the equivalents of "brainfuck", a minimalistic programming
;; language begotten by Urban Mueller, deviating from the provenance,
;; however, by encoding the octuple instruction set subject to
;; appropriation into triplets of arbitrary characters whose
;; combinations of the corresponding ASCII code's parity serves as the
;; identifying criterion of distinguishment.
;; 
;; 
;; Concept
;; =======
;; The #b programming language represents a sequence of zero or more
;; brainfuck commands in the form of three arbitrary characters, each
;; three consecutive entities --- skipping linefeeds --- resolves to one
;; instruction.
;; 
;; == #b: A BRAINFUCK ENCODING ==
;; In its most condensed haecceity, #b represents an encoding of
;; brainfuck's command roster, its foundry an assignment of three
;; consequent character's numeric ASCII code parities to the plaintext
;; brainfuck operation that answers to this combination.
;; 
;; == THREE CHARACTER CODES' PARITIES EQUAL ONE BRAINFUCK COMMAND ==
;; While the conceptual notion describes the transformation triplet in
;; terms of three parities, or a 3-tuple
;; 
;;   (parity_1, parity_2, parity_3)
;; 
;; for a compound of three accolent non-linefeed characters, a
;; notational pecularity assigns to the odd parity the musical symbol
;; "#" and to its even counterpart "b" from the same field, that is:
;; 
;;   ---------------
;;   Parity | Symbol
;;   -------+-------
;;   odd    | #
;;   even   | b
;;   ---------------
;; 
;; In concrete diction, for the eight brainfuck operations the following
;; parity constellations hold:
;; 
;;   ---------------------------------------------------
;;   Parity triplet     | Symbols | brainfuck equivalent
;;   -------------------+---------+---------------------
;;   (even, even, even) |   bbb   | +
;;   ...................................................
;;   (even, even, odd ) |   bb#   | >
;;   ...................................................
;;   (even, odd,  even) |   b#b   | ,
;;   ...................................................
;;   (odd,  even, even) |   #bb   | [
;;   ...................................................
;;   (even, odd,  odd ) |   b##   | ]
;;   ...................................................
;;   (odd,  even, odd ) |   #b#   | .
;;   ...................................................
;;   (odd,  odd,  even) |   ##b   | <
;;   ...................................................
;;   (odd,  odd,  odd ) |   bbb   | -
;;   ---------------------------------------------------
;; 
;; A pseudocode representation of the conversion principle shall now be
;; adduced as a warklume for augmented acquaintance with the conversion
;; process:
;; 
;;   function getBrainfuckCommand (identifier)
;;     let command <- nil
;;     
;;     if identifier = "bbb" then
;;       command <- "plus"
;;     else if identifier = "bb#" then
;;       command = "moveRight"
;;     else if identifier = "b#b" then
;;       command = "inputCharacter"
;;     else if identifier = "#bb" then
;;       command = "jumpForward"
;;     else if identifier = "b##" then
;;       command = "jumpBack"
;;     else if identifier = "#b#" then
;;       command = "outputCharacter"
;;     else if identifier = "##b" then
;;       command = "moveLeft"
;;     else if identifier = "###" then
;;       command = "decrement"
;;     else
;;       error: "Unrecognized command identifier."
;;     end if
;;     
;;     return command
;;   end function
;;   
;;   function extractBrainfuckCommands (sharpFlatCode)
;;     let brainfuckCommands <- empty list
;;     { Builds a three-character identifier of "#"s and "b"s. }
;;     let buffer            <- empty character sequence
;;     
;;     for token in sharpFlatCode do
;;       if token is linefeed then
;;         skip this token
;;       else if ASCII code of token is odd then
;;         append "#" to buffer
;;       else if ASCII code of token is even then
;;         append "b" to buffer
;;       else
;;         error: "Cannot analyze token."
;;       end if
;;       
;;       if size of buffer = 3 then
;;         let brainfuckCommand <- getBrainfuckCommand(buffer)
;;         append brainfuckCommand to brainfuckCommands
;;         clear buffer
;;       end if
;;     end for
;;     
;;     return brainfuckCommands
;;   end function
;; 
;; 
;; Architecture
;; ============
;; #b expresses its lealty to the stock-father brainfuck by the
;; architectural principle's verbatim appropriation.
;; 
;; == THE TAPE: AN INFINITE SEQUENCE OF CELLS ==
;; The memory is realized as a tape composed of a theoretically infinite
;; tally of cells, ordered in a linear arrangement akin to a vector, and
;; expanding bilaterally along both axes. Ensuing from this notion,
;; every cell is co-located with a two neighbors, one on the sinistral,
;; the other on the dextral side.
;; 
;; == THE CELL: A SCALAR OCTET STORAGE ==
;; Each entity among these stores an aefauld unsigned byte value
;; from the range [0, 255]. Initially assuming the default value of zero
;; (0), is response to operations permits a gradual incrementation or
;; decrementation of the state. Transgressions along the two marches
;; incite a wrapping around: Upon descending below the lower threshold
;; of zero, the cell assumes the maximum of 255; if exceeding this upper
;; limit, the value is automatically set to the minimum of zero.
;; 
;; == THE CELL POINTER: A REFERENCE TO THE ACTIVE CELL ==
;; A cell pointer selects at any instant in the program's execution the
;; currently active cell, the cynosure of all commands intent on the
;; perquisition and manipulation of the memory. Operations exists for
;; poco a poco translation of this cursor along the sinistral and
;; dextral axes.
;; 
;; 
;; Data Types
;; ==========
;; #b's data type hierarchy constitutes a simply bifurcation into
;; unsigned octets and ASCII characters, assigning an excellent role to
;; the former group, while the latter partakes of the communicative
;; onuses only.
;; 
;; == UNSIGNED BYTES: THE CURRENCY OF THE MEMORY ==
;; The paravant significance of the type system constitutes the
;; bailiwick of unsigned bytes commorant in the range [0, 255]. Their
;; wonining in the program memory's cells begets the consectary of their
;; ubiquity, namely in the realms of basic arithmetic, control flow
;; steering, and a jointure with the subsidiary character type.
;; 
;; == ASCII CHARACTERS: THE CURRENCY OF COMMUNICATION ==
;; A paravail constituent, the ASCII character species' adit is
;; vindicated by their necessity along the communication conduits only.
;; Input from the system derives as a request for a single character,
;; which, proceeding from its transcription into the corresponding
;; numeric ASCII code, is persisted in the current memory cell. In an
;; athwart direction, output is issued by converting the active cell
;; byte into the respective character for a display purpose.
;; 
;; 
;; Instructions
;; ============
;; #b's instruction set constitutes a verbatim emulation of its
;; brainfuck inspiration's octuple, merely deviating in its design.
;; 
;; == OVERVIEW ==
;; An apercu shall now educate about the available instructions:
;; 
;;   ------------------------------------------------------------------
;;   Parity triplet     | Symbols | Effect
;;   -------------------+---------+------------------------------------
;;   (even, even, even) |   bbb   | Increments the current cell value
;;                      |         | by one (1).
;;                      |         | If the new value exceeds the
;;                      |         | maximum of 255, it is set to the
;;                      |         | minimum of 0 (zero).
;;   ..................................................................
;;   (even, even, odd ) |   bb#   | Moves the cell pointer one step to
;;                      |         | the right.
;;   ..................................................................
;;   (even, odd,  even) |   b#b   | Queries the standard input for an
;;                      |         | ASCII character and stores its
;;                      |         | character code in the current cell.
;;   ..................................................................
;;   (odd,  even, even) |   #bb   | If the current cell value equals
;;                      |         | zero (0), moves the instruction
;;                      |         | pointer (IP) forward to the
;;                      |         | position immediately succeeding the
;;                      |         | matching closing command "b##".
;;                      |         | Aliter proceeds as usual.
;;   ..................................................................
;;   (even, odd,  odd ) |   b##   | If the current cell value does not
;;                      |         | equal zero (0), moves the
;;                      |         | instruction pointer (IP) back to
;;                      |         | the position immediately succeeding
;;                      |         | the matching opening command "#bb".
;;                      |         | Aliter proceeds as usual.
;;   ..................................................................
;;   (odd,  even, odd ) |   #b#   | Prints to the standard output the
;;                      |         | character whose ASCII code equals
;;                      |         | the current cell value.
;;   ..................................................................
;;   (odd,  odd,  even) |   ##b   | Moves the cell pointer one step to
;;                      |         | the left.
;;   ..................................................................
;;   (odd,  odd,  odd ) |   bbb   | Decrements the current cell value
;;                      |         | by one (1).
;;                      |         | If the new value violates the
;;                      |         | minimum of 0 (zero), it is set to
;;                      |         | the maximum of 255.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the lucidity invested into its descriptions, the protolog's
;; explications are inflicted with a scant set of ambiguities, a
;; specimens' select shall be the coming sections' material.
;; 
;; == HOW ARE INCOMPLETE CHARACTER TRIPLETS HANDLED? ==
;; The decoding process from a piece of #b source code to a sequence of
;; brainfuck commands encompasses the aggregation of three consecutive
;; non-linefeed characters into a parity triplet whose configuration
;; is designated the claviger of its construe. Yet no declaration
;; concerning the failure to build a complete triplet --- a case that
;; might transpire at the end of the #b source --- is issued in the
;; language's provenance.
;; 
;; A bivious application of choices offers itself to tenable
;; contemplations:
;; 
;;   (1) An incomplete parity group instigates an error.
;;   (2) An incomplete parity group is simply ignored.
;; 
;; The first case (1) has been adjudged to carry greater aptitude, as
;; indeliberate vices might be detected by conformance to its
;; patration requirement.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, succeeding a conjoined lexer and parser stage with the
;; interpreter's duties.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-17
;; 
;; Sources:
;;   [esolang2023Nope.]
;;   The Esolang contributors, "Nope.", 2023
;;   URL: "https://esolangs.org/wiki/Nope.##b"
;;   Notes:
;;     - Demonstrates an interpreter authored in #b for the esoteric
;;       programming language "Nope.",
;;   
;;   [esolang2023Sharpflat]
;;   The Esolang contributors, "Sharp flat", 2023
;;   URL: "https://esolangs.org/wiki/Sharp_flat"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype parity ()
  "The ``parity'' type enumerates the recognized modes of parity,
   including as an adscititious member the ``:no-parity'' sentinel that
   answers to the linefeed character's ASCII code."
  '(member
    :even
    :odd
    :no-parity))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized #b commands."
  '(member
    :increment
    :move-right
    :input
    :jump-forward
    :jump-back
    :output
    :move-left
    :decrement))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type T)
                                        (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each member of which
   constitutes a cons whose first compartment subsumes into the
   INDICATOR-TYPE, whereas the second moeity assumes the VALUE-TYPE,
   both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element
                       `(cons ,indicator-type ,value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-identifier ()
  "The ``command-identifier'' type defines a command name as a string
   composed of exactly three characters."
  '(string 3))

;;; -------------------------------------------------------

(deftype command-table ()
  "The ``command-table'' type defines a mapping of command identifiers
   to representative objects, realized as an association list, or alist,
   of ``command-identifier'' indicators to ``command'' values."
  '(association-list-of command-identifier command))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype sharp-flat-program ()
  "The ``sharp-flat-program'' type defines an exectuable form of a #b
   program as a vector of zero or more commands in the language."
  '(vector command *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   by these attributes being a commorant of the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory-cell ()
  "The ``memory-cell'' type defines an entry in the sparse program tape,
   represented as an association list, whence ensues a manifestation as
   a cons, the first compartment of which stores the integer cell index,
   while the second segment amplects the ``octet'' cell value."
  '(cons integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum +LINEFEED-ASCII-CODE+))

;;; -------------------------------------------------------

(defparameter +LINEFEED-ASCII-CODE+ 10
  "The ASCII code for the linefeed character.")

;;; -------------------------------------------------------

(defun get-character-parity (plaintext-character)
  "Returns the PLAINTEXT-CHARACTER's parity, or the sentinel
   ``:no-parity'' if confronted with the linefeed character."
  (declare (type character plaintext-character))
  (let ((ascii-code (char-code plaintext-character)))
    (declare (type fixnum ascii-code))
    (the parity
      (cond
        ((= ascii-code +LINEFEED-ASCII-CODE+) :no-parity)
        ((oddp  ascii-code)                   :odd)
        ((evenp ascii-code)                   :even)
        (T
          (error "Cannot determine the parity of \"~c\"."
            plaintext-character))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token buffer.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token-buffer ()
  "Creates and returns a new token buffer, that is, a dynamic string
   which permits insertions at its end."
  (the string
    (make-array 0
      :element-type 'character
      :initial-element #\Null
      :adjustable      T
      :fill-pointer    0)))

;;; -------------------------------------------------------

(defun write-to-token-buffer (buffer new-element)
  "Appends the NEW-ELEMENT to the rear of the token BUFFER and returns
   no value."
  (declare (type string    buffer))
  (declare (type character new-element))
  (format buffer "~c" new-element)
  (values))

;;; -------------------------------------------------------

(defun token-buffer-complete-p (buffer)
  "Determines whether the token BUFFER is complete, that is, contains
   exactly three elements, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type string buffer))
  (the boolean
    (not (null
      (>= (length buffer) 3)))))

;;; -------------------------------------------------------

(defun clear-token-buffer (buffer)
  "Clears the token BUFFER, deleting all of its elements in the process,
   and returns no value."
  (declare (type string buffer))
  (setf (fill-pointer buffer) 0)
  (values))

;;; -------------------------------------------------------

(defun token-buffer-empty-p (buffer)
  "Determines whether the token BUFFER is empty, that is, contains no
   elements, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string buffer))
  (the boolean
    (not (null
      (zerop (length buffer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of command table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-table +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+
  '(("bbb" . :increment)
    ("bb#" . :move-right)
    ("b#b" . :input)
    ("#bb" . :jump-forward)
    ("b##" . :jump-back)
    ("#b#" . :output)
    ("##b" . :move-left)
    ("###" . :decrement))
  "Associates the recognized command identifiers with representative
   command objects.")

;;; -------------------------------------------------------

(defun get-command (identifier)
  "Returns the #b command corresponding to the IDENTIFIER, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type command-identifier identifier))
  (the command
    (or (cdr (assoc identifier +COMMANDS+ :test #'string=))
        (error "Invalid command identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns from the piece of #b source CODE a
   one-dimensional simple array of commands."
  (declare (type string code))
  
  (let ((commands     NIL)
        (token-buffer (make-token-buffer)))
    (declare (type (list-of command) commands))
    (declare (type string            token-buffer))
    
    (flet ((process-parity (parity)
            "Based upon the PARITY, determines whether to write the
             corresponding command identifier constituent to the
             TOKEN-BUFFER or not, in any case returning no value."
            (declare (type parity parity))
            (case parity
              (:odd       (write-to-token-buffer token-buffer #\#))
              (:even      (write-to-token-buffer token-buffer #\b))
              (:no-parity NIL)
              (otherwise  (error "Invalid parity: ~s." parity)))
            (values)))
      
      (loop for token of-type character across code do
        (process-parity
          (get-character-parity token))
        
        ;; Exactly three non-linefeed characters consumed?
        ;; => Determine and collect the corresponding #b instruction.
        (when (token-buffer-complete-p token-buffer)
          (push (get-command token-buffer) commands)
          (clear-token-buffer token-buffer))))
    
    ;; An incomplete character triple at the CODE's desinence instigates
    ;; an error.
    (unless (token-buffer-empty-p token-buffer)
      (error "Unterminated command identifier detected: ~s."
        token-buffer))
    
    (the sharp-flat-program
      (coerce
        (nreverse commands)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Table
  (:constructor make-jump-table ()))
  "The ``Jump-Table'' class serves in the association of jump points in
   a sequence of #b commands, mapping the indices of the forward jump
   operations to that of the matching back jump instructions, and vice
   versa."
  (jump-points NIL :type (association-list-of fixnum fixnum)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-position end-position)
  "Associates the forward jump START-POSITION with the back jump
   END-POSITION in the JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-position))
  (declare (type fixnum     end-position))
  (push (cons start-position end-position)
        (jump-table-jump-points jump-table))
  (push (cons end-position start-position)
        (jump-table-jump-points jump-table))
  (values))

;;; -------------------------------------------------------

(defun get-opposite-jump-point (jump-table jump-position)
  "Returns for the forward or back JUMP-POSITION the affiliated opposite
   jump position in the JUMP-TABLE, or signals an error of an
   unspecified upon the absence of such a correlation."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     jump-position))
  (the fixnum
    (or (cdr (assoc jump-position
               (jump-table-jump-points jump-table)
               :test #'eql))
        (error "No jump position associated with ~d." jump-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements a sparse vector of octets, realized
   as an association list whose indicators, or keys, store the cell
   indices, each entry cons' second compartment comprehending the cell
   value."
  (cells   NIL :type (association-list-of integer octet))
  (pointer 0   :type integer))

;;; -------------------------------------------------------

(defun move-pointer-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns no
   value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun move-pointer-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns no
   value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun get-current-cell (memory)
  "Returns the entry in the MEMORY under its cell pointer as a direct
   references to the maintained integer-octet cons.
   ---
   If no such entry exists yet, a fresh one is created, inserted into
   the MEMORY's association list, and ultimately returned."
  (declare (type Memory memory))
  (the memory-cell
    (or (assoc
          (memory-pointer memory)
          (memory-cells   memory)
          :test #'=)
        (let ((new-cell (cons (memory-pointer memory) 0)))
          (declare (type memory-cell new-cell))
          (push new-cell (memory-cells memory))
          new-cell))))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the octet stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the octet
    (cdr (get-current-cell memory))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE into the MEMORY's current cell, contingently
   wrapping its value into the octet range [0, 255] prior to the
   induction, and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (let ((current-cell (get-current-cell memory)))
    (declare (type memory-cell current-cell))
    (setf (cdr current-cell)
          (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory)
  "Increments the MEMORY's current cell by one and returns no value.
   ---
   If the active cell's value, ensuing from this operation's invocation,
   exceeds the valid maximum of 255, it is set to the lower threshold of
   zero (0)."
  (declare (type Memory memory))
  (incf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory)
  "Decrements the MEMORY's current cell by one and returns no value.
   ---
   If the active cell's value, ensuing from this operation's invocation,
   descends below the valid minimum of zero (0), it is set to the upper
   threshold of 255."
  (declare (type Memory memory))
  (decf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun current-cell-zero-p (memory)
  "Determines whether the MEMORY's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop (current-cell-value memory))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (commands)
  "Builds and returns a ``Jump-Table'' for the #b COMMANDS, thus
   associating each forward jump's position in the program with the
   matching back jump location, and vice versa.
   ---
   An error of an unspecified type is signaled if any of the jump points
   lacks an affiliated opposite."
  (declare (type sharp-flat-program commands))
  (let ((jump-table      (make-jump-table))
        (start-positions NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) start-positions))
    (loop
      for command  of-type command across commands
      for position of-type fixnum  from   0 by 1
      do
        (case command
          (:jump-forward
            (push position start-positions))
          (:jump-back
            (if start-positions
              (let ((start-position (pop start-positions))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (connect-jump-points
                  jump-table
                  start-position
                  end-position))
              (error "Unmatched back jump command at position ~d."
                position)))
          (otherwise
            NIL)))
    (when start-positions
      (error "Unmatched forward jump commands at positions ~{~d~^, ~}."
        start-positions))
    (the Jump-Table jump-table)))

;;; -------------------------------------------------------

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &aux (jump-table (build-jump-table program)))))
  "The ``Interpreter'' class is ordained with the onus of processing a
   #b program and applying actual effect to the same."
  (program    (error "Missing #b program.") :type sharp-flat-program)
  (jump-table (error "Missing jump table.") :type jump-table)
  (ip         0                             :type fixnum)
  (memory     (make-memory)                 :type Memory))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command located at the INTERPRETER's instruction pointer
   (IP), or signals an error of an unspecified type upon the IP's
   transgression of the program boundaries."
  (declare (type Interpreter interpreter))
  (the command
    (if (array-in-bounds-p
          (interpreter-program interpreter)
          (interpreter-ip      interpreter))
      (aref
        (interpreter-program interpreter)
        (interpreter-ip      interpreter))
      (error "Invalid instruction pointer location: ~d."
        (interpreter-ip interpreter)))))

;;; -------------------------------------------------------

(defun program-finished-p (interpreter)
  "Determines whether the INTERPRETER's program has terminated,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-program interpreter)))))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates the INTERPRETER's internally managed #b command sequence
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop
    until (program-finished-p interpreter)
    for current-command
      of-type command
      =       (get-current-command interpreter)
    do
      (case current-command
        (:increment
          (increment-current-cell
            (interpreter-memory interpreter)))
        
        (:move-right
          (move-pointer-right
            (interpreter-memory interpreter)))
        
        (:input
          (format T "~&Please enter an ASCII character: ")
          (setf (current-cell-value (interpreter-memory interpreter))
                (char-code (read-char *standard-input* NIL 0)))
          (clear-input *standard-input*))
        
        (:jump-forward
          (when (current-cell-zero-p
                  (interpreter-memory interpreter))
            (setf (interpreter-ip interpreter)
              (get-opposite-jump-point
                (interpreter-jump-table interpreter)
                (interpreter-ip         interpreter)))))
        
        (:jump-back
          (unless (current-cell-zero-p
                    (interpreter-memory interpreter))
            (setf (interpreter-ip interpreter)
              (get-opposite-jump-point
                (interpreter-jump-table interpreter)
                (interpreter-ip         interpreter)))))
        
        (:output
          (format T "~c"
            (code-char
              (current-cell-value
                (interpreter-memory interpreter)))))
        
        (:move-left
          (move-pointer-left
            (interpreter-memory interpreter)))
        
        (:decrement
          (decrement-current-cell
            (interpreter-memory interpreter)))
        
        (otherwise
          (error "Invalid command ~s at position ~d."
            current-command
            (interpreter-ip interpreter))))
      
      (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Sharp-flat (code)
  "Interprets the piece of #b source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!" employing a transparent syntax.
(interpret-Sharp-flat
  "bbbbbbbbbbbbbbbbbbbbbbbb#bbbb#bbbbbbbbbbbbbbbbbbbbbbbb##b###b##bb#bbbbbbbbbbbbbbbbbbbbb
bbb#b#bb#bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb#bbbb#bbbbbbbbbbbbbbbbbbbbbbbb##b###b##bb#
bbbbbbbbbbbbbbb#b#bbbbbbbbbbbbbbbbbbbbb#b##b#bbbbbbbbb#b#bb#bbbbbbbbbbbb#bbbb#bbbbbbbbb
bbbbbbbbbbbbbbb##b###b##bb##b###b##b##b##bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
#b#bb#bb##b#bbbbbbbbb#b####################b##########################b#bb#bb#bbb#b#")

;;; -------------------------------------------------------

;; Print "Hello World!" employing an opaque syntax.
(interpret-Sharp-flat
  "NjzDrpXLZldbvdZVNxdDDrHzILfHtcvjPvPlhpJHxjrDHJLHzfNRzNASjmYIhyIjDqZzPdVfHJdXppHdTLXhxjd
JjFaVodHgfxBBJzbxPBzbFDnpDTjzbRhhLvlbxRtzDlrVKvjXBetbzFBxTTlTvRXPZnlnFhzjVfiiFQMePAwdbI
XXFDxjZNfpBnblPOnWjtRXLdBhtDnXJHTVrtpdxMnWAncXjTlBXJZfuFIVbcrXbvDltLLnpDADnZBAJPzrxzHVl
vntjTJXZLtBnHnRkmZkskhsqJxSQfgMMnGwDQuPwQNPVpDxXDZXBtdRVNZBtRhffPzvvVzLVZFbzjrhdLnpnpDF
srEpTydNaQzMZtRnNbdpByncyUWGKOSkukeUCWagqGYRAEwQemEYuGUSoCMWWMSmIGWcgaLoVnoZXETTFcnm")

;;; -------------------------------------------------------

;; Infinitely repeating cat program employing a transparent syntax.
(interpret-Sharp-flat "bbb#bbb#b#b#b##")

;;; -------------------------------------------------------

;; Infinitely repeating cat program employing an opaque syntax.
(interpret-Sharp-flat "dXzyHhtQVCfMFAS")

;;; -------------------------------------------------------

;; Reverse one line of input employing a transparent syntax.
(interpret-Sharp-flat
  "bb#bbb#bbbb#bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb#b##############################b####b#bb##bb##bb####bb##bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb#b####b#bb#b###bb##")

;;; -------------------------------------------------------

;; Reverse one line of input employing a transparent syntax.
(interpret-Sharp-flat
  "PpyTPLmbxtRybhnFFvRPPXRtJTrpbnzBrrBRTBThLnBmlYaagwskuuEEQYGcyawSWwAemesWUgOlAoCyRaTfKwNJuShNWSmeLLsabvTjbHLNrnhDhHPfxZrHHJdPbnnNLlLNvVixAcwajIrdMZSsAvdsA")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language "Nope.".
(interpret-Sharp-flat
  "ftnZHnbxrLtfBRNJzJqZHFzittrHhxFnlRZxDVdPbVhpTXtJBNTXjtTVTTpLZFvSgXeWCrkWrHwgBahRu
ztdnHffvxfhLdZpDVDdjpjHLtNBVhxZDNgtFVPwNTNJJhBHnpVrnxVlpRTdTlJzPjfFltaUXgUCRECDla
XNhsPohlvATaoIwUGaeoIQMgwqaGKOEYCyqCaMWCosUwIaTiDDEPFnjdPVtpRZZDjRCbNHNEHRvVnvXP
dDBXPZPHNRJftDXXvPHekFGUKTMeDLoHPJwxK")
