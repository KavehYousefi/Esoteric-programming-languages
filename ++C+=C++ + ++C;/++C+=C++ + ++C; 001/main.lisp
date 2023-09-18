;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "++C+=C++ + ++C;", invented by the Esolang user
;; "FiveUnderlines" and presented on January 20th, 2023, the kenspeckle
;; attribute of wich resides in its derivation from Urban Mueller's
;; "brainfuck" in substituting each of the cleronomy's octuple
;; instructions by a combination of a quadruple designator and a binary
;; mode responsible for assigning the concrete effect to twifaced
;; command token.
;; 
;; 
;; Concept
;; =======
;; The "++C+=C++ + ++C;" language's haecceity wones in its replication
;; of its brainfuck stock-father's potentials by a reduced instruction
;; set, the entelechy of which issues from its "mode" state, selecting
;; for most command tokens one of two possible causata.
;; 
;; == THE MODE: A CAUSATUM SELECTOR FOR AN INSTRUCTION ==
;; Its twifaced potential's impartation results for a preponderance
;; among "++C+=C++ + ++C;"'s commands by the contemporaneous mode's
;; influence upon the executed instruction.
;; 
;; Empighted at the default value of zero (0), this flag's apprehension
;; is impounded to admit only two members: zero (0) and one (1).
;; 
;; While a direct specification of the mode state is forensical to the
;; language's nature, a command serves as the accoutrement for switching
;; its current value to the obverse. Naturally, for this negation
;; process a contingency of two possibilities can be enumerated:
;; 
;;   ------------------------
;;   Current mode | Next mode
;;   -------------+----------
;;   0            | 1
;;   ........................
;;   1            | 0
;;   ------------------------
;; 
;; 
;; Architecture
;; ============
;; "++C+=C++ + ++C;"'s architecture is witeless in its adherence to
;; brainfuck's impositions: A bilaterally expanse of
;; unsigned-byte-valued cells comprises the tape-like memory, each such
;; salvatory administering attention to the integral range of [0, 255]
;; by wrapping around transcending values.
;; 
;; A cursor, known as the "cell pointer", selects at any instant in the
;; program's execution the currently active cell, this constituting the
;; sole entity endowed with amenability to memory modifications. The
;; pointer's motility permits its gradual translation along the tape's
;; axes in both airts.
;; 
;; 
;; Data Type
;; =========
;; The language's lealty to its stock-father does not fail to perpetuate
;; in the type system, accommodating a paravaunt significance to the
;; octet type as the memory cells' occupant, while ASCII character are
;; impounded to the paravail regions of communication.
;; 
;; 
;; Syntax
;; ======
;; "++C+=C++ + ++C;" programs, in regard to their donet, admit any
;; character, however, responding only to designated command tokens,
;; themselves always composed of an aefauld entity's extent.
;; 
;; As a corollary, non-operational specimens may be apprehended to the
;; chevisance of commentary supererogations.
;; 
;; 
;; Instructions
;; ============
;; Concomitant to its equipollence to brainfuck, the instruction set
;; autochthonous in "++C+=C++ + ++C;" is curtailed by an aefauld tally
;; to a septuple cardinality, the claviger of its relative
;; compendiousness maintaining its woning in the binary mode and its
;; twissel capacity to replicate brainfuck operations.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a basic mete of gnarity
;; regarding the language's operational warklumes:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   C       | Switches the mode.
;;           |---------------------------------------------------------
;;           | If the mode equals zero (0), sets the mode to one (1).
;;           |---------------------------------------------------------
;;           | If the mode equals one (1), sets the mode to zero (0).
;;           |---------------------------------------------------------
;;           | The following causata hold for the current mode in
;;           | conjunction with a switch:
;;           |   ------------------------
;;           |   Current mode | Next mode
;;           |   -------------+----------
;;           |   0            | 1
;;           |   ........................
;;           |   1            | 0
;;           |   ------------------------
;;           |---------------------------------------------------------
;;           | At the program's inchoation, the mode assumes the value
;;           | zero (0).
;;   ..................................................................
;;   +       | If the mode equals zero (0), decrements the current cell
;;           | value by one.
;;           | If transgressing the lower march of zero (0), the cell
;;           | state wraps around to the maximum of 255.
;;           |---------------------------------------------------------
;;           | If the mode equals one (1), increments the current cell
;;           | value by one.
;;           | If transcending the upper march of 255, the cell state
;;           | wraps around to the minimum of zero (0).
;;   ..................................................................
;;   =       | If the mode equals zero (0), moves the cell pointer one
;;           | step to the left.
;;           |---------------------------------------------------------
;;           | If the mode equals one (1), moves the cell pointer one
;;           | step to the right.
;;   ..................................................................
;;   ,       | If the mode equals zero (0), prints the character whose
;;           | ASCII code corresponds to the current cell value to the
;;           | standard output.
;;           |---------------------------------------------------------
;;           | If the mode equals one (1), queries the standard input
;;           | for an ASCII character and stores its character code in
;;           | the current cell.
;;   ..................................................................
;;   (       | Operates independently of the mode. If the current cell
;;           | value equals zero (0), moves the instruction pointer
;;           | forward to the location immediately succeeding the
;;           | matching ")". Otherwise proceeds as usual.
;;   ..................................................................
;;   )       | Operates independently of the mode. If the current cell
;;           | value does not equal zero (0), moves the instruction
;;           | pointer back to the location immediately succeeding the
;;           | matching "(". Otherwise proceeds as usual.
;;   ..................................................................
;;   ;       | Operates independently of the mode. Immediately
;;           | terminates the program.
;;           | If no such command exists at the codes's desinence, the
;;           | program, when completely processed, will signal a
;;           | runtime error of an unspecified type.
;;   ------------------------------------------------------------------
;; 
;; == COMMAND + MODE = BRAINFUCK INSTRUCTION ==
;; The collaborative force of "++C+=C++ + ++C;" commands and the mode
;; as equiparations to brainfuck operations shall be limned in the alow
;; juxtaposition:
;; 
;;   -----------------------------
;;   Command | Mode = 0 | Mode = 1
;;   --------+----------+---------
;;   +       | -        | +
;;   .............................
;;   =       | <        | >
;;   .............................
;;   ,       | .        | ,
;;   .............................
;;   (       | [        | [
;;   .............................
;;   )       | ]        | ]
;;   -----------------------------
;; 
;; == BRAINFUCK INSTRUCTIONS REPLICATION ==
;; The athwart affiliation shall be exposed now, demonstrating the
;; replication of brainfuck's octuple instruction set by the conjoined
;; champarty of a "++C+=C++ + ++C;" token and a mode:
;; 
;;   ----------------------------------
;;   brainfuck | ++C+=C++ + ++C; | Mode
;;   ----------+-----------------+-----
;;   <         | =               | 0
;;   ..................................
;;   >         | =               | 1
;;   ..................................
;;   -         | +               | 0
;;   ..................................
;;   +         | +               | 1
;;   ..................................
;;   .         | ,               | 0
;;   ..................................
;;   ,         | ,               | 1
;;   ..................................
;;   [         | (               | any
;;   ..................................
;;   ]         | )               | any
;;   ----------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Ensuing from its lucid explications, and the fact of its derivation
;; from brainfuck, the same imports and inspires several well
;; establishes regulations, the "++C+=C++ + ++C;" is not inflicted with
;; any patent or severe insufficiencies.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has been provided in Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-07
;; 
;; Sources:
;;   [esolang2023++C+=C++_+_++C;]
;;   The Esolang contributors, "++C+=C++ + ++C;", January 21st, 2023
;;   URL: "https://esolangs.org/wiki/%2B%2BC%2B%3DC%2B%2B_%2B_%2B%2BC;"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list compact of zero or more elements
   conforming to the ELEMENT-TYPE, the default of which assumes the
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, associating with each key of the KEY-TYPE a value of
   the VALUE-TYPE, for the both of which the comprehensive ``T''
   provides the default."
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

(deftype jump-table ()
  "The ``jump-table'' type defines an association betwixt forward and
   back jump commands in a \"++C+=C++ + ++C;\" program, designated by
   their positions in the supplied code, and realized as a hash table
   whose fixnum keys map to values of the same species."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   consecutive bits, thus a commorant of the range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   unsigned-byte-valued cells occupying a theoretically infinite
   expanse, realized as a hash table whose integer keys model the cell
   indices, and whose ``octet'' values refer to the cell values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype mode ()
  "The ``mode'' type defines the interpretation mode as a bit datum."
  'bit)

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   same comprehend, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Termination-Error (error)
  ((position
    :initarg       :position
    :initform      (error "Missing position.")
    :reader        termination-error-position
    :type          fixnum
    :documentation "The location in the code inflicted with a missing
                    termination command \";\"."))
  (:report
    (lambda (condition stream)
      (declare (type Termination-Error condition))
      (declare (type destination       stream))
      (format stream "Missing a \";\" at position ~d."
        (termination-error-position condition))))
  (:documentation
    "The ``Termination-Error'' condition serves to signal an anomalous
     situation whose etiology wones in a missing termination command
     (\";\") at a program's desinence."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (code)
  "Supputates and returns for the piece of \"++C+=C++ + ++C;\" source
   CODE the jump table, the same associates each forward jump command's
   location in the CODE with the corresponding back jump position, and
   vice versa."
  (declare (type string code))
  (let ((jump-table (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      if (char= token #\() do
        (push position forward-jump-points)
      else if (char= token #\)) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched jump end point at position ~d." position))
      finally
        (when forward-jump-points
          (error "Unmatched jump start points at positions ~{~d~^, ~}."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun interpret-|++C+=C++ + ++C;| (code)
  "Interprets the piece of \"++C+=C++ + ++C;\" source CODE and returns
   no value."
  (declare (type string code))
  (let ((ip                  0)
        (current-instruction (when (plusp (length code))
                               (char code 0)))
        (jump-table          (compute-jump-table code))
        (mode                0)
        (memory              (make-hash-table :test #'eql))
        (cell-pointer        0)
        (terminated-p        NIL))
    (declare (type fixnum              ip))
    (declare (type (or null character) current-instruction))
    (declare (type jump-table          jump-table))
    (declare (type mode                mode))
    (declare (type memory              memory))
    (declare (type integer             cell-pointer))
    (declare (type boolean             terminated-p))
    
    (labels
        ((advance ()
          "Advances the instruction pointer IP to the next location in
           the CODE, if possible, updates the CURRENT-INSTRUCTION, and
           returns no value."
          (setf current-instruction
            (when (array-in-bounds-p code (1+ ip))
              (char code
                (incf ip))))
          (values))
         
         (jump-to-opposite-boundary ()
          "Expecting to reside at a forward or back jump command,
           relocates the instruction pointer (IP) to the opposite march,
           and returns no value.
           ---
           If no end point could be retrieved for the current IP
           position, an error of an unspecified type is signaled."
          (setf ip
            (or (gethash ip jump-table)
                (error "No jump target associated with the position ~d."
                  ip)))
          (values))
         
         (switch-mode ()
          "Toggles the MODE to its obverse state and returns no value."
          (setf mode
            (- 1 mode))
          (values))
         
         (current-cell ()
          "Returns the byte value stored in the current cell."
          (the octet
            (gethash cell-pointer memory 0)))
         
         ((setf current-cell) (new-value)
          "Stores the NEW-VALUE, contingently prepared by wrapping into
           in the unsigned byte range [0, 255], in the current cell and
           returns no value."
          (declare (type integer new-value))
          (setf (gethash cell-pointer memory 0)
                (mod new-value 256))
          (values))
         
         (print-current-cell ()
          "Prints the character whose ASCII code corresponds to the
           current cell to the standard output and returns no value."
          (write-char
            (code-char
              (current-cell)))
          (values))
         
         (query-for-input ()
          "Queries the standard input for a character, stores its ASCII
           code in the current cell, and returns no value."
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell)
            (char-code
              (read-char)))
          (clear-input)
          (values)))
      
      (loop while current-instruction do
        (case current-instruction
          ((NIL)
            (loop-finish))
          
          (#\C
            (switch-mode))
          
          (#\+
            (case mode
              (0         (decf (current-cell)))
              (1         (incf (current-cell)))
              (otherwise (error "Invalid mode: ~d." mode))))
          
          (#\=
            (case mode
              (0         (decf cell-pointer))
              (1         (incf cell-pointer))
              (otherwise (error "Invalid mode: ~d." mode))))
          
          (#\,
            (case mode
              (0         (print-current-cell))
              (1         (query-for-input))
              (otherwise (error "Invalid mode: ~d." mode))))
          
          (#\(
            (when (zerop (current-cell))
              (jump-to-opposite-boundary)))
          
          (#\)
            (unless (zerop (current-cell))
              (jump-to-opposite-boundary)))
          
          (#\;
            (setf terminated-p T)
            (loop-finish))
          
          (otherwise
            NIL))
        
        (advance))
      
      (unless terminated-p
        (error 'Termination-Error :position ip))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-|++C+=C++ + ++C;| "C+C(C,);")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; A pseudocode formulation shall be adduced:
;; 
;;   mode <- 1
;;   { Either "0" (= 48) or "1" (= 49). }
;;   memory[0] <- user input
;;   
;;   mode <- 0
;;   print memory[0]
;;   
;;   mode <- 1
;;   { Move to memory cell memory[1]. }
;;   cellPointer <- cellPointer + 1
;;   memory[1]   <- 6
;;   { Set memory[0] to 0 or 1, while memory[1] decreases to 0. }
;;   while memory[1] != 0 do
;;     mode        <- 0
;;     memory[1]   <- memory[1]   - 1
;;     
;;     { Move to memory[0]. }
;;     cellPointer <- cellPointer - 1
;;     memory[0]   <- memory[0]   - 8
;;     
;;     mode        <- 1
;;     { Move to memory[1] in order to drive the loop. }
;;     cellPointer <- cellPointer + 1
;;   end while
;;   
;;   { Current state: mode = 1, cell pointer on memory[1]. }
;;   memory[1] <- 49
;;   
;;   mode <- 0
;;   cellPointer <- cellPointer - 1
;;   while memory[0] != 0 do
;;     mode        <- 1
;;     { Move to memory[1]. }
;;     cellPointer <- cellPointer + 1
;;     
;;     mode <- 0
;;     print memory[1]
;;     
;;     { Move to memory[0] for conducting the loop. }
;;     cellPointer <- cellPointer - 1
;;   end while
(interpret-|++C+=C++ + ++C;|
  "
  C,
  C,
  C
  =
  ++++++
  (
    C+=
    ++++++++
    C=
  )

  +++++++++++++++++++++++++++++++++++++++++++++++++
  C=
  (
    C=C,=
  )
  ;
  ")
