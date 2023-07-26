;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deadfuck", invented by the Esolang user "None1" and
;; presented in the year 2023, its design being based upon a slightly
;; curtailed variant of Urban Mueller's "brainfuck" with the command
;; tokens' nominal supersession by the identifiers employed in the
;; esoteric programming language "DFS", invented by "None1" themself.
;; 
;; 
;; Concept
;; =======
;; The Deadfuck programming language constitutes a derivation from
;; brainfuck, the legacy of which merely echews from the eight commands
;; the cell value deduction specimen, further modified through the
;; replacement of the inspiring provenance's identifiers by names
;; desumed from DFS, a Deadfish scion.
;; 
;; 
;; Architecture
;; ============
;; Deadfuck's architecture constitutes an acquisition of a particularly
;; popular brainfuck infrastructure, composed of an bilaterally infinite
;; tape of octet-valued cells, operated upon by a cell pointer.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF CELLS ==
;; The program memory establishes a componancy based upon an infinite
;; tally of cells, extending bilaterally in a linear fashion.
;; 
;; == THE CELLS: SCALAR OCTET STORAGES ==
;; Every cell provides an unsigned byte's salvatory, thus inhabiting the
;; closed integer interval [0, 255]. Upon modification, the value
;; automatically wraps around to accommodate these marches, that is, if
;; transcending the upper bourne of 255, the value returns to the lower
;; extremum of zero (0).
;; 
;; The latter state also defines a cell's default.
;; 
;; == THE CELL POINTER: THE ACTIVE CELL MARKER ==
;; At any instant in the program, an aefauld cell homologates
;; perquisitions and manipulations, the same being selected by an entity
;; known as the "cell pointer". This cursor's amenability to gradual
;; translations capacitates the infinite tape's traversal in order to
;; access any cell.
;; 
;; 
;; Data Types
;; ==========
;; A bifurcation into unsigned bytes as the paravant tokens of
;; programming and characters as parhedral elements suffices for the
;; a complete treatise on Deadfuck's type system.
;; 
;; == OCTETS: COMMORANTS OF THE MEMORY ==
;; The excellence apportioned to the unsigned byte type, spanning the
;; integer range [0, 255], is corroborated by the memory cells'
;; castaldy, whence stems the indagation and manipulation by the
;; commands' vast preponderance.
;; 
;; == CHARACTERS: COMMUNICATIVE AUXILIARIES ==
;; The character type, answering to the ASCII standard, partakes of a
;; parhedral agency, utilized exclusively for the communications along
;; the input and output conduits.
;; 
;; To this end, an input reception, committed in the form of a
;; character, enters the currently active cell through transcription
;; into its ASCII code.
;; 
;; Proceeding in the athwart airt, an output request commences by
;; translating the current cell's octet value into the corresponding
;; ASCII character, ere its issuing unto the console. 
;; 
;; 
;; Instructions
;; ============
;; Deadfuck's diorism originates from a kensback acquisition of its
;; brainfuck provenance, imprimis curtailed into a heptad from the
;; cleronomy's octuple instructions by the deduction member's omission;
;; secondary, located at a nominal ambit, through the command
;; identifiers' substitution by those desumed from Deadfish.
;; 
;; == OVERVIEW ==
;; A cursory exhibition's dation intends to accommodate a foundational
;; experience with the language:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the current cell value by one.
;;           | If the new value exceeds the maximum of 255, it is
;;           | wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   d       | Queries the standard input for an ASCII character and
;;           | stores its character code in the current cell.
;;   ..................................................................
;;   s       | Prints the ASCII character corresponding to the current
;;           | cell value, when construed as a character code, to the
;;           | standard output.
;;   ..................................................................
;;   o       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   O       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   l       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "L" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   L       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "l" command.
;;           | Otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == DEADFUCK AND BRAINFUCK ==
;; A set of visual equiparations shall aid in illustrating the vinculum
;; betwixt Deadfuck and its entheus, brainfuck.
;; 
;; Please note the absence of a decrementation operator, denoted by "-"
;; in the stock-father, from Deadfuck.
;; 
;;   --------------------
;;   Deadfuck | brainfuck
;;   ---------+----------
;;   i        | +
;;   ....................
;;   d        | ,
;;   ....................
;;   s        | .
;;   ....................
;;   o        | <
;;   ....................
;;   O        | >
;;   ....................
;;   l        | [
;;   ....................
;;   L        | ]
;;   --------------------
;; 
;; Conversely, brainfuck maps to Deadfuck in the following manner:
;; 
;;   --------------------
;;   brainfuck | Deadfuck
;;   ----------+---------
;;   +         | i
;;   ....................
;;   -         | (None.)
;;   ....................
;;   ,         | d
;;   ....................
;;   .         | s
;;   ....................
;;   <         | o
;;   ....................
;;   >         | O
;;   ....................
;;   [         | l
;;   ....................
;;   ]         | L
;;   --------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The pellucid delineations in the protolog, especially as engaged in
;; coefficacy with the cognate DFS specification, serve to expel any
;; significant ambiguities.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, directed at simplicity as its cynosure.
;; 
;; A kenspeckle, and aiblins kensback, attribute of its realization, the
;; surrogate roles for parameters have been assigned to special
;; variables, akin to globally active, but only locally accessible
;; references, entalenting the program with a clandestine alternative
;; for explicit global variables, while concomitantly curtailing the
;; function signatures.
;; 
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; It constitutes a peisant element of gnarity to remember that special
;; variables, ligated into a consanguinity with global variables as a
;; general species, and exacerbated by their implicit and contingently
;; arbitrary declarations, merit the wite of encumbering programs with
;; superfluous complexity. For a more detailed treatise on the
;; contingency for detriments incurred by this feature please refer to
;; [stackoverflow2019q56725814].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-25
;; 
;; Sources:
;;   [esolang2023Deadfuck]
;;   The Esolang contributors, "Deadfuck", 2023
;;   URL: "https://esolangs.org/wiki/Deadfuck"
;;   
;;   [stackoverflow2012q41091118]
;;   The Stack Overflow contributors,
;;     "What's the canonical way to join strings in a list?", 2012
;;   URL: "https://stackoverflow.com/a/41091118"
;;   Notes:
;;     - Demonstrates the usance of special variables in the context of
;;       the ``format'' function.
;;   
;;   [stackoverflow2019q56725814]
;;   The Stack Overflow contributors, "Using Local Special Variables",
;;     2019
;;   URL: "https://stackoverflow.com/questions/56725814/
;;         using-local-special-variables"
;;   Notes:
;;     - Discusses the disadvantages of special variables, which
;;       comprehend:
;;        o Lack of referential transparency, ...
;;          ... which renders it more difficult to reason functionally
;;          about one's code, meaning that functions may produce
;;          different results with syntactically equivalent calls.
;;        o Introduction of bugs, ...
;;          ... as lexical variable at other locations in the code,
;;          e.g. in a system function, will be overwritten.
;;        o Confusion ...
;;          .. for readers unacquainted with special (dynamic) binding
;;        o Dubious necessity, ...
;;          ... as lexical binding or even anaphoric macros may be
;;          utilized instead.
;;   
;;   [tutorialspoint2023assemblyaddrmodes]
;;   The Tutorials Point contributors, "Assembly - Addressing Modes",
;;     2023
;;   URL: "https://www.tutorialspoint.com/assembly_programming/
;;         assembly_addressing_modes.htm"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
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
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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
  "The ``jump-table'' type defines a mapping of forward jump command
   positions to locations of the matching back jumps in a piece of
   Deadfuck source code, represented by a hash table which associates
   fixnum keys to values of the same type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   thus being a commorant of the closed integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector
   of octet-valued cells, amenable to a signed integer index, and
   modeled by a hash table that maps integer keys to octet values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table ()
  "Populates the JUMP-TABLE for a piece of Deadfuck CODE with the
   vinculums betwixt the positions of the forward and backward jump
   commands in the program and returns no value."
  (declare (special code))
  (declare (special jump-table))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for position of-type fixnum    from 0 below (length code)
      for token    of-type character =    (char code position)
      if (char= token #\l) do
        (push position forward-jump-points)
      else if (char= token #\L) do
        (if forward-jump-points
          (let ((start-position (pop forward-jump-points))
                (end-position   position))
            (declare (type fixnum start-position))
            (declare (type fixnum end-position))
            (setf (gethash start-position jump-table) end-position)
            (setf (gethash end-position   jump-table) start-position))
          (error "Unmatched back jump point at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-current-cell ()
  "Returns the unsigned byte value stored in the MEMORY cell at the
   CELL-POINTER."
  (declare (special memory))
  (declare (special cell-pointer))
  (the octet
    (gethash cell-pointer memory 0)))

;;; -------------------------------------------------------

(defun increment-current-cell ()
  "Increments the MEMORY cell value at the CELL-POINTER by one,
   contingently wrapping its value around to fit into the unsigned byte
   range of [0, 255], and returns no value."
  (declare (special memory))
  (declare (special cell-pointer))
  (setf (gethash cell-pointer memory 0)
        (mod (1+ (gethash cell-pointer memory 0)) 256))
  (values))

;;; -------------------------------------------------------

(defun set-current-cell (new-value)
  "Stores the NEW-VALUE, contingently after wrapping it around into the
   unsigned byte range of [0, 255], in the MEMORY cell at the
   CELL-POINTER and returns no value."
  (declare (special      memory))
  (declare (special      cell-pointer))
  (declare (type integer new-value))
  (setf (gethash cell-pointer memory 0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun read-input-into-current-cell ()
  "Queries the standard input for an ASCII character, stores its
   character code in the MEMORY cell at the CELL-POINTER, and returns no
   value."
  (declare (special memory))
  (declare (special cell-pointer))
  (set-current-cell
    (char-code
      (read-char *standard-input* NIL 0)))
  (clear-input *standard-input*)
  (values))

;;; -------------------------------------------------------

(defun print-current-cell ()
  "Prints to the standard output the character corresponding to the
   MEMORY cell value at the CELL-POINTER when construed as an ASCII code
   and returns no value."
  (declare (special memory))
  (declare (special cell-pointer))
  (write-char
    (code-char
      (get-current-cell)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-zero-p ()
  "Determines whether the MEMORY cell at the CELL-POINTER contains the
   value zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (special memory))
  (declare (special cell-pointer))
  (the boolean
    (not (null
      (zerop (get-current-cell))))))

;;; -------------------------------------------------------

(defun move-cell-pointer-left ()
  "Translates the memory'S CELL-POINTER one cell to the left and returns
   no value."
  (declare (special cell-pointer))
  (decf cell-pointer)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right ()
  "Translates the memory's CELL-POINTER one cell to the right and
   returns no value."
  (declare (special cell-pointer))
  (incf cell-pointer)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Deadfuck (code)
  "Interprets the piece of Deadfuck source CODE and returns no value."
  (declare (special     code))
  (declare (type string code))
  (let ((jump-table   (make-hash-table :test #'eql))
        (memory       (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (special         jump-table))
    (declare (type jump-table jump-table))
    (declare (special         memory))
    (declare (type memory     memory))
    (declare (special         cell-pointer))
    (declare (type integer    cell-pointer))
    
    (compute-jump-table)
    
    (loop
      with ip of-type fixnum = 0
      while (< ip (length code))
      do
        (case (char code ip)
          ((NIL)
            (loop-finish))
          
          (#\i
            (increment-current-cell))
          
          (#\d
            (format T "~&Please enter an ASCII character: ")
            (read-input-into-current-cell))
          
          (#\s
            (print-current-cell))
          
          (#\o
            (move-cell-pointer-left))
          
          (#\O
            (move-cell-pointer-right))
          
          (#\l
            (when (current-cell-zero-p)
              (setf ip
                (or (gethash ip jump-table)
                    (error "No back jump point defined for the ~
                            position ~d."
                      ip)))))
          
          (#\L
            (unless (current-cell-zero-p)
              (setf ip
                (or (gethash ip jump-table)
                    (error "No forward jump point defined for the ~
                            position ~d."
                      ip)))))
          
          (otherwise
            NIL))
      
      (incf ip)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-Deadfuck
  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiis
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiis
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiisiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")

;;; -------------------------------------------------------

;; Infinitely repeating program which terminates on a user input equal
;; to the null character.
(interpret-Deadfuck "ildsL")

;;; -------------------------------------------------------

;; Truth-machine which exploits the cell value wrapping behavior at the
;; upper bound of 255 to continue at the minimum zero (0).
;; 
;; Memory layout:
;;   cell[0] <- user input
;;   cell[1] <- ASCII code 49 for character "1"
;; 
;; Concept:
;;   move to cell[1]
;;   increment cell[1] to the value 49 (= ASCII character "1")
;;   move to cell[0]
;;   query user input and store in cell[0]
;;   print ASCII character for cell[0] (either "0" or "1")
;;   increment cell[0] by the amount 208, wrapping around at 255
;;     => thus input of 48 becomes 0, and 49 becomes 1
;;   if cell[0] = 0 then
;;     jump to end of program
;;   else
;;     while cell[0] != 0 do
;;       move to cell[1], which contains the ASCII code 49 for "1"
;;       print cell[1] (= "1")
;;       move to cell[0] for next while loop test cycle
;;     end while
;;   end if
(interpret-Deadfuck
  "Oiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
   od
   s
   iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
   iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
   iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
   iiiiiiiiiiiiiiiiiiiiiiiiiiii
   lOsoL")
