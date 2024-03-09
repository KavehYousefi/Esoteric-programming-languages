;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "miscmisc2", invented by the Esolang user "Transoptimal" and
;; presented on December 9th, 2022, the specimen's haecceity wones in
;; its conflation of data and instructions in an infinite memory compact
;; of signed integer cells, the first components of which is allotted
;; the governance over the instruction pointer, a designation of the
;; instruction segment and its immediately succeeding operands.
;; 
;; 
;; Concept
;; =======
;; The miscmisc2 programming language's foundry constitutes a memory
;; composed of signed integer-valued cells, enumerated with integral
;; indices starting from inclusive zero (0), and extending into the
;; positive infinite range, inwith whose bournes both data and
;; instructions partake of a simulataneous sustenance.
;; 
;; == THE MEMORY: AN INFINITE VECTOR OF SIGNED INTEGERS ==
;; The nucleus and vinculum betwixt the language's components, the
;; memory ostends a unilaterally infinite dispansion of cells, akin to
;; a vector, the subscripts of which start with the index zero (0) and
;; increment towards the positive infinite axis.
;; 
;; Each cell, initially set to the state of zero (0), may lend a
;; salvatory to a scalar integer number of any magnitude and sign.
;; 
;; == THE INSTRUCTION POINTER IS LOCATED IN THE FIRST CELL ==
;; That cell empight on the first memory position, zero (0),
;; accommodates the instruction pointer's, or simply pointer's, abode,
;; the value consigned to this unit's castaldy determines the location
;; into the memory where the next instruction may be retrieved.
;; 
;; == INSTRUCTIONS AND THEIR ARGUMENTS FORM A LOGICAL COMPOUND ==
;; Located immediately dextrally to an instruction's commorancy in the
;; memory reside its one through three arguments, depending on the
;; concrete operational specimen's requirements. These constituents, as
;; a corollary, serve in a logical segment's establishment by being in
;; direct adjacency to the instruction.
;; 
;; == MISCMISC2 PROGRAMS CONFLATE DATA AND INSTRUCTIONS ==
;; miscmisc2's infrastructure is founded upon a confluence of data and
;; instructions in the memory, both represented by signed integer
;; objects of any magnitude, occupying a vector that, enumerated
;; commencing with the subscript zero (0), extends infinitely along the
;; positive axis.
;; 
;; A forbisen for this twifaced concept will be limned by our memory's
;; illustration:
;; 
;;   +----------------------------------------------------------+
;;   | Index | 0 |  1 | 2 |  3 | 4 |  5 |  6 | 7 |  8 |  9 | 10 |
;;   |-------|---+----+---+----+---+----+----+---+----+----+----|
;;   | Value | 6 | 46 | 0 | 38 | 2 | 17 | 38 | 2 | 10 | 63 | 0  |
;;   |-------|---+----+---+----+---+----+----+---+----+----+----|
;;   | Role  | p |    |   |    |   |    | q  | x | y  |    |    |
;;   +----------------------------------------------------------+
;; 
;; Given the state of the pointer p as:
;; 
;;   p = memory[0] = 6
;; 
;; We yield by supputation the following:
;; 
;;   q = memory[p]   = memory[memory[0]]   = memory[6] = 38
;;   x = memory[p+1] = memory[memory[0]+1] = memory[7] =  2
;;   y = memory[p+2] = memory[memory[0]+2] = memory[8] = 10
;; 
;; q designates the current command, whose value of q = 38 answers, in
;; an interpretation as an ASCII code, to the character "&"; this token
;; is affiliated with the cell references transfer instruction, its
;; avail the two arguments x and y. Siccan diorism produces, as a
;; consectary, the operational ultimity:
;; 
;;   Copy the value of memory[x] to memory[y]
;;   
;;   -- Or, more precisely, with x = 2 and y = 10:
;;   
;;   Copy the value of memory[2] to memory[10]
;; 
;; == MISCMISC2 PROGRAMS OPERATE IN CYCLES ==
;; The execution principle of miscmisc2 bifurcates its programs into a
;; twain of ordered stages: Imprimis, given the memory initialized with
;; zero (0) states, the source code, specified as an integer sequence,
;; enjoys a transfer into the first cells; ensuing from this parasceve,
;; a cycle of current instruction reception from the pointer cell at the
;; index zero (0), the retrieval of the respective items in the memory,
;; and an execution thereof, partakes; contingently terminating the
;; program upon its continuation antecedent's failure.
;; 
;; A more formal elucidation shall be the alow treatise's substance:
;; 
;; Given:
;; 
;;   M: Constitutes the program memory, composed of an infinite tally
;;      of signed integer cells, enumerated commencing with the index
;;      0, that is:
;;        M = (m[0], m[1], ..., m[i], ...),
;;      where
;;        i    >= 0
;;        m[i] is a signed integer.
;;   p: The value of the first memory cell at the index 0, that is:
;;        p = m[0]
;;      This value, if employed as an index into the memory itself,
;;      usually references at the position of the instruction q in
;;      the memory, thus acting as the instruction pointer.
;;   q: The value stored in the memory cell at the index p, thus
;;      designating the next instruction's numeric code:
;;        q = m[p].
;;   x: The value immediately following the instruction q in the
;;      memory, acting as the first argument to the same, if
;;      necessary:
;;        x = m[p+1].
;;   y: The value immediately following the parameter x in the
;;      memory, acting as the second argument to the instruction q, if
;;      necessary:
;;        y = m[p+2].
;;   z: The value immediately following the parameter y in the
;;      memory, acting as the third argument to the instruction q, if
;;      necessary:
;;        z = m[p+3].
;; 
;; The following procedure holds:
;; 
;;   (A) The initialization segment commences.
;;   
;;   (0) If the miscmisc2 source code is provided in the form of a
;;       string, or ordered sequence, of characters, S, with
;;         S = (s[0], s[1], ..., s[i], ..., s[N-1])
;;       where
;;         N    >= 0
;;         0    <= i < N
;;         s[i] is the i-th character in the string S,
;;       then each character s[i] is substituted by its Unicode code
;;       point c[i],
;;       where
;;         c[i] >= 0
;;       thus yielding a new ordered sequence C
;;         C = (c[0], c[2], ..., c[i], ..., c[N-1]).
;;   
;;   (1) Given
;;         C = (c[0], c[1], ..., s[i], ..., c[N-1])
;;       where
;;         N    >= 0 and
;;         0    <= i < N
;;         c[0] is a signed integer
;;       which is either provided directly, or obtained in the
;;       prevenient step (0) by an adscititious supputation effort, the
;;       first N-1 cells of the memory, commencing with the index 0, are
;;       initialized to the terms of C, that is:
;;         m[i] = c[i], for 0 <= i < N
;;       The remaining infinite memory elements are initialized to 0:
;;         m[j] = 0, for j > N
;;   
;;   (B) The execution cycle commences.
;;   
;;   (2) If it holds:
;;         (p < 1) or (m[i] = 0), for all i >= m[p]
;;       the program immediately terminates.
;;   
;;   (3) Execute the next instruction answering to the integer code q
;;       with:
;;         q = m[p]
;;         x = m[p+1]
;;         y = m[p+2]
;;         z = m[p+3],
;;       where it holds:
;;         q, x, y, z are signed integer numbers.
;;       Unrecognized instruction codes q are introduce no-operations
;;       (NOPs).
;;   
;;   (5) Repeat with step -> (2).
;; 
;; == INPUT AND OUTPUT ARE IMPLEMENTATION-DEPENDENT ==
;; The modes and intrinsics of the input and output conduits introduce
;; an implementation-dependent aspect in the language.
;; 
;; The following minimum requirements apply to these athwart fluences:
;; 
;;   (a) The input provider must upon each request return a signed or
;;       unsigned integer number of any magnitude. The value zero (0)
;;       appropriates a dioristic agency in that is delivery conflates
;;       with the construe as an end-of-file (EOF) sentinel, that is,
;;       the communication of its provenance's exhaustion.
;;   
;;   (b) The output receptor must upon each request consume a signed or
;;       unsigned integer number of any magnitude. The purpose resident
;;       at its ultimity does not wist of further impositions.
;; 
;; The miscmisc2 standard redes, without alternatives' exclusion, this
;; solution assessed with a beneficial mete of sanity these defaults:
;; 
;;   (a) The input is accommodated via a queue of signed integers.
;;   (b) The output employs either a stack of signed integers or a
;;       text string.
;; 
;; 
;; Instructions
;; ============
;; A septuple cardinality maintains its purview over miscmisc2's
;; instruction set, in its competences encompassing basic arithmetics,
;; the copying of cell values, input and output facilities, as well as
;; an aefauld jump-based control flow construct.
;; 
;; Every operation's representation proceeds in terms of a positive
;; integer value, for which may serve, as a conceptual warklume, the
;; affiliated ASCII character. No-operations, or NOPs, to whom
;; appertains the status of any unreserved numeric value, advance the
;; instruction pointer value by a magnitude of four (4), while
;; exercising abstinence from further epiphenomena.
;; 
;; == OVERVIEW ==
;; Pursuing to impart a cursory degree of nortelry anenst the language's
;; operational aspect, a tabular exposition will become the
;; communication's medium.
;; 
;; A requisitum to the operation's apprehension, a certain set of
;; identifiers shall perform as adminicular constituents:
;; 
;;   ------------------------------------------------------------------
;;   Symbol    | Definition
;;   ----------+-------------------------------------------------------
;;   memory[i] | The value of the memory cell located at the index i,
;;             | where it holds for i:
;;             |   0 <= i <= +infinity
;;   ..................................................................
;;   p         | The value stored in the first memory cell memory[0],
;;             | stevened the "pointer". This value usually designates
;;             | the index of the next instruction in the memory to
;;             | execute.
;;   ..................................................................
;;   q         | The value stored in the memory cell whose index
;;             | derives from the pointer value p, that is:
;;             |   q = memory[p] = memory[memory[0]]
;;             | This object usually designates the ASCII code of the
;;             | current command token to execute.
;;   ..................................................................
;;   x         | The value stored in the memory cell immediately
;;             | succeeding the command value q, that is:
;;             |   x = memory[p+1] = memory[memory[0]+1]
;;             | This object usually designates the first argument to
;;             | the command specified by q.
;;   ..................................................................
;;   y         | The value stored in the memory cell located two
;;             | positions dextrally to the command value q, that is:
;;             |   y = memory[p+2] = memory[memory[0]+2]
;;             | This object usually designates the second argument to
;;             | the command specified by q.
;;   ..................................................................
;;   z         | The value stored in the memory cell located three
;;             | positions dextrally to the command value q, that is:
;;             |   z = memory[p+3] = memory[memory[0]+3]
;;             | This object usually designates the third argument to
;;             | the command specified by q.
;;   ..................................................................
;; 
;; Entalented with this imperative terminology, the instruction list
;; ensues:
;; 
;;   ------------------------------------------------------------------
;;   Command | ASCII code | Effect
;;   --------+------------+--------------------------------------------
;;   !       |     33     | Sets the cell at memory[y] to the value of
;;           |            | of A. If the cell manipulated by this
;;           |            | instruction does not constitute the pointer
;;           |            | cell p, that is, memory[0], increments the
;;           |            | value of p by three; otherwise p is not
;;           |            | altered.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   let hasModifiedPointer <- false
;;           |            |   
;;           |            |   if y = 0 then
;;           |            |     hasModifiedPointer <- true
;;           |            |   else if
;;           |            |     hasModifiedPointer <- false
;;           |            |   end if
;;           |            |   
;;           |            |   memory[y] <- x
;;           |            |   
;;           |            |   if not hasModifiedPointer
;;           |            |     p <- p + 3
;;           |            |   end if
;;   ..................................................................
;;   &       |     38     | Sets the cell at memory[y] to the value of
;;           |            | of the cell at memory[x]. If the cell
;;           |            | manipulated by this instruction does not
;;           |            | constitute the pointer cell p, that is,
;;           |            | memory[0], increments the value of p by
;;           |            | three; otherwise p is not altered.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   let hasModifiedPointer <- false
;;           |            |   
;;           |            |   if y = 0 then
;;           |            |     hasModifiedPointer <- true
;;           |            |   else if
;;           |            |     hasModifiedPointer <- false
;;           |            |   end if
;;           |            |   
;;           |            |   memory[y] <- memory[x]
;;           |            |   
;;           |            |   if not hasModifiedPointer
;;           |            |     p <- p + 3
;;           |            |   end if
;;   ..................................................................
;;   +       |     43     | Adds the value of the cell y to that of the
;;           |            | cell x and stores the sum in the cell z.
;;           |            | Subsequently increments p by four.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   z <- x + y
;;           |            |   p <- p + 4
;;   ..................................................................
;;   ,       |     44     | Writes the value of cell x to the output
;;           |            | sink. Subsequently increments p by two.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   printToOutput(x)
;;           |            |   p <- p + 2
;;   ..................................................................
;;   -       |     45     | Subtracts the value of the cell y from that
;;           |            | of the cell x and stores the difference in
;;           |            | the cell z. Subsequently increments p by
;;           |            | four.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   z <- x - y
;;           |            |   p <- p + 4
;;   ..................................................................
;;   .       |     46     | Queries the input provider for an integer
;;           |            | number and stores this object in the cell
;;           |            | x. Subsequently increments p by two.
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   x <- get integer input
;;           |            |   p <- p + 2
;;   ..................................................................
;;   ?       |     63     | Alters the state of p pursuant to these
;;           |            | stipulations:
;;           |            |   - If the value of x is positive, sets p
;;           |            |     to the value of y.
;;           |            |   - If the value of x is negative, sets p
;;           |            |     to the value of z.
;;           |            |   - If the value of x is zero (0),
;;           |            |     increments p by four (4).
;;           |            |--------------------------------------------
;;           |            | The following pseudocode formulation
;;           |            | applies to this operation:
;;           |            | 
;;           |            |   if x > 0 then
;;           |            |     p <- y
;;           |            |   else if x < 0 then
;;           |            |     p <- z
;;           |            |   else if x = 0 then
;;           |            |     p <- p + 4
;;           |            |   end if
;;   ------------------------------------------------------------------
;; 
;; Please remember that any value engaged in no affiliation with an
;; operative value does not contribute an effect, and, being a
;; no-operation, or NOP, simply increments the pointer P by the value
;; four (4).
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-08
;; 
;; Sources:
;;   [esolang2023miscmisc2]
;;   The Esolang contributors, "miscmisc2", July 9th, 2023
;;   URL: "https://esolangs.org/wiki/Miscmisc2"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype memory ()
  "The ``memory'' type defines the program memory, this being vector of
   integer cells, indexed commencing with the subscript zero (0), and
   dispanding in a bourneless manner towards the positive axis, as a
   dynamic ``integer'' vector."
  '(vector integer *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-memory ()
  "Creates and returns a new empty ``memory'' object."
  (the memory
    (make-array 0
      :element-type    'integer
      :initial-element 0
      :adjustable      T
      :fill-pointer    0)))

;;; -------------------------------------------------------

(defun make-initialized-memory (&rest initial-elements)
  "Creates and returns a new ``memory'' object whose first elements are
   populated by the INITIAL-ELEMENTS."
  (the memory
    (make-array (length initial-elements)
      :element-type     'integer
      :initial-contents initial-elements
      :adjustable       T
      :fill-pointer     T)))

;;; -------------------------------------------------------

(defun transfer-program-into-memory (memory code)
  "Transfers the miscmisc2 source CODE's characters, each converted into
   its respective character code, into the MEMORY, commencing from the
   index zero (0), and returns the modified MEMORY."
  (declare (type memory memory))
  (declare (type string code))
  (loop for token of-type character across code do
    (vector-push-extend (char-code token) memory))
  (the memory memory))

;;; -------------------------------------------------------

(defun all-zero-p (memory start-index)
  "Determines whether the MEMORY cells, starting from the inclusive
   START-INDEX, contains the value zero (0), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type memory memory))
  (declare (type fixnum start-index))
  (the boolean
    (or
      ;; Undiscovered cells are always assumed as zero (0).
      (not (array-in-bounds-p memory start-index))
      (not (null
        (loop
          for current-index
            of-type fixnum
            from    start-index
            below   (fill-pointer memory)
          always
            (zerop (aref memory current-index))))))))

;;; -------------------------------------------------------

(defun ensure-index (memory desiderated-index)
  "Ascertains the MEMORY's accommodation of an element at the
   DESIDERATED-INDEX by contingent appendage of the requisite
   zero-valued elements, and in any case returns the MEMORY."
  (declare (type memory memory))
  (declare (type fixnum desiderated-index))
  (loop while (<= (fill-pointer memory) desiderated-index) do
    (vector-push-extend 0 memory))
  (the memory memory))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the MEMORY cell value designated by the INDEX."
  (declare (type memory memory))
  (declare (type fixnum index))
  (ensure-index memory index)
  (the integer
    (aref memory index)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE In the MEMORY cell designated by the INDEX and
   returns the NEW-VALUE."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type fixnum  index))
  (ensure-index memory index)
  (setf (aref memory index) new-value)
  (the integer new-value))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory parser.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-surrounding-whitespaces (string)
  "Returns a new string based upon the input STRING with the surrounding
   whitespaces removed."
  (declare (type string string))
  (the string
    (string-trim '(#\Space #\Tab #\Newline) string)))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline))))))

;;; -------------------------------------------------------

(defun locate-next-whitespace (source start)
  "Proceeding from the START position into the SOURCE, returns the
   position of the next whitespace character, or responds with the
   SOURCE's length upon such an entity's lacuna."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun parse-memory (source)
  "Parses a sequence of zero or more whitespace-separated signed or
   unsigned integer values, representative of the initial program
   memory, and returns a ``memory'' representation thereof."
  (declare (type string source))
  (let ((normalized-source (remove-surrounding-whitespaces source)))
    (declare (type string normalized-source))
    (let ((start-position 0)
          (end-position   (locate-next-whitespace normalized-source 0))
          (final-position (length normalized-source)))
      (declare (type fixnum start-position))
      (declare (type fixnum end-position))
      (declare (type fixnum final-position))
      (the memory
        (apply #'make-initialized-memory
          (loop while (< start-position final-position) collect
            (prog1
              (parse-integer normalized-source
                :start start-position
                :end   end-position)
              (shiftf start-position end-position
                (locate-next-whitespace normalized-source
                  (min final-position
                    (1+ end-position)))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory builder.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode-program (command-tokens)
  "Generates the ``memory'' object whose initial elements are obtained
   from the COMMAND-TOKENS' character codes."
  (declare (type string command-tokens))
  (the memory
    (apply #'make-initialized-memory
      (map 'list #'char-code command-tokens))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (memory)
  "Executes the program commorant in the MEMORY and returns no value."
  (declare (type memory memory))
  
  (symbol-macrolet
      ((p (the integer (memory-cell-at memory    0)))
       (q (the integer (memory-cell-at memory    p)))
       (x (the integer (memory-cell-at memory (+ p 1))))
       (y (the integer (memory-cell-at memory (+ p 2))))
       (z (the integer (memory-cell-at memory (+ p 3)))))
    (declare (type integer p))
    (declare (type integer q))
    (declare (type integer x))
    (declare (type integer y))
    (declare (type integer z))
    
    (loop until (or (< p 1) (all-zero-p memory p)) do
      (case q
        ;; !
        (33
          (let ((modifies-pointer-p (not (null (zerop y)))))
            (declare (type boolean modifies-pointer-p))
            (setf (memory-cell-at memory y) x)
            (unless modifies-pointer-p
              (incf p 3))))
        
        ;; &
        (38
          (let ((modifies-pointer-p (not (null (zerop y)))))
            (declare (type boolean modifies-pointer-p))
            (setf (memory-cell-at memory y)
              (memory-cell-at memory x))
            (unless modifies-pointer-p
              (incf p 3))))
        
        ;; +
        (43
          (setf z (+ x y))
          (incf p 4))
        
        ;; ,
        (44
          (format T "~d " x)
          (incf p 2))
        
        ;; -
        (45
          (setf z (- x y))
          (incf p 4))
        
        ;; .
        (46
          (format T "~&>> ")
          (setf x (parse-integer (read-line)))
          (clear-input)
          (incf p 2))
        
        ;; ?
        (63
          (cond
            ((> x 0)
              (setf p y))
            ((minusp x)
              (setf p z))
            (T
              (incf p 4))))
        
        (otherwise
          (incf p 4)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-miscmisc2 (code)
  "Interprets the piece of miscmisc2 source CODE provided as a sequence
   of zero or more whitespace-separated signed or unsigned integers, and
   returns no value."
  (declare (type string code))
  (execute-program
    (parse-memory code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the ASCII character codes of the message "Hello world!".
(interpret-miscmisc2
  "1 44 72 44 101 44 108 44 108 44 111 44 32 44 119 44 111 44 114 44 108 44 100 44 33")

;;; -------------------------------------------------------

;; Print the ASCII character codes of the message "Hello world!".
(execute-program
  (make-initialized-memory
    1 44 72 44 101 44 108 44 108 44 111 44 32 44 119 44 111 44 114 44 108 44 100 44 33))

;;; -------------------------------------------------------

;; Repeating numeric cat program which terminates on an input of
;; zero (0).
(execute-program
  (make-initialized-memory
    1 46 0 38 2 17 38 2 10 63 0 16 13 33 0 0 44 0 33 1 0))

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The memory layout assumes the following conformation, where the
;; leftmost column enumerates the memory cell indices, the central one
;; the respective cell's dever, and the dextral rank its content.
;; 
;;   ------------------------------------------------------------------
;;    # | Role       | Value
;;   ---+------------+-------------------------------------------------
;;    0 | Pointer    | 0
;;   ---+------------+-------------------------------------------------
;;    1 | Input      | 46
;;   ..................................................................
;;    2 | Input(x)   | 0, later user input
;;   ---+------------+-------------------------------------------------
;;    3 | CopyRef    | 38
;;   ..................................................................
;;    4 | CopyRef(x) | 0, later copy of user input
;;   ..................................................................
;;    5 | CopyRef(y) | 10 (= index of output argument x)
;;   ---+------------+-------------------------------------------------
;;    6 | CopyRef    | 38
;;   ..................................................................
;;    7 | CopyRef(x) | 0, later copy of user input
;;   ..................................................................
;;    8 | CopyRef(y) | 12 (= index of jump argument x)
;;   ---+------------+-------------------------------------------------
;;    9 | Output     | 44
;;   ..................................................................
;;   10 | Output(x)  | 0, later copy of user input
;;   ---+------------+-------------------------------------------------
;;   11 | Jump       | 63
;;   ..................................................................
;;   12 | Jump(x)    | 0, later copy of user input
;;   ..................................................................
;;   13 | Jump(y)    | 9 (= index of output argument x)
;;   ..................................................................
;;   14 | Jump(z)    | 0 (= halts program with pointer = 0)
;;   ------------------------------------------------------------------
(execute-program
  (make-initialized-memory
    1
    46 0
    38 2 10
    38 2 12
    44 0
    63 0 9 0))
