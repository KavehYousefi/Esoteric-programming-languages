;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Track", invented by the Esolang user "Wh1teWolf".
;; 
;; Concept
;; =======
;; The esoteric programming language Track is described by a code in the
;; shape of a two-dimensional grid, composed of at most 30 cells width
;; and an arbitrary tally establishing the height, while all data is
;; maintained in a one-dimensional memory accommodating ten cells.
;; 
;; == TRACK CODE SPANS A GRID ==
;; A kenspeckle, though not completely forinsecal attribute of Track
;; resides in the design of its source code, which produces a
;; two-dimensional, navigable grid of characters. A limitation is
;; imposed on the horizontal axis, that is, the width, to less than or
;; equal to 30 characters, but no such march inflicts the height with
;; an encumbrance. Each of the 21 commands manifests in a single
;; character, and spaces introduce tolerated but negligible content,
;; intended for a utilization as fillers.
;; 
;; The grid is amenable to specifications via Cartesian co-ordinates:
;; The x-coordinate inquires into the horizontal axis, while the
;; vertical direction responds to the y-coordinate. The top-left corner
;; is designated by the x-y twain (0, 0), with linearly increasing
;; x-coordinate towards the right and y-coordinate towards the bottom.
;; 
;; == AN INSTRUCTION POINTER NAVIGATES THROUGH THE GRID ==
;; As the grid provides a static reticulation, a motile component must
;; imbue by its presence the program with animation: This touches the
;; bailiwick of the instruction pointer (IP). The instruction pointer's
;; wike occupies the navigation through the code grid and execution of
;; its command tokens, until a termination condition eventuates. In
;; order to apply to its requirement in mobility, this pointer must
;; incorporate two pieces of information:
;; 
;;   (1) The current location, that is, the (x, y) coordinate tuple
;;       specifiying the current code grid cell.
;;   (2) A direction along which to pursue across the grid.
;; 
;; == TRACK = GRID + POINTER ==
;; At the entry of a program, the instruction pointer is commorant in
;; the top-left location (0, 0) cell of the code grid, airted at the
;; right. It evaluates the current token, performing the associated
;; operation, which, among other things, may modify its direction, and
;; proceeds in its current orientation to the abutting cell.
;; 
;; Upon the instruction pointer's attempt to traverse beyond either the
;; left, top, or bottom margin, the program immediately terminates. If
;; the pointer infringes on the right edge, a more sophisticated
;; inquisition must be administered: In the case of the pointer's
;; residence in the desinent grid row, the program iterum ceases its
;; operation; however, with at least one row below, the pointer instead
;; relocates to the first column of the next row, simulating a
;; "linebreak".
;; 
;; == THE MEMORY: A TAPE OF TEN INTEGERS ==
;; The data castaldy in Track manifests itself in the accommodation of a
;; tape-like memory, compact of ten cells, and enumerated by the indices
;; or subscripts 0 to 9. Each such unit stores an arbitrary signed
;; integer value, amenable to certain instructions.
;; 
;; == THE MEMORY POINTER: A REFERENCE TO THE SELECTED CELL ==
;; Similar to the code grid, a pointer is ordained to operate on the
;; one-dimensional memory, known as its "memory pointer", and
;; initialized to that cell with the index zero (0), purporting to
;; maintain a reference to the currently selected cell. Destitute of the
;; means to undertake a free navigation across the memory, as is the
;; case with the very similar esoteric programming language "brainfuck"
;; by Urban Mueller, any of the ten cells must be indexed directly by
;; its subscript using one of the ten euonymous commands "0", "1", "2",
;; "3", "4", "5", "6", "7", "8", or "9".
;; 
;; 
;; Architecture
;; ============
;; A Track program operates on a tape-like memory of exactly ten cells,
;; enumerated with the indices zero (0) to nine (9). Each cell stores
;; a signed integer number of any magnitude, that is an occupant of
;; the range [-infinity, +infinity]. A pointer, agnominated the
;; "memory pointer", memorizes the currently selected cell.
;; 
;; 
;; Data Types
;; ==========
;; The language employs a very constricted type system, compact of
;; integer numbers for internal, and a few external, purposes, as well
;; as characters along the user-program interface.
;; 
;; == INTEGER DATA DEFINES THE STATE ==
;; The repository of all data, the memory, comprises ten cells which
;; solely store integer numbers of any sign and size. The scant
;; allowance of arithmetic operations, exhausted already by augmentation
;; and deducation, applies to these cells' bailiwick. Beside
;; manipulations and indagations, the language offers the capability to
;; prompt as well as print cell data either in integer or character
;; form.
;; 
;; == CHARACTER DATA TARGETS INPUT AND OUTPUT ==
;; The second component of the type system, characters are employed
;; along the interface betwixt the user and the program. This role
;; represents a rather superficial occupation, as inputs find themselves
;; in immediate translation into numeric character codes for storage in
;; a cell, and, in athwart airt, printed ASCII content proceeds from
;; integer cell data transiently construed in this particular fashion.
;; 
;; 
;; Syntax
;; ======
;; Track's character set is bifurcated into one-character commands and
;; whitespaces, with all content expressed in ASCII entities.
;; 
;; == EACH INSTRUCTION IS DESCRIBED BY A SINGLE CHARACTER ==
;; Each of the twenty-one instructions in Track is denominated by a
;; single character from the ASCII repertoire.
;; 
;; == SPACES CONTRIBUTE STRUCTURAL SUPPORT ==
;; The space character, while expressively tolerated, associates with
;; no operation at all, instead being an avail for accomplishing the
;; desiderated source code shape; in corollary, its encounter is
;; responded by the instruction pointer with a simple skipping.
;; 
;; == NEWLINES PARTITION THE CODE INTO ROWS ==
;; Linebreaks constitute a significant occurrence in the code,
;; separating rows from each other. Foreby their usage as topological
;; instruments, they do not convey any further interpretation.
;; 
;; 
;; Instructions
;; ============
;; Track's circumference encompasses twenty-one commands, including such
;; for the navigation in the two-dimensional source code, memory cell
;; manipulation, and input/output operations.
;; 
;; == TYPES OF INSTRUCTIONS ==
;; The instruction set can be discriminated into several categories,
;; mediated by the intention of its effect or the targeted component:
;; 
;;   - Navigational commands
;;   - Arithmetic commands
;;   - Input/Output commands
;; 
;; Navigational commands are apportioned the onus of helming the
;; instruction pointer's motion through the source code. The roster
;; defines: ">", "<", "^", "V", and "@".
;; 
;; Arithmetic commands function as sustinent actuators in the selection
;; and modification of cells, with the former deed, while rather remote
;; in association, yet ligated with consanguinity to the latter's
;; successful conclusion. As members tally: "0", "1", "2", "3", "4",
;; "5", "6", "7", "8", "9", "+", and "-".
;; 
;; Input/Output commands accommodate a conduit betwixt the user and the
;; program, proceeding bilaterally, and tolerating as a token of
;; currency either a character or a number. These instructions partake
;; of the array ".", ":", ",", and ";".
;; 
;; == OVERVIEW ==
;; Track's instruction set constitutes a composition of 21 members, each
;; of these will be disquisited in a cursory manner by the below table:
;; 
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;    >      | Sets the instruction pointer direction to right.
;;   ..................................................................
;;    <      | Sets the instruction pointer direction to left.
;;   ..................................................................
;;    ^      | Sets the instruction pointer direction to up.
;;   ..................................................................
;;    V      | Sets the instruction pointer direction to down.
;;   ..................................................................
;;    @      | Sets the instruction pointer direction based upon the
;;           | current cell's value:
;;           |   Current cell value | New instruction pointer direction
;;           |   -------------------+----------------------------------
;;           |    1                 | right
;;           |    2                 | left
;;           |    3                 | down
;;           |    4                 | up
;;           |    otherwise         | right
;;   ..................................................................
;;    +      | Increases the current cell's value by one.
;;   ..................................................................
;;    -      | Decreases the current cell's value by one.
;;   ..................................................................
;;    0      | Moves the memory pointer to the cell at the index 0.
;;   ..................................................................
;;    1      | Moves the memory pointer to the cell at the index 1.
;;   ..................................................................
;;    2      | Moves the memory pointer to the cell at the index 2.
;;   ..................................................................
;;    3      | Moves the memory pointer to the cell at the index 3.
;;   ..................................................................
;;    4      | Moves the memory pointer to the cell at the index 4.
;;   ..................................................................
;;    5      | Moves the memory pointer to the cell at the index 5.
;;   ..................................................................
;;    6      | Moves the memory pointer to the cell at the index 6.
;;   ..................................................................
;;    7      | Moves the memory pointer to the cell at the index 7.
;;   ..................................................................
;;    8      | Moves the memory pointer to the cell at the index 8.
;;   ..................................................................
;;    9      | Moves the memory pointer to the cell at the index 9.
;;   ..................................................................
;;    .      | Outputs the ASCII character associated with the current
;;           | cell's value.
;;   ..................................................................
;;    :      | Outputs the current cell's value verbatim, that is, as
;;           | an integer number.
;;   ..................................................................
;;    ,      | Prompts the user for an ASCII character and stores its
;;           | character code in the current cell.
;;   ..................................................................
;;    ;      | Prompts the user for an integer number and stores it in
;;           | the current cell.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Track's documentation, while meticulous in the elucidation of the
;; source code processing, command description, and provision of
;; examples, exercises some reticence anenst several details.
;; 
;; == HOW DOES THE INSTRUCTION POINTER REACT ON THE EDGES? ==
;; The instruction pointer's advance depends upon an intrinsically
;; managed direction for navigation inside of the code grid. It remains,
;; on the other hand, no oristic disquisition of the behavior adduced
;; regarding the grid boundaries. In particular, if a dextrally
;; proceeding instruction pointer collides with the right margin, the
;; causatum eludes explicitness.
;; 
;; The "99 bottles of beer" ostends a dependence upon a "linebreak",
;; that is, an automatic relocation from the desinent column of the
;; current row to the first of the next; however, the same piece of
;; epideictic code, such as any other, requires a termination criterion,
;; incarnated in the impotence of further advancing. Sustinent to these
;; considerations, the maximum horizontal grid expansion is expressively
;; stated as 30 cells.
;; 
;; A consectary of this, an attempt of transgressing the rightmost side
;; incites a "linebreak", relocating the instruction pointer to the
;; first column of the following line, if such exists; otherwise the
;; program will be terminated. Any other collision between this pointer
;; and an edge will immediately terminate the program.
;; 
;; 
;; Implementation
;; ==============
;; This realization of the Track programming language is based in
;; paravaunt consideration upon lucidity. It models the code grid using
;; a hash table in the vein of a sparse matrix, relaying the data
;; management to a fixed-sized array. Intent on an accommodation of
;; some liberty for experiments, the maximum grid width of 30 cells is
;; neither checked nor imposed.
;; 
;; == THE CODE GRID AS A HASH TABLE ==
;; The capital entity of the system, the source code grid, finds its
;; representation in an eponymous class ``Grid'', a composite of the
;; occupied dimensions and the character cells. A direct translation of
;; the mathematical structure would certainly be accomplished by avail
;; of a two-dimensional character array --- despite this inclination,
;; the contingency for sparsity, referring to the expected presence of
;; ineffectuous spaces for the sake of achieving a particular code
;; shape, justifies a data structure pertinent to sparse matrix
;; implementations: in this case, the hash table. Each of its entries
;; assigns to a two-dimensional location specifier a character. The
;; keys are provided in the form of a dedicated ``Location'' class, the
;; constituents of which encompass merely the x- and y-coordinates.
;; Inquests into nonextant cells inside of the grid bounds are replied
;; with a default space entity, whereas any content foreby the marches
;; delivers a ``NIL'' sentinel.
;; 
;; At the start of the Track source interpretation, the one-dimensional
;; code string is transformed into a new ``Grid'' instance, with the
;; linebreaks incumbent as significations of new rows. The longest
;; subsequence betwixt two linebreaks, or a linebreak and the end of
;; the input string, representing a row, determines the grid width.
;; 
;; == THE INSTRUCTION POINTER AS A LOCATION ==
;; The instruction pointer itself does not amount to anything besides a
;; simple ``Location''. The direction, typified by the same name,
;; is incorporated separately into the implementation, while its
;; interrelations with the instruction pointer, of course, persist,
;; helming and modifying the latter's progress in conjunction with the
;; former.
;; 
;; == MEMORY ==
;; The stringently fixed characteristic inherent to the memory,
;; implicated by the restriction to ten cells, vindicates its
;; manifestation as a static array holding integer elements. The index
;; into the current cell, the designation of which is attended to by a
;; pointer, is accomplished by a simple integer datum in the congruent
;; range [0, 9].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-16
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Track"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which conform to the KEY-TYPE and the values to
   the VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type defines the valid directions for the movement
   of the instruction pointer in the Track code."
  '(member :left :right :up :down))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (&optional (x 0) (y 0))))
  "A ``Location'' represents a two-dimensional position specifier
   pertaining to the Cartesian co-ordinates x and y."
  (x 0 :type fixnum)
  (y 0 :type fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Grid
  "The ``Grid'' class models a two-dimensional array of characters,
   amenable to queries by Cartesian co-ordinates."
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  (cells (make-hash-table :test #'equalp)
         :type (hash-table-of Location character)))

;;; -------------------------------------------------------

(defun grid-at (grid location)
  "Returns the character in the GRID specified by the LOCATION, or
   ``NIL'' if outside of its bounds."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the (or null character)
    (when (and (<= 0 (location-x location) (1- (grid-width  grid)))
               (<= 0 (location-y location) (1- (grid-height grid))))
      (gethash location (grid-cells grid) #\Space))))

;;; -------------------------------------------------------

(defun build-code-grid (code)
  "Creates and returns a ``Grid'' based upon the CODE."
  (declare (type string code))
  (let ((grid (make-grid)))
    (declare (type Grid grid))
    (loop
      with x         of-type fixnum    =      0
      with y         of-type fixnum    =      0
      for  character of-type character across code
      do
      (when (zerop (grid-height grid))
        (setf (grid-height grid) 1))
      (case character
        (#\Newline
          (incf (grid-height grid))
          (setf x 0)
          (incf y 1))
        (otherwise
          (setf (gethash (make-location x y) (grid-cells grid))
                character)
          (setf (grid-width grid)
                (max (1+ x) (grid-width grid)))
          (incf x 1))))
    (the Grid grid)))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid                            grid))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "~&~d x ~d" (grid-width grid) (grid-height grid))
  (let ((location (make-location 0 0)))
    (declare (type Location location))
    (loop for y of-type fixnum from 0 below (grid-height grid) do
      (format stream "~&")
      (loop for x of-type fixnum from 0 below (grid-width grid) do
        (setf (location-x location) x)
        (setf (location-y location) y)
        (format stream "~a" (grid-at grid location))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Track (code)
  "Interprets the piece of Track CODE and returns no value."
  (declare (type string code))
  (let* ((grid           (build-code-grid code))
         ;; Instruction pointer (IP) location.
         (location       (make-location 0 0))
         ;; The instruction pointer direction.
         (direction      :right)
         (character      (grid-at grid location))
         
         (memory         (make-array 10
                           :element-type    'integer
                           :initial-element 0))
         (memory-pointer 0))
    (declare (type Grid                grid))
    (declare (type Location            location))
    (declare (type direction           direction))
    (declare (type (or null character) character))
    (declare (type (vector integer 10) memory))
    (declare (type (integer 0 9)       memory-pointer))
    
    (flet ((advance ()
            "Moves the instruction pointer's LOCATION one cell into the
             current DIRECTION, updating the CHARACTER in the course,
             and returning no value."
            (case direction
              (:left     (decf (location-x location)))
              (:right    (incf (location-x location)))
              (:up       (decf (location-y location)))
              (:down     (incf (location-y location)))
              (otherwise (error "Invalid advance direction: ~s."
                           direction)))
            
            ;; Shall the instruction pointer perform a "linebreak" from
            ;; the right margin to the leftmost cell in the next lower
            ;; row?
            (when (and (eq direction :right)
                       (>= (location-x location)
                           (grid-width grid))
                       (<  (location-y location)
                           (1- (grid-height grid))))
              (setf (location-x location) 0)
              (incf (location-y location) 1))
            
            (setf character (grid-at grid location))
            (values)))
      
      (loop do
        (cond
          
          ((null character)
            (loop-finish))
          
          ((char= character #\Space)
            (advance))
          
          ((char= character #\>)
            (setf direction :right)
            (advance))
          
          ((char= character #\<)
            (setf direction :left)
            (advance))
          
          ((char= character #\^)
            (setf direction :up)
            (advance))
          
          ((char= character #\V)
            (setf direction :down)
            (advance))
          
          ((char= character #\@)
            (setf direction
              (case (aref memory memory-pointer)
                (1         :right)
                (2         :left)
                (3         :down)
                (4         :up)
                (otherwise :right)))
            (advance))
          
          ((char= character #\+)
            (incf (aref memory memory-pointer))
            (advance))
          
          ((char= character #\-)
            (decf (aref memory memory-pointer))
            (advance))
          
          ((char= character #\.)
            (write-char (code-char (aref memory memory-pointer)))
            (advance))
          
          ((char= character #\:)
            (format T "~d" (aref memory memory-pointer))
            (advance))
          
          ((char= character #\,)
            (format T "~&Please input a character: ")
            (let ((input (read-char)))
              (declare (type character input))
              (clear-input)
              (setf (aref memory memory-pointer) (char-code input)))
            (advance))
          
          ((char= character #\;)
            (format T "~&Please input a number: ")
            (let ((input (parse-integer (read-line))))
              (declare (type integer input))
              (clear-input)
              (setf (aref memory memory-pointer) input))
            (advance))
          
          ((digit-char-p character)
            (setf memory-pointer (digit-char-p character))
            (advance))
          
          (T
            (error "Invalid character ~s at location ~a."
              character location))))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of additional operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-code-for-text (text &key (destination T)
                                    (grid-width  30))
  "Creates a Track program capable of printing the TEXT and writes it to
   the DESTINATION, extending each line to a maximum GRID-WIDTH tally of
   columns."
  (declare (type string                          text))
  (declare (type (or null (eql T) stream string) destination))
  (declare (type (integer 1 *)                   grid-width))
  (if destination
    (let ((column 0))
      (declare (type (integer 0 *) column))
      (flet ((append-character (character)
              "Writes the CHARACTER to the DESTINATION, taking heed not
               to trespass the GRID-WIDTH, and returns no value."
              (declare (type character character))
              (when (>= column grid-width)
                (terpri destination)
                (setf column 0))
              (write-char character destination)
              (incf column)
              (values)))
        (loop
          with last-char-code of-type fixnum    =      0
          for  character      of-type character across text
          for  character-code of-type fixnum    =      (char-code character)
          do
          (let ((char-code-offset (- character-code last-char-code)))
            (declare (type fixnum char-code-offset))
            (cond
              ((plusp char-code-offset)
                (loop repeat char-code-offset do
                  (append-character #\+)))
              ((minusp char-code-offset)
                (loop repeat (- char-code-offset) do
                  (append-character #\-)))
              (T
                NIL)))
        (append-character #\.)
        (setf last-char-code character-code))))
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (get-code-for-text text
          :destination output
          :grid-width  grid-width)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
(interpret-Track
"V<
>^")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Track ",.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Track
">,.V
^  <")

;;; -------------------------------------------------------

;; Counting from 1 to 10 (looping counter), version 1.
(interpret-Track
"V               V       <     
>1++++++++++++++>1-0+:1@^  ")

;;; -------------------------------------------------------

;; Counting from 1 to 10 (looping counter), version 2.
(interpret-Track
"++++++++++++++>0-1+:0@V       
              ^       <       ")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; This example takes advantage of the fact that the command "@"
;; redirects the instruction pointer downward if the current cell value
;; equals 3, and upward if it amounts to 4.
;; 
;; Concept:
;;   Store the user input number in the first cell
;;   Print the user input number
;;   Increase the cell value by three
;;   If the cell value equals 3, move down, terminating the program
;;   If the cell value equals 4, redirect the control flow into a printing loop
;; 
;; Pseudocode:
;;   memory[0] <- inputNumber()
;;   printNumber()
;;   memory[0] <- memory[0] + 3
;;   if memory[0] = 3 then
;;     moveDown()
;;   else
;;     moveUp()
;;     repeatedly print "1"
;;   end if
;; 
(interpret-Track
"V     >--->:>V
>;:+++@   ^  <")

;;; -------------------------------------------------------

;; 99 bottles of beer.
;; 
;; Memory layout:
;;   cells[0] = Counter from 99 down to 0, designating the current
;;              number of bottles.
;;   cells[1] = Prints text of the lyrics, excluding spaces, commas,
;;              and linebreaks.
;;   cells[2] = Prints space and comma between words.
;;   cells[3] = Direction control for repeated iteration; starts from
;;              104 and counts down to 4, which finally breaks the loop.
;;   cells[4] = Prints linebreaks; "4.." means: two new lines follow
;;              each stanza.
;; 
(interpret-Track
"3++++++++++++++++++++++++++++ 
+++++++++++++++++++++++++++++ 
+++++++++++++++++++++++++++++ 
+++++++++++++++++0            
+++++++++++++++++++++++++++++ 
+++++++++++++++++++++++++++++ 
+++++++++++++++++++++++++++++ 
++++++++++++                  
                              
2++++++++++++++++++++++++++++ 
++++0                         

V                           0<
>:2.1++++++++++++++++++++++++ 
+++++++++++++++++++++++++++++ 
+++++++++++++.+++++++++++++.+ 
++++..--------.-------.++++++ 
++++++++.2.1----.---------.2. 
1----.+++..+++++++++++++.2.1- 
--.-.2.1++++++.------------.- 
--.2.1++++++++++++++++++.---- 
------------------.++++++++++ 
+..2++++++++++++.------------ 
.0:2.1----------.++++++++++++ 
+.+++++..--------.-------.+++ 
+++++++++++.2.1----.--------- 
.2.1----.+++..+++++++++++++.2 
++++++++++++++.-------------- 
4++++++++++.1++.------------- 
------.++++++++++.------.2.1+ 
+++++++++.-.---------.2.1-.++ 
+++++++++.++++++++.---------. 
2.1++.---------------.+++++++ 
+++++++++++..2.1----------.++ 
+++++++++.2.1---------------- 
---.+++++++++++++++++.---.+++ 
+++.-------.----------.2+++++ 
+++++++.------------.0-:2.1-- 
.+++++++++++++.+++++..------- 
-.-------.++++++++++++++.2.1- 
---.---------.2.1----.+++..++ 
+++++++++++.2.1---.-.2.1+++++ 
+.------------.---.2.1+++++++ 
+++++++++++.----------------- 
-----.+++++++++++..2+++++++++ 
+++++.--------------4..------ 
----1------------------------ 
----------------------------- 
-----------------------       
V>V<                          
>3-@                         ^")

;;; -------------------------------------------------------

;; Print to the standard output the Track code necessary to output
;; "Hello, World!".
(get-code-for-text "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Returns a string representing the Track code necessary to output
;; "Hello, World!".
(get-code-for-text "Hello, World!" :destination NIL)

;;; -------------------------------------------------------

;; Create the Track code for printing "Hello, World!" and interpret it
;; in order to display the text.
(interpret-Track
  (get-code-for-text "Hello, World!" :destination NIL))
