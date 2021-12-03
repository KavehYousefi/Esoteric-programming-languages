;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "BF+BF", invented by the Esolang user "IAM".
;; 
;; Concept
;; =======
;; BF+BF partakes of a double heritage: that of "Extended Brainfuck
;; Type I" and "Befunge", both of which themselves esoteric in nature,
;; with the former defining the architecture, and the latter its
;; two-dimensional Cartesian layout, which responds to a more
;; sophisticated navigation than strictly linear specimens. Its commands
;; originate from both sources.
;; 
;; == [B]E[F]UNGE + [B]RAIN[F]UCK = BF+BF ==
;; It may be a tenable conjecture to trace the agnomination to the
;; significant letters inhabiting its parentage, with one "BF" referring
;; to "BeFunge" and the other to "BrainFuck", or vice versa. A
;; second, subordinate explanation involves the duplication of "BF" as
;; a hint to the equinumerant augmentation of dimensions as compared to
;; Extended Brainfuck Type I.
;; 
;; == BRAINFUCK SUPPLIES THE ARCHITECTURE ==
;; Extended Brainfuck Type I itself was begotten by Urban Mueller's
;; "brainfuck", expanding the basic memory model by a singular storage,
;; and augmenting the octuple instruction set by nine members. BF+BF
;; appropriates all instances of this extended 17 pieces set almost
;; verbatim, borrowing a slightly less tally from its inspiration's
;; second moeity. The increase in dimensions directly influences BF+BF's
;; assumption of this lineage, in the course of which the bilaterally
;; infinite linear tape is modified into a two-dimensional grid, the
;; cells of which expand unboundedly to the right and bottom, but not
;; to the left and top. A corollary of the extension, commands have been
;; introduced to helm the traversal across the data grid in all four
;; cardinal directions. The type specification, resolving to integers
;; destitute of any confinement, have been equally transported.
;; 
;; == BEFUNGE SUPPLIES THE FORM ==
;; While the data storage responds to brainfuck, Befunge's provision
;; manifests most conspicuously in the topological concepts, with a
;; portion of instructions also added to the supply.
;; 
;; The provenance language operates on a two-dimensional grid of ASCII
;; characters --- aiblins the incipient or at least a very early
;; proponent of such a design ---, the instruction pointer of which
;; incorporates not only its row and column indices, but also a
;; direction into which the next motion will goad it. It is equally
;; significant to note that Befunge relays its data management to a
;; stack --- a choice completely neglected by BF+BF for brainfuck's
;; tape. BF+BF mimics the reseau meticulously, albeit discrepancies in
;; the command range exhibit a slightly modified, dioristic appearance.
;; 
;; == THE INSTRUCTION POINTER NAVIGATES THROUGH THE CODE GRID ==
;; The instruction pointer, the responsible unit for the execution of
;; a program, is initially set to the top-left corner (0, 0) of the
;; reticulation for the code, and oriented to the right. Its progress
;; continuously advances along the pointer's direction, transitioning
;; from a cell to its neighbor. Each selected cell's character, lest
;; being a space, represents an instruction, which is executed by the
;; interpreter ere the pointer's departure. Navigational instructions
;; airt the orientation, which affects the axis to follow. The control
;; flow, if not captured in an infinite configuration, terminates by
;; encountering the "@" command.
;; 
;; == THE DATA POINTER OPERATES ON THE DATA GRID ==
;; A second Cartesian arrangement, the data grid, is conjoined in a
;; conceptual matter, but essentially dislodged from the code array.
;; This data respository, too, is initialized at the top-left position
;; (0, 0), but its pointer, the data pointer, remains destitute of an
;; intrinsic direction --- migrations along the structure must be stated
;; in explicit terms by mediation of the respective commands.
;; 
;; == THE STORAGE PROVIDES AN AUXILIARY VALUE ==
;; In addition to the data array, a scalar value, known as the storage,
;; provides the means for a further integer datum to participate in
;; collaborations and exchange with the tape, a facility of particular
;; avail in the binary operations "AND", "OR", and "XOR".
;; 
;; 
;; Architecture
;; ============
;; The language's architecture is edified upon two distinct grids not
;; entwined in any correlation, being yet bound to a kinship by
;; their attributes: the code grid and the data grid. Their purposes
;; vindicate a segregation of the twain, with the code grid being the
;; bailiwick of the "Syntax" section, whereas the data analogue shall
;; receive its disquisition in this locus.
;; 
;; == DATA GRID ==
;; BF+BF reduplicates brainfuck's memory model based upon linearly
;; arranged cells, now extending into two axes, thus spanning a
;; Cartesian grid --- or, likewise, a matrix --- of integer numbers
;; permissive to any magnitude and sign. The grid is infinite to the
;; right and bottom.
;; 
;; == STORAGE ==
;; Equivalent to its data specification, yet eloigned from the grid in a
;; physical as well a logical notion, the storage offers a scalar
;; placeholder for an arbitrary integer value. The implication
;; emanating from this simplicity designates no particular structure,
;; apart from a simple variable, as necessitated for its economy.
;; 
;; 
;; Data Types
;; ==========
;; BF+BF operates primarily on integer numbers of unbounded magnitude,
;; resorting to character data solely for the sake of program-user
;; communications. The castaldy of states is delegated to a partially
;; infinite grid and an insular scalar storage.
;; 
;; == INTEGERS ARE THE INTERNAL ACTORS ==
;; Both the data grid cells and the storage unit constitute a conditory
;; for integer numbers devoid of any particular restriction, that is,
;; admitted as occupants of the range [-infinity, +infinity]. All
;; instructions not dedicated to the input/output purposes align with
;; the access and manipulation of this type.
;; 
;; == CHARACTERS MEDIATE BETWIXT THE SYSTEM AND THE USER ==
;; The relevance of characters does not rise beyond the interface of
;; the program and the user as specified by the input/ouput facilities.
;; Translations from one realm to the other are installed by two
;; commands' avail.
;; 
;; == MEMORY ==
;; As stated before, the data is stored in a two-dimensional array,
;; or Cartesian grid, of unbounded integer cells.
;; 
;; == STORAGE ==
;; Adhering to the nature of the memory cells, the storage instantiates
;; a single datum of the same arbitrary integer.
;; 
;; 
;; Syntax
;; ======
;; A BF+BF program consists of a two-dimensional grid of ASCII
;; characters, encompassing solely valid instructions. Linebreaks and
;; spaces establish the only exception, as the former's statement
;; demarcates the desinence of one line and the beginning of another,
;; while the latter corresponds to a numeric value of zero, designating
;; its utility as a filler for cells requisite to ascertain the
;; reticulate structure. Any further content alien to the official
;; instruction set is inflicted with proscription and its presence will
;; instigate an error.
;; 
;; An instruction pointer, located initially at the cell (0, 0), and
;; directed rightwards, consumes the code grid commands by moving
;; along its orientation from a cell to its immediate near-dweller.
;; Certain instructions modify its orientation, which in turn incites a
;; redirection of the pointer during its navigation along the code.
;; 
;; The coefficiency of the code structure, vianding the topology, and
;; the instruction semantics, among other aspects responsible for the
;; control flow, produce a language whose shape is a vehicle to its
;; functionality; that is, both layout and navigation can be harnessed
;; in order to realize effects. Callid handling empowers the programmer
;; to implement a topology which might be of artistic vallidom, as much
;; as of fendy effectuousness. An example of the latter's
;; administration, an iteration using "[" and "]" can be substituted
;; by a control flow that by redirecting the instruction pointer at the
;; end of a program to its incipiency simulates repetitions. Provided as
;; an example, the cat program, which repeatedly inquires the user for a
;; character and prints selfsame to the standard output, may naturally
;; be implemented using the traditional iterative solution
;;   1[,.1]@
;; However, the potence inhabiting the program design homologates the
;; following equipollent alternative, conjoining in its functionality
;; a visual clue to its ouroboros-like conception:
;;   R,.D
;;   .  ,
;;   ,  .
;;   U.,L
;; 
;; 
;; Instructions
;; ============
;; BF+BF's command repertoire constitutes an amalgam of the extended
;; brainfuck variant and Befunge's, tallying in complete a cardinality
;; of 31 members.
;; 
;; == TYPES OF INSTRUCTIONS ==
;; These facilities can be categorized into a hierarchy with respect to
;; their intentions:
;;   - Operational instructions
;;     - Arithmetic instructions
;;     - Logical instructions
;;     - Logistic instructions
;;   - Input/Output instructions
;;   - Navigational instructions
;;     - Code grid navigation
;;     - Data grid navigation
;; 
;; OPERATIONAL INSTRUCTIONS occupy the bailiwick of manipulations on
;; the data grid cells or the storage. A tripartite division is applied
;; into ARITHMETIC INSTRUCTIONS, specialized in the modification of such
;; a place either by mathematical means or by direct setting; the
;; LOGICAL INSTRUCTIONS, based upon bitwise operators; and the LOGISTIC
;; INSTRUCTIONS, a particular case that engages in the transfer between
;; a data grid cell and the storage.
;; 
;; INPUT/OUTPUT INSTRUCTIONS provide the means for communicating betwixt
;; the system and the user in a bilateral fashion, with a conduit either
;; transporting a character into the system or translating a cell value
;; to the display.
;; 
;; NAVIGATIONAL INSTRUCTIONS either manipulate the consumption of the
;; source code or the selection of a cell, the former species of which
;; effects the instruction pointer, describing the CODE GRID NAVIGATION,
;; while the second subcategory refers to the data grid pointer in the
;; substance of the DATA GRID NAVIGATION.
;; 
;; == OVERVIEW ==
;; The following apercu designates each command in conjunction with its
;; effect and the provenance language:
;; 
;;   Command | Description
;;   --------+---------------------------------------------------------
;;    <      | Moves the data pointer one cell to the right.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    >      | Moves the data pointer one cell to the left.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    A      | Moves the data pointer one cell to up.
;;   ..................................................................
;;    V      | Moves the data pointer one cell to up.
;;   ..................................................................
;;    v      | Synonym of the "V" command, which see.
;;   ..................................................................
;;    +      | Increments the data cell at the pointer by one.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    -      | Decrements the data cell at the pointer by one.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    .      | Prints the ASCII character associated with the memory
;;           | cell value ath the pointer. An error occurs if the cell
;;           | value does not conform to a valid ASCII character code.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    ,      | Prompts the user for an ASCII character and stores its
;;           | character code in the memory cell at the pointer. An
;;           | error occurs if the received character cannot be matched
;;           | against the ASCII repertoire.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    [      | If the value of the cell at the data pointer equals
;;           | zero (0), moves the instruction pointer in the code
;;           | along its current direction to the character immediately
;;           | following the matching "]", given the correct nesting
;;           | level. An error occurs if no matching "]" can be found.
;;           | If the cell value differs from 0, the instruction
;;           | pointer simply moves to the command following this "[".
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    ]      | If the value of the cell at the data poiner does not
;;           | equal zero (0), moves the instruction pointer in the
;;           | code opposite to its current direction to the character
;;           | immediately following the matching "[", given the
;;           | the correct nesting level. An error occurs if no
;;           | matching "[" can be found. If the cell value equals 0,
;;           | the instruction pointer simply moves to the command
;;           | following this "]".
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    $      | Sets the value in the storage to the value of the cell
;;           | at the data pointer.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    !      | Sets the value of the cell at the data pointer to the
;;           | value in the storage.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    {      | Performs a single logical left shift on the value of
;;           | the cell at the data pointer.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    }      | Performs a single logical right shift on the value of
;;           | the cell at the data pointer.
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    ~      | Sets the value of the cell at the data pointer to its
;;           | bitwise NOT, that is:
;;           |   cells[dataPointer] <- not(cells[dataPointer])
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    ^      | Sets the value of the cell at the data pointer to the
;;           | result of itself XOR-combined with the value in
;;           | the storage, that is:
;;           |   cells[dataPointer] <- cells[dataPointer] XOR storage
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    &      | Sets the value of the cell at the data pointer to the
;;           | result of itself AND-combined with the value in
;;           | the storage, that is:
;;           |   cells[dataPointer] <- cells[dataPointer] AND storage
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    |      | Sets the value of the cell at the data pointer to the
;;           | result of itself OR-combined with the value in
;;           | the storage, that is:
;;           |   cells[dataPointer] <- cells[dataPointer] OR storage
;;           | This instruction has been inherited from Extended
;;           | Brainfuck Type I.
;;   ..................................................................
;;    0      | Sets the value of the cell at the pointer to 0.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    1      | Sets the value of the cell at the pointer to 1.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    2      | Sets the value of the cell at the pointer to 2.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    3      | Sets the value of the cell at the pointer to 3.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    4      | Sets the value of the cell at the pointer to 4.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    5      | Sets the value of the cell at the pointer to 5.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    6      | Sets the value of the cell at the pointer to 6.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    7      | Sets the value of the cell at the pointer to 7.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    8      | Sets the value of the cell at the pointer to 8.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    9      | Sets the value of the cell at the pointer to 9.
;;           | This instruction has been inherited from Befunge.
;;   ..................................................................
;;    U      | Sets the instruction pointer's direction to up.
;;           | This instruction mimics Befunge's "^".
;;   ..................................................................
;;    D      | Sets the instruction pointer's direction to up.
;;           | This instruction mimics Befunge's "v".
;;   ..................................................................
;;    L      | Sets the instruction pointer's direction to up.
;;           | This instruction mimics Befunge's "<".
;;   ..................................................................
;;    R      | Sets the instruction pointer's direction to up.
;;           | This instruction mimics Befunge's ">".
;;   ..................................................................
;;    @      | Terminates the program.
;;           | This instruction has been inherited from Befunge and
;;           | Extended Brainfuck Type I.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The original BF+BF specification author admits in unambiguous
;; diction the laconicism administered to the elucidations; the
;; concluding lacunae thus constitute an object of apprehension. A
;; selection of the same shall be subjected to disquisition, conceiting
;; no comprehensiveness.
;; 
;; == DO BOTH GRIDS EXTEND INFINITELY? ==
;; Both the code grid and the data grid are included in the claim of an
;; infinite expansion to the right and bottom sides. While the latter
;; structure may benefit from this liberty, a definition of this ilk
;; displays a rather nocent influence upon the code array: Once having
;; entered the adit to the unbounded space, the program will extend with
;; unbridled inertia, never to cease. Furthermore, as one grid might be
;; confined excessively, the other could be construed as being
;; encumbered with arbitrariness, as an advancing of the memory into
;; the left and top spheres does not bear the potentials to inflict any
;; obvious difficulties. Natheless, this documentation concords with the
;; original writ.
;; 
;; == HOW SHALL THE "^" COMMAND BE DISAMBIGUATED? ==
;; The confluence of the two instruction sets received from its
;; inspirations, Befunge and Extended Brainfuck Type I, have been
;; adopted verbatim in most cases --- a fact whose benison revolves in
;; the token "^" (circumflex), as its role as a data pointer redirection
;; to the top, similar to Befunge's decree on the program counter,
;; collides with its interpretation as the logical "XOR" operator of
;; Extended Brainfuck Type I. The original document precedes the
;; former's declaration, but assures the latter's equivalency with the
;; brainfuck strain. Because of this ambivalence it has been chosen to
;; supersede the data grid oriented sense with the majuscular letter
;; "A", a shape whose outline approximates the predecessor rather well
;; and dovetails with its opposite "V"; concomitantly, "^" perpetuates
;; its brainfuck construe in being the "XOR" combinator.
;; 
;; 
;; Implementation
;; ==============
;; The creation of an interpreter for BF+BF implies the necessity of
;; catering for several challenges:
;;   (1) The one-dimensional source code must be transliterated into
;;       a two-dimensional code array with the contingence of infinite
;;       expansion.
;;   (2) An instruction pointer must possess the competence of
;;       navigation through the code grid.
;;   (3) The data array, equally dynamic as its operative counterpart,
;;       must manifest in some form.
;;   (4) A data pointer must be provided to access and manipulate the
;;       data grid.
;; 
;; == THE CODE ARRAY: A HASH TABLE AS A SPARSE MATRIX ==
;; Sparse matrices constitute a proven warkloom for representing
;; matrices whose dimensions exhibit nimiety in size, by which
;; characteristic an allocation in the vein of a static array defies
;; rationality. The code grid partakes of very similar requirements,
;; and hence is qualified for the reception of the same treatment. In
;; this case, a hash table accommodates its services, with the keys
;; being the Cartesian coordinates (x, y), encapsulated in a class
;; ``Location'', and associated with a single character, the same
;; defaults to the space (" ") if in the infinite region. The source
;; code is preprocessed and transformed into a sparse matrix of the
;; class ``Grid'', with linebreaks signifying the termination of a row.
;; 
;; == THE INSTRUCTION POINTER: LOCATION + DIRECTION ==
;; The ``Location'' idea is appropriated in several other instances
;; in the interpreter, including the instruction pointer, a cursor
;; intended to move across the code grid. Thus positioned, its direction
;; is defined by an eponymous custom type, a representative of its
;; orientation. The class in this capacity bears the euonym
;; ``Instruction-Pointer''.
;; 
;; == THE DATA ARRAY: A SIMULACRUM OF THE CODE ARRAY ==
;; The principles of the code array solution perspire into the
;; incarnation of the data repository in the form of the same ``Grid''
;; class, distinguished merely by the hash table values, which in lieu
;; of a character amount to an integer.
;; 
;; == THE DATA POINTER: A SYNONYM FOR A LOCATION ==
;; A simplification of the ``Instruction-Pointer'' class, the
;; ``Data-Pointer'' merely assumes the ``Location'' concept, without
;; incorporating an unnecessary direction, capable of operating upon the
;; data matrix.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-11-29
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BF%2BBF"
;;   -> "https://esolangs.org/wiki/Extended_Brainfuck#Extended_Type_I"
;;   -> "https://esolangs.org/wiki/Befunge"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype direction ()
  "The ``direction'' type enumerates the valid direction for navigating
   an instruction pointer across the BF+BF code grid."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries whose keys all conform to the KEY-TYPE and whose values
   adhere to the VALUE-TYPE."
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
                being   the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance functions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-opposite-direction (direction)
  "Returns the ``direction'' opposite to the input DIRECTION."
  (declare (type direction direction))
  (the direction
    (case direction
      (:left     :right)
      (:right    :left)
      (:up       :down)
      (:down     :up)
      (otherwise (error "Invalid direction to reverse: ~s."
                   direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class represents a point in a two-dimensional grid,
   designated by a horizontal co-ordinate 'x' (or column index) and a
   vertical co-ordinate 'y' (or row index)."
  (x 0 :type (integer * *))
  (y 0 :type (integer * *)))

;;; -------------------------------------------------------

(defun location-is-valid (location)
  "Checks whether the LOCATION designates a valid position in a grid,
   returning a generalized Boolean value equal to true if this can be
   confirmed, otherwise ``NIL''."
  (declare (type Location location))
  (and (>= (location-x location) 0)
       (>= (location-y location) 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction-Pointer".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction-Pointer
  (:conc-name   ip-)
  (:constructor make-instruction-pointer (x y)))
  "The ``Instruction-Pointer'' class represents a cursor composed of a
   location and a direction, capable of navigating through code grid."
  (x         0      :type (integer * *))
  (y         0      :type (integer * *))
  (direction :right :type direction))

;;; -------------------------------------------------------

(defun ip-move (pointer)
  "Moves the instruction POINTER one step into the current direction,
   and returns the modified POINTER."
  (declare (type Instruction-Pointer pointer))
  (case (ip-direction pointer)
    (:left     (decf (ip-x pointer)))
    (:right    (incf (ip-x pointer)))
    (:up       (decf (ip-y pointer)))
    (:down     (incf (ip-y pointer)))
    (otherwise (error "Invalid IP direction: ~s." (ip-direction pointer)))))

;;; -------------------------------------------------------

(defun ip-location (pointer)
  "Returns the ``Location'' of the instruction POINTER."
  (declare (type Instruction-Pointer pointer))
  (the Location (make-location (ip-x pointer) (ip-y pointer))))

;;; -------------------------------------------------------

(defun (setf ip-location) (new-location pointer)
  "Moves the instruction POINTER to the NEW-LOCATION, and returns the
   modified POINTER."
  (declare (type Instruction-Pointer pointer))
  (declare (type Location            new-location))
  (setf (ip-x pointer) (location-x new-location))
  (setf (ip-y pointer) (location-y new-location))
  (the Instruction-Pointer pointer))

;;; -------------------------------------------------------

(defun ip-reverse-direction (pointer)
  "Reverses the direction of the instruction POINTER, and returns the
   modified POINTER."
  (declare (type Instruction-Pointer pointer))
  (setf (ip-direction pointer)
        (get-opposite-direction (ip-direction pointer)))
  (the Instruction-Pointer pointer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Data-Pointer".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Data-Pointer
  (:conc-name   dp-)
  (:constructor make-data-pointer (x y)))
  "The ``Data-Pointer'' class provides a cursor for operating on a
   data grid."
  (x 0 :type (integer * *))
  (y 0 :type (integer * *)))

;;; -------------------------------------------------------

(defun dp-location (pointer)
  "Returns the ``Location'' of the data POINTER."
  (declare (type Data-Pointer pointer))
  (the Location (make-location (dp-x pointer) (dp-y pointer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Grid
  (:constructor make-grid (&optional (default-value NIL))))
  "The ``Grid'' class models a rectangular arrangement of arbitrary
   values, amenable to Cartesian co-ordinates for the retrieval and
   manipulation of its cells."
  (cells         (make-hash-table :test #'equalp)
                   :type (hash-table-of Location (or character integer)))
  (width         0 :type (integer 0 *))
  (height        0 :type (integer 0 *))
  (default-value 0 :type T))

;;; -------------------------------------------------------

(defun grid-at (grid location)
  "Returns the element of the GRID specified by the LOCATION, or its
   default value if no such position can be found."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the T
    (gethash location
      (grid-cells grid)
      (grid-default-value grid))))

;;; -------------------------------------------------------

(defun (setf grid-at) (new-value grid location)
  "Sets the GRID element at the LOCATION to the NEW-VALUE, and returns
   the modified GRID."
  (declare (type Grid     grid))
  (declare (type Location location))
  (declare (type T        new-value))
  (setf (gethash
          (copy-location location)
          (grid-cells grid)
          (grid-default-value grid))
        new-value)
  (the Grid grid))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid                            grid))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "~&Grid (~d x ~d)" (grid-width grid) (grid-height grid))
  (let ((pointer (make-location 0 0)))
    (declare (type Location pointer))
    (loop for y of-type (integer 0 *) from 0 below (grid-height grid) do
      (setf (location-y pointer) y)
      (format stream "~&")
      (loop for x of-type (integer 0 *) from 0 below (grid-width grid) do
        (setf (location-x pointer) x)
        (format stream "~a" (grid-at grid pointer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-code-grid (code)
  "Creates and returns a code ``Grid'' based upon the BF+BF CODE."
  (declare (type string code))
  (let ((grid     (make-grid #\Space))
        (position (make-location 0 0)))
    (declare (type Grid grid))
    (loop
      for  character    of-type character across code
      with fresh-line-p of-type boolean   =      T
      do
      (when fresh-line-p
        (incf (grid-height grid))
        (setf fresh-line-p NIL))
      (cond
        ((member character '(#\Newline) :test #'char=)
          (incf (location-y position))
          (setf (location-x position) 0)
          (setf fresh-line-p T))
        (T
          (setf (grid-at grid position) character)
          (incf (location-x position))
          (setf (grid-width grid)
                (max (grid-width grid) (location-x position))))))
    (the Grid grid)))

;;; -------------------------------------------------------

(defun interpret-BF+BF (code)
  "Interprets the BF+BF CODE and returns no value."
  (declare (type string code))
  (let* ((code-grid           (build-code-grid code))
         (instruction-pointer (make-instruction-pointer 0 0))
         (character           (grid-at code-grid (make-location 0 0))))
    (declare (type Grid                code-grid))
    (declare (type Instruction-Pointer instruction-pointer))
    (declare (type (or null character) character))
    
    (let ((memory       (make-grid 0))
          (data-pointer (make-data-pointer 0 0))
          (storage      0))
      (declare (type Grid         memory))
      (declare (type Data-Pointer data-pointer))
      (declare (type integer      storage))
      
      (labels
          ((tick ()
            "Moves the INSTRUCTION-POINTER to the next cell according to
             its current orientation and returns no value."
            (ip-move instruction-pointer)
            (unless (location-is-valid (ip-location instruction-pointer))
              (error "Invalid instruction pointer position in the ~
                      code grid: ~a."
                instruction-pointer))
            (setf character
                 (grid-at code-grid (ip-location instruction-pointer)))
            (values))
           
           (current-cell ()
            "Returns the value of the cell at the DATA-POINTER."
            (the integer (grid-at memory (dp-location data-pointer))))
           
           ((setf current-cell) (new-cell-value)
            "Sets the value of the cell at the DATA-POINTER to the
             NEW-CELL-VALUE and returns no value."
            (declare (type integer new-cell-value))
            (setf (grid-at memory (dp-location data-pointer))
                  new-cell-value)
            (values)))
        
        (loop do
          (cond
            ((null character)
              (error "Premature end of file at position ~s."
                instruction-pointer))
            
            ((char-equal character #\V)
              (incf (dp-y data-pointer))
              (tick))
            
            ((char= character #\>)
              (incf (dp-x data-pointer))
              (tick))
            
            ((char= character #\<)
              (decf (dp-x data-pointer))
              (tick))
            
            ;; The letter "A" deviates from the standard's "^" in order
            ;; to circumvent its ambiguity, as the latter token is also
            ;; claimed to conform to the Extended brainfuck Type 1
            ;; logical operator "XOR" (exclusive OR).
            ((char= character #\A)
              (decf (dp-x data-pointer))
              (tick))
            
            ((char= character #\+)
              (incf (current-cell))
              (tick))
            
            ((char= character #\-)
              (decf (current-cell))
              (tick))
            
            ;; '0'--'9'.
            ((digit-char-p character)
              (setf (current-cell)
                    (digit-char-p character))
              (tick))
            
            ((char= character #\.)
              (write-char (code-char (current-cell)))
              (tick))
            
            ((char= character #\,)
              (format T "~&Please enter a character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input)
              (tick))
            
            ((char= character #\[)
              (cond
                ;; Current cell = 0? => Skip past matching ']'.
                ((zerop (current-cell))
                  (let ((start-position
                          (copy-location
                            (ip-location instruction-pointer))))
                    (declare (type Location start-position))
                    (tick)
                    (loop with level of-type fixnum = 0 do
                      (case character
                        ((NIL)
                          (error "Could not find a matching ']' for ~
                                  the '[' at location ~a."
                            start-position))
                        (#\[
                          (incf level)
                          (tick))
                        (#\]
                          (cond
                            ((zerop level)
                              (tick)
                              (loop-finish))
                            (T
                              (decf level)
                              (tick))))
                        (otherwise
                          (tick))))))
                ;; Current cell != 0? => Advance normally.
                (T
                  (tick))))
            
            ((char= character #\])
              (cond
                ;; Current cell != 0? => Jump back to matching '['.
                ((not (zerop (current-cell)))
                  (let ((start-position     (copy-location (ip-location instruction-pointer)))
                        (original-direction (ip-direction instruction-pointer)))
                    (declare (type Location  start-position))
                    (declare (type direction original-direction))
                    
                    ;; The instruction pointer must seek in the opposite
                    ;; direction.
                    (ip-reverse-direction instruction-pointer)
                    (tick)
                    
                    (loop with level of-type fixnum = 0 do
                      (case character
                        ((NIL)
                          (error "Could not find a matching ']' for ~
                                  the '[' at location ~a."
                            start-position))
                        (#\]
                          (incf level)
                          (tick))
                        (#\[
                          (cond
                            ((zerop level)
                              (setf (ip-direction instruction-pointer)
                                    original-direction)
                              (tick)
                              (loop-finish))
                            (T
                              (decf level)
                              (tick))))
                        (otherwise
                          (tick))))
                  
                  (setf (ip-direction instruction-pointer)
                        original-direction)))
                ;; Current cell = 0? => Advance normally.
                (T
                  (tick))))
            
            ((char= character #\@)
              (loop-finish))
            
            ((char= character #\$)
              (setf storage (current-cell))
              (tick))
            
            ((char= character #\!)
              (setf (current-cell) storage)
              (tick))
            
            ((char= character #\})
              (setf (current-cell) (ash (current-cell) -1))
              (tick))
            
            ((char= character #\{)
              (setf (current-cell) (ash (current-cell) 1))
              (tick))
            
            ((char= character #\~)
              (setf (current-cell)
                    (lognot (current-cell)))
              (tick))
            
            ((char= character #\^)
              (setf (current-cell)
                    (logxor (current-cell) storage))
              (tick))
            
            ((char= character #\&)
              (setf (current-cell)
                    (logand (current-cell) storage))
              (tick))
            
            ((char= character #\|)
              (setf (current-cell)
                    (logior (current-cell) storage))
              (tick))
            
            
            ((char= character #\U)
              (setf (ip-direction instruction-pointer) :up)
              (tick))
            
            ((char= character #\D)
              (setf (ip-direction instruction-pointer) :down)
              (tick))
            
            ((char= character #\L)
              (setf (ip-direction instruction-pointer) :left)
              (tick))
            
            ((char= character #\R)
              (setf (ip-direction instruction-pointer) :right)
              (tick))
            
            ((member character '(#\Space #\Tab) :test #'char=)
              (tick))
            
            (T
              (error "Invalid command ~s at position ~a."
                character instruction-pointer)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Build a grid from a BF+BF program.
(build-code-grid
"v8[>4[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]D
                                        >
D.------.+++.<.-<.>>.+++..+++++++.--->.>L
R--------.>>+.>++.@")

;;; -------------------------------------------------------

;; Print "Hello World!"
(interpret-BF+BF
"v8[>4[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]D
                                        >
D.------.+++.<.-<.>>.+++..+++++++.--->.>L
R--------.>>+.>++.@")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-BF+BF ",.@")

;;; -------------------------------------------------------

;; One-time cat program with augmentation of the input character's code.
(interpret-BF+BF ",+.@")

;;; -------------------------------------------------------

;; One-time cat program with change of direction.
(interpret-BF+BF
",D
 .
 @")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-BF+BF "1[,.1]@")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which utilizes topology and
;; navigation instead of the commands "[" and "]" for iteration.
(interpret-BF+BF
"R,.D
.  ,
,  .
U.,L")

;;; -------------------------------------------------------

;; Moving "outside" of the (infinite) data grid.
(interpret-BF+BF ",.,>.@")
