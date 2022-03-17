;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Cerberus", invented by the Esolang user "Tetrapyronia", and
;; based upon the discernment and reckoning of character groups among
;; a treble line pattern.
;; 
;; Concept
;; =======
;; Cerberus subjects its programs to a throughout peculiar structure of
;; exactly three lines height --- a signum very likely responsible for
;; its agnomination's inspiration in allusion to the eponymous creature
;; of the Greek mythology ---, the significant constituents of which
;; assume their expression in the dot "." and hyphen "-" characters.
;; Instructions are encoded in homogenous groups of such tokens, the
;; cardinality of a compound being the discriminating attribute applied
;; in conjunction with the respective character in order to determine
;; the represented command.
;; 
;; == A CERBERUS PROGRAM FORMS A GRID ==
;; The kenspeckle accountrement invested in Cerberus's appearance
;; relates to its shape: Each program embraces exactly a tally of three
;; lines, the extent of the same diverges from the horizontal dimension
;; in its unbounded capacity. An epiphenomenal observation gleaned from
;; this reticulation circumscribes the form as a Cartesian grid.
;; 
;; == DOTS AND HYPHENS DESIGNATE COMMMANDS ==
;; Without intolerance towards any characters, the language yet
;; apportions merely to dots (".") and hyphens ("-") an effective and
;; mensurable meaning. Other content, maugre its acceptance,
;; appropriates a commentary function: The first column to include a
;; character apart from the dot or hyphen terminates the instruction
;; section and incites the comment portion, expanding from this location
;; to the grid's right boundary.
;; 
;; == CONNECTED GROUPS OF CHARACTERS FORM AN INSTRUCTION ==
;; Adjacent cells building an unbroken path based upon the parity of
;; their characters are entered into the registry of a common group. In
;; the course of this assemblage, the group's mold does not carry any
;; significance, instead its cardinality, the tally of its members,
;; enjoys an ascription of effect, receiving its meaning in allocation
;; with the character being the compound's foundation. An instruction
;; thus is unambiguously identified by a tuple (character, group size).
;; A cell once having been a participant in the group formation cannot
;; iterum register for another such a reckoning, in corollary being
;; designated as "visited" or "marked".
;; 
;; == INSTRUCTIONS ARE PRODUCED BY TRAVERSAL ALONG THE SECOND LINE ==
;; A program's interpretation commences by positioning of the
;; instruction pointer on the first column of the second line,
;; memorizing this point as a temporary point of reference, traversing
;; all unvisited neighbors bearing the same character recursively, and
;; tallying the thus connected components. Note that the diorism of a
;; group entails two restrictions imposed upon the search in a cell's
;; periphery in that:
;;   
;;   (a) Only the immediately abutting neighbors to the left, top,
;;       right, and bottom may be considered. Diagonally located
;;       candidates elude its access.
;;   (b) The thus assayed environment must be veridically adjacent, that
;;       means, a cell may not skip in any direction another
;;       incompatible cell in order to capture a compatible one.
;; 
;; The twain of splicing character and cell count affiliates with an
;; instruction that is captured in the ultimate desideratum: the
;; instruction list.
;; 
;; The instruction retrieval segues into the detection of the next
;; eligible cell located on the second line by proceeding rightwards
;; from the germination point until the instruction pointer intersects
;; with an unvisited cell, repeating with this unit the process of group
;; formation and the affiliation with an instruction similiter.
;; 
;; The following apercu shall avail as an explication of the instruction
;; extraction process, the cynosure of which constitutes the production
;; of a list of instructions.
;;   
;;   (1)  Locate the instruction pointer (IP) in the first column of the
;;        second row.
;;   (2)  Let "p" be the instruction pointer location.
;;   (3)  Let "c" be the character stored in the current cell.
;;   (4)  Mark the current cell as visited.
;;   (5)  For each neighbor to the left, top, right, and bottom: Check
;;        whether the neighbor is yet unvisited and contains the
;;        character c.
;;        (5.1) If true: Repeat the procedure for the neighbor starting
;;              with the step (4).
;;   (6)  Let "s" be the size of the collected group.
;;   (7)  Find the instruction associated with the combination of the
;;        character c and the tally s, forming the tuple (c, s).
;;   (8)  Append this instruction to the overall instruction list.
;;   (9)  Starting at the memorized instruction pointer position p,
;;        move the instruction pointer dextrally along the second line
;;        until encountering a yet unvisited cell.
;;   (10) Repeat the process with the step (2).
;; 
;; Please note that the steps (3) through (6) may be construed as the
;; group formation section, while (7) and (8) apply to the instruction
;; retrieval. The orrow constituents describe auxiliary and
;; administrative efforts.
;; 
;; == DATA MANAGEMENT PROCEEDS IN A STACK ==
;; All data's castaldy is consigned to the representation of integers,
;; unbounded in magnitude and expansive along both lateralities of the
;; number line. The assignment of the storage facility is allotted to a
;; stack of theoretically infinite capacity.
;; 
;; 
;; Architecture
;; ============
;; Data management in Cerberus embraces the consignment of arbitrarily
;; large signed integers to a single stack. Several operations exist
;; which, either as a manifestation of their purpose or as a mere
;; concomitant, practice manipulations of the same. A corollary of the
;; peculiarity apportioned to its syntax, a further architectural entity
;; experiences its establishment in the reticulate source code design.
;; 
;; == STACK ==
;; A last-in-first-out data structure, the stack, occupies the wike of
;; the program data management, the entities being thus governed
;; constituting integer numbers in the range [-infinity, +infinity].
;; 
;; == SOURCE CODE ==
;; A Cerberus program being strictly arranged in three lines of
;; arbitrary horizontal extent, several realizations, each with their
;; own vantages and vices, proffer amenability, among these the grid,
;; graph, and raw line-wise structuring. For a disquisition and
;; application of this inquiry, please consult the section
;; "Implementation". Its assessment as the most appropriate designation,
;; the code architecture shall be considered a grid in this document.
;; 
;; 
;; Data Types
;; ==========
;; The type system incorporated into Cerberus comports to a single
;; species: unbounded signed integer numbers. The appropriation of their
;; office in consectary covers all aspects of operational manifestation,
;; including, without exhaustion, arithmetics as much as input and
;; output. An epiphenomenal produce of its architecture, the maintaining
;; stack, too, participates in this account.
;; 
;; == INTEGERS ==
;; Any expression of data castaldy and currency constitutes the
;; exclusive bailiwick of integer numbers, permissive to both positive
;; and negative laterality and unbounded along any of these axes, thus
;; commorant in the range [-infinity, +infinity].
;; 
;; == STACK ==
;; The stewardship anenst the integer entities ascribes to a stack's
;; concerns, a last-in-first-out storage with emphasis upon its top
;; position, the locality inside of the same all operations apply.
;; 
;; 
;; Syntax
;; ======
;; Cerberus's ultimate diorism occupies its woning in the structure and
;; interpretation of its source code. A program consists of three lines,
;; permissive to any content, natheless, exclusively dots (".") and
;; hyphens ("-") introduce a contribution to the entailed instructions,
;; doing so by the formation of groups composed of homogeneous items,
;; the cardinality of which in conjunction with the repeated character
;; corresponds to a command.
;; 
;; == PROGRAMS ARE COMPOSITIONS OF THREE LINES ==
;; Any program must be constructed from exactly three lines, imposing no
;; specific requirement on their length, nor on the content. However,
;; the only characters eligible as instruction constituents are defined
;; in terms of dots (".") and hyphens ("-"). Inside of this reticulate
;; design, a column occupied by a character apart from this twain
;; terminates the actual program section, eventuating the interpretation
;; from this location to the end of the code as comment.
;; 
;; == INSTRUCTIONS ARE ENCODED AS CHARACTER GROUPS ==
;; A connected path of cells concurring in their maintained character
;; datum establishes a group, appropriated in coefficiency with the
;; representative character it affiliates with a particular instruction.
;; 
;; 
;; Instructions
;; ============
;; The compass of Cerberus's instruction set enumerates twelve members,
;; subsumable into logical categories of consanguinity in purpose, and
;; denoted by group sizes of connected cells incorporating the same
;; character.
;; 
;; == HOMOGENOUS GROUPS FORM COMMANDS ==
;; A conspicuously patent diorism embues Cerberus's haecceity concerning
;; the commands' design. Starting with an unmarked cell, all unvisited
;; cells accessible either directly or indirectly, by mediation of its
;; neighbors, storing the same character as the original unit are
;; tallied, forming a group. Subsequently, this account, known as the
;; group size, in conjunction with the starting character contribute to
;; the establishment of a tuple whose identity correlates unambivalently
;; with an instruction.
;; 
;; == TYPES OF INSTRUCTIONS ==
;; The instruction set can be divided into three categories:
;;   
;;   - Stack manipulation instructions
;;   - Arithmetic instructions
;;   - Control flow instructions
;; 
;; Stack manipulation instructions exercise direct and purposeful
;; accessing effects on the stack. While nearly all commands in some
;; regard influence the data storage, representatives of this category
;; conduct a treatment on a basic level, for instance, by rotating the
;; two topmost elements or duplicating the head.
;; 
;; Arithmetic instructions constitute a species of mathematical
;; significance. As kenspeckle as its ostentation, Cerberus's purveyance
;; entails merely two such candidates: the binary subtraction operation
;; and the factorial.
;; 
;; Control flow instructions handle the navigation across a program in
;; the course of its execution. Two members' coefficiency already
;; exhausts this purview: Markers, associating an integer value with an
;; instruction pointer location, can be registered and queried for a
;; contingent return, thus basically providing a go-to facility.
;; 
;; == OVERVIEW ==
;; The following table's ordination embraces both the designation of the
;; character and group size conjunction, as well as an identifying
;; weftage in the form of the aforementioned twain's coefficiency, the
;; characteristics being associated to the appertaining effect.
;;   
;;   Char. | Tally | Ident. | Effect
;;   ------+-------+--------------------------------------------------
;;    .    | 1     | .      | Pushes the value 1 unto stack.
;;   ..................................................................
;;    -    | 1     | -      | Pushes the value -1 unto stack.
;;   ..................................................................
;;    .    | 2     | ..     | Queries the user for an integer and
;;         |       |        | pushes the response unto the stack.
;;   ..................................................................
;;    -    | 2     | --     | Pops the top stack element and prints it.
;;   ..................................................................
;;    .    | 3     | ...    | Pops the top stack element A and the new
;;         |       |        | top stack element B, computes the
;;         |       |        | difference (A - B), and pushes this
;;         |       |        | result unto the stack.
;;   ..................................................................
;;    -    | 3     | ---    | Pops the top stack element A, computes
;;         |       |        | its factorial A!, and pushes the result
;;         |       |        | unto the stack. The calculation proceeds
;;         |       |        | by means of following bifurcation:
;;         |       |        |   (a) If A >= 0, A! is computed as usual.
;;         |       |        |   (b) If A < 0, the reckoning advances
;;         |       |        |       according to this formula:
;;         |       |        |         A! = -((-A)!)
;;   ..................................................................
;;    .    | 4     | ....   | Swaps the position of the two top stack
;;         |       |        | elements.
;;   ..................................................................
;;    -    | 4     | ----   | Duplicates the top stack element.
;;   ..................................................................
;;    .    | 5     | .....  | No-op. Performs no operation.
;;   ..................................................................
;;    -    | 5     | -----  | No-op. Performs no operation.
;;   ..................................................................
;;    .    | 6     | ...... | Pops the top stack element A and stores
;;         |       |        | its value as a marker name, associated
;;         |       |        | with current instruction pointer (IP)
;;         |       |        | location as the marker position.
;;   ..................................................................
;;    -    | 6     | ------ | Pops the top stack element A and searches
;;         |       |        | for a marker using its value as a name.
;;         |       |        | If such a marker exists, the instruction
;;         |       |        | pointer is relocated to the associated
;;         |       |        | location; otherwise, no effect applies.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its resourceful and throughout conception, as a consequence of
;; its brevity, the Cerberus specification is inflicted with some
;; instances of ambivalence or insufficiency. A select shall be the
;; following enumeration's material.
;; 
;; == HOW ARE EXCESSIVE GROUP SIZES HANDLED? ==
;; In counterdistinguishment to the specification's investment into the
;; explication concerning insufficiently populated groups' application
;; as no-operations, the widdershins circumstance, involved in nimiety
;; of members, thus excess in their cardinality, and ultimately
;; disassociation with any recognized command, escapes the writ's
;; address. Adjudgment is thus exercised that too large group sizes,
;; which alludes to any tally aboon six characters length, shall also be
;; subjected to a no-operation treatment.
;; 
;; == HOW SHALL A VACANT STACK BE HANDLED? ==
;; The contingency of vacancy inflicting a stack during an operation
;; dependent upon its top member's indagation or removal lacks an
;; adequate addressing. Two possibilities may be presented:
;;   
;;   (a) An error is signaled.
;;   (b) A default value apropos for the particular use case is
;;       returned.
;; 
;; For the sake of safety regarding contingent future iterations of the
;; programming language, an error shall be signaled in such situations
;; in order to prevent a reliance upon unverified behavior.
;; 
;; 
;; Implementation
;; ==============
;; The offered implementation accoutres the scion of simplicity and
;; a pursuit of illustration; the paragon of efficiency and
;; realiability, hence, will be attested by diligent conspectuity as,
;; to a certain mete, violated.
;; 
;; The Cerberus language's manifestation in Common Lisp is consigned to
;; the castaldy of a treble stage architecture:
;;   
;;   (1) GRID CONSTRUCTION:
;;       A transformation is applied to the one-dimesional Cerberus
;;       source code string in order to generate a shape accommodating
;;       superior comfort for processing: a two-dimensional grid
;;       representation.
;;   (2) INSTRUCTION EXTRACTION:
;;       In the face of its rather convoluted construction, the
;;       extraction of instructions from the pattern grid constitutes
;;       a graduated and interleaved process.
;;       (2.a) GROUP FORMATION:
;;             The first unmarked cell on the second line avails in the
;;             recognition of the next group, the size and representive
;;             character as an indivisible jumelle act in the agency of
;;             an instruction designator.
;;       (2.b) INSTRUCTION RETRIEVAL:
;;             A task of considerable alleviated complexity, the
;;             instruction designator will be utilized to request the
;;             unambiguously correlating actual instruction.
;;   (3) INSTRUCTION PROCESSING:
;;       The produce of the extraction process, a collection of
;;       instructions remains in the requisite of effectuation, the same
;;       is administered by this desinent stage.
;; 
;; == (1) GRID CONSTRUCTION ==
;; In an incipient stage, the one-dimensional source code string
;; experiences its molding into a dedicated ``Grid'' class instance. Its
;; realization proceeds by means of consuming non-newline content as
;; the current grid row's columns and construing linebreaks as the
;; annexation of a further row. In this fashion non-newline characters
;; are subjected to a transcription into cells, while concomitantly
;; wariness prevails about the encounter of a non-command token. Among
;; the thus generated grid rows, the minimum column index containing a
;; character of no operational utility determines the comment section,
;; starting with which the grid must be curtailed.
;; 
;; The ``Grid'' class itself submits to a diorism of a triplet, being
;; a composition of the width, height, and a collection of cells. While
;; the second compartment must be a fixed fact, set by virtue of the
;; Cerberus specification to three lines, the current implementation
;; expects and imputes fidelity to this requirement as a causatum of the
;; programmer's effort. The model's cynosure resides in the ``cells''
;; slot and its chosen representation. A sparse structure, founded upon
;; a hash table, associates with a two-dimensional position, the
;; ``Location'' class, composed of an x- and an y-coordinate, thus a
;; reformulation of the column and row specifiers, a ``Cell'' instance.
;; Indagation and manipulation appropriates eath conduct, while
;; impositions concerning space are alleviated. Any location not
;; included in the column range [0, width-1] and [0, height-1] naturally
;; produces a negative response upon inquisition.
;; 
;; With respect to its onuses, the ``Cell'' class entails in its
;; amplexation, besides its location inside of the grid lending it
;; tenancy and the represented character, a flag determining its state
;; as marked, that is, whether it has already been incorporated during
;; the group formation stage. The incipient piece of information, the
;; spatial datum, permits both its own detection and the perusal of its
;; neighborhood, as potential units in its surrounding only differ by
;; a location coordinate of one step distance, hence, when given a cell,
;; its maximum of four accolent residents are guaranteed amenability.
;; In the course of the group establishment, each cell must be vouched
;; at most a single contribution to the group size, founded upon which
;; condition a labeling declaims necessity. The ``visited-p'' slot
;; partakes of this wike, being in its inchoation set to false, and
;; modified in the juncture of a first visit as true, after that moment
;; its account will not be permitted adit to the size accumulation.
;; 
;; This stage's corollary resolves to the availability of a code grid,
;; ensconcing unvisited cells and, following a purge with concomitant
;; curtailment, eschewing the potential comment section in its tail.
;; 
;; == (2) INSTRUCTION EXTRACTION ==
;; The rearrangement of the source code into a reticulate design serves
;; the paravaunt objective of augmented processing. Its establishment
;; is succeeded by the recognition and extraction of the instructions
;; embodied in its patterns. Albeit being subsumbed into a single tier,
;; the concomitant natures of the group formation and instruction
;; retrieval do not vindicate on their own a confluence of these two
;; substages when assaying in particular the former's intricacy.
;; 
;; == (2.a) GROUP FORMATION ==
;; The basic, and most complex, service during the extraction stage
;; comprises the recognition of groups from the grid. An instruction
;; pointer (IP), located at the first column and always proceeding
;; along the second row, memorizes the current cell's character, seeking
;; in its neighborhood all cells containing the same content and being
;; accessible from this starting position. The result of this process
;; embraces two pieces of information: (1) The character contained in
;; the incepting cell, and thus connecting the group members, and
;; (2) the cardinality of the thus circumscribed group.
;; 
;; The below extended pseudocode shall supply an abbozzo regarding the
;; conceivable implementation of the grouping stage during a Cerberus
;; program's processing:
;;   
;;   { For the sake of simplicity in our calculations, the "groupSize" }
;;   { will be accessible on a global level, albeit only regarded in   }
;;   { the function "visitCell".                                       }
;;   let groupSize <- 0, with groupSize in [0, +infinity]
;;   
;;   { Marks an unvisited cell, adds it to the "groupSize", and        }
;;   { recursively applies the same procedure to its unvisited         }
;;   { neighbor cells.                                                 }
;;   function visitCell (grid : Grid, cell : Cell)
;;     if not (cell.isVisited) then
;;       cell.isVisited = true
;;       groupSize <- groupSize + 1
;;       checkNeighbor (grid, cell, left)
;;       checkNeighbor (grid, cell, top)
;;       checkNeighbor (grid, cell, right)
;;       checkNeighbor (grid, cell, bottom)
;;     end if
;;     return (cell.character, groupSize)
;;   end function
;;   
;;   { Checks whether the cell contains an unvisited neighbor in the   }
;;   { specified direction, invoking on this neighbor the "visitCell"  }
;;   { operation upon confirmation.                                    }
;;   procedure checkNeighbor (grid      : Grid,
;;                            cell      : Cell,
;;                            direction : {left, top, right, bottom})
;;     if grid.hasNeighborAt (grid, cell, direction) then
;;       let neighbor <- grid.cellAt (cell)
;;       if (neighbor.character = cell.character) then
;;         visitCell (grid, neighbor)
;;       end if
;;     end if
;;   end procedure
;; 
;; The "visitCell" function ought to be invoked on the unmarked cell at
;; the instruction pointer, so as to return, following the cascading
;; applications, the result tuple.
;; 
;; This act of discerment is repeated after each separate instruction
;; extraction's patration by passing over the marked cells on the second
;; grid line and unto the nearest unvisited candidate. Each termination
;; of this process in its implemented form produces an instruction
;; designator, cohering as a twain of character and tally.
;; 
;; == (2.b) INSTRUCTION RETRIEVAL ==
;; A warklume of our ultimate instruction production, the recognition of
;; a group and its composition into a tally and character compound,
;; capacitates the retrieval of the instruction associated with the
;; same. The instruction designator tuple being represented by a string,
;; the unique combination defines a key into a hash table, associated
;; with an ``instruction'' object in the form of a keyword symbol,
;; enumerated in an eponymous type. With reference to the "Instructions"
;; section, the following reification exposes the correspondences
;; betwixt the instruction designators and instruction objects. Please
;; note that the third column, "Ident.", describes the identifying
;; string object, while the desinent fourth column refers to the
;; ``instruction''-type object.
;;   
;;   Char. | Tally | Ident. | ``instruction''
;;   ------+-------+--------------------------------------------------
;;    .    | 1     | .      | :push-1
;;   ..................................................................
;;    -    | 1     | -      | :push-minus-1
;;   ..................................................................
;;    .    | 2     | ..     | :input
;;   ..................................................................
;;    -    | 2     | --     | :output
;;   ..................................................................
;;    .    | 3     | ...    | :subtract
;;   ..................................................................
;;    -    | 3     | ---    | :factorial
;;   ..................................................................
;;    .    | 4     | ....   | :swap
;;   ..................................................................
;;    -    | 4     | ----   | :duplicate
;;   ..................................................................
;;    .    | 5     | .....  | :no-op
;;   ..................................................................
;;    -    | 5     | -----  | :no-op
;;   ..................................................................
;;    .    | 6     | ...... | :set-marker
;;   ..................................................................
;;    -    | 6     | ------ | :jump-to-marker
;; 
;; In concord with the aboon explications, the group formation (2.a) and
;; instruction retrieval (2.b) stages install an interleaved enterprise,
;; with an iterative inquest upon the former to seek the next cell on
;; the second line not yet marked, then produce its instruction
;; designator, ere the latter moeity partakes of this designator's
;; reification into a concrete ``instruction'' object. The conclusion of
;; this endeavor exercises its manifestation in an ordered sequence of
;; instructions, ready for their application.
;; 
;; == (3) INSTRUCTION PROCESSING ==
;; The exertion of effect by means of the yielded instructions'
;; processing does not carry much capacity for convolutions: An
;; instruction pointer (IP) originates at the first member, proceeding
;; strictly forward along the sequence, contingently participating in a
;; redirection based upon the inquests of markers.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-03-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Cerberus"
;;       o Specification of the Cerberus programming language.
;;   -> "https://esolangs.org/wiki/Tiangou"
;;       o Specification of Tiangou, a derivative and wimpmode of the
;;         Cerberus programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack of zero or more
   elements, each of which conforms to the ELEMENT-TYPE, defaulting to
   ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines an output sink for printing
   operations, entailing, without a claim of exhaustion, ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type defines the valid directions for specifying
   translations of points and indagation of neighborhoods."
  '(member :bottom :left :right :top))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the valid Cerberus instructions."
  '(member
    :push-1
    :push-minus-1
    :input
    :output
    :subtract
    :factorial
    :swap
    :duplicate
    :no-op
    :no-op
    :set-marker
    :jump-to-marker))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class specifies a Cartesian point of signed integer
   resolution, eligible for addressing cells in a grid."
  (x 0 :type fixnum)
  (y 0 :type fixnum))

;;; -------------------------------------------------------

(defun location-move (location direction)
  "Moves the LOCATION one step in the DIRECTION and returns the modified
   LOCATION."
  (declare (type Location  location))
  (declare (type direction direction))
  (case direction
    (:left
      (decf (location-x location)))
    (:right
      (incf (location-x location)))
    (:top
      (decf (location-y location)))
    (:bottom
      (incf (location-y location)))
    (otherwise
      (error "Invalid direction for move: ~s." direction)))
  (the Location location))

;;; -------------------------------------------------------

(defun location-at (location direction)
  "Returns a new ``Location'' translated one step relative to the input
   LOCATION into the DIRECTION."
  (declare (type Location  location))
  (declare (type direction direction))
  (the Location (location-move (copy-location location) direction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Cell
  "The ``Cell'' class defines a unit participating in the composition of
   a Cerberus code ``Grid''."
  (location  NIL :type (or null Location))
  (character NIL :type (or null character))
  (visited-p NIL :type boolean))

;;; -------------------------------------------------------

(defmethod print-object ((cell Cell) stream)
  (declare (type Cell        cell))
  (declare (type destination stream))
  (format stream "Cell(location=(~d,~d), content=~c)"
    (location-x (cell-location cell))
    (location-y (cell-location cell))
    (cell-character cell)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((width
    :initarg       :width
    :initform      0
    :type          fixnum
    :documentation "The number of columns constituting the GRID.")
   (height
    :initarg       :height
    :initform      0
    :type          fixnum
    :documentation "The number of rows constituting the GRID.")
   (cells
    :initarg       :cells
    :initform      (make-hash-table :test #'equalp)
    :type          (hash-table-of Location Cell)
    :documentation "A sparse matrix containing the cells, indexing each
                    such with an unambiguous location."))
  (:documentation
    "The ``Grid'' class provides a view on a Cerberus program as a
     two-dimensional Cartesian arrangement of cells, each of which
     stores a single character from the source code.
     ---
     This structure capacitates an eath navigation and processing of the
     program."))

;;; -------------------------------------------------------

(defun make-empty-grid ()
  "Creates and returns an empty ``Grid'' of zero columns but a nominal
   heighto of three rows."
  (the Grid (make-instance 'Grid :width 0 :height 3)))

;;; -------------------------------------------------------

(defun grid-cell-at (grid location)
  "Returns the GRID cell designated by the LOCATION, or ``NIL'' if the
   position infringes on the GRID's bounds."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the (or null Cell)
    (gethash location
      (slot-value grid 'cells))))

;;; -------------------------------------------------------

(defun (setf grid-cell-at) (new-cell grid location)
  "Sets the GRID cell at the LOCATION to the NEW-CELL and returns the
   modified GRID."
  (declare (type Cell     new-cell))
  (declare (type Grid     grid))
  (declare (type Location location))
  (setf (gethash (copy-location location) (slot-value grid 'cells))
        new-cell)
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-delete-tail (grid start-column)
  "Removes from the GRID all columns beginning at the START-COLUMN
   index, and returns the modified GRID."
  (declare (type Grid   grid))
  (declare (type fixnum start-column))
  (with-slots (width cells) grid
    (declare (type fixnum                        width))
    (declare (type (hash-table-of Location Cell) cells))
    (with-hash-table-iterator (advance cells)
      (loop do
        (multiple-value-bind (has-more-cells-p location cell)
            (advance)
          (declare (type T                  has-more-cells-p))
          (declare (type (or null Location) location))
          (declare (type (or null Cell)     cell))
          (declare (ignore                  cell))
          (if (and has-more-cells-p
                   (>= (location-x location)
                       start-column))
            (remhash location cells)
            (loop-finish)))))
    (setf width (min width (1+ start-column))))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-contains-point-p (grid point)
  "Checks whether the POINTS is located inside of the GRID's boundaries,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location point))
  (with-slots (width height) grid
    (declare (type fixnum width))
    (declare (type fixnum height))
    (the boolean
      (not (null
        (and
          (<= 0 (location-x point) (1- width))
          (<= 0 (location-y point) (1- height))))))))

;;; -------------------------------------------------------

(defun grid-column-at (grid column-index)
  "Returns the COLUMN-INDEX-th column of the GRID as a list of three
   elements, these being the top, center, and bottom cell."
  (declare (type Grid   grid))
  (declare (type fixnum column-index))
  (let ((location (make-location column-index 0)))
    (declare (type Location location))
    (the (list-of Cell)
      (list
        (grid-cell-at grid location)
        (progn
          (location-move location :bottom)
          (grid-cell-at grid location))
        (progn
          (location-move location :bottom)
          (grid-cell-at grid location))))))

;;; -------------------------------------------------------

(defun (setf grid-column-at) (new-column grid column-index)
  "Inserts into the GRID at the COLUMN-INDEX the NEW-COLUMN, represented
   by a list of three elements, these being the top, center, and bottom
   cells, and returns the modified GRID."
  (declare (type (list-of Cell) new-column))
  (declare (type Grid           grid))
  (declare (type fixnum         column-index))
  (destructuring-bind (top-cell center-cell bottom-cell)
      new-column
    (declare (type Cell top-cell))
    (declare (type Cell center-cell))
    (declare (type Cell bottom-cell))
    (let ((insertion-location (make-location column-index 0)))
      (declare (type Location insertion-location))
      (setf (grid-cell-at grid insertion-location) top-cell)
      (location-move insertion-location :bottom)
      (setf (grid-cell-at grid insertion-location) center-cell)
      (location-move insertion-location :bottom)
      (setf (grid-cell-at grid insertion-location) bottom-cell)))
  (with-slots (width) grid
    (declare (type fixnum width))
    (setf width (max width (1+ column-index))))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-append (target appendage)
  "Appends columns of the APPENDAGE grid to the TARGET grid, and returns
   the modified TARGET."
  (declare (type Grid target))
  (declare (type Grid appendage))
  (let ((target-width (slot-value target 'width)))
    (declare (type fixnum target-width))
    (loop
      for appendage-column-index
        of-type fixnum
        from    0
        below   (slot-value appendage 'width)
      for target-insertion-index
        of-type fixnum
        =       (+ target-width appendage-column-index)
      do
        (let ((new-column
                (grid-column-at appendage appendage-column-index)))
          (declare (type (list-of Cell) new-column))
          ;; Update the cells of the column to insert to reflect their
          ;; position (particularly their column indices) inside of the
          ;; TARGET.
          (dolist (cell new-column)
            (declare (type Cell cell))
            (setf (location-x (cell-location cell))
                  target-insertion-index))
          (setf (grid-column-at target target-insertion-index)
                new-column))))
  (the Grid target))

;;; -------------------------------------------------------

(defun cell-neighbor-at (grid cell direction)
  "Returns the neighbor of the CELL, being a component of the GRID,
   located in the DIRECTION, or ``NIL'' if none such exists inside of
   the CELL grid's boundaries."
  (declare (type Grid      grid))
  (declare (type Cell      cell))
  (declare (type direction direction))
  (let ((neighbor-location
          (location-at (cell-location cell) direction)))
    (declare (type Location neighbor-location))
    (the (or null Cell)
      (when (grid-contains-point-p grid neighbor-location)
        (grid-cell-at grid neighbor-location)))))

;;; -------------------------------------------------------

(defun cell-has-unvisited-neighbor-at (grid cell direction)
  "Checks whether the CELL, being a component of the GRID, is abutted by
   a yet unvisited neighbor holding the same character and being located
   in the DIRECTION, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Grid      grid))
  (declare (type Cell      cell))
  (declare (type direction direction))
  (let ((neighbor (cell-neighbor-at grid cell direction)))
    (declare (type (or null Cell) neighbor))
    (the boolean
      (when neighbor
        (not (null
          (and
            (not   (cell-visited-p neighbor))
            (char= (cell-character neighbor)
                   (cell-character cell)))))))))

;;; -------------------------------------------------------

(defun make-grid-from-code (code)
  "Creates and returns a new ``Grid'' from the piece of Cerberus CODE."
  (declare (type string code))
  (let ((grid                 (make-instance 'Grid))
        (comment-start-column NIL))
    (declare (type Grid             grid))
    (declare (type (or null fixnum) comment-start-column))
    (with-slots (width height) grid
      (declare (type fixnum width))
      (declare (type fixnum height))
      (loop
        with location  of-type Location  = (make-location 0 0)
        for  character of-type character across code
        do
        (case character
          ;; Linebreak found.
          (#\Newline
            (setf (location-x location) 0)
            (location-move location :bottom)
            (setf height (max height (1+ (location-y location)))))
          
          ;; Command character found.
          ((#\. #\-)
            (setf (grid-cell-at grid location)
                  (make-cell
                    :character character
                    :location  (copy-location location)))
            (setf width (1+ (location-x location)))
            (setf height (max height 1))
            (location-move location :right))
          
          ;; Comment character found.
          (otherwise
            (setf comment-start-column
              (if comment-start-column
                (min comment-start-column (location-x location))
                (location-x location)))
            (setf width (1+ (location-x location)))
            (location-move location :right)))))
    (when comment-start-column
      (grid-delete-tail grid comment-start-column))
    (the Grid grid)))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-slots (width height) grid
    (declare (type fixnum width))
    (declare (type fixnum height))
    (format stream "~&~d x ~d grid:" width height)
    (let ((location (make-location 0 0)))
      (declare (type Location location))
      (dotimes (y height)
        (declare (type fixnum y))
        (setf (location-y location) y)
        (format stream "~&")
        (dotimes (x width)
          (declare (type fixnum x))
          (setf (location-x location) x)
          (format stream "~c"
            (cell-character
              (grid-cell-at grid location))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-group (grid start-cell)
  "Beginning with the START-CELL, being a component of the GRID, marks
   it and the reachable cells in its surrounding and returns a
   representative string identifier designating the tally and character
   composing the group.
   ---
   The thus returned identifier constitutes a string composed of the
   START-CELL's character, repeated a tally of times equal to the
   group size, that is, the number of connected eligible cells. If, for
   instance, the START-CELL contains the character '-', and the detected
   group size equals four, the result comprises '----'."
  (declare (type Cell start-cell))
  (let ((group-size 0))
    (declare (type (integer 0 *) group-size))
    (labels
        ((check-neighbor (cell direction)
          "Checks whether the CELL abuts a neighbor in the DIRECTION
           containing the same character without having been visited
           yet, on confirmation moving to the same and indagating its
           surroundings recursively, otherwise partaking of no further
           action, in any case returning no value."
          (declare (type Cell      cell))
          (declare (type direction direction))
          (when (cell-has-unvisited-neighbor-at grid cell direction)
            (visit-cell
              (cell-neighbor-at grid cell direction)))
          (values))
         
         (visit-cell (cell)
          "Marks this CELL as visited, prepends it to the GROUP, and
           checks the CELL's abutting neighbors in all four cardinal
           directions for containing the same character without having
           been visited yet, on confirmation invoking this function
           on each eligible candidate, in any case returning no value."
          (declare (type Cell cell))
          (unless (cell-visited-p cell)
            (incf group-size)
            (setf (cell-visited-p cell) T))
          (check-neighbor cell :left)
          (check-neighbor cell :top)
          (check-neighbor cell :right)
          (check-neighbor cell :bottom)
          (values)))
      
      (visit-cell start-cell)
      
      (the string
        (make-string group-size
          :initial-element (cell-character start-cell))))))

;;; -------------------------------------------------------

(declaim (type (hash-table-of string instruction) +INSTRUCTION-TABLE+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TABLE+ (make-hash-table :test #'equal)
  "Associates with each recognized group specifier, represented by a
   string whose tally and characters designate its type, an
   ``instruction''.")

;;; -------------------------------------------------------

;; Build the +INSTRUCTION-TABLE+.
(flet ((add-instruction (group instruction)
        "Associates the GROUP specifier with the INSTRUCTION and returns
         no value."
        (declare (type string      group))
        (declare (type instruction instruction))
        (setf (gethash group +INSTRUCTION-TABLE+) instruction)
        (values)))
  (add-instruction "."      :push-1)
  (add-instruction "-"      :push-minus-1)
  (add-instruction ".."     :input)
  (add-instruction "--"     :output)
  (add-instruction "..."    :subtract)
  (add-instruction "---"    :factorial)
  (add-instruction "...."   :swap)
  (add-instruction "----"   :duplicate)
  (add-instruction "....."  :no-op)
  (add-instruction "-----"  :no-op)
  (add-instruction "......" :set-marker)
  (add-instruction "------" :jump-to-marker)
  (values))

;;; -------------------------------------------------------

(defun get-instruction-for (group)
  "Returns the ``instruction'' associated with the GROUP, resorting to
   the default ``no-op'' object upon the absence of an association."
  (declare (type string group))
  (the instruction (gethash group +INSTRUCTION-TABLE+ :no-op)))

;;; -------------------------------------------------------

(defun extract-instructions (grid)
  "Returns for the Cerberus code GRID a vector of instructions entailed
   in the same."
  (declare (type Grid grid))
  (let ((ip (make-location 0 1)))
    (declare (type Location ip))
    (labels
        ((current-cell ()
          "Returns the GRID cell at the instruction pointer IP, or
           ``NIL'' if none such exists."
          (the (or null Cell)
            (when (grid-contains-point-p grid ip)
              (grid-cell-at grid ip))))
         
         (advance-ip ()
          "Starting at the IP location, moves the instruction pointer
           IP to the next unvisited GRID cell, and returns no value."
          (loop do
            (cond
              ((not (grid-contains-point-p grid ip))
                (loop-finish))
              ((cell-visited-p (current-cell))
                (location-move ip :right))
              (T
                (loop-finish))))
          (values)))
      
      (the (simple-array instruction (*))
        (loop
          while   (grid-contains-point-p grid ip)
          collect (get-instruction-for (form-group grid (current-cell)))
          into    instructions
          do      (advance-ip)
          finally (return
                    (coerce instructions
                      '(simple-array instruction (*)))))))))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the INSTRUCTIONS and imbues them with effect, while
   returning no value."
  (declare (type (vector instruction *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (stack       NIL)
          (markers     (make-hash-table :test #'eql)))
      (declare (type fixnum                         ip))
      (declare (type (or null instruction)          instruction))
      (declare (type (stack-of integer)             stack))
      (declare (type (hash-table-of integer fixnum) markers))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION, and returns
             no value."
            (setf instruction
              (when (< ip (1- (length instructions)))
                (aref instructions (incf ip))))
            (values))
           
           (move-to (new-ip-location)
            "Moves the instruction pointer IP to the NEW-IP-LOCATION,
             updates the current INSTRUCTION, and returns no value."
            (declare (type fixnum new-ip-location))
            (setf ip new-ip-location)
            (setf instruction (aref instructions ip))
            (values))
           
           (compute-factorial (n)
            "Computes and returns the factorial of the integer N."
            (declare (type (integer 1 *) n))
            (the (integer 1 *)
              (loop
                for i
                  of-type (integer 1 *)
                  from 1
                  upto n
                for factorial
                  of-type (integer 1 *)
                  =       i
                  then    (* factorial i)
                finally
                  (return factorial))))
           
           (get-marker (name)
            "Returns the instruction pointer location associated with
             the marker NAME, if such exists, otherwise responding with
             ``NIL''."
            (declare (type integer name))
            (the (or null fixnum)
              (gethash name markers)))
           
           (set-marker (name ip-location)
            "Associates the marker NAME with the instruction pointer
             position IP-LOCATION and returns no value."
            (declare (type integer name))
            (declare (type fixnum  ip-location))
            (setf (gethash name markers) ip-location)
            (values)))
        
        (loop while instruction do
          (case instruction
            ;; End of code.
            ((NIL)
              (loop-finish))
            
            ;; "."
            (:push-1
              (push 1 stack)
              (advance))
            
            ;; "-"
            (:push-minus-1
              (push -1 stack)
              (advance))
            
            ;; ".."
            (:input
              (format T "~&Please input an integer: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (push input stack))
              (advance))
            
            ;; "--"
            (:output
              (write (pop stack))
              (advance))
            
            ;; "..."
            (:subtract
              (let ((a (pop stack))
                    (b (pop stack)))
                (declare (type integer a))
                (declare (type integer b))
                (push (- a b) stack))
              (advance))
            
            ;; "---"
            (:factorial
              (let ((a (pop stack)))
                (declare (type integer a))
                (push
                  (cond
                    ((plusp a)
                      (compute-factorial a))
                    ((zerop a)
                      0)
                    ((minusp a)
                      (- (compute-factorial (abs a))))
                    (T
                      (error "Invalid factorial argument: ~a." a)))
                  stack))
              (advance))
            
            ;; "...."
            (:swap
              (rotatef (first stack) (second stack))
              (advance))
            
            ;; "----"
            (:duplicate
              (push (first stack) stack)
              (advance))
            
            ;; "....." or "-----"
            (:no-op
              (advance))
            
            ;; "......"
            (:set-marker
              (let ((marker-name (pop stack)))
                (declare (type integer marker-name))
                (set-marker marker-name ip))
              (advance))
            
            ;; "------"
            (:jump-to-marker
              (let ((marker-name (pop stack)))
                (declare (type integer marker-name))
                (let ((ip-location (get-marker marker-name)))
                  (declare (type (or null fixnum) ip-location))
                  (cond
                    (ip-location
                      (move-to ip-location)
                      (advance))
                    (T
                      (advance))))))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Cerberus-code (code)
  "Interprets the piece of Cerberus CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions
      (make-grid-from-code code)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Cerberus-instructions (instructions)
  "Interprets the piece of Cerberus INSTRUCTIONS and returns no value."
  (declare (type (vector instruction *) instructions))
  (process-instructions instructions)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code generator.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of instruction string) +PATTERNS+))

;;; -------------------------------------------------------

(defparameter +PATTERNS+
  (make-hash-table :test #'eq)
  "Associates with each Cerberus ``instruction'' a string representation
   of its three-rows pattern.")

;;; -------------------------------------------------------

(flet ((add-pattern (instruction
                     first-row-pattern
                     second-row-pattern
                     third-row-pattern)
        "Associates with the Cerberus INSTRUCTION a three-rows' pattern
         composed by stacking the FIRST-ROW-PATTERN, SECODN-ROW-PATTERN,
         and THRID-ROW-PATTERN, and returns no value."
        (declare (type instruction instruction))
        (declare (type string      first-row-pattern))
        (declare (type string      second-row-pattern))
        (declare (type string      third-row-pattern))
        (setf (gethash instruction +PATTERNS+)
              (format NIL "~a~%~a~%~a"
                first-row-pattern
                second-row-pattern
                third-row-pattern))
        (values)))
  
  (add-pattern :push-1
    "..-.--"
    ".---.-"
    "..-.--")
  (add-pattern :push-minus-1
    "..-.--"
    ".-...-"
    "..-.--")
  (add-pattern :input
    "..-..--.-.---"
    ".---.--.--..-"
    "..-..-.--...-")
  (add-pattern :output
    ".-...-"
    "..-.--"
    "..-.--")
  (add-pattern :subtract
    ".--.--"
    ".-...-"
    ".--.--")
  (add-pattern :factorial
    "..-..-"
    ".---.-"
    "..-..-")
  (add-pattern :swap
    "..-"
    ".--"
    ".--")
  (add-pattern :duplicate
    "..-..--.--.--"
    ".---.-..-...-"
    "..-..-..--.--")
  (add-pattern :set-marker
    "...-.---.--.--"
    ".--...-..-...-"
    "..---.-..--.--")
  (add-pattern :jump-to-marker
    "..-..-.-----.-..--.--"
    ".---.--..-...-..-...-"
    "..-..--...-.---.--.--")
  (values))

;;; -------------------------------------------------------

(defun get-grid-for-instruction (instruction)
  "Returns a fresh ``Grid'' representing the INSTRUCTION's pattern, or
   signals an error if no such correlation exists."
  (declare (type instruction instruction))
  (let ((pattern (gethash instruction +PATTERNS+)))
    (declare (type (or null string) pattern))
    (the Grid
      (if pattern
        (make-grid-from-code pattern)
        (error "Invalid instruction: ~s." instruction)))))

;;; -------------------------------------------------------

(defun make-grid-from-instructions (instructions)
  "Creates and a new ``Grid'' produced from the patterns appertaining to
   the INSTRUCTIONS."
  (declare (type (vector instruction *) instructions))
  (let ((grid (make-empty-grid)))
    (declare (type Grid grid))
    (loop for instruction of-type instruction across instructions do
      (grid-append grid
        (get-grid-for-instruction instruction)))
    (the Grid grid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Tiangou compiler.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-Tiangou (code)
  "Creates and returns a vector of Cerberus instructions from the piece
   of Tiangou CODE."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of instruction) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, updates the current CHARACTER, and returns no
               value."
              (setf character
                (when (< position (1- (length code)))
                  (char code (incf position))))
              (values))
             
             (space-character-p (subject)
              "Checks whether the SUBJECT represents a space character,
               returning on confirmation a ``boolean'' value of ``T'',
               otherwise ``NIL''."
              (declare (type character subject))
              (the boolean
                (not (null
                  (member subject '(#\Space #\Tab) :test #'char=)))))
             
             (skip-spaces ()
              "Starting at the current POSITION, skips zero or more
               adjacent spaces and returns no value."
              (loop
                while (and character (space-character-p character))
                do    (advance))
              (values))
             
             (expect-spaces ()
              "Checks whether the current CHARACTER represents a space,
               on confirmation skipping all adjacent spaces and
               returning no value, otherwise an error is signaled.
               ---
               If the CHARACTER equals ``NIL'', no further action is
               committed."
              (when character
                (if (space-character-p character)
                  (skip-spaces)
                  (error "Expected one or more space, but encountered ~
                          '~c'."
                    character)))
              (values))
             
             (skip-empty-lines ()
              "Starting at the current POSITION, skips all adjacent
               newlines and spaces, returning no value."
              (loop
                while (and character
                           (or (space-character-p character)
                               (char= character #\Newline)))
                do    (advance))
              (values)))
          
          (loop do
            (case character
              ;; End of file.
              ((NIL)
                (loop-finish))
              
              ;; Spaces? => Skip.
              ((#\Space #\Tab)
                (skip-spaces))
              
              ;; Push 1 to stack.
              (#\p
                (push :push-1 instructions)
                (advance)
                (expect-spaces))
              
              ;; Push -1 to stack.
              (#\q
                (push :push-minus-1 instructions)
                (advance)
                (expect-spaces))
              
              ;; Push user input to stack.
              (#\i
                (push :input instructions)
                (advance)
                (expect-spaces))
              
              ;; Output popped head of stack.
              (#\o
                (push :output instructions)
                (advance)
                (expect-spaces))
              
              ;; Push difference of two popped stack elements to stack.
              (#\-
                (push :subtract instructions)
                (advance)
                (expect-spaces))
              
              ;; Push factorial of popped element to stack.
              (#\!
                (push :factorial instructions)
                (advance)
                (expect-spaces))
              
              ;; Swap the first and second stack element.
              (#\"
                (push :swap instructions)
                (advance)
                (expect-spaces))
              
              ;; Duplicate top of stack.
              (#\:
                (push :duplicate instructions)
                (advance)
                (expect-spaces))
              
              ;; Define a marker with popped element as a name.
              (#\<
                (push :set-marker instructions)
                (advance)
                (expect-spaces))
              
              ;; Jump to marker with popped element as a name.
              (#\>
                (push :jump-to-marker instructions)
                (advance)
                (expect-spaces))
              
              ;; Skip trailing newlines and ascertain that no code
              ;; follows.
              (#\Newline
                (skip-empty-lines)
                (if character
                  (error "A Tiangou program must consist of exactly ~
                          one line, but additional content was found ~
                          in the character '~c' at position ~d."
                    character position)
                  (loop-finish)))
              
              ;; Any other character is proscribed.
              (otherwise
                (error "Invalid character '~a' at position ~d."
                  character position)))))))
    
    (the (simple-array instruction (*))
      (coerce (nreverse instructions)
        '(simple-array instruction (*))))))

;;; -------------------------------------------------------

(defun interpret-Tiangou (code)
  "Compiles the piece of Tiangou CODE into Cerberus instructions,
   executes these, and returns no value."
  (declare (type string code))
  (process-instructions
    (compile-Tiangou code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-Cerberus-code
".-...-..--.--....-..-.-..--
.-.---..-.--.----..-..-..--
--.-..-.--.-.....-.-..--.--")

;;; -------------------------------------------------------

;; Explicitly define the instructions for a truth-machine program and
;; execute them.
(interpret-Cerberus-instructions
  (extract-instructions
    (make-grid-from-instructions
      (coerce
        (list :input
              :push-1
              :set-marker
              :duplicate
              :duplicate
              :output
              :jump-to-marker)
        '(vector instruction *)))))

;;; -------------------------------------------------------

;; An infinitely repeating cat program delineated by its instructions.
(interpret-Cerberus-instructions
  (make-array 7
    :element-type     'instruction
    :initial-contents '(:push-1
                        :set-marker
                        :input
                        :duplicate
                        :output
                        :push-1
                        :jump-to-marker)))

;;; -------------------------------------------------------

;; The same infinite cat program as above, however, supplied in the form
;; of an automatically generated code grid.
;; 
;; Please note that the convenience vianded by the automated code
;; production has possibly been defrayed with an unoptimized grid
;; output.
(interpret-Cerberus-code
"..-.--...-.---.--.--..-..--.-.---..-..--.--.--.-...-..-.--..-..-.-----.-..--.--
.---.-.--...-..-...-.---.--.--..-.---.-..-...-..-.--.---.-.---.--..-...-..-...-
..-.--..---.-..--.--..-..-.--...-..-..-..--.--..-.--..-.--..-..--...-.---.--.--")

;;; -------------------------------------------------------

;; Compile the Tiangou truth-machine code and execute it as a Cerberus
;; program.
(interpret-Tiangou "i p < : : o >")
