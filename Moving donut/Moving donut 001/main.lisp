;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Moving donut", invented by the Esolang user "Cinnamony" and
;; presented on June 20th, 2023 the peculiarity of which wones in its
;; programs' arrangement on a two-dimensional shape, similar to a
;; labyrinth, along whose interior an instruction pointer, conceived as
;; a "donut", perambulates, executing the encountered tokens as
;; instructions.
;; 
;; 
;; Concept
;; =======
;; The Moving donut programming language subscribes to the
;; two-dimensional species, the foundation of which is realized in terms
;; of a maze, presenting this daedoloid aedes to a donut as the sole
;; traveler, the guise of an instruction pointer, whose navigation and
;; encounters with operational symbols defines a program's causata.
;; 
;; == MOVING DONUT: AN INSTRUCTION POINTER INSIDE OF A MAZE ==
;; The language's dioristic indicium partakes of its commorancy in the
;; fact that a two-dimensional reticulation serves as the program's
;; expression, an effective establishment of a maze whose boundaries,
;; the walls, serve to impound and conduct the traveler's trajectory.
;; 
;; == THE MAZE: "#" FOR WALLS, "O" FOR D[O]NUT ==
;; The usual maze's ordonnance enlists among its components as the
;; foundational essentials the walls, expressed in hash signs ("#") and
;; the donut, represented anenst its initial location by a majuscular
;; "O".
;; 
;; Any other character may contribute its presence to the path betwixt
;; the "#" walls, with a select few's participation accompassing an
;; actual operational response.
;; 
;; == THE DONUT: LOCATION + DIRECTION ==
;; The delectable protagonist, our donut, is delineated by a componency
;; that enumerates an attributes' twain: namely, the two-dimensional
;; location, compact of its x- and y-coordinates inside of the grid, and
;; the current direction.
;; 
;; The sole instruction pointer's assumption being its agency, the
;; participant's initial location is determined by the symbol "O".
;; Exactly on such agent must be present in a Moving donut program.
;; 
;; == THE MOTION PRINCIPLE: TURNING AT WALLS ==
;; At the execution's inchoation airted in a dextral orientation, the
;; donut's gest propagates along this steady course until the collision
;; with a wall. Depending on the exact circumstances' configuration, the
;; following procedure ensues:
;; 
;;   (1) The donut rotates by 90 degrees in clockwise direction, from
;;       its own perspective. If no wall impedes the advancement, the
;;       donut ultimately assumes this airt as its current state.
;;   (2) If a wall incapacitates the traversal in the probed route, the
;;       donut resorts to the failed direction's obverse, which
;;       concomitantly constitutes a 90 degrees turn in
;;       counter-clockwise direction from its original state ere the
;;       contact with the wall. Again, if no wall impedes the route, the
;;       donut retains this state as its new orientation.
;;   (3) If the second attempt has also been foiled by a wall, the donut
;;       is considered to be entangled in an impasse, which immediately
;;       terminates the program.
;;       Please note that the donut, following its initial contact with
;;       a wall, never attempts to return in the athwart direction of
;;       its location; otherwise any program would establish an infinite
;;       loop. This means, for instance, that a pointer that, traveling
;;       in a sinistrodextral course, from left to right, when resting
;;       in front of a wall, will never resort to return in a
;;       dextrosinistral manner, back from right to left.
;; 
;; The coming tabular exposition illustrates the instruction pointer's
;; deportment in regard to its movement options. Please heed the
;; following points:
;; 
;;   (1) The instruction pointer is expected to have encountered a wall
;;       while traveling in its "current direction"; as a corollary, the
;;       continued propagation in the same airt eludes its options.
;;   (2) The instruction pointer, traveling in its "current direction",
;;       will never resort to its athwart motion, that is, a volte-face
;;       of 180 degrees; hence, the obverse direction does not tally
;;       among its choices.
;;   (3) The instruction pointer will always incline towards the
;;       direction orthogonal in the clockwise position from its
;;       "current direction" as the "preferred direction".
;;   (4) In the case of the "preferred direction" being obstructed, the
;;       "alternative direction", always orthogonal in a
;;       counterclockwise airt, will be probed.
;; 
;; Proceeding from the aboon quadruple regulations, the following
;; behavior applies to the change in direction:
;; 
;;   ---------------------------------------------------------------
;;   Current direction | Preferred direction | Alternative direction
;;   ------------------+---------------------+----------------------
;;   right             | down                | up
;;   ...............................................................
;;   down              | left                | right
;;   ...............................................................
;;   left              | up                  | down
;;   ...............................................................
;;   up                | right               | left
;;   ---------------------------------------------------------------
;; 
;; A more complete treatise, adduced with utmost compendiousness, shall
;; juxtapose all stages of contemplation --- even embracing those
;; notions manumitted from a wall's obtrusion --- in a single view:
;; 
;;   ------------------------------------------------------------------
;;   Current   | Natural course | Preferred course | Alternative course
;;   direction | if free        | if blocked       | if blocked
;;   ----------+----------------+------------------+-------------------
;;   right     | right          | down             | up
;;   ..................................................................
;;   down      | down           | left             | right
;;   ..................................................................
;;   left      | left           | up               | down
;;   ..................................................................
;;   up        | up             | right            | left
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK: A DONOR OF MEMORY AND EFFECT ==
;; That departments inherent to the language attained from brainfuck
;; constitutes the memory model on one hand, and a preponderance among
;; the instructions in design and effect on the other.
;; 
;; == THE MEMORY: A INFINITE BYTE SEQUENCE ==
;; Rather latent than patent, brainfuck's influence is exerted in the
;; program memory as a bilaterally infinite expanse of cells, any one
;; among these stores a single unsigned byte in the range [0, 255],
;; initialized to zero (0).
;; 
;; A mobile cell pointer operates on these constituents, at any time
;; maintaining the currently active member, whose indagation and
;; manipulation endows the language with its computational entelechy.
;; 
;; == INSTRUCTIONS: BRAINFUCK'S OCTUPLE, AND SOME MORE ==
;; Moving donut's majority of operations registers its provenance in the
;; brainfuck entheus, comprehending the basic arithmetics, eath memory
;; management, character input and output, as well as jump-based control
;; flow.
;; 
;; A less extensive subset tallies among the advenient contrivances of
;; the Moving donut language itself, most significantly, a switch vested
;; with the competence to alter betwixt the standard character input and
;; output conduits and a numeric variant.
;; 
;; Latin minuscles, when engaged in collision with the donut, are
;; printed ipsissima verba.
;; 
;; 
;; Architecture
;; ============
;; A bivial species of treatise ought to comprehended in the
;; architecture's adhibition: imprimis, that section appertaining to the
;; reticulate program form, subsequently the substratum allocated to the
;; tape-like memory.
;; 
;; == PROGRAMS ARE GRIDS ==
;; The kenspeckle ordonnance in attendance of a Moving donut program
;; expresses its frame as a maze, the foundry plan's capacitation
;; permits a grid design's superimposition.
;; 
;; Deviants in a composition that endeavor's a rectangular shape's
;; patration may be remedied by inserting spaces or other ineffective
;; sentinel symbols in the irregular locations.
;; 
;; A grid cell's onus resolve to the lococession for a singular symbol's
;; presence.
;; 
;; A consectary from this reticulation's lealty to a Cartesian
;; formation, every cell answers to a coordinate twain, conceivable
;; either as the traditional mathematical (x, y) jumelle, or a more
;; tabular exposition as (column, row)-tuple.
;; 
;; The potential for infinity maintains its woning in such a topological
;; composition, as lacunae in the walls introduce the donut's capacity
;; for escaping from the natural confines. This species of transpiration
;; may be handled by the perversion's admission, and the concluding
;; assumption that all unspecified segments, as has been already
;; proposed for the irregularity in the rectangular format, obey to a
;; clandestine no-operation symbol's attendance.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF BYTES ==
;; A most conspicuous expression of the language's brainfuck cleronomy,
;; the program memory's manifestation issues in the form of a
;; bilaterally infinite tape of unsigned byte values, stored in cells.
;; 
;; Each cell in this linear arrangement encumbers itself with an aefauld
;; unsigned byte's castaldy, an occupant in the integer range [0, 255],
;; whose bournes' transgression results in an immediate wrapping around
;; in order to adjust to this constraint. At the program's inchoation,
;; any cell harbors the default value of zero (0).
;; 
;; A cell pointer, or simply pointer, is ordained to select at any
;; instant the currently active cell, establishing the sole entity
;; endowed with respondency to indagation and manipulations. Being a
;; mobile concept, operations exist for the cell pointer's gradual
;; translation in both sinistral and dextral direction.
;; 
;; 
;; Data Types
;; ==========
;; The Moving donut language deploys two species of objects reckoned in
;; approximate equiparation: signed integers and characters.
;; 
;; == INTEGERS ==
;; The paravaunt significance allotted to integers in brainfuck enjoys a
;; fortification by the advient supererogation installed through the
;; input/output mode toggling command "A", which warklume now
;; homologates direct integer input and output, as counterdistinguished
;; from the original character conversion routines.
;; 
;; == CHARACTERS ==
;; The first moiety in commensuration, characters, desumed from the
;; ASCII repertoire, are entalented with brainfuck's act of
;; administration, which consignes to them the accommodation of the
;; input and output conduits' commorancy, and augmented by further
;; utility in immediate printing when supplied as Latin minuscles.
;; 
;; 
;; Instructions
;; ============
;; The Moving donut language's instruction set amplects, beside the
;; brainfuck cleronomy --- in a few cases modulated in a fashion that
;; admits the scion's diorisms ---, warklooms whose dation vouchsafes
;; them the input and output modalities' affectation, as well as
;; immediate printing commodities for Latin majuscles.
;; 
;; The partially metaphorical constituents commorant in the symbols'
;; haecceity rede the syntactical and operational components'
;; conflation when attending to the instructions and their causata, in
;; particular trenchant as applicable to the walls, designated as hash
;; signs ("#"), whose influence emanates in a rather indirection manner.
;; 
;; == OVERVIEW ==
;; The following apercu shall lecture those fraisting for the symbols
;; and their responses in a compendious manner:
;; 
;;   ------------------------------------------------------------------
;;   Symbol | Effect
;;   -------+----------------------------------------------------------
;;   O      | Specifies the start location of the donut in the grid.
;;          | Exactly one such token must be present, otherwise an
;;          | error of the type "InvalidDonutCountError" is signaled.
;;          | Its subsequent encounter after the program start does
;;          | not bear any effect.
;;   ..................................................................
;;   #      | Symbolizes a wall, that is, impedes the donut, the same
;;          | first attempts to find a route to its right, then, if
;;          | failing to its left from the original state, and, upon an
;;          | iterum impasse, terminates the program.
;;   ..................................................................
;;   >      | Moves the cell pointer one step to the right.
;;          | This instruction constitutes a verbatim appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   <      | Moves the cell pointer one step to the left.
;;          | This instruction constitutes a verbatim appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   +      | Increments the current cell by one. If the value exceeds
;;          | the maximum of 255, it wraps around to the minimum of
;;          | zero (0).
;;          | This instruction constitutes a verbatim appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   -      | Decrements the current cell by one. If the value descends
;;          | below the minimum of zero (0), it wraps around to the
;;          | maximum of 255.
;;          | This instruction constitutes a verbatim appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   ,      | If the current input/output mode assumes the character
;;          | variant, queries the standard input for an ASCII
;;          | character whose character code is stored in the current
;;          | cell.
;;          | If the current input/output mode assumes the numeric
;;          | variant, queries the standard input for a signed integer
;;          | number, which is contingently wrapped around into the
;;          | unsigned byte range [0, 255], and stored in the current
;;          | cell.
;;          | This instruction constitutes a modified appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   .      | If the current input/output mode assumes the character
;;          | variant, prints to the standard output the character
;;          | whose ASCII code corresponds to the current cell value.
;;          | If the current input/output mode assumes the numeric
;;          | variant, prints the current cell value in its numeric
;;          | form to the standard output.
;;          | This instruction constitutes a modified appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   [      | If the current cell value equals zero (0), relocates the
;;          | donut to the matching "]" token, thus skipping all
;;          | interstitial operations.
;;          | Otherwise proceeds as usual.
;;          | This instruction constitutes a modified appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   ]      | If the current cell value does not equal zero (0),
;;          | reverses the donut's direction and jumps to the matching
;;          | "[" token.
;;          | Otherwise proceeds as usual.
;;          | This instruction constitutes a modified appropriation
;;          | from brainfuck.
;;   ..................................................................
;;   A      | Toggles the input/output mode from character to numeric,
;;          | and vice versa.
;;          | The character mode assumes the program's default.
;;   ..................................................................
;;   char   | Prints the lower-case character {char} to the standard
;;   ****   | output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Moving donut's protolog, being a device invested in rather lucid
;; diction, yet suffers from a few ambivalencies' cumbrance, a subset
;; thereof shall now be adduced.
;; 
;; == HOW SHALL THE SYSTEM RESPOND TO ANOMALOUS DONUT COUNTS? ==
;; The original specification relates of the donut as an aefauld entity,
;; whose delegacy resides in the "O" letter to empight the visual
;; equivalency to its initial position. Maugre its express reference to
;; the programming language "Mice in a maze", the perimeter of the same
;; amplects the capacity for a multitude of equivalent instruction
;; pointer representatives, the singularity in the donut's case eludes
;; an immediate transliteration.
;; 
;; It has been adjudged to impose one and only one donut's
;; participation, tantamount to exactly one "O" in the program; in the
;; aliter circumstance, an error of the type "InvalidDonutCountError"
;; shall be inflicted.
;; 
;; == HOW SHALL AN ESCAPING DONUT BE HANDLED? ==
;; Moving donut programs exist in the expectancy that a series of walls
;; furnishes a perfect bourne around its effective ambitus. However, a
;; conclusive statement about the treatment of a fugitive donut, having
;; escaped through an aperture in the mural confine, remains in
;; destitution.
;; 
;; It has been adjudged that such a renegade pastry shall be attended
;; with an agathokakological adhibition that homologates its manumission
;; into the boundless space in the surrounding, but also inflicts its
;; status with the malidiction of infinite desultory perambulation.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, founded upon two stages' sequential application, namely
;; the conversion of the flat Moving donut source string into a
;; two-dimensional grid, manifesting in a sparse matrix of characters,
;; and the subsequent evaluation of the same.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-25
;; 
;; Sources:
;;   [esolang2023Movingdonut]
;;   The Esolang contributors, "Moving donut", June 20th, 2023
;;   URL: "https://esolangs.org/wiki/Moving_donut"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype size ()
  "The ``size'' type defines an unsigned integer value greater than or
   equal to zero (0), utible especially as a diimension or index
   denotation."
  '(integer 0 *))

;; --------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries which map keys of the KEY-TYPE to values of the
   VALUE-TYPE, assuming the default configuration of ``T'' in both
   cases."
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

;; --------------------------------------------------------

(deftype list-of (&optional (element-type T) (size NIL size-supplied-p))
  "The ``list-of'' type defines a list composed of elements that conform
   to the ELEMENT-TYPE, defaulting to the comprehensive ``T'', the tally
   of which must either equal exactly the SIZE, or, if none is supplied,
   an arbitrary account."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (and size-supplied-p
                     (= size
                        (length (the list candidate))))
                T)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;; --------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a two-dimensional arrangement
   of characters in a sparse formation as a hash table, the entries of
   which answer to a ``Location'', supplying a character object."
  '(hash-table-of Location character))

;; --------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid directions for a donut
   (instruction pointer, IP) to traverse."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype io-mode ()
  "The ``io-mode'' type enumerates the recognized variants of input and
   output modes in the context of brainfuck communication facilities."
  '(member
    :character-mode
    :number-mode))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   adjacent bits, thus being an occupant of the closed integer range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-direction-options (current-direction)
  "Returns for the CURRENT-DIRECTION the possible subsequent directions
   either in the case of the current state's perpetuation or its
   redirection.
   ----
   The always treble-length list produced by this operation always
   proceeds by the principle of decreasing preference, that is:
     (1) The first element equals the CURRENT-DIRECTION.
     (2) The second element is tantamount to an orthogonal (90 degrees)
         rotation in clockwise orientation from the CURRENT-DIRECTION's
         perspective.
     (3) The third and desinent entry delivers the direction orthogonal
         (90 degrees) in counter-clockwise orientation from the
         CURRENT-DIRECTION.
   Please note that the function never returns the airt opposite to the
   CURRENT-DIRECTION, forecause the Moving donut programming language
   abstains from the homologation of a volte-face for its instruction
   pointer (IP), the eponymous moving donut."
  (declare (type direction current-direction))
  (the (list-of direction 3)
    (case current-direction
      (:right    '(:right :down  :up))
      (:down     '(:down  :left  :right))
      (:left     '(:left  :up    :down))
      (:up       '(:up    :right :left))
      (otherwise (error "Invalid direction: ~s." current-direction)))))

;;; -------------------------------------------------------

(defun get-opposite-direction (current-direction)
  "Returns the direction obverse to the CURRENT-DIRECTION."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:right    :left)
      (:down     :up)
      (:left     :right)
      (:up       :down)
      (otherwise (error "Invalid direction: ~s." current-direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input/output mode operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-opposite-io-mode (current-io-mode)
  "Returns for the CURRENT-IO-MODE the obverse mode."
  (declare (type io-mode current-io-mode))
  (the io-mode
    (case current-io-mode
      (:character-mode :number-mode)
      (:number-mode    :character-mode)
      (otherwise       (error "Invalid I/O mode: ~s."
                         current-io-mode)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class encapsulates a two-dimensional Cartesian
   location with integer-precision coordinates, conceivable either as an
   (x, y)- or a (column, row)-tuple."
  (x 0 :type integer)
  (y 0 :type integer))

;;; -------------------------------------------------------

(defun location-translate (location direction)
  "Translates the LOCATION one step into the DIRECTION and returns no
   value."
  (declare (type Location  location))
  (declare (type direction direction))
  (case direction
    (:right    (incf (location-x location)))
    (:down     (incf (location-y location)))
    (:left     (decf (location-x location)))
    (:up       (decf (location-y location)))
    (otherwise (error "Invalid direction ~s for translating ~
                       the location ~s."
                 direction location)))
  (values))

;;; -------------------------------------------------------

(defun location-get-translated-copy (location direction)
  "Returns a copy of the LOCATION translated one step into the
   DIRECTION."
  (declare (type Location  location))
  (declare (type direction direction))
  (let ((new-location (copy-location location)))
    (declare (type Location new-location))
    (location-translate new-location direction)
    (the Location new-location)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((cells
    :initarg       :cell
    :initform      (make-hash-table :test #'equalp)
    :type          character-matrix
    :documentation "A sparse two-dimensional arrangement of characters,
                    amenable to non-negative integer indices.")
   (width
    :initarg       :width
    :initform      0
    :type          size
    :documentation "The tally of column.")
   (height
    :initarg       :height
    :initform      0
    :type          size
    :documentation "The tally of rows."))
  (:documentation
    "The ``Grid'' class furnishes a sparse two-dimensional grid of
     characters, the same in their conjugality represents a piece of
     Moving donut source code in an eath navigable reticulate
     topology."))

;;; -------------------------------------------------------

(defun grid-cell-at-coordinates (grid column row)
  "Returns the character stored at the GRID location specified by the
   COLUMN and ROW indices."
  (declare (type Grid grid))
  (declare (type size column))
  (declare (type size row))
  (the character
    (gethash (make-location column row)
      (slot-value grid 'cells)
    #\Space)))

;;; -------------------------------------------------------

(defun grid-cell-at-location (grid location)
  "Returns the character stored at the GRID's LOCATION."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the character
    (gethash location
      (slot-value grid 'cells)
    #\Space)))

;;; -------------------------------------------------------

(defun build-grid (code)
  "Creates and returns a new ``Grid'' which establishes a
   two-dimensional perspective on the piece of Moving donut source
   CODE."
  (declare (type string code))
  (let ((grid (make-instance 'Grid)))
    (declare (type Grid grid))
    (with-slots (cells width height) grid
      (declare (type character-matrix cells))
      (declare (type size             width))
      (declare (type size             height))
      (flet ((set-grid-cell (column row token)
              "Stores the TOKEN in the GRID at the location specified by
               the COLUMN and ROW indices, updates the GRID's
               dimensions, and returns no value."
              (declare (type size      column))
              (declare (type size      row))
              (declare (type character token))
              (setf (gethash (make-location column row) cells) token)
              (setf width  (max width  (1+ column)))
              (setf height (max height (1+ row)))
              (values)))
        (loop
          with column of-type size      =      0
          and  row    of-type size      =      0
          for  token  of-type character across code
          do
            (case token
              (#\Newline
                (incf row)
                (setf column 0)
                (setf height (max height row)))
              (otherwise
                (set-grid-cell column row token)
                (incf column))))))
    (the Grid grid)))

;;; -------------------------------------------------------

(defun grid-get-start-points (grid)
  "Returns a list of the locations in the GRID occupied by a donut, this
   means, an \"O\" character, in no specific order."
  (declare (type Grid grid))
  (with-slots (cells width height) grid
    (declare (type character-matrix cells))
    (declare (type size             width))
    (declare (type size             height))
    (the (list-of Location)
      (loop
        for location
          of-type Location
          being the hash-keys in cells
        using
          (hash-value token)
        when (char= token #\O)
          collect location))))

;;; -------------------------------------------------------

(defun grid-passable-point-p (grid point)
  "Determines whether the POINT in the GRID can be traversed, that is,
   it does not constitute a wall (\"#\"), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location point))
  (the boolean
    (char/= (grid-cell-at-location grid point) #\#)))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-slots (cells width height) grid
    (declare (type character-matrix cells))
    (declare (type size             width))
    (declare (type size             height))
    (format stream "~&~d x ~d grid:" width height)
    (dotimes (row height)
      (declare (type size row))
      (format stream "~&")
      (dotimes (column width)
        (declare (type size column))
        (format stream "~c"
          (grid-cell-at-coordinates grid column row))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Donut".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Donut ()
  ((location
    :initarg       :location
    :initform      (make-location 0 0)
    :accessor      donut-location
    :type          Location
    :documentation "The donut's position on the grid.")
   (direction
    :initarg       :direction
    :initform      :right
    :accessor      donut-direction
    :type          direction
    :documentation "The donut's traveling direction.")
   (finished-p
    :initarg       :finished-p
    :initform      NIL
    :accessor      donut-finished-p
    :type          boolean
    :documentation "Determines whether the donut has reached an
                    impasse on the grid, thus effectively forcing a
                    program termination."))
  (:documentation
    "The ``Donut'' constitues the instruction pointer's (IP) mobile
     representative, the essential role of which vindicates the
     Moving donut programming language name's dation."))

;;; -------------------------------------------------------

(defun make-donut (location)
  "Creates and returns a new ``Donut'' empight at the LOCATION, and
   facing in dextral direction."
  (declare (type Location location))
  (the Donut
    (make-instance 'Donut :location location)))

;;; -------------------------------------------------------

(defun donut-move (donut)
  "Moves the DONUT one step into its current direction and returns no
   value."
  (declare (type Donut donut))
  (location-translate
    (slot-value donut 'location)
    (slot-value donut 'direction))
  (values))

;;; -------------------------------------------------------

(defun donut-reverse-direction (donut)
  "Reverses the DONUT's direction and returns no value."
  (declare (type Donut donut))
  (setf (slot-value donut 'direction)
    (get-opposite-direction
      (slot-value donut 'direction)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((donut Donut) stream)
  (declare (type Donut       donut))
  (declare (type destination stream))
  (format stream "(Donut :location (~d, ~d) :direction ~s)"
    (location-x (slot-value donut 'location))
    (location-y (slot-value donut 'location))
    (slot-value donut 'direction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of integer-valued cells, amenable
                    to signed integer indices.")
   (pointer
    :initform      0
    :type          integer
    :documentation "Contains the index (key) of the currently active
                    cell among the CELLS."))
  (:documentation
    "The ``Memory'' class realizes a program's memory as imposed by the
     brainfuck programming language's standard, that is, as a
     bilaterally infinite tape of octet-valued cells, amenable to the
     current unit's indagation in concord with the mobile cell
     pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns no
   value."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns no
   value."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the octet
    (gethash
      (slot-value memory 'pointer)
      (slot-value memory 'cells)
      0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores the NEW-VALUE, contingently succeeding a prior adjustment in
   order to wrap around into the byte range [0, 255], in the MEMORY's
   current cell, and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash
          (slot-value memory 'pointer)
          (slot-value memory 'cells)
          0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell by one, contingently wrapping
   the state around in order to ascertain the byte range [0, 255], and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell by one, contingently wrapping
   the state around in order to ascertain the byte range [0, 255], and
   returns no value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minuscle-p (candidate)
  "Determines whether the CANDIDATE represents a lower-case letter,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (and (alpha-char-p candidate)
           (lower-case-p candidate))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Donut-Count-Error (error)
  ((number-of-donuts
    :initarg       :number-of-donuts
    :initform      (error "Missing number of donuts.")
    :reader        invalid-donut-count-error-number-of-donuts
    :type          (integer 0 *)
    :documentation "The invalid number of donuts specified by the
                    program."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Donut-Count-Error condition))
      (declare (type destination               stream))
      (format stream "Expected exactly one donut, but encountered ~d."
        (invalid-donut-count-error-number-of-donuts condition))))
  (:documentation
    "The ``Invalid-Donut-Count-Error'' condition serves to signal an
     anomalous tally of donut instances on a program grid."))

;;; -------------------------------------------------------

(defun signal-invalid-donut-count-error (number-of-donuts)
  "Signals an ``Invalid-Donut-Count-Error'' apprizing about the
   NUMBER-OF-DONUTS constituting an invalid account."
  (declare (type (integer 0 *) number-of-donuts))
  (error 'Invalid-Donut-Count-Error :number-of-donuts number-of-donuts))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((grid
    :initarg       :grid
    :initform      (error "Missing grid.")
    :type          Grid
    :documentation "The Moving donut program molded into the form of a
                    character grid.")
   (donut
    :initform      NIL
    :type          (or null Donut)
    :documentation "The donut representing the instruction pointer (IP)
                    which traverses the code GRID.")
   (current-token
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the DONUT's location in the GRID.")
   (io-mode
    :initform      :character-mode
    :type          io-mode
    :documentation "Specifies the input/output mode for brainfuck
                    communication conduits.")
   (memory
    :initform      (make-memory)
    :type          Memory
    :documentation "The brainfuck-compatible tape serving as the program
                    memory."))
  (:documentation
    "The ``Interpreter'' class applies itself to the contribution of
     actual effect to a piece of Moving donut source code supplied in a
     grid data structure."))

;;; -------------------------------------------------------

(defun interpreter-find-donut (interpreter)
  "Detects the donut's start position in the INTERPRETER's code grid,
   creates and memorizes its ``Donut'' representation, and returns no
   value."
  (declare (type Interpreter interpreter))
  (let ((available-donuts
          (grid-get-start-points
            (slot-value interpreter 'grid))))
    (declare (type (list-of Location) available-donuts))
    (if (= (length available-donuts) 1)
      (setf (slot-value interpreter 'donut)
        (make-donut
          (copy-location
            (first available-donuts))))
      (signal-invalid-donut-count-error
        (length available-donuts))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-update-current-character (interpreter)
  "Updates the INTERPRETER's current token to the entity commorant at
   its donut's location inside of the traversed grid and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'current-token)
    (grid-cell-at-location
      (slot-value interpreter 'grid)
      (donut-location
        (slot-value interpreter 'donut))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Finds and memorizes the INTERPRETER's donut, initializes its current
   token, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (interpreter-find-donut               interpreter)
  (interpreter-update-current-character interpreter)
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (grid)
  "Creates and returns a new ``Interpreter'' which operates on the GRID
   representation of a Moving donut program."
  (declare (type Grid grid))
  (the Interpreter
    (make-instance 'Interpreter :grid grid)))

;;; -------------------------------------------------------

(defun interpreter-can-move-to-p (interpreter probed-direction)
  "Determines whether the donut maintained by the INTERPRETER is
   capacitated to move into the PROBED-DIRECTION from its current
   location, which implies that no wall impedes the destination cell,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type direction   probed-direction))
  (the boolean
    (grid-passable-point-p
      (slot-value interpreter 'grid)
      (location-get-translated-copy
        (donut-location
          (slot-value interpreter 'donut))
        probed-direction))))

;;; -------------------------------------------------------

(defun interpreter-look-around (interpreter)
  "Indagates for the donut maintained by the INTERPRETER the surrounding
   grid cells in order to determine the next movement direction, on
   success either retaining or modifying the donut's orientation,
   otherwise designating the same as finished, having reached an
   impasse, and in any case returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (grid donut) interpreter
    (declare (type Grid            grid))
    (declare (type (or null Donut) donut))
    (let ((direction-options
            (get-direction-options
              (donut-direction donut))))
      (declare (type (list-of direction) direction-options))
      (loop
        for possible-direction of-type direction in direction-options
        
        when (interpreter-can-move-to-p interpreter possible-direction)
          do
            (setf (donut-direction donut) possible-direction)
            (return NIL)
        
        finally
          (setf (donut-finished-p donut) T))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Moves the donut maintained by the INTERPRETER one step along its
   current position, if not already finished, and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-look-around interpreter)
  (unless (donut-finished-p (slot-value interpreter 'donut))
    (donut-move
      (slot-value interpreter 'donut))
    (interpreter-update-current-character interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-query-for-input (interpreter)
  "Depending on the current input/output mode, queries the user either
   for an ASCII character, whose character code is stored in the
   INTERPRETER's program memory, or an integer intended for the same
   destination, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (io-mode memory) interpreter
    (declare (type Memory  memory))
    (declare (ignorable    memory))
    (declare (type io-mode io-mode))
    (declare (ignorable    io-mode))
    (case io-mode
      (:character-mode
        (format T "~&Please input a character: ")
        (setf (memory-current-cell memory)
              (char-code (read-char))))
      (:number-mode
        (format T "~&Please input an integer: ")
        (setf (memory-current-cell memory)
              (parse-integer
                (read-line))))
      (otherwise
        (error "Invalid I/O mode: ~s." io-mode))))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defun interpreter-issue-output (interpreter)
  "Depending on the input/output mode, prints the current cell value
   stored in the INTERPRETER's memory either as the ASCII character
   whose character code corresponds to the numeric value, or verbatim as
   an integer number to the standard output, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (io-mode memory) interpreter
    (declare (type Memory  memory))
    (declare (ignorable    memory))
    (declare (type io-mode io-mode))
    (declare (ignorable    io-mode))
    (case io-mode
      (:character-mode
        (format T "~c"
          (code-char
            (memory-current-cell memory))))
      (:number-mode
        (format T " ~d"
          (memory-current-cell memory)))
      (otherwise
        (error "Invalid I/O mode: ~s." io-mode))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-jump-forward (interpreter)
  "Proceeding from the current position into the INTERPRETER's grid,
   moves the donut in the opposite airt to the matching back jump
   (\"]\") command, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (grid donut current-token io-mode memory) interpreter
    (declare (type Donut               donut))
    (declare (type (or null character) current-token))
    (declare (ignorable                current-token))
    ;; The donut must search in the obverse direction.
    (interpreter-advance interpreter)
    
    (loop with level of-type fixnum = 0 do
      (cond
        ;; No matching forward jump point found?
        ;; => Unmatched "]".
        ((donut-finished-p donut)
          (error "Unmatched forward jump for donut ~a." donut))
        
        ;; Matching "]"?
        ;; => Conclude search.
        ((and (char= current-token #\])
              (zerop level))
          (loop-finish))
        
        ;; "]" in different level?
        ;; => Continue search.
        ((and (char= current-token #\])
              (not (zerop level)))
          (decf level)
          (interpreter-advance interpreter))
        
        ;; "[" encountered?
        ;; => Increase level and continue search.
        ((char= current-token #\[)
          (incf level)
          (interpreter-advance interpreter))
        
        ;; Any other character?
        ;; => Continue search.
        (T
          (interpreter-advance interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-jump-back (interpreter)
  "Proceeding from the current position into the INTERPRETER's grid,
   moves the donut in the opposite airt to the matching forward jump
   (\"[\") command, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (grid donut current-token io-mode memory) interpreter
    (declare (type Donut               donut))
    (declare (type (or null character) current-token))
    (declare (ignorable                current-token))
    ;; The donut must search in the obverse direction.
    (donut-reverse-direction donut)
    
    (interpreter-advance interpreter)
    
    (loop with level of-type fixnum = 0 do
      (cond
        ;; No matching forward jump point found?
        ;; => Unmatched "]".
        ((donut-finished-p donut)
          (error "Unterminated back jump for donut ~a." donut))
        
        ;; Matching "["?
        ;; => Conclude search.
        ((and (char= current-token #\[)
              (zerop level))
          (donut-reverse-direction donut)
          (loop-finish))
        
        ;; "[" in different level?
        ;; => Continue search.
        ((and (char= current-token #\[)
              (not (zerop level)))
          (decf level)
          (interpreter-advance interpreter))
        
        ;; "]" encountered?
        ;; => Increase level and continue search.
        ((char= current-token #\])
          (incf level)
          (interpreter-advance interpreter))
        
        ;; Any other character?
        ;; => Continue search.
        (T
          (interpreter-advance interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-switch-io-mode (interpreter)
  "Switches the INTERPRETER's input/output mode and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'io-mode)
    (get-opposite-io-mode
      (slot-value interpreter 'io-mode)))
  (values))

;;; -------------------------------------------------------

(defun process-grid (interpreter)
  "Processes the Moving donut program comprehended in the INTERPRETER's
   code grid and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (grid donut current-token io-mode memory) interpreter
    (declare (type Grid                grid))
    (declare (type (or null Donut)     donut))
    (declare (type (or null character) current-token))
    (declare (ignorable                current-token))
    (declare (type Memory              memory))
    (declare (ignorable                memory))
    (declare (type io-mode             io-mode))
    (declare (ignorable                io-mode))
    
    (interpreter-advance interpreter)
    
    (loop until (donut-finished-p donut) do
      (cond
        ;; Ignore the donut's start position.
        ((char= current-token #\O)
          NIL)
        
        ;; Lowercase letters are printed verbatim.
        ((minuscle-p current-token)
          (format T "~c" current-token))
        
        ;; Move the cell pointer right.
        ((char= current-token #\>)
          (memory-move-right memory))
        
        ;; Move the cell pointer left.
        ((char= current-token #\<)
          (memory-move-left memory))
        
        ;; Increment the current cell value.
        ((char= current-token #\+)
          (memory-increment memory))
        
        ;; Decrement the current cell value.
        ((char= current-token #\-)
          (memory-increment memory))
        
        ;; Store the user input in the current cell.
        ((char= current-token #\,)
          (interpreter-query-for-input interpreter))
        
        ;; Print the current cell value.
        ((char= current-token #\.)
          (interpreter-issue-output interpreter))
        
        ;; If the current cell value equals zero (0), jump forward to
        ;; the matching "]".
        ((char= current-token #\[)
          (when (zerop (memory-current-cell memory))
            (interpreter-jump-forward interpreter)))
        
        ;; If the current cell value does not equal zero (0), jump back
        ;; to the matching "[".
        ((char= current-token #\])
          (unless (zerop (memory-current-cell memory))
            (interpreter-jump-back interpreter)))
        
        ;; Switch the input/output mode.
        ((char= current-token #\A)
          (interpreter-switch-io-mode interpreter))
        
        ;; Ignore any other character.
        (T
          NIL))
      
      (interpreter-advance interpreter)))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Moving-donut (code)
  "Interprets the piece of Moving donut source CODE and returns no
   value."
  (declare (type string code))
  (process-grid
    (make-interpreter
      (build-grid code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character matrix operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-character-matrix (&rest lines)
  "Concatenates the LINES by ligating each two by a single linebreak
   into a two-dimensional shape and returns the resulting string."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "helloworld".
(interpret-Moving-donut
"  ######
  #dlro#
######w#
#Ohello#
########")

;;; -------------------------------------------------------

;; Infinite cat program which terminates on a user input equal to the
;; null character.
(interpret-Moving-donut
"########
#O+[,.]#
########")

;;; -------------------------------------------------------

;; Infinite cat program which terminates on a user input equal to the
;; null character.
(interpret-Moving-donut
  (build-character-matrix
    "########"
    "#O+[,.]#"
    "########"))

;;; -------------------------------------------------------

;; Infinitely repeating cat program which employs the topology for
;; representing an perpetual loop.
(interpret-Moving-donut
  (build-character-matrix
    "#####"
    "#O,.#"
    " #  #"
    " #  #"
    " ####"))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Moving-donut
  (build-character-matrix
    "####"
    "#.]#" 
    "##.#"
    " #[#"
    " #,#"
    "#OA#"
    "####"))

;;; -------------------------------------------------------

;; Exploit redirections to print the message
;; "movingdonutisaveryinterestinglanguage"
(interpret-Moving-donut
"#############
Omovingdonut#
#    ######i#
#    #yrevas#
#     i######
#     n#
#     t#
#     e#
#     r#
#     e#
#     s#
#     t#
#     i#
##    n#
#ugnalg#
#a######
#g#
#e#
###")
