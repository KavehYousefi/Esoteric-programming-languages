;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Snake Shit", invented by the Esolang user "Totobird
;; Creations" and presented on March 4th, 2022, the episemon received
;; in its dation appertains to its simulation of a snake's perambulation
;; inside of a two-dimensional grid, the creature itself in the
;; instruction pointer's agency, with a concomitant incorporation of the
;; data aspect realized by the snake's conceived non-negative length.
;; 
;; 
;; Concept
;; =======
;; The Snake Shit programming language ostends its program's in the a
;; two-dimensional Cartesian guise, the cells' occupants being any
;; characters to one's avail, with a particular subset desumed therefrom
;; with an operative competence, occasionally relying upon an argument
;; that always wones to the respective dextral laterality, while, as an
;; interesting concomitant, the snake's sideness acts as a vehicle for
;; the data management by conveying in its measurement the sole datum
;; engaged with arithmetics, logical operations, and control flow
;; mechanisms.
;; 
;; == THE PROGRAM: A TWO-DIMENSIONAL GRID ==
;; The program layout conforms to a two-dimensional grid, homologating
;; in its rows' account a divergent mete, such that some lines may be
;; curtailed, while other extend more generously.
;; 
;; A parallel aspect resident in this arrangement of cells relates to
;; the liberality of any symbol's participation, while only members
;; extracted from the valid instruction set may accompass causata. A
;; rede not peisant as a behest intimates the usance of the underscore
;; character, "_", for the snake's possible trail's emphasis.
;; 
;; == PROGRAM TERMINATION: THE SNAKE'S VALEDICTION ==
;; The program termination constitutes a variable upon the snake's
;; motion and is instigated by the creature's attempt to move unto an
;; area not occupied by a character.
;; 
;; == THE SNAKE: INSTRUCTIONS + DATA ==
;; A dioristic proprium partaking in this bailiwick appertains to the
;; data castaldy's assignment to the conceived snake itself, whose
;; length, furnishing one among the components' twain --- besides and in
;; champarty with the variable set --- realizes a non-negative integral
;; account for a usance during the program.
;; 
;; A motile medium in this context, its latreutical vallidom amplects in
;; particular the vinculum betwixt the operational, especially those
;; arithmetic in their nature, warklumes and their consignment to and
;; inquisition from the variables.
;; 
;; Amplified lucidity applied to this diction, the snake's length is
;; subjected to modifications and perquisitions by the instructions'
;; effort, and stored in variables for a future retrieval.
;; 
;; As a consectary, a twifaced aspect wones in the snake as a multum in
;; parvo protagonist: its position and airt being the instruction
;; pointer's, and ultimate, the instructive bailiwick's account, whereas
;; its length establishes the data management.
;; 
;; == VARIABLES: SNAKE LENGTH STORAGES ==
;; A warklume acquired as a supererogation to the snake's length as the
;; active token of deliberation, a set of arbitrary length capaciates
;; the definition of variables, each such proceeding from an aefauld
;; character's denomination, and entalented with the capacity for an
;; unsigned integer datum, which is desumed from the snake's sideness.
;; 
;; 
;; Architecture
;; ============
;; The architectural vista upon Snake Shift bewrays a twissel
;; componency, imprimis in the conceived snake itself --- more
;; concretely, its length, and, secondly, in the variables deployed for
;; the telos of the same's preterite evolutions, both specifications
;; applying to non-negative numbers.
;; 
;; 
;; Data Types
;; ==========
;; Snake Shit's type system wists of two species' participation: the
;; preeminent non-negative integer numbers and the subordinate
;; character type, the former's attendance to the snake length and the
;; appertaining perquisition and modulation efforts serves to vindicate
;; the significance's superiority in a mete commensurate to the latter's
;; restriction to output purposes that discounts a more favorable
;; docimasy.
;; 
;; == NON-NEGATIVE INTEGERS: THE PARAVAUNT DATA OBJECTS ==
;; Snake Shit's paravaunt utility in data aspects resides in the
;; reference to non-negative integer numbers, both produced in the
;; vinculum to the eponymous snake's dispansion and, an epiphenomenon
;; thereof, specifications airted at its modulation.
;; 
;; == CHARACTERS: TOKENS OF OUTPUT COMMUNICATION ==
;; A parhedral instrument, characters supply the tokens of the
;; communicative intercourse in respect to literal output issuances.
;; 
;; 
;; Syntax
;; ======
;; A Snake Shit program's syntactical conformation follows the
;; delineations of a two-dimensional Cartesian grid of characters.
;; 
;; Maugre any content admission's averment, merely instructional
;; constituents and arguments produce a meaning causatum, the orra
;; participants being apportioned merely a commentary value.
;; 
;; == INSTRUCTIONS ==
;; Instructions are graithed by a particular symbol ligated into
;; affiliation withal, and zero or one argument, the presence of which,
;; if homologation applies to thereto, always empights this additional
;; datum to the immediate dextral adjacency of the operation name.
;; 
;; == WHITESPACES ==
;; Spaces subsume into the same species as any non-operative content,
;; a commentary purpose's establishment.
;; 
;; Newlines, on the other, appropriate a far more stringent regulation
;; in their demarcation of lines. Each such bourne's transcendence
;; segues into a new row for the program grid.
;; 
;; == COMMENTS ==
;; Any content not associated with a Snake Shit instruction, such one's
;; argument, if in the circumference of the former's actuation, or a
;; newline entity is deprived of a causatum, and thus allied with a
;; descant's agency.
;; 
;; == UNDERSCORE CHARACTERS MAY HELP TRACE THE SNAKE'S PATH ==
;; Disencumbered from a stipulation's status, a rede by the language
;; standard references the tracing of the snake's possible path in the
;; grid by underscores ("_") as a behovable instrument for the source
;; code's apprehension.
;; 
;; 
;; Instructions
;; ============
;; Fifteen operations' amplectation exhausts Snake Shit's operational
;; bailiwick, the circumference of which touches the subjects of
;; navigation across the grid, the snake length's manipulation, partly
;; in champarty with variables, input and output, as well as a
;; conditional execution facility.
;; 
;; == ARGUMENTS ARE COLLOCATED AT A COMMAND'S DEXTRAL LATERALITY ==
;; A subset of commands depends upon a, usually mandatory, argument,
;; either of a non-negative integer, character, or instruction form,
;; the presence of which always is empighted in immediate adjacency to
;; the desiderating operation identifier.
;; 
;; == UNRECOGNIZED SYMBOLS CONSTITUTE NO-OPERATIONS ==
;; Symbols not engaged in an affiliation with one of the instruction
;; identifiers impose ineffectual content, that is, are construed as a
;; no-operation (NOP).
;; 
;; == INSTRUCTION CATEGORIES ==
;; Snake Shit's operative competences span athwart the topics of control
;; flow helming, basic arithmetics, variable management, as well as
;; input and output communications.
;; 
;; The following tabular exposition shall impart an apercu concerning
;; the facilities and offering a concomitant subsumption in accordance
;; with their specific bailiwicks.
;; 
;; Please heed that sections in the circumambiency of braces, "{" and
;; "}", are intended for replacement by actual Snake Shit code in the
;; ultimate program.
;; 
;;   ==================================================================
;;   CONTROL FLOW HELMING
;;   ------------------------------------------------------------------
;;   ${dir}    | Set snake start point to this and direction to {dir}.
;;   ..................................................................
;;   <         | Set snake direction to left.
;;   ..................................................................
;;   >         | Set snake direction to right.
;;   ..................................................................
;;   ^         | Set snake direction to up.
;;   ..................................................................
;;   v         | Set snake direction to down.
;;   ..................................................................
;;   ~{cmd}    | Execute command {cmd} if snake length exceeds zero.
;;   ==================================================================
;;   ARITHMETIC OPERATIONS
;;   ------------------------------------------------------------------
;;   +{amount} | Increment snake length by the {amount}.
;;   ..................................................................
;;   -{amount} | Decrement snake length by the {amount}.
;;   ..................................................................
;;   ={length} | Set snake length to {length}.
;;   ..................................................................
;;   ?{max}    | Set snake length to random integer in [0, max).
;;   ==================================================================
;;   VARIABLE CASTALDY
;;   ------------------------------------------------------------------
;;   @{name}   | Store snake length in variable of {name}.
;;   ..................................................................
;;   %{name}   | Set snake length to value of variable of {name}.
;;   ==================================================================
;;   INPUT AND OUTPUT FACILITIES
;;   ------------------------------------------------------------------
;;   &         | Set snake length to integral user input.
;;   ..................................................................
;;   #{char}   | Print character {char}.
;;   ..................................................................
;;   *         | Print snake length.
;;   ==================================================================
;; 
;; == OVERVIEW ==
;; An apercu shall be the medium for the subsequent basic nortelry's
;; dation concerning the language's instructions.
;; 
;; Please heed that succedaneous segments are ensconced in a jumelle of
;; braces, "{" and "}", the interior of which designates the placeholder
;; signification. Such fragments, including in the diorism the braces
;; themselves, ought to be substituted by actual Snake Shit code in the
;; program.
;; 
;;   ------------------------------------------------------------------
;;   Command   | Effect
;;   ----------+-------------------------------------------------------
;;   ${dir}    | Sets the instruction pointer's (IP) initial direction
;;             | to {dir} and moves, proceeding from the location of
;;             | the "$" symbol, one step into this airt. If a dextral
;;             | motion, signified by the ">" token, applies, this
;;             | argument, as is the stipulation with all commands, is
;;             | skipped during the perambulation procedure.
;;             |-------------------------------------------------------
;;             | If this command's initiating symbol, "$", is
;;             | encountered a second time during a program's
;;             | execution, its presence is ignored, in conjunction
;;             | its argument, akin to the transpiration of any
;;             | no-operation character. If, however, the accompanying
;;             | direction symbol concurs without this command's
;;             | involvement, the respective redirection instructions
;;             | are accompassed, for which please consult the table
;;             | entries "<", ">", "^", and "v" alow.
;;             |-------------------------------------------------------
;;             | {dir} must be one of the four cardinal direction
;;             | specifiers:
;;             |   -------------------------------
;;             |   {dir} value | Initial direction
;;             |   ------------+------------------
;;             |   <           | left
;;             |   ................................
;;             |   >           | right
;;             |   ................................
;;             |   ^           | up
;;             |   ................................
;;             |   v           | down
;;             |   -------------------------------
;;             |-------------------------------------------------------
;;             | If {dir} does not constitute a valid direction symbol,
;;             | an error of the type "InvalidArgumentError" is
;;             | signaled.
;;             |-------------------------------------------------------
;;             | If no "$" command can be detected in the code, an
;;             | error of the type "MissingStartPointError" is
;;             | signaled.
;;             |-------------------------------------------------------
;;             | If more than one "$" command exists in the code, an
;;             | error of the type "AmbiguousStartPointsError" is
;;             | signaled.
;;   ..................................................................
;;   <         | Changes the instruction pointer's (IP) direction to a
;;             | sinistral motion.
;;   ..................................................................
;;   >         | Changes the instruction pointer's (IP) direction to a
;;             | dextral motion.
;;   ..................................................................
;;   ^         | Changes the instruction pointer's (IP) direction to an
;;             | upward motion.
;;   ..................................................................
;;   v         | Changes the instruction pointer's (IP) direction to a
;;             | downward motion.
;;   ..................................................................
;;   +{amount} | Increments the snake's length by the {amount}.
;;             |-------------------------------------------------------
;;             | If {amount} does not constitute a non-negative
;;             | integer number, an error of the type
;;             | "InvalidArgumentError" is signaled.
;;   ..................................................................
;;   -{amount} | Decrements the snake's length by the {amount}.
;;             |-------------------------------------------------------
;;             | If {amount} does not constitute a non-negative
;;             | integer number, an error of the type
;;             | "InvalidArgumentError" is signaled.
;;   ..................................................................
;;   ={length} | Sets the snake's length to {length}.
;;             |-------------------------------------------------------
;;             | If {length} does not constitute a non-negative
;;             | integer number, an error of the type
;;             | "InvalidArgumentError" is signaled.
;;   ..................................................................
;;   ?{max}    | If {max} is greater than zero (0), sets the snake's
;;             | length to a random integer number betwixt inclusive
;;             | zero (0) and exclusive {max}, that is, the interval
;;             | spanned by [0, {max}); otherwise, if {max} equals
;;             | zero (0), sets the snake length to zero (0).
;;             |-------------------------------------------------------
;;             | If {max} does not constitute a non-negative integer
;;             | number, an error of the type "InvalidArgumentError" is
;;             | signaled.
;;   ..................................................................
;;   @{name}   | Stores the snake's length in the variable designated
;;             | by the {name}. If no variable with siccan name exists,
;;             | a new one is declared ere the transfer; otherwise the
;;             | already extant content is superseded by the new value.
;;             |-------------------------------------------------------
;;             | {name} must be a single character or an escaped
;;             | entity. The following escape code are recognized:
;;             |   ----------------------------
;;             |   Escape code | Effect
;;             |   ------------+---------------
;;             |   \n          | Newline
;;             |   ............................
;;             |   \t          | Horizontal tab
;;             |   ............................
;;             |   \\          | Backslash
;;             |   ----------------------------
;;             |-------------------------------------------------------
;;             | If {name} does not constitute a character, an error of
;;             | the type "InvalidArgumentError" is signaled.
;;   ..................................................................
;;   %{name}   | Sets the snake's length to the value of the variable
;;             | designated by the {name}.
;;             |-------------------------------------------------------
;;             | {name} must be a single character or an escaped
;;             | entity. The following escape code are recognized:
;;             |   ----------------------------
;;             |   Escape code | Effect
;;             |   ------------+---------------
;;             |   \n          | Newline
;;             |   ............................
;;             |   \t          | Horizontal tab
;;             |   ............................
;;             |   \\          | Backslash
;;             |   ----------------------------
;;             |-------------------------------------------------------
;;             | If {name} does not constitute a character, an error of
;;             | the type "InvalidArgumentError" is signaled.
;;             |-------------------------------------------------------
;;             | If the {name} does not designate an extant variable,
;;             | an error of the type "NoSuchVariableError" is
;;             | signaled.
;;   ..................................................................
;;   &         | Queries the standard input for a non-negative integer
;;             | number and sets the snake's length to the same.
;;             |-------------------------------------------------------
;;             | If the input does not resolve to a non-negative
;;             | integer number, an error of the type
;;             | "InvalidInputError" is signaled.
;;   ..................................................................
;;   #{char}   | Prints the character {char} to the standard output,
;;             | destitute of any decoration, such as spaces or
;;             | linebreaks.
;;             |-------------------------------------------------------
;;             | {char} must be a single character or an escaped
;;             | entity. The following escape code are recognized:
;;             |   ----------------------------
;;             |   Escape code | Effect
;;             |   ------------+---------------
;;             |   \n          | Newline
;;             |   ............................
;;             |   \t          | Horizontal tab
;;             |   ............................
;;             |   \\          | Backslash
;;             |   ----------------------------
;;             |-------------------------------------------------------
;;             | If {char} does not constitute a character, an error of
;;             | the type "InvalidArgumentError" is signaled.
;;   ..................................................................
;;   *         | Prints the snake's length as to the standard output,
;;             | destitute of any decoration, such as spaces or
;;             | linebreaks.
;;   ..................................................................
;;   ~{cmd}    | If the snake length does not equal zero (0), executes
;;             | the command {cmd}; otherwise proceeds as usual.
;;             |-------------------------------------------------------
;;             | {cmd} may either be a command or any content, in any
;;             | case the respective regulations hold.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its investment with elucidations and forbisens, the Snake Shit
;; protolog's infliction with certain adumbrations begets ambiguous
;; aspects, which at this location shall be attended to in a selected
;; grade.
;; 
;; == HOW ARE ANOMALOUS START POINTS HANDLED? ==
;; The dollar sign "$" establishes the snake's incipial position in the
;; grid; a statement anenst the contingencies for absentees and
;; multitudes, however, eludes the protolog's dation.
;; 
;; It has been adjudged that the aefauld circumstance of tolerable
;; admission constitutes a singular start point; this specification's
;; lacuna shall signal an error of the type "MissingStartPointError",
;; whereas a nimiety beyond the sole exemplary instigates an
;; "AmbiguousStartPointsError".
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp, adhibiting to Snake Shit's cynosure, the program grid, a
;; special dedication by its representation as a vector of lines, such
;; holds the potency for a two-dimensional navigation without an
;; eloignment of each row's concrete spatial dispansion from the
;; interpretation process' contemplations.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-22
;; 
;; Sources:
;;   [esolang2022SnakeShit]
;;   The Esolang contributors, "Snake Shit", March 5th, 2022
;;   URL: "https://esolangs.org/wiki/Snake_Shit"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts along which
   a motion is capaciated in a Snake Shit program."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of its diorism lays its amplectation around siccan operations
   as ``format'' and ``write-char'', without the contingency's
   exhaustion."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list the componency of which is
   delineated by zero or more elements of the ELEMENT-TYPE, defaulting
   to the generic sentinel ``*''."
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

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integral object whose
   minimum cannot descend below zero (0), but whose range is not
   governed by an upper bourne, thus accommodating the closed discrete
   interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic sentinel ``*''."
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

(deftype variable-registry ()
  "The ``variable-registry'' type defines a mapping of variable names to
   snake length measures, realized as a hash table the keys of which
   comprehend ``character'' instances, while the associated values
   assume the form of ``non-negative-integer'' numbers."
  '(hash-table-of character non-negative-integer))

;;; -------------------------------------------------------

(deftype file-source ()
  "The ``file-source'' type defines a source for file contents'
   obtention."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value (object)
  "Returns a Boolean truth value tantamount to the OBJECT, which
   resolves to ``T'' for a non-``NIL'' input, otherwise assuming
   ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-symbol-character-p (candidate)
  "Determines whether the CANDIDATE represents the start symbol \"$\",
   on confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (char= candidate #\$))))

;;; -------------------------------------------------------

(defun direction-character-p (candidate)
  "Determines whether the CANDIDATE represents a direction signifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (find candidate "<>^v" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-direction (token)
  "Parses the TOKEN and returns the represented ``direction'' object."
  (declare (type character token))
  (the direction
    (case token
      (#\<       :left)
      (#\>       :right)
      (#\^       :up)
      (#\v       :down)
      (otherwise (error "Invalid direction token: ~s." token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of location.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class furnishes the diorism of a two-dimensional
   point in a Cartesian system, compact of an x- and a y-coordinate,
   both participants are endowed with an integral resolution."
  (x 0 :type fixnum :read-only NIL)
  (y 0 :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-location ((location x-variable y-variable) &body body)
  "Evaluates the LOCATION, establishes local symbol macros which bind
   its x-coordinate to the X-VARIABLE and its y-component to the
   Y-VARIABLE, executes the BODY forms, and returns the desinent form's
   results."
  (let ((evaluated-location (gensym)))
    (declare (type symbol evaluated-location))
    `(let ((,evaluated-location ,location))
       (declare (type Location ,evaluated-location))
       (symbol-macrolet
           ((,x-variable (the fixnum (location-x ,evaluated-location)))
            (,y-variable (the fixnum (location-y ,evaluated-location))))
         (declare (type fixnum ,x-variable))
         (declare (ignorable   ,x-variable))
         (declare (type fixnum ,y-variable))
         (declare (ignorable   ,y-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun translate-location (location direction)
  "Translates the LOCATION one step into the DIRECTION and returns the
   modified LOCATION."
  (declare (type Location  location))
  (declare (type direction direction))
  (with-location (location x y)
    (case direction
      (:left     (decf x))
      (:right    (incf x))
      (:up       (decf y))
      (:down     (incf y))
      (otherwise (error "Invalid direction: ~s." direction))))
  (the Location location))

;;; -------------------------------------------------------

(defun set-location-to (target source)
  "Translates the TARGET location to the SOURCE's coordinates and
   returns the modified TARGET."
  (declare (type Location target))
  (declare (type Location source))
  (with-location (target target-x target-y)
    (with-location (source source-x source-y)
      (setf target-x source-x)
      (setf target-y source-y)))
  (the Location target))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character grid.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Grid
  "The ``Grid'' class furnishes a model for the representation of a
   Cartesian grid of characters, its mold capacitates the incorporation
   of \"ragged\" edges, that is, lines of concurrent commencement but
   disparate extent."
  (lines        (error "Missing lines.")
                :type      (simple-array string (*))
                :read-only T)
  (width        0
                :type      fixnum
                :read-only T)
  (height       0
                :type      fixnum
                :read-only T)
  (start-points NIL
                :type      (list-of Location)
                :read-only NIL))

;;; -------------------------------------------------------

(defun detect-start-points (grid)
  "Detects the start points in the GRID, designated by the dollar symbol
   \"$\", stores their locations in the same, and returns the modified
   GRID."
  (declare (type Grid grid))
  (setf (grid-start-points grid)
    (loop
      for line of-type string across (grid-lines grid)
      and y    of-type fixnum from   0 by 1
      append
        (loop
          for     x of-type fixnum from 0 below (length line)
          when    (start-symbol-character-p (char line x))
          collect (make-location x y))))
  (the Grid grid))

;;; -------------------------------------------------------

(defun build-grid (code)
  "Generates and returns for the piece of Snake Shit source CODE a
   character grid, the dever of which resides in its reformulation of
   the one-dimensional input in an approximate Cartesian forbisen."
  (declare (type string code))
  (let ((width  0)
        (height 0))
    (declare (type fixnum width))
    (declare (type fixnum height))
    (with-input-from-string (input code)
      (declare (type string-stream input))
      (loop
        for line of-type (or null string) = (read-line input NIL)
        while line
        collect
          (prog1 line
            (setf width (max width (length line)))
            (incf height))
          into lines
        finally
          (return
            (detect-start-points
              (make-grid
                :lines  (coerce lines '(simple-array string (*)))
                :width  width
                :height height)))))))

;;; -------------------------------------------------------

(defun grid-contains-point-p (grid point)
  "Determines whether the POINT constitutes an admissible location into
   the GRID, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location point))
  (the boolean
    (get-boolean-value
      (with-location (point x y)
        (and
          (<= 0 x (1- (grid-width  grid)))
          (<= 0 y (1- (grid-height grid))))))))

;;; -------------------------------------------------------

(defun get-grid-line (grid y)
  "Returns the Y-th line stored in the character GRID."
  (declare (type Grid   grid))
  (declare (type fixnum y))
  (the string
    (aref (grid-lines grid) y)))

;;; -------------------------------------------------------

(defun get-grid-line-length (grid y)
  "Returns the number of characters in character GRID's Y-th line."
  (declare (type Grid   grid))
  (declare (type fixnum y))
  (the fixnum
    (length
      (get-grid-line grid y))))

;;; -------------------------------------------------------

(defun blank-grid-location-p (grid location)
  "Determines whether the LOCATION designates a not explicitly
   specified, yet admissive point in the GRID, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the boolean
    (and
      (grid-contains-point-p grid location)
      (with-location (location x y)
        (let ((line-at-y (get-grid-line grid y)))
          (declare (type string line-at-y))
          (not (array-in-bounds-p line-at-y x)))))))

;;; -------------------------------------------------------

(defun valid-grid-location-p (grid location)
  "Determines whether the LOCATION designates a position into the GRID,
   that is, its coordinates respect the latter's rectangular spatial
   expansion and obviate any contingent gaps left blank, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the boolean
    (and
      (grid-contains-point-p grid location)
      (with-location (location x y)
        (let ((line-at-y (get-grid-line grid y)))
          (declare (type string line-at-y))
          (array-in-bounds-p line-at-y x))))))

;;; -------------------------------------------------------

(defun get-grid-entry (grid location)
  "Returns the character stored in the GRID at the specified LOCATION."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the (or null character)
    (cond
      ((not (grid-contains-point-p grid location))
        NIL)
      ((blank-grid-location-p grid location)
        NIL)
      (T
        (with-location (location x y)
          (char (get-grid-line grid y) x))))))

;;; -------------------------------------------------------

(defun grid-character-satisfies-p (grid location predicate)
  "Determines whether the LOCATION designates a valid point in the
   GRID and, if ascertained in this pursuit, the character at the
   specified position satisfies the PREDICATE, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid                     grid))
  (declare (type Location                 location))
  (declare (type (function (character) *) predicate))
  (the boolean
    (get-boolean-value
      (and (valid-grid-location-p grid location)
           (funcall predicate
             (get-grid-entry grid location))))))

;;; -------------------------------------------------------

(defun grid-character-equals-p (grid location expected-character)
  "Determines whether the LOCATION designates a valid point in the
   GRID and, if ascertained in this pursuit, the character at the
   specified position equals the EXPECTED-CHARACTER, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid      grid))
  (declare (type Location  location))
  (declare (type character expected-character))
  (the boolean
    (get-boolean-value
      (and (valid-grid-location-p grid location)
           (char= (get-grid-entry grid location)
                  expected-character)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of pointer.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pointer
  (:copier NIL))
  "The ``Pointer'' class implements a mobile cursor covenable for the
   perambulation of a two-dimensional Cartesian grid by its champarty of
   a location specifier and a direction, both pieces of information
   entalented with the capacity for modulations."
  (location  (make-location 0 0)
             :type      Location
             :read-only NIL)
  (direction :right
             :type      direction
             :read-only NIL))

;;; -------------------------------------------------------

(defun copy-pointer (template)
  "Creates and returns a new ``Pointer'' as a copy of the TEMPLATE.
   ---
   Please note that the TEMPLATE location will be copied into the newly
   produced pointer, in lieu of a simple reference shaing, thus
   ascertaining a perfection in the two entities' independence."
  (declare (type Pointer template))
  (the Pointer
    (make-pointer
      :location  (copy-location (pointer-location template))
      :direction (pointer-direction template))))

;;; -------------------------------------------------------

(defun set-pointer-to (destination source)
  "Copies the SOURCE pointer's location and direction into the
   DESTINATION and returns the modified latter.
   ---
   Please note that the SOURCE location will be copied into the
   DESTINATION, in lieu of a simple reference sharing, thus ascertaining
   a perfection in the two entities' independence."
  (declare (type Pointer destination))
  (declare (type Pointer source))
  (set-location-to
    (pointer-location destination)
    (pointer-location source))
  (setf (pointer-direction destination)
    (pointer-direction source))
  (the Pointer destination))

;;; -------------------------------------------------------

(defun move-pointer (pointer)
  "Moves the POINTER one step into its currently assigned direction and
   returns the modified POINTER."
  (declare (type Pointer pointer))
  (translate-location
    (pointer-location  pointer)
    (pointer-direction pointer))
  (the Pointer pointer))

;;; -------------------------------------------------------

(defun move-pointer-to (pointer new-location)
  "Relocates the POINTER to the NEW-LOCATION and returns the modified
   POINTER."
  (declare (type Pointer  pointer))
  (declare (type Location new-location))
  (set-location-to (pointer-location pointer) new-location)
  (the Pointer pointer))

;;; -------------------------------------------------------

(defun get-next-pointer-location (pointer)
  "Projects the position which the POINTER would proceed to if mandated
   to move, and returns a new ``Location'' representation thereof,
   without actually accompassing its motion."
  (declare (type Pointer pointer))
  (the Location
    (translate-location
      (copy-location
        (pointer-location pointer))
      (pointer-direction pointer))))

;;; -------------------------------------------------------

(defun valid-pointer-location-p (grid pointer)
  "Determines whether the POINTER resides at a valid location inside of
   the GRID, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Grid    grid))
  (declare (type Pointer pointer))
  (the boolean
    (valid-grid-location-p grid
      (pointer-location pointer))))

;;; -------------------------------------------------------

(defun get-selected-character (grid pointer)
  "Returns the character located at the POINTER's position into the
   GRID."
  (declare (type Grid    grid))
  (declare (type Pointer pointer))
  (the (or null character)
    (get-grid-entry grid
      (pointer-location pointer))))

;;; -------------------------------------------------------

(defun selected-character-equals-p (grid pointer expected-character)
  "Determines whether the LOCATION designates a valid point in the
   GRID and, if ascertained in this pursuit, the character at the
   specified position equals the EXPECTED-CHARACTER, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid      grid))
  (declare (type Pointer   pointer))
  (declare (type character expected-character))
  (the boolean
    (grid-character-equals-p grid
      (pointer-location pointer)
      expected-character)))

;;; -------------------------------------------------------

(defun selected-character-satisfies-p (grid pointer predicate)
  "Determines whether the POINTER designates a valid point in the GRID
   and, if ascertained in this pursuit, the character at the specified
   position satisfies the PREDICATE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid                     grid))
  (declare (type Pointer                  pointer))
  (declare (type (function (character) *) predicate))
  (the boolean
    (grid-character-satisfies-p grid
      (pointer-location pointer)
      predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command ()
  ((end-point
    :initarg       :end-point
    :initform      (error "Missing command end point.")
    :reader        get-command-end-point
    :type          Location
    :documentation "Specifies the position into the ensconcing grid
                    immediately succeeding the command and its
                    contingent argument's expanse."))
  (:documentation
    "The abstract ``Command'' class furnishes a common foundry for all
     concrete class representation of Snake Shit operations, its
     perimeter embracing the unifying pieces of information pertinent to
     every member of this species, namely, the end point of its extent
     and the contingency for an argument obeying to any type."))

;;; -------------------------------------------------------

(defgeneric get-command-argument (command)
  (:documentation
    "Returns the COMMAND's argument, or, if none such is specified for
     its species, signals an error of an unspecified type.")
  
  (:method ((command Command))
    (error "Commands of the class ~s do not accept arguments."
      (class-of command))))

;;; -------------------------------------------------------

(defmacro define-command (command-name
                          (&optional argument-type
                                     (argument-documentation ""))
                          &optional (command-documentation ""))
  "Defines a class which inherits from the ``Command'' class, endowed,
   if the ARGUMENT-TYPE is specified, with a slot nevened ``argument''
   and subsumed into the ARGUMENT-TYPE, with a reader function
   ``get-command-argument'', and associated with the documentation
   string ARGUMENT-DOCUMENTATION, the class itself documented via the
   optional COMMAND-DOCUMENTATION."
  `(defclass ,command-name (Command)
     ,(when argument-type
        `((argument
           :initarg       :argument
           :reader        get-command-argument
           :type          ,argument-type
           :documentation ,argument-documentation)))
     (:documentation ,command-documentation)))

;;; -------------------------------------------------------

(define-command Start-Command
  (direction "The instruction pointer (IP) direction at the program's
              inchoation.")
  "The ``Start-Command'' class encapsulates the start point into the
   Snake Shit program grid, delegated to the \"$\" symbol's occasion,
   with an imperative direction specifier as its dextral compernage.")

;;; -------------------------------------------------------

(define-command Decrement-Command
  (non-negative-integer "The non-negative amount by which to decrement
                         the snake's length.")
  "The ``Decrement-Command'' class encapsulates a behest directed at the
   snake length's curtailment.")

;;; -------------------------------------------------------

(define-command Direction-Command
  (direction "The new instruction pointer (IP) direction to assume.")
  "The ``Direction-Command'' class encapsulates a behest for the
   instruction pointer (IP) airt's redirection, realized by the
   quadruple contingencies of the symbols \"<\", \">\", \"v\", and
   \"^\".")

;;; -------------------------------------------------------

(define-command EOF-Command
  ()
  "The ``EOF-Command'' class applies itself to the apprizal about the
   program's completion by a transgression of the demarcated Snake Shit
   area's bournes.")

;;; -------------------------------------------------------

(define-command Increment-Command
  (non-negative-integer "The non-negative amount by which to increment
                         the snake's length.")
  "The ``Increment-Command'' class encapsulates a behest directed at the
   snake length's augmentation.")

;;; -------------------------------------------------------

(define-command Input-Command
  ()
  "The ``Input-Command'' class encapsulates the behest involved in a
   query for a number.")

;;; -------------------------------------------------------

(define-command Load-Length-Command
  (character "The name of the variable to load the snake length from.")
  "The ``Load-Length-Command'' class encapsulates the behest directed at
   loading the snake length from a variable designated by its name.")

;;; -------------------------------------------------------

(define-command NOP-Command
  ()
  "The ``NOP-Command'' class encapsulates the notion of a no-operation,
   or NOP, sentinel, the dever of which wones in its succedaneous
   installation in such circumstances where a non-effective character
   occurs.")

;;; -------------------------------------------------------

(define-command Print-Character-Command
  (character "The literal character to output.")
  "The ``Print-Character-Command'' class encapsulates the request for a
   character's issuance unto the standard output, the introduction of
   which proceeds from the \"#\" sign, accompanied by the literal
   character to its right.")

;;; -------------------------------------------------------

(define-command Print-Length-Command
  ()
  "The ``Print-Length-Command'' class encapsulates the request for the
   snake length's issuance unto the standard output, the introduction of
   which proceeds from the \"*\" sign.")

;;; -------------------------------------------------------

(define-command Random-Length-Command
  (non-negative-integer "The exclusive upper bourne of the random
                         range for the snake length.")
  "The ``Random-Length-Command'' class encapsulates the request for the
   snake length's aleatory assignment of a new length, the admissible
   range being delineated by [0, n), where n signifies the integral
   instruction argument.")

;;; -------------------------------------------------------

(define-command Save-Length-Command
  (character "The name of the variable to save the snake length in.")
  "The ``Save-Length-Command'' class encapsulates the behest directed at
   storage of the snake length in a variable designated by its name.")

;;; -------------------------------------------------------

(define-command Skip-Command
  ;(Command "The command to skip if the snake's length equal zero (0).")
  (Location "This skip command's end point for futur referral.")
  "The ``Skip-Command'' class encapsulates a behest for an accolent
   instruction's conditional execution or elision.")

;;; -------------------------------------------------------

(define-command Set-Length-Command
  (non-negative-integer "The non-negative length to assume by the
                         snake.")
  "The ``Set-Command'' class encapsulates a behest directed at the snake
   length's supersession by a new amount.")

;;; -------------------------------------------------------

(defmethod print-object ((command Command) (destination T))
  (declare (type Command     command))
  (declare (type destination destination))
  (format destination "(~a argument=~s end-point=~a)"
    (type-of               command)
    (get-command-argument  command)
    (get-command-end-point command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((grid
    :initarg       :grid
    :initform      (error "Missing grid for interpreter.")
    :type          Grid
    :documentation "The Snake Shit program formatted as a grid.")
   (ip
    :initform      (make-pointer :location (make-location 0 0))
    :type          Pointer
    :documentation "The current instruction pointer (IP) location inside
                    of the GRID.")
   (argument-pointer
    :initform      (make-pointer :location (make-location 0 0))
    :type          Pointer
    :documentation "A pointer dedicated to the task of consuming the
                    argument, always located dextrally to the
                    instruction pointer (IP) in the GRID.
                    ---
                    Upon reading a command, a duty allotted to the
                    instruction pointer (IP), its dependence upon an
                    argument is assessed; and, if ascertained, the
                    ARGUMENT-POINTER assumes the cell immediately to the
                    command token's right, whence the operand's
                    consumption proceeds by this pointer's
                    adminiculum.")
   (program-started-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the Snake Shit program has
                    already processed its incipial start point
                    command.")
   (snake-length
    :initform      0
    :type          non-negative-integer
    :documentation "The current snake length.")
   (variables
    :initform      (make-hash-table :test #'eql)
    :type          variable-registry
    :documentation "Maps the one-letter variable identifiers to stored
                    non-negative snake lengths."))
  (:documentation
    "The ``Interpreter'' class assumes the dever of accompassing actual
     feck to a Snake Shit program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the globally accessible random number generator, while
   ignoring the INTERPRETER's presence, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (setf *random-state*
    (make-random-state T))
  (values))

;;; -------------------------------------------------------

(defun prepare-argument-pointer (interpreter)
  "Relocates the INTERPRETER's argument pointer to the cell immediately
   on the instruction pointer's (IP) dextral laterality and returns no
   value."
  (declare (type Interpreter interpreter))
    (with-slots (ip argument-pointer) interpreter
    (declare (type Pointer ip))
    (declare (type Pointer argument-pointer))
    (move-pointer-to argument-pointer
      (pointer-location ip))
    (move-pointer argument-pointer))
  (values))

;;; -------------------------------------------------------

(defun read-numeric-argument (interpreter)
  "Proceeding from the cell located immediately dextrally to the
   INTERPRETER's instruction pointer (IP) position in its grid, consumes
   a non-negative integer number and returns a ``non-negative-integer''
   object representation thereof.
   ---
   If no non-negative integer value could be detected, an error of the
   type ``Invalid-Argument-Error'' is issued."
  (declare (type Interpreter interpreter))
  (with-slots (grid argument-pointer) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer argument-pointer))
    (the non-negative-integer
      (handler-case
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop
              while (selected-character-satisfies-p
                      grid argument-pointer #'digit-char-p)
              do
                (write-char
                  (get-selected-character grid argument-pointer)
                  digits)
                (move-pointer argument-pointer))))
        (error ()
          (error 'Invalid-Argument-Error
            :grid             grid
            :location         (pointer-location argument-pointer)
            :expected-content "a non-negative integer"))))))

;;; -------------------------------------------------------

(defun read-direction-argument (interpreter)
  "Proceeding from the cell located immediately dextrally to the
   INTERPRETER's instruction pointer (IP) position in its grid, consumes
   a direction symbol and returns a ``direction'' representation
   thereof.
   ---
   If no direction symbol could be detected, an error of the type
   ``Invalid-Argument-Error'' is issued."
  (declare (type Interpreter interpreter))
  (with-slots (grid argument-pointer) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer argument-pointer))
    (the direction
      (if (selected-character-satisfies-p grid argument-pointer
                                          #'direction-character-p)
        (handler-case
          (prog1
            (parse-direction
              (get-selected-character grid argument-pointer))
            (move-pointer argument-pointer))
          (error ()
            (error 'Invalid-Argument-Error
              :grid             grid
              :location         (pointer-location argument-pointer)
              :expected-content "a direction symbol")))
        (error 'Invalid-Argument-Error
          :grid             grid
          :location         (pointer-location argument-pointer)
          :expected-content "a direction symbol")))))

;;; -------------------------------------------------------

(defun read-character-argument (interpreter)
  "Proceeding from the cell located immediately dextrally to the
   INTERPRETER's instruction pointer (IP) position in its grid, consumes
   the present character or an escaped compound and returns a character
   representation thereof.
   ---
   If no character could be detected, or if an escaped entity is either
   incomplete or invalid in its assemblage, an error of the type
   ``Invalid-Argument-Error'' is issued."
  (declare (type Interpreter interpreter))
  (with-slots (grid argument-pointer) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer argument-pointer))
    (the character
      (case (get-selected-character grid argument-pointer)
        ((NIL)
          (error 'Invalid-Argument-Error
            :grid             grid
            :location         (pointer-location argument-pointer)
            :expected-content "a character literal"))
        (#\\
          (move-pointer argument-pointer)
          (let ((escaped-character
                  (get-selected-character grid argument-pointer)))
            (declare (type (or null character) escaped-character))
            (case escaped-character
              ((NIL)
                (error 'Invalid-Argument-Error
                  :grid             grid
                  :location         (pointer-location argument-pointer)
                  :expected-content "a complete escape code"))
              (#\n
                (prog1 #\Newline
                  (move-pointer argument-pointer)))
              (#\t
                (prog1 #\Tab
                  (move-pointer argument-pointer)))
              (#\\
                (prog1 #\\
                  (move-pointer argument-pointer)))
              (otherwise
                (error 'Invalid-Argument-Error
                  :grid             grid
                  :location         (pointer-location argument-pointer)
                  :expected-content "a valid escape code")))))
        (otherwise
          (prog1
            (get-selected-character grid argument-pointer)
            (move-pointer argument-pointer)))))))

;;; -------------------------------------------------------

(defun empight-on-start (interpreter)
  "Relocates the INTERPRETER's instruction pointer (IP) to its grid's
   start point and returns no value.
   ---
   If no start point could be detected, an error of the type
   ``Missing-Start-Point-Error'' is signaled.
   ---
   If more than one start point is detected, an error of the type
   ``Ambiguous-Start-Points-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (with-slots (grid ip) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer ip))
    (let ((start-points (grid-start-points grid)))
      (declare (type (list-of Location) start-points))
      (cond
        ((null start-points)
          (error 'Missing-Start-Point-Error :grid grid))
        ((> (length start-points) 1)
          (error 'Ambiguous-Start-Points-Error
            :grid         grid
            :start-points start-points))
        (T
          (move-pointer-to ip
            (first start-points))))))
  (values))

;;; -------------------------------------------------------

(defun read-next-command (interpreter)
  "Proceeding from the current location into the INTERPRETER's grid,
   reads an instruction and returns a ``Command'' representation
   thereof."
  (declare (type Interpreter interpreter))
  (with-slots (grid ip argument-pointer) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer ip))
    (declare (type Pointer argument-pointer))
    
    (prepare-argument-pointer interpreter)
    
    (the Command
      (case (get-selected-character grid ip)
        ((NIL)
          (make-instance 'EOF-Command
            :end-point (pointer-location ip)))
        
        (#\$
          (make-instance 'Start-Command
            :argument  (read-direction-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\+
          (make-instance 'Increment-Command
            :argument  (read-numeric-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\-
          (make-instance 'Decrement-Command
            :argument  (read-numeric-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\=
          (make-instance 'Set-Length-Command
            :argument  (read-numeric-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\?
          (make-instance 'Random-Length-Command
            :argument  (read-numeric-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\@
          (make-instance 'Save-Length-Command
            :argument  (read-character-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\%
          (make-instance 'Load-Length-Command
            :argument  (read-character-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        ((#\< #\> #\v #\^)
          (make-instance 'Direction-Command
            :argument  (parse-direction
                         (get-selected-character grid ip))
            :end-point (get-next-pointer-location ip)))
        
        (#\&
          (make-instance 'Input-Command
            :end-point (get-next-pointer-location ip)))
        
        (#\#
          (make-instance 'Print-Character-Command
            :argument  (read-character-argument interpreter)
            :end-point (pointer-location argument-pointer)))
        
        (#\*
          (make-instance 'Print-Length-Command
            :end-point (get-next-pointer-location ip)))
        
        (#\~
          (make-instance 'Skip-Command
            :argument  (copy-location (pointer-location ip))
            :end-point (let ((original-ip (copy-pointer ip)))
                         (declare (type Pointer original-ip))
                         (setf (pointer-direction ip) :right)
                         (move-pointer ip)
                         (prog1
                           (get-command-end-point
                             (read-next-command interpreter))
                           (set-pointer-to ip original-ip)))))
        
        (otherwise
          (make-instance 'NOP-Command
            :end-point (get-next-pointer-location ip)))))))

;;; -------------------------------------------------------

(defun move-ip-beyond-command (interpreter command)
  "Relocates the INTERPRETER's instruction pointer (IP) beyond the
   COMMAND's occupied segment and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (with-slots (ip) interpreter
    (declare (type Pointer ip))
    (case (pointer-direction ip)
      (:right
        (move-pointer-to ip
          (get-command-end-point command)))
      (otherwise
        (move-pointer ip))))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's program is exhausted, a case
   whose signification proceeds from the instruction pointer's (IP)
   transcendence of the underlying grid's bournes, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (grid ip) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer ip))
    (the boolean
      (not (valid-pointer-location-p grid ip)))))

;;; -------------------------------------------------------

(defun save-snake-length (interpreter variable-name)
  "Stores the current snake length in the INTERPRETER's variable
   registry under the VARIABLE-NAME and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type character   variable-name))
  (with-slots (variables snake-length) interpreter
    (declare (type variable-registry    variables))
    (declare (type non-negative-integer snake-length))
    (setf (gethash variable-name variables) snake-length))
  (values))

;;; -------------------------------------------------------

(defun load-snake-length (interpreter variable-name)
  "Loads the snake length associated with the VARIABLE-NAME in the
   INTERPRETER, sets the current snake length to the same, and returns
   no value.
   ---
   If no variable with the name VARIABLE-NAME could detected, an error
   of the type ``No-Such-Variable-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (declare (type character   variable-name))
  (with-slots (variables snake-length) interpreter
    (declare (type variable-registry    variables))
    (declare (type non-negative-integer snake-length))
    (multiple-value-bind (new-snake-length contains-name-p)
        (gethash variable-name variables)
      (declare (type (or null non-negative-integer) new-snake-length))
      (declare (type T                              contains-name-p))
      (if contains-name-p
        (setf snake-length new-snake-length)
        (error 'No-Such-Variable-Error
          :interpreter    interpreter
          :offending-name variable-name))))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-command-processor
    (command-class (interpreter-variable command-variable)
     &body body)
  "Defines an implementation of the generic function ``process-command''
   in an eath manner, by assigning as a designation to the first
   argument the INTERPRETER-VARIABLE and to the second the
   COMMAND-VARIABLE, evaluating the BODY forms, and returning no value."
  `(defmethod process-command ((,interpreter-variable Interpreter)
                               (,command-variable     ,command-class))
     (declare (type Interpreter    ,interpreter-variable))
     (declare (ignorable           ,interpreter-variable))
     (declare (type ,command-class ,command-variable))
     (declare (ignorable           ,command-variable))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-command-processor Start-Command (interpreter command)
  (with-slots (program-started-p ip) interpreter
    (declare (type boolean program-started-p))
    (declare (type Pointer ip))
    (cond
      (program-started-p
        (case (pointer-direction ip)
          ;; Skip the "$" symbol and its directional argument.
          (:right
            (move-pointer ip)
            (move-pointer ip))
          (otherwise
            (move-pointer ip))))
      (T
        (setf program-started-p T)
        (let ((initial-direction (get-command-argument command)))
          (declare (type direction initial-direction))
          (setf (pointer-direction ip) initial-direction)
          (move-pointer ip))))))

;;; -------------------------------------------------------

(define-command-processor Decrement-Command (interpreter command)
  (with-slots (snake-length) interpreter
    (declare (type non-negative-integer snake-length))
    (setf snake-length
      (max 0
        (- snake-length
           (get-command-argument command)))))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Direction-Command (interpreter command)
  (with-slots (ip) interpreter
    (declare (type Pointer ip))
    (setf (pointer-direction ip)
      (get-command-argument command))
    (move-pointer ip)))

;;; -------------------------------------------------------

(define-command-processor EOF-Command (interpreter command))

;;; -------------------------------------------------------

(define-command-processor Increment-Command (interpreter command)
  (with-slots (snake-length) interpreter
    (declare (type non-negative-integer snake-length))
    (incf snake-length
      (get-command-argument command)))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Input-Command (interpreter command)
  (format T "~&>> ")
  (with-slots (snake-length) interpreter
    (declare (type non-negative-integer snake-length))
    (let ((input (read-line)))
      (declare (type string input))
      (handler-case
        (setf snake-length
          (parse-integer input))
        (error ()
          (error 'Invalid-Input-Error :offending-input input)))))
  (clear-input)
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Load-Length-Command (interpreter command)
  (load-snake-length interpreter
    (get-command-argument command))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor NOP-Command (interpreter command)
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Print-Character-Command (interpreter command)
  (format T "~c"
    (get-command-argument command))
  (finish-output)
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Print-Length-Command (interpreter command)
  (with-slots (snake-length ip) interpreter
    (declare (type non-negative-integer snake-length))
    (declare (type Pointer              ip))
    (format T "~d" snake-length)
    (move-pointer ip)))

;;; -------------------------------------------------------

(define-command-processor Random-Length-Command (interpreter command)
  (with-slots (snake-length) interpreter
    (declare (type non-negative-integer snake-length))
    (let ((upper-range-threshold (get-command-argument command)))
      (declare (type non-negative-integer upper-range-threshold))
      (setf snake-length
        (if (zerop upper-range-threshold)
          upper-range-threshold
          (random upper-range-threshold)))))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Save-Length-Command (interpreter command)
  (save-snake-length interpreter
    (get-command-argument command))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Set-Length-Command (interpreter command)
  (with-slots (snake-length) interpreter
    (declare (type non-negative-integer snake-length))
    (setf snake-length
      (get-command-argument command)))
  (move-ip-beyond-command interpreter command))

;;; -------------------------------------------------------

(define-command-processor Skip-Command (interpreter command)
  (with-slots (snake-length ip) interpreter
    (declare (type non-negative-integer snake-length))
    (declare (type Pointer              ip))
    (cond
      ((zerop snake-length)
        (move-ip-beyond-command interpreter command))
      (T
        (setf (pointer-direction ip) :right)
        (move-pointer ip)))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Snake Shit program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (empight-on-start interpreter)
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (read-next-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Snake-Shit (code)
  "Interprets the piece of Snake Shit source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-instance 'Interpreter :grid
      (build-grid code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Snake-Shit-Error (error)
  ()
  (:documentation
    "The ``Snake-Shit-Error'' condition type serves as a foundation for
     all conditions dedicated to the apprizal about anomalous
     circumstances during a Snake Shit program's obtention, lexical
     analyzation, parsing, and interpretation."))

;;; -------------------------------------------------------

(define-condition Ambiguous-Start-Points-Error (Snake-Shit-Error)
  ((grid
    :initarg       :grid
    :initform      (error "Missing grid.")
    :reader        ambiguous-start-points-error-grid
    :type          Grid
    :documentation "The grid comprehending the supernumerary start
                    points.")
   (start-points
    :initarg       :start-points
    :initform      (error "Missing start points.")
    :reader        ambiguous-start-points-error-start-points
    :type          (list-of Location)
    :documentation "A list of the two or more start points' locations,
                    arranged in accord with no specific order."))
  (:report
    (lambda (condition stream)
      (declare (type Ambiguous-Start-Points-Error condition))
      (declare (type destination                  stream))
      (format stream "~{~a~^, ~}"
        (ambiguous-start-points-error-start-points condition))))
  (:documentation
    "The ``Ambiguous-Start-Points-Error'' condition type serves to
     signal an anomalous situation issuing from the Snake Shit program
     endowed with more than one start point, designated by \"$\"
     symbols."))

;;; -------------------------------------------------------

(define-condition Invalid-Argument-Error (Snake-Shit-Error)
  ((grid
    :initarg       :grid
    :initform      (error "Missing grid.")
    :reader        invalid-argument-error-grid
    :type          Grid
    :documentation "The grid comprehending illegal argument.")
   (location
    :initarg       :location
    :initform      (error "Missing location.")
    :reader        invalid-argument-error-location
    :type          T
    :documentation "The location at which the unexpected argument has
                    been encountered.")
   (expected-content
    :initarg       :expected-content
    :initform      (error "Missing expected content.")
    :reader        invalid-argument-error-expected-content
    :type          string
    :documentation "A description of the expected, and absent,
                    argument."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Argument-Error condition))
      (declare (type destination            stream))
      (format stream "Expected ~a at the position ~s."
        (invalid-argument-error-expected-content condition)
        (invalid-argument-error-location         condition))))
  (:documentation
    "The ``Invalid-Argument-Error'' condition type serves to signal an
     anomalous situation issuing from the employment of an ineligible
     argument for a command or its lacuna."))

;;; -------------------------------------------------------

(define-condition Invalid-Input-Error (Snake-Shit-Error)
  ((offending-input
    :initarg       :offending-input
    :initform      (error "Missing offending input.")
    :reader        invalid-input-error-offending-input
    :type          string
    :documentation "The input, stored in its verbatim string form, which
                    could not be converted into a non-negative integer
                    object."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Input-Error condition))
      (declare (type destination         stream))
      (format stream "The input ~s cannot be parsed as a ~
                      non-negative integer value."
        (invalid-input-error-offending-input condition))))
  (:documentation
    "The ``Invalid-Input-Error'' condition type serves to signal an
     anomalous situation issuing from the transmission of an input
     lacking in the covenableness as a non-negative integer object's
     representation."))

;;; -------------------------------------------------------

(define-condition Missing-Start-Point-Error (Snake-Shit-Error)
  ((grid
    :initarg       :grid
    :initform      (error "Missing grid.")
    :reader        missing-start-point-error-grid
    :type          Grid
    :documentation "The grid lacking a start point."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Start-Point-Error condition))
      (declare (ignore                         condition))
      (declare (type destination               stream))
      (format stream "No start point defined for the program.")))
  (:documentation
    "The ``Missing-Start-Point-Error'' condition type serves to signal
     an anomalous situation issuing from a Snake Shit program destitute
     of any start point, designated by \"$\" symbols."))

;;; -------------------------------------------------------

(define-condition No-Such-Variable-Error (Snake-Shit-Error)
  ((interpreter
    :initarg       :interpreter
    :initform      (error "Missing interpreter.")
    :reader        no-such-variable-error-interpreter
    :type          Interpreter
    :documentation "The interpreter whose variable registry lacked the
                    requested variable name.")
   (offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        no-such-variable-error-offending-name
    :type          character
    :documentation "The identifier whose inquisition failed forecause
                    no variable responds to this designation."))
  (:report
    (lambda (condition stream)
      (declare (type No-Such-Variable-Error condition))
      (declare (type destination            stream))
      (format stream "No variable with the name \"~c\" exists."
        (no-such-variable-error-offending-name condition))))
  (:documentation
    "The ``No-Such-Variable-Error'' condition type serves to signal an
     anomalous situation issuing from the attempt to query a variable's
     value whose name has not yet been assigned."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Snake Shit code loader operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +RESOURCES-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +RESOURCES-DIRECTORY+
  (make-pathname)
  "Specifies the directory when the project's resources, in particular
   its exemplary programs, may be retrieved.
   ---
   Please substitute the +PROJECT-DIRECTORY+ path by your personal
   directory which comprehends the requisite Common Lisp source files
   for the interpreter.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
   
     ------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------------
     make-path-name   | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                \"Users\"
                      |                \"Kaveh\"
                      |                \"Snake Shit\"))
     ............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/Snake Shit/\")
     ------------------------------------------------------------")

;;; -------------------------------------------------------

(defgeneric read-file-content (source)
  (:documentation
    "Reads a Snake Shit program from the SOURCE and returns the thus
     obtained code string.")
  
  (:method ((source stream))
    "Returns the content of the SOURCE stream."
    (declare (type stream source))
    (the string
      (with-output-to-string (snake-shit-code)
        (declare (type string-stream snake-shit-code))
        (loop
          for current-line
            of-type (or null string)
            =       (read-line source NIL NIL)
          while current-line
          do    (format snake-shit-code "~%~a" current-line)))))
  
  (:method ((source pathname))
    "Returns the content of the file located by the path SOURCE."
    (declare (type pathname source))
    (the string
      (with-open-file (input-stream source
                       :element-type      'character
                       :direction         :input
                       :if-does-not-exist :error)
        (declare (type file-stream input-stream))
        (read-file-content input-stream))))
  
  (:method ((source string))
    "Returns the content of the file designated by the path SOURCE."
    (declare (type string source))
    (the string
      (with-open-file (input-stream source
                       :element-type      'character
                       :direction         :input
                       :if-does-not-exist :error)
        (declare (type file-stream input-stream))
        (read-file-content input-stream)))))

;;; -------------------------------------------------------

(defun load-Snake-Shit-program (source)
  "Loads a Snake Shit program from the SOURCE, interprets the same, and
   returns no value."
  (declare (type (or pathname stream string) source))
  (interpret-Snake-Shit
    (read-file-content source))
  (values))

;;; -------------------------------------------------------

(defun load-Snake-Shit-resource (file-name)
  "Loads a Snake Shit program located by the FILE-NAME in the resources
   directory, specified by the +RESOURCES-DIRECTORY+ constant,
   interprets the same, and returns no value."
  (declare (type file-source file-name))
  (load-Snake-Shit-program
    (merge-pathnames +RESOURCES-DIRECTORY+ file-name))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code builder operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates the LINES into a single string, where each twissel's
   intermede is invested with an aefauld newline character as the
   sepiment, thus forming a unified equivalency of the input LINES'
   spatial information."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~&~a~}" lines)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print a random number desumed from the integral range [0, 6].
(interpret-Snake-Shit "$> ?7 *")

;;; -------------------------------------------------------

;; Print a random number desumed from the integral range [1, 6], in this
;; simulating a die's roll.
(interpret-Snake-Shit "$> ?6 +1 *")

;;; -------------------------------------------------------

;; Infinite numeric cat program.
(interpret-Snake-Shit
  (concatenate-lines
    "$>_&_*v"
    " ^____<"))

;;; -------------------------------------------------------

;; Test the conditional skip facility by omitting the output of "a" and
;; proceeding to the printing of "b".
(interpret-Snake-Shit "$>~#a#b")

;;; -------------------------------------------------------

;; Test the conditional skip facility by omitting the output of "a" and
;; proceeding to the printing of "b".
(interpret-Snake-Shit
  (concatenate-lines
    "$v"
    "~#a"
    "#b"))

;;; -------------------------------------------------------

;; Counter which count downs from inclusive ten (10) to inclusive
;; zero (0), printing each cycle state on its personal line.
(interpret-Snake-Shit
  (concatenate-lines
    "$>_=10_>_~v_*"
    "       _  *"
    "       _  #\\n"
    "       _  -1"
    "       ^__<"))

;;; -------------------------------------------------------

;; Truth-machine which employs character output.
(interpret-Snake-Shit
  (concatenate-lines
    "$>_&_~v#0"
    "    >_v"
    "    __#1"
    "    ^_<"))

;;; -------------------------------------------------------

;; Truth-machine which employs the snake length as output.
(interpret-Snake-Shit
  (concatenate-lines
    "$>_&_~v*"
    "    >_v"
    "    __*"
    "    ^_<"))

;;; -------------------------------------------------------

;; Mirror-machine.
;; 
;; Please note that restriction of inputs, especially that of the first
;; variable "a", to unsigned non-negative integers.
(load-Snake-Shit-resource "Mirror-machine.txt")

;;; -------------------------------------------------------

(load-Snake-Shit-resource "Test_x10.txt")

;;; -------------------------------------------------------

(load-Snake-Shit-resource "Number_Guessing_Game.txt")
