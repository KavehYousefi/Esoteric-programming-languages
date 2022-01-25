;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tic Tac Toe", invented by the Esolang user "JWinslow23",
;; a derivative of the esoteric language "brainfuck" by Urban Mueller.
;; 
;; Concepts
;; ========
;; Tic Tac Toe's approach to programming conforms to the simulation of
;; playing the eponymous pen-and-pencil game "tic-tac-toe". While no
;; actual competition animates the process, adherence to the rules
;; remains a requisite. A program's ceremonials are expressed in a
;; manner akin to natural language, and their effect ultimately produces
;; brainfuck commands.
;; 
;; == THE BASICS OF THE GAME "TIC TAC TOE" ==
;; Based upon the paramount significant of its acquaintance for the
;; comprehension of this language, in the following a catagraph of the
;; inspiring game shall be delivered. Readers endowed with sufficient
;; knowledge in this regard are implored to skip this section.
;; 
;; Tic-tac-toe is usually played on a piece of paper using a pen, albeit
;; any surface amenable to akin manipulation is lend admission. By
;; creation of two parallel lines horizontally and such a twain in the
;; vertical expansion, a 3 times 3 grid of nine squares is produced,
;; as the following sketch shall limn:
;; 
;;        |   |
;;     ---+---+---
;;        |   |
;;     ---+---+---
;;        |   |
;; 
;; A prerequisite to the proceeding, two players must partake, each one
;; designated with his own dioristic mark: The first player is
;; represented by a cross "X", the second by a circle "O".
;; 
;; Starting with the first player, wielding the signum "X", the active
;; participant must choose exactly one vacant square and occupy it with
;; his mark. With the deed executed, the next player takes his turn,
;; proceeding in this fashion until a victory on any side or an impasse
;; on both proclaims the game's desinence. A square thus recognized is
;; persistently attached to the marking player, and hence cannot be
;; cleared or appropriated by either the opponent or the possessor.
;; 
;; The objective of this disport resides in the endeavor of a player
;; to fill a rank with a continuous sequence of his own marks. The
;; concrete configuration of such a winning adjacency enumerates in the
;; following:
;; 
;;   - A row, that is, three horizontally aligned squares.
;;   - A column, that is, three vertically aligned squares.
;;   - A diagonal, that is, either the three squares from the top-left
;;     corner to the right-bottom, or that triple from the top-right
;;     corner to the bottom-left.
;; 
;; The diagrams to the bottom shall elucidate the eight victorious
;; settings in a visual manner:
;; 
;;                              Horizontal
;;         Top                    Center                   Bottom
;;      # | # | #                  |   |                    |   |
;;     ---+---+---              ---+---+---              ---+---+---
;;        |   |                  # | # | #                  |   |
;;     ---+---+---              ---+---+---              ---+---+---
;;        |   |                    |   |                  # | # | #
;;   
;;                               Vertical
;;        Left                     Center                   Right
;;      # |   |                    | # |                    |   | #
;;     ---+---+---              ---+---+---              ---+---+---
;;      # |   |                    | # |                    |   | #
;;     ---+---+---              ---+---+---              ---+---+---
;;      # |   |                    | # |                    |   | #
;;   
;;                               Diagonal
;;     Top-left to bottom-right            Top-right to bottom-left
;;      # |   |                                 |   | #
;;     ---+---+---                           ---+---+---
;;        | # |                                 | # |
;;     ---+---+---                           ---+---+---
;;        |   | #                             # |   |
;; 
;; Concomitantly to the individual striving of a participant for a
;; successful combination, the subordinate warkloom intended for a
;; conquest is ascribed to the hindrance of his opponent. If a player is
;; threatened with defeat by his peer's looming completion of a line,
;; he might empight his mark, given that it is his turn, into the
;; opponent's trajectory, thus foiling the hostile progress.
;; 
;; == "TIC TAC TOE": THE GAME AND THE LANGUAGE ==
;; A Tic Tac Toe program constituting a simulation of the game produces
;; a brainfuck program encoded in the moves of the provenance language.
;; A corollary from these relations appertains to the necesssity to
;; indagate the vincula betwixt the game, Tic Tac Toe, and brainfuck
;; with augmented focus. It shall ostend forensecal to airt the process
;; from the desinence to the primacy, yet the telos as the goading
;; principle will ultimately facilitate our comprehension of the
;; designment, when we attend to the scrutinity of Tic Tac Toe and
;; brainfuck, ere we proceed to the implications of the ludibund
;; department.
;; 
;; == EACH SQUARE REPRESENTS ONE BRAINFUCK INSTRUCTION ==
;; Tic Tac Toe's purpose aligns with the optation to generate brainfuck
;; code, with the means the signature of its kenspeckle appearance. The
;; assignment of a game move to a target instruction is mediated by the
;; board's squares.
;; 
;; Motivated by the requirement of conveying to the program the commands
;; in the form of square markings, these compartments perforce must be
;; amenable to an identifying agnomination. The language imparts the
;; following nine names to the equinumerant squares:
;; 
;;    a1 | a2 | a3
;;   ----+----+----
;;    b1 | b2 | b3
;;   ----+----+----
;;    c1 | c2 | c3
;; 
;; The question arises how the operation on squares may actually produce
;; an effect. To this end, each such cell accommodates the space for a
;; particular brainfuck instruction:
;; 
;;     +  |  -  |  >
;;   -----+-----+-----
;;     <  | NOP |  [
;;   -----+-----+-----
;;     ]  |  ,  |  .
;; 
;; An apercu shall be adduced in the following table; for a more
;; elaborate treatment, please consult the "Instructions" section.
;; 
;;   Square | brainfuck command
;;   -------+------------------
;;    a1    | +
;;    a2    | -
;;    a3    | >
;;    b1    | <
;;    b2    | NOP (equivalent to an ignore character in brainfuck)
;;    b3    | [
;;    c1    | ]
;;    c2    | ,
;;    c3    | .
;; 
;; Our treatise's denouement registers the fact that, by marking the
;; board squares in a constructive order, a valid brainfuck instruction
;; sequence may be generated. The rules to such an obtention embrace
;; some convolutions that urge further lecturing.
;; 
;; == EACH PROGRAM CONSISTS OF THREE STAGES ==
;; Every Tic Tac Toe program describes a composite of three stages,
;; the last two of which may occur multiple times. An overview shall be
;; presented:
;; 
;;   (1) Main player mark selection.
;;       The player whose marks shall contribute to the resulting
;;       brainfuck code, here clept the "main player", is selected. The
;;       other player, known as the "opponent" automatically assumes the
;;       remaining mark.
;;       The syntax constitutes:
;;         I will play as {MARK}.
;;       where {MARK} must be either of 'X' or 'O'.
;;   
;;   (2) Game number declaration.
;;       Given the fact that each square can be occupied only once, and
;;       that the main player's opponent may appropriate a necessary
;;       place, a program may comprehend more that one game. Every game
;;       must be introduced with its number, a value starting with
;;       one (1) and increasing continually with a cadence of 1.
;;       The syntax constitutes:
;;         Game {NUMBER}:
;;       where {NUMBER} must be an integer greater than or equal to 1,
;;       with each invocation being one value greater than the previous.
;;   
;;   (3) Player moves.
;;       The moves of the players, starting with the holder of the 'X'
;;       mark, are listed. If the actor constitutes the main player,
;;       and his move does not eventuate a victory, the brainfuck
;;       command associated with his chosen square contributes to the
;;       result. The moves, just like in the actual game tic-tac-toe,
;;       must alternate betwixt the players.
;;       The syntax constitutes:
;;         {MARK} went on {SQUARE}.
;;       where the {MARK} is either of 'X' or 'O', and the {SQUARE}
;;       resolves to one of the nine square names "a1" to "c3".
;; 
;; == STAGE 1: MAIN PLAYER SELECTION ==
;; Emulating the real-life disport of tic-tac-toe, the language engages
;; in its inchoation with the allotting of one of the two marks, 'X' and
;; 'O', to the twain of participants. The programmer's representative
;; mark {MARK}, established via the command
;;   I will play as {MARK}.
;; where the argument may only produce 'X' or 'O', designates the main
;; player's signature, an association retained through the complete
;; course of the program. The overthwart mark is subsequently assigned
;; to the opponent.
;; 
;; The choice is yet apportioned an infinitely more ponderant magnitude
;; by the language as opposed to the game, forecause only those squares
;; occupied by the main player will contribute to the ultimately
;; generated brainfuck code.
;; 
;; Please note that, irregardless of the main player's signature, the
;; first turn of each game must be an act of the 'X' mark.
;; 
;; == STAGE 2: GAME DECLARATION ==
;; The confrotation with the gnarity that
;;   (a) only the main player's chosen squares contribute to the
;;       output brainfuck code,
;;   (b) only a non-NOP operation and only operations which do not
;;       issue a victor contribute to the output brainfuck code,
;;   (c) each board contains one instance of a brainfuck command and,
;;       as it may be marked merely once, this instance may at most
;;       contribute one time to the output brainfuck code,
;; apprizes us about a severe stricture, which would render the
;; resulting code fairly impotent.
;; 
;; The Tic Tac Toe language responds to this conflict by permitting an
;; arbitrary number of games to be played in the course of a single
;; program. That means, a completed board ends one game, and a new
;; round will start with a fresh grid, retaining, however, the main
;; player's identity and thus his mark. A declaration assumes the
;; pattern
;;   Game {NUMBER}:
;; where {NUMBER} conveys an integer value greater than or equal to one
;; (1).
;; 
;; Disencumbered from the tally of games, their enumeration must
;; entertain conformance to a set of rules: The first game is numbered
;; with 1, and any subsequent game increases the previous counter by
;; a magnitude of 1, which generates for "n" games the sequence 1, 2,
;; 3, ..., n. The command order must thus conform to the pattern
;;   Game 1:
;;   ...
;;   Game 2:
;;   ...
;;   Game N:
;;   ...
;; 
;; == STAGE 3: MOVES ==
;; The preceding instructions being subsumed into the ceremonials for a
;; programs's presceve, the warklooms whose potentials may produce
;; actual effect are now admitted treatment.
;; 
;; The most conspicuous parallel to the inspirational disport, the
;; language's statements follow a design evocative of a session played
;; by two opponents. Starting with the player responding to the 'X'
;; mark, regardless of whether this refers to the main player or the
;; opponent, each participant chooses a vacant square to occupy by his
;; symbol, ere his peer takes his turn. Conformance to the alternation
;; is mandatory, such that no player may skip a round or appropriate two
;; or more actions in immediate succession, in the same degree as the
;; vacancy of a square imposes a prerequisite for its embrace of the
;; symbol. The responsible command
;;   {MARK} went on {SQUARE}.
;; orders the currently expected {MARK} 'X' or 'O' to conquer one of the
;; nine possible {SQUARE} values, that is, "a1" throughout "c3".
;; 
;; Thus being marked, if effectuated by the main player, the brainfuck
;; command associated with the chosen square (for which please see the
;; table under the section "EACH SQUARE REPRESENTS ONE BRAINFUCK
;; INSTRUCTION") contributes to the resulting instruction sequence. An
;; exemption from this, if a main player's square wins the game, the
;; brainfuck instruction is not counted into the result. For example,
;; if the main player fills the left column by conquering in this exact
;; order the squares "b1", "a1", and "c1", the commands "<" for "b1" and
;; "+" for "a1" are admitted, while the command "]" represented by "c1",
;; however, by inflicting a victory upon the game, is not introduced
;; into the output brainfuck program.
;; 
;; 
;; Architecture
;; ============
;; Tic Tac Toe's fidelity to its brainfuck heritage promulgates through
;; the architecture and type system. The language operates on an
;; infinite tape of unbounded integer numbers, stored in the cells,
;; a single instance of which at any time constitutes the active entity,
;; designated by a pointer.
;; 
;; == DATA IS STORED IN A TAPE ==
;; The salvatory dedicated to any data management describes the
;; bailiwick of a tape-like series of cells, known as the memory, which
;; bilaterally extends in its tally into infinity. A cell embraces a
;; scalar integer value of paregal liberality, that means the range
;; [-infinity, +infinity], initialized to zero.
;; 
;; == THE POINTER MARKS THE ACTIVE CELL ==
;; At any instant in the program, a single cell is designated as the
;; active or selected member. The mechanism for this emphasis is
;; designed by the memory pointer, a motile reference, amenable to
;; certain operations for its navigation across the tape. Similarly,
;; commands exist to manipulate the active cell.
;; 
;; 
;; Data Types
;; ==========
;; Akin to brainfuck, Tic Tac Toe implements a dichotomy of data types
;; into integers for operative purposes and characters as tokens in the
;; communication with the user.
;; 
;; == INTEGERS REPRESENT THE PROGRAM DATA ==
;; The superior role of integers manifests itself in their castaldy by
;; the central memory. Any data under a program's purview subsumes into
;; this category, including the scan operations for their augmentation
;; and deduction.
;; 
;; == CHARACTERS REPRESENT THE INTERFACE ==
;; Characters as one of the multifarious applications of integers, and
;; the sole inherent beside the verbatim utilization, exert their
;; usefulness in the restricted area of user-program communication:
;; The user, if queried for an input, responds with a character, which
;; by subsequent transformation into its ASCII code finds its way into
;; the memory; in a symmetrical fashion, data output involves integer
;; numbers construed as ASCII codes and printed in this aspect. The
;; lacuna of text manipulation capabilities bewrays this type's inferior
;; consideration.
;; 
;; == THE TAPE AS A COLLECTION ==
;; The diorism administered to the memory as an architectural component
;; reverbs in its definition in the data representation. The cells,
;; linearly ordered, demonstrate a linkage which permits their traversal
;; in both airts. The tape as a data type assumes a one-dimensional
;; realization, usually rendered conrete as a random access vector or
;; a linked list.
;; 
;; 
;; Syntax
;; ======
;; Tic Tac Toe employs a syntax similar to natural language in order to
;; describe the configuration as well as the progress of one or more
;; games, the coefficiency of which produces a program.
;; 
;; == A PROGRAM MIMICS A GAME PROTOCOL ==
;; Tic Tac Toe's design deliberately entertains a mimicry of natural
;; language, the display of its endeavors rendering tantamount to a
;; protocol of a game's progress or the communications in the selfsame's
;; relation to a heedful audience.
;; 
;; == INSTRUCTION DESIGN ==
;; The concinnity yielded from the natural apparel particularizes
;; paravaunt the command architecture. Established by one or more fixed
;; tokens, arguments are interspersed into the sentence-like structure.
;; 
;; == CASE AND SPACING RULES ==
;; The language's perimeter embraces case-sentitivity as well as
;; liberality in the expression of whitespaces, including in their
;; compass spaces, tabs, and linebreaks.
;; 
;; == GRAMMAR ==
;; The language syntax can be expressed in the following Extended
;; Backus-Naur form (EBNF) description.
;; 
;;   playerDeclaration := "I" , "will" , "play" , "as" , mark , "." ;
;;   gameDeclaration   := "Game" , integer , ":" ;
;;   move              := mark , "went" , "on" , square , "." ;
;;   mark              := "X" | "O" ;
;;   square            := "a1" | "a2" | "a3"
;;                     |  "b1" | "b2" | "b3"
;;                     |  "c1" | "c2" | "c3" ;
;;   integer           := digit , { digit } ;
;;   digit             := "0" | ... | "9" ;
;; 
;; 
;; Instructions
;; ============
;; Ultimately, Tic Tac Toe assumes brainfuck's instruction set verbatim,
;; introducing as an adscititious member the no-operation command "NOP".
;; 
;; == OVERVIEW ==
;; The following table shall describe the nine available instructions,
;; in juxtaposition, if possible, to their brainfuck equivalents.
;; 
;;   Square | bf command | Effect
;;   -------+------------+---------------------------------------------
;;    a1    | +          | Increases the value of the current cell by
;;          |            | one.
;;   ..................................................................
;;    a2    | -          | Decreases the value of the current cell by
;;          |            | one.
;;   ..................................................................
;;    a3    | >          | Moves the memory pointer one cell to the
;;          |            | right.
;;   ..................................................................
;;    b1    | <          | Moves the memory pointer one cell to the
;;          |            | left.
;;   ..................................................................
;;    b2    |            | Does not exercise any effect, that is, a
;;          |            | NOP (no operation). Cannot be associated
;;          |            | with any particular brainfuck command, but
;;          |            | is the equivalent of an unrecognized
;;          |            | character, a comment, in the same.
;;   ..................................................................
;;    b3    | [          | If the current cell value equals zero, moves
;;          |            | the instruction pointer forward to the
;;          |            | character immediately following the matching
;;          |            | ']'; otherwise simply advances to the next
;;          |            | character.
;;   ..................................................................
;;    c1    | ]          | If the current cell value does not equal
;;          |            | zero, moves the instruction pointer backward
;;          |            | to the character immediately following the
;;          |            | matching '['; otherwise simply advances to
;;          |            | the next character.
;;   ..................................................................
;;    c2    | ,          | Prompts the user for a character and stores
;;          |            | the ASCII code associated with the same into
;;          |            | the current cell.
;;   ..................................................................
;;    c3    | .          | Prints to the standard output the ASCII
;;          |            | character associated with the value of the
;;          |            | current cell.
;; 
;; 
;; Implementation
;; ==============
;; The processing and execution of the Tic Tac Toe programming language
;; depends upon the efforts of three components: lexer, parser, and
;; interpreter. Their coefficiency transliterates the code string into
;; tokens, which themselves are molded into an abstract syntax tree
;; (AST), and finally traversed by an interpreter which imbues the
;; compound with actual effect. While the lexer's operations instantiate
;; a quotidian endeavor, the remaining two collaborators' mechanisms
;; remain worthy of further elucidation.
;; 
;; == THE PARSER PRODUCES AN AST FROM TOKENS ==
;; The lexer's production of tokens and the parser's reception of same
;; impels the latter participant's generation of an abstract syntrax
;; tree, commonly abbreviated to AST, and designating a hierarchical
;; formulation of the original source code in a tree arrangement. The
;; paravaunt components of the language, such as assignments, operators,
;; iteration facilities, and literals, experience an expression in the
;; form of nodes, the content of which encompasses all information
;; necessary for their execution in the context of a program.
;; 
;; The concrete reproduction of these nodes, albeit ligated into some
;; consanguinity across many if not most programming languages, in Tic
;; Tac Toe culminates in a very restricted ambit --- a consectary of its
;; utterly simple syntax.
;; 
;; == A SINGLE NODE CLASS ==
;; Sophistication in the design of interpreters preponderantly betokens
;; an interface from which dedicated node subclasses, each one a custom
;; produce for the handling of a particular language facility,
;; originate. The solution adjudged apropos to this simple
;; implementation does not advocate lealty to such best practices,
;; instead offering a single ``Node'' class with a hash table as a
;; storage of arbitrary key-value pairs in lieu of class slots or
;; fields.
;; 
;; As a dioristic element, conducive to the discriminating of nodes by
;; category akin to subclasses, each such object stores a mandatory
;; "type" keyword. Without the class identity, and more potent in
;; juxtaposition, recognition and dispatchment are empowered to manifest
;; by this datum. Any further information is contained in a hash table
;; "attributes", the keys of which designate the attribute names, again
;; as keyword symbols, associated with arbitrary values.
;; 
;; This generalized node definition conditions a mete of flexibility
;; elusive to the static haecceity inherent to a hierarchical type
;; architecture. A consanguinity presides in the style of programming
;; known as "duck typing", where the dynamic attachment, manipulation,
;; detachment of data and operations to an object supersedes the
;; identity molded into classes and their static protocol or interface.
;; There remains, on the other hand, a conspicuous costage levied upon
;; this solution's simplicity, in that the cambistry applied to the
;; attributes map obscures the substance of the simulated nodes,
;; betraying the fidelity to the syntax tree model. Demonstrated, for
;; instance, on a conceptual "MoveNode", in a characterization of the
;; setting of a mark by the Tic Tac Toe instruction
;;   {MARK} went on {SQUARE}.
;; the blueprint would offer the operations
;;   MoveNode.getPlayer () : Mark
;;   MoveNode.getSquare () : string
;; while the here chosen representation by mediation of attributes
;; calligates the characteristics into
;;   Node.getAttribute (attributeName : string) : Object
;; ultimately deprieving the node of its identity.
;; 
;; == FOUR TYPES OF NODES ==
;; The discount in expressiveness already subjected to disquisition in
;; the preceding sections, and culpable of diminishing the diorisms of
;; the individual node types, effectuates the following documentation,
;; the contents of the same comprehend the commands, or significant
;; events in the parsing process, and their causata on the associated
;; ``Node'' data. Each of the four compartments describes a particular
;; rendition of a ``Node'' instance, complemented with the juxtaposition
;; of arguments and attributes.
;; 
;;   Command         | (Start of file.)
;;   Node type       | :program
;;   Node attributes | :statements {nodes : Node[0..*]}
;;   ............................................................
;;   Command         | I will play as {MARK}.
;;   Node type       | :player-declaration
;;   Node attributes | :mark {MARK : mark}
;;   ............................................................
;;   Command         | Game {NUMBER}:
;;   Node type       | :game-declaration
;;   Node attributes | :number {NUMBER : integer [1, +infinity]}
;;   ............................................................
;;   Command         | {MARK} went on {SQUARE}.
;;   Node type       | :move
;;   Node attributes | :mark {MARK : mark}
;;                   | :square {SQUARE : string}
;; 
;; The tabular exposition limns in a lucid manner that the AST root
;; element is in any case --- even if confronted with a null program ---
;; of the ``:program'' type.
;; 
;; == INTERPRETER: A BRAINFUCK CODE GENERATOR ==
;; The establishment and allocation of the AST structure manifests the
;; catalyst for the interpreter's officiation. Basically a derivation
;; of brainfuck, reason redes that a Tic Tac Toe program transliterates
;; into the archetype's form; the interpreter thus visits the nodes
;; in pursue of constructing the brainfuck equivalent from its own
;; representation. In a final step, the resulting code will be executed
;; in the fashion apportioned sufficient acquaintance with this simple
;; target language.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-31
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Tic_Tac_Toe"
;;   -> "https://en.wikipedia.org/wiki/Tic-tac-toe"
;;       o Description of the game tic-tac-toe.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements
   conforming to the ELEMENT-TYPE, which defaults to ``T''."
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

(deftype key-value-list-of (&optional (key-type T) (value-type T))
  "The ``key-value-list-of'' type defines an empty or even-sized list
   of elements, two consecutive members of which form a key-value pair,
   the key conforming to the KEY-TYPE and the value to the VALUE-TYPE,
   both of which default to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (evenp (length (the list object)))
            (loop
              for (key value)
                of-type (T T)
                on      (the list object)
                by      #'cddr
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE, both defaulting to a
   comprehensive ``T''."
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

(deftype game-state ()
  "The ``game-state'' type defines the possible states appertaining to
   the progress of a program in the guise of a game.
   ---
   The four state constitute:
     :PLAYER-SELECT --- Signifies that the arriving command is expected
                        to associate a mark with the main player.
     :GAME-START    --- Signifies that the arriving command is expected
                        to start the first game round.
     :GAME-ON       --- Signifies that the arriving command is expected
                        to convey the setting of a mark by a player.
     :GAME-WON      --- Signifies that the arriving command is expected
                        to start a further new game round."
  '(member :player-select :game-start :game-on :game-won))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of marks.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype mark ()
  "The ``mark'' type defines the possible contents for squares in a
   tic-tac-toe board."
  '(member :empty :X :O))

;;; -------------------------------------------------------

(defun mark-symbol (mark)
  "Returns a string representation of the MARK."
  (declare (type mark mark))
  (the string
    (case mark
      (:empty    " ")
      (:X        "X")
      (:O        "O")
      (otherwise (error "Invalid mark ~s to print." mark)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of square operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +SQUARE-IDENTIFIERS+
  '(("a1" . 0) ("a2" . 1) ("a3" . 2)
    ("b1" . 3) ("b2" . 4) ("b3" . 5)
    ("c1" . 6) ("c2" . 7) ("c3" . 8))
  "Associates each square name with its row-major index in a tic-tac-toe
   board.")

;;; -------------------------------------------------------

(defun square-position (square-identifier)
  "Returns the row-major index of the SQUARE-IDENTIFIER in a
   tic-tac-toe board."
  (declare (type string square-identifier))
  (the (or null fixnum)
    (cdr
      (assoc square-identifier +SQUARE-IDENTIFIERS+ :test #'string=))))

;;; -------------------------------------------------------

(defun square-name-p (identifier)
  "Checks whether the IDENTIFIER constitutes the name of a square,
   returning a ``boolean'' value of ``T'' on confirmation and ``NIL''
   on mismatch."
  (declare (type string identifier))
  (the boolean
    (not (null (find identifier +SQUARE-IDENTIFIERS+
                 :key #'car :test #'string=)))))

;;; -------------------------------------------------------

(defparameter +SQUARE-COMMANDS+
  '(("a1" . #\+) ("a2" . #\-) ("a3" . #\>)
    ("b1" . #\<) ("b2" . NIL) ("b3" . #\[)
    ("c1" . #\]) ("c2" . #\,) ("c3" . #\.))
  "Associates each square name with a brainfuck command or the special
   'NOP' (no operation) instruction.")

;;; -------------------------------------------------------

(defun square-command (square-identifier)
  "Returns the brainfuck command associated with the SQUARE-IDENTIFIER,
   or ``NIL'' in the case of the NOP (no operation) instruction."
  (declare (type string square-identifier))
  (the (or null character)
    (cdr (assoc square-identifier +SQUARE-COMMANDS+ :test #'string=))))

;;; -------------------------------------------------------

(defparameter +WINNING-POSITIONS+
  '(("a1" "a2" "a3")
    ("b1" "b2" "b3")
    ("c1" "c2" "c3")
    
    ("a1" "b1" "c1")
    ("a2" "b2" "c2")
    ("a3" "b3" "c3")
    
    ("a1" "b2" "c3")
    ("a3" "b2" "c1"))
  "Defines the names identifying the eight possible winning positions
   on a tic-tac-toe board.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Board".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Board
  "The ``Board'' class models the tic-tac-toe board, amenable to the
   reception of player marks."
  (squares
    (make-array 9
      :element-type 'mark :initial-element :empty :adjustable NIL)
    :type (vector mark 9)))

;;; -------------------------------------------------------

(defun board-mark (board square)
  "Returns the ``mark'' of the SQUARE on the BOARD."
  (declare (type Board  board))
  (declare (type string square))
  (the mark (aref (board-squares board) (square-position square))))

;;; -------------------------------------------------------

(defun (setf board-mark) (mark board square)
  "Sets the MARK on the BOARD's SQUARE, substituting any previous
   content there, and returns the modified BOARD."
  (declare (type Board  board))
  (declare (type string square))
  (declare (type mark   mark))
  (setf (aref (board-squares board) (square-position square)) mark)
  (the Board board))
;;; -------------------------------------------------------

(defun board-is-occupied (board square)
  "Checks whether the BOARD's SQUARE is occupied, that is, it contains
   a mark not of the species ``:empty'', returning a ``boolean'' value
   of ``T'' on confirmation and ``NIL'' otherwise."
  (declare (type Board  board))
  (declare (type string square))
  (the boolean (not (eq (board-mark board square) :empty))))

;;; -------------------------------------------------------

(defun board-is-full (board)
  "Checks whether the all squares of the BOARD are occupied, returning
   a ``boolean'' value of ``T'' on confirmation, or ``NIL'' if at least
   one square is still vacant."
  (declare (type Board board))
  (the boolean (not (find :empty (board-squares board)))))

;;; -------------------------------------------------------

(defun board-is-solved (board)
  "Checks whether the BOARD is solved, that is, a player mark has
   assumed a winning position, returning on confirmation two values:
   (1) the winning ``mark'' and (2) the winning configuration, or, upon
   not being solved, the two values (1) ``NIL'' and (2) ``NIL''."
  (declare (type Board board))
  (the (values (or null mark) (or null (list-of string)))
    (loop
      for
        winning-configuration
        of-type (list-of string)
        in      +WINNING-POSITIONS+
      for
        winning-marks
        of-type (list-of mark)
        =       (mapcar
                  #'(lambda (square-name)
                      (declare (type string square-name))
                      (row-major-aref (board-squares board)
                        (square-position square-name)))
                  winning-configuration)
      if (= (count :X winning-marks :test #'eq) 3)
        return (values :X winning-configuration)
      else if (= (count :O winning-marks :test #'eq) 3)
        return (values :O winning-configuration)
      end
      finally
        (return (values NIL NIL)))))

;;; -------------------------------------------------------

(defun board-clear (board)
  "Removes any marks from the BOARD, designating all of its squares as
   vacant, and returns the modified BOARD."
  (declare (type Board board))
  (loop
    for square-position
      of-type fixnum
      from    0
      below   (length (board-squares board))
    do
      (setf (aref (board-squares board) square-position) :empty))
  (the Board board))

;;; -------------------------------------------------------

(defun board-command-at (board square)
  "Returns the brainfuck command associated with the SQUARE in the
   BOARD, or ``NIL'' in the case of the NOP (no operation)."
  (declare (type Board  board))
  (declare (ignore      board))
  (declare (type string square))
  (the (or null character) (square-command square)))

;;; -------------------------------------------------------

(defmethod print-object ((board Board) stream)
  (declare (type Board                           board))
  (declare (type (or null (eql T) stream string) stream))
  (format stream "~& ~a | ~a | ~a "
    (mark-symbol (board-mark board "a1"))
    (mark-symbol (board-mark board "a2"))
    (mark-symbol (board-mark board "a3")))
  (format stream "~&---+---+---")
  (format stream "~& ~a | ~a | ~a "
    (mark-symbol (board-mark board "b1"))
    (mark-symbol (board-mark board "b2"))
    (mark-symbol (board-mark board "b3")))
  (format stream "~&---+---+---")
  (format stream "~& ~a | ~a | ~a "
    (mark-symbol (board-mark board "c1"))
    (mark-symbol (board-mark board "c2"))
    (mark-symbol (board-mark board "c3"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class describes a significant portion recognized during
   the analyzation of a piece of Tic Tac Toe source code."
  (type  NIL :type (or null keyword))
  (value NIL :type T))

;;; -------------------------------------------------------

(defun token-has-type-of (token expected-type)
  "Checks whether the TOKEN type matches the EXPECTED-TYPE, returning
   a ``boolean'' value of ``T'' on match, and otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (token-type token) expected-type)))))

;;; -------------------------------------------------------

(defun token-is-word (token expected-word)
  "Checks whether the TOKEN constitutes a ``:WORD'' containing the
   EXPECTED-WORD as its value, returning a ``boolean'' value of ``T''
   on confirmation or ``NIL'' on mismatch."
  (declare (type Token token))
  (the boolean
    (not (null
      (and (token-has-type-of token :word)
           (string= (token-value token) expected-word))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Cannot create a lexer without a source.")
    :type          string
    :documentation "The Tic Tac Toe source code.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current location in the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the location in the SOURCE."))
  (:documentation
    "The ``Lexer'' recognizes and returns the tokens of a piece of
     Tic Tac Toe code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((Lexer lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (plusp (length source))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a ``Lexer'' operating on the Tic Tac Toe SOURCE
   code."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Relocates the LEXER to the next character of its maintained source,
   if possible, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (< position (1- (length source)))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER constitutes a whitespace, returning a
   ``boolean'' value of ``T'' on confirmation and ``NIL'' otherwise."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline #\Linefeed)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting the current position, the LEXER skips zero or more adjacent
   whitespaces, relocating its pointer to the first non-whitespace
   character, and returning the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position, the LEXER reads zero or more
   digits, building a non-negative integer number thereform, and returns
   it encapsulated in a ``Token''."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :integer
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-word (lexer)
  "Starting at the current position, orders the LEXER to read zero or
   more alphanumeric characters and returns them as a string."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (word)
        (loop while (and character (alphanumericp character)) do
          (write-char character word)
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns from the LEXER the next ``Token''.
   ---
   Upon its exhaustion, the LEXER continuously returns a fresh instance
   of a token with the type ``:EOF'' and the value ``NIL''. An invalid
   character incites the signaling of an error of unspecified type."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ((null character)
          (make-token :EOF NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((alpha-char-p character)
          (let ((word (lexer-read-word lexer)))
            (declare (type string word))
            (cond
              ((string= word "X")
                (make-token :mark :X))
              ((string= word "O")
                (make-token :mark :O))
              ((square-name-p word)
                (make-token :square word))
              (T
                (make-token :word word)))))
        
        ((char= character #\.)
          (lexer-advance lexer)
          (make-token :dot "."))
        
        ((char= character #\:)
          (lexer-advance lexer)
          (make-token :colon ":"))
        
        (T
          (error "Invalid character ~s at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Abstract syntax tree (AST).                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Node ()
  ((type
    :initarg       :type
    :initform      (error "The node must be initialized with a type.")
    :accessor      node-type
    :type          keyword
    :documentation "The node type.")
   (attributes
    :initarg       :attributes
    :initform      (make-hash-table :test #'eq)
    :type          (hash-table-of keyword T)
    :documentation "Maintains the node attributes as a mapping of
                    keyword-typed names to arbitrary values, obviating
                    in this way the necessity of specialized ``Node''
                    subclasses."))
  (:documentation
    "The ``Node'' class encapsulates all information requisite to
     express a node in an abstract syntax tree (AST)."))

;;; -------------------------------------------------------

(defun make-node (type &rest attribute-key-value-pairs)
  "Creates and returns a new ``Node'' of the TYPE, initializing its
   attributes from the ATTRIBUTE-KEY-VALUE-PAIRS, a flat list the
   consecutive elements of which form key-value twains."
  (declare (type keyword                       type))
  (declare (type (key-value-list-of keyword T) attribute-key-value-pairs))
  (let ((node (make-instance 'node :type type)))
    (declare (type Node node))
    (loop
      for (name value)
        of-type (keyword T)
        on      attribute-key-value-pairs
        by      #'cddr
      do
        (setf (gethash name (slot-value node 'attributes)) value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node name)
  "Returns the NODE attribute value specified by NAME, or ``NIL'' if
   it cannot be retrieved."
  (declare (type Node    node))
  (declare (type keyword name))
  (the T (gethash name (slot-value node 'attributes))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (value node name)
  "Associates the NODE attribute specified by the NAME with the VALUE,
   tacitly overwriting any extant entry with the NAME, and returns the
   modified NODE."
  (declare (type T       value))
  (declare (type Node    node))
  (declare (type keyword name))
  (with-slots (attributes) node
    (declare (type (hash-table-of keyword T) attributes))
    (setf (gethash name attributes) value))
  (the Node node))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node                            node))
  (declare (type (or null (eql T) stream string) stream))
  (with-slots (type attributes) node
    (declare (type keyword                   type))
    (declare (type (hash-table-of keyword T) attributes))
    (format stream "Node(type = ~s" type)
    (maphash
      #'(lambda (name value)
          (declare (type keyword name))
          (declare (type T       value))
          (format stream ", ~a = ~s" name value))
      attributes))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "A parser expects a lexer.")
    :type          Lexer
    :documentation "The lexer as the purveyor of the tokens to compose
                    an abstract syntax tree (AST) from.")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The last token delivered by the LEXER."))
  (:documentation
    "The ``Parser'' class provides an entity for constructing an
     abstract syntax tree (AST) representation of a program from a
     series of tokens, furnished by a ``Lexer''."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Parser          parser))
    (declare (type (or null Token) current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which receives its tokens from
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token is of the
   EXPECTED-TOKEN-TYPE, on confirmation requesting and storing the next
   token from the PARSER's internal lexer and returning the modified
   PARSER, otherwise signaling an error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Parser          parser))
    (declare (type (or null Token) current-token))
    (if (and current-token
             (token-has-type-of current-token expected-token-type))
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected token of type ~s, but encountered token ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-eat-word (parser expected-word)
  "Checks whether the PARSER's current token is of the type ``:WORD'',
   with the value EXPECTED-WORD, on confirmation requesting and storing
   the next token from the PARSER's internal lexer and returning the
   modified PARSER, otherwise signaling an error."
  (declare (type Parser parser))
  (declare (type string expected-word))
  (with-slots (lexer current-token) parser
    (declare (type Parser          parser))
    (declare (type (or null Token) current-token))
    (if (and current-token
             (token-is-word current-token expected-word))
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected token of type :WORD with a value of ~s, ~
              but encountered token ~s."
        expected-word current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-player-declaration (parser)
  "Parses a player declaration using the PARSER.
   ---
   The following grammar underlies the process:
     
     playerDeclaration := 'I', 'will', 'play', 'as', mark , '.';
     mark:             := 'X' | 'O' ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (parser-eat-word parser "I")
    (parser-eat-word parser "will")
    (parser-eat-word parser "play")
    (parser-eat-word parser "as")
    
    (let ((mark (token-value current-token)))
      (declare (type mark mark))
      (parser-eat parser :mark)
      (parser-eat parser :dot)
      
      (the Node (make-node :player-declaration :mark mark)))))

;;; -------------------------------------------------------

(defun parser-parse-game-declaration (parser)
  "Parses a game declaration using the PARSER.
   ---
   The following grammar underlies the process:
     
     gameDeclaration := 'Game' , integer , ':' ;
     integer         := digit , { digit } ;
     digit           := '0' | ... | '9' ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (parser-eat parser :word)
    
    (let ((game (make-node :game-declaration)))
      (declare (type Node game))
      
      ;; Read the game number.
      (let ((round (token-value current-token)))
        (declare (type integer round))
        (parser-eat parser :integer)
        (parser-eat parser :colon)
        (setf (node-attribute game :number) round))
      
      ;; Collect zero or more player moves.
      (loop while current-token do
        (cond
          ((token-has-type-of current-token :mark)
            (push (parser-parse-move parser)
                  (node-attribute game :moves)))
          (T
            (loop-finish))))
      
      (setf (node-attribute game :moves)
            (nreverse (node-attribute game :moves)))
      
      (the Node game))))

;;; -------------------------------------------------------

(defun parser-parse-move (parser)
  "Parses a move declaration using the PARSER.
   ---
   The following grammar underlies the process:
     
     move     := mark , 'went' , 'on' , square , '.' ;
     mark     := 'X' | 'O' ;
     squares  := 'a1' | 'a2' | 'a3'
              |  'b1' | 'b2' | 'b3'
              |  'c1' | 'c2' | 'c3' ;"
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((mark (token-value current-token)))
      (declare (type mark mark))
      (parser-eat      parser :mark)
      (parser-eat-word parser "went")
      (parser-eat-word parser "on")
      
      (let ((square-name (token-value current-token)))
        (declare (type string square-name))
        (parser-eat parser :square)
        (parser-eat parser :dot)
        
        (the Node (make-node :move :mark mark :square square-name))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses a Tic Tac Toe program by aid of the PARSER and returns the
   root node of the thus generated abstract syntax tree (AST)."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    
    (let ((program (make-node :program :statements NIL)))
      (declare (type Node program))
      
      ;; Strive to read the introducing player mark declaration.
      (if (token-is-word current-token "I")
        (setf (node-attribute program :player-declaration)
              (parser-parse-player-declaration parser))
        (error "Expected word 'I', but found ~s." current-token))
      
      ;; Collect zero or more game declarations.
      (loop do
        (cond
          ((null current-token)
            (loop-finish))
          
          ((token-has-type-of current-token :EOF)
            (loop-finish))
          
          ((token-is-word current-token "Game")
            (push (parser-parse-game-declaration parser)
                  (node-attribute program :statements)))
          
          (T
            (error "Unexpected token ~s." current-token))))
      
      (setf (node-attribute program :statements)
            (nreverse (node-attribute program :statements)))
      
      (the Node program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of AST visitor interface.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Visitor ()
  ()
  (:documentation
    "A ``Visitor'' processes an abstract syntax tree (AST) by traversing
     its nodes and exerting some action upon each such."))

;;; -------------------------------------------------------

(defgeneric visitor-visit-node (visitor node)
  (:documentation
    "Ordains the AST VISITOR to process the NODE, the return value, if
     any, dependent upon the concrete circumstances."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter (Visitor)
  ((parser
    :initarg       :parser
    :initform      (error "The interpreter depends upon a parser.")
    :type          Parser
    :documentation "The parser whose abstract syntax tree (AST) shall
                    be processed by the interpreter.")
   (board
    :initarg       :board
    :initform      (make-board)
    :type          Board
    :documentation "The board used in simulating the games.")
   (main-player
    :initarg       :main-player
    :initform      NIL
    :type          (or null mark)
    :documentation "The player's mark: either 'X' or 'O'.")
   (active-player
    :initarg       :active-player
    :initform      :X
    :type          mark
    :documentation "The player whose turn it is: either 'X' or 'O'.")
   (game-number
    :initarg       :game-number
    :initform      0
    :type          (integer 0 *)
    :documentation "The current game round number.")
   (state
    :initarg       :state
    :initform      :player-select
    :type          game-state
    :documentation "The game state, influenced by the processed
                    statements.")
   (commands
    :initarg       :commands
    :initform      (make-array 0
                     :element-type 'character
                     :adjustable   T
                     :fill-pointer 0)
    :type          string
    :documentation "The instructions generated while simulating the
                    tic-tac-toe games."))
  (:documentation
    "The ``Interpreter'' class traverses an abstract syntax tree (AST),
     processing each node, and thus executing the Tic Tac Toe program."))

;;; -------------------------------------------------------

(defun make-interpreter (parser)
  "Creates and returns an ``Interpreter'' which operates on the PARSER's
   abstract syntax tree."
  (declare (type Parser parser))
  (the Interpreter (make-instance 'Interpreter :parser parser)))

;;; -------------------------------------------------------

(defmethod visitor-visit-node ((interpreter Interpreter) node)
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (dispatch-node interpreter (node-type node) node))

;;; -------------------------------------------------------

(defgeneric dispatch-node (visitor node-type node)
  (:documentation
    "Ordains the VISITOR to process the NODE of the specified NODE-TYPE,
     the return value, if any, being dependent upon the concrete
     circumstances.
     ---
     This method's raison d'etre solely resides in its role as an
     adminiculum for the official AST ``Visitor'' function
     ``visitor-visit-node'', appropriating the ``Node'' class'
     ``type'' slot as a discriminating datum for the dispatchment."))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :player-declaration))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (type Node        node))
  (with-slots (main-player state) interpreter
    (declare (type (or null mark) main-player))
    (declare (type game-state     state))
    (cond
      ((not (eq state :player-select))
        (error "Cannot select a player mark in the stage ~s." state))
      (T
        (setf main-player (node-attribute node :mark))
        (setf state       :game-start))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :game-declaration))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (type Node        node))
  (with-slots (state game-number active-player) interpreter
    (declare (type game-state              state))
    (declare (type (or null (integer 0 *)) game-number))
    (declare (type mark                    active-player))
    (let ((expected-game-number (1+ game-number))
          (declared-game-number (node-attribute node :number)))
      (declare (type (integer 1 *) expected-game-number))
      (declare (type (integer 1 *) declared-game-number))
      (cond
        ((not (eq state :game-start))
          (error "Cannot declare a game number at the stage ~s." state))
        
        ((/= declared-game-number expected-game-number)
          (error "Invalid game number: Expected ~d but found ~d."
            expected-game-number declared-game-number))
        
        (T
          (setf game-number   declared-game-number)
          (setf state         :game-on)
          (setf active-player :X)
          
          (dolist (move (node-attribute node :moves))
            (declare (type Node move))
            (visitor-visit-node interpreter move))
          
          ;; Prepare for next game.
          (setf state :game-start)))))
  (values))

;;; -------------------------------------------------------

(defun change-active-player (interpreter)
  "Changes the active player maintained by the INTERPRETER to the next
   player and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (active-player) interpreter
    (declare (type mark active-player))
    (setf active-player
      (case active-player
        (:X        :O)
        (:O        :X)
        (otherwise (error "Invalid active player: ~s." active-player)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :move))
                          (node        Node))
  (declare (type Interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (type Node        node))
  (with-slots (state board main-player active-player game-number commands)
      interpreter
    (declare (type game-state state))
    (declare (type Board      board))
    (declare (type mark       main-player))
    (declare (type mark       active-player))
    (declare (type string     commands))
    (declare (ignorable       commands))
    
    (let ((square      (node-attribute node :square))
          (this-player (node-attribute node :mark)))
      (declare (type string square))
      (declare (ignorable   square))
      (declare (type mark   this-player))
      
      (cond
        ;; No declaration of the type "I will play as X/O." found yet?
        ;; => Cannot attempt a move.
        ((eq state :player-select)
          (error "The player ~s attempted a move, but the program ~
                  expected an 'I will play as X/O.' declaration."
            active-player))
        
        ;; No declaration of the type "Game ...:" found yet?
        ;; => Cannot attempt a move.
        ((eq state :game-start)
          (error "The player ~s attempted a move, but the program ~
                  expected a 'Game ...:' declaration."
            active-player))
        
        ;; The game is won?
        ;; => No more moves possible.
        ((eq state :game-won)
          (error "The game #~d is already won, but ~a has nonetheless ~
                  attemped a move."
            game-number this-player))
        
        ;; Not in the stage to accept moves?
        ;; => Error.
        ((not (eq state :game-on))
          (error "Cannot commit a move in the state ~s." state))
        
        ;; The targeted square is already occupied?
        ;; => Cannot set a mark.
        ((board-is-occupied board square)
          (error "The square ~s is already occupied by ~s, so ~s ~
                  cannot move into it."
            square (board-mark board square) active-player))
        
        ;; It is not the turn of the moving player to attempt a move?
        ;; => Error.
        ((not (eq this-player active-player))
          (error "It was the '~a' player's turn to move, but ~a has ~
                  acted."
            active-player this-player))
        
        ;; All squares of the board are already occupied?
        ;; => No player may commit a move.
        ((board-is-full board)
          (error "All squares of the board are already occupied, ~
                  thus the player ~s cannot attempt a move."
            active-player))
        
        ;; Cases:
        ;;   (1) Board not solved, and non-NOP command.
        ;;   (2) Board not solved, but NOP command.
        ;;   (3) Board solved by main player
        ;;   (4) Board solved by opponent.
        (T
          (let ((bf-command (board-command-at board square)))
            (declare (type (or null character) bf-command))
            
            (setf (board-mark board square) active-player)
            
            (let ((winner (board-is-solved board)))
              (declare (type (or null mark) winner))
              
              (cond
                ;; Board solved by main player.
                ((and winner (eq winner main-player))
                  (setf state :game-won)
                  (board-clear board))
                
                ;; Board solved by opponent.
                ((and winner (not (eq winner main-player)))
                  (setf state :game-won)
                  (board-clear board))
                
                ;; Board not solved, and move by main player.
                ((and (not winner)
                      (eq active-player main-player))
                  ;; Append non-NOP command to the COMMANDS.
                  (when bf-command
                    (vector-push-extend bf-command commands))
                  (change-active-player interpreter))
                
                ;; Board not solved, and move by opponent.
                ((and (not winner)
                      (not (eq active-player main-player)))
                  (change-active-player interpreter))
                
                ;; Board not solved, but NOP command.
                (T
                  (error "Invalid state with winner ~s, active player ~s and moving player ~s."
                    winner active-player this-player)))))))))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter Interpreter)
                          (node-type   (eql :program))
                          (node        Node))
  (visitor-visit-node interpreter
    (node-attribute node :player-declaration))
  
  (dolist (statement (node-attribute node :statements))
    (declare (type Node statement))
    (visitor-visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) contained in the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (parser) interpreter
    (declare (type Parser parser))
    (let ((ast (parser-parse parser)))
      (declare (type Node ast))
      (visitor-visit-node interpreter ast)))
  (values))

;;; -------------------------------------------------------

(defun interpret-brainfuck-commands (commands)
  "Interprets the brainfuck COMMANDS, and returns no value."
  (declare (type (vector character) commands))
  
  (let ((position 0)
        (command  (aref commands 0)))
    (declare (type fixnum              position))
    (declare (type (or null character) command))
    
    (let ((memory  (make-hash-table :test #'eql))
          (pointer 0))
      (declare (type (hash-table-of integer integer) memory))
      (declare (type integer                         pointer))
      
      (flet
          ((advance ()
            "Moves the POSITION one command forward, if possible, and
             updates the current COMMAND."
            (if (< position (1- (length commands)))
              (setf command (aref commands (incf position)))
              (setf command NIL))
            (values))
           
           (recede ()
            "Moves the POSITION one command back, if possible, and
             updates the current COMMAND."
            (if (plusp position)
              (setf command (aref commands (decf position)))
              (setf command NIL))
            (values)))
        
        (loop do
          (case command
            ;; No more commands remaining.
            ((NIL)
              (loop-finish))
            
            ;; Move pointer right.
            (#\>
              (incf pointer)
              (advance))
            
            ;; Move pointer left.
            (#\<
              (decf pointer)
              (advance))
            
            ;; Increment the memory under the pointer.
            (#\+
              (incf (gethash pointer memory 0))
              (advance))
            
            ;; Decrement the memory under the pointer.
            (#\-
              (decf (gethash pointer memory 0))
              (advance))
            
            ;; Output the current cell value.
            (#\.
              (write-char (code-char (gethash pointer memory 0)))
              (advance))
            
            ;; Input a character and its code in the current cell.
            (#\,
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type (or null character) input))
                (clear-input)
                (setf (gethash pointer memory) (char-code input)))
              (advance))
            
            ;; Jump past matching "]" if the current cell value
            ;; equals 0.
            (#\[
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched '['."))
                      (#\]
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (#\[
                        (incf level)
                        (advance))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back after '[' if the current cell value does
            ;; not equal 0.
            (#\]
              (cond
                ((not (zerop (gethash pointer memory 0)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched ']'."))
                      (#\[
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#\]
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            (otherwise
              (error "Invalid command '~c' at index ~d."
                command position)))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Tic-Tac-Toe (code)
  "Interprets the piece of Tic Tac Toe CODE and returns no value."
  (declare (type string code))
  (let ((lexer (make-lexer code)))
    (declare (type Lexer lexer))
    (let ((parser (make-parser lexer)))
      (declare (type Parser parser))
      (let ((interpreter (make-interpreter parser)))
        (declare (type Interpreter interpreter))
        (interpreter-interpret interpreter)
        (interpret-brainfuck-commands
          (slot-value interpreter 'commands)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
;; Tantamount to the brainfuck program
;;   +[]
;; 
(interpret-Tic-Tac-Toe
  "I will play as X.
   
   Game 1:
   X went on a1.
   O went on a3.
   X went on b3.
   O went on b1.
   X went on c1.
   O went on a2.
   X went on b2.")

;;; -------------------------------------------------------

;; Infinite cat program.
;; Tantamount to the brainfuck program
;;   ,[.,]
;; 
(interpret-Tic-Tac-Toe
  "
  I will play as X.

  Game 1:
  X went on c2.
  O went on a1.
  X went on b3.
  O went on b2.
  X went on c3.
  O went on c1.
  X went on a3.

  Game 2:
  X went on c2.
  O went on b2.
  X went on c1.
  O went on b3.
  X went on c3.
  ")
