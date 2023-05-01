;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tablebase", presented by the Esolang user "Lemonz" in the
;; year 2022, and conceived to employ two-dimensional coordinates,
;; composed of a majuscular letter row and a numeric column index, for
;; commands and data.
;; 
;; 
;; Concept
;; =======
;; Tablebase programs utilize a dioristic syntaxis of two-dimensional
;; coordinates for instructions and memory cell locations in order to
;; accompass arithmetic operations, control flow, input/output
;; communication, and memory modifications.
;; 
;; == COORDINATES DESIGNATE COMMANDS AND MEMORY CELLS ==
;; Tablebase applies itself to a kenspeckle mode of expressions'
;; provision, designating commands as well as memory cell addresses by
;; coordinates, such are two-dimensional compositions from a row and
;; a column, the former assuming Latin majuscles, whereas the latter
;; belong to the unsigned positive integer species.
;; 
;; The coordinates "A1" through "G7" are invested with the reservation
;; for commands, concomitant to the remaining identifiers' availability
;; to reference memory cells.
;; 
;; == THE MEMORY IS COMPOSED OF INTEGERS, AMENABLE TO COORDINATES ==
;; The memory's composition proceeds from an infinite extent, each cell
;; designated by a coordinate and assigns the onus as a salvatory to a
;; single integer datum of any sign and magnitude.
;; 
;; 
;; Architecture
;; ============
;; The program memory is supplied in the form of a two-dimensional
;; Cartesian grid of signed integers, amenable to a maximum of 26 row
;; names in the guise of Latin majuscles, and a theoretically infinite
;; tally of positive integer column subscripts, as a corollary
;; abstaining from a quantitative upper margin's imposition in regards
;; of the data storage's capacity.
;; 
;; 
;; Data Types
;; ==========
;; The type system's paravaunt constituent is realized in the signed
;; integer species, with characters and strings occupying an echolon
;; rather puisne in significance.
;; 
;; == INTEGERS ==
;; The principal elements in assignment to a program's data management,
;; integers, not confined regarding sign and magnitude, and endowed with
;; ubiquity in the all instructional aspects.
;; 
;; == CHARACTERS ==
;; The currency of communications, characters, extricated from the ASCII
;; repertoire, engage in the interface betwixt machine and user as input
;; and output tokens; in the former case responding to a request for a
;; character to store in its ASCII code form in the memory; in the
;; latter committing a memory's integer datum to the standard output
;; conduit in its ASCII character aspect.
;; 
;; == STRINGS ==
;; Apportioned the least effective rank, strings only participate in the
;; language by an aefauld command's mediation, "D6", whose prescription
;; hechts a string --- its obtention elusive to further explications ---
;; to be induced into the program memory.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical perspective, Tablebase programs comprise a
;; sequence of coordinate-encoded instructions, with literal decimal
;; integers contributing the sole exception. The tokens' delineation is
;; realized by one or more whitespace characters.
;; 
;; == COORDINATES ==
;; Coordinates are defined as character sequences compact of two
;; segments, the first being a Latin majuscle serving to determine the
;; row index; the second moeity is limned by a positive integer of any
;; magnitude. The immediate adjacency, without intruding sepiment, must
;; ligate these two parcels.
;; 
;; == INTEGER LITERALS ==
;; At certain locations, integer literals in the form of decimal numbers
;; are expected to supply a parameter.
;; 
;; == WHITESPACES ==
;; A requisitum betwixt each two tokens, that is, instructions and
;; parameters, whitespaces may be liberally distributed in the
;; surroundings, their diorism enumerating the space, horizontal tab, as
;; well as the newline character.
;; 
;; == COMMENTS ==
;; Comments are provided in a dedicated command's guise, introduced via
;; the identifier "B5" and finding its conclusion in "B4"
;; 
;; 
;; Instructions
;; ============
;; Tablebase's instruction set tallies 49 members, nevend by the
;; coordinates spanning the rows "A" through "G" and the columns from
;; one (1) to seven (7).
;; 
;; Their competence's perimeter embraces a wide array:
;; 
;;   - arithmetics
;;   - control flow
;;   - input and output.
;; 
;; == OVERVIEW ==
;; The following table shall furnish an apercu treating the 49 available
;; commands.
;; 
;; Please note that entries dependent upon parameters list the same
;; ensconced in a jumelle of curly braces "{" and "}", the same do not
;; contribute to the language's actual grammar and must be substituted,
;; as their contained variables, by valid Tablebase tokens.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   A1 {c}     | Returns the value stored in the cell with the
;;              | coordinate {c}.
;;              | The returned value is utile for commands which accept
;;              | a decimal literal parameter, such as "G4" and "G7".
;;   ..................................................................
;;   A2 {c}     | Prints to the standard output the character whose
;;              | ASCII code equals the cell with the coordinate {c}.
;;   ..................................................................
;;   A3 {c}     | Repeats the controlled part, demarcated by the
;;              | commands "A7" and "B7", the number of times specified
;;              | by the cell with the coordinate {c}.
;;              | The controlled part must immediately succeed the
;;              | parameter {c}.
;;   ..................................................................
;;   A4         | No-operation.
;;   ..................................................................
;;   A5         | Pauses the program's execution until any input is
;;              | committed.
;;   ..................................................................
;;   A6 {c}     | Execute the controlled part, demarcated by the
;;              | commands "A7" and "B7", once if the cell with the
;;              | coordinate {c} does not equal zero (0). Otherwise
;;              | skips the control block.
;;              | The controlled part must immediately succeed the
;;              | parameter {c}.
;;   ..................................................................
;;   A7         | Starts a controlled part, which is terminated by the
;;              | matching "B7" command.
;;   ..................................................................
;;   B1         | Prints to the standard output an arbitrary error
;;              | message.
;;              | The nature, extent and format of the issued message
;;              | are not subject to further specification. The
;;              | condition does not terminate or otherwise influence
;;              | the program flow.
;;   ..................................................................
;;   B2 {c}     | Prints to the standard output the value of the cell
;;              | with the coordinate {c}.
;;   ..................................................................
;;   B3 {c}     | Repeats the controlled part, demarcated by the
;;              | commands "A7" and "B7", while the cell with the
;;              | coordinate {c} does not equal zero (0).
;;              | The controlled part must immediately succeed the
;;              | parameter {c}.
;;   ..................................................................
;;   B4         | Terminates the comment started by the preceding "B5"
;;              | command.
;;   ..................................................................
;;   B5         | Starts a comment which is terminated by the next "B4"
;;              | command.
;;   ..................................................................
;;   B6         | Prints to the standard output the program's source
;;              | code, thus providing a quine.
;;   ..................................................................
;;   B7         | Terminates the controlled part commenced by the
;;              | matching "A7" command.
;;   ..................................................................
;;   C1 {c}     | Queries the user for a signed integer number and
;;              | it in the cell with the coordinate {c}.
;;              | The provision, nature, extent and format of the user
;;              | prompt are not subject to further specification.
;;   ..................................................................
;;   C2 {c}     | Cubes the value of the cell with the coordinate {c}
;;              | and stores the result back into the cell at {c}, that
;;              | is:
;;              |   cells[{c}] <- cells[{c}] * cells[{c}] * cells[{c}]
;;   ..................................................................
;;   C3         | No-operation.
;;   ..................................................................
;;   C4         | No-operation.
;;   ..................................................................
;;   C5 {a} {b} | Divides the value of the cell with the coordinate {a}
;;              | by the value of the cell with the coordinate {b} and
;;              | stores the remainder in the cell at {a}, that is:
;;              |   cells[{a}] <- cells[{a}] modulo cells[{b}]
;;   ..................................................................
;;   C6         | No-operation.
;;   ..................................................................
;;   C7         | Terminates the program immediately.
;;   ..................................................................
;;   D1         | Waits one second before resuming the program.
;;   ..................................................................
;;   D2         | Clears the output.
;;   ..................................................................
;;   D3 {c}     | Queries the user for an ASCII character and stores
;;              | its ASCII code in the cell with the coordinate {c}.
;;              | The provision, nature, extent and format of the user
;;              | prompt are not subject to further specification.
;;   ..................................................................
;;   D4 {c}     | Squares the value of the cell with the coordinate {c}
;;              | and stores the result back into the cell at {c}, that
;;              | is:
;;              |   cells[{c}] <- cells[{c}] * cells[{c}]
;;   ..................................................................
;;   D5         | No-operation.
;;   ..................................................................
;;   D6 {c}     | Queries the user for a string, calculates its ASCII
;;              | codes, concatenates their binary representations, and
;;              | stores the resulting non-negative integer in the cell
;;              | with the coordinate {c}.
;;   ..................................................................
;;   D7         | Suppresses the appending of a newline character
;;              | during the next output command.
;;              | If the "D7" command has already been invoked prior,
;;              | with no intermediate issuing of output, the next
;;              | "D7" invocations remain ineffectuous.
;;   ..................................................................
;;   E1 {a} {b} | Adds to the value of the cell with the coordinate {a}
;;              | the value of the cell with the coordinate {b} and
;;              | stores the sum in the cell at {a}, that is:
;;              |   cells[{a}] <- cells[{a}] + cells[{b}]
;;   ..................................................................
;;   E2 {c}     | Sets the value of the cell with the coordinate {c} to
;;              | zero (0).
;;   ..................................................................
;;   E3 {a} {b} | Divides the value of the cell with the coordinate
;;              | {a} by the value of the cell with the coordinate {b},
;;              | rounds down the quotient to the next smallest
;;              | integer, and stores the result in the cell at {a},
;;              | that is:
;;              |   cells[{a}] <- floor(cells[{a}] / cells[{b}])
;;   ..................................................................
;;   E4 {c}     | Sets the value of the cell with the coordinate {c} to
;;              | positive infinity.
;;   ..................................................................
;;   E5         | No-operation.
;;   ..................................................................
;;   E6         | No-operation.
;;   ..................................................................
;;   E7         | Exports the hitherto issued output to a file.
;;              | The output's nature and extent are not subject to
;;              | further specification.
;;   ..................................................................
;;   F1 {a} {b} | Subtracts the value of the cell with the coordinate
;;              | {b} from the value of the cell with the coordinate
;;              | {a} and stores the difference in the cell at {a},
;;              | that is:
;;              |   cells[{a}] <- cells[{a}] - cells[{b}]
;;   ..................................................................
;;   F2 {a} {b} | Multiplies the value of the cell with the coordinate
;;              | {a} by the value of the cell with the coordinate {b}
;;              | and stores the product in the cell at {a}, that is:
;;              |   cells[{a}] <- cells[{a}] * cells[{b}]
;;   ..................................................................
;;   F3         | No-operation.
;;   ..................................................................
;;   F4         | Opens a calculator.
;;   ..................................................................
;;   F5 {a} {b} | Generates a random number in the closed range with
;;              | the inclusive minimum equal to the cell with the
;;              | coordinate {a} and the inclusive maximum tantamount
;;              | to that cell with the coordinate {b}, and stores the
;;              | result in the cell at {a}, that is:
;;              |   cells[{a}] <- random_in_range({a}, {b})
;;   ..................................................................
;;   F6         | Deletes the program following its execution.
;;   ..................................................................
;;   F7         | Waits a random interval for throttling.
;;              | The employed interval's nature and extent are not
;;              | subject to further specification.
;;   ..................................................................
;;   G1         | Resets every coordinate to the default value zero.
;;   ..................................................................
;;   G2 {t} {s} | Copies the value of the cell with the coordinate {s}
;;              | to cell with the coordinate {t}.
;;   ..................................................................
;;   G3 {x}     | Repeats the next command {x} three (3) times.
;;   ..................................................................
;;   G4 {n}     | Searches the signed decimal integer number {n} in the
;;              | program memory and, if contained, prints to the
;;              | standard output the first matching cell's coordinate.
;;   ..................................................................
;;   G5 {x}     | Executes the next command {x} with a probability of
;;              | 50 percent.
;;   ..................................................................
;;   G6         | Prints to the standard output statistics regarding
;;              | the interpreter.
;;              | The output's nature, circumference and format are not
;;              | subject to further specification.
;;   ..................................................................
;;   G7 {c} {n} | Stores the signed decimal integer number {n} in the
;;              | cell with the coordinate {c}.
;;   ------------------------------------------------------------------
;; 
;; == COMMAND CATEGORIES ==
;; The 49-member command roster follows a rather cavalier forbisen, with
;; a subset seemingly haphazardly assigned to coordinates. An
;; illustration vouchsafed augmented cohesion shall be the following
;; adduction's material, by subsuming the available items into
;; categories, fashioned in an imbricate, not disjunct, manner such that
;; a single entity may engage in an affiliation with multiple groupings.
;; 
;;   ==================================================================
;;   CONDITIONALS AND ITERATIONS
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   A3      | Repeat (loop) times.
;;   ..................................................................
;;   A6      | Execute if not zero.
;;   ..................................................................
;;   B3      | Loop while not zero.
;;   ..................................................................
;;   G3      | Repeat thrice.
;;   ..................................................................
;;   G5      | Execute randomly.
;;   ------------------------------------------------------------------
;;   
;;   ==================================================================
;;   DETERMINISTIC CONTROL FLOW
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   A7      | Start control block.
;;   ..................................................................
;;   B1      | Return error.
;;   ..................................................................
;;   B7      | End control block.
;;   ..................................................................
;;   C7      | Terminate program.
;;   ..................................................................
;;   D1      | Wait one second.
;;   ..................................................................
;;   F6      | Delete program after execution.
;;   ..................................................................
;;   F7      | Wait random interval.
;;   ------------------------------------------------------------------
;;   
;;   ==================================================================
;;   INPUT/OUTPUT
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   A2      | Output ASCII character.
;;   ..................................................................
;;   A5      | Pause until input.
;;   ..................................................................
;;   B1      | Return error.
;;   ..................................................................
;;   B2      | Output number.
;;   ..................................................................
;;   B6      | Print source code.
;;   ..................................................................
;;   C1      | Input number.
;;   ..................................................................
;;   D2      | Clear output.
;;   ..................................................................
;;   D3      | Input ASCII character.
;;   ..................................................................
;;   D6      | Store string.
;;   ..................................................................
;;   D7      | Suppress next linebreak.
;;   ..................................................................
;;   E7      | Export output as a file.
;;   ..................................................................
;;   G4      | Find value in memory and output its coordinate.
;;   ..................................................................
;;   G6      | Return interpreter stats.
;;   ------------------------------------------------------------------
;;   
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   C2      | Cube.
;;   ..................................................................
;;   C5      | Modulo.
;;   ..................................................................
;;   D4      | Square.
;;   ..................................................................
;;   E1      | Addition.
;;   ..................................................................
;;   E2      | Set to zero.
;;   ..................................................................
;;   E3      | Floor.
;;   ..................................................................
;;   E4      | Set to infinity.
;;   ..................................................................
;;   F1      | Subtraction.
;;   ..................................................................
;;   F2      | Multiplication.
;;   ..................................................................
;;   F4      | Open the calculator.
;;   ..................................................................
;;   F5      | Supply random number in range.
;;   ..................................................................
;;   G1      | Reset memory.
;;   ..................................................................
;;   G2      | Copy memory cell.
;;   ..................................................................
;;   G7      | Set to literal decimal number.
;;   ------------------------------------------------------------------
;;   
;;   ==================================================================
;;   META-COMMANDS
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   B4      | End comment.
;;   ..................................................................
;;   B5      | Start comment.
;;   ..................................................................
;;   B6      | Print source code.
;;   ..................................................................
;;   F6      | Delete program after execution.
;;   ..................................................................
;;   G6      | Return interpreter stats.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the minuteness commorant in its explications, the Tablebase
;; protologue is inflicted with a few uncertain aspects; a subset of the
;; same shall be the following points' material.
;; 
;; == HOW ARE STRINGS STORED IN THE MEMORY? ==
;; The command "D6" instructs the storage of a string in the program
;; memory; a twain of predicaments issues from this statement:
;; 
;;   (a) How shall the string be acquired?
;;   (b) How shall the string be stored?
;; 
;; The string's provenance has been deemed to replicate the character
;; acquisition from the command "D3", by querying the user for a line
;; of text.
;; 
;; The persistence aspect of point (b) involves a more peisant
;; imposition, as a string's induction into the memory violates the
;; specification's explicit requisitum in maintaining integer cells.
;; 
;; The contrarient requirements have been reconciled by establishing
;; that the string will be converted into an unsigned integer
;; presentation based upon its ASCII character's codes prior to the
;; transfer into the storage.
;; 
;; 
;; Implementation
;; ==============
;; The complete program's implementation manifests in the language
;; Common Lisp, with a bipartite subsumption into the matters of its
;; actual interpreter on one hand, and the calculator on the other.
;; 
;; == TOKENS AND LEXER ==
;; Both elements participating in the entirety rely on a common
;; foundation for the divering parsers' input provision: a lexer that
;; produces a sequence of tokens.
;; 
;; == INTERPRETER: RECURSIVE DESCENT PARSING ==
;; The Tablebase interpreter employs the very common recursive descent
;; approach for parsing.
;; 
;; == CALCULATOR: PRATT PARSING ==
;; Endowed with utter conspicuousness, the calculator implementation's
;; parser departs from the unadulterated recursive descent principle in
;; order to subscribe to a Pratt parser.
;; 
;; The Pratt parser establishes a warklume with rather little
;; vouchsafement in crebritude. Assigned to the species of
;; operator-precedence parsers, the impelling force resides in the
;; definition of numeric operator precedences in order to assemble
;; expressions from tokens.
;; 
;; This project utilizes the nomenclature presented by Denis Lantsman
;; [lantsman2018prattparsers] for the "Desmos" application.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-04-27
;; 
;; Sources:
;;   [esolang2022tablebase]
;;   The Esolang contributors, "Tablebase", 2022
;;   URL: "https://esolangs.org/wiki/Tablebase"
;;   
;;   [googology2023romanfactorial]
;;   The Googology Wiki contributors, "Roman factorial", 2023
;;   URL: "https://googology.fandom.com/wiki/Roman_factorial"
;;   Notes:
;;     - Describes the Roman factorial in a compendious manner.
;;   
;;   [grand1997javalangref]
;;   Mark Grand, "Java Language Reference", 2nd Edition July 1997,
;;               "Chapter 4.14 Order of Operations"
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   Notes:
;;     - Describes and lists the order of operations established in the
;;       Java programming language.
;;   
;;   [lantsman2018prattparsers]
;;   Denis Lantsman, "How Desmos uses Pratt Parsers", 2018
;;   URL: "https://engineering.desmos.com/articles/pratt-parser/"
;;   Notes:
;;     - Provides a pellucid explanation of the Pratt parser concept.
;;   
;;   [oeis2023romanfactorial]
;;   N. J. A. Sloane, "A159333 - OEIS", in
;;     "The On-Line Encyclopedia of Integer Sequences", 2023
;;   URL: "https://oeis.org/A159333"
;;   Notes:
;;     - Describes the Roman factorial.
;;   
;;   [openuniversity2016usescicalc]
;;   The Open University, "Using a scientific calculator", 2016
;;   URL: "https://www.open.edu/openlearn/mod/oucontent/
;;         view.php?printable=1&id=4256"
;;   Notes:
;;     - Describes the handling and expected functionalities of a
;;       scientific calculator.
;;   
;;   [pratt1973top]
;;   Vaughan R. Pratt, "Top Down Operator Precedence", 1973
;;   URL: "https://daesan.com/wp-content/uploads/2018/05/
;;         top_down_operator_precedence.pdf"
;;   
;;   [stackoverflow2011genrandcl6158990]
;;   The Stack Overflow contributors,
;;     "Generating randoms numbers in a certain range for common lisp",
;;     2011
;;   URL: "https://stackoverflow.com/questions/6158990/
;;         generating-randoms-numbers-in-a-certain-range-for-common-lisp"
;;   Notes:
;;     - Generation of pseudo-random numbers in a closed range in
;;       Common Lisp.
;;   
;;   [weisstein2023romanfactorial]
;;   Eric W. Weisstein, "Roman Factorial."
;;     From MathWorld--A Wolfram Web Resource.
;;     https://mathworld.wolfram.com/RomanFactorial.html 
;;   URL: "https://mathworld.wolfram.com/RomanFactorial.html"
;;   Notes:
;;     - Describes the Roman factorial.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of files.                                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (or pathname stream string) +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "Specifies the directory comprehending the project's Common Lisp
   source files.")

;;; -------------------------------------------------------

(load (merge-pathnames +PROJECT-DIRECTORY+ "calculator.lisp"))
(load (merge-pathnames +PROJECT-DIRECTORY+ "interpreter.lisp"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of package.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :tablebase
  (:use
    :cl
    :tablebase.interpreter))

;;; -------------------------------------------------------

(in-package :tablebase)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program.
(interpret-Tablebase "D3 H5 B3 H5 A7 A2 H5 D3 H5 B7 C7 B5 Translated to brainfuck as ,[.,] B4")

;;; -------------------------------------------------------

;; Quine.
(interpret-Tablebase "B6 C7")

;;; -------------------------------------------------------

;; Print quine and export it to a file.
(interpret-Tablebase "B6 E7 C7")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Tablebase
  "C1 H1
   B3 H1
   A7
     G7 H2 49
     D7
     A2 H2
   B7
   G7 H2 48
   A2 H2
   C7")

;;; -------------------------------------------------------

;; Looping counter: Counts up from inclusive 1 to inclusive 69, printing
;; for each counter state on a separate line asterisks ("*"), the tally
;; of which being a tantamount of the current value, that is:
;; 
;;   ------------------------------------------------------------------
;;   Counter state | Line output
;;   --------------+---------------------------------------------------
;;    1            | *
;;    2            | **
;;    3            | ***
;;   ...           | ...
;;   69            | *********************************************************************
;;   ------------------------------------------------------------------
(interpret-Tablebase
  "G7 H1 1
   G7 I1 1
   G7 X1 1
   G7 P1 69
   
   B5 Repeat until P1 = 0. B4
   B3 P1
   A7
     B5 Repeat until H1 = 0. B4
     B3 H1
     A7
       B5 Set: P2 = 42. B4
       G7 P2 42
       
       D7
       B5 Print P2, that is, '*'. B4
       A2 P2
       
       B5 Subtract: H1 = H1 - I1 = H1 - 1. B4
       F1 H1 I1
     B7
     
     B5 Add: X1 = X1 + I1 = X1 + 1. B4
     E1 X1 I1
     
     B5 Set: H1 = memory[X1]. B4
     G2 H1 X1
     
     B5 Append a linebreak to the just displayed line. B4
     D7
     G7 P3 10
     A2 P3
     
     B5 Subtract: P1 = P1 - I1 = P1 - 1. B4
     F1 P1 I1
   B7")

;;; -------------------------------------------------------

;; Print "Ha Ha Ha " utilizing a counting loop ("A3") with a constant
;; repetition tally.
(interpret-Tablebase
  "G7 H2 72  B5 Store the ASCII code of letter 'H'.         B4
   G7 H3 97  B5 Store the ASCII code of letter 'a'.         B4
   G7 H4 32  B5 Store the ASCII code of the space ' '.      B4
   G7 H1 3   B5 Specify the number of times to repeat 'Ha'. B4
   B5 Repeat thrice: Print 'Ha '. B4
   A3 H1
     A7
       D7
       A2 H2
       D7
       A2 H3
       D7
       A2 H4
     B7")

;;; -------------------------------------------------------

;; Print "Ha Ha Ha " utilizing a counting loop ("A3") and numeric user
;; input to determine the tally of repetitions.
(interpret-Tablebase
  "G7 H2 72  B5 Store the ASCII code of letter 'H'.       B4
   G7 H3 97  B5 Store the ASCII code of letter 'a'.       B4
   G7 H4 32  B5 Store the ASCII code of the space ' '.    B4
   C1 H1     B5 Input the number of times to repeat 'Ha'. B4
   B5 Repeat the desired number of times: Print 'Ha '. B4
   A3 H1
     A7
       D7
       A2 H2
       D7
       A2 H3
       D7
       A2 H4
     B7")

;;; -------------------------------------------------------

;; Generate a sequence of ten (10) random Latin majuscular letters
;; (ASCII code range: [65, 90]).
;; 
;; Memory layout:
;;   memory[H1] = ASCII code of minimum letter "A" (= 65)
;;   memory[H2] = ASCII code of maximum letter "Z" (= 90)
;;   memory[H3] = number of random letters to produce
;;   memory[H4] = current random majuscle generated in the loop
(interpret-Tablebase
  "G7 H1 65
   G7 H2 90
   G7 H3 10
   A3 H3
   A7
     G2 H4 H1    B5 Copy minimum into H4 for receiving random number. B4
     F5 H4 H2    B5 Store random number in H4.                        B4
     D7          B5 Suppress next linebreak.                          B4
     A2 H4       B5 Print random number as ASCII character.           B4
   B7")

;;; -------------------------------------------------------

;; Query the user for two integers, compute the sum, and print it.
(interpret-Tablebase
  "C1 H1
   C1 H2
   E1 H1 H2
   B2 H1")

;;; -------------------------------------------------------

;; Demonstrate all binary arithmetic operations using two numeric
;; inputs.
;; 
;; Memory layout:
;;   memory[H1] = right operand (user input)
;;   memory[H2] = left  operand (user input)
;;   memory[H3] = copy of left operand for addition
;;   memory[H4] = copy of left operand for subtraction
;;   memory[H5] = copy of left operand for multiplication
;;   memory[H6] = copy of left operand for floor division
;;   memory[H7] = copy of left operand for modulo (remainder)
(interpret-Tablebase
  "C1 H2
   C1 H1
   
   B5 Copy left operand for addition. B4
   G2 H3 H2
   E1 H3 H1
   B2 H3
   
   B5 Copy left operand for subtract. B4
   G2 H4 H2
   F1 H4 H1
   B2 H4
   
   B5 Copy left operand for multiplication. B4
   G2 H5 H2
   F2 H5 H1
   B2 H5
   
   B5 Copy left operand for floor division. B4
   G2 H6 H2
   E3 H6 H1
   B2 H6
   
   B5 Copy left operand for modulo (remainder). B4
   G2 H7 H2
   C5 H7 H1
   B2 H7")

;;; -------------------------------------------------------

;; Fibonacci sequence printer: Query the user for the desiderated tally
;; N of Fibonacci numbers F(0) to F(N-1) to generate, and print these.
;; 
;; Memory layout:
;;   memory[H1] = F(0)
;;   memory[H2] = F(1)
;;   memory[H3] = F(N) = F(0) + F(1)
;;   memory[H4] = N    = length of Fibonacci sequence to generate
;;                     = user input
;;   memory[H5] = employed for checking and reducing memory[H4] in
;;                judging whether to print F(0) and F(1)
(interpret-Tablebase
  "
  B5 Set F0 = 0. B4
  G7 H1 0
  B5 Set F1 = 1. B4
  G7 H2 1
  
  B5 Set N to user input. B4
  C1 H4
  
  B5 Use H5 as a subtrahend for printing F0 and/or F1 if N > 0. B4
  G7 H5 1
  B5 If N >= 1, print F0. B4
  A6 H4
  A7
    B5 Print F0. B4
    B2 H1
    F1 H4 H5
  B7
  
  B5 If N >= 2, print F1. B4
  A6 H4
  A7
    B5 Print F1. B4
    B2 H2
    F1 H4 H5
  B7
  
  A3 H4
  A7
    B5 Set F2 = F0 for coming addition. B4
    G2 H3 H1
    
    B5 Set F2 = F0 + F1. B4
    E1 H3 H2
    
    B5 Print F2. B4
    B2 H3
    
    B5 Set F0 = F1. B4
    G2 H1 H2
    B5 Set F1 = F2. B4
    G2 H2 H3
    
    B5
      Set F2 = 0 for demonstrating purposes. This step can actually
      be neglected.
    B4
    E2 H3
  B7
  ")

;;; -------------------------------------------------------

;; Load a Tablebase source file which marks its program for deletion,
;; ere printing its content, and execute it.
(load-Tablebase-file
  (make-pathname
    :device    "C"
    :directory '(:relative "resources")
    :name      "example-file_001_delete-quine"
    :type      "txt"))

;;; -------------------------------------------------------

;; Open the calculator.
(interpret-Tablebase "F4")
