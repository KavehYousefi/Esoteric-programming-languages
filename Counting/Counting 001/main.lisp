;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Counting", invented by the Esolang user "A" and presented
;; on March 18th, 2020, the principle of which involves an implicitly
;; operating perpetual iteration which probes a series of "when" blocks,
;; executing those essayed as eligible by their predicate, while
;; employing an automatically incrementing instruction count, or
;; counter, amenable to the activated blocks' tallies and cycle
;; conclusions, and an aefauld variable, the accumulator, furnished for
;; the programmer's own inquisitions and manipulations.
;; 
;; 
;; Concept
;; =======
;; The Counter programming language's foundry appertains to an infinite
;; program loop, inwith whose confines a series of "when" conditional
;; blocks are probed, executing those rendered eligible through their
;; predicate's satisfaction, while concomitantly incrementing a counter
;; variable for each affirmative instance of docimasy.
;; 
;; == CONDITIONAL "WHEN" BLOCKS CONTRIBUTE THE MAIN FUNCTIONALITY ==
;; A Counter program's chief components are realized in terms of "when"
;; blocks: a condition unit whose head bears a predicate, and whose body
;; is compact of zero or more consequent actions.
;; 
;; The predicate's affirmation impels the complete body's evaluation,
;; in the aliter case, no effect is accompassed by particular unit under
;; perquisition.
;; 
;; The general structure, in a compendious and abstract illustration,
;; renders a tantamount to
;; 
;;   when predicate
;;     action[1]
;;     ...
;;     action[i]
;;     ...
;;     action[N]
;; 
;; == EXPRESSIONS AND INSTRUCTIONS FURNISH THE WARKLUMES ==
;; Both the antecedent and the body statements are homologated an adit
;; to a wide array of unary and binary operations from the bailiwicks of
;; arithmetics and logic. Additionally, the latter block segment may
;; avail itself with a basic string functionality, as well as an input,
;; output, and an unconditional program termination facility.
;; 
;; == "WHEN" BLOCKS ESTABLISH THE ONLY FUNCTIONAL UNITS ==
;; No further code sections may be present beside "when" blocks, which
;; means that "rovering" actions in the code, outside any conditional's
;; periphery, are encumbered with strict interdiction.
;; 
;; == DATA = INTEGERS + STRINGS + VARIABLES ==
;; The Counting language accoutres the developer with integers of any
;; magnitude and sign, as well as string to whom no natural bournes in
;; extent are provided.
;; 
;; The potentials commorant in the literal numeric and textual objects
;; experience an augmentation by the variables' mediation.
;; 
;; == THE ONLY VARIABLES: [ACC]UMULATOR AND INSTRUCTION [C]OU[NT]ER ==
;; Merely a twain of variables entalents a Counter program with the
;; warklumes for dynamic reaction, the accumulator "acc" and the
;; instruction counter, or simply counter, "cnt"; both participants
;; are capacitated to influence the "when" block head as well as the
;; body.
;; 
;; The following apercu shall serves in the essential nortelry's
;; provision, ere an enhanced treatise ensues:
;; 
;;   ------------------------------------------------------------------
;;   Variable | Description
;;   ---------+--------------------------------------------------------
;;   acc      | The "accumulator", a modifiable integer-valued
;;            | variable, initialized with zero (0), which accoutres
;;            | the aefauld available storage.
;;            | Besides its inquisitive access, the accumulator answers
;;            | merely to incrementations by addivie compound
;;            | assignment via the "+=" binary operator, and to inputs
;;            | committed via the standard input conduit.
;;   ..................................................................
;;   cnt      | The "instruction count" or "counter", an automatically
;;            | modified variable, initialized to zero (0), and
;;            | incrementing by one (1) after every successfully
;;            | executed "when" block's procession.
;;            | The counter cannot be altered by the programmer.
;;   ------------------------------------------------------------------
;; 
;; == THE PROGRAM OPERATES IN A PERPETUAL LOOP ==
;; The Counting code's entirety is executed infinitely inside of an
;; implicitly transpiring loop, selecting during each cycle the eligible
;; "when" blocks, executing their body, and incrementing as a
;; consequence the instruction counter. An additional epiphenomenon, the
;; counter is also augmented by the value one (1) immediately at an
;; iteration cycle's conclusion, and ere its iterum commencement.
;; 
;; A pseudocode expression shall impart the operation procedure's
;; concept with augmented formality:
;; 
;;   Given:
;;     whenBlocks --- An ordered series of zero or more "when" blocks,
;;                    composed of exactly one predicate, denoted as
;;                      predicate(whenBlock)
;;                    and an ordered series of zero or more actions
;;                      actions(whenBlock)
;;                    the latter of which comprise its body.
;;   
;;   It holds:
;;     instructionCount <- 0      { "cnt" variable, or counter. }
;;     accumulator      <- 0      { "acc" variable. }
;;     
;;     { The program operates in a tacit perpetual loop. }
;;     repeat do
;;       { Advenient Boolean flag which memorizes whether all "when" }
;;       { blocks have failed, thus incrementing the                 }
;;       { "instructionCount" variable by one for the next cycle.    }
;;       hasMatchedNoWhenBlock <- true
;;       
;;       { Probe all "whenBlocks" for their eligibility. }
;;       for whenBlock in whenBlocks do
;;         if predicate(whenBlock) is satisfied then
;;           execute actions(whenBlock)
;;           
;;           { Update counter variable. }
;;           instructionCount      <- instructionCount + 1
;;           
;;           { Obviate superfluous incrementation at cycle end. }
;;           hasMatchedNoWhenBlock <- false
;;         end if
;;       end for
;;       
;;       { No "when" block has matched? => Increment counter by one }
;;       { in lieu of the failing conditionals.                     }
;;       if hasMatchedNoWhenBlock then
;;         instructionCount <- instructionCount + 1
;;       end if
;;     end repeat
;; 
;; 
;; Architecture
;; ============
;; Counting's simplistic nature of data castaldy disencumbers it from
;; the dependency on a particular architectural concept, resolving
;; merely to the management of a twissel of variables: the instruction
;; count and the accumulator, both scalar integers of any magnitude,
;; the former, however, impounded to be non-negative axis, while the
;; latter's perimeter wists of no inherent marches.
;; 
;; 
;; Data Types
;; ==========
;; The type system inherent to the language bifurcates into the
;; paravaunt species of signed integers and the rather parergal
;; supputation that applies to strings as communicative adminicula.
;; 
;; == INTEGERS ==
;; Counting's numeric compartment, subjected to a general conspectuity,
;; acquires the depth of integer carrying any sign and magnitude.
;; 
;; A subset of the integral range is apportioned to the instruction
;; count or counter, the same does not descend into the negative range,
;; and thus wones in the interval [0, +infinity].
;; 
;; == STRING ==
;; An attenuated competence's dation relates of the string type, content
;; ensconced in double quotation marks ('"'), but unbridled in the
;; matters of its composition's mickleness. Very few operations, in
;; particular string concatenation and repetition, bear the sufficiency
;; to already exhaust these warklumes dedicated to exclusive
;; communication.
;; 
;; 
;; Syntax
;; ======
;; A Counting program, proceeding from a vista upon its syntaxis, limns
;; a series of zero or more conditional "when" blocks, each compact of
;; a predicate in its header and zero or more consequential statements
;; comprising its body.
;; 
;; == INSTRUCTIONS ==
;; A Counting program's paravaunt syntactical components are realized in
;; terms of the "when" block: a conditional unit whose predicate's
;; satisfaction renders it eligible for execution.
;; 
;; Introduced via the "when" keyword, a mandatory singular entry
;; condition is stated, succeeded by at least one newline, if not
;; empighted at the code's desinence and empty, and concluded by a
;; series of zero or more statement lines that comprise its body, each
;; such segregated from its neighbors by one or more linebreaks itself.
;; 
;; The statement definition comprehends the following species:
;; 
;;   - Veridical statements, such as "read" and "out".
;;   - Assignment statements, as the sole member among whom the additive
;;     compound assignment is tallied.
;;   - Non-destructive expressions, which, ultimately, contribute no
;;     causatum.
;; 
;; Both the "when" predicate and its body may harness a wide array of
;; unary and binary expressions for arithmetic, logical, and textual
;; applications.
;; 
;; == WHITESPACES ==
;; Among the whitespace species, the administration of newline
;; characters accounts for an consequential segment of the nomothesia:
;; Such entities demarcate the bournes betwixt "when" block's head,
;; ensconcing the predicate, and its body, as well as acting in the
;; segregation of each consecutive body statement's twissel.
;; 
;; Concretely, the "when" block head must be separated by the block body
;; through one or more linebreaks; siclike, any body statement requires
;; a sepiment from its successor by one or more newlines.
;; 
;; Whereas the apportionment of newlines with an structural agency
;; imposes some stringency in their distribution, the instalment of
;; space characters and horizontal tabs does not participate in any
;; effective manner, and thus may be actuated in concord with one's
;; private deliberations.
;; 
;; == COMMENTS ==
;; With the provision of line comments, mandated to occupy a horizontal
;; spatiality of their own, the supplementation of apostils is
;; capacitated. Such a segment's inchoation is realized by a single
;; percentage symbol "%" and succeeded by an arbitrary text whose
;; conclusion conflates with the dextral margin, either in response to
;; a linebreak or the end of the source (EOF).
;; 
;; Please heed that a comment's appendage to a instruction line empights
;; a thing of impossibility, as the introduction sentinel "%" tholes the
;; potential to be confounded with the eponymous arithmetic remainder
;; operator. As a forbisen of such, the program line
;; 
;;   acc += 5 % 5
;; 
;; is vested with the capacity to both represent a remainder operation
;; 
;;   acc += (5 % 5)
;; 
;; or a very short comment following the assignment, that is, unraveled
;; into its component twain:
;; 
;;   acc += 5
;;   % 5
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) description shall imbue the
;; syntactical analysis with enhanced formality:
;; 
;;   program         := padding
;;                   ,  [ { innerWhenBlock } , [ finalWhenBlock ] ]
;;                   ,  padding
;;                   ;
;;   
;;   innerWhenBlock  := whenBlockHead , newlines
;;                   ,  [ whenBlockBody , newlines ]
;;                   ;
;;   finalWhenBlock  := whenBlockHead
;;                   ,  [ newlines , whenBlockBody ]
;;                   ;
;;   
;;   whenBlockHead   := "when" , expression ;
;;   whenBlockBody   := [ bodyLine , { newlines , bodyLine } ] ;
;;   bodyLine        := statement | comment ;
;;   comment         := "%" , { character - newline } ;
;;   statement       := assignStatement
;;                   |  halt
;;                   |  outStatement
;;                   |  readStatement
;;                   |  expression
;;                   ;
;;   assignStatement := "acc"  , "+=" , expression ;
;;   halt            := "halt" ;
;;   outStatement    := "out"  , expression ;
;;   readStatement   := "read" , "acc" ;
;;   
;;   expression      := integer
;;                   |  string
;;                   |  binaryOperation
;;                   |  unaryOperation
;;                   |  relation
;;                   ;
;;   unaryOperation  := [ "+" | "-" ] , expression ;
;;   binaryOperation := expression , binaryOperator , expression ;
;;   binaryOperator  := "+" | "-" | "*" | "/" | "%" | "^" ;
;;   relation        := expression , relOperator , expression ;
;;   relOperator     := "==" | "!-" | "<" | "<=" | ">" | ">=" ;
;;   
;;   string          := '"' , { character - '"' } , '"' ;
;;   integer         := [ "+" | "-" ] , digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   
;;   padding         := { newline } ;
;;   newlines        := newline , { newline } ;
;;   newline         := "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The Counting programming language's instruction set tallies a mere
;; treble of statements, but offered a very mickle range of expressions
;; desumed from the vales of arithmetics, logic, and string
;; manipulation.
;; 
;; == OVERVIEW ==
;; The available statement commands shall now be intrined into an apercu
;; which pursues to apprize in a cursory manner about their foundational
;; aspects:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   halt           | Immediately terminates the program.
;;   ..................................................................
;;   out expression | Prints the {expression} to the standard output.
;;       ********** |--------------------------------------------------
;;                  | {expression} must be any expression.
;;   ..................................................................
;;   read variable  | Queries the user for a signed or unsigned integer
;;        ********  | number and stores the same in the {variable}.
;;                  |--------------------------------------------------
;;                  | {variable} must be the accumulator, "acc".
;;   ------------------------------------------------------------------
;; 
;; == UNARY OPERATORS ==
;; The set of unary operators --- always specimens of the prefix
;; realm --- does not endeavor a mimicry of the binary compernage's
;; ramosity; imprimis, forecause its operand tally is restricted to an
;; aefauld element; also, as the complete species' exhaustion can be
;; intrined into a twain of signs and the logical NOT operator:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Operand | Effect
;;   ---------+---------+----------------------------------------------
;;   +        | boolean | invalid
;;            |--------------------------------------------------------
;;            | integer | plus sign
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ..................................................................
;;   -        | boolean | invalid
;;            |--------------------------------------------------------
;;            | integer | minus sign
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ..................................................................
;;   !        | boolean | logical NOT
;;            |--------------------------------------------------------
;;            | integer | invalid
;;            |--------------------------------------------------------
;;            | string  | invalid
;;   ------------------------------------------------------------------
;; 
;; == BINARY OPERATORS ==
;; A rather generous nimiety applies to the purview of binary operations
;; in the Counting programming language.
;; 
;; The trinity of its types, in conjunction with these operators, begets
;; a rather intricate circumstance of causatum and invaliditiy, and thus
;; shall be the following elucidating table's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Left op. | Right op. | Effect
;;   ---------+----------+-----------+---------------------------------
;;   +        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | addition
;;            |--------------------------------------------------------
;;            | integer  | string    | string concatenation
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | string concatenation
;;            |--------------------------------------------------------
;;            | string   | string    | string concatenation
;;   ..................................................................
;;   -        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | subtraction
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   *        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | multiplication
;;            |--------------------------------------------------------
;;            | integer  | string    | string repetition
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | string repetition
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   /        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | division
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   %        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | remainder
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ^        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | exponentiation, power
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   &&       | boolean  | boolean   | logical AND
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | invalid
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ||       | boolean  | boolean   | logical OR
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | invalid
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | invalid
;;   ..................................................................
;;   ==       | boolean  | boolean   | equality
;;            |--------------------------------------------------------
;;            | boolean  | integer   | equality
;;            |--------------------------------------------------------
;;            | boolean  | string    | equality
;;            |--------------------------------------------------------
;;            | integer  | boolean   | equality
;;            |--------------------------------------------------------
;;            | integer  | integer   | equality
;;            |--------------------------------------------------------
;;            | integer  | string    | equality
;;            |--------------------------------------------------------
;;            | string   | boolean   | equality
;;            |--------------------------------------------------------
;;            | string   | integer   | equality
;;            |--------------------------------------------------------
;;            | string   | string    | equality
;;   ..................................................................
;;   !=       | boolean  | boolean   | inequality
;;            |--------------------------------------------------------
;;            | boolean  | integer   | inequality
;;            |--------------------------------------------------------
;;            | boolean  | string    | inequality
;;            |--------------------------------------------------------
;;            | integer  | boolean   | inequality
;;            |--------------------------------------------------------
;;            | integer  | integer   | inequality
;;            |--------------------------------------------------------
;;            | integer  | string    | inequality
;;            |--------------------------------------------------------
;;            | string   | boolean   | inequality
;;            |--------------------------------------------------------
;;            | string   | integer   | inequality
;;            |--------------------------------------------------------
;;            | string   | string    | inequality
;;   ..................................................................
;;   <        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | less than
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | less than (lexicographic)
;;   ..................................................................
;;   <=       | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | less than or equal to
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | less than or equal to (lexicog.)
;;   ..................................................................
;;   >        | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | greater than
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | greater than (lexicographic)
;;   ..................................................................
;;   >=       | boolean  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | integer   | invalid
;;            |--------------------------------------------------------
;;            | boolean  | string    | invalid
;;            |--------------------------------------------------------
;;            | integer  | boolean   | invalid
;;            |--------------------------------------------------------
;;            | integer  | integer   | greater than or equal to
;;            |--------------------------------------------------------
;;            | integer  | string    | invalid
;;            |--------------------------------------------------------
;;            | string   | boolean   | invalid
;;            |--------------------------------------------------------
;;            | string   | integer   | invalid
;;            |--------------------------------------------------------
;;            | string   | string    | greater than or equal to (lex.)
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The tortuous nature of the Counting specification's development, its
;; inchoation an ideation's produce on the originator's, "A"'s, user
;; page, and propagated through several renditions, unto its current,
;; contingently incomplete, status, the language's infliction with
;; ambiguity should not be an encheson for mazement. A subset of the
;; most peisant specimens among these shall now be consigned to further
;; disquisitions.
;; 
;; == AT WHICH POINT DOES THE COUNTER INCREMENTATION TRANSPIRE? ==
;; The instruction count, or counter, realizes a variable of automatic
;; and tacit modulation, eloigned from the program's facility to aliter
;; engage in its manipulation, and increments upon each eligible "when"
;; block's execution. The mode of this alteration, however, is inflicted
;; by an antilogy anenst its exact instant of transpiration.
;; 
;; Two conflicting assertions are produced in the protolog:
;; 
;;   (a) In inchoate presentation, the instruction count, addressed
;;       directly by this nomenclature, is claimed to increment
;;       succeeding every executed "when" block --- skipped block being
;;       excluded.
;;   (b) The instruction count, here nevened as the "counter", is
;;       incremented every time ere the "when" block's execution.
;; 
;; A corroboration aliunde, the Counter language's specification ostends
;; a rather wide array of adumbrated statements; hence, it has been
;; adjudged that the instruction count, or counter, shall be incremented
;; immediately in the immediate succession of an activated "when"
;; block's procession.
;; 
;; == WHICH REGULATIONS GOVERN THE ACCUMULATOR'S ALTERATIONS? ==
;; The accumulator's, "acc"'s, potential, as related of by the original
;; specification, is pinioned via a set of rather stringent regulations,
;; communicated ipsissima verba:
;; 
;;   The accumulator, initialized at 0, can only be added by the
;;   (potentially processed) instruction count. The accumulator cannot
;;   be subtracted or assigned by the instruction count, and so on.
;; 
;; While the protolog's authority shall not be assailed with dubitation,
;; several example programs established at the same location circumvent
;; the eidolon, such as the "Fibonacci sequence", to whom the following
;; extract ought to be credited:
;; 
;;   when cnt == acc - cnt
;;     acc += 0 - cnt
;; 
;; The kenspeckle fact wones in the second line, the same, maugre not
;; immdiate in its commitment, effectively reduces the accumulator by
;; the counter's value; a consectary, tantamount in its response, yet
;; infinitely more entalented with candor, would produce:
;; 
;;   when cnt == acc - cnt
;;     acc -= cnt
;; 
;; This begets an inquisition into the concrete operations admissible to
;; the accumulator's modulation.
;; 
;; It has been adjudged, abstaining for further perquisitions into the
;; sensibility of the decreed imperatives, to homologate only the
;; compound assignment with addition, denoted with "+=", for the
;; accumulator's assignment; further species of association, even the
;; simple binding via "=", shall be inflicted with prohibition.
;; 
;; As a fact bearing little to no causatum, most of the compound
;; assignments from the arithmetic realm acquainted to popular
;; programming languages, such as C, Common Lisp and Java, have been
;; prepared in a clandestine manner, but may not be accessed.
;; 
;; Invested with augmented explicitness, the conceivable accumulator
;; assignment operators, in conjunction with their homologation status
;; and implementation state shall be limned aboon. Please heed that
;; their realization in this interpreter is constrained to a parasceve,
;; not a manifestation, which means that, while the responsible lexer
;; and parser components are capacitated with their recognition and
;; *theoretical* assemblage, the actual attempt will result in an error
;; of an unspecified type.
;; 
;;   --------------------------------------------------------------
;;   Assignment operator | Homologated? | Implemented? | Available?
;;   --------------------+--------------+--------------+-----------
;;   =                   | no           | yes          | no
;;   ..............................................................
;;   +=                  | yes          | yes          | yes
;;   ..............................................................
;;   -=                  | no           | yes          | no
;;   ..............................................................
;;   *=                  | no           | yes          | no
;;   ..............................................................
;;   /=                  | no           | yes          | no
;;   ..............................................................
;;   %=                  | no           | yes          | no
;;   --------------------------------------------------------------
;; 
;; == MAY "WHEN" BLOCKS BE NESTED? ==
;; A counterdistinguishment from the "when" blocks' intrinsic
;; regulations' explication, the prime nomothete, the original
;; specification, does not define the contingency or abstinence of
;; nestings in relation with "when" blocks.
;; 
;; It has been adjudged to resort to any nesting's interdiction, as the
;; "when" blocks, providing the central objects of Counting's logical
;; discourse, at any instant occupy a paravaunt role. The commorancy in
;; the topmost echolon, as a corollary, must be replicated in a
;; program's ordonnance.
;; 
;; == MUST THE "WHEN" BLOCK BODY BE INDENTED? ==
;; All forbisen enumerating the protolog's forbisens ostend an
;; equivalency in the "when" blocks' indentation --- albeit the ligating
;; nature of its stringency is neither explicitly stated nor denied.
;; 
;; It has been adjudged to ascertain no imposition of indentations. This
;; instance of nomothesia derives from the fact that the program
;; structure does not seem to be a victim of ambiguous inflictions in
;; the face of their absence. Neither are "when" blocks endowed with the
;; capability of nesting, nor may advenient, "loose", statements be
;; interspersed outside of such conditional confines. Merely linebreaks
;; partake of a significant agency in the source code's conformation.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp utilizes a recursive
;; descent parser, extended and complemented by a Pratt parser component
;; for the processing of expressions, the species of which derives from
;; Douglas Crockford's [crockford2007topdownopprec] augmentation of the
;; basic principles to incorporate statements.
;; 
;; == INTERPRETATION CONSTITUTES A TRIPLE STAGE PROCESS ==
;; The complete interpretation is defined in terms of a graduated
;; processing, separable into three chief tiers:
;; 
;;   (1) A lexer generates from the Counting program string a sequence
;;       of tokens.
;;   (2) The parser queries these tokens and assembles an abstract
;;       syntax tree (AST), the nodes of which represent the language
;;       facilities.
;;   (3) The interpreter traverses the AST and embues it with effect.
;; 
;; == EXPRESSIONS ARE ASSEMBLED VIA PRATT PARSING ==
;; The parser combines aspects of recursive descent and Pratt's
;; solution, with the former apprehending the general process, aided by
;; the latter for the assemblage of expressions. The Pratt component's
;; conventions and notions are derived from Denis
;; Lantsman [lantsman2018prattparsers].
;; 
;; 
;; Appendices
;; ==========
;; Ensuing from the documentation's main subjects, a certain repertoire
;; appertaining to topics harboring significance to a more remote mete
;; remain. This orra material, maugre its paravail status, shall be the
;; following sections' bailiwick.
;; 
;; == APPENDIX A: COUNTER SPECIFICATION CHRONICLE ==
;; The Counting language's specification, from its incipiency on the
;; user page of the Esolang member "A" to the formal establishment's
;; entelechy in most adjacent recency, bewrays a tale of vicissitudes;
;; ensuing from this, a novantique ultimity governs every proposed
;; recency.
;; 
;; The contributions' fasti shall be the following tabular
;; illustration's cynosure, for those parties entalented with interest
;; in the specimen's evolution --- a particular conspectuity remains
;; airted at the conflicts and consiliences as its cynosure.
;; 
;; A certain legend of keys, attached to entries as bullet points,
;; serves in the alternations' emphasis betwixt subsequent renditions of
;; the language, and shall be exposed below, ere the actual listing
;; ensues:
;; 
;;   ------------------------------------------------------------------
;;   Bullet point | Purpose
;;   -------------+----------------------------------------------------
;;   -            | An original list item, with no vinculum to any
;;                | preceding fact, and inherent to the inchoate
;;                | protolog.
;;   ..................................................................
;;   o            | A list item retained unmodified from the previous
;;                | entry.
;;   ..................................................................
;;   ~            | A list item based upon a previous entry, but
;;                | modified to some extent, such as to introduce a
;;                | logical conflict.
;;   ..................................................................
;;   +            | A newly introduced list item not corresponding to
;;                | any information previously present.
;;   ------------------------------------------------------------------
;; 
;; Proceeding from this piece of gnarity, the day-book assumes the
;; following circumference:
;; 
;;   ------------------------------------------------------------------
;;   Date          | Rev. ID | Summary
;;   Time          | Page    | 
;;   --------------+---------+-----------------------------------------
;;   18 March 2020 | 70347   | First notes on the language by user "A".
;;   07:54         | user    | Claims:
;;                 |         | - Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | - Is incremented every time after a
;;                 |         |   line's execution.
;;                 |         | - Instruction count starts at 1.
;;   ..................................................................
;;   18 March 2020 | 70353   | Claims:
;;   08:08         | user    | o Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | o Is incremented every time after a
;;                 |         |   line's execution.
;;                 |         | o Instruction count starts at 1.
;;                 |         | + Accumulator starts at 0.
;;   ..................................................................
;;   18 March 2020 | 70356   | Claims:
;;   08:17         | user    | o Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | o Is incremented every time after a
;;                 |         |   line's execution.
;;                 |         | o Instruction count starts at 1.
;;                 |         | o Accumulator starts at 0.
;;                 |         | + Program executes without stopping,
;;                 |         |   even if no condition is fulfilled.
;;   ..................................................................
;;   18 March 2020 | 70357   | Claims:
;;   08:26         | user    | o Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | ~ Is incremented every time after a
;;                 |         |   line's execution (every non-skipped
;;                 |         |   line).
;;                 |         | o Instruction count starts at 1.
;;                 |         | + "when" lines are not counted.
;;                 |         | o Accumulator starts at 0.
;;                 |         | + Accumulator can be set to input.
;;                 |         | o Program executes without stopping,
;;                 |         |   even if no condition is fulfilled.
;;   ..................................................................
;;   18 March 2020 | 70359   | Claims:
;;   10:17         | user    | o Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | o Is incremented every time after a
;;                 |         |   line's execution (every non-skipped
;;                 |         |   line).
;;                 |         | o Instruction count starts at 1.
;;                 |         | o "when" lines are not counted.
;;                 |         | o Accumulator starts at 0.
;;                 |         | o Accumulator can be set to input.
;;                 |         | o Program executes without stopping,
;;                 |         |   even if no condition is fulfilled.
;;                 |         | + Prepending of question:
;;                 |         |     "So, should the instruction count be
;;                 |         |      about how many "when" blocks it has
;;                 |         |      passed?".
;;   ==================================================================
;;   18 March 2020 | 70361   | First contribution of original author
;;   10:22         | article | "A" to the article page, created by user
;;                 |         | "D" for the language. "A"'s claims:
;;                 |         | o Instruction count increments after
;;                 |         |   every line of instruction (every
;;                 |         |   non-skipped line).
;;                 |         | ~ Is incremented every time before a
;;                 |         |   line's execution.
;;                 |         | + "when" lines are not counted.
;;                 |         | - Instruction count starts at 1.
;;   ==================================================================
;;   18 March 2020 | 70365   | Claims:
;;   14:58         | article | ~ Instruction count increments after
;;                 |         |   every executed "when" block.
;;                 |         | ~ Skipped blocks are not counted.
;;                 |         | ~ Is incremented every time before the
;;                 |         |   execution of the "when" block.
;;                 |         | o Instruction count starts at 1.
;;   ..................................................................
;;   18 March 2020 | 70366   | Last modification of the behavioral
;;   15:00         | article | language aspects. Claims:
;;                 |         | o Instruction count increments after
;;                 |         |   every executed "when" block.
;;                 |         | o Skipped blocks are not counted.
;;                 |         | o Is incremented every time before the
;;                 |         |   execution of the "when" block.
;;                 |         | ~ Instruction count starts at 0.
;;                 |         | + The counter is incremented by 1 even
;;                 |         |   if no conditions are fulfilled.
;;   ------------------------------------------------------------------
;; 
;; == APPENDIX B: OPERATOR BINDING POWERS AND ASSOCIATIVITIES ==
;; This implementation's reliance upon the Pratt parser and its innate
;; components, the binding power and associativity, rede a compendious
;; tendance's adhibition upon the subject.
;; 
;; A listing of the available operators, their binding powers, and
;; associativity configurations shall be ostended in the following
;; tabular illustration:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Binding power | Associativity | Apostil
;;   ---------+---------------+---------------+------------------------
;;   (...)    | 0             | none          | Effectively endowed
;;            |               |               | with the highest
;;            |               |               | binding power.
;;   ..................................................................
;;   =        | 20            | right-to-left | Not used.
;;   ..................................................................
;;   +=       | 20            | right-to-left | 
;;   ..................................................................
;;   -=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   *=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   /=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   %=       | 20            | right-to-left | Not used.
;;   ..................................................................
;;   ||       | 40            | left-to-right | 
;;   ..................................................................
;;   &&       | 50            | left-to-right | 
;;   ..................................................................
;;   ==       | 90            | left-to-right | 
;;   ..................................................................
;;   !=       | 90            | left-to-right | 
;;   ..................................................................
;;   <        | 100           | left-to-right | 
;;   ..................................................................
;;   <=       | 100           | left-to-right | 
;;   ..................................................................
;;   >        | 100           | left-to-right | 
;;   ..................................................................
;;   >=       | 100           | left-to-right | 
;;   ..................................................................
;;   +        | 130           | left-to-right | 
;;   ..................................................................
;;   -        | 130           | left-to-right | 
;;   ..................................................................
;;   *        | 140           | left-to-right | 
;;   ..................................................................
;;   /        | 140           | left-to-right | 
;;   ..................................................................
;;   %        | 140           | left-to-right | 
;;   ..................................................................
;;   ^        | 150           | right-to-left | 
;;   ..................................................................
;;   - (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   + (sign) | 170           | right-to-left | 
;;   ..................................................................
;;   !        | 170           | right-to-left | 
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-13
;; 
;; Sources:
;;   [cppreference2023c++operprec]
;;   The cppreference contributors, "C++ Operator Precedence",
;;     September 10th, 2023
;;   URL: "https://en.cppreference.com/w/cpp/language/
;;         operator_precedence"
;;   Notes:
;;     - Lists the C++ operators in conjunction with their precedence
;;       and associativity attributes.
;;   
;;   [crockford2007topdownopprec]
;;   Douglas Crockford, "Top Down Operator Precedence", 2007-02-21
;;   URL: "http://crockford.com/javascript/tdop/tdop.html"
;;   Notes:
;;     - Describes a Pratt parser implementing a subset of JavaScript,
;;       yclept "Simplified JavaScript".
;;     - Introduces the concept of "statement denotation" ("std") for
;;       incorporating statements into the Pratt parser's homogeneous
;;       expression system.
;;   
;;   [crockford2010tdopparse.js]
;;   Douglas Crockford, "parse.js", 2010-06-26
;;   URL: "http://crockford.com/javascript/tdop/parse.js"
;;   Notes:
;;     - Presents the source code for a Pratt parser implementing a
;;       subset of JavaScript, yclept "Simplified JavaScript".
;;     - The main page, supplying elucidations, can be found under
;;       -> "https://crockford.com/javascript/tdop/tdop.html".
;;   
;;   [esolang2021Counting]
;;   The Esolang contributors, "Counting", November 2nd, 2021
;;   URL: "https://esolangs.org/wiki/Counting"
;;   
;;   [grand1997javalangref]
;;   Mark Grand,
;;     "Java Language Reference", 2nd Edition July 1997,
;;     "Chapter 4.14 Order of Operations",
;;     1997
;;   URL: "http://web.deu.edu.tr/doc/oreily/java/langref/ch04_14.htm"
;;   Notes:
;;     - Describes and lists the order of operations established in the
;;       Java programming language.
;;   
;;   [kumar2016javaprecassoc]
;;   Krishan Kumar, "Java Operators: Precedence and Associativity", 2016
;;   URL: "https://cs-fundamentals.com/java-programming/
;;         java-operators-precedence-and-associativity"
;;   Notes:
;;     - Lists the operator precedences and associativities specified
;;       for the Java programming language.
;;   
;;   [lantsman2018prattparsers]
;;   Denis Lantsman, "How Desmos uses Pratt Parsers", 2018
;;   URL: "https://engineering.desmos.com/articles/pratt-parser/"
;;   Notes:
;;     - Provides a pellucid explanation of the Pratt parser concept.
;;   
;;   [pratt1973top]
;;   Vaughan R. Pratt, "Top Down Operator Precedence", 1973
;;   URL: "https://daesan.com/wp-content/uploads/2018/05/
;;         top_down_operator_precedence.pdf"
;;   
;;   [pythonswfoundation2023operprec]
;;   Python Software Foundation,
;;     "The Python Language Reference",
;;     section 6.17, "Operator precedence",
;;     September 14th, 2023
;;   URL: "https://docs.python.org/3/reference/
;;         expressions.html#operator-precedence"
;;   Notes:
;;     - Lists the operators in Python in conjunction with their
;;       precedences.
;;   
;;   [sedgewick2022operatorprecedence]
;;   Robert Sedgewick, Kevin Wayne,
;;     "Appendix A: Operator Precedence in Java", 2022
;;   URL: "https://introcs.cs.princeton.edu/java/11precedence/"
;;   Notes:
;;     - Operator precedence in Java.
;;   
;;   [stackoverflow2011q2811319]
;;   The Stack Overflow contributors,
;;     "What is the difference between >>> and >> operators in Java?",
;;     2011
;;   URL: "https://stackoverflow.com/questions/2811319/
;;         difference-between-and"
;;   Notes:
;;     - Describes the ">>>" operator in Java, an unsigned right shift.
;;   
;;   [williams2022onrecursivedescent]
;;   URL: "https://chidiwilliams.com/post/
;;         on-recursive-descent-and-pratt-parsing/"
;;   Notes:
;;     - Introduces a "ParseRule" class and a "Precedence" enumerated
;;       type which in conjunction bundle the Pratt parser concepts.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
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

(deftype identifier-table ()
  "The ``identifier-table'' type defines an association of identifier
   names to representative tokens, manifesting as a hash table whose
   string keys map to the former, whereas the latter are implemented by
   ``Token'' instances."
  '(hash-table-of string Token))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines a non-negative integer
   value greater than or equal to zero, but unbounded towards the upper
   extremum."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' composed of zero or more entries, each
   indicator, or key, of which conforms to the INDICATOR-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep indicator indicator-type)
                     (typep value     value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-amp'' type defines a collection composed of zero or
   more abstract syntax tree (AST) node attributes, amenable to access
   via their names, and implemented as a property list, or plist, the
   same stores the attribute names as keyword symbols, affiliated with
   values of the comprehensive type ``T''."
  '(property-list-of keyword T))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized variants of
   operator associativities, employed for the apportionment of
   priorities betwixt equipollent operators."
  '(member :none :left :right))

;;; -------------------------------------------------------

(deftype nud-processor ()
  "The ``nud-processor'' type defines a function responsible for parsing
   a nud token in order to obtain an abstract syntax tree (AST) node
   representation of its expression, usually in the context of a
   ``Nud-Parselet'', identified as a function that consumes the nud
   ``Token'' to parse and a ``Token-Stream'', the latter as a
   contingency for further token acquisitions, and returns a ``Node''
   response."
  '(function (Token Token-Stream) Node))

;;; -------------------------------------------------------

(deftype led-processor ()
  "The ``led-processor'' type defines a function responsible for parsing
   a led token in order to obtain an abstract syntax tree (AST) node
   representation of its expression, usually in the context of a
   ``Led-Parselet'', identified as a function that consumed the left
   expression as a ``Node'', the led ``Token'' as the entity to parse,
   and a ``Token-Stream'', the latter being a contingency for futher
   token acquisitions, and returns a ``Node'' response."
  '(function (Node Token Token-Stream) Node))

;;; -------------------------------------------------------

(deftype std-processor ()
  "The ``std-processor'' type defines a function responsible for parsing
   a std (statement) token in order to obtain an abstract syntax tree
   (AST) node representation of its statement, usually in the context of
   an ``Std-Parselet'', identified as a function that consumes the std
   ``Token'' to parse and a ``Token-Stream'', the latter as a
   contingenty for further token acquisitions, and returns a ``Node''
   response."
  '(function (Token Token-Stream) Node))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list of zero or more
   ``Node'' instances."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized variants of
   binary operators from the arithmetics and logic realm."
  '(member
    :addition
    :subtraction
    :multiplication
    :division
    :remainder
    :addition-assignment
    :subtraction-assignment
    :multiplication-assignment
    :division-assignment
    :remainder-assignment
    :logical-and
    :logical-or
    :equal-to
    :not-equal-to
    :less-than
    :less-than-or-equal-to
    :greater-than
    :greater-than-or-equal-to))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized variants of
   unary prefix operators from the arithmetic and logical realm."
  '(member
    :plus
    :minus
    :logical-not))

;;; -------------------------------------------------------

(deftype unsigned-integer ()
  "The ``unsigned-integer'' type defines a non-negative integer in the
   range [0, +infinity]."
  '(integer 0 *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant object extracted during
   the lexical analyzation of a piece of Counting source code."
  (type   (error "Missing token type.")  :type keyword :read-only T)
  (value  (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized language identifiers with representative
   tokens.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token-type)
        "Associates the identifier NAME with a new token of the
         TOKEN-TYPE and returns no value."
        (declare (type string  name))
        (declare (type keyword token-type))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type name))
        (values)))
  (register-identifier "acc"  :acc)
  (register-identifier "cnt"  :cnt)
  (register-identifier "halt" :halt)
  (register-identifier "out"  :out)
  (register-identifier "read" :read)
  (register-identifier "when" :when)
  (values))

;;; -------------------------------------------------------

(defun get-identifier (name)
  "Returns the token associated with the NAME, or creates and returns a
   new ``:identifier'' token comprehending the probed datum."
  (declare (type string name))
  (the Token
    (or (gethash name +IDENTIFIERS+)
        (make-token :identifier name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal tab
   character, on confirmation returning a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
      (source
       &aux
        (position 0)
        (character
          (when (array-in-bounds-p source position)
            (char source position))))))
  "The ``Lexer'' class occupies the wike of the detection and extraction
   of significant objects from a piece of Counting source code, the same
   are delivered in the form of tokens."
  (source    (error "Missing source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'', and
   ``character'' to eponymous local symbol macros for general access,
   executes the BODY forms, and returns the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type string               source))
         (declare (ignorable                 source))
         (declare (type fixnum               position))
         (declare (ignorable                 position))
         (declare (type (or null character)  character))
         (declare (ignorable                 character))
         ,@body))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Returns the LEXER's current character, and concomitantly advances its
   position cursor to the next character in its source, if possible."
  (declare (type Lexer lexer))
  (the (or null character)
    (with-lexer (lexer)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source
              (incf position))))))))

;;; -------------------------------------------------------

(defun lexer-move-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION and returns
   no value."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (with-lexer (lexer)
    (setf position new-position)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier, and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (get-identifier
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (and character (alpha-char-p character)) do
            (write-char character identifier)
            (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Proceeding from the current position into the LEXER source, reads an
   unsigned integer number and returns an ``:integer'' token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (make-token :integer
        (parse-integer source :start position :end
          (loop
            for     end of-type fixnum from position by 1
            while   (and character (digit-char-p character))
            do      (lexer-advance lexer)
            finally (return end)))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Proceeding from the current position into the LEXER, reads a string,
   ensconced in double quotation marks, and returns a ``:string'' token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (lexer-advance lexer)
      (make-token :string
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop do
            (case character
              ((NIL)
                (error "Unterminated string literal at position ~d."
                  position))
              (#\"
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char character content)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Consumes the LEXER's current character and returns a token
   representation thereof, composed of the TOKEN-TYPE in conjunction
   with the character as its value."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (the Token
    (with-lexer (lexer)
      (prog1
        (make-token token-type character)
        (lexer-advance lexer)))))

;;; -------------------------------------------------------

(defun lexer-character-sequence-follows-p (lexer expected-characters)
  "Determines whether, proceeding from the current position into the
   LEXER's source, the sequence of EXPECTED-CHARACTERS follows, on
   confirmation relocating the position cursor to the location
   immediately succeeding the matching segment in the source while
   returning a ``boolean'' value of ``T'', otherwise returning ``NIL''
   without translating the cursor."
  (declare (type Lexer  lexer))
  (declare (type string expected-characters))
  (the boolean
    (with-lexer (lexer)
      (loop
        with initial-position
          of-type fixnum
          =       position
        for expected-character
          of-type character
          across  expected-characters
        if (or (null character)
               (char/= expected-character character))
          do
            (lexer-move-to lexer initial-position)
            (return NIL)
        else do
          (lexer-advance lexer)
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent spaces and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (values))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end of file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ;; Separators (newline).
        ((char= character #\Newline)
          (lexer-read-symbol lexer :newline))
        
        ;; Atomar tokens (literals, variables, etc.).
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        ((digit-char-p character)
          (lexer-read-number lexer))
        ((char= character #\")
          (lexer-read-string lexer))
        
        ;; Relational operators.
        ((lexer-character-sequence-follows-p lexer "==")
          (make-token :equal-to "=="))
        ((lexer-character-sequence-follows-p lexer "<=")
          (make-token :less-than-or-equal-to "<="))
        ((char= character #\<)
          (lexer-read-symbol lexer :less-than))
        ((lexer-character-sequence-follows-p lexer ">=")
          (make-token :greater-than-or-equal-to "!="))
        ((char= character #\>)
          (lexer-read-symbol lexer :greater-than))
        
        ;; Assignment with binary operators.
        ((lexer-character-sequence-follows-p lexer "+=")
          (make-token :addition-assignment "+="))
        ((lexer-character-sequence-follows-p lexer "-=")
          (make-token :subtraction-assignment "-="))
        ((lexer-character-sequence-follows-p lexer "*=")
          (make-token :multiplication-assignment "*="))
        ((lexer-character-sequence-follows-p lexer "/=")
          (make-token :division-assignment "/="))
        ((lexer-character-sequence-follows-p lexer "%=")
          (make-token :remainder-assignment "%="))
        
        ;; Binary arithmetic operators.
        ((char= character #\+)
          (lexer-read-symbol lexer :addition))
        ((char= character #\-)
          (lexer-read-symbol lexer :subtraction))
        ((char= character #\*)
          (lexer-read-symbol lexer :multiplication))
        ((char= character #\/)
          (lexer-read-symbol lexer :division))
        ((char= character #\%)
          (lexer-read-symbol lexer :remainder))
        ((char= character #\^)
          (lexer-read-symbol lexer :power))
        
        ;; Binary logical operators.
        ((lexer-character-sequence-follows-p lexer "&&")
          (make-token :logical-and "&&"))
        ((lexer-character-sequence-follows-p lexer "||")
          (make-token :logical-or "||"))
        ((char= character #\!)
          (lexer-read-symbol lexer :logical-not))
        
        ;; Parentheses.
        ((char= character #\()
          (lexer-read-symbol lexer :left-parenthesis))
        ((char= character #\))
          (lexer-read-symbol lexer :right-parenthesis))
        
        ;; Miscellaneous characters.
        (T
          (lexer-read-symbol lexer :character))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Stream".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token-Stream
  (:constructor make-token-stream
    (lexer
     &aux (current-token (lexer-get-next-token lexer)))))
  "The ``Token-Stream'' class offers an abstract view on the lexer's
   token provision facilities, augmented by an instance of
   supererogation in the contingency for looking ahead, or peeking, the
   subsequent token without its consumption."
  (lexer         (error "Missing token stream lexer.")
                 :type      Lexer
                 :read-only T)
  (current-token (make-token :eof NIL)
                 :type      Token
                 :read-only NIL))

;;; -------------------------------------------------------

(defun token-stream-peek (token-stream)
  "Returns, without consuming, the next token from the TOKEN-STREAM."
  (declare (type Token-Stream token-stream))
  (the Token
    (token-stream-current-token token-stream)))

;;; -------------------------------------------------------

(defun token-stream-consume (token-stream)
  "Returns the current token from the TOKEN-STREAM, while concomitantly
   consuming and replacing it with the successor from the internally
   managed lexer."
  (declare (type Token-Stream token-stream))
  (the Token
    (prog1
      (token-stream-current-token token-stream)
      (setf (token-stream-current-token token-stream)
        (lexer-get-next-token
          (token-stream-lexer token-stream))))))

;;; -------------------------------------------------------

(defun token-stream-matches-p (token-stream expected-token-type)
  "Determines whether the current token in the TOKEN-STREAM conforms to
   the EXPECTED-TOKEN-TYPE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Token-Stream token-stream))
  (declare (type keyword      expected-token-type))
  (the boolean
    (token-type-p
      (token-stream-current-token token-stream)
      expected-token-type)))

;;; -------------------------------------------------------

(defun token-stream-expect (token-stream expected-token-type)
  "Determines whether the current token in the TOKEN-STREAM conforms to
   the EXPECTED-TOKEN-TYPE, on confirmation consuming and returning the
   same, otherwise responding with an error of an unspecified type."
  (declare (type Token-Stream token-stream))
  (declare (type keyword      expected-token-type))
  (the Token
    (if (token-type-p
          (token-stream-current-token token-stream)
          expected-token-type)
      (token-stream-consume token-stream)
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (token-stream-current-token token-stream)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node)
  "The ``Node'' interface furnishes a common foundry for all classes
   aspiring to represent Counter language facilities as abstract syntax
   tree (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Binary-Operation-Node
  (:include Node))
  "The ``Binary-Operation-Node'' class provides a manifestation of a
   binary operation, commorant in the realms of arithmetics or logic,
   composed of an operator and its operand twain, and furnished as an
   abstract syntax tree (AST) node."
  (operator      (error "Missing operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Compound-Assignment-Node
  (:include Node))
  "The ``Compound-Assignment-Node'' class provides a manifestation of an
   assignment operating in champarty with a binary operation, commorant
   in the realms of arithmetics, and composed of an operator and its
   operand twain, and furnished as an abstract syntax tree (AST) node."
  (operator      (error "Missing operator.")
                 :type      binary-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      Node
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      Node
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Group-Node
  (:include Node))
  "The ``Group-Node'' class designs a parenthesized expression as a
   composite of coherent elements, molded into an abstract syntax tree
   (AST) node."
  (expression (error "Missing expression.") :type Node :read-only T))

;;; -------------------------------------------------------

(defstruct (Halt-Node
  (:include Node))
  "The ``Halt-Node'' class' wike conforms to the realization of a
   \"halt\" statement in abstract syntax tree (AST) node form.")

;;; -------------------------------------------------------

(defstruct (Identifier-Node
  (:include Node))
  "The ``Identifier-Node'' class serves as an encapsulation of an
   identifier or variable, addressed by its designating name."
  (name (error "Missing name.") :type keyword :read-only T))

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include Node))
  "The ``Nude-Node'' class serves as an integer literal's encapsulation
   in an abstract syntax tree (AST) node."
  (value (error "Missing value.") :type unsigned-integer :read-only T))

;;; -------------------------------------------------------

(defstruct (Out-Node
  (:include Node))
  "The ``Out-Node'' class comprehends the abstract syntax tree (AST)
   node representation of a print statement (\"out\")."
  (argument (error "Missing argument.") :type Node :read-only T))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include Node))
  "The ``Program-Node'' class contributes an abstract syntax tree (AST)
   node representation of the complete parsed Counting program."
  (statements NIL :type node-list :read-only T))

;;; -------------------------------------------------------

(defstruct (Read-Node
  (:include Node))
  "The ``Read-Node'' class provides an abstract syntax tree (AST) node
   representation of a \"read\" statement."
  (argument (error "Missing argument.") :type Node :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Node
  (:include Node))
  "The ``String-Node'' class serves as a string literal's encapsulation
   in an abstract syntax tree (AST) node."
  (value (error "Missing value.") :type string :read-only T))

;;; -------------------------------------------------------

(defstruct (Unary-Operation-Node
  (:include Node))
  "The ``Unary-Operation-Node'' class' onus resolves to the
   encapsulation of a unary prefix operation desumed from the vale of
   arithmetics."
  (operator (error "Missing operator.")
            :type      unary-operator
            :read-only T)
  (operand  (error "Missing operand.")
            :type      Node
            :read-only T))

;;; -------------------------------------------------------

(defstruct (When-Block-Node
  (:include Node))
  "The ``When-Block-Node'' class forms an abstract syntax tree (AST)
   node representation of a \"when\" block, the same amplects an
   antecedent necessary for its activation, as well as a sequence of
   zero or more statements or expressions."
  (condition  (error "Missing condition.")
              :type      Node
              :read-only T)
  (statements NIL
              :type      node-list
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Precedence".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence (binding-power associativity)))
  "The ``Precedence'' class serves in the representation of a token's
   precedence information, that is, a coefficiency of the binding power
   and associativity properties."
  (binding-power 0     :type integer       :read-only T)
  (associativity :none :type associativity :read-only T))

;;; -------------------------------------------------------

(defun make-dummy-precedence ()
  "Creates and returns a ``Precedence'' object with a zero-valued
   binding power and no associativity."
  (the Precedence
    (make-precedence 0 :none)))

;;; -------------------------------------------------------

(defun precedence-get-effective-binding-power (precedence)
  "Returns for the PRECEDENCE the effective binding power, capacitated
   to contingently modulate the actual binding power in order to
   accommodate for the associativity's influence."
  (declare (type Precedence precedence))
  (the integer
    (case (precedence-associativity precedence)
      ((:none :left)
        (precedence-binding-power precedence))
      (:right
        (1- (precedence-binding-power precedence)))
      (otherwise
        (error "Unrecognized associativity in ~s."
          precedence)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Parselet
  "The ``Parselet'' interface establishes a common foundry for all
   classes intent on parsing a token, or series thereof, to yield an
   abstract syntax tree (AST) node representation.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Nud-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Nud-Parselet
  (:include Parselet))
  "The ``Nud-Parselet'' class establishes a parselet dedicated to the
   evaluation of a nud token, that is, one independent of the preceding
   expressions."
  (precedence (make-dummy-precedence)
              :type      precedence
              :read-only T)
  (processor  (error "Missing nud processor.")
              :type      nud-processor
              :read-only T))

;;; -------------------------------------------------------

(defun nud-parselet-parse (nud-parselet nud-token token-stream)
  "Invokes the NUD-PARSELET's processor in order to parse the NUD-TOKEN,
   contingently accessing the TOKEN-STREAM for additional data
   appropriation, and returns a node representation of the parse
   result."
  (declare (type Nud-Parselet nud-parselet))
  (declare (type Token        nud-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (nud-parselet-processor nud-parselet)
      nud-token token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Led-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Led-Parselet
  (:include Parselet))
  "The ``Led-Parselet'' class establishes a parselet intended for the
   evaluation of a led token, that is, one relying on the preceding
   expression as its sinistral input."
  (precedence (make-dummy-precedence)
              :type      Precedence
              :read-only T)
  (processor  (error "Missing led processor.")
              :type      led-processor
              :read-only T))

;;; -------------------------------------------------------

(defun led-parselet-parse (led-parselet
                           left-expression
                           led-token
                           token-stream)
  "Applies the LED-PARSELET to the LED-TOKEN, consuming the
   LEFT-EXPRESSION as its sinistral argument, in conjunction with the
   TOKEN-STREAM for contingent additional data appropriation, and
   returns a node representation of the parse result."
  (declare (type Led-Parselet led-parselet))
  (declare (type Node         left-expression))
  (declare (type Token        led-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (led-parselet-processor led-parselet)
      left-expression
      led-token
      token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Std-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Std-Parselet
  (:include Parselet))
  "The ``Std-Parselet'' class establishes a parselet dedicated to the
   evaluation of an std token, that is, a token representative of a
   statement, as counterdistinguished from expressions that affiliate
   with the nud and led species."
  (processor (error "Missing std processor.")
             :type      std-processor
             :read-only T))

;;; -------------------------------------------------------

(defun std-parselet-parse (std-parselet
                           std-token
                           token-stream)
  "Invokes the STD-PARSELET's processor in order to parse the STD-TOKEN,
   contingently accessing the TOKEN-STREAM for additional data
   appropriation, and returns a node representation of the parse
   result."
  (declare (type Std-Parselet std-parselet))
  (declare (type Token        std-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (std-parselet-processor std-parselet)
      std-token
      token-stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parselet registries.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (integer Token-Stream) Node)
                parse-expression))

;;; -------------------------------------------------------

(declaim (type (hash-table-of keyword Nud-Parselet) +NUD-PARSELETS+))
(declaim (type (hash-table-of keyword Led-Parselet) +LED-PARSELETS+))
(declaim (type (hash-table-of keyword Std-Parselet) +STD-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +NUD-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized nud token types with ``Nud-Parselet''s.")

(defparameter +LED-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized led token types with ``Led-Parselets''s.")

(defparameter +STD-PARSELETS+
  (make-hash-table :test #'eq)
  "Associates the recognized std (statement) token types with
  ``Std-Parselets''.")

;;; -------------------------------------------------------

(defun nud-token-p (token)
  "Determines whether the TOKEN represents a nud token, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +NUD-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-nud-parselet (token)
  "Returns the ``Nud-Parselet'' allied with the nud TOKEN, or signals an
   error of an unspecified type if no such correspondence can be
   attested."
  (declare (type Token token))
  (the Nud-Parselet
    (or (gethash (token-type token) +NUD-PARSELETS+)
        (error "No nud token: ~s." token))))

;;; -------------------------------------------------------

(defun get-nud-binding-power (token)
  "Returns the effective binding power associated with the nud TOKEN, or
   signals an error of an unspecified type if no such correspondence can
   be attested."
  (declare (type Token token))
  (the integer
    (precedence-get-effective-binding-power
      (nud-parselet-precedence
        (get-nud-parselet token)))))

;;; -------------------------------------------------------

(defun parse-nud-token (nud-token token-stream)
  "Parses the NUD-TOKEN using the TOKEN-STREAM for contingent additional
   token appropriation, and returns a node representation of the parse
   result."
  (declare (type Token        nud-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (nud-parselet-processor
        (get-nud-parselet nud-token))
      nud-token
      token-stream)))

;;; -------------------------------------------------------

(defun register-nud-token (token-type nud-parselet)
  "Associates the TOKEN-TYPE with the NUD-PARSELET, overwriting any
   already extant affiliations for the former if necessary, and returns
   no value."
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet nud-parselet))
  (setf (gethash token-type +NUD-PARSELETS+) nud-parselet)
  (values))

;;; -------------------------------------------------------

(defun register-operand (token-type node-type)
  "Registers a ``Nud-Parselet'', which upon invocation generates a new
   node of the NODE-TYPE, representing an operand destitute of any
   arguments itself, associates the parselet with the TOKEN-TYPE, and
   returns no value."
  (declare (type keyword token-type))
  (declare (type keyword node-type))
  (register-nud-token token-type
    (make-nud-parselet
      :processor
        #'(lambda (token token-stream)
            (declare (type Token        token))          
            (declare (type Token-Stream token-stream))
            (declare (ignore            token-stream))
            (the Node
              (case node-type
                (:integer
                  (make-number-node :value (token-value token)))
                (:string
                  (make-string-node :value (token-value token)))
                ((:cnt :acc)
                  (make-identifier-node :name (token-type token)))
                (otherwise
                  (error "Unrecognized operand type: ~s."
                    node-type))))))))

;;; -------------------------------------------------------

(defun register-prefix-operation (token-type
                                  operator
                                  binding-power
                                  associativity)
  "Registers a ``Nud-Parselet'', which upon invocation generates a new
   ``Unary-Operation-Node'' utilizing the OPERATOR, the precedence of
   which is defined by the BINDING-POWER and ASSOCIATIVITY's champarty,
   associates the parselet with the TOKEN-TYPE, and returns no value.
   ---
   This operation accoutres a convenience warklume based upon the more
   general, and more potent, function ``register-nud-token'', the same
   please see."
  (declare (type keyword       token-type))
  (declare (type keyword       operator))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-nud-token token-type
    (make-nud-parselet
      :precedence
        (make-precedence binding-power associativity)
      :processor
        #'(lambda (token token-stream)
            (declare (type Token        token))
            (declare (type Token-Stream token-stream))
            (the Unary-Operation-Node
              (make-unary-operation-node
                :operator operator
                :operand
                  (parse-expression
                    (get-nud-binding-power token)
                    token-stream)))))))

;;; -------------------------------------------------------

(defun led-token-p (token)
  "Determines whether the TOKEN represents a led token, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +LED-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-led-parselet (token)
  "Returns the ``Led-Parselet'' affiliated with the led TOKEN, or
   signals an error of an unspecified type if no such correspondence can
   be attested."
  (declare (type Token token))
  (the Led-Parselet
    (or (gethash (token-type token) +LED-PARSELETS+)
        (error "No led token: ~s." token))))

;;; -------------------------------------------------------

(defun get-led-binding-power (token)
  "Returns the effective binding power associated with the led TOKEN, or
   signals an error of an unspecified type if no such correspondence can
   be attested."
  (declare (type Token token))
  (the integer
    (precedence-get-effective-binding-power
      (led-parselet-precedence
        (get-led-parselet token)))))

;;; -------------------------------------------------------

(defun parse-led-token (left-expression led-token token-stream)
  "Parses the LED-TOKEN employing the LEFT-EXPRESSION as its sinistral
   argument, in conjunction with the TOKEN-STREAM for the contingent
   appropriation of further tokens, and returns a node representation of
   the parse result."
  (declare (type Node         left-expression))
  (declare (type Token        led-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (led-parselet-processor
        (get-led-parselet led-token))
      left-expression
      led-token
      token-stream)))

;;; -------------------------------------------------------

(defun register-led-token (token-type led-parselet)
  "Associates the TOKEN-TYPE with the LED-PARSELET, overwriting any
   already extant entry for the former, and returns no value."
  (declare (type keyword      token-type))
  (declare (type Led-Parselet led-parselet))
  (setf (gethash token-type +LED-PARSELETS+) led-parselet)
  (values))

;;; -------------------------------------------------------

(defun register-compound-assignment (token-type
                                     operator
                                     binding-power
                                     associativity)
  "Registers a ``Led-Parselet'', which upon invocation generates a new
   ``Compound-Assignment-Node'' utilizing the OPERATOR, the precedence
   of which is defined by the BINDING-POWER and ASSOCIATIVITY's
   champarty, associates the parselet with the TOKEN-TYPE, and returns
   no value.
   ---
   This operation accoutres a convenience warklume based upon the more
   general, and more potent, function ``register-led-token'', the same
   please see."
  (declare (type keyword       token-type))
  (declare (type keyword       operator))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-led-token token-type
    (make-led-parselet
      :precedence
        (make-precedence binding-power associativity)
      :processor
        #'(lambda (left-expression token token-stream)
            (declare (type Node         left-expression))
            (declare (type Token        token))
            (declare (type Token-Stream token-stream))
            (the Compound-Assignment-Node
              (make-compound-assignment-node
                :operator      operator
                :left-operand  left-expression
                :right-operand
                  (parse-expression
                    (get-led-binding-power token)
                    token-stream)))))))

;;; -------------------------------------------------------

(defun register-binary-operation (token-type
                                  operator
                                  binding-power
                                  associativity)
  "Registers a ``Led-Parselet'', which upon invocation generates a new
   ``Binary-Operation-Node'' utilizing the OPERATOR, the precedence of
   which is defined by the BINDING-POWER and ASSOCIATIVITY's champarty,
   associates the parselet with the TOKEN-TYPE, and returns no value.
   ---
   This operation accoutres a convenience warklume based upon the more
   general, and more potent, function ``register-led-token'', the same
   please see."
  (declare (type keyword       token-type))
  (declare (type keyword       operator))
  (declare (type integer       binding-power))
  (declare (type associativity associativity))
  (register-led-token token-type
    (make-led-parselet
      :precedence
        (make-precedence binding-power associativity)
      :processor
        #'(lambda (left-expression token token-stream)
            (declare (type Node         left-expression))
            (declare (type Token        token))
            (declare (type Token-Stream token-stream))
            (the Binary-Operation-Node
              (make-binary-operation-node
                :operator      operator
                :left-operand  left-expression
                :right-operand
                  (parse-expression
                    (get-led-binding-power token)
                    token-stream)))))))

;;; -------------------------------------------------------

(defun std-token-p (token)
  "Determines whether the TOKEN represents an std token, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +STD-PARSELETS+))))))

;;; -------------------------------------------------------

(defun get-std-parselet (token)
  "Returns the ``Std-Parselet'' allied with the std TOKEN, or signals an
   error of an unspecified type if no such correspondence can be
   attested."
  (declare (type Token token))
  (the Std-Parselet
    (or (gethash (token-type token) +STD-PARSELETS+)
        (error "No std token: ~s." token))))

;;; -------------------------------------------------------

(defun parse-std-token (std-token token-stream)
  "Parses the STD-TOKEN using the TOKEN-STREAM for contingent additional
   token appropriation, and returns a node representation of the parse
   result."
  (declare (type Token        std-token))
  (declare (type Token-Stream token-stream))
  (the Node
    (funcall
      (std-parselet-processor
        (get-std-parselet std-token))
      std-token
      token-stream)))

;;; -------------------------------------------------------

(defun register-std-token (token-type std-parselet)
  "Associates the TOKEN-TYPE with the STD-PARSELET, overwriting any
   already extant affiliations for the former if necessary, and returns
   no value."
  (declare (type keyword      token-type))
  (declare (type Std-Parselet std-parselet))
  (setf (gethash token-type +STD-PARSELETS+) std-parselet)
  (values))

;;; -------------------------------------------------------

(defun register-statement (token-type node-constructor)
  "Associates the TOKEN-TYPE with a new ``Std-Parselet'', upon
   invocation producing a new node by mediation of the NODE-CONSTRUCTOR,
   with an attribute of the name ``:argument'' which corresponds with
   the subsequently parsed expression."
  (declare (type keyword  token-type))
  (declare (type function node-constructor))
  (register-std-token token-type
    (make-std-parselet :processor
      #'(lambda (std-token token-stream)
          (declare (type Token        std-token))
          (declare (ignore            std-token))
          (declare (type Token-Stream token-stream))
          (the Node
            (funcall node-constructor
              :argument (parse-expression 0 token-stream)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression parser.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (current-binding-power token-stream)
  "Parses the subsequent expression with a priority specified by the
   CURRENT-BINDING-POWER, utilizing the TOKEN-STREAM for the tokens'
   acquisition, and returns a ``Node'' representation of the parsed
   expression."
  (declare (type integer      current-binding-power))
  (declare (type Token-Stream token-stream))
  (let ((nud-token       (token-stream-consume token-stream))
        (left-expression NIL))
    (declare (type Token          nud-token))
    (declare (type (or null Node) left-expression))
    ;; Parse the nud (or initial) token into a node.
    (setf left-expression
      (parse-nud-token nud-token token-stream))
    ;; Parse the led (or consequent) token(s) into one or more nodes.
    (loop
      for next-token of-type Token = (token-stream-peek token-stream)
      
      if (not (led-token-p next-token)) do
        (loop-finish)
      else if (<= (get-led-binding-power next-token)
                  current-binding-power) do
        (loop-finish)
      else do
        (token-stream-consume token-stream)
        (setf left-expression
          (parse-led-token left-expression next-token token-stream)))
    (the Node left-expression)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Registration of parselets.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Literals.
(register-operand :integer :integer)
(register-operand :string  :string)

;; Identifiers and variables.
(register-operand :cnt :cnt)
(register-operand :acc :acc)

;;; -------------------------------------------------------

;; Unary operators.
(register-prefix-operation :addition    :plus        170 :right)
(register-prefix-operation :subtraction :minus       170 :right)
(register-prefix-operation :logical-not :logical-not 170 :right)

;;; -------------------------------------------------------

(register-nud-token :left-parenthesis
  (make-nud-parselet
    :precedence (make-dummy-precedence)
    :processor
      #'(lambda (nud-token tokens)
          (declare (type Token        nud-token))
          (declare (ignore            nud-token))
          (declare (type Token-Stream tokens))
          (the Node
            (prog1
              (make-group-node :expression
                (parse-expression 0 tokens))
              (token-stream-expect tokens :right-parenthesis))))))

;;; -------------------------------------------------------

;; Arithmetics with concomitant assignment.
(register-compound-assignment
  :addition-assignment
  :addition 20 :right)
(register-compound-assignment
  :subtraction-assignment
  :subtraction 20 :right)
(register-compound-assignment
  :multiplication-assignment
  :multiplication 20 :right)
(register-compound-assignment
  :division-assignment
  :division 20 :right)
(register-compound-assignment
  :remainder-assignment
  :remainder 20 :right)

;; Binary logical operators.
(register-binary-operation :logical-or  :logical-or  40 :left)
(register-binary-operation :logical-and :logical-and 50 :left)

;; Equality and inequality.
(register-binary-operation :equal-to     :equal-to     90 :left)
(register-binary-operation :not-equal-to :not-equal-to 90 :left)

;; Relational operators.
(register-binary-operation
  :less-than
  :less-than 100 :left)
(register-binary-operation
  :less-than-or-equal-to
  :less-than-or-equal-to 100 :left)
(register-binary-operation
  :greater-than
  :greater-than 100 :left)
(register-binary-operation
  :greater-than-or-equal-to
  :greater-than-or-equal-to 100 :left)

;; Binary arithmetic operators.
(register-binary-operation :addition       :addition       130 :left)
(register-binary-operation :subtraction    :subtraction    130 :left)
(register-binary-operation :multiplication :multiplication 140 :left)
(register-binary-operation :division       :division       140 :left)
(register-binary-operation :remainder      :remainder      140 :left)
(register-binary-operation :power          :power          150 :right)

;;; -------------------------------------------------------

(register-statement :out  #'make-out-node)
(register-statement :read #'make-read-node)

(register-std-token :halt
  (make-std-parselet :processor
    #'(lambda (std-token tokens)
        (declare (type Token        std-token))
        (declare (ignore            std-token))
        (declare (type Token-Stream tokens))
        (declare (ignore            tokens))
        (the Halt-Node
          (make-halt-node)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of main parser.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-empty-line-tokens (tokens)
  "Proceeding from the current element in the token stream TOKENS, skips
   a sequence of zero or more accolent newline tokens and returns no
   value."
  (declare (type Token-Stream tokens))
  (loop while (token-stream-matches-p tokens :newline) do
    (token-stream-consume tokens))
  (values))

;;; -------------------------------------------------------

(defun skip-comment-tokens (tokens)
  "Determines whether the current element in the token stream TOKENS
   represents a comment token, on confirmation skipping all tokens until
   the end of the current line or the end of the source, in the former
   case also omitting all subsequent empty lines, otherwise accompassing
   no action, in any case returning no value."
  (declare (type Token-Stream tokens))
  (when (or (token-stream-matches-p tokens :remainder-assignment)
            (token-stream-matches-p tokens :remainder))
    (token-stream-consume tokens)
    (loop
      until (or (token-stream-matches-p tokens :newline)
                (token-stream-matches-p tokens :eof))
      do    (token-stream-consume tokens)))
  (skip-empty-line-tokens tokens)
  (values))

;;; -------------------------------------------------------

(defun parse-end-of-line (tokens)
  "Determines whether the current element in the token stream TOKENS
   represents an instruction line terminator, that is, either a newline
   or an end-of-file entity, in the former case skipping all consecutive
   newlines, in the latter case merely the current element, always,
   however, returning no value; while, upon a negative probing, an error
   of an unspecified type is signaled."
  (declare (type Token-Stream tokens))
  (let ((current-token (token-stream-peek tokens)))
    (declare (type Token current-token))
    (case (token-type current-token)
      (:eof
        (token-stream-consume tokens))
      (:newline
        (skip-empty-line-tokens tokens))
      (otherwise
        (error "Expected the end of the line, but encountered the ~
                token ~s."
          current-token))))
  (values))

;;; -------------------------------------------------------

(defun parse-when-block (tokens)
  "Proceeding from the current element in the token stream TOKENS,
   consumed a \"when\" block, compact of an activation condition and an
   arbitrary tally of body statements, and returns a ``When-Block-Node''
   representation of the compound."
  (declare (type Token-Stream tokens))
  (token-stream-expect tokens :when)
  (let ((condition (parse-expression 0 tokens))
        (body      NIL))
    (declare (type Node      condition))
    (declare (type node-list body))
    
    ;; Collect the "when" block body statements.
    (loop
      ;; Skip contingently preceding blank and comment lines.
      initially
        (skip-empty-line-tokens tokens)
        (skip-comment-tokens    tokens)
      
      for next-token
        of-type Token
        =       (token-stream-peek tokens)
      
      ;; Expression follows?
      if (nud-token-p next-token)
        collect
          (prog1
            (parse-expression       0 tokens)
            (parse-end-of-line      tokens)
            (skip-empty-line-tokens tokens)
            (skip-comment-tokens    tokens))
        into
          statements
      ;; Statement follows?
      else if (std-token-p next-token)
        collect
          (prog2
            (token-stream-consume   tokens)
            (parse-std-token        next-token tokens)
            (parse-end-of-line      tokens)
            (skip-empty-line-tokens tokens)
            (skip-comment-tokens    tokens))
        into
          statements
      ;; Any other content does not belong to the "when" block's body.
      else do
        (loop-finish)
      
      ;; Skip contingently succeeding blank and comment lines, and store
      ;; the STATEMENTS into the "when" block BODY.
      finally
        (skip-empty-line-tokens tokens)
        (skip-comment-tokens    tokens)
        (setf body statements))
    
    (the When-Block-Node
      (make-when-block-node
        :condition  condition
        :statements body))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Parses a program utilizing the token stream TOKENS and returns a
   ``Program-Node'' representation of its consumed \"when\" blocks."
  (declare (type Token-Stream tokens))
  (the Program-Node
    (loop
      initially
        (skip-empty-line-tokens tokens)
        (skip-comment-tokens    tokens)
      
      for current-token
        of-type Token
        =       (token-stream-peek tokens)
      
      if (token-type-p current-token :eof) do
        (token-stream-consume tokens)
        (loop-finish)
      else if (token-type-p current-token :when)
        collect
          (prog1
            (parse-when-block tokens)
            (skip-empty-line-tokens tokens)
            (skip-comment-tokens    tokens))
          into statements
      else do
        (error "No \"when\" block token: ~s." current-token)
      end
      
      finally
        (return
          (make-program-node :statements statements)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of counter objects.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (CObject)
  "The ``CObject'' interface accommodates a common foundry for all
   classes dedicated to the representation of objects recognized by the
   Counter programming language.")

;;; -------------------------------------------------------

(defstruct (CBoolean
  (:include     CObject)
  (:constructor make-cboolean
                  (generalized-boolean
                   &aux (value (not (null generalized-boolean))))))
  "The ``CBoolean'' class encapsulates a ``boolean'' object."
  (value (error "Missing value.") :type boolean :read-only T))

;;; -------------------------------------------------------

(defstruct (CInteger
  (:include     CObject)
  (:constructor make-cinteger (value)))
  "The ``CInteger'' class encapsulates a signed integer object."
  (value (error "Missing value.") :type integer :read-only T))

;;; -------------------------------------------------------

(defstruct (CReference
  (:include     CObject)
  (:constructor make-creference (target)))
  "The ``CReference'' class encapsulates a reference to an identifier,
   usually a variable, by its name's adminiculum."
  (target (error "Missing target.") :type keyword :read-only T))

;;; -------------------------------------------------------

(defstruct (CString
  (:include     CObject)
  (:constructor make-cstring (value)))
  "The ``CString'' class encapsulates a string object."
  (value (error "Missing value.") :type string :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-binary-operator (operator left-operand right-operand)
  (:documentation
    "Applies the binary OPERATOR to the LEFT-OPERAND and the
     RIGHT-OPERAND and returns a result appropriate for this
     combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation
    (operator (left-operand-type right-operand-type result-type)
     &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operator'', with the first parameter assigned an
   automatically generated name and associated with a dispatching
   ``eql'' specializer assuming the OPERATOR type, the second parameter
   fixated by the agnomination ``left'' and the LEFT-OPERAND-TYPE as its
   specializer, the third input nevened as ``right'' and specialized to
   the RIGHT-OPERAND-TYPE, and expecting to return an object of the
   RESULT-TYPE as its desinent BODY form.
   ---
   If the first BODY form produces a string, the same is reappropriated
   for the purpose of a the ``defmethod'' definition's documentation
   string; otherwise an automatically generated docstring is afforded."
  (let ((operator-name (gensym)))
    (declare (type symbol operator-name))
    `(defmethod apply-binary-operator
         ((,operator-name (eql ,operator))
          (left           ,left-operand-type)
          (right          ,right-operand-type))
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Applies the binary operator ~s to the LEFT ~
                       operand, of the ~s type, and the RIGHT operand, ~
                       conforming to the ~s species, and returns a ~
                       value of the type ~s."
            operator left-operand-type right-operand-type result-type))
       (declare (type binary-operator     ,operator-name))
       (declare (ignore                   ,operator-name))
       (declare (type ,left-operand-type  left))
       (declare (ignorable                left))
       (declare (type ,right-operand-type right))
       (declare (ignorable                right))
       (the ,result-type
         (progn
           ,@body)))))

;;; -------------------------------------------------------

(define-binary-operation :addition (CInteger CInteger CInteger)
  (make-cinteger
    (+ (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :addition (CInteger CString CString)
  "Returns a new ``CString'' formed by concatenating the LEFT number and
   the RIGHT text."
  (make-cstring
    (format NIL "~d~a"
      (cinteger-value left)
      (cstring-value  right))))

;;; -------------------------------------------------------

(define-binary-operation :addition (CString CInteger CString)
  "Returns a new ``CString'' formed by concatenating the LEFT text and
   the RIGHT number."
  (make-cstring
    (format NIL "~a~d"
      (cstring-value  left)
      (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :subtraction (CInteger CInteger CInteger)
  (make-cinteger
    (- (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (CInteger CInteger CInteger)
  (make-cinteger
    (* (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (CInteger CString CString)
  "Returns a new ``CString'' formed by repeating the RIGHT text a tally
   of LEFT times."
  (make-cstring
    (format NIL "~v@{~a~:*~}"
      (cinteger-value left)
      (cstring-value  right))))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (CString CInteger CString)
  "Returns a new ``CString'' formed by repeating the LEFT text a tally
   of RIGHT times."
  (make-cstring
    (format NIL "~v@{~a~:*~}"
      (cinteger-value right)
      (cstring-value  left))))

;;; -------------------------------------------------------

(define-binary-operation :division (CInteger CInteger CInteger)
  (make-cinteger
    (round
      (cinteger-value left)
      (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :remainder (CInteger CInteger CInteger)
  (make-cinteger
    (mod
      (cinteger-value left)
      (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :power (CInteger CInteger CInteger)
  (make-cinteger
    (expt
      (cinteger-value left)
      (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :logical-and (CBoolean CBoolean CBoolean)
  (make-cboolean
    (and (cboolean-value left)
         (cboolean-value right))))

;;; -------------------------------------------------------

(define-binary-operation :logical-or (CBoolean CBoolean CBoolean)
  (make-cboolean
    (or (cboolean-value left)
        (cboolean-value right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (CBoolean CBoolean CBoolean)
  (make-cboolean
    (and (cboolean-value left)
         (cboolean-value right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (CInteger CInteger CBoolean)
  (make-cboolean
    (= (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (CString CString CBoolean)
  (make-cboolean
    (string=
      (cstring-value left)
      (cstring-value right))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (CObject CObject CBoolean)
  (make-cboolean NIL))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (CBoolean CBoolean CBoolean)
  (make-cboolean
    (not (and
      (cboolean-value left)
      (cboolean-value right)))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (CInteger CInteger CBoolean)
  (make-cboolean
    (/= (cinteger-value left)
        (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (CString CString CBoolean)
  (make-cboolean
    (string/=
      (cstring-value left)
      (cstring-value right))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (CObject CObject CBoolean)
  (make-cboolean T))

;;; -------------------------------------------------------

(define-binary-operation :less-than (CInteger CInteger CBoolean)
  (make-cboolean
    (< (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (CObject CObject CBoolean)
  (error "The binary operator \"<\" cannot be applied to ~s and ~s."
    left right))

;;; -------------------------------------------------------

(define-binary-operation :less-than-or-equal-to (CInteger
                                                 CInteger
                                                 CBoolean)
  (make-cboolean
    (< (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than-or-equal-to (CObject
                                                 CObject
                                                 CBoolean)
  (error "The binary operator \"<=\" cannot be applied to ~s and ~s."
    left right))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (CInteger CInteger CBoolean)
  (make-cboolean
    (> (cinteger-value left)
       (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (CObject CObject CBoolean)
  (error "The binary operator \">\" cannot be applied to ~s and ~s."
    left right))

;;; -------------------------------------------------------

(define-binary-operation :greater-than-or-equal-to (CInteger
                                                    CInteger
                                                    CBoolean)
  (make-cboolean
    (>= (cinteger-value left)
        (cinteger-value right))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than-or-equal-to (CObject
                                                    CObject
                                                    CBoolean)
  (error "The binary operator \">=\" cannot be applied to ~s and ~s."
    left right))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-unary-operator (operator operand)
  (:documentation
    "Applies the unary OPERATOR to the OPERAND and returns a value
     appropriate for this combination."))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :plus))
                                 (operand  CInteger))
  "Represents the mathematical plus (\"+\") sign OPERATOR, which returns
   its numeric OPERAND unmodified."
  (declare (type unary-operator operator))
  (declare (type CInteger       operand))
  (the CInteger
    (make-cinteger
      (+ (cinteger-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :minus))
                                 (operand  CInteger))
  "Represents the mathematical minus (\"-\") sign OPERATOR, which
   negates its OPERAND's polarity."
  (declare (type unary-operator operator))
  (declare (type CInteger       operand))
  (the CInteger
    (make-cinteger
      (- (cinteger-value operand)))))

;;; -------------------------------------------------------

(defmethod apply-unary-operator ((operator (eql :logical-not))
                                 (operand  CBoolean))
  "Represents the logical NOT (\"!\") OPERATOR, which negates a Boolean
   truth value."
  (declare (type unary-operator operator))
  (declare (type CBoolean       operand))
  (the CBoolean
    (make-cboolean
      (not (cboolean-value operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Boolean predicate operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric object-true-p (object)
  (:documentation
    "Determines whether the OBJECT shall be regarded as tantamount to a
     Boolean \"true\" value, returning on confirmation a ``boolean''
     value of ``T'', otherwise ``NIL''."))

;;; -------------------------------------------------------

(defmethod object-true-p ((object CBoolean))
  "Determines whether ``CBoolean'' OBJECT is true, which is the case for
   a value of ``T'', returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type CBoolean object))
  (the boolean
    (cboolean-value object)))

;;; -------------------------------------------------------

(defmethod object-true-p ((object CInteger))
  "Always returns ``T'', as any integer OBJECT is tantamount to an
   affirmative Boolean value."
  (declare (type CInteger object))
  (declare (ignore        object))
  (the boolean T))

;;; -------------------------------------------------------

(defmethod object-true-p ((object CObject))
  "Always returns ``T'', as any non-false OBJECT is tantamount to an
   affirmative Boolean value."
  (declare (type CObject object))
  (declare (ignore       object))
  (the boolean T))

;;; -------------------------------------------------------

(defmethod object-true-p ((object CReference))
  "Always returns ``T'', as any OBJECT reference is tantamount to an
   affirmative Boolean value."
  (declare (type CReference object))
  (declare (ignore          object))
  (the boolean T))

;;; -------------------------------------------------------

(defmethod object-true-p ((object CString))
  "Always returns ``T'', as any string OBJECT is tantamount to an
   affirmative Boolean value."
  (declare (type CString object))
  (declare (ignore       object))
  (the boolean T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Halt-Condition" condition.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:documentation
    "The ``Halt-Condition'' condition serves to signal the intention to
     immediately terminate a Counter program."))

;;; -------------------------------------------------------

(defun signal-program-halt ()
  "Signals a ``Halt-Condition'' condition."
  (signal 'Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (real 0.0 *) +DEFAULT-CYCLE-DELAY+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CYCLE-DELAY+ 0.05
  "The default number of seconds to wait betwixt two consecutive cycles
   in a Counting program's perpetual loop.")

;;; -------------------------------------------------------

(defstruct (Interpreter
  (:constructor make-interpreter
    (tree
     &key (cycle-delay +DEFAULT-CYCLE-DELAY+))))
  "The ``Interpreter'' class is encumbered with the governance of a
   Counter program's abstract syntax tree (AST) representation in order
   to accompass utility to the same."
  (tree                   (error "Missing tree.")
                          :type      Node
                          :read-only T)
  ;; The "cnt" variable.
  (counter                0
                          :type      integer
                          :read-only NIL)
  ;; The "acc" variable.
  (accumulator            0
                          :type      integer
                          :read-only NIL)
  ;; A Boolean flag which memorizes whether all "when" blocks have
  ;; failed, in which case the instruction count, or COUNTER, must be
  ;; incremented adveniently at the current cycle's desinence.
  (has-matched-when-block NIL
                          :type      boolean
                          :read-only NIL)
  ;; An optional delay betwixt two iteration cycles for the purpose of
  ;; augmented visibility regarding the outputs.
  (cycle-delay            +DEFAULT-CYCLE-DELAY+
                          :type      (real 0.0 *)
                          :read-only T))

;;; -------------------------------------------------------

(defgeneric interpreter-resolve-reference (interpreter object)
  (:documentation
    "Returns the value of the OBJECT, construed as a reference, in the
     INTERPRETER's context.")
  
  (:method ((interpreter Interpreter) (object CReference))
    "Returns the value of the variable mentioned in the reference OBJECT
     as defined by the INTERPRETER's context."
    (declare (type Interpreter interpreter))
    (declare (type CReference  object))
    (the CInteger
      (make-cinteger
        (case (creference-target object)
          (:acc      (interpreter-accumulator interpreter))
          (:cnt      (interpreter-counter     interpreter))
          (otherwise (error "Invalid reference target in ~s."
                       object))))))
  
  (:method ((interpreter Interpreter) (object CObject))
    "Ignores the INTERPRETER and returns the object's value."
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type CObject     object))
    (the CObject object)))

;;; -------------------------------------------------------

(defgeneric interpreter-get-object-value (interpreter object)
  (:documentation
    "Returns the OBJECT's value in the INTERPRETER's context.")
  
  (:method ((interpreter Interpreter) (object CBoolean))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type CBoolean    object))
    (the boolean
      (cboolean-value object)))
  
  (:method ((interpreter Interpreter) (object CInteger))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type CInteger    object))
    (the integer
      (cinteger-value object)))
  
  (:method ((interpreter Interpreter) (object CReference))
    (declare (type Interpreter interpreter))
    (declare (type CReference  object))
    (the (or boolean integer string)
      (interpreter-get-object-value interpreter
        (interpreter-resolve-reference interpreter object))))
  
  (:method ((interpreter Interpreter) (object CString))
    (declare (type Interpreter interpreter))
    (declare (ignore           interpreter))
    (declare (type CString     object))
    (the string
      (cstring-value object))))

;;; -------------------------------------------------------

(defun interpreter-assign-value (interpreter target new-value)
  "Expecting the TARGET to designate a reference intelligible to the
   INTERPRETER, associates the NEW-VALUE with the same and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type CObject     target))
  (declare (type CObject     new-value))
  ;; Only the accumulator variable, "acc", is amenable to modifications.
  (if (and (typep target 'CReference)
           (eq (creference-target target) :acc))
    (setf (interpreter-accumulator      interpreter)
          (interpreter-get-object-value interpreter new-value))
    (error "Cannot assign a value to the target ~s." target))
  (values))

;;; -------------------------------------------------------

(defun interpreter-increment-counter (interpreter)
  "Increments the INTERPRETER's instruction counter, or counter, by one
   and returns no value."
  (declare (type Interpreter interpreter))
  (incf (interpreter-counter interpreter))
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-visit-node (interpreter node)
  (:documentation
    "Processes the NODE in the INTERPRETR's context and returns a value
     appropriate for this combination."))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Program-Node))
  "Evaluates the program NODE in the INTERPRETER's context, executing
   its instructions according to their specified order in a perpetually
   repeating loop, contingently updating the instruction count, or
   counter, succeeding each cycle that did not attest a matching
   \"when\" block, and, if halted in some fashion, returns no value.
   ---
   The ``Program-Node'' furnishes the entry point into the abstract
   syntax tree's (AST) traversal."
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (loop do
    (handler-case
      (progn
        ;; Reset the flag which memorizes whether all "when" blocks have
        ;; failed to match.
        (setf (interpreter-has-matched-when-block interpreter) NIL)
        
        ;; Execute the program body statement.
        (dolist (statement (program-node-statements node))
          (declare (type Node statement))
          (interpreter-visit-node interpreter statement))
        
        ;; If all "when" blocks have failed to match, increment the
        ;; instruction count, or counter, by one.
        (unless (interpreter-has-matched-when-block interpreter)
          (interpreter-increment-counter interpreter))
        
        ;; Delay the next cycle's execution by the specified number of
        ;; seconds for the user's inquisitive purposes.
        (sleep (interpreter-cycle-delay interpreter)))
      
      ;; If a ``Halt-Condition'' has been signaled by the Counting
      ;; \"halt\" instruction, simply terminate the loop.
      (Halt-Condition ()
        (loop-finish))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Binary-Operation-Node))
  "Applies the binary operation node's binary operator to the sinistral
   and dextral operands, contingently utilizing the INTERPRETER's
   context, and returns the result."
  (declare (type Interpreter           interpreter))
  (declare (type Binary-Operation-Node node))
  (the CObject
    (apply-binary-operator
      (binary-operation-node-operator node)
      (interpreter-resolve-reference interpreter
        (interpreter-visit-node interpreter
          (binary-operation-node-left-operand node)))
      (interpreter-resolve-reference interpreter
        (interpreter-visit-node interpreter
          (binary-operation-node-right-operand node))))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node
    ((interpreter Interpreter)
     (node        Compound-Assignment-Node))
  "Applies the compound assignment node's binary operator by modifying
   its sinistral side, the target, in concord with the operator by the
   dextral moeity, utilizing the INTERPRETER's context in order to
   assign the target variable's new state, and returns no value."
  (declare (type Interpreter              interpreter))
  (declare (type Compound-Assignment-Node node))
  (let ((operator
          (compound-assignment-node-operator node))
        (target
          (interpreter-visit-node interpreter
            (compound-assignment-node-left-operand node))))
    (declare (type binary-operator operator))
    (declare (type CObject         target))
    (declare (ignorable            target))
    ;; In the current Counter language rendition only the compound
    ;; assignment via addition ("+=") is defined.
    (if (eq operator :addition)
      (interpreter-assign-value interpreter target
        (apply-binary-operator operator
          (interpreter-resolve-reference interpreter target)
          (interpreter-resolve-reference interpreter
            (interpreter-visit-node interpreter
              (compound-assignment-node-right-operand node)))))
      (error "The compound assignment operator ~s is not homologated."
        operator)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Group-Node))
  "Evaluates the group NODE's expression, contingently employing the
   INTERPRETER's context, and returns the yielded result."
  (declare (type Interpreter interpreter))
  (declare (type Group-Node  node))
  (the CObject
    (interpreter-visit-node interpreter
      (group-node-expression node))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Halt-Node))
  "Ignores both the INTERPRETER and the committed halt NODE and signals
   a ``Halt-Condition'' in order to terminate the executing program with
   immediacy."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Halt-Node   node))
  (declare (ignore           node))
  (signal-program-halt))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Identifier-Node))
  "Resolves the variable name ensconced in the identifier NODE in the
   INTERPRETER's context and returns its associated value."
  (declare (type Interpreter     interpreter))
  (declare (type Identifier-Node node))
  (the CReference
    (make-creference
      (identifier-node-name node))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Number-Node))
  "Ignores the INTERPRETER and returns the number NODE's integral
   value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Number-Node node))
  (the CInteger
    (make-cinteger
      (number-node-value node))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Out-Node))
  "Prints the expression contained in the output NODE, contingently
   aided by the INTERPRETER's context, on a line of its own to the
   standard output and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Out-Node    node))
  (format T "~%~a"
    (interpreter-get-object-value interpreter
      (interpreter-resolve-reference interpreter
        (interpreter-visit-node interpreter
          (out-node-argument node)))))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Read-Node))
  "Queries the standard input for a signed or unsigned integer number,
   stores it in the variable referenced by the read NODE and identified
   by the INTERPRETER's context, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Read-Node   node))
  (let ((target
          (interpreter-visit-node interpreter
            (read-node-argument node))))
    (declare (type CObject target))
    (format T "~&>> ")
    (finish-output)
    (interpreter-assign-value interpreter target
      (make-cinteger
        (parse-integer
          (read-line))))
    (clear-input))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        String-Node))
  "Ignores the INTERPRETER and returns the string NODE's textual value."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type String-Node node))
  (the CString
    (make-cstring
      (string-node-value node))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        Unary-Operation-Node))
  "Applies the unary operator stored in the unary operation NODE to its
   sole operand, contingently utilizing the INTERPRETER's context, and
   returns the resulting value."
  (declare (type Interpreter          interpreter))
  (declare (type Unary-Operation-node node))
  (the CObject
    (apply-unary-operator
      (unary-operation-node-operator node)
      (interpreter-resolve-reference interpreter
        (interpreter-visit-node interpreter
          (unary-operation-node-operand node))))))

;;; -------------------------------------------------------

(defmethod interpreter-visit-node ((interpreter Interpreter)
                                   (node        When-Block-node))
  "Probes the when NODE in the INTERPRETER's context, upon its
   predicate's ascertained eligibility executing body statements, while
   updating the instruction counter, or counter, and in any case returns
   no value."
  (declare (type Interpreter     interpreter))
  (declare (type When-Block-Node node))
  (when (object-true-p
          (interpreter-visit-node interpreter
            (when-block-node-condition node)))
    (setf (interpreter-has-matched-when-block interpreter) T)
    (dolist (statement (when-block-node-statements node))
      (declare (type Node statement))
      (interpreter-visit-node interpreter statement))
    ;; Increment the instruction count, or counter.
    (interpreter-increment-counter interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Evaluates the abstract syntax tree (AST) representation of the
   Counter program under the INTERPRETER's castaldy and returns no
   value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Counting (code
                           &key (cycle-delay +DEFAULT-CYCLE-DELAY+))
  "Interprets the piece of Counter source CODE and returns no value."
  (declare (type string       code))
  (declare (type (real 0.0 *) cycle-delay))
  (interpreter-interpret
    (make-interpreter
      (parse-program
        (make-token-stream
          (make-lexer code)))
      :cycle-delay cycle-delay)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Counting
  "when 1
     out \"Hello, World!\"
     halt")

;;; -------------------------------------------------------

;; Query the user for the tally of times to repeat the message
;; "Hello, World!" in immediate succession, and print the same as such.
(interpret-Counting
  "when cnt == 0
     read acc
     out  \"Hello, World! \" * acc
     halt")

;;; -------------------------------------------------------

;; Range from 2 to 100.
;; 
;; Counter which prints the number in the closed interval [2, 100].
(interpret-Counting
  "when cnt > 100
     halt
   when cnt >= 2
     out cnt")

;;; -------------------------------------------------------

;; Infinite counter which tallies from one (1) up to infinity.
(interpret-Counting
  "when cnt > 0
     out cnt")

;;; -------------------------------------------------------

;; Infinite counter which counts up starting from two (2) with a step
;; size of two (2).
(interpret-Counting
  "when cnt % 3 == 2
     acc += 2
     out acc")

;;; -------------------------------------------------------

;; Line-based infinite cat program.
(interpret-Counting
  "when 1
     read acc
     out acc")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Counting
  "when cnt == 0
     read acc
     out  acc
   when acc == 0
     halt
   when acc == 1
     out acc")

;;; -------------------------------------------------------

;; Add two numbers.
;; 
;; Please note:
;;   The first input is not homologated to assume a value less than or
;;   equal to zero (0), while the second respond may embrace any signed
;;   integer.
(interpret-Counting
  "when cnt == 0
     read acc
   
   when cnt == acc
     read acc
     out  acc + cnt
     halt"
  :cycle-delay 0)

;;; -------------------------------------------------------

;; Triangular numbers.
;; 
;; Implements a looping counter which outputs for a user input of "N"
;; the numbers in the intral range [1, N] by a series of asterisks ("*")
;; tantamount to the current counter value.
(interpret-Counting
  "when cnt == 0
     read acc
   when cnt <= acc
     out \"*\" * cnt
   when cnt > acc
     halt
   
   % 1 + 2 + ... + acc")

;;; -------------------------------------------------------

;; Factorial sequence.
;; 
;; Given:
;;   n --- a user input integer >= 2
;; Computes:
;;   F(0) = 2 * n
;;   F(i) = F(i-1) * i
;; 
;; Please note:
;;   Repeats without printing for a user input of n <= 1.
(interpret-Counting
  "when cnt == 0
     read acc
   when cnt < acc
    acc += 0 - acc + acc * cnt
    out acc")

;;; -------------------------------------------------------

;; Digital root calculator.
;; 
;; Returns: 9.
(interpret-Counting
  "when cnt - 1 == 1 + (acc - 1) % 9 
     out cnt - 1
     halt"
  :cycle-delay 0)

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-Counting
  "
  % Bottles in range [3, 99].
  when (99 - cnt) > 2
    out (99 - cnt) + \" bottles of beer on the wall,\"
    out (99 - cnt) + \" bottles of beer.\"
    out \"Take one down, pass it around,\"
    out (99 - cnt - 1) + \" bottles of beer on the wall.\"
    out \"\"
  
  % Two bottles.
  when (99 - cnt) == 2
    out (99 - cnt) + \" bottles of beer on the wall,\"
    out (99 - cnt) + \" bottles of beer.\"
    out \"Take one down, pass it around,\"
    out (99 - cnt - 1) + \" bottle of beer on the wall.\"
    out \"\"
  
  % One bottle.
  when (99 - cnt) == 1
    out (99 - cnt) + \" bottle of beer on the wall,\"
    out (99 - cnt) + \" bottle of beer.\"
    out \"Take one down, pass it around,\"
    out \"No bottles of beer on the wall.\"
    halt
  "
  :cycle-delay 0)
