;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "SOAP", presented by the Esolang user "BoundedBeans" in the
;; year 2022, and founded upon the manipulation of sets containing
;; natural numbers.
;; 
;; 
;; Concept
;; =======
;; The SOAP programming language is based upon the application of set
;; operations in order to activate the various conditional, input, and
;; output facilities.
;; 
;; == SOAP: PROGRAMMING WITH SETS ==
;; The norning of the language "SOAP", as an acronym for
;; "*S*et *O*riented *A*rithmetic *P*rogramming" alreadys serves in the
;; bewrayal of its basic concepts, a ligation of the mathematical set
;; notion whose manipulations helms a program.
;; 
;; == SETS ADMIT NATURAL NUMBERS ==
;; The exclusive data type released to currency in SOAP set constitutes
;; the natural numbers, that is, positive integers greater than zero
;; (0), with no intrinsic maximum's encumbrance.
;; 
;; == SET MANIPULATIONS DRIVE THE PROGRAM ==
;; The indagation and manipulation of one or more sets realizes all
;; programmatic actions.
;; 
;; Each SOAP program entails three data segments:
;; 
;;   (1) A main set compact of natural numbers, and initially empty.
;;   (2) A variable, clept as and accessed by "%", storing a natural
;;       number, while being amenable to basic modifications, as well as
;;       applications in sets.
;;   (3) A cell which ensconces the last character committed by the user
;;       as an input.
;; 
;; 
;; Architecture
;; ============
;; SOAP's architecture is founded upon a treble account of constituents,
;; enumerating a main set involved in most applications, a scalar
;; integer variable, and a character conditory for storing the recent
;; user input.
;; 
;; == MAIN SET ==
;; The main set's diorism establishes a globally valid set of natural
;; numbers applicable in a preponderant tally of occasions.
;; 
;; Initially empty, its indagation and manipulation imposes a
;; prerequisite to the operation of the instructions' majority,
;; participating in all such operations implicitly either as the sole,
;; if a unary procedure, or as the first operand, in the binary case.
;; 
;; == INTEGER VARIABLE ==
;; A globally active entity, the singular variable establishes an
;; adminicular storage for a natural number. Its tolerance in every
;; occasion admissive to literal integers supplies the capacity to
;; define a dynamic implement.
;; 
;; == INPUT STORAGE ==
;; A dedicated unit's manifestation attends to the requirements of the
;; user input handling. This scalar character cell embraces an
;; externally supplied datum via the instruction "~", and permits adit
;; to its future indagation as a predicament for the "'" iteration
;; construct.
;; 
;; 
;; Data Types
;; ==========
;; The exhaustion of SOAP's type system proceeds from a treble
;; components: natural numbers, sets of these numbers, and Unicode
;; characters, with the first subject among the three participants most
;; importantly supplying the elements of the second, the second being
;; apportioned the paravaunt significance in all programs, and concluded
;; with the third's input/output-only contribution.
;; 
;; == NATURAL NUMBERS ==
;; SOAP embraces a specialized subset of integers in the natural
;; numbers, that is, that portion assigning a commorancy to the unsigned
;; positive members, destitute of the zero (0) element.
;; 
;; In their scalar guise, these numeric objects are encountered with
;; scarce usance, preponderantly employing a role as the members of set
;; literals.
;; 
;; == SETS ==
;; The prepotent species to declare its woning in the SOAP language ---
;; whence also is derived its agnomination --- the sets contribute an
;; unordered sequence of unique elements; in this particular case, only
;; natural numbers enjoy adit.
;; 
;; == CHARACTERS ==
;; Both input and output justify an agency's attribution among the SOAP
;; type roster, with a capacity equipollent to the nimious Unicode
;; standard.
;; 
;; 
;; Syntax
;; ======
;; SOAP's syntaxis imposes the application of its facilities by a
;; single-character adminicle employed as the identifier. The actual
;; structure constitutes a dependency upon the concrete facility.
;; 
;; == INSTRUCTIONS ==
;; All language constructs are introduced by adminiculum of a single
;; identifier character.
;; 
;; Procedure invocations dependent upon a parameter expect their input
;; to be stated in immediate succession to the command designator, with
;; the contingency of whitespaces in the interstice.
;; 
;; More complex designs exist for the conditional and iterative
;; facilities.
;; 
;; == PARAMETERS ==
;; Instruction parameters may belong to one of three possible
;; categories:
;; 
;;   (a) Integers:
;;       The only numeric type resolves to natural numbers, that is,
;;       positive integers in the unbounded range [0, +infinity]. All
;;       numeric objects are specified in either of two forms:
;;       (a.1) As a base-3 (ternary) unsigned literal; as a corollary,
;;             only the digits 0, 1 and 2 are tolerated.
;;       (a.2) As a variable access, represented by the token "%".
;;   (b) Set literals:
;;       Sets serve to define an unordered sequence of zero or more
;;       natural numbers, either supplied directly or by reference to
;;       the single global variable. Two possible variants exist:
;;       (b.1) A listing of zero or more integers, each of which may be
;;             stated either by its positive base-3 literal value or as
;;             the variable specifier "%", as described under the
;;             point (a), which please see.
;;             Any two elements are separated by a single comma (","),
;;             with the whole sequence ensconced in a jumelle of the
;;             braces "{" and "}".
;;       (b.2) The empty set, denoted by "Ø", which contains no elements
;;             at all, being in this a tantamount of "{}".
;;   (c) Character literals:
;;       Characters are stated as literals, appropriated from the
;;       Unicode repertoire.
;; 
;; == WHITESPACES ==
;; The introduction of whitespaces, comprehending the space, tab and
;; newline character, is homologated in accord with one's personal
;; delectation.
;; 
;; == COMMENTS ==
;; The language in its current iteration lacks any provisions for
;; comments.
;; 
;; == GRAMMAR ==
;; SOAP's grammar can be subjected to the following formulation in the
;; Extended Backus-Naur Form (EBNF):
;; 
;;   program         := commands ;
;;   commands        := { command } ;
;;   command         := flipMembership
;;                   |  complement
;;                   |  binaryOperation
;;                   |  loop
;;                   |  ifCharacter
;;                   |  varIncrement
;;                   |  varDecrement
;;                   |  printCharacter
;;                   |  inputCharacter
;;                   ;
;;   
;;   flipMembership  := "*" , setElement ;
;;   complement      := "c" ;
;;   binaryOperation := union
;;                   |  intersection
;;                   |  leftDifference
;;                   |  rightDifference
;;                   ;
;;   union           := "∪" , setLiteral ;
;;   intersection    := "∩" , setLiteral ;
;;   leftDifference  := "-" , setLiteral ;
;;   rightDifference := "_" , setLiteral ;
;;   
;;   loop            := loopPredicate , "[" , commands , "]" ;
;;   loopPredicate   := setRelation , setLiteral ;
;;   setRelation     := "⊆" | "⊂" | "⊄" | "⊇" | "⊃" | "⊅" | "=" ;
;;   
;;   ifCharacter     := "'" , character , "/" , commands , "\" ;
;;   
;;   varIncrement    := ":" ;
;;   varDecrement    := ";" ;
;;   
;;   printCharacter  := '"' , character ;
;;   inputCharacter  := "~" ;
;;   
;;   setLiteral      := bracketedSet | emptySet ;
;;   bracketedSet    := "{" , { setElement } , "}" ;
;;   emptySet        := "Ø" ;
;;   
;;   setElement      := number | variable ;
;;   variable        := "%" ;
;;   number          := digit , { digit } ;
;;   digit           := "0" | "1" | "2" ;
;; 
;; 
;; Instructions
;; ============
;; SOAP offers eighteen instructions, comprehending unary and binary set
;; operations, character input and output facilities, as well as several
;; iteration constructs based upon the notion of set relations,
;; concluding with an "if" conditional whose antecedant involves a
;; stated character's juxtaposition with a preceding input.
;; 
;; == INSTRUCTION CATEGORIES ==
;; The SOAP instruction set's eighteen members cardinality can be
;; subsumed into a septuple division.
;; 
;;   ------------------------------------------------------------------
;;   Subject            | Oper. | Effect summary
;;   -------------------+-------+--------------------------------------
;;   Nullary operations | c     | Set complement
;;   ..................................................................
;;   Unary operations   | *𝑖    | Addition/Removal
;;   ..................................................................
;;   Binary operations  | ∪𝐴    | Set union
;;                      | ∩𝐴    | Set intersection
;;                      | -𝐴    | Left set difference
;;                      | _𝐴    | Right set difference
;;   ..................................................................
;;   Iterations         | ⊆𝐴[𝛬] | Loop while subset
;;                      | ⊂𝐴[𝛬] | Loop while proper subset
;;                      | ⊄𝐴[𝛬] | Loop while not subset
;;                      | ⊇𝐴[𝛬] | Loop while superset
;;                      | ⊃𝐴[𝛬] | Loop while proper superset
;;                      | ⊅𝐴[𝛬] | Loop while not superset
;;                      | =𝐴[𝛬] | Loop while equal
;;   ..................................................................
;;   Conditionals       | '𝑐/𝛬\ | Character-equality conditional
;;   ..................................................................
;;   Variables          | :     | Increment
;;                      | ;     | Decrement
;;   ..................................................................
;;   Input/Output       | "𝑐    | Output
;;                      | ~     | Input
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; Preceding a more detailed exposition, the following cursory treatise
;; shall supply an inchoate nortelry regarding the language's
;; capabilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   *𝑖      | Flips the membership of the integer 𝑖 in the main set:
;;           | If is an element of the main set, it is removed;
;;           | otherwise it is added to the same.
;;   ..................................................................
;;   c       | Sets the main set 𝑀 to its own complement 𝑀′, that is:
;;           |   𝑀 = 𝑀′
;;   ..................................................................
;;   ∪𝐴      | Sets the main set 𝑀 to its union with the set literal
;;           | 𝐴, that is:
;;           |   𝑀 = 𝑀 ∪ 𝐴
;;   ..................................................................
;;   ∩𝐴      | Sets the main set 𝑀 to its intersection with the set
;;           | literal 𝐴, that is:
;;           |   𝑀 = 𝑀 ∩ 𝐴
;;   ..................................................................
;;   -𝐴      | Sets the main set 𝑀 to the difference of itself deducted
;;           | by the set literal 𝐴, that is:
;;           |   𝑀 = 𝑀 - 𝐴
;;           | or
;;           |   𝑀 = 𝑀 \ 𝐴
;;   ..................................................................
;;   _𝐴      | Sets the main set 𝑀 to the difference of the set
;;           | literal 𝐴 deducted by 𝑀, that is:
;;           |   𝑀 = 𝐴 - 𝑀
;;           | or
;;           |   𝑀 = 𝐴 \ 𝑀
;;   ..................................................................
;;   ⊆𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes a subset of the set
;;           | literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊆ 𝐴
;;   ..................................................................
;;   ⊂𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes a proper subset of the
;;           | set literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊂ 𝐴
;;   ..................................................................
;;   ⊄𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes no subset of the set
;;           | literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊄ 𝐴
;;   ..................................................................
;;   ⊇𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes a superset of the set
;;           | literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊇ 𝐴
;;   ..................................................................
;;   ⊃𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes a proper superset of
;;           | the set literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊃ 𝐴
;;   ..................................................................
;;   ⊅𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 constitutes no superset of the set
;;           | literal 𝐴, that is, as long as it holds:
;;           |   𝑀 ⊅ 𝐴
;;   ..................................................................
;;   =𝐴[𝛬]   | Repeatedly executes the ordered instruction sequence 𝛬
;;           | while the main set 𝑀 equals the set literal 𝐴, that is,
;;           | as long as it holds:
;;           |   𝑀 = 𝐴
;;   ..................................................................
;;   :       | Increments the value of the variable "%" by one.
;;   ..................................................................
;;   ;       | If the value of the variable "%" is greater than one,
;;           | decrements it by one; otherwise exercises no effect.
;;   ..................................................................
;;   "𝑐      | Prints the literal character 𝑐 to the standard output.
;;   ..................................................................
;;   ~       | Queries the user for an input character and stores the
;;           | same.
;;   ..................................................................
;;   '𝑐/𝛬\   | If the last user input committed equals the character
;;           | 𝑐, executes the ordered instruction sequence 𝛬 once;
;;           | otherwise skips this instruction.
;;   ------------------------------------------------------------------
;; 
;; == ‘*’: INCLUSION/EXCLUSION ==
;; The ‘*’ operator negates a specified integer literal's membership
;; status in the main set.
;; 
;; Signature:
;;   *𝑖
;; 
;; Parameters:
;;   𝑖 — A positive integer whose membership in the main set 𝑀 shall be
;;       inverted (flipped).
;; 
;; Interface:
;;   mainSet.flipMembership(𝑖) : void
;; 
;; Pseudocode:
;;   if mainSet.contains(𝑖) then
;;     mainSet.add(𝑖)
;;   else
;;     mainSet.remove(𝑖)
;;   end if
;; 
;; Description:
;;   If the positive integer 𝑖 constitutes a member of the main set 𝑀,
;;   it is removed from the same; otherwise, it is added to 𝑀.
;; 
;; Side effects:
;;   - Depending on the presence or absence of the element 𝑖 in 𝑀, the
;;     integer is either added or removed from 𝑀.
;; 
;; Exceptional situations:
;;   - If the perquired value 𝑖 does not represent a positive integer,
;;     an error of the type "IllegalArgumentError" is signaled.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘c’: COMPLEMENT ==
;; The ‘c’ operator stores in the main set 𝑀 its own absolute
;; complement.
;; 
;; Signature:
;;   c
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   mainSet.complement() : void
;; 
;; Pseudocode (mathematical):
;;   𝑀 ← 𝑀′
;; 
;; Pseudocode (programmatic):
;;   mainSet ← complement(mainSet)
;; 
;; Description:
;;   Calculates the absolute complement 𝑀′ of the main set 𝑀 and stores
;;   the result in 𝑀.
;; 
;; Side effects:
;;   - The main set 𝑀 is modified.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘∪’: UNION ==
;; The ‘∪’ operator stores in the main set 𝑀 the union of its own
;; elements and that of a specified second set.
;; 
;; Signature:
;;   ∪𝐴
;; 
;; Parameters:
;;   𝐴 — The integer set to unite with the main set 𝑀.
;; 
;; Interface:
;;   mainSet.unionWith(𝐴) : void
;; 
;; Pseudocode (mathematical):
;;   𝑀 ← 𝑀 ∪ 𝐴
;; 
;; Pseudocode (programmatic):
;;   mainSet ← union(mainSet, 𝐴)
;; 
;; Description:
;;   Calculates the union of the elements in the main set 𝑀 and that in
;;   the specified set 𝐴 and stores the result in 𝑀.
;; 
;; Side effects:
;;   - If the set 𝐴 contains elements not present in the main set 𝑀, the
;;     latter is modified to integrate the new members.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘∩’: INTERSECTION ==
;; The ‘∩’ operator stores in the main set 𝑀 the intersection of its own
;; elements and that of a specified second set.
;; 
;; Signature:
;;   ∩𝐴
;; 
;; Parameters:
;;   𝐴 — The integer set to intersect with the main set 𝑀.
;; 
;; Interface:
;;   mainSet.intersectionWith(𝐴) : void
;; 
;; Pseudocode (mathematical):
;;   𝑀 ← 𝑀 ∩ 𝐴
;; 
;; Pseudocode (programmatic):
;;   mainSet ← intersect(mainSet, 𝐴)
;; 
;; Description:
;;   Calculates the intersection of the elements in the main set 𝑀 and
;;   that in the specified set 𝐴 and stores the result in 𝑀.
;; 
;; Side effects:
;;   - If the set 𝐴 contains elements not shared by the main set 𝑀, the
;;     latter is modified to expel the unmatching members.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘-’: LEFT DIFFERENCE ==
;; The ‘-’ operator stores in the main set 𝑀 the difference of its own
;; elements pruned by those of the second set.
;; 
;; Signature:
;;   -𝐴
;; 
;; Parameters:
;;   𝐴 — The integer set to subtract from the main set 𝑀.
;; 
;; Interface:
;;   mainSet.leftDifference(𝐴) : void
;; 
;; Pseudocode (mathematical):
;;   𝑀 ← 𝐴 \ 𝑀
;; 
;; Pseudocode (programmatic):
;;   mainSet ← difference(mainSet, 𝐴)
;; 
;; Description:
;;   Calculates the difference of the elements in the main set 𝑀 cleared
;;   from those shared with the specified set 𝐴 and stores the result in
;;   𝑀.
;; 
;; Side effects:
;;   - If the set 𝐴 contains elements present in the main set 𝑀, the
;;     latter is modified to expel the shared members.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘_’: RIGHT DIFFERENCE ==
;; The ‘_’ operator stores in the main set 𝑀 the difference of the
;; specified set pruned by 𝑀's elements.
;; 
;; Signature:
;;   _𝐴
;; 
;; Parameters:
;;   𝐴 — The integer set to be reduced by main set 𝑀.
;; 
;; Interface:
;;   mainSet.rightDifference(𝐴) : void
;; 
;; Pseudocode (mathematical):
;;   𝑀 ← 𝑀 \ 𝐴
;; 
;; Pseudocode (programmatic):
;;   mainSet ← difference(𝐴, mainSet)
;; 
;; Description:
;;   Calculates the difference of the elements in the specified set 𝐴
;;   cleared from those shared with the main set 𝑀 and stores the result
;;   in 𝑀.
;; 
;; Side effects:
;;   - If the main set 𝑀 contains elements present in the set 𝐴, these
;;     are expelled from 𝑀.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊆’: LOOP WHILE SUBSET ==
;; The ‘⊆’ construct executes the iteration body as long as the main set
;; 𝑀 represents a subset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊆𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileSubsetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isSubsetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 constitutes a subset of
;;   the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊂’: LOOP WHILE PROPER SUBSET ==
;; The ‘⊂’ construct executes the iteration body as long as the main set
;; 𝑀 represents a proper subset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊂𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileProperSubsetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isProperSubsetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 constitutes a proper
;;   subset of the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊄’: LOOP WHILE NOT SUBSET ==
;; The ‘⊄’ construct executes the iteration body as long as the main set
;; 𝑀 does not represent a subset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊄𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileNotSubsetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isNotSubsetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 does not constitute a
;;   subset of the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊇’: LOOP WHILE SUPERSET ==
;; The ‘⊇’ construct executes the iteration body as long as the main set
;; 𝑀 represents a superset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊇𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileSupersetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isSupersetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 constitutes a superset
;;   of the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊃’: LOOP WHILE PROPER SUPERSET ==
;; The ‘⊃’ construct executes the iteration body as long as the main set
;; 𝑀 represents a proper superset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊃𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileProperSupersetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isProperSupersetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 constitutes a proper
;;   superset of the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘⊅’: LOOP WHILE NOT SUPERSET ==
;; The ‘⊅’ construct executes the iteration body as long as the main set
;; 𝑀 does not represent a superset of the specified set 𝐴.
;; 
;; Signature:
;;   ⊅𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileNotSupersetOf(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.isNotSupersetOf(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 does not constitute a
;;   superset of the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘=’: LOOP WHILE EQUAL ==
;; The ‘=’ construct executes the iteration body as long as the main set
;; 𝑀 equals the the specified set 𝐴.
;; 
;; Signature:
;;   =𝐴[𝛬]
;; 
;; Parameters:
;;   𝐴 — The integer set to juxtapose with the main set 𝑀.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   loopWhileEquals(mainSet, 𝐴, 𝛬) : void
;; 
;; Pseudocode:
;;   while mainSet.equals(𝐴) do
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end while
;; 
;; Description:
;;   Repeats the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 while the main set 𝑀 equals the set 𝐴.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this loop.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this loop.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘:’: VARIABLE INCREMENTATION ==
;; The ‘:’ operator increments the ‘%’ variable by one.
;; 
;; Signature:
;;   :
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   incrementVariable() : void
;; 
;; Pseudocode:
;;   % ← % + 1
;; 
;; Description:
;;   Increments the value of the ‘%’ variable by one.
;; 
;; Side effects:
;;   - The ‘%’ variable is incremented.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘;’: VARIABLE DECREMENTATION ==
;; The ‘;’ operator decrements the ‘%’ variable by one.
;; 
;; Signature:
;;   ;
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   decrementVariable() : void
;; 
;; Pseudocode:
;;   % ← % - 1
;; 
;; Description:
;;   If the current value of the variable ‘%’ is greater than one (1),
;;   decrements it by one. Otherwise no effect is exerted.
;; 
;; Side effects:
;;   - The ‘%’ variable is decremented.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘"’: CHARACTER OUTPUT ==
;; The ‘"’ operator prints a literal character to the standard output.
;; 
;; Signature:
;;   "𝑐
;; 
;; Parameters:
;;   𝑐 — The character literal to write to the standard output.
;; 
;; Interface:
;;   printCharacter(𝑐) : void
;; 
;; Pseudocode:
;;   print(𝑐)
;; 
;; Description:
;;   Prints the character literal 𝑐 to the standard output.
;; 
;; Side effects:
;;   - A printing operation is committed to the system's standard
;;     output.
;; 
;; Exceptional situations:
;;   - If the parameter 𝑐 does not represent a character, an error of
;;     the type "IllegalArgumentError" is signaled.
;;   - If the character 𝑐 does not constitute a representable character,
;;     for instance because of the underlying system's incapability to
;;     employ the Unicode repertoire, an error of the type
;;     "IncompatibleEncodingError" is signaled.
;;   - If the standard output lacks amenability to the character
;;     printing operation, an error of the type "InputOutputError" is
;;     signaled.
;;     Several issues may constitute the etyology of such an aberrant
;;     respondency, embracing, without the claim of exhaustion:
;;     o The unavailability of a standard output, such as a missing
;;       physical screen device.
;;     o Denied authorization to commit behests to the standard output.
;;     o Inability of the standard output to issue characters, or at
;;       least such of the desiderated repertoire.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘~’: CHARACTER INPUT ==
;; The ‘~’ operator reads a character from the standard input and stores
;; it in the input cell.
;; 
;; Signature:
;;   ~
;; 
;; Parameters:
;;   None.
;; 
;; Interface:
;;   readCharacter() : void
;; 
;; Pseudocode:
;;   inputCell ← read character from standard input
;; 
;; Description:
;;   Queries the standard input for a single character and stores the
;;   response in the input cell.
;; 
;; Side effects:
;;   - A character query is issued to the system's standard input.
;;   - Upon success the character input cell is modified.
;; 
;; Exceptional situations:
;;   - If the standard input fails to respond to the character query,
;;     an error of the type "InputOutputError" is signaled.
;;     Several issues may constitute the etyology of such an aberrant
;;     respondency, embracing, without the claim of exhaustion:
;;     o The unavailability of a standard input, such as a missing
;;       keyboard device.
;;     o Denial of authorization to commit behests to the standard
;;       input.
;;     o Inability of the standard input to issue characters, or at
;;       least such of the desiderated repertoire.
;; 
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; 
;; == ‘'’: EXECUTE IF CHARACTER EQUALS ==
;; The ‘'’ construct executes the condition body once if and only if the
;; character in the input cell equals the specified character.
;; 
;; Signature:
;;   '𝑐/𝛬\
;; 
;; Parameters:
;;   𝑐 — The character to juxtapose to the input cell value.
;;   𝛬 — The ordered sequence of instructions (𝜆[1], …, 𝜆[N]) to execute.
;; 
;; Interface:
;;   executeIfCharacterEquals(𝑐, 𝛬) : void
;; 
;; Pseudocode:
;;   if inputCellCharacter = 𝑐
;;     loop for instruction 𝜆 in 𝛬 do
;;       execute 𝜆
;;     end loop
;;   end if
;; 
;; Description:
;;   Executes the instructions (𝜆[1], …, 𝜆[N]) from the ordered
;;   instruction sequence 𝛬 once if the character stored in the input
;;   cell equals the specified character 𝑐. Otherwise no effect is
;;   elicited.
;; 
;; Side effects:
;;   - Any side effect issued by an instruction 𝜆 from 𝛬 propagates
;;     through this conditional block.
;; 
;; Exceptional situations:
;;   - Any exceptional situation issued by an instruction 𝜆 from 𝛬
;;     propagates through this conditional block.
;; 
;; Examples:
;;   Example 1: Query a character from the user; if it equals the letter
;;              "a", print the string "yes".
;;     ~'a/"y"e"s\
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The throughout elucidations in the protolog dispel the contingency
;; for ambivalencies.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's implementation in Common Lisp founds upon a
;; tripartite division in lexer, parser, and interpreter. The latter
;; stage involves an array of particular difficulties in the dichotomy
;; betwixt finite and infinite sets.
;; 
;; == SOAP EVALUATIONS: THREE TIERS IN COEFFICIENCY ==
;; A SOAP program's execution encompasses a series of processes
;; enterparted into three strata, each such a dedicated agent's ambit:
;; 
;;   (1) A LEXER consumes the SOAP source code specified in string form,
;;       producing a series of token encapsulating the significant
;;       objects perceived and extracted during the analyzation process.
;;   (2) A PARSER digests the tokens provided by the lexer, assembling
;;       these into an abstract syntax tree (AST) representation of the
;;       initial SOAP code, with the involved language facilities
;;       enveloped into dedicated nodes.
;;   (3) The INTEPRETER accepts the abstract syntax tree, traversing its
;;       nodes in order to apply actual effects to the program
;;       constituents.
;; 
;; == THE EXISTENCE AND COEFFICIENCY OF INFINITY IMPOSES COMPLEXITIES ==
;; A particular object of difficulty, and the culprit to the ensuing
;; convolutions regarding the set handling, relates to the haecceity
;; residing in the set of natural numbers: its infinite account of
;; members.
;; 
;; == FINITE SETS DO NOT IMPOSE PERPLEXITY ==
;; A finite set's realization does torely impose a cumbrance unto an
;; implementation. Many programming language already proffer solutions
;; in their standard library for the respective data type; and, even
;; efficient solutions, may be conceived in an eath manner by one's own
;; effort. The pertinence of Common Lisp embraces the foundational list
;; type, ubiquitous in all of its aspects, to be utilized in such a
;; manner, mediated through responsible operations. In its most simple
;; notion, the set permits its manifestation as an arbitrary sequence,
;; such as a list, an array, or even a map, whose inclusion or ousting
;; of items depends upon their presence or absence.
;; 
;; Derivations from this lucidity, operations on single sets, as well as
;; such on multiple — usually two, whence further arities can be
;; extrapolated — siclike do not partake of confounding concepts. For
;; the former species, as a forbisen, the membership test involving an
;; item 𝑖 to be confirmed in its presence in a set 𝐴, or negated
;; therein, resolves to a simple search, as proved by this testimony in
;; Common Lisp parlace:
;; 
;;   (find 𝑖 𝐴)
;; 
;; Similiter, a binary operation's instance shall be an adduction's
;; material, presenting the union operator 𝐴∪𝐵 as a simple extension of
;; an extant set 𝐴 by those elements from 𝐵 not yet present:
;; 
;;   (let ((new-set (copy-list 𝐴)))
;;     (declare (type list new-set))
;;     (loop for candidate of-type T in 𝐵 do
;;       (unless (find candidate 𝐴)
;;         (push candidate 𝐴))))
;; 
;; Referring to the claim of such set operations' common availability,
;; Common Lisp offers the built-in function "union" itself:
;; 
;;   (union 𝐴 𝐵)
;; 
;; Nearly all formulae concerning set operations are capable of being
;; replicated without tergiversation in a modern programming language
;; for a *finite* set. This ability, however, fails to germinate in the
;; *infinite* realm, as the abstract mathematical notion presupposes
;; the contingency of iterating a finite tally of elements in a mode
;; paregal to infinite ones — this conceived competence eludes the
;; pragmatic reality of computer science.
;; 
;; == INFINITE SETS: THEORETICAL IDEALS COLLIDE WITH THE REALITY ==
;; In simple terms — and with respect to our previous examples —,
;; please attempt to find an element 𝑖 in an infinitely large set (or
;; sequence) 𝐴 deploying any programming language to your avail. The
;; solution ostended aboon in
;; 
;;   (find 𝑖 𝐴)
;; 
;; even if it would not violate Common Lisp's definition of a sequence
;; as restricted in the tally of its sequences' members, could for an
;; absent candidate 𝑖 never cease to search — thus testing until the
;; end of all secles. Likewise, of course, binary set operations are
;; inflicted with the same damning confines and conclusions.
;; 
;; == THE COMPLEMENT OPERATION "c" CAN PRODUCE AN INFINITE SET ==
;; Maugre its innocuous design, the main set 𝑀, initially empty and
;; thus finite, may devolve into an infinite specimen, by instigation of
;; the complement operation "c". Given the explicitly stated perimeter
;; of SOAP's integer type, the natural numbers excluding zero, but
;; extending into any mickleness of the positive gamut, by inverting a
;; finite main set, the complement, perforce, must assume all erstwhile
;; excluded elements — a boundless account of such exists.
;; 
;; The material issues thus emanate: How can we represent vexing
;; infinite sets in a programming language, and how can we attune these
;; with the less demanding finite sets?
;; 
;; == A DETAILED TREATISE ON THE SET TYPE PROBLEMS ==
;; Considering the existence of the two set categories, according to the
;; extent as the paravaunt criterion, and encompassing
;; 
;;  ( i) finite sets
;;  (ii) infinite sets
;; 
;; several predicaments appertaining to the ambit of programming invest
;; in the thus germinating dichotomy:
;; 
;;   (1) DISCREPANCIES IN THE MODELING OF FINITE VERSUS INFINITE SETS:
;;       The acquaintance allied with the finite set representation in
;;       programming languages has already been a treatise's subject;
;;       infinite sets, on the other hand, are torely incorporated into
;;       the standard library. It constitutes, natheless, a consequence
;;       of sane nouse that the latter case cannot be derived
;;       immediately from the former.
;;         Any finite set's foundry might be an eligible sequence, such
;;       as a simple list. — But how WILL infinite sequences be
;;       realized?
;;   (2) DISCREPANCIES IN NILADIC AND UNARY OPERATIONS:
;;       Ensuing from the intricacies of the representational
;;       department, the few operations reliant upon no argument besides
;;       the set itself, or at most a single non-set input, must be
;;       adapted to the dioristic haecceity of the unbounded ilk.
;;         If, as a forbisen, we intend to add an element to a finite
;;       versus an infinite set, the internal principles must be
;;       attended cautiously. A finite set simply embraces its new
;;       member by extending its underlying sequence. For the infinite
;;       species determining a candidate's absence and including it
;;       relay to a distinguished process.
;;   (3) COMPATIBILITY OF BINARY OPERATIONS:
;;       As just stated, the diverging diorisms promulgate from the
;;       representation into the operational facets; they experience an
;;       aggravation as binary set operations may involve both
;;       variants — finite and infinite — to evaluate in the same
;;       context, thus magnifying the discrepancies' causata.
;;         An example of this constitutes the question what kind of set
;;       would result from a union of an infinite and a finite input.
;;       A twain of finite specimens simply merges the entirety of its
;;       donors, while barring duplicates, into a new finite result.
;;       Faced with one or two participants being of the unbounded type,
;;       does a finite or infinite legatee enter into existence, and
;;       how shall the potentially incompatible models merge?
;; 
;; In order to attend to these predicaments, the constrasting jumelle is
;; subjected to a pair of abstract representations:
;; 
;;   (1) The "inclusive set" as a manifestation of the finite set.
;;   (2) The "exclusive set" as a manifestation of the infinite set.
;; 
;; == INCLUSIVE SETS: THE EMPTY SET AUGMENTED BY A WHITELIST ==
;; "Inclusive" sets apply themselves to the representation of finite
;; sets. Their content, satisfied by their elements, is established
;; through a direct castaldy in an eligible sequence.
;; 
;; Any element not mentioned in this sequence does not partake of the
;; set. Given the boundless extend of the universe 𝕌, an infinite tally
;; of elements does not contribute to the inclusive specimen. In this
;; light, an inclusive set concords with an "additive" concept, in that
;; its membership — the foundation anchored in an inchoate
;; identification with the empty set Ø — develops proportionally to the
;; insertion of new candidates; removals diminish the cardinality.
;; 
;; Maintaining its members by adminiculum of an accounting sequence, the
;; same can be regarded as a "whitelist" — excluding all the innumerable
;; items from the universe 𝕌 not — or not yet — permitted admission.
;; 
;; The inclusion of a yet absent element constitutes an insertion in
;; this whitelist.
;; 
;; The removal of an object, that is, one whose present can be attested
;; in the whitelist, renders tantamount to a deletion from the same
;; sequence.
;; 
;; == EXCLUSIVE SETS: THE UNIVERSE REDUCED BY A BLACKLIST ==
;; An athwart approach designates the "exclusive" sets, the
;; representatives of infinite sets. Their description proceeds from the
;; exclusion of a finite amount of elements from the endless universe 𝕌.
;; 
;; Proceeding from an initial equivalency with the universe 𝕌 and its
;; complete coverage of all objects commorant therein, a finite sequence
;; is maintained which determines which members of 𝕌 do NOT partake of
;; this exclusive set. This list, norned the "blacklist", ostends in its
;; incipiency a vacant state.
;; 
;; If an element shall be removed from the exclusive set, it is
;; registered in the blacklist, in corollary being excised from the copy
;; of the infinite universe 𝕌.
;; 
;; Adding an absent object, that is, one currently subscribing to the
;; blacklist, resolves to its removal from the same.
;; 
;; A visual equiparation anenst the inclusive and exclusive set concepts
;; shall be the following table's onus:
;; 
;;   ------------------------------------------------------------------
;;               | Inclusive set            | Exclusive set
;;   ------------+--------------------------+--------------------------
;;   Foundation  | Empty set Ø.             | Universe 𝕌.
;;   ..................................................................
;;   Cardinality | Finite.                  | Infinite.
;;   ..................................................................
;;   Storage     | "Whitelist" of present   | "Blacklist" of absent
;;               | objects from universe 𝕌. | objects from universe 𝕌.
;;   ..................................................................
;;   Insertion   | Add element 𝑥 to the     | Remove element 𝑥 from the
;;               | whitelist.               | blacklist.
;;   ..................................................................
;;   Removal     | Remove element 𝑥 from    | Add element 𝑥 to the
;;               | the whitelist.           | blacklist.
;;   ------------------------------------------------------------------
;; 
;; Our capacitation to contemplate sets as inclusive (additive) or
;; exclusive (subtractive) models homologates a perquisition into their
;; operations.
;; 
;; == UNARY SET OPERATIONS ==
;; The unary set operations are restricted to a single specimen only,
;; exhausted by the complement 𝐴' of an input set 𝐴. The illustration
;; below shall educate about its realization:
;; 
;;   ------------------------------------------------------------------
;;   Op. | 𝐴   | Result 𝐴'
;;   ----+-----+-------------------------------------------------------
;;   𝐴'  | Ex. | An inclusive set which is produced by embracing all
;;       |     | blacklisted members of 𝐴 as the elements of 𝐴':
;;       |     |   𝐴'.elements = 𝐴.blacklist
;;       |.............................................................
;;       | In. | An exclusive set which is produced by designating all
;;       |     | elements of 𝐴 as blacklisted members of 𝐴':
;;       |     |   𝐴'.blacklist = 𝐴.elements
;;   ------------------------------------------------------------------
;; 
;; == BINARY SET-TO-ELEMENT OPERATIONS ==
;; The perimeter of our interest regarding the first, less assuming ilk
;; of binary operations embraces the coefficiency of a single set 𝐴 and
;; an arbitrary second non-set object 𝑖 in the questions of membership.
;; 
;; In the SOAP programming language's categorization, this datum 𝑖
;; always presents a natural number greater than zero (0).
;; 
;; The following table abbreviates the exclusive form of A using "Ex.",
;; whereas the inclusive variant is curtailed to "In.".
;; 
;;   ------------------------------------------------------------------
;;   Operation | 𝐴   | Result 𝐴*
;;   ----------+-----+-------------------------------------------------
;;   Insertion | Ex. | An exclusive set which is produced by removing
;;             |     | from 𝐴's blacklist the element 𝑖:
;;             |     |   𝐴*.blacklist = 𝐴.blacklist - {𝑖}
;;             |.......................................................
;;             | In. | An inclusive set which is produced by adding to
;;             |     | 𝐴's elements the element 𝑖:
;;             |     |  𝐴*.elements = 𝐴.elements ∪ {𝑖}
;;   ------------------------------------------------------------------
;;   Removal   | Ex. | An exclusive set which is produced by adding to
;;             |     | 𝐴's blacklist the element 𝑖:
;;             |     |   𝐴*.blacklist = 𝐴.blacklist ∪ {𝑖}
;;             |.......................................................
;;             | In. | An inclusive set which is produced by removing
;;             |     | from 𝐴's elements the element 𝑖:
;;             |     |  𝐴*.elements = 𝐴.elements - {𝑖}
;;   ------------------------------------------------------------------
;; 
;; == BINARY SET-TO-SET OPERATIONS ==
;; The probably most common type of vinculum applicable to more than one
;; set, the binary operations' principles in regard to the various cases
;; of external and internal specimens shall be subjected to a deeper
;; perquisition.
;; 
;; For binary operations or relations, each set type combination assumes
;; its location in the table in a fixed order:
;; 
;;   ---------------------
;;   Set 𝐴     | Set 𝐵
;;   ----------+----------
;;   Exclusive | Exclusive
;;   Exclusive | Inclusive
;;   Inclusive | Exclusive
;;   Inclusive | Inclusive
;;   ---------------------
;; 
;; A recapitulation of the three available operations is exhausted in
;; this table. Please notice that the tabular exposition succeeding this
;; immediate illustration will only refer to the formulae, owing to
;; space constraints.
;; 
;;   ------------------------------------------------------------------
;;   Operation name | Formula | Apostil
;;   ---------------+---------+----------------------------------------
;;   Union          | 𝐴∪𝐵     | -
;;   ..................................................................
;;   Intersection   | 𝐴∩𝐵     | -
;;   ..................................................................
;;   Difference     | 𝐴-𝐵     | The difference operator is not
;;                  |         | commutative, that is, in general it
;;                  |         | holds:
;;                  |         |   𝐴-𝐵 ≠ 𝐵-𝐴.
;;                  |         | The former case, 𝐴-𝐵, is agnominated
;;                  |         | "left difference" in this specification
;;                  |         | — a term forinsecal to the actual set
;;                  |         | theory; the opposite case of 𝐵-𝐴 has
;;                  |         | been denoted as "right difference".
;;   ------------------------------------------------------------------
;; 
;; The following tetrad of columns allots the first portion to the
;; discussed binary set operation, succeeded by the combinations of
;; external ("Ex.") and inclusive ("In.") sets 𝐴 and 𝐵, and concluding
;; with the resulting set 𝐶's diorism.
;; 
;; Please note for the set difference that, maugre the general case of
;; 
;;   𝐴-𝐵 ≠ 𝐵-𝐴
;; 
;; the juxtapositions of set types (exclusive versus inclusive) retain
;; validity in their causata; as a corollary, the table will only
;; maintain the case of 𝐴-𝐵.
;; 
;;   ------------------------------------------------------------------
;;   Op. | 𝐴   | 𝐵   | Result 𝐶
;;   ----+-----+-----+-------------------------------------------------
;;   𝐴∪𝐵 | Ex. | Ex. | An exclusive set which is produced by removing
;;       |     |     | from 𝐴's blacklist exactly those elements not
;;       |     |     | not contained in 𝐵's blacklist.
;;       |     |     | Hence, 𝐶's blacklist is the intersection of
;;       |     |     | those of 𝐴 and 𝐵:
;;       |     |     |   𝐶.blacklist = 𝐴.blacklist ∩ 𝐵.blacklist
;;       |.............................................................
;;       | Ex. | In. | An exclusive set which is produced by removing
;;       |     |     | from 𝐴's blacklist exactly those elements
;;       |     |     | contained in 𝐵's elements.
;;       |     |     | Hence, 𝐶's blacklist is the difference of 𝐴's
;;       |     |     | blacklist reduced by 𝐵's elements:
;;       |     |     |   𝐶.blacklist = 𝐴.blacklist - 𝐵.elements
;;       |.............................................................
;;       | In. | Ex. | An exclusive set which is produced by removing
;;       |     |     | from 𝐵's blacklist exactly those elements
;;       |     |     | contained in 𝐴's elements.
;;       |     |     | Hence, 𝐶's blacklist is the difference of 𝐵's
;;       |     |     | blacklist reduced by 𝐴's elements:
;;       |     |     |   𝐶.blacklist = 𝐵.blacklist - 𝐴.elements
;;       |.............................................................
;;       | In. | In. | An inclusive set which is produced by adding to
;;       |     |     | 𝐴's elements exactly those elements contained in
;;       |     |     | 𝐵 but not yet extant in 𝐴.
;;       |     |     |   𝐶.elements = 𝐴.elements ∪ 𝐵.elements
;;   ------------------------------------------------------------------
;;   𝐴∩𝐵 | Ex. | Ex. | An exclusive set which contains all elements
;;       |     |     | from the universe 𝕌, except for those contained
;;       |     |     | in 𝐴's blacklist and/or 𝐵's blacklist.
;;       |     |     | Hence, 𝐶's blacklist is the union of 𝐴's and
;;       |     |     | 𝐵's blacklists:
;;       |     |     |   𝐶.blacklist = 𝐴.blacklist ∪ 𝐵.blacklist
;;       |.............................................................
;;       | Ex. | In. | An inclusive set which contains exactly those
;;       |     |     | members from 𝐵's elements which are not
;;       |     |     | contained in 𝐴's blacklist.
;;       |     |     | 𝐶 may at most include 𝐵's elements and none
;;       |     |     | other from the universe 𝕌. Deducted from these
;;       |     |     | are the blacklisted members of 𝐴.
;;       |     |     | Hence, 𝐶's elements are the difference of
;;       |     |     | 𝐵's elements reduced by 𝐴's blacklist:
;;       |     |     |   𝐶.elements = 𝐵.elements - 𝐴.blacklist
;;       |.............................................................
;;       | In. | Ex. | An inclusive set which contains exactly those
;;       |     |     | members from 𝐴's elements which are not
;;       |     |     | contained in 𝐵's blacklist.
;;       |     |     | 𝐶 may at most include 𝐴's elements and none
;;       |     |     | other from the universe 𝕌. Deducted from these
;;       |     |     | are the blacklisted members of 𝐵.
;;       |     |     | Hence, 𝐶's elements are the difference of
;;       |     |     | 𝐴's elements reduced by 𝐵's blacklist:
;;       |     |     |   𝐶.elements = 𝐴.elements - 𝐵.blacklist
;;       |.............................................................
;;       | In. | In. | An inclusive set which contains exactly those
;;       |     |     | elements shared by 𝐴 and 𝐵:
;;       |     |     |   𝐶.elements = 𝐴.elements ∩ 𝐵.elements
;;   ------------------------------------------------------------------
;;   𝐴-𝐵 | Ex. | Ex. | An inclusive set which contains exactly those
;;       |     |     | elements contained in 𝐴's blacklist but not
;;       |     |     | concomitantly blacklisted by 𝐵.
;;       |     |     | This operation removes from 𝐴 all elements of
;;       |     |     | the universe 𝕌 except for those blacklisted by 𝐵
;;       |     |     | but not blacklisted by 𝐴.
;;       |     |     | The elements in 𝐴's blacklist do not exist in 𝐴
;;       |     |     | anyhow; whereas those only excluded from 𝐵 are
;;       |     |     | not removed from 𝐴, as they elude 𝐵 itself.
;;       |     |     | Hence, 𝐶's elements are the difference of 𝐵's
;;       |     |     | blacklist reduced by 𝐴's blacklist:
;;       |     |     |   𝐶.elements = 𝐵.blacklist - 𝐴.blacklist
;;       |.............................................................
;;       | Ex. | In. | An exclusive set which is produced by extending
;;       |     |     | 𝐴's blacklist by 𝐵's elements.
;;       |     |     | Hence, 𝐶's blacklist is the union of 𝐴's
;;       |     |     | blacklist and 𝐵's elements:
;;       |     |     |   𝐶.blacklist = 𝐴.blacklist ∪ 𝐵.elements
;;       |.............................................................
;;       | In. | Ex. | An inclusive set which is produced by removing
;;       |     |     | from 𝐴 all elements except for those blacklisted
;;       |     |     | by 𝐵.
;;       |     |     | Hence, 𝐶's elements are intersection of 𝐴's
;;       |     |     | elements and 𝐵's blacklist:
;;       |     |     |   𝐶 = 𝐴.elements ∩ 𝐵.blacklist
;;       |.............................................................
;;       | In. | In. | An inclusive set which is produced by removing
;;       |     |     | from 𝐴 those elements also present in 𝐵:
;;       |     |     |   𝐶 = 𝐴.elements - 𝐵.elements
;;   ------------------------------------------------------------------
;; 
;; == SET PREDICATES ==
;; Set predicates assume the guise of binary relationships betwixt two
;; sets which may be different or identical. In its haecceity functions
;; of two arguments, their response always resolves to a Boolean value.
;; 
;; For binary operations or relations, each set type combination assumes
;; its location in the table in a fixed order:
;; 
;;   ---------------------
;;   Set 𝐴     | Set 𝐵
;;   ----------+----------
;;   Exclusive | Exclusive
;;   Exclusive | Inclusive
;;   Inclusive | Exclusive
;;   Inclusive | Inclusive
;;   ---------------------
;; 
;; A recapitulation of the seven available relations is exhausted in
;; this table. Please notice that the tabular exposition succeeding this
;; immediate illustration will only refer to the formulae, owing to
;; space constraints.
;; 
;;   ------------------------------------------------------------------
;;   Relation name   | Formula | Apostil
;;   ----------------+---------+---------------------------------------
;;   Subset          | 𝐴⊆𝐵     | It holds: 𝐴⊆𝐵 ⟺ 𝐵⊇𝐴.
;;   ..................................................................
;;   Proper subset   | 𝐴⊂𝐵     | It holds: 𝐴⊂𝐵 ⟺ 𝐵⊃𝐴.
;;   ..................................................................
;;   No subset       | 𝐴⊄𝐵     | -
;;   ..................................................................
;;   Superset        | 𝐴⊇𝐵     | It holds: 𝐴⊇𝐵 ⟺ 𝐵⊆𝐴.
;;   ..................................................................
;;   Proper superset | 𝐴⊃𝐵     | It holds: 𝐴⊃𝐵 ⟺ 𝐵⊂𝐴.
;;   ..................................................................
;;   No superset     | 𝐴⊅𝐵     | -
;;   ..................................................................
;;   Equal           | 𝐴=𝐵     | -
;;   ------------------------------------------------------------------
;; 
;; In the following tabular exposition, the input sets are denoted by
;; 𝐴 and 𝐵, mapping to the Boolean output 𝑥.
;; 
;;   ------------------------------------------------------------------
;;   Op. | 𝐴   | 𝐵   | Result 𝑥
;;   ----+-----+-----+-------------------------------------------------
;;   𝐴⊆𝐵 | Ex. | Ex. | Satisfied if every member of 𝐴's blacklist
;;       |     |     | also constitues a member of 𝐵's blacklist.
;;       |     |     | 𝐵 may, of course, contain additional blacklist
;;       |     |     | elements not shared by 𝐴.
;;       |     |     |   ∀ 𝑎 ∈ 𝐴.blacklist : 𝑎 ∈ 𝐵.blacklist
;;       |.............................................................
;;       | Ex. | In. | Always false, as an exclusive set is infinite,
;;       |     |     | whereas an inclusive set is finite.
;;       |.............................................................
;;       | In. | Ex. | Satisfied if no member of 𝐴's elements is
;;       |     |     | present in 𝐵's blacklist.
;;       |     |     |   ∀ 𝑎 ∈ 𝐴.elements : 𝑎 ∉ 𝐵.blacklist
;;       |.............................................................
;;       | In. | In. | Satisfied if every member of 𝐴's elements also
;;       |     |     | constitutes a member of 𝐵's elements.
;;       |     |     | 𝐵 may, of course, contain additional elements
;;       |     |     | not shared by 𝐴.
;;       |     |     |   ∀ 𝑎 ∈ 𝐴.elements : 𝑎 ∈ 𝐵.elements
;;   ------------------------------------------------------------------
;;   𝐴⊂𝐵 | Ex. | Ex. | Satisfied if every member of 𝐴's blacklist
;;       |     |     | also constitues a member of 𝐵's blacklist, while
;;       |     |     | concomitantly the cardinality |𝐴| of 𝐴's
;;       |     |     | blacklist is less than the cardinality |𝐵| of
;;       |     |     | 𝐵's blacklist.
;;       |     |     |   (∀ 𝑎 ∈ 𝐴.blacklist : 𝑎 ∈ 𝐵.blacklist)
;;       |     |     |   ∧
;;       |     |     |   (|𝐴.blacklist| < |𝐵.blacklist|)
;;       |.............................................................
;;       | Ex. | In. | Always false, as an exclusive set is infinite,
;;       |     |     | whereas an inclusive set is finite.
;;       |.............................................................
;;       | In. | Ex. | Satisfied if no member of 𝐴's elements is
;;       |     |     | present in 𝐵's blacklist.
;;       |     |     | Its finite nature renders 𝐴 by its haecceity a
;;       |     |     | subset of the infinite 𝐵.
;;       |     |     |   ∀ 𝑎 ∈ 𝐴.elements : 𝑎 ∉ 𝐵.blacklist
;;       |.............................................................
;;       | In. | In. | Satisfied if every member of 𝐴's elements also
;;       |     |     | constitutes a member of 𝐵's elements, while
;;       |     |     | concomitantly the cardinality of 𝐴's elements is
;;       |     |     | less than that of 𝐵's.
;;       |     |     |   (∀ 𝑎 ∈ 𝐴.elements : 𝑎 ∈ 𝐵.elements)
;;       |     |     |   ∧
;;       |     |     |   (|𝐴.elements| < |𝐵.elements|)
;;   ------------------------------------------------------------------
;;   𝐴⊄𝐵 | Ex. | Ex. | Satisfied if 𝐴⊆𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊄𝐵 = ¬(𝐴⊆𝐵)
;;       |.............................................................
;;       | Ex. | In. | Satisfied if 𝐴⊆𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊄𝐵 = ¬(𝐴⊆𝐵)
;;       |.............................................................
;;       | In. | Ex. | Satisfied if 𝐴⊆𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊄𝐵 = ¬(𝐴⊆𝐵)
;;       |.............................................................
;;       | In. | In. | Satisfied if 𝐴⊆𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊄𝐵 = ¬(𝐴⊆𝐵)
;;   ------------------------------------------------------------------
;;   𝐴⊇𝐵 | Ex. | Ex. | Equivalent to 𝐵⊆𝐴.
;;       |.............................................................
;;       | Ex. | In. | Equivalent to 𝐵⊆𝐴.
;;       |.............................................................
;;       | In. | Ex. | Equivalent to 𝐵⊆𝐴.
;;       |.............................................................
;;       | In. | In. | Equivalent to 𝐵⊆𝐴.
;;   ------------------------------------------------------------------
;;   𝐴⊃𝐵 | Ex. | Ex. | Equivalent to 𝐵⊂𝐴.
;;       |.............................................................
;;       | Ex. | In. | Equivalent to 𝐵⊂𝐴.
;;       |.............................................................
;;       | In. | Ex. | Equivalent to 𝐵⊂𝐴.
;;       |.............................................................
;;       | In. | In. | Equivalent to 𝐵⊂𝐴.
;;   ------------------------------------------------------------------
;;   𝐴⊅𝐵 | Ex. | Ex. | Satisfied if 𝐴⊇𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊅𝐵 = ¬(𝐴⊇𝐵)
;;       |.............................................................
;;       | Ex. | In. | Satisfied if 𝐴⊇𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊅𝐵 = ¬(𝐴⊇𝐵)
;;       |.............................................................
;;       | In. | Ex. | Satisfied if 𝐴⊇𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊅𝐵 = ¬(𝐴⊇𝐵)
;;       |.............................................................
;;       | In. | In. | Satisfied if 𝐴⊇𝐵 is not satisfied, that is, it
;;       |     |     | holds:
;;       |     |     |   𝐴⊅𝐵 = ¬(𝐴⊇𝐵)
;;   ------------------------------------------------------------------
;;   𝐴=𝐵 | Ex. | Ex. | Satisfied if the blacklist of 𝐴 contains exactly
;;       |     |     | the same members as the blacklist of 𝐵.
;;       |     |     |   (∀ 𝑎 ∈ 𝐴.blacklist : 𝑎 ∈ 𝐵.blacklist)
;;       |     |     |   ∧
;;       |     |     |   (|𝐴.blacklist| = |𝐵.blacklist|)
;;       |.............................................................
;;       | Ex. | In. | Always false, as an exclusive set is infinite,
;;       |     |     | whereas an inclusive set is finite.
;;       |.............................................................
;;       | In. | Ex. | Always false, as an exclusive set is infinite,
;;       |     |     | whereas an inclusive set is finite.
;;       |.............................................................
;;       | In. | In. | Satisfied if every member of 𝐴's elements also
;;       |     |     | constitutes a member of 𝐵's elements, while
;;       |     |     | concomitantly the cardinality of 𝐴's elements is
;;       |     |     | equal to that of 𝐵's.
;;       |     |     |   (∀ 𝑎 ∈ 𝐴.elements : 𝑎 ∈ 𝐵.elements)
;;       |     |     |   ∧
;;       |     |     |   (|𝐴.elements| = |𝐵.elements|)
;;   ------------------------------------------------------------------
;; 
;; Having accumulated sufficient gnarity with the topic of inclusive and
;; exclusive set manifestations, the more palpable administration in the
;; context of the interpreter's implementations shall be promoted to our
;; cynosure.
;; 
;; == SOAPSETS: A DICHOTOMY INTO INCLUSIVE AND EXCLUSIVE SETS ==
;; The two set variants' common foundry is established upon the
;; "SOAPSet" interface, a slim and abstract component of little
;; expressive puissance. Its desistence from any actual data's
;; provision, consequently depriving it of an abstract class' substance,
;; respects the stark segregation in its two specializations'
;; principles:
;; 
;;   (1) the finite     cardinality "InclusiveSOAPSet" and
;;   (2) the infinitely large       "ExclusiveSOAPSet",
;; 
;; both complements of each other's notions.
;; 
;; A recapitulation shall quickly reiterate the set-to-class relations,
;; the mathematical terminology located to the sinistral column, the
;; programming counterpart to the dextral:
;; 
;;   --------------------------------
;;   Set type      | Class
;;   --------------+-----------------
;;   Inclusive set | InclusiveSOAPSet
;;   Exclusive set | ExclusiveSOAPSet
;;   --------------------------------
;; 
;; Please note that the implementation's naming conventions endeavor to
;; retain some abstract validity, thus assuming the principles
;; acquainted with many open standards and widespread programming
;; languages, norned frequently "camel-case". Common Lisp, however,
;; adhibits a contrasting identifier design in segregating words by
;; mediation of hyphens. This latter, concrete case, as its more closely
;; conforms to the provided implementation, will yet be subordinate in
;; order to facilitate the topic's comprehension irregardless of the
;; computer science background. Mainly those passage that relate to the
;; specifics of our concrete realization will engage in the Lisp
;; parlance.
;; 
;; The following table extends that aboon by the Common Lisp class
;; names:
;; 
;;   ------------------------------------------------------------------
;;   Set type      | General class    | Common Lisp class
;;   --------------+------------------+--------------------------------
;;   Inclusive set | InclusiveSOAPSet | Inclusive-SOAPSet
;;   Exclusive set | ExclusiveSOAPSet | Exclusive-SOAPSet
;;   ------------------------------------------------------------------
;; 
;; The following UML class diagram shall illustrate in a rather coarse
;; manner the tripartite constituents and their relationships:
;; 
;;                  ┌───────────────┐
;;                  │ <<interface>> │
;;                  │    SOAPSet    │
;;                  ├───────────────┤
;;                  └───────────────┘
;;                    △          △
;;                    ┆           ┆
;;                    ┆           ┆
;;   ┌──────────────────┐       ┌──────────────────┐
;;   │ InclusiveSOAPSet │       │ ExclusiveSOAPSet │
;;   ├──────────────────┤       ├──────────────────┤
;;   └──────────────────┘       └──────────────────┘
;; 
;; == "InclusiveSOAPSet": A LIST OF PRESENT ELEMENTS ==
;; In selecting a medium for the inclusive set type, the dedicated
;; ``InclusiveSOAPSet'' class is consigned to the represented concept's
;; tenets.
;; 
;; The simplicity of economy appertaining to a finite member cardinality
;; sanctions a sequence's employment. The choice favors the Common Lisp
;; ``list'' type as a natural warkloom. Not only does this ilk of
;; storage appropriate a paravaunt role in the Lisp family, and, ensuing
;; from this circumstance, enjoys a very generous array of available
;; operations; the list itself already features among the proffered
;; solutions to the set abstract data type (ADT).
;; 
;; An apostil shall be adduced regarding the vices present in this
;; rather straightforward approach: Maugre its vindication in the
;; context of a scant cardinality — whatever mete one might attach to
;; the subjective reckoning —, more advanced representations prefer a
;; hashed solution, for instance by a hash table's reappropriation
;; whose keys assume the elements, and whose key's presence determines
;; the membership, vouchsafing no significance to the value. Removing an
;; elements becomes tantamount to deleting the entry the key of which
;; identifies with the same. Our project with its prototypical valure
;; resorts to the eath comprehension and handling of the list, however.
;; 
;; Referring to the inclusive set definition, elements can be construed
;; as items in a "whitelist", with any other entity from the universe 𝕌
;; assayed as currently denied the adit.
;; 
;; The SOAP programming language's restriction to natural numbers as the
;; sole currency active in the data aspect reverberates in the list's
;; specialization on these unsigned positive integers.
;; 
;; == "Exclusive-SOAPSet": A LIST OF ABSENT ELEMENTS ==
;; The exclusive set type's potentials encompass the circumference of
;; infinite cardinality, capacitated to define membership in terms of
;; the candidates' absence in a "blacklist".
;; 
;; In an approach parallel to the "InclusiveSOAPSet"'s whitelist, the
;; blacklist's negative workings realize in the form of a Common Lisp
;; ``list'', again appropriated in the role of a set — this time,
;; however, enumerating those objects from the universe 𝕌 not present in
;; the set; all items in the exclusive instance's blacklist thus are
;; not members of the represented set, whereas the infinite elements of
;; 𝕌 are considered to participate in the same.
;; 
;; An absent element's inclusion into the exclusive set amounts to a
;; deletion from the blacklist, as its absence constitutes a tantamount
;; to its occurrence in this list. A present item's exclusion, on the
;; other hand, resolves to its insertion into the blacklist, forecause
;; any object not embraced therein is regarded as a member.
;; 
;; Iterum, as with any set of the SOAP language, all elements can only
;; amount to natural numbers; as a consectary, the blacklist specializes
;; on the storage of unsigned positive integers.
;; 
;; The reception of intelligence anenst the two "SOAPSet"
;; specializations merits a slightly enhanced iteration of the
;; acquainted class diagram:
;; 
;;                  ┌───────────────┐
;;                  │ <<interface>> │
;;                  │    SOAPSet    │
;;                  ├───────────────┤
;;                  └───────────────┘
;;                    △          △
;;                    ┆           ┆
;;                    ┆           ┆
;;   ┌──────────────────┐       ┌──────────────────┐
;;   │ InclusiveSOAPSet │       │ ExclusiveSOAPSet │
;;   ├──────────────────┤       ├──────────────────┤
;;   │ -elements        │       │ -blacklist       │
;;   └──────────────────┘       └──────────────────┘
;; 
;; The "SOAPSet" interface participates in the definition of a common
;; base for the tangible manifestations as "InclusiveSOAPSet" and
;; "ExclusiveSOAPSet", as much as it determines the various set
;; operations.
;; 
;; A more complete explication of the UML class diagram, embracing the
;; available set operations, shall finally be adduced:
;; 
;;          ┌─────────────────────────────────────────────────┐
;;          │                  <<interface>>                  │
;;          │                     SOAPSet                     │
;;          ├─────────────────────────────────────────────────┤
;;          │ +flipMembership(i : naturalNumber)    : boolean │
;;          │ +complement()                         : SOAPSet │
;;          │ +union(otherSet : SOAPSet)            : SOAPSet │
;;          │ +intersection(otherSet : SOAPSet)     : SOAPSet │
;;          │ +leftDifference(otherSet : SOAPSet)   : SOAPSet │
;;          │ +rightDifference(otherSet : SOAPSet)  : SOAPSet │
;;          | +subsetOf(otherSet : SOAPSet)         : boolean |
;;          | +properSubsetOf(otherSet : SOAPSet)   : boolean |
;;          | +notSubsetOf(otherSet : SOAPSet)      : boolean |
;;          | +supersetOf(otherSet : SOAPSet)       : boolean |
;;          | +properSupersetOf(otherSet : SOAPSet) : boolean |
;;          | +notSupersetOf(otherSet : SOAPSet)    : boolean |
;;          | +equals(otherSet : SOAPSet)           : boolean |
;;          └─────────────────────────────────────────────────┘
;;                             △          △
;;                             ┆           ┆
;;                             ┆           ┆
;;   ┌──────────────────────────────┐ ┌───────────────────────────────┐
;;   │       InclusiveSOAPSet       │ │       ExclusiveSOAPSet        │
;;   ├──────────────────────────────┤ ├───────────────────────────────┤
;;   │ -elements : naturalNumber[*] │ │ -blacklist : naturalNumber[*] │
;;   └──────────────────────────────┘ └───────────────────────────────┘
;; 
;; == APOSTIL: COMMON LISP'S OBJECT-ORIENTED MODEL ==
;; Common Lisp adheres to an object-oriented approach whose dispatchment
;; facilities are generally esteemed as more potent than the widespread
;; class- or instance-oriented approaches championed by the forbisens
;; established through C++, Java, or JavaScript. In Common Lisp's
;; comprehension, "methods" do not belong to their ensconcing classes'
;; or instances' possessions, but in lieu of this act as independent
;; entities. Methods are not defined and overwritten by classes; rather,
;; so-called generic functions are implemented by specializing the
;; realizing function's argument list on types.
;; 
;; Supplied as an example, the "SOAPSet" operation "union", which
;; merges two participating sets' elements into a new object, would be
;; defined in the interface "SOAPSet" and implemented in the class
;; "InclusiveSOAPSet" in a Java programming context as such:
;; 
;;   public interface SOAPSet
;;   {
;;     abstract public SOAPSet union (SOAPSet rightSet);
;;   }
;;   
;;   public class InclusiveSOAPSet implements SOAPSet
;;   {
;;     // Fields and constructors omitted.
;;     
;;     public SOAPSet union (SOAPSet rightSet)
;;     {
;;       // Implementation omitted.
;;     }
;;   }
;; 
;; The "union" method would be established in the ownership of the
;; "SOAPSet" interface and bequeathed for its realization in
;; the "InclusiveSOAPSet" concrete class.
;; 
;; The Common Lisp approach veers from this notion in diassociating
;; functions from classes. Interfaces do not exist in the language.
;; 
;;   (defgeneric soapset-union (left-set right-set))
;;   
;;   (defclass Inclusive-SOAPSet (SOAPSet)
;;     (
;;       ;; Slots omitted.
;;     )
;;   )
;;   
;;   (defmethod soapset-union ((left-set  Inclusive-SOAPSet)
;;                             (right-set SOAPSet))
;;     ;; Implementation omitted.
;;   )
;; 
;; Please note that the name "soapset-union" has been chosen in order to
;; obviate a confounding with the already extant Common Lisp built-in
;; function "union". The "soapset-union" method is neither defined in an
;; interface, as such constructs register no commorancy in the language,
;; nor does its stated implementation physically belong to the
;; "Inclusive-SOAPSet" class.
;; 
;; 
;; == APPENDIX A: PROJECT FILES ==
;; The extensive perimeter intrinsic to this project has been reckoned
;; to embrace a complexity sufficiently potent as to impose a
;; destructuring into several interrelated and coefficent files. The
;; order of their importing into a main context constitutes a variable
;; of a fixed ordonnance, elucidated in the following table.
;; 
;; Please note that at least one infrastructure of nearly official
;; weight exists for such project management purposes, realized in the
;; definition of file interfaces using packages, and their orders and
;; relationships' enunciation by the "ASDF" system. This simple example,
;; however, has been adjudged as rather inflicted with a digressive
;; cumbrance in an advenient structuring's adhibition, rather than its
;; enjoying — a more serious enterprise certainly would be assayed in an
;; athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File             | Role
;;   ----+------------------+------------------------------------------
;;    1  | types.lisp       | Defines the custom types employed in the
;;       |                  | subsequent files, such as ``list-of'' and
;;       |                  | ``destination''.
;;   ..................................................................
;;    2  | token.lisp       | Implements the tokens, which are produced
;;       |                  | by the lexer during the scanning of the
;;       |                  | SOAP source code string.
;;   ..................................................................
;;    3  | lexer.lisp       | Implements the lexer, responsible for
;;       |                  | extracting the tokens from a piece of
;;       |                  | SOAP source code specified in string
;;       |                  | form.
;;   ..................................................................
;;    4  | node.lisp        | Implements the abstract syntax tree (AST)
;;       |                  | nodes, each such encapsulating a SOAP
;;       |                  | language construct, such as a loop or a
;;       |                  | binary set operation with its operands,
;;       |                  | intended to be assembled by a parser.
;;   ..................................................................
;;    5  | parser.lisp      | Implements the parser, whose onus it is
;;       |                  | to assemble the tokens generated by the
;;       |                  | lexer into an abstract syntax tree (AST),
;;       |                  | compact of nodes, and eligible for an
;;       |                  | evaluation by the interpreter.
;;   ..................................................................
;;    6  | soapset.lisp     | Implements the set abstract data type
;;       |                  | (ADT) in the form of "SOAPSets",
;;       |                  | entailing all requisite operations for
;;       |                  | the interpreter to apply during the
;;       |                  | processing of the respective abstract
;;       |                  | syntax tree (AST) nodes.
;;   ..................................................................
;;    7  | interpreter.lisp | Implements the interpreter, the agent
;;       |                  | tasked with traversing the abstract
;;       |                  | syntax tree (AST) produced by the parser
;;       |                  | in order to induce actual effect.
;;   ..................................................................
;;    8  | tests.lisp       | Implements the test cases and examples
;;       |                  | for demonstrating the interpreter's
;;       |                  | conformance with the SOAP programming
;;       |                  | language.
;;   ..................................................................
;;    9  | main.lisp        | Establishes the starting point into this
;;       |                  | application, in particular loading the
;;       |                  | aforementioned Common Lisp source files.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-02-18
;; 
;; Sources:
;;   [compart2012unicodemathalphsym]
;;     Title:    Unicode Block “Mathematical Alphanumeric Symbols”
;;     Author:   Compart Deutschland GmbH
;;     Year:     2021
;;     URL:      https://www.compart.com/en/unicode/block/U+1D400
;;     Accessed: 2023-02-18
;;     Notes:
;;     - Unicode block for mathematical alphanumeric symbols, utile as
;;       a source for the research and insertion of the same.
;;   
;;   [esolang2022soap]
;;     Title:    SOAP - Esolang
;;     Author:   Various
;;     Year:     2022
;;     URL:      https://esolangs.org/wiki/SOAP
;;     Accessed: 2023-02-17
;;     Notes:
;;     - Specification of the esoteric programming language "SOAP".
;;   
;;   [steele1990cltlang2nd]
;;     Title:    Common Lisp the Language, 2nd Edition
;;     Author:   Guy L. Steele Jr.
;;     Year:     1990
;;     Section:  Chapter 15.5 "Using Lists as Sets"
;;     URL:      https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node152.html
;;     Accessed: 2023-02-17
;;     Notes:
;;     - Describes the deployment of Common Lisp lists as a
;;       representation of the set data type.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type pathname *project-directory*))

;;; -------------------------------------------------------

(defparameter *project-directory*
  (make-pathname)
  "Specifies the directory containing the Common Lisp project files.
   ---
   Please substitute this global variable's content by the actual
   directory on your system which contains the SOAP interpreter's source
   files.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
   
     ------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------------
     make-path-name   | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                 \"Users\"
                      |                 \"Kaveh\"
                      |                 \"SOAP\"
                      |                 \"SOAP_001\"))
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/SOAP/SOAP_001/\")
     ------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operations.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-file (source-file)
  "Loads the Common Lisp SOURCE-FILE, its commorancy expected to
   constitute the *PROJECT-DIRECTORY*, and returns no value."
  (declare (type string source-file))
  (load
    (merge-pathnames *project-directory* source-file)
    :external-format :utf-8)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of project files.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-file "types.lisp")
(import-file "token.lisp")
(import-file "lexer.lisp")
(import-file "node.lisp")
(import-file "parser.lisp")
(import-file "soapset.lisp")
(import-file "interpreter.lisp")
(import-file "tests.lisp")
