;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Malfunge", invented by the Esolang user "ArthroStar11" and
;; presented on August 23rd, 2021, its conception, a twifold conflation
;; of Chris Pressey's "Befunge" as one entheus and Ben Olmstead's
;; "Malboge" as the second moeity, that of its programs' designment as
;; two-dimensional grids, or "playfields", always measuring 256 cells
;; in both dimensions, the symbols of which operate on a ring of stacks,
;; quintuple in their account, to whom a maximum of 1000 signed integer
;; elements may be consigned.
;; 
;; 
;; Concept
;; =======
;; The Malfunge programming language's proprium derives from its
;; operation on a two-dimensional Cartesian grid, the "playfield", whose
;; horizontal and vertical spatial dispansions both homologate a size
;; of exactly 256 cells, each such a single character's commorancy, and,
;; if engaged in an affiliation with an epiphenomenal vallidom,
;; nuncupated to the current among five available signed integer stacks'
;; perquisition and modulation.
;; 
;; == [MAL]FUNGE: [MAL]BOLGE + BE[FUNGE] ==
;; Malfunge's agnomination itself entertain a divulgence of its
;; conjoined entheuses, *Mal*bolge and *Be*funge, while the latter's
;; presence conspicuously inspires a paravaunt parcery in the common
;; bairn's status as a congener; this bona materna's compass enlisting,
;; purported as a tmema rather than a patration, the two-dimensional
;; program grid, even appropriated in the terminology of a "playfield",
;; several instruction pointer (IP) redirection mechanisms, a string
;; mode, as well as the --- severely extended --- stack-based memory
;; model.
;; 
;; == A MALFUNGE PROGRAM: A 256 X 256 PLAYFIELD ==
;; A Procrustean tenet's application is administered to any Malfunge
;; program's conformation, thilk imposes the nomothesia of a Cartesian
;; grid in two dimensions, both equinumerant in a tally of 256 partages,
;; with construct's agnomination, desumed from its Befunge entheus,
;; nevened the "playfield".
;; 
;; == IN EACH CELL WONES EITHER AN INSTRUCTION TOKEN OR A NOP ==
;; Inwith this 256 x 256 reticulation, each cell accommodates an aefauld
;; character's commorancy, to whom either as a singular and independent
;; specimen is affiliated a ligation to an operative expression, or, if
;; deprived of this ilk's telos, an ineffectual, or NOP, status.
;; 
;; == THE INSTRUCTION POINTER COMMENCES IN THE TOP-LEFT CORNER ==
;; Vauncing from the top-left corner, and at its inchoation airted in a
;; dextral mode, an instruction pointer's (IP) emprise accompts for the
;; playfield's traversal, executing each command token's associated
;; epiphenomenon. A multitude of warklumes exist for the direction's
;; modulation.
;; 
;; == THE EXECUTION MAY NOT VIOLATE THE PLAYFIELD'S BOURNES ==
;; A program's cessation is subsumed into the governance of a dedicated
;; instruction, represented by the "@" symbol, wisting of no aliter
;; desinence. In particular, the grid bournes' transgression will yield
;; a fatal and abortive error's instigation, its communication realized
;; within the type "InvalidPlayfieldPositionError".
;; 
;; == THE MEMORY: A QUINTUPLE STACK COMPREHENDING 1000 INTEGERS ==
;; Malfunge's memory department succeeds from a diorism whose compass
;; admits five stacks, any such a salvatory to at most 1000 members,
;; the species of whom is specified as 32-bit signed integer numbers.
;; 
;; == THE STACKS FORM A RING, WITH ONE MEMBER ALWAYS THE ACTIVE UNIT ==
;; This quintuple coefficiency's ordonnance complies to an annulate
;; catena, proceeding from one specimen as the incipial member, whence
;; the stack pointer reside as a warklume for the currently active
;; storage unit's designation. The language' avail does not lapse in
;; those behests deliverance whose capacitation includes the
;; homologation of the pointer's stillatim traversal along both axes.
;; 
;; 
;; Instructions
;; ============
;; An accompt's mickleness amplecting 36 members exhausts the Malfunge
;; instruction set, the perimeter of whom wists of an octuple merists'
;; numbers to entrepart the competences' avails; scilicet:
;; 
;;   (1) Redirections
;;   (2) Control flow governance
;;   (3) Arithmetics
;;   (4) Data managment
;;   (5) Stack management
;;   (6) Input and Output
;;   (7) Strings
;;   (8) Logic
;; 
;; Every instruction agnomination's mold encompasses a single character
;; only; with those symbols eloigned from an explicit operative cause
;; constituting tantamounts to ineffectuous no-operations (NOPs).
;; 
;; == OVERVIEW ==
;; The Malfunge instruction set's complete membership, entreparted by
;; the eight modes of subsumption, shall be the following apercu's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   REDIRECTIONS
;;   ------------------------------------------------------------------
;;   <       | Applies a retroflector's principles on the instruction
;;           | pointer (IP) direction depending on its point of
;;           | allision with the retroflector.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | right
;;           |   ......................................................
;;           |   right        | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction down; otherwise, chooses the
;;           |                | upward airt.
;;           |   ......................................................
;;           |   up           | left
;;           |   ......................................................
;;           |   down         | left
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   >       | Applies a retroflector's principles on the instruction
;;           | pointer (IP) direction depending on its point of
;;           | allision with the retroflector.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction down; otherwise, chooses the
;;           |                | upward airt.
;;           |   ......................................................
;;           |   right        | left
;;           |   ......................................................
;;           |   up           | right
;;           |   ......................................................
;;           |   down         | right
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   ^       | Applies a retroflector's principles on the instruction
;;           | pointer (IP) direction depending on its point of
;;           | allision with the retroflector.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | up
;;           |   ......................................................
;;           |   right        | up
;;           |   ......................................................
;;           |   up           | down
;;           |   ......................................................
;;           |   down         | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction right; otherwise, chooses the
;;           |                | leftward airt.
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   v       | Applies a retroflector's principles on the instruction
;;           | pointer (IP) direction depending on its point of
;;           | allision with the retroflector.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | down
;;           |   ......................................................
;;           |   right        | down
;;           |   ......................................................
;;           |   up           | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction right; otherwise, chooses the
;;           |                | leftward airt.
;;           |   ......................................................
;;           |   down         | up
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   |       | If the instruction pointer (IP) travels along a
;;           | horizontal axis, inverts its direction; otherwise
;;           | changes the same to a horizontal motion based on a
;;           | conditional perquisition.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | right
;;           |   ......................................................
;;           |   right        | left
;;           |   ......................................................
;;           |   up           | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction right; otherwise, chooses the
;;           |                | leftward airt.
;;           |   ......................................................
;;           |   down         | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction right; otherwise, chooses the
;;           |                | leftward airt.
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   _       | If the instruction pointer (IP) travels along a vertical
;;           | axis, inverts its direction; otherwise changes the same
;;           | to a vertical motion based on a conditional
;;           | perquisition.
;;           |---------------------------------------------------------
;;           | The concrete nomothesia shall be ostended alow:
;;           |   ------------------------------------------------------
;;           |   IP direction | Effect
;;           |   -------------+----------------------------------------
;;           |   left         | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction down; otherwise, chooses the
;;           |                | upward airt.
;;           |   ......................................................
;;           |   right        | Pops the current stack's top element;
;;           |                | if the same equals zero (0), sets the
;;           |                | direction down; otherwise, chooses the
;;           |                | upward airt.
;;           |   ......................................................
;;           |   up           | down
;;           |   ......................................................
;;           |   down         | up
;;           |   ------------------------------------------------------
;;           |---------------------------------------------------------
;;           | If an element from the current stack is required and
;;           | the respective salvatory is empty during the request, an
;;           | error of the type "EmptyStackError" will be signaled.
;;   ..................................................................
;;   /       | Reflects the instruction pointer (IP) direction on a
;;           | "mirror" slanted towards the dextral laterality,
;;           | producing the following epiphenomenon in the IP's airt:
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   left              | down
;;           |   .................................
;;           |   right             | up
;;           |   .................................
;;           |   up                | right
;;           |   .................................
;;           |   down              | left
;;           |   ---------------------------------
;;   ..................................................................
;;   \       | Reflects the instruction pointer (IP) direction on a
;;           | "mirror" slanted towards the sinistral laterality,
;;           | producing the following epiphenomenon in the IP's airt:
;;           |   ---------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+--------------
;;           |   left              | up
;;           |   .................................
;;           |   right             | down
;;           |   .................................
;;           |   up                | left
;;           |   .................................
;;           |   down              | right
;;           |   ---------------------------------
;;   ..................................................................
;;   ?       | Changes the instruction pointer (IP) direction in an
;;           | aleatory manner.
;;   ==================================================================
;;   CONTROL FLOW GOVERNANCE
;;   ------------------------------------------------------------------
;;   o       | Skips the next command in the instruction pointer (IP)
;;           | direction.
;;   ..................................................................
;;   @       | Immediately terminates the program.
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   0       | Pushes the number zero (0) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   1       | Pushes the number one (1) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   2       | Pushes the number two (2) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   3       | Pushes the number three (3) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   4       | Pushes the number four (4) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   5       | Pushes the number five (5) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   6       | Pushes the number six (6) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   7       | Pushes the number seven (7) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   8       | Pushes the number eight (8) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   9       | Pushes the number nine (9) onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   P       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b", supputates the sum
;;           | "c" as
;;           |   c = b + a
;;           | and pushes the onto stack immediately succeeding the
;;           | current one.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following principle holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   c <- b + a
;;           |   push c onto stacks[(stackPointer + 1) % 5]
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;           |---------------------------------------------------------
;;           | If the next stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   M       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b", supputates the
;;           | difference "c" as
;;           |   c = b - a
;;           | and pushes the onto stack immediately succeeding the
;;           | current one.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following principle holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   c <- b - a
;;           |   push c onto stacks[(stackPointer + 1) % 5]
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;           |---------------------------------------------------------
;;           | If the next stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   T       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b", supputates the
;;           | product "c" as
;;           |   c = b * a
;;           | and pushes the onto stack immediately succeeding the
;;           | current one.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following principle holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   c <- b * a
;;           |   push c onto stacks[(stackPointer + 1) % 5]
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;           |---------------------------------------------------------
;;           | If the next stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   D       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b", supputates the
;;           | integer quotient "c" as
;;           |   c = floor(b / a)
;;           | and pushes the onto stack immediately succeeding the
;;           | current one.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following principle holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   c <- b / a
;;           |   push c onto stacks[(stackPointer + 1) % 5]
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;           |---------------------------------------------------------
;;           | If the next stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   T       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b", supputates the
;;           | remainder "c" as
;;           |   c = b modulo a
;;           | and pushes the onto stack immediately succeeding the
;;           | current one.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following principle holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   c <- b modulo a
;;           |   push c onto stacks[(stackPointer + 1) % 5]
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;           |---------------------------------------------------------
;;           | If the next stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ==================================================================
;;   DATA MANAGEMENT
;;   ------------------------------------------------------------------
;;   d       | Duplicates the top stack element by pushing a copy of
;;           | the same immediately above it onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is empty at the instant of this
;;           | command's invocation, an error of the type
;;           | "EmptyStackError" will be signaled.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ==================================================================
;;   STACK MANAGEMENT
;;   ------------------------------------------------------------------
;;   S       | Advances the current stack pointer to the next stack;
;;           | if already located on the desinent one, wraps around to
;;           | the first member.
;;   ..................................................................
;;   s       | Recedes the current stack pointer to the previous stack;
;;           | if already located on the first one, wraps around to the
;;           | desinent member.
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   ,       | Queries the standard input conduit for a character and
;;           | pushes its ASCII code onto the current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   .       | Queries the standard input conduit for a signed or
;;           | unsigned integer number and pushes the same onto the
;;           | current stack.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited at the instant of this
;;           | command's invocation, an error of the type
;;           | "FullStackError" will be signaled.
;;   ..................................................................
;;   ;       | Pops the current stack's top element and prints the
;;           | character whose ASCII code corresponds to this value to
;;           | the standard output conduit.
;;           |---------------------------------------------------------
;;           | If the current stack is empty at the instant of this
;;           | command's invocation, an error of the type
;;           | "EmptyStackError" will be signaled.
;;   ..................................................................
;;   :       | Pops the current stack's top element and prints the same
;;           | in its verbatim numeric form to the standard output
;;           | conduit.
;;           |---------------------------------------------------------
;;           | If the current stack is empty at the instant of this
;;           | command's invocation, an error of the type
;;           | "EmptyStackError" will be signaled.
;;   ==================================================================
;;   STRINGS
;;   ------------------------------------------------------------------
;;   "       | Instigates the string mode, which, commencing with the
;;           | subsequent symbol, pushes each encountered character's
;;           | ASCII code onto the current stack, until a closing '"'
;;           | token is encountered.
;;           |---------------------------------------------------------
;;           | Neither the instigating nor the concluding '"' symbol
;;           | will be introduced into the collected ASCII codes.
;;           |---------------------------------------------------------
;;           | If the current stack is surfeited during this command's
;;           | operations, an error of the type "FullStackError" will
;;           | be signaled.
;;   ==================================================================
;;   LOGIC
;;   ------------------------------------------------------------------
;;   I       | Pops the top element from the current stack, here norned
;;           | "a", then the new top element, "b". If it holds b > a,
;;           | then the number one (1) is pushed onto the stack,
;;           | otherwise the number zero (0).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   b <- pop from stacks[stackPointer]
;;           |   if b > a then
;;           |     push 1 onto stacks[stackPointer]
;;           |   else
;;           |     push 0 onto stacks[stackPointer]
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the current stack cannot furnish a sufficient
;;           | quantity of elements for this command, an error of the
;;           | type "EmptyStackError" will be signaled during the
;;           | violating request.
;;   ..................................................................
;;   i       | Pops the top element from the current stack. If this
;;           | value equals zero (0), pushes the number one (1) onto
;;           | the stack; otherwise inserts zero (0).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following holds:
;;           |   a <- pop from stacks[stackPointer]
;;           |   if a = 0 then
;;           |     push 1 onto stacks[stackPointer]
;;           |   else
;;           |     push 0 onto stacks[stackPointer]
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the current stack is empty at the instant of this
;;           | command's invocation, an error of the type
;;           | "EmptyStackError" will be signaled.
;;   ------------------------------------------------------------------
;; 
;; == REDIRECTION OPERATIONS AS SUBJECT OF AN EPEXEGESIS ==
;; A point of reference whose summons occurs with crebritude in the
;; Malfunge language's protolog relates to the condition redirection
;; constructs "h-cond" and "v-cond", the former furnishes a tantamount
;; to Befunge's "_" token, the "horizontal IF", the latter a paregal to
;; "|", as a "vertical IF".
;; 
;; The following apercu shall furnishes a tabular species of
;; equiparation:
;; 
;;   ------------------------------------------------------------------
;;   Malfunge | Befunge       | Causatum
;;   name     | name          | 
;;   ---------+---------------+----------------------------------------
;;   h-cond   | horizontal IF | Pops the current stack's top value and
;;            |               | indagates:
;;            |               |   (a) If the popped value equal zero,
;;            |               |       sets the instruction poiner (IP)
;;            |               |       direction to RIGHT.
;;            |               |   (b) If the popped value does not
;;            |               |       equal zero, sets the instruction
;;            |               |       pointer (IP) direction to LEFT.
;;            |               |----------------------------------------
;;            |               | In a pseudocode diction, it holds:
;;            |               |   let a <- pop from current stack
;;            |               |   if a = 0 then
;;            |               |     direction <- right
;;            |               |   else
;;            |               |     direction <- left
;;            |               |   end if
;;   ..................................................................
;;   v-cond   | vertical   IF | Pops the current stack's top value and
;;            |               | indagates:
;;            |               |   (a) If the popped value equal zero,
;;            |               |       sets the instruction poiner (IP)
;;            |               |       direction to DOWN.
;;            |               |   (b) If the popped value does not
;;            |               |       equal zero, sets the instruction
;;            |               |       pointer (IP) direction to UP.
;;            |               |----------------------------------------
;;            |               | In a pseudocode diction, it holds:
;;            |               |   let a <- pop from current stack
;;            |               |   if a = 0 then
;;            |               |     direction <- down
;;            |               |   else
;;            |               |     direction <- up
;;            |               |   end if
;;   ------------------------------------------------------------------
;; 
;; Applying one conspectuity on the feelefold variegation exercising
;; their purview over Malfunge's redirection conductions, an operiferous
;; purlicue's dation may contribute an enhaused mete of nortelry's
;; conduciveness in the entirety's apprehension:
;; 
;;   ==================================================================
;;   Leftward retroflector
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   <    | left      | right
;;        |............................................................
;;        | right     | v-cond (pop "a"; if a = 0: down; else: up)
;;        |............................................................
;;        | up        | left
;;        |............................................................
;;        | down      | left
;;   ==================================================================
;;   
;;   ==================================================================
;;   Rightward retroflector
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   >    | left      | v-cond (pop "a"; if a = 0: down; else: up)
;;        |............................................................
;;        | right     | left
;;        |............................................................
;;        | up        | right
;;        |............................................................
;;        | down      | right
;;   ==================================================================
;;   
;;   ==================================================================
;;   Upward retroflector
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   ^    | left      | up
;;        |............................................................
;;        | right     | up
;;        |............................................................
;;        | up        | down
;;        |............................................................
;;        | down      | h-cond (pop "a"; if a = 0: right; else: left)
;;   ==================================================================
;;   
;;   ==================================================================
;;   Downward retroflector
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   v    | left      | down
;;        |............................................................
;;        | right     | down
;;        |............................................................
;;        | up        | h-cond (pop "a"; if a = 0: right; else: left)
;;        |............................................................
;;        | down      | up
;;   ==================================================================
;;   
;;   ==================================================================
;;   Vertical paddle
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   |    | left      | right
;;        |............................................................
;;        | right     | left
;;        |............................................................
;;        | up        | h-cond (pop "a"; if a = 0: right; else: left)
;;        |............................................................
;;        | down      | h-cond (pop "a"; if a = 0: right; else: left)
;;   ==================================================================
;;   
;;   ==================================================================
;;   Horizontal paddle
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   _    | left      | v-cond (pop "a"; if a = 0: down; else: up)
;;        |............................................................
;;        | right     | v-cond (pop "a"; if a = 0: down; else: up)
;;        |............................................................
;;        | up        | down
;;        |............................................................
;;        | down      | up
;;   ==================================================================
;;   
;;   ==================================================================
;;   Right-slanting mirror
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   /    | left      | down
;;        |............................................................
;;        | right     | up
;;        |............................................................
;;        | up        | right
;;        |............................................................
;;        | down      | left
;;   ==================================================================
;;   
;;   ==================================================================
;;   Left-slanting mirror
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   \    | left      | up
;;        |............................................................
;;        | right     | down
;;        |............................................................
;;        | up        | left
;;        |............................................................
;;        | down      | right
;;   ==================================================================
;;   
;;   ==================================================================
;;   Random redirection
;;   ------------------------------------------------------------------
;;   Sym. | Direction | Causatum
;;   -----+-----------+------------------------------------------------
;;   ?    | left      | random direction
;;        |............................................................
;;        | right     | random direction
;;        |............................................................
;;        | up        | random direction
;;        |............................................................
;;        | down      | random direction
;;   ==================================================================
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort in the
;; programming language Common Lisp, in this wike's fulfilment preceding
;; the program execution stage by a tier whose onus wones in input
;; source code's linear string format into a veridicous two-dimensional
;; "playfield" incarnation, thilk constructs an array of characters
;; along two axes.
;; 
;; The most peisant contribution, however, is betokened in a state
;; machine as a hid component inwith the interpreter, ordained to the
;; responsibility of the several modes' --- standard, string
;; construction, command skipping, and halting --- governail. The
;; consequent superimposition's apologia shall be the protreptic
;; potential accommodated a commorancy in this alternative to the
;; traditional deployment of conditionals and flags.
;; 
;; The treatise at hand's conformation commences with a vindication of
;; the state machine approach founded upon a complexity's imposition
;; upon the Malfunge execution model, serving as a prevenience to the
;; praecognitum's accoutrement that references the "state" design
;; pattern as the concept's reification, ere the actual implementation
;; presents this chapter's coda.
;; 
;; == A MALFUNGE PROGRAM MAY ASSUME DISTINCT MODES ==
;; A certain intricacy potential's woning is accommodated in Malfunge
;; anent the deportments as particular commands' consequences; with a
;; more concrete diction's administration:
;; 
;;   - STRING MODE:
;;     The command '"' introduces a "string mode", whose governance
;;     ostends the kenspeckle construe of all encountered symbols, with
;;     the '"' character as its aefauld exemption, in the agency as
;;     literal content to be pushed, in its corresponding ASCII code
;;     form, to the memory's current stack. The aforementioned
;;     sentinel's mediation eventuates this mode's valediction towards
;;     the usual symbol interpretation.
;;   
;;   - COMMAND SKIPPING:
;;     An unconditionally active single command omission behest,
;;     realized by the "o" symbol's involvement, imposes any operative
;;     causata's abstinence for a single step, ere the default premises
;;     anew apply.
;;   
;;   - PROGRAM TERMINATION:
;;     The "@" command's evaluation immediately terminates the Malfunge
;;     program; as a consectary, any pursuit of further progressions
;;     ought per force thole an interdiction's subjection in this
;;     terminal mode.
;; 
;; == THREE MODES + STANDARD BEHAVIOR + ERROR = FIVE STATES ==
;; The pair royale of modes distilled from the aboon identification, as
;; well as, in this concrete circumstance, the paregon of the initial or
;; standard notion of command executions and the illicit incursions
;; that erroneous conducts on a program may be peccant of, when in a
;; conceptual application of one's conspectuity, experience a
;; conferrumination to produce a quintuple of states.
;; 
;; Its gendrure a purlicue's vouchsafement, the following membership of
;; five exhausts the contingency:
;; 
;;   (1) Standard    state
;;   (2) String mode state
;;   (3) Skipping    state
;;   (4) Halting     state
;;   (5) Error       state
;; 
;; == CHARACTERS INSTIGATE THE TRANSITIONS BETWIXT THESE STATES ==
;; The vincula's governail betwixt the states, as a function of the
;; conceivable input symbols' involvement, shall be assigned as an onus
;; to the following tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Present     | Input                                  | Next state
;;   state       |                                        |
;;   ------------+----------------------------------------+------------
;;   Standard    | "                                      | String mode
;;               |-----------------------------------------------------
;;               | o                                      | Skipping
;;               |-----------------------------------------------------
;;               | @                                      | Halted
;;               |-----------------------------------------------------
;;               | Any character except 'o', '"', and "@" | Standard
;;               |-----------------------------------------------------
;;               | Source exhausted                       | Error
;;   ..................................................................
;;   String mode | "                                      | Standard
;;               |-----------------------------------------------------
;;               | Any character except '"'               | String mode
;;               |-----------------------------------------------------
;;               | Source exhausted                       | Error
;;   ..................................................................
;;   Skipping    | Any character                          | Standard
;;               |-----------------------------------------------------
;;               | Source exhausted                       | Error
;;   ..................................................................
;;   Halted      | Any character                          | Error
;;   ..................................................................
;;   Error       | Any character                          | Error
;;   ------------------------------------------------------------------
;; 
;; == THE STATE DESIGN PATTERN AS AN ALTERNATIVE TO CONDITIONS ==
;; A simplistic approach, endowed with a conception as eath as its
;; sophistication's shallowness, would aiblins attend to the proclivity
;; of conditionals and execution flags in the interpreter's designment.
;; In this project, however, an endeavor has been pursued to entertain
;; the "state" design pattern's usufructure.
;; 
;; == THE STATE DESIGN PATTERN: STATES DEFINE BEHAVIORS ==
;; The "state" design pattern subsumes into the behavioral species,
;; such is assigned the onus of facilitating the interaction betwixt
;; objects in a complex system.
;; 
;; The state species as particular forbisen emerges from the intricacies
;; to whom a system tholes its affliction if the deportment depends on
;; its internal configuration at a certain instant in time. Most
;; frequently, siccan physignomy's ostention registers a multiplicity in
;; internally processed conditional statements, or "if"-constructs.
;; 
;; == AN ANAGOGE: CONDITIONALS CAN BE REPLACED BY SUBCLASSES ==
;; The anagnorisis to an extant aggregate of conditionals' reformulation
;; is instigated by mediation of the polymorphic principle: Subclasses
;; of a common interface or class may react to an eponymous method
;; invocation in distinct manners. This accoutrement with a succedaneum
;; for catenas of "if" statements in dedicated classes identifies the
;; claviger of a system's dynamic behavior's partage into independent
;; units.
;; 
;; == COMPLEX SYSTEM = CONTEXT (DATA) + STATE MACHINE (BEHAVIOR) ==
;; The state pattern's approach to this circumstance's alleviation
;; expresses itself in the high-level segregation of the system into
;; a "context", to whom the commonly utible data castaldy and a
;; reference to the active state object is apportioned, and the "state"
;; moeity, defining a state machine, while constituting the recipient
;; of that parcery responsible for implementing the software's logic,
;; as well as transitioning between the subsequently necessitated
;; states.
;; 
;; Adduced as a purlicue, the context should, in a rather kenspeckle
;; mete, remain devoid of sophistication in operative value, while the
;; several states entrepart the functionality in a manner rendering
;; each entity from this species sufficiently competent and potent for
;; a well-defined subset of devers.
;; 
;; == THE CONTEXT: SERVANT TO THE CLIENT, CLIENT OF THE STATES ==
;; The "Context" defines the service component, thilk's exposure to the
;; client begets the operative expectations.
;; 
;; The kenspeckle attribute of the state design pattern harbors its
;; commorancy in the alienation of the operative logic from the context
;; into a series of linked states, everichon among these wisting in its
;; responsibilities of both the instructions to perform and the next
;; state to assign.
;; 
;; This delegation of responsibilities to an unspecific account of
;; states, each a subset of competences' salvatory, experiences its
;; manifestation in a field maintaining a reference to the currently
;; active state. A subsequent stage to its inchoation with an incipial
;; state, the transitions themselves elude the context's private
;; efforts; in lieu of this castaldy's encumbrance, the concrete states
;; by their own effort, their diorisms please behold alow, commit to the
;; requisite segues, actuated in the current state field's modulation.
;; 
;; == THE STATE: AN INTERFACE FOR THE ACTUAL OPERATIVE UNITS ==
;; In the "State" interface is realized the firmament of the implicitly
;; defined state machine. In the most common manifestation, an aefauld
;; operation's imposition delineates the slim covenant to whom the
;; context's trust is arraigned, and inwith whose circumference the
;; conditional case represented by the state subclass advances its
;; entelechia, producing some causatum, ere progressing into the
;; consectaneous next state.
;; 
;; == THE CONCRETE STATES: IMPLEMENTATIONS OF PARCELS OF LOGIC ==
;; Every concrete state implements the "State" interface in its pursuit
;; to produce a specific parcel of functionality from the "Context"
;; object's offered services.
;; 
;; In an actual apprehension, the concrete state's competences
;; bifurcate into a twissel of devers:
;; 
;;   (1) The expected causata's actuation; that is, the realization of
;;       the logic.
;;   
;;   (2) A transition to the next state in this implicitly defined
;;       state machine; its reification ensuing from the modification of
;;       the "Context" object's managed current state reference.
;; 
;; The following approximated simulacrum of a UML class diagram's dation
;; shall contribute a visual apercu concerning the participating
;; entities in such a state-centric solution.
;; 
;; Please heed that the public method "request()" of the "Context" class
;; simply invokes its currently active state's "handle()" operation,
;; effectively delegating the main onus to the concrete "State" subclass
;; implementation.
;; 
;;      +-------------------------------------+
;;      |               Context               |
;;      |-------------------------------------|
;;      | - currentState : State              |
;;      |-------------------------------------|
;;      | + setCurrentState (newState) : void |
;;      | + request ()                 : any  |
;;      +-------------------------------------+
;;                         |
;;                         |
;;                         |
;;                         V
;;               +-------------------+
;;               |   <<interface>>   |
;;               |       State       |
;;               |-------------------|
;;               |-------------------|
;;               | + handle () : any |
;;               +-------------------+
;;                         ^
;;                         |
;;                         | implements
;;                         |
;;             +-----------+----------+
;;             |                      |
;;             |                      |
;;             |                      |
;;   +-------------------+   +-------------------+
;;   |  ConcreteState1   |   |  ConcreteState2   |
;;   |-------------------|   |-------------------|
;;   |-------------------|   |-------------------|
;;   | + handle () : any |   | + handle () : any |
;;   +-------------------+   +-------------------+
;; 
;; == THE MALFUNGE INTERPRETER MAY ASSUME A QUADRUPLE OF STATES ==
;; The context entity's identification eludes an intricacy's tholance,
;; assigning this resolution to the ``Interpreter'' class itself, whose
;; slots (fields) and operations engender those avails whose existency
;; register requisitums for the states' success.
;; 
;; The singleton specification appertaining context exhibits an
;; asymmetry with respect to the rather feelefold enumeration by thilk
;; the states, quadruple in this tally, emerge. In the following
;; paragraphs, the thus delineated membership shall be limned with more
;; details.
;; 
;; As a verb meted with peisant puissance for its further consequences:
;; A mode adscititious in its caract, and as such inflicted with
;; eloignment from a materialization as a veridicous state, shall be
;; nevened by an "Error"'s inflictation, thilk as a concomitant in
;; consectary and ultimity aborts the program in its entire process.
;; 
;; Standard-State
;;   The incipial and paravaunt mode of a Malfunge operations'
;;   entelechy, whose sole parcels of eloignment are accompted in the
;;   string collation (please consult the -> STRING-MODE-STATE), symbol
;;   skipping (see -> SKIP-STATE), and the terminal halting state
;;   (subject of a discussion as -> HALT-STATE).
;; 
;; String-Mode-State
;;   Instigated by the double quotation mark, '"', and moribund in the
;;   paregal jumelle, this mode serves in the collation of symbols in
;;   their literal form, everichon among these encounters' replication
;;   reified as the corresponding ASCII code inserting at the current
;;   memory stack's top position. A corollary of its conclusion, the
;;   -> STANDARD-STATE, responsible for this mode's encheson, is iterum
;;   occupied.
;; 
;; Skip-State
;;   Enjoying its gendrure by the -> STANDARD-STATE's consultation with
;;   the "o" token, this state merely neglects its inducted symbol, ere
;;   its control flow's return to the aforementioned compernage.
;; 
;; Halt-State
;;   The terminal state, arrived into by the "@" symbol's adminiculum
;;   during the -> STANDARD-STATE's moils, and whose assumption
;;   conditions the program's formal cessation. In this vale of
;;   desinence, no other command's procession is adhibited any
;;   tolerance.
;; 
;; == STATE TRANSITIONS OCCUR BY READING THE PLAYFIELD SYMBOLS ==
;; The proposed model of a state machine in its transitions' ambit
;; relies on the instigating symbols' viand on the interpreter's
;; traversal of the underlying code, the playfield, whose currently
;; sojourned cell's character establishes the sustenance to the active
;; program state, whence the expected epiphenomenal reactions ensue.
;; 
;; The following approximation of a UML state diagram shall be the
;; cynosure's attendant thilk ostens the admissible program states.
;; 
;; Please heed the diction's adjustment from Common Lisp's consuetude
;; involving the single word's vinculum to assume hyphens ("-") towards
;; one entalented with superior concinnity with human parlance by its
;; tokens' experience of spaces as merists.
;; 
;;   (O)
;;    |   character
;;    |   except 'o' and '"'                      +--------+
;;    |   +---+                         @         |        |
;;    |   |   |   +------------------------------>| Halted |----->(X)
;;    V   |   v   |                               |        |
;; +----------------+     o       +----------+    +--------+
;; |                |------------>|          |         |
;; | Standard state |             | Skipping |         | character
;; |                |<------------|          |--+      |
;; +----------------+  character  +----------+  |      +-----------+
;;    |      ^    |                             |                  |
;;    | "    |    +------------+                | source           |
;;    |      +-----------+     | source         | exhausted        |
;;    V                  |     | exhausted      |                  |
;; +----------------+    | "   |                V                  |
;; |                |----+     |            +-------+              |
;; |  String mode   |          +----------->|       |<-------------+
;; |                |---------------------->| error |
;; +----------------+   source exhausted    |       |--------------+
;;    |          ^                          +-------+              |
;;    |          |                            |   ^                |
;;    |          |                            |   |                |
;;    +----------+                            |   +----------------+
;;    character except '"'                    |       character
;;                                            V
;;                                           (X)
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-09-23
;; 
;; Sources:
;;   [esolang2021Malfunge]
;;   The Esolang contributors, "Malfunge", August 26th, 2021
;;   URL: "https://esolangs.org/wiki/Malfunge"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype character-grid ()
  "The ``character-grid'' type defines a two-dimensional grid tallying
   256 characters along both of its axes, molded into a simple array."
  '(simple-array character (256 256)))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts along which
   the control flow in a Malfunge program may cair."
  '(member :up :right :down :left))

;;; -------------------------------------------------------

(deftype 32-bit-integer ()
  "The ``32-bit-integer'' type defines a signed integral number whose
   conformation is edified upon 32 accolent bits, thus proffering to its
   membership a gamut occupying the closed interval
   [-2,147,483,648, +2,147,483,647]."
  '(signed-byte 32))

;;; -------------------------------------------------------

(deftype integer-stack ()
  "The ``integer-stack'' type defines a stack utile to be naited in a
   Malfunge program, enumerating a certain tally of 32-bit signed
   integer elements as its members."
  '(vector 32-bit-integer *))

;;; -------------------------------------------------------

(deftype integer-stack-vector ()
  "The ``integer-stack-vector'' type defines a vector compact of zero or
   more ``integer-stack'' objects."
  '(simple-array integer-stack (*)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member complying to the ELEMENT-TYPE, for which is
   imposed the generic sentinel ``*'' as the default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type defines a dyadic function which accepts
   two 32-bit integer numbers and produces an object desumed from the
   same realm."
  '(function (32-bit-integer 32-bit-integer) 32-bit-integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, the
   compass of which admits, among others, the functions ``format'',
   ``print'', and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, delivers the selfsame ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-boolean-value
      (member (char-code candidate) '(10 11 12 13) :test #'=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Concatenates the LINES by splicing each accolent twissel via a single
   newline character and returns the result as a fresh simple string."
  (declare (type (list-of string) lines))
  (the simple-string
    (coerce
      (format NIL "~{~&~a~}" lines)
      'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of playfield.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 256 256) +PLAYFIELD-WIDTH+))
(declaim (type (integer 256 256) +PLAYFIELD-HEIGHT+))

;;; -------------------------------------------------------

(defparameter +PLAYFIELD-WIDTH+ 256
  "The tally of a playfield's cells bedight along the horizontal axis.")

(defparameter +PLAYFIELD-HEIGHT+ 256
  "The tally of a playfield's cells bedight along the vertical axis.")

;;; -------------------------------------------------------

(defclass Playfield ()
  ((cells
    :initform      (make-array
                     (list +PLAYFIELD-WIDTH+ +PLAYFIELD-HEIGHT+)
                     :element-type    'character
                     :initial-element #\Space
                     :adjustable      NIL
                     :fill-pointer    NIL)
    :accessor      playfield-cells
    :type          character-matrix
    :documentation "A two-dimensional array of characters representing
                    the playfield's cells.")
   (printable-width
    :initform      0
    :type          (integer 0 256)
    :documentation "The width of the playfield actually utilized by its
                    symbols, as counterdistinguished from the entire
                    available horizontal dispansion.
                    ---
                    This slot's purpose solely appertains to the
                    facilitation of the playfield's printing in an
                    aesthetical manner, restricting the output to the
                    tmema actually necessary, while omitting purely
                    vacant margins on the dextral and lower parcels.")
   (printable-height
    :initform      0
    :type          (integer 0 256)
    :documentation "The height of the playfield actually utilized by its
                    symbols, as counterdistinguished from the entire
                    available vertical dispansion.
                    ---
                    This slot's purpose solely appertains to the
                    facilitation of the playfield's printing in an
                    aesthetical manner, restricting the output to the
                    tmema actually necessary, while omitting purely
                    vacant margins on the dextral and lower parcels."))
  (:documentation
    "The ``Playfield'' class applies itself to the modeling of a
     Malfunge program's two-dimensional Cartesian ordonnace of
     characters, the so called \"playfield\"."))

;;; -------------------------------------------------------

(defun valid-playfield-position-p (playfield x y)
  "Determines whether the two-dimensional position specifier, compact
   of the zero-based column-index X and the zero-based row index Y,
   designates a valid position into the PLAYFIELD, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Playfield playfield))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (the boolean
    (resolve-to-boolean-value
      (array-in-bounds-p
        (playfield-cells playfield)
        y x))))

;;; -------------------------------------------------------

(defun check-whether-position-is-valid
    (playfield x y
     &optional (intended-action "" intended-action-supplied-p))
  "Determines whether the two-dimensional position specifier, compact
   of the zero-based column-index X and the zero-based row index Y,
   designates a valid position into the PLAYFIELD, returning on
   confirmation no value; otherwise signals an error of the type
   ``Invalid-Playfield-Position-Error''."
  (declare (type Playfield playfield))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type string    intended-action))
  (declare (type T         intended-action-supplied-p))
  (unless (valid-playfield-position-p playfield x y)
    (apply #'error 'Invalid-Playfield-Position-Error
      :playfield playfield
      :probed-x  x
      :probed-y  y
      (when intended-action-supplied-p
        (list :intended-action intended-action))))
  (values))

;;; -------------------------------------------------------

(defun get-playfield-symbol (playfield x y)
  "Returns the symbol stored in the PLAYFIELD cell amenable to the
   position compact of the zero-based column index X and the zero-based
   row index Y."
  (declare (type Playfield playfield))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (check-whether-position-is-valid playfield x y "query")
  (the character
    (aref (playfield-cells playfield) y x)))

;;; -------------------------------------------------------

(defun set-playfield-symbol (playfield x y new-symbol)
  "Stores the NEW-SYMBOL in the PLAYFIELD cell amenable to the position
   compact of the zero-based column index X and the zero-based row index
   Y and returns no value."
  (declare (type Playfield playfield))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type character new-symbol))
  (check-whether-position-is-valid playfield x y "change")
  (with-slots (cells printable-width printable-height) playfield
    (declare (type character-grid  cells))
    (declare (type (integer 0 256) printable-width))
    (declare (type (integer 0 256) printable-height))
    (psetf
      (aref cells y x) new-symbol
      printable-width  (max printable-width  (1+ x))
      printable-height (max printable-height (1+ y))))
  (values))

;;; -------------------------------------------------------

(defun parse-playfield (code)
  "Parses the piece of Malfunge source CODE and returns a fresh
   ``Playfield'' representation thereof."
  (declare (type string code))
  (let ((playfield (make-instance 'Playfield))
        (x         0)
        (y         0))
    (declare (type Playfield playfield))
    (declare (type fixnum    x))
    (declare (type fixnum    y))
    (loop for current-symbol of-type character across code do
      (cond
        ((newline-character-p current-symbol)
          (setf x 0)
          (incf y 1))
        (T
          (set-playfield-symbol playfield x y current-symbol)
          (incf x 1))))
    (the Playfield playfield)))

;;; -------------------------------------------------------

(defmethod print-object ((playfield Playfield) (stream T))
  (declare (type Playfield   playfield))
  (declare (type destination stream))
  (format stream "~&Playfield (excerpt: ~d x ~d)"
    (slot-value playfield 'printable-width)
    (slot-value playfield 'printable-height))
  (dotimes (y (slot-value playfield 'printable-height))
    (declare (type fixnum y))
    (format stream "~&")
    (dotimes (x (slot-value playfield 'printable-width))
      (declare (type fixnum x))
      (format stream "~c"
        (get-playfield-symbol playfield x y)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun invert-direction (input-direction)
  "Returns the direction yielded by the INPUT-DIRECTION's inversion."
  (declare (type direction input-direction))
  (the direction
    (case input-direction
      (:left     :right)
      (:right    :left)
      (:up       :down)
      (:down     :up)
      (otherwise
        (error "Cannot invert the direction ~s." input-direction)))))

;;; -------------------------------------------------------

(defun reflect-direction-on-right-slanting-mirror (input-direction)
  "Returns the direction yielded by the INPUT-DIRECTION's reflection on
   a conceived mirror slanted towards the dextral laterality, as being
   represented in a Malfunge program by the symbol \"/\"."
  (declare (type direction input-direction))
  (the direction
    (case input-direction
      (:left     :down)
      (:right    :up)
      (:up       :right)
      (:down     :left)
      (otherwise
        (error "Cannot reflect the direction ~s." input-direction)))))

;;; -------------------------------------------------------

(defun reflect-direction-on-left-slanting-mirror (input-direction)
  "Returns the direction yielded by the INPUT-DIRECTION's reflection on
   a conceived mirror slanted towards the sinistral laterality, as being
   represented in a Malfunge program by the symbol \"\\\"."
  (declare (type direction input-direction))
  (the direction
    (case input-direction
      (:left     :up)
      (:right    :down)
      (:up       :left)
      (:down     :right)
      (otherwise
        (error "Cannot reflect the direction ~s." input-direction)))))

;;; -------------------------------------------------------

;; h-cond (horizontal IF).
(defun conditionally-choose-horizontal-direction (discriminator)
  "If the DISCRIMINATOR equals zero (0), returns the ``:right''
   direction, otherwise the ``:left'' one.
   ---
   This operation serves in a tantamount's furnishment to the Malfunge
   \"h-cond\" predicate, which concurs with Befunge's \"horizontal IF\"
   instruction (\"_\")."
  (declare (type 32-bit-integer discriminator))
  (the direction
    (if (zerop discriminator)
      :right
      :left)))

;;; -------------------------------------------------------

;; v-cond (vertical IF).
(defun conditionally-choose-vertical-direction (discriminator)
  "If the DISCRIMINATOR equals zero (0), returns the ``:down''
   direction, otherwise the ``:up'' one.
   ---
   This operation serves in a tantamount's furnishment to the Malfunge
   \"v-cond\" predicate, which concurs with Befunge's \"vertical IF\"
   instruction (\"|\")."
  (declare (type 32-bit-integer discriminator))
  (the direction
    (if (zerop discriminator)
      :down
      :up)))

;;; -------------------------------------------------------

(defun select-random-direction ()
  "Randomly selects and return a direction."
  (the direction
    (case (random 3)
      (0         :left)
      (1         :right)
      (2         :up)
      (3         :down)
      (otherwise
        (error "Unexpected random number generator state.")))))

;;; -------------------------------------------------------

(defun move-into-direction (input-x input-y input-direction)
  "Supputates the new position obtained by moving one step from the
   current location (INPUT-X, INPUT-Y) into the INPUT-DIRECTION, and
   returns two values:
     (1) The new x-coordinate.
     (2) The new y-coordinate."
  (declare (type fixnum    input-x))
  (declare (type fixnum    input-y))
  (declare (type direction input-direction))
  (the (values fixnum fixnum)
    (case input-direction
      (:left     (values (1- input-x)     input-y))
      (:right    (values (1+ input-x)     input-y))
      (:up       (values     input-x  (1- input-y)))
      (:down     (values     input-x  (1+ input-y)))
      (otherwise
        (error "Cannot move into the direction ~s." input-direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer stack operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0 1000) +MAXIMUM-INTEGER-STACK-SIZE+))

;;; -------------------------------------------------------

(defparameter +MAXIMUM-INTEGER-STACK-SIZE+ 1000
  "The inclusive maximum accompt of elements for whom an integer stack
   may accommodate.")

;;; -------------------------------------------------------

(defun make-empty-integer-stack ()
  "Creates and returns an initially vacant integer stack."
  (the integer-stack
    (make-array 0
      :element-type    '32-bit-integer
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))

;;; -------------------------------------------------------

(defun get-integer-stack-size (stack)
  "Returns the tally of elements partaking of the integer STACK."
  (declare (type integer-stack stack))
  (the (integer 0 1000)
    (fill-pointer stack)))

;;; -------------------------------------------------------

(defun integer-stack-is-full-p (stack)
  "Determines whether the integer STACK tholes a state of plenitude,
   thilk renders its admissibility to further elements inconceivable,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type integer-stack stack))
  (the boolean
    (resolve-to-boolean-value
      (>= (get-integer-stack-size stack)
          +MAXIMUM-INTEGER-STACK-SIZE+))))

;;; -------------------------------------------------------

(defun check-whether-integer-stack-is-full (stack)
  "Determines whether the integer STACK tholes a state of plenitude,
   thilk renders its admissibility to further elements inconceivable,
   on confirmation signaling an error of the type ``Full-Stack-Error'',
   otherwise accompasses no causatum, while returning no value."
  (declare (type integer-stack stack))
  (when (integer-stack-is-full-p stack)
    (error 'Full-Stack-Error
      :offended-stack stack
      :maximum-size   +MAXIMUM-INTEGER-STACK-SIZE+))
  (values))

;;; -------------------------------------------------------

(defun push-onto-integer-stack (recipient new-element)
  "Inserts the NEW-ELEMENT at the RECIPIENT stack's top position and
   returns no value; or, upon its surfeiture, signals an error of the
   type ``Full-Stack-Error''."
  (declare (type integer-stack  recipient))
  (declare (type 32-bit-integer new-element))
  (check-whether-integer-stack-is-full recipient)
  (vector-push-extend new-element recipient)
  (values))

;;; -------------------------------------------------------

(defun integer-stack-is-empty-p (stack)
  "Determines whether the integer STACK is destitute of any elements,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type integer-stack stack))
  (the boolean
    (resolve-to-boolean-value
      (zerop
        (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun check-if-integer-stack-is-empty
    (stack
     &optional (intended-action "" intended-action-supplied-p))
  "Determines whether the integer STACK is destitute of any elements, on
   confirmation signaling an error of the type ``Empty-Stack-Error'',
   contingently accompanied by the INTENDED-ACTION as part of its
   ensconced message; otherwise returns no value without any further
   causata."
  (declare (type integer-stack stack))
  (declare (type string        intended-action))
  (declare (type T             intended-action-supplied-p))
  (when (integer-stack-is-empty-p stack)
    (apply #'error 'Empty-Stack-Error :offended-stack stack
      (when intended-action-supplied-p
        (list :offending-action intended-action))))
  (values))

;;; -------------------------------------------------------

(defun pop-from-integer-stack (stack)
  "Removes and returns the top element from the integer STACK; or, upon
   its vacancy at the instant of this operation's invocation, signals an
   error of the type ``Empty-Stack-Error''."
  (declare (type integer-stack stack))
  (check-if-integer-stack-is-empty stack "pop from")
  (the 32-bit-integer
    (prog1
      (aref stack
        (1- (fill-pointer stack)))
      (decf (fill-pointer stack)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of stack manager.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack-Manager ()
  ((number-of-stacks
    :initarg       :number-of-stacks
    :initform      (error "Missing number of stacks.")
    :type          fixnum
    :documentation "The tally of STACKS partaking in this collection.")
   (current-stack-index
    :initform      0
    :type          fixnum
    :documentation "The zero-based index of the currently member among
                    the STACKs.")
   (stacks
    :type          integer-stack-vector
    :documentation "A vector of integer stacks, enumerating the
                    NUMBER-OF-STACKS cardinality, the currently active
                    member referenced by the zero-based
                    CURRENT-STACK-INDEX."))
  (:documentation
    "The ``Stack-Manager'' class applies itself to the castaldy of an
     annulate catena of integer-valued stacks, bedight in a circinate
     fashion, designating at any instant one member as the active one,
     the navigation among which may proceed in a wrapping mode along
     both axes."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((manager Stack-Manager) &key)
  "Compiles the requisite tally of stacks specified by the stack
   MANAGER, stores the same in the MANAGER, and returns no value."
  (declare (type Stack-Manager manager))
  (with-slots (number-of-stacks stacks) manager
    (declare (type fixnum               number-of-stacks))
    (declare (type integer-stack-vector stacks))
    (setf stacks
      (make-array number-of-stacks
        :element-type 'integer-stack
        :initial-contents
          (loop repeat number-of-stacks collect
            (make-empty-integer-stack))
        :adjustable   NIL
        :fill-pointer NIL)))
  (values))

;;; -------------------------------------------------------

(defun make-stack-manager (number-of-stacks)
  "Creates and returns a fresh ``Stack-Manager'' whose membership
   tallies the NUMBER-OF-STACKS."
  (declare (type fixnum number-of-stacks))
  (the Stack-Manager
    (make-instance 'Stack-Manager :number-of-stacks number-of-stacks)))

;;; -------------------------------------------------------

(defun get-current-stack (manager)
  "Returns the stack MANAGER's currently selected cell."
  (declare (type Stack-Manager manager))
  (the integer-stack
    (aref
      (slot-value manager 'stacks)
      (slot-value manager 'current-stack-index))))

;;; -------------------------------------------------------

(defun get-next-stack (manager)
  "Returns the stack immediately succeeding the stack MANAGER's
   currently selected one, contingently wrapping around at the desinent
   position."
  (declare (type Stack-Manager manager))
  (with-slots (stacks current-stack-index number-of-stacks) manager
    (declare (type integer-stack-vector stacks))
    (declare (type fixnum               current-stack-index))
    (declare (type fixnum               number-of-stacks))
    (declare (type fixnum               current-stack-index))
    (the integer-stack
      (aref stacks
        (mod (1+ current-stack-index) number-of-stacks)))))

;;; -------------------------------------------------------

(defun select-next-stack (manager)
  "Moves the stack MANAGER's selected stack to the next member,
   contingently wrapping around at the desinent position, and returns no
   value."
  (declare (type Stack-Manager manager))
  (with-slots (current-stack-index number-of-stacks) manager
    (declare (type fixnum current-stack-index))
    (declare (type fixnum number-of-stacks))
    (setf current-stack-index
      (mod (1+ current-stack-index) number-of-stacks)))
  (values))

;;; -------------------------------------------------------

(defun select-previous-stack (manager)
  "Moves the stack MANAGER's selected stack to the prevenient member,
   contingently wrapping around at the first position, and returns no
   value."
  (declare (type Stack-Manager manager))
  (with-slots (current-stack-index number-of-stacks) manager
    (declare (type fixnum current-stack-index))
    (declare (type fixnum number-of-stacks))
    (setf current-stack-index
      (mod (1- current-stack-index) number-of-stacks)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Context".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Context ()
  ()
  (:documentation
    "The ``Context'' interface establishes the foundry entreparted by
     all classes serving the purpose of an environment for the
     ``Program-State'' implementations data access."))

;;; -------------------------------------------------------

(defgeneric get-context-state (context)
  (:documentation
    "Returns the CONTEXT's currently active state."))

;;; -------------------------------------------------------

(defgeneric set-context-state (context new-state)
  (:documentation
    "Sets the CONTEXT's state to the NEW-STATE and returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Program-State".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-State ()
  ()
  (:documentation
    "The ``Program-State'' interface serves in a common firmament's
     edification whence ensue all contingencies for a state homologated
     to be occupied during a Malfunge program's execution."))

;;; -------------------------------------------------------

(defgeneric get-state-context (state)
  (:documentation
    "Returns the context affiliated with the program STATE."))

;;; -------------------------------------------------------

(defgeneric process-symbol (state symbol)
  (:documentation
    "Processes the symbol in the program STATE's environment and returns
     no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Abstract-State".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Abstract-State (Program-State)
  ((context
    :initarg       :context
    :initform      (error "Missing state context.")
    :type          Context
    :documentation "The state's context, furnishing an environment for
                    its operative entelechy."))
  (:documentation
    "The ``Abstract-State'' abstract class applies itself to the wike of
     the program state classes' common data ensconcement, which in
     particular appertains to the interpreter context's castaldy."))

;;; -------------------------------------------------------

(defmethod get-state-context ((state Abstract-State))
  (declare (type Abstract-State state))
  (the Context
    (slot-value state 'context)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program state classes.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Standard-State (Abstract-State)
  ()
  (:documentation
    "The ``Standard-State'' class implements the state ordained to the
     preponderance among a Malfunge program operation's infusion with
     efficacy."))

;;; -------------------------------------------------------

(defclass Skip-State (Abstract-State)
  ()
  (:documentation
    "The ``Skip-State'' class serves in the modeling of a program state
     which simply ignores the Malfunge program's currently selected
     character, ere reverting to a ``Standard-State''."))

;;; -------------------------------------------------------

(defclass String-Mode-State (Abstract-State)
  ()
  (:documentation
    "The ``String-Mode-State'' class serves in the modeling of a program
     state which, vauncing from the symbol immediately succeeding the
     instigating quotation mark ('\"'), gathers all character prevenient
     to the concluding '\"' jumelle in the currently active program
     stack, ere reverting to a ``Standard-State''."))

;;; -------------------------------------------------------

(defclass Halt-State (Abstract-State)
  ()
  (:documentation
    "The ``Halt-State'' class serves in the modeling of a program state
     which serves to designate a Malfunge program execution's cessation,
     homologating no symbols for further procession."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum +NUMBER-OF-STACKS+))

;;; -------------------------------------------------------

(defparameter +NUMBER-OF-STACKS+ 5
  "The tally of stacks constituting the Malfunge memory.")

;;; -------------------------------------------------------

(defclass Interpreter (Context)
  ((playfield
    :initarg       :playfield
    :initform      (error "Missing playfield.")
    :reader        interpreter-playfield
    :type          Playfield
    :documentation "The Malfunge program as a two-dimensional grid of
                    characters.")
   (ip-x
    :initform      0
    :accessor      interpreter-ip-x
    :type          fixnum
    :documentation "The zero-based column index into the PLAYFIELD at
                    which the instruction pointer (IP) contemporaneously
                    resides.")
   (ip-y
    :initform      0
    :accessor      interpreter-ip-y
    :type          fixnum
    :documentation "The zero-based row index into the PLAYFIELD at
                    which the instruction pointer (IP) contemporaneously
                    resides.")
   (ip-direction
    :initform      :right
    :accessor      interpreter-ip-direction
    :type          direction
    :documentation "The instruction pointer's (IP) contemporaneous
                    traveling direction.")
   (has-halted-p
    :initform      NIL
    :accessor      interpreter-has-halted-p
    :type          boolean
    :documentation "A Boolean flag which determines whether the program
                    has been terminated via an \"@\" command
                    invocation.")
   (stacks
    :initform      (make-stack-manager +NUMBER-OF-STACKS+)
    :accessor      interpreter-stacks
    :documentation "The program memory as a quintuple ring of
                    integer-valued stacks.")
   (state
    :type          Program-State
    :documentation "The interpreter's currently active state."))
  (:documentation
    "The ``Interpreter'' class constitutes the parcery's recipient which
     appertains to the accompassing of actual efficacy to a Malfunge
     program communicated in a two-dimensional playfield's guise."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Configures the INTERPRETER's incipial state to a fresh
   ``Standard-State'', initializes the random number generator, and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (state) interpreter
    (declare (type Program-State state))
    (psetf
      state
        (make-instance 'Standard-State :context interpreter)
      *random-state*
        (make-random-state T)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (playfield)
  "Creates and returns a fresh ``Interpreter'' whose efforts are
   invested into the Malfunge PLAYFIELD's evalution."
  (declare (type Playfield playfield))
  (the Interpreter
    (make-instance 'Interpreter :playfield playfield)))

;;; -------------------------------------------------------

(defmethod get-context-state ((interpreter Interpreter))
  (declare (type Interpreter interpreter))
  (the Program-State
    (slot-value interpreter 'state)))

;;; -------------------------------------------------------

(defmethod set-context-state ((interpreter Interpreter)
                              (new-state   Program-State))
  (declare (type Interpreter   interpreter))
  (declare (type Program-State new-state))
  (setf (slot-value interpreter 'state) new-state)
  (values))

;;; -------------------------------------------------------

(defun advance-to-next-symbol (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) one step into
   the currently assumed direction and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip-x ip-y ip-direction) interpreter
    (declare (type fixnum    ip-x))
    (declare (type fixnum    ip-y))
    (declare (type direction ip-direction))
    (setf (values ip-x ip-y)
      (move-into-direction ip-x ip-y ip-direction)))
  (values))

;;; -------------------------------------------------------

(defun get-current-symbol (interpreter)
  "Returns the symbol commorant in the INTERPRETER's playfield at the
   position designated by the INTERPRETER's contemporaneous instruction
   pointer (IP) location."
  (declare (type Interpreter interpreter))
  (the character
    (get-playfield-symbol
      (interpreter-playfield interpreter)
      (interpreter-ip-x      interpreter)
      (interpreter-ip-y      interpreter))))

;;; -------------------------------------------------------

(defun process-current-symbol (interpreter)
  "Evaluates the INTERPRETER's currently selected symbol and returns no
   value."
  (declare (type Interpreter interpreter))
  (process-symbol
    (get-context-state  interpreter)
    (get-current-symbol interpreter))
  (values))

;;; -------------------------------------------------------

(defun apply-binary-operator (interpreter operator)
  "Removes the top element, \"a\", from the INTERPRETER's current stack,
   succeeded by a removal of the new top element, \"b\", applies the
   OPERATOR to b and a in this exact order, pushes the result onto the
   next stack's top, and returns no value."
  (declare (type Interpreter     interpreter))
  (declare (type binary-operator operator))
  (let ((right-operand
          (pop-from-integer-stack
            (get-current-stack
              (interpreter-stacks interpreter))))
        (left-operand
          (pop-from-integer-stack
            (get-current-stack
              (interpreter-stacks interpreter)))))
    (declare (type 32-bit-integer right-operand))
    (declare (type 32-bit-integer left-operand))
    (push-onto-integer-stack
      (get-next-stack
        (interpreter-stacks interpreter))
      (funcall operator left-operand right-operand)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Malfunge program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-has-halted-p interpreter) do
    (process-current-symbol interpreter)
    (advance-to-next-symbol interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Standard-State" operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\>)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        (:left
          (conditionally-choose-vertical-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        (:right :left)
        (:up    :right)
        (:down  :right)
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\<)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        (:left :right)
        (:right
          (conditionally-choose-vertical-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        (:up    :left)
        (:down  :left)
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\^)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        (:left  :up)
        (:right :up)
        (:up    :down)
        (:down
          (conditionally-choose-horizontal-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\v)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        (:left  :down)
        (:right :down)
        (:up
          (conditionally-choose-horizontal-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        (:down  :up)
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\|)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        ((:left :right)
          (invert-direction
            (interpreter-ip-direction context)))
        ((:up :down)
          (conditionally-choose-horizontal-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\_)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (case (interpreter-ip-direction context)
        ((:left :right)
          (conditionally-choose-vertical-direction
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
        ((:up :down)
          (invert-direction
            (interpreter-ip-direction context)))
        (otherwise
          (error "Invalid direction: ~s."
            (interpreter-ip-direction context))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\/)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (reflect-direction-on-right-slanting-mirror
        (interpreter-ip-direction context))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\\)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (reflect-direction-on-left-slanting-mirror
        (interpreter-ip-direction context))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\?)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-ip-direction context)
      (select-random-direction)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\o)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (set-context-state context
      (make-instance 'Skip-State :context context)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\P)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (apply-binary-operator
    (get-state-context state)
    #'+)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\M)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (apply-binary-operator
    (get-state-context state)
    #'-)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\T)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (apply-binary-operator
    (get-state-context state)
    #'*)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\D)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (apply-binary-operator
    (get-state-context state)
    #'round)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\m)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (apply-binary-operator
    (get-state-context state)
    #'mod)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\S)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (select-next-stack
    (interpreter-stacks
      (get-state-context state)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\s)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (select-previous-stack
    (interpreter-stacks
      (get-state-context state)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\,)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (format        *query-io* "~&Please enter a character: ")
  (finish-output *query-io*)
  (push-onto-integer-stack
    (get-current-stack
      (interpreter-stacks
        (get-state-context state)))
    (char-code
      (read-char *query-io* NIL #\Null)))
  (clear-input *query-io*)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\.)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (format        *query-io* "~&Please enter an integer number: ")
  (finish-output *query-io*)
  (push-onto-integer-stack
    (get-current-stack
      (interpreter-stacks
        (get-state-context state)))
    (parse-integer
      (read-line *query-io* NIL "0")))
  (clear-input *query-io*)
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\:)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (format *query-io* "~d"
    (pop-from-integer-stack
      (get-current-stack
        (interpreter-stacks
          (get-state-context state)))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\;)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (format *query-io* "~c"
    (code-char
      (pop-from-integer-stack
        (get-current-stack
          (interpreter-stacks
            (get-state-context state))))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\")))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (set-context-state context
      (make-instance 'String-Mode-State :context context)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\I)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (let ((a (pop-from-integer-stack
               (get-current-stack
                 (interpreter-stacks context))))
          (b (pop-from-integer-stack
               (get-current-stack
                 (interpreter-stacks context)))))
      (declare (type 32-bit-integer a))
      (declare (type 32-bit-integer b))
      (push-onto-integer-stack
        (get-current-stack
          (interpreter-stacks context))
        (if (> b a)
          1
          0))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\i)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (let ((top-element
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
      (declare (type 32-bit-integer top-element))
      (push-onto-integer-stack
        (get-current-stack
          (interpreter-stacks context))
        (if (zerop top-element)
          1
          0))))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\d)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (let ((top-element
            (pop-from-integer-stack
              (get-current-stack
                (interpreter-stacks context)))))
      (declare (type 32-bit-integer top-element))
      (push-onto-integer-stack
        (get-current-stack
          (interpreter-stacks context))
        top-element)
      (push-onto-integer-stack
        (get-current-stack
          (interpreter-stacks context))
        top-element)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol (eql #\@)))
  (declare (type Standard-State state))
  (declare (type character      symbol)
           (ignore              symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (setf (interpreter-has-halted-p context) T)
    (set-context-state context
      (make-instance 'Halt-State :context context)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  Standard-State)
                           (symbol character))
  (declare (type Standard-State state))
  (declare (type character      symbol))
  (when (digit-char-p symbol)
    (push-onto-integer-stack
      (get-current-stack
        (interpreter-stacks
          (get-state-context state)))
      (digit-char-p symbol)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Skip-State" operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-symbol ((state  Skip-State)
                           (symbol character))
  (declare (type Skip-State state))
  (declare (type character  symbol)
           (ignore          symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (set-context-state context
      (make-instance 'Standard-State :context context)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "String-Mode-State" operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-symbol ((state  String-Mode-State)
                           (symbol (eql #\")))
  (declare (type String-Mode-State state))
  (declare (type character         symbol))
  (let ((context (get-state-context state)))
    (declare (type Interpreter context))
    (set-context-state context
      (make-instance 'Standard-State :context context)))
  (values))

;;; -------------------------------------------------------

(defmethod process-symbol ((state  String-Mode-State)
                           (symbol character))
  (declare (type String-Mode-State state))
  (declare (type character         symbol))
  (push-onto-integer-stack
    (get-current-stack
      (interpreter-stacks
        (get-state-context state)))
    (char-code symbol))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Halt-State" operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-symbol ((state  Halt-State)
                           (symbol character))
  (declare (type Halt-State state))
  (declare (type character  symbol))
  (error 'Program-Halted-Error :processed-symbol symbol)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Malfunge-Error (error)
  ()
  (:documentation
    "The ``Malfunge-Error'' condition type serves as a substratum upon
     which are expected to be edified all error species appertaining to
     a Malfunge program's conception, evaluation, and execution."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Malfunge-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :type          integer-stack
    :documentation "The stack which, being destitute of elements, has
                    been requested to deliver or remove its top item.")
   (offending-action
    :initarg       :offending-action
    :initform      "peek into or pop from"
    :reader        empty-stack-error-offending-action
    :type          string
    :documentation "A textual representation of the action whose attempt
                    at the OFFENDED-STACK in its vacant state has
                    instigated this erroneous situation.
                    ---
                    This description ought to ostend a sufficient
                    concinnity with the weftage inwith whose context the
                    insertion proceeds, and which amount to:
                    
                      Cannot {OFFENDING-ACTION} an empty stack.
                    
                    The default tmema amounts to \"peek into or pop
                    from\", ultimately conducing the entire message's
                    gendure:
                    
                      Cannot peek into or pop from an empty stack."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (type destination       stream))
      (format stream "Cannot ~a an empty stack."
        (empty-stack-error-offending-action condition))))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology ensues from the
     attempt to indagate or remove an element from an empty stack."))

;;; -------------------------------------------------------

(define-condition Full-Stack-Error (Malfunge-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        full-stack-error-offended-stack
    :type          integer-stack
    :documentation "The stack which, during the state of its
                    MAXIMUM-SIZE's achievement, has been ordered to
                    receive a further element.")
   (maximum-size
    :initarg       :maximum-size
    :initform      (error "Missing maximum stack size.")
    :reader        full-stack-error-maximum-size
    :type          fixnum
    :documentation "The inclusive maximum tally of admissible elements
                    the same has been violated by a behest."))
  (:report
    (lambda (condition stream)
      (declare (type Full-Stack-Error condition))
      (declare (type destination      stream))
      (format stream "Cannot push onto a stack whose maximum size ~
                      of ~d elements has already been reached."
        (full-stack-error-maximum-size condition))))
  (:documentation
    "The ``Full-Stack-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology ensues from the
     attempt to insert an element into an already surfeited stack."))

;;; -------------------------------------------------------

(define-condition Invalid-Playfield-Position-Error (Malfunge-Error)
  ((playfield
    :initarg       :playfield
    :initform      (error "Missing playfield.")
    :reader        invalid-playfield-position-error-playfield
    :type          Playfield
    :documentation "The playfield which could not accommodate the
                    invalid position (PROBED-X, PROBED-Y).")
   (probed-x
    :initarg       :probed-x
    :initform      (error "Missing probed x-coordinate.")
    :reader        invalid-playfield-position-error-probed-x
    :type          fixnum
    :documentation "The probed position's zero-based x-coordinate.")
   (probed-y
    :initarg       :probed-y
    :initform      (error "Missing probed y-coordinate.")
    :reader        invalid-playfield-position-error-probed-y
    :type          fixnum
    :documentation "The probed position's zero-based y-coordinate.")
   (intended-action
    :initarg       :intended-action
    :initform      "access"
    :reader        invalid-playfield-position-error-intended-action
    :type          string
    :documentation "An optional compendious description of the invalid
                    position access' purpose, thilk has instigated this
                    anomalous circumstance."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Playfield-Position-Error condition))
      (declare (type destination                      stream))
      (format stream "Cannot ~a the point with the coordinates ~
                      (~d, ~d), as it violates the playfield's ~
                      admissible bournes, spanned by the column ~
                      gamut of [0, ~d] and row range of [0, ~d]."
        (invalid-playfield-position-error-intended-action condition)
        (invalid-playfield-position-error-probed-x        condition)
        (invalid-playfield-position-error-probed-y        condition)
        (1- +PLAYFIELD-WIDTH+)
        (1- +PLAYFIELD-HEIGHT+))))
  (:documentation
    "The ``Invalid-Playfield-Position-Error'' condition type serves in
     the apprizal about an anomalous circumstance whose etiology emerges
     from the attempt to reference a playfield cell by a x-y position
     ayond its admissible marches."))

;;; -------------------------------------------------------

(define-condition Program-Halted-Error (Malfunge-Error)
  ((processed-symbol
    :initarg       :processed-symbol
    :initform      (error "Missing processed-symbol.")
    :reader        program-halted-error-processed-symbol
    :type          character
    :documentation "The symbol whose evaluation, maugre the Malfunge
                    program's termination, served to incur this
                    anomalous state."))
  (:report
    (lambda (condition stream)
      (declare (type Program-Halted-Error condition))
      (declare (type destination          stream))
      (format stream "Cannot process the symbol \"~c\", as the ~
                      program has already terminated."
        (program-halted-error-processed-symbol condition))))
  (:documentation
    "The ``Program-Halt-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology emerges from the
     attempt to process a playfield symbol after the program's explicit
     termination by adminiculum of the \"@\" operation's involvement."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of entry operation.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Malfunge (code)
  "Interprets the piece of Malfunge source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-playfield
        (concatenate-lines code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hello world", succeeded by an aefauld "null character".
(interpret-Malfunge
  (concatenate-lines
    "0\"!dlrow olleh\"o>d;v"
    "                >  <"
    "                @"))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Malfunge
  (concatenate-lines
    ".o>dd:v"
    "  >   <"
    "  @"))

;;; -------------------------------------------------------

;; Perpetually repeating cat program.
(interpret-Malfunge
  (concatenate-lines
    "o>,;v"
    " ^  <"))
