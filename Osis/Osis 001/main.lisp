;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Osis", invented by the Esolang user "A" and presented on
;; August 15th, 2019, the kenspeckle proprium commorant in its haecceity
;; relates to the pursuit of a mathematically founded "sequence-based"
;; programming paradigm, in whose context an infinite sequence of signed
;; integer numbers is gradually populated in an eternally perpetuated
;; iteration, governing a strictly monotically incrementing index,
;; commencing from zero (0), whose sequence term generation proceeds by
;; mediation of rules, or, if such are defined, base cases.
;; 
;; 
;; Concept
;; =======
;; The Osis programming language offers a sequence-based programming
;; environment inwith an integer-valued sequence and a stack
;; partaking of the same species of members operate in champarty in
;; their pursuit of arithmetic competences. All operations' expression
;; proceeds in terms of single-character identifiers, the devers'
;; appropriation the stack's indagation and manipulation.
;; 
;; == OSIS: A SEQUENCE-BASED PROGRAMMING LANGUAGE ==
;; The Osis language's author assigns a membership to the
;; "sequence-based" paradagim to their creation, a diction that relates
;; to the mathematical concept of sequences as ordered lists as the
;; programming effort's substratum.
;; 
;; The sequence as a mathematical construct's haecceity, as a
;; foundational element of Osis, shall receive further tendance in the
;; subsequent sections.
;; 
;; == SEQUENCES IN MATHEMATICS: ORDERED LISTS ==
;; The mathematical realm assigns to the sequence the diorism of an
;; ordered enumeration, the circumference of the same homologates
;; duplicates' presence.
;; 
;; == SEQUENCE ELEMENTS (TERMS) RESPOND TO INDICES ==
;; A sequence's members are stevened its "terms" or "elements", the
;; selection therefrom proceeds by adminiculum of a designator nevened
;; the "index" or "rank", its amenability consigned to integer objects.
;; The elements themselves may be desumed from any species admissible to
;; one's delectation.
;; 
;; == SEQUENCES MAY BE FINITE OR INFINITE ==
;; A bifurcation applies to finite and infinite sequences, the former
;; class bears the proprium of its content's tallying into the "length".
;; 
;; == SEQUENCES MAY BE DESCRIBED BY LISTINGS OR RULES (FORMULAE) ==
;; A second criterion for the subsumption of the concrete sequence
;; specimen appertains to its terms' formulation, an attribute which
;; may either be by listing some or all of its members, or proceeding
;; by a formula's, or rule's, specification.
;; 
;; An example for the first case, the listed variant, shall the kithed
;; in the sequence of the first ten odd numbers:
;; 
;;   (1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
;; 
;; Extrapolating in a deliberation similiter, an infinite listing
;; exposes, without its --- impossibly to accomplish --- elements'
;; exhaustion:
;; 
;;   (2, 3, 5, 7, 11, 13, 17, 19, 23, 27, ...)
;; 
;; The second contingency for a dioristic expression, a treatise on a
;; sequence by a formula or rule, realizes its interpretation via a
;; statement of the operations imposed as requisites for its terms'
;; replication.
;; 
;; In a reference to the incipient listing example, the following
;; formula yields a, now infinite, tally of odd numbers:
;; 
;;   a(n) = 1 + (2 * n), for n >= 0
;; 
;; Complying with this stated forbisen, the following elements --- an
;; excerpt of a inchoate subsequence shall be accommodated --- manifest:
;; 
;;   -----------------------
;;   n | a(n) = 1 + (2 * n)
;;   --+--------------------
;;   0 | 1 + (2 * 0) = 1
;;   .......................
;;   1 | 1 + (2 * 1) = 3
;;   .......................
;;   2 | 1 + (2 * 2) = 5
;;   .......................
;;   3 | 1 + (2 * 3) = 7
;;   .......................
;;   4 | 1 + (2 * 4) = 8
;;   .......................
;;   [...]
;;   -----------------------
;; 
;; The just ostended design employs a very widespread notation for a
;; sequence as a dependency on its index n: a(n). A grade of amplified
;; sophistication in this subject's viscerals shall be the subsequent
;; section's dation.
;; 
;; == SEQUENCE NOTATION ==
;; A very common notation assigns to an element's reference, amplected
;; in the possession of the sequence "a" the symbolic expression
;; 
;;   a(n)
;; 
;; where n defines the term's index.
;; 
;; Many cases mold the abstraction into a specialized design, where
;; the terms of the sequence a are appropriated from a numeric type, and
;; its indices chosen as integral values greater than or equal to
;; zero (0).
;; 
;; As a consectary, the following terminology apprehends significance
;; for a term's inquisition:
;; 
;;   a(n)
;; 
;; where:
;; 
;;   n is an element of the natural numbers >= 0, and
;;   a(i) is an element of the real numbers, for 0 <= i < length(a).
;; 
;; == OSIS SEQUENCES: INFINITE, RULE-BAED LISTS OF SIGNED INTEGERS ==
;; A very particular and bespoke ilk of sequence definition issues from
;; Osis's deployment, enumerating its infinite manifold of signed
;; integer terms by mediation of non-negative integers indices, their
;; delineation a produce of a formula whose compenency entails merely
;; Osis instructions.
;; 
;; A more compendious, more stringent exhibition shall be supplied in a
;; tabular guise:
;; 
;;   ------------------------------------------------------------------
;;   Attribute   | Design chosen by Osis
;;   ------------+-----------------------------------------------------
;;   Length      | Infinite.
;;   ..................................................................
;;   Description | Formula-based, with every token of the rule being an
;;               | Osis instruction.
;;   ..................................................................
;;   Elements    | Signed integer numbers of any magnitude.
;;               | Formally, it holds for such a sequence a(n):
;;               |   a(n) is an element of the integer numbers,
;;               | with
;;               |   -infinity <= a(n) <= +infinity
;;               | for every n being an index.
;;   ..................................................................
;;   Indices     | Non-negative integer numbers greater than or equal
;;               | to zero, and extending into the positive infinite
;;               | laterality with a step size of one (1).
;;               | Formally, for every index n it holds:
;;               |   0 <= n <= +infinity
;;               | with
;;               |   n2 - n1 = 1
;;               | for every n1, n2 being an index.
;;   ------------------------------------------------------------------
;; 
;; == THE STACK: A PARHEDRAL SALVATORY ==
;; The sequence endowed with Osis's paravaunt storage mechanism, a
;; parergon to this data management aspect enjoys a supplementation by
;; the program stack, a last-in first-out collection of signed integer
;; objects admissible to any magnitude and polarity.
;; 
;; The lacuna of traditional expression evaluation concepts, namely
;; variables and composition by value passing, is mitigated in this
;; language by the stack's unbounded capacity, the calculations applied
;; to which are received as the ultimity for the current iteration
;; index' contribution to the predominant sequence.
;; 
;; == OSIS PROGRAMS: A COMPOSITION OF FORMULA AND BASE CASES ==
;; Osis qua a rule-based description of a numeric sequence exercises a
;; bifurcation of its code into a twissel of general comparments: the
;; formula, the circumference of its instruction, and an optional list
;; of base cases that serve in the first sequence element's immediate
;; specification, produced in the obverse order of their intended
;; assignment.
;; 
;; A visual adduction limns for us such:
;; 
;;   formula baseCases
;; 
;; which, ensuing from a more detailed exposition, ostends:
;; 
;;   formula baseCase(m) ... baseCase(m-1) baseCase(1) baseCase(0)
;; 
;; where m >= 0.
;; 
;; Please note that the base cases, always whose inception always
;; conflates with the first sequence element, a(0), are expected to
;; follow a kenspeckle arrangement in the widdershins direction, with
;; the desinent base case item, baseCase(m), empight immediately after
;; the the code section, followed by the penultimate sequence element to
;; initialize, and, finally, concluding with the actual first sequence
;; term, baseCase(0).
;; 
;; In concord with this peculiarism, in order to attend to the
;; specification of the sequence elements
;; 
;;   a(0) = 10
;;   a(1) = -5
;;   a(2) =  4
;; 
;; the respective base cases ought to state:
;; 
;;   4  -5  10
;; 
;; A more expressive illustration for this base case tally of m = 3
;; shall be imparted alow:
;; 
;;   -----------------------------------------------------------
;;   Base case value  |      4      |     -5      |     10
;;   -----------------+-------------+-------------+-------------
;;   Base case name   | baseCase(2) | baseCase(1) | baseCase(0)
;;   -----------------+-------------+-------------+-------------
;;   Sequence element |        a(2) |        a(1) |        a(0)
;;   -----------------------------------------------------------
;; 
;; == THE FORMULA: INSTRUCTIONS TO COMPUTE THE CURRENT SEQUENCE TERM ==
;; The formula section, laying its amplectation around a list of zero or
;; more Osis instructions, contributes during each of the n cycles,
;; where 0 <= n <= +infinity, the rules according to which the sequence
;; element at the n-th position shall be supputated.
;; 
;; This computation manifests solely if for the respective n-th term no
;; base case has been defined; its presence, however, supersedes the
;; rules' efficiency for an immediate insertion of the respective
;; literal integer object.
;; 
;; == BASE CASES: DEFAULT VALUES FOR SEQUENCE TERMS ==
;; A sequence of zero or more base cases, defined in the guise of
;; literal signed integer objects, may be procured, such define with
;; a more elevated puissance that the formula the first terms of the
;; sequence.
;; 
;; Given a list of m base cases
;; 
;;   baseCase(0) baseCase(1) ... baseCase(m)
;; 
;; for the sequence a, the introducing m sequence terms are specified as
;; follows:
;; 
;;   a(0) = baseCase(0)
;;   a(1) = baseCase(1)
;;   ...
;;   a(m) = baseCase(m)
;; 
;; Continuing with the (m+1)-th index, the terms including a(m+1) and
;; perpetuting into infinity are set according to the formula:
;; 
;;   a(m+1) = compute formula
;;   a(m+2) = compute formula
;;   ...
;; 
;; == THE BASE CASE a(0) CONSTITUTES AN IMPLICIT DEFAULT ==
;; Several instructions' bailiwicks imbricate in their dependency upon
;; some prevenient sequence term, siccan may be adduced, as a forbisen,
;; as the immediately preceding cyles, a(n-1), or an absolute, like
;; holds in a(2). For the particular case of the first element's
;; reference, a(0), if no base case has been defined, the state is
;; assumed to be zero (0), that is, in these cases of acquisitions:
;; 
;;   a(0) = 0
;; 
;; == OSIS PROGRAMS OPERATE IN INFINITE CYCLES ==
;; Its telos an infinite integer sequence's computation, an Osis program
;; perpetuates by repeating the formula segment in an eternal cycle,
;; governing a counter, the index, or "parameter", the same designates
;; the currently supputated sequence element's position, commencing with
;; the value zero (0), and augmented by a step size of one (1) upon each
;; cycle's patration.
;; 
;; The following pseudocode formulation's onus shall be the principle's
;; elucidation in a general diction entalented with a higher mete of
;; stringency:
;; 
;;   Input:
;;     sequence:  An infinite, indexed sequence of signed integers, the
;;                indices commencing with zero (0).
;;     formula:   A list of zero or more instructions which define the
;;                rules for any of the sequence elements' computation
;;                during repetitions which have no base case defined.
;;     baseCases: A list of zero or more base cases which, indexed
;;                from zero (0), provide the default values for the
;;                sequence.
;;   
;;   Procedure:
;;     let index <- 0
;;     
;;     repeat
;;       if baseCases(index) exists then
;;         sequence(index) <- baseCases(index)
;;       else
;;         sequence(index) <- execute the formula
;;       end if
;;       
;;       index <- index + 1 
;;     end repeat
;; 
;; 
;; Architecture
;; ============
;; Osis deploys a twain of data structures for its architecture's
;; patration: the integer-valued, contingenly infinite sequence as the
;; paravaunt and permanent constituent, and the stack operating on the
;; same species of tokens, maugre entrusted with a rather provisional
;; adminiculum, its responsibility the provision of a temporary data
;; storage during an iteration cycle's formula supputation, ere the
;; final result is inserted into the sequence at the current repetition
;; index.
;; 
;; 
;; Data Types
;; ==========
;; Osis operates on signed integer numbers manumitted from any
;; polarity's or magnitude's imposition.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical perspective, an Osis program's conformation
;; follows a champarty of a formula segment and a base cases listing,
;; the former apprehends zero or more one-character instructions, while
;; the latter, if present, ensues from a segregating space sequence,
;; with zero or more space-separated signed integer literals.
;; 
;; == GRAMMAR ==
;; An amplification in its stringency describes the donet's formulation
;; alow in the Extended Backus-Naur Form's (EBNF) diction:
;; 
;;   program       := padding , formula , spaces , baseCases , padding ;
;;   formula       := { command } ;
;;   command       := unaryOp | binaryOp | sequenceRef | digit ;
;;   unaryOp       := "_" | '"' | "|" | "$" | "!" | "," ;
;;   binaryOp      := "+" | "-" | "*" | "/" | "%" | "^" ;
;;   sequenceRef   := ";" | "{" | "}" | "(" | ":" | "`" ;
;;   baseCases     := [ baseCase , { spaces , baseCase } ] ;
;;   baseCase      := signedInteger ;
;;   signedInteger := [ "+" | "-" ] , digit , { digit } ;
;;   digit         := "0" | "1" | "2" | "3" | "4"
;;                 |  "5" | "6" | "7" | "8" | "9"
;;                 ;
;;   padding       := [ spaces ] ;
;;   spaces        := space , { spaces } ;
;;   space         := " " ;
;; 
;; 
;; Instructions
;; ============
;; A tally of 28 members collaborates in the delineation of Osis's
;; instruction set's entirety, everichon among these an arithmetic
;; warklume, in its compass embraced unary and binary operations, the
;; insertion of numeric literals, as well as references to the hitherto
;; supputated number sequence's terms.
;; 
;; == CATEGORIES OF INSTRUCTIONS ==
;; Governed by a strictly arithmetic instruction set, Osis operative
;; facilities subsume into a rather generous dispansion of memberships,
;; the same shall be the following cursory enumeration's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Category           | Command | Causatum
;;   -------------------+---------+------------------------------------
;;   Unary operations   | _       | Negate sign
;;                      |..............................................
;;                      | "       | Double
;;                      |..............................................
;;                      | |       | Halve
;;                      |..............................................
;;                      | $       | Square
;;                      |..............................................
;;                      | !       | Factorial
;;                      |..............................................
;;                      | ,       | Prime
;;   ------------------------------------------------------------------
;;   Binary operations  | +       | Addition
;;                      |..............................................
;;                      | -       | Subtraction
;;                      |..............................................
;;                      | *       | Multiplication
;;                      |..............................................
;;                      | /       | Integer division
;;                      |..............................................
;;                      | %       | Remainder (modulo)
;;   ------------------------------------------------------------------
;;   References         | ;       | Push a(n-t), where t = top of stack
;;                      |..............................................
;;                      | {       | Push a(n-1)
;;                      |..............................................
;;                      | }       | Push a(n-2)
;;                      |..............................................
;;                      | (       | Push a(n-3)
;;                      |..............................................
;;                      | :       | Push a(t), where t = top of stack
;;                      |..............................................
;;                      | `       | Push n (cycle index or parameter)
;;   ------------------------------------------------------------------
;;   Numerals           | 0       | Push the digit 0
;;                      |..............................................
;;                      | 1       | Push the digit 1
;;                      |..............................................
;;                      | 2       | Push the digit 2
;;                      |..............................................
;;                      | 3       | Push the digit 3
;;                      |..............................................
;;                      | 4       | Push the digit 4
;;                      |..............................................
;;                      | 5       | Push the digit 5
;;                      |..............................................
;;                      | 6       | Push the digit 6
;;                      |..............................................
;;                      | 7       | Push the digit 7
;;                      |..............................................
;;                      | 8       | Push the digit 8
;;                      |..............................................
;;                      | 9       | Push the digit 9
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; Under a stringency of compendiousness in its exposition, the table
;; adduced alow shall adhibit a requisite mete of gnarity with the Osis
;; language's instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   _       | Pops the top stack element, negates its sign, and pushes
;;           | the result unto the stack.
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   "       | Pops the top stack element, multiplies it by two (2),
;;           | and pushes the product unto the stack.
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   |       | Pops the top stack element, divides it by two (2), thus
;;           | halving it, and pushes the quotient unto the stack.
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   $       | Pops the top stack element, multiplies it by itself,
;;           | thus squaring it, and pushes the product unto the stack.
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   ,       | Pops the top stack element, "t", calculates the t-th
;;           | prime number, and pushes the same unto the stack.
;;           |---------------------------------------------------------
;;           | The prime number's enumeration starts with the index
;;           | zero (0), which answers to the smallest prime number
;;           | two (2).
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   +       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the sum utilizing "a" as the
;;           | augend and "b" as the addend, and pushes the sum unto
;;           | the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b   <- pop from stack
;;           |   let a   <- pop from stack
;;           |   let sum <- a + b
;;           |   push sum unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   -       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the difference utilizing "a"
;;           | as the minuend and "b" as the subtrahend, and pushes the
;;           | difference unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b          <- pop from stack
;;           |   let a          <- pop from stack
;;           |   let difference <- a - b
;;           |   push difference unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   *       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the product utilizing "a" as
;;           | the multiplicand and "b" as the multiplier, and pushes
;;           | the product unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b       <- pop from stack
;;           |   let a       <- pop from stack
;;           |   let product <- a * b
;;           |   push product unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   /       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the integer division quotient
;;           | utilizing "a" as the dividend and "b" as the divisor,
;;           | and pushes the integer quotient unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b        <- pop from stack
;;           |   let a        <- pop from stack
;;           |   let quotient <- round(a / b)
;;           |   push quotient unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   %       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the remainder utilizing "a" as
;;           | the dividend and "b" as the divisor, and pushes the
;;           | remainder unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b         <- pop from stack
;;           |   let a         <- pop from stack
;;           |   let remainder <- a modulo b
;;           |   push remainder unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   ^       | Pops the top stack element, "b", pops the new top stack
;;           | element, "a", calculates the power utilizing "a" as
;;           | the base and "b" as the exponent, and pushes the power
;;           | value unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let b     <- pop from stack
;;           |   let a     <- pop from stack
;;           |   let power <- a ^ b
;;           |   push power unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   !       | Pops the top stack element, "t", calculates its
;;           | factorial t!, and pushes the same unto the stack.
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   ;       | Pops the top stack element, "t", queries the sequence
;;           | element a(n-t), where n represents the currently
;;           | calculated sequence index, and pushes a(n-t) unto the
;;           | the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, given
;;           |   n --- the currently calculated sequence index,
;;           | the following holds:
;;           |   let indexOffset  <- pop from stack
;;           |   let sequenceTerm <- a(n - indexOffset)
;;           |   push sequenceTerm unto stack
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   {       | Queries the sequence element a(n-1), where n represents
;;           | the currently calculated sequence index, and pushes
;;           | a(n-1) unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, given
;;           |   n --- the currently calculated sequence index,
;;           | the following holds:
;;           |   let sequenceTerm <- a(n - 1)
;;           |   push sequenceTerm unto stack
;;           |---------------------------------------------------------
;;           | If the sequence term a(0) is required, but not
;;           | specified, its value is assumed to be zero (0).
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   }       | Queries the sequence element a(n-2), where n represents
;;           | the currently calculated sequence index, and pushes
;;           | a(n-2) unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, given
;;           |   n --- the currently calculated sequence index,
;;           | the following holds:
;;           |   let sequenceTerm <- a(n - 2)
;;           |   push sequenceTerm unto stack
;;           |---------------------------------------------------------
;;           | If the sequence term a(0) is required, but not
;;           | specified, its value is assumed to be zero (0).
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   (       | Queries the sequence element a(n-3), where n represents
;;           | the currently calculated sequence index, and pushes
;;           | a(n-3) unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, given
;;           |   n --- the currently calculated sequence index,
;;           | the following holds:
;;           |   let sequenceTerm <- a(n - 3)
;;           |   push sequenceTerm unto stack
;;           |---------------------------------------------------------
;;           | If the sequence term a(0) is required, but not
;;           | specified, its value is assumed to be zero (0).
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   :       | Pops the topmost element, "t", employs the same as an
;;           | index to query the t-th sequence element a(t), and
;;           | pushes a(t) unto the stack.
;;           |---------------------------------------------------------
;;           | In pseudocode's diction, the following holds:
;;           |   let index        <- pop from stack
;;           |   let sequenceTerm <- a(index)
;;           |   push sequenceTerm unto stack
;;           |---------------------------------------------------------
;;           | If the sequence term a(0) is required, but not
;;           | specified, its value is assumed to be zero (0).
;;           |---------------------------------------------------------
;;           | If the program stack is destitute of a sufficient tally
;;           | of elements required for this operation, an error of the
;;           | type "EmptyStack" is signaled.
;;   ..................................................................
;;   `       | Pushes the current sequence index, "n", also nevened the
;;           | "parameter", unto the stack.
;;           |---------------------------------------------------------
;;           | Please note that the parameter n commences with the
;;           | value zero (0) and increments by a value of one (1)
;;           | after each cycle's patration. The currently processed
;;           | element of the program's series, as a corollary, is
;;           | denoted by "a(n)".
;;           |---------------------------------------------------------
;;           | If the sequence term a(0) is required, but not
;;           | specified, its value is assumed to be zero (0).
;;   ..................................................................
;;   0       | Push the number zero (0) unto the stack.
;;   ..................................................................
;;   1       | Push the number one (1) unto the stack.
;;   ..................................................................
;;   2       | Push the number two (2) unto the stack.
;;   ..................................................................
;;   3       | Push the number three (3) unto the stack.
;;   ..................................................................
;;   4       | Push the number four (4) unto the stack.
;;   ..................................................................
;;   5       | Push the number five (5) unto the stack.
;;   ..................................................................
;;   6       | Push the number six (6) unto the stack.
;;   ..................................................................
;;   7       | Push the number seven (7) unto the stack.
;;   ..................................................................
;;   8       | Push the number eight (8) unto the stack.
;;   ..................................................................
;;   9       | Push the number nine (9) unto the stack.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Osis protolog, maugre its establishment of elucidations and
;; forbisens, tholes a tally of ambiguities, the etiology registered for
;; their intrusion is elicited by several revision committed through the
;; original language author themself, and the status of inchoate
;; subsistence which yet perpetuates through the causata of the
;; standard's senescence. A select subset of the blemishes shall be the
;; following treatise's subject.
;; 
;; == HOW SHALL THE FORMULA'S AND BASE CASES' SEGREGATION PROCEED? ==
;; Both engaged in a physical and a logical vista, Osis cleaves its
;; programs into a formula or rule segment and a succeeded sequence of
;; zero or more bases cases, each comprised of an integral datum. Maugre
;; the segregation, the most recent language standard rendition does not
;; impose nor rede a sepiment for the dioristic twissel.
;; 
;; The duality of the decimal digits and the arithmetics signs, "+" and
;; "-", inflicts an inroad for confounding circumstances in the code,
;; the ultimity conceived from these frailties accounts for severe
;; ambivalencies' contingency at the march betwixt the formula segment's
;; desinence and the first base case's inchoation.
;; 
;; Ostended by this piece of Osis code's adduction
;; 
;;   +1 2
;; 
;; the unremedied situation replicates a twain of divergent
;; interpretations for the digit 1 --- as either a component of the
;; formula section, that is,
;; 
;;   Formula:    +1
;;   Base cases: 2
;; 
;; or, alternatively, as the first base case:
;; 
;;   Formula:    +
;;   Base cases: 1, 2
;; 
;; A rendition of the language, in its preveniency dated to August 15th,
;; 2019; 13:19, posseded a period token, ".", as the vinculum betwixt
;; the formula's coda and the base cases' instalment, such proprium has
;; subsequently tholed its elision.
;; 
;; It has been adjudged, in the face of the space character's presence
;; as a division mechanism betwixt base case elements, to introduce the
;; same constituent as an imperative sepiment betwixt the formula's
;; termination and the base cases' introduction.
;; 
;; In reference to the example adduced aboon, namely,
;; 
;;   +1 2
;; 
;; the correct design for a base case pair (1, 2) prescribes the
;; formulation
;; 
;;   + 1 2
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-14
;; 
;; Sources:
;;   [bendersky2023myfavprimeng]
;;   Eli Bendersky, "My favorite prime number generator",
;;     August 22nd, 2023
;;   URL: "https://eli.thegreenplace.net/2023/
;;         my-favorite-prime-number-generator/"
;;   Notes:
;;     - Demonstrates a potentially infinite prime number generator
;;       implementation based upon the Sieve of Eratosthenes.
;;     - The implementation is realized in the Python programming
;;       language.
;;       Please note, if not entalented with the requisitum of gnarity
;;       for the language, the following diorisms:
;;       o The operator "in", when deployed in a dictionary's context,
;;         probes for a key in the same.
;;       o The method "Dictionary.setdefault(key, defaultValue)"
;;         determines whether the KEY exists in the DICITIONARY, and, if
;;         such is ascertained, returns its associated value; otherwise,
;;         upon the indicator's absence, a new entry with the KEY mapped
;;         to the DEFAULTVALUE is inserted, and the latter datum
;;         returns.
;;   
;;   [eppstein2002sieveoferatosthenes]
;;   David Eppstein, "SIEVE OF ERATOSTHENES (PYTHON RECIPE)",
;;     February 28th, 2002
;;   URL: "https://code.activestate.com/recipes/
;;         117119-sieve-of-eratosthenes/"
;;   Notes:
;;     - Demonstrates a potentially infinite prime number generator
;;       implementation based upon the Sieve of Eratosthenes.
;;     - The implementation is realized in the Python programming
;;       language.
;;       Please note, if not entalented with the requisitum of gnarity
;;       for the language, the following diorisms:
;;       o The operator "in", when deployed in a dictionary's context,
;;         probes for a key in the same.
;;       o The method "Dictionary.setdefault(key, defaultValue)"
;;         determines whether the KEY exists in the DICITIONARY, and, if
;;         such is ascertained, returns its associated value; otherwise,
;;         upon the indicator's absence, a new entry with the KEY mapped
;;         to the DEFAULTVALUE is inserted, and the latter datum
;;         returns.
;;   
;;   [esolang2019Osis]
;;   The Esolang contributors, "Osis", December 25th, 2019
;;   URL: "https://esolangs.org/wiki/Osis"
;;   
;;   [pierce2021sequences]
;;   Rod Pierce, "Sequences", in "Math Is Fun", October 15th, 2021
;;   Accessed February 17th, 2024
;;   URL: "https://www.mathsisfun.com/algebra/sequences-series.html"
;;   Notes:
;;     - Describes the sequence as a mathematical concept in a lucid
;;       parlance.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of predicate operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-entry-satisfies-p (hash-table predicate)
  "Determines whether every entry of the HASH-TABLE satisfies the
   PREDICATE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   The PREDICATE must be a dyadic function which accepts the currently
   probed HASH-TABLE key and the affiliated value in this exact order,
   and responds with a generalized boolean value, construing a
   non-``NIL'' output as a compliance to the fathoming, and ``NIL'' as
   a failure in the key-value twissel's eligibility. The signature thus
   conforms to:
     lambda (key value) => generalized-boolean"
  (declare (type hash-table         hash-table))
  (declare (type (function (* *) *) predicate))
  (the boolean
    (not (null
      (loop
        for    key of-type T being the hash-keys in hash-table
        using  (hash-value value)
        always (funcall predicate key value))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
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
            (every-entry-satisfies-p
              (the hash-table candidate)
              #'(lambda (key value)
                  (declare (type T key))
                  (declare (type T value))
                  (and (typep key   key-type)
                       (typep value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   which adhere to the ELEMENT-TYPE, its default assumed as the generic
   sentinel ``*''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized variation of Osis
   instructions."
  '(member
    :negation
    :double
    :halve
    :square
    :factorial
    :modulo
    :addition
    :subtraction
    :multiplication
    :integer-division
    :power
    :prime
    :push-a[n-t]
    :push-a[n-1]
    :push-a[n-2]
    :push-a[n-3]
    :push-a[n]
    :push-index
    :push-0
    :push-1
    :push-2
    :push-3
    :push-4
    :push-5
    :push-6
    :push-7
    :push-8
    :push-9))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines an association of a command
   character to a token representation thereof, realized in a hash
   table's mold, the keys among which comprehend ``character'' objects,
   the same answer to ``Token'' instances."
  '(hash-table-of character Token))

;;; -------------------------------------------------------

(deftype formula ()
  "The ``formula'' type defines a mathematical sequence's substrate, the
   formula or rule, as a list compact of zero or more Osis commands."
  '(list-of command))

;;; -------------------------------------------------------

(deftype numeric-sequence ()
  "The ``numeric-sequence'' type defines a sequence in the mathematical
   sense, incorporating in this diorism an ordered list of elements, the
   terms, as a vector composed of zero or more signed integer objects."
  '(vector integer *))

;;; -------------------------------------------------------

(deftype index ()
  "The ``index'' type defines an index into a mathematical sequence as a
   non-negative integer number greater than or equal to zero (0), but
   unbrindled along the upper march, and thus a commorant of the
   integral interval [0, +infinity].
   ---
   The common imposition reding the subscript's lower bourne as one (1)
   has been extended into a zero (0) element's membership in order to
   accommodate a tolerance for the term a(0), usually in its purpose to
   reference the respective base case."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype prime-candidate ()
  "The ``prime-candidate'' type defines an number theoretically
   covenable for the role of a prime number, however, destitute yet of
   an actual ascertainment."
  '(integer 2 *))

;;; -------------------------------------------------------

(deftype composite-number-map ()
  "The ``composite-number-map'' type defines a mapping of composite
   integer numbers to a list of those prime numbers that attest their
   key's property of being a composite, manifesting in a hash table, the
   keys of which are represented by ``prime-candidate'' objects, while
   each affiliated value comprehends a list of the ``prime-candidate''s
   that a veridically prime numbers.
   ---
   Please note the fact that an integer number not employed as a key to
   this map indubitably qualifies as a prime number; as a corollary, all
   keys are non-primes, that is, they pose composite numbers."
  '(hash-table-of prime-candidate (list-of prime-candidate)))

;;; -------------------------------------------------------

(deftype prime-generator ()
  "The ``prime-generator'' type defines a niladic function endowed with
   the dever of producing upon each invocation, commencing with the
   least prime number two (2), the next greater prime.
   ---
   As a corollary, the imposed signature resolves to:
     lambda () => prime-number"
  '(function () prime-candidate))

;;; -------------------------------------------------------

(deftype cycle-handler ()
  "The ``cycle-handler'' type defines a callback function which, invoked
   with the responsible ``Interpreter'' instance, may return any,
   ultimately ignored, result, invested with the expectancy to react in
   some liberally chosen manner to the Osis program's current state.
   ---
   As a corollary, the imposed signature resolves to:
     lambda (interpreter) => ignored-result"
  '(function (Interpreter) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass of which is enumerated, in an exemplary form, by the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class applies itself to the encapsulation of a
   significant object extracted from a piece of Osis source code."
  (type  (error "Missing token type.")  :type keyword :read-only T)
  (value (error "Missing token value.") :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifiers.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'eql)
  "Associates the recognized formula identifier characters with
   representative commands.")

;;; -------------------------------------------------------

(flet ((register-identifier (character command)
        "Associates the character with the COMMAND in the +IDENTIFIER+
         table, substituting any already extant affiliation for the
         former, and returns no value."
        (declare (type character character))
        (declare (type command   command))
        (setf (gethash character +IDENTIFIERS+)
              (make-token :command command))
        (values)))
  (register-identifier #\_ :negation)
  (register-identifier #\" :double)
  (register-identifier #\| :halve)
  (register-identifier #\$ :square)
  (register-identifier #\^ :power)
  (register-identifier #\! :factorial)
  (register-identifier #\% :modulo)
  (register-identifier #\+ :addition)
  (register-identifier #\- :subtraction)
  (register-identifier #\* :multiplication)
  (register-identifier #\/ :integer-division)
  (register-identifier #\, :prime)
  (register-identifier #\; :push-a[n-t])
  (register-identifier #\{ :push-a[n-1])
  (register-identifier #\} :push-a[n-2])
  (register-identifier #\( :push-a[n-3])
  (register-identifier #\: :push-a[n])
  (register-identifier #\` :push-index)
  (register-identifier #\0 :push-0)
  (register-identifier #\1 :push-1)
  (register-identifier #\2 :push-2)
  (register-identifier #\3 :push-3)
  (register-identifier #\4 :push-4)
  (register-identifier #\5 :push-5)
  (register-identifier #\6 :push-6)
  (register-identifier #\7 :push-7)
  (register-identifier #\8 :push-8)
  (register-identifier #\9 :push-9)
  (values))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a command identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (nth-value 1
        (gethash candidate +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun get-command-token (identifier)
  "Returns the ``:command'' token associated with the IDENTIFIER, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type character identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (error "No valid command identifier: ~s." identifier))))

;;; -------------------------------------------------------

(defun get-digit-command (digit)
  "Returns the command associated with the DIGIT, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type (integer 0 9) digit))
  (the command
    (case digit
      (0         :push-0)
      (1         :push-1)
      (2         :push-2)
      (3         :push-3)
      (4         :push-4)
      (5         :push-5)
      (6         :push-6)
      (7         :push-7)
      (8         :push-8)
      (9         :push-9)
      (otherwise (error "Invalid digit: ~s." digit)))))

;;; -------------------------------------------------------

(defun sign-token-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical signum
   symbol, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (not (null
      (member (token-type candidate) '(:plus :minus) :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class provides a lexical analyzer, or lexer, its
   dedication airted at the extraction of tokens from a piece of Osis
   source code."
  (source    (error "Missing lexer source.")
             :type      string
             :read-only T)
  (position  0
             :type      fixnum
             :read-only NIL)
  (character NIL
             :type      (or null character)
             :read-only NIL))

;;; -------------------------------------------------------

(defun has-next-character-p (lexer)
  "Determines whether at least one character follows after the LEXER's
   position cursor, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (not (null
      (array-in-bounds-p
        (lexer-source lexer)
        (1+ (lexer-position lexer)))))))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Returns the LEXER's current character, while concomitantly advances
   the LEXER's position cursor to the next character in its source, if
   possible."
  (declare (type Lexer lexer))
  (the (or null character)
    (prog1
      (lexer-character lexer)
      (setf (lexer-character lexer)
        (when (has-next-character-p lexer)
          (aref
            (lexer-source lexer)
            (incf (lexer-position lexer))))))))

;;; -------------------------------------------------------

(defun read-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   a sequence of one or more spaces and returns a ``:space'' token
   representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (make-token :space
      (with-output-to-string (spaces)
        (declare (type string-stream spaces))
        (loop
          while
            (and (lexer-character lexer)
                 (space-character-p
                   (lexer-character lexer)))
          do
            (write-char (advance-lexer lexer) spaces))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (cond
      ((null (lexer-character lexer))
        (make-token :eof NIL))
      
      ((space-character-p (lexer-character lexer))
        (read-spaces lexer))
      
      ((digit-char-p (lexer-character lexer))
        (make-token :digit
          (digit-char-p
            (advance-lexer lexer))))
      
      ((char= (lexer-character lexer) #\+)
        (make-token :plus
          (advance-lexer lexer)))
      
      ((char= (lexer-character lexer) #\-)
        (make-token :minus
          (advance-lexer lexer)))
      
      ((identifier-character-p (lexer-character lexer))
        (get-command-token
          (advance-lexer lexer)))
      
      ((char= (lexer-character lexer) #\.)
        (make-token :period
          (advance-lexer lexer)))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          (lexer-character lexer)
          (lexer-position  lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Osis program.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Osis-Program
  (:constructor make-osis-program (formula base-cases)))
  "The ``Osis-Program'' class encapsulates the information requisite
   for the replication of an Osis program, namely its formula as an
   instruction list, and its zero or more base cases."
  (formula    (error "Missing formula.")
              :type      formula
              :read-only T)
  (base-cases (error "Missing base cases.")
              :type      (list-of integer)
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (lexer
                             &aux (current-token
                                    (get-next-token lexer)))))
  "The ``Parser'' class receives the dever of an Osis program's
   assemblage from a sequence of tokens produced by a lexer's supply."
  (lexer         (error "Missing lexer.") :type Lexer :read-only T)
  (current-token (make-token :eof NIL)    :type Token :read-only NIL))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the probed token,
   while concomitantly replacing it in the PARSER by the next one
   requested from the underlying lexer; otherwise an error of an
   unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
    (if (token-type-p (parser-current-token parser) expected-token-type)
      (prog1
        (parser-current-token parser)
        (setf (parser-current-token parser)
          (get-next-token
            (parser-lexer parser))))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (parser-current-token parser)))))

;;; -------------------------------------------------------

(defun eat-current-token (parser)
  "Returns the PARSER's current token, while concomitantly replacing it
   in the PARSER by the next one requested from the underlying lexer."
  (declare (type Parser parser))
  (the Token
    (prog1
      (parser-current-token parser)
      (setf (parser-current-token parser)
        (get-next-token
          (parser-lexer parser))))))

;;; -------------------------------------------------------

(defun skip-spaces (parser)
  "Proceeding from the current token maintained by the PARSER, skips a
   sequence of zero or more accolent spaces and returns no value."
  (declare (type Parser parser))
  (loop while (token-type-p (parser-current-token parser) :space) do
    (eat-current-token parser))
  (values))

;;; -------------------------------------------------------

(defun parse-formula (parser)
  "Parses the formula segment of an Osis program in the PARSER's
   context and returns an eligible representation thereof."
  (declare (type Parser parser))
  (skip-spaces parser)
  (the formula
    (loop append
      (case (token-type (parser-current-token parser))
        (:command
          (list
            (token-value
              (eat-current-token parser))))
        (:digit
          (list
            (get-digit-command
              (token-value
                (eat-current-token parser)))))
        ((:plus :minus)
          (list
            (token-value
              (get-command-token
                (token-value
                  (eat-current-token parser))))))
        (otherwise
          (loop-finish))))))

;;; -------------------------------------------------------

(defun parse-base-case (parser)
  "Parses a base case in the PARSER's context and returns an covenable
   representation thereof."
  (declare (type Parser parser))
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (when (sign-token-p (parser-current-token parser))
          (write-char
            (token-value
              (eat-current-token parser))
            digits))
        (loop
          while (token-type-p (parser-current-token parser) :digit)
          do
            (format digits "~d"
              (token-value
                (eat-current-token parser))))))))

;;; -------------------------------------------------------

(defun parse-base-cases (parser)
  "Parses a sequence of zero or more base cases utilizing the PARSER and
   returns a reversed vector comprehending the discoveries."
  (the (list-of integer)
    (nreverse
      (when (token-type-p (parser-current-token parser) :space)
        (loop
          initially
            (skip-spaces parser)
          until
            (token-type-p (parser-current-token parser) :eof)
          collect
            (prog1
              (parse-base-case parser)
              (skip-spaces     parser)))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses an Osis program in the PARSER's context and returns an
   ``Osis-Program'' representation thereof."
  (declare (type Parser parser))
  (the Osis-Program
    (prog1
      (make-osis-program
        (parse-formula    parser)
        (parse-base-cases parser))
      (skip-spaces parser)
      (eat-token   parser :eof))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun double-number (number)
  "Multiplies the NUMBER by two (2) and returns the product."
  (declare (type integer number))
  (the integer
    (* number 2)))

;;; -------------------------------------------------------

(defun halve (number)
  "Divides the NUMBER by two, thus halving its value, and returns the
   rounded quotient."
  (declare (type integer number))
  (the integer
    (round number 2)))

;;; -------------------------------------------------------

(defun square (number)
  "Multiplies the NUMBER by itself and returns the product."
  (declare (type integer number))
  (the integer
    (* number number)))

;;; -------------------------------------------------------

(defun factorial (number)
  "Returns the factorial of the NUMBER."
  (declare (type integer number))
  (the (integer 1 *)
    (if (plusp number)
      (loop
        for factor
          of-type (integer 1 *)
          from    1
          to      number
        for factorial
          of-type (integer 1 *)
          =       1
          then    (* factorial factor)
        finally
          (return factorial))
      1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of prime number generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contains-key-p (hash-table probed-key)
  "Determines whether the HASH-TABLE comprehend the PROBED-KEY,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type hash-table hash-table))
  (declare (type T          probed-key))
  (the boolean
    (not (null
      (nth-value 1
        (gethash probed-key hash-table))))))

;;; -------------------------------------------------------

(defun make-prime-number-generator ()
  "Generates and returns a prime number generator as a niladic function
   which upon each invocation returns the next prime number.
   ---
   The prime number generator's state is delineated by a twain of
   constituents: the composite map and the prime candidate.
   ---
   The composite map maintains an association of a non-prime number, or
   \"composite\", to a list of the primes comprising the same, commonly
   designated in the original Python code as the dictionary \"D\". As a
   consectary, any key embraced by the composite map's key set does not
   constitute a prime number, while the associated list of integral
   objects does.
   ---
   The state's second moiety is contributed by the currently probed
   number, usually designated as \"q\", and perpetually, commencing with
   the smallest prime number 2, incremented by 1 in order to discover
   the subsequent prime."
  (let ((composite-map   (make-hash-table :test #'eql))   ;; D
        (prime-candidate 2))                              ;; q
    (declare (type composite-number-map composite-map))
    (declare (type prime-candidate      prime-candidate))
    (the function
      #'(lambda ()
          (the prime-candidate
            (loop do
              (if (not (contains-key-p composite-map prime-candidate))
                (return
                  (prog1 prime-candidate
                    (setf
                      (gethash
                        (* prime-candidate prime-candidate)
                        composite-map)
                      (list prime-candidate))
                    (incf prime-candidate 1))))
                (let ((factors (gethash prime-candidate composite-map)))
                  (declare (type (list-of prime-candidate) factors))
                  (dolist (factor factors)
                    (declare (type prime-candidate factor))
                    (push factor
                      (gethash
                        (+ factor prime-candidate)
                        composite-map)))
                  (remhash prime-candidate composite-map)
                  (incf prime-candidate 1))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Osis-Error (error)
  ()
  (:documentation
    "The ``Osis-Error'' condition type procures a common foundry upon
     whose basic concepts all errors appertaining to the evaluation of a
     piece of Osis code maintain a reference point."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Osis-Error)
  ((offending-action
    :initarg       :offending-action
    :initform      "Cannot peek into or pop from an empty stack."
    :reader        empty-stack-error-offending-action
    :type          string
    :documentation "A string description of the action whose engagement
                    is peccable of this error's instigation."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (type destination       stream))
      (format stream "~a"
        (empty-stack-error-offending-action condition))))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in an anomalous
     situation's communication whose instigation registers as its
     etiology the attempt to perquire or modify an empty program
     stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          Osis-Program
    :documentation "The Osis program to evaluate.")
   (start-index
    :initarg       :start-index
    :initform      NIL
    :reader        get-start-index
    :type          index
    :documentation "The inclusive start iteration index.")
   (end-index
    :initarg       :end-index
    :initform      NIL
    :reader        get-end-index
    :type          (or null index)
    :documentation "The inclusive maximum iteration index.
                    ---
                    A value of ``NIL'' serves to signal no termination
                    in the sequence calculation.")
   (index
    :initarg       :index
    :initform      (error "Missing sequence index.")
    :reader        get-current-index
    :type          index
    :documentation "The sequence counter or index \"n\", which upon each
                    cycle's patration increments its state.")
   (sequence
    :initform      (make-array 0
                     :element-type    'integer
                     :initial-element 0
                     :adjustable      T
                     :fill-pointer    0)
    :reader        get-sequence
    :type          numeric-sequence
    :documentation "Maintains the hitherto supputated and compiled
                    sequence of terms, or elements, a(n).")
   (stack
    :initform      NIL
    :reader        get-stack
    :type          (list-of integer)
    :documentation "The program stack as a list-based storage of signed
                    integer objects.")
   (prime-generator
    :initform      (make-prime-number-generator)
    :type          prime-generator
    :documentation "A function which upon each invocation returns the
                    next larger prime number.")
   (prime-sequence
    :initform      (make-array 0
                     :element-type    'prime-candidate
                     :initial-element 2
                     :adjustable      T
                     :fill-pointer    0)
    :type          (vector prime-candidate *)
    :documentation "The hitherto gathered prime numbers.")
   (cycle-handler
    :initarg       :cycle-handler
    :initform      NIL
    :type          (or null cycle-handler)
    :documentation "An optional callback function expected to react upon
                    each cycle's patration."))
  (:documentation
    "The ``Interpreter'' class serves in the provision of an entity
     capacitated to evaluate an Osis program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Transfers the base cases from the Osis program maintained by the
   INTERPRETER into its sequence vector and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program sequence) interpreter
    (declare (type Osis-Program     program))
    (declare (type numeric-sequence sequence))
    (let ((base-cases (osis-program-base-cases program)))
      (declare (type (list-of integer) base-cases))
      (dolist (base-case base-cases)
        (declare (type integer base-case))
        (vector-push-extend base-case sequence))))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program
                         &key (start-index   0)
                              (end-index     NIL)
                              (cycle-handler NIL))
  "Creates and returns a new ``Interpreter'' dedicated to the Osis
   PROGRAM's evaluation, the iteration commencing with the START-INDEX
   and terminating in the END-INDEX, if such is non-``NIL'', the latter
   case of which capacitates an infinite repetition, upon each cycle's
   patration, notifying the optional CYCLE-HANDLER about the progress."
  (declare (type Osis-Program            program))
  (declare (type index                   start-index))
  (declare (type (or null index)         end-index))
  (declare (type (or null cycle-handler) cycle-handler))
  (the Interpreter
    (make-instance 'Interpreter
      :program       program
      :start-index   start-index
      :index         start-index
      :end-index     end-index
      :cycle-handler cycle-handler)))

;;; -------------------------------------------------------

;; a(n) defined, for n?
(defun has-sequence-element-p (interpreter index)
  "Determines whether the INDEX corresponds to an valid sequence element
   in the INTERPRETER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (with-slots (sequence) interpreter
    (declare (type numeric-sequence sequence))
    (the boolean
      (not (null
        (< -1 index (fill-pointer sequence)))))))

;;; -------------------------------------------------------

;; a(n)
(defun get-sequence-element (interpreter index)
  "Returns the sequence element at the INDEX as maintained by the
   INTERPRETER, or signals an error of an unspecified type upon the
   INDEX' disqualification."
  (declare (type Interpreter interpreter))
  (declare (type integer     index))
  (with-slots (sequence) interpreter
    (declare (type numeric-sequence sequence))
    (the integer
      (cond
        ;; Sequence term a(index) exists?
        ((has-sequence-element-p interpreter index)
          (aref sequence index))
        ;; Sequence term a(0) is requested, but does not exist?
        ;; => Return the default value of a(0) = 0, according to the
        ;;    Osis language specification.
        ((and (zerop index) (zerop (length sequence)))
          0)
        (T
          (error "No sequence term a(~d) is defined for the index ~d."
            index index))))))

;;; -------------------------------------------------------

(defun memorize-term (interpreter new-element)
  "Appends the NEW-ELEMENT to the INTERPRETER's number sequence and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-element))
  (vector-push-extend new-element
    (slot-value interpreter 'sequence))
  (values))

;;; -------------------------------------------------------

(defun push-to-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-value))
  (push new-value
    (slot-value interpreter 'stack))
  (values))

;;; -------------------------------------------------------

(defun pop-from-stack (interpreter)
  "Removes and returns the INTERPRETER stack's top element."
  (declare (type Interpreter interpreter))
  (the integer
    (or (pop (slot-value interpreter 'stack))
        (error 'Empty-Stack-Error))))

;;; -------------------------------------------------------

(defun stack-empty-p (interpreter)
  "Determines whether the INTERPRETER's stack is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (null (slot-value interpreter 'stack))))

;;; -------------------------------------------------------

(defun ensure-prime-sequence-length (interpreter position)
  "Ensures that the prime number sequence governed by the INTERPRETER
   accommodates at least a tally of primes equal to the zero-based
   POSITION, and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type index       position))
  (with-slots (prime-generator prime-sequence) interpreter
    (declare (type prime-generator            prime-generator))
    (declare (ignorable                       prime-generator))
    (declare (type (vector prime-candidate *) prime-sequence))
    (loop while (< (fill-pointer prime-sequence) (1+ position)) do
      (vector-push-extend
        (funcall prime-generator)
        prime-sequence)))
  (values))

;;; -------------------------------------------------------

(defun get-prime (interpreter position)
  "Returns the POSITION-th prime number utilizing the INTERPRETER's
   internally managed prime sequence."
  (declare (type Interpreter interpreter))
  (declare (type index       position))
  (ensure-prime-sequence-length interpreter position)
  (with-slots (prime-sequence) interpreter
    (declare (type (vector prime-candidate *) prime-sequence))
    (the prime-candidate
      (aref prime-sequence position))))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-command-processor (command (interpreter-variable)
                                    &body body)
  "Defines an implementation of the generic function ``process-command''
   installing as its first formal parameter the INTERPRETER-VARIABLE's
   designation, as its second an automatically generated and ignored
   symbol, dispatching by ``eql''-equality on the evaluated
   COMMAND-TYPE, and injecting the BODY forms into the method
   definition, which itself returns no value."
  (let ((command-variable (gensym)))
    (declare (type symbol command-variable))
    `(defmethod process-command ((,interpreter-variable Interpreter)
                                 (,command-variable     (eql ,command)))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type command     ,command-variable))
       (declare (ignore           ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

;; _
(define-command-processor :negation (interpreter)
  "Pops the top element from the INTERPRETER's stack, negates its sign,
   pushes the new value unto the stack, and returns no value."
  (push-to-stack interpreter
    (- (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(define-command-processor :double (interpreter)
  "Pops the top element from the INTERPRETER's stack, multiplies it by
   two (2), pushes the product unto the stack, and returns no value."
  (push-to-stack interpreter
    (double-number
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

(define-command-processor :halve (interpreter)
  "Pops the top element from the INTERPRETER's stack, halves its value,
   rounds it, pushes the result unto the stack, and returns no value."
  (push-to-stack interpreter
    (halve
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; $
(define-command-processor :square (interpreter)
  "Pops the top element from the INTERPRETER's stack, squares it, pushes
   the product unto the stack, and returns no value."
  (push-to-stack interpreter
    (square (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; !
(define-command-processor :factorial (interpreter)
  "Pops the top element from the INTERPRETER's stack, calculates the
   factorial for the same, pushes the result unto the stack, and returns
   no value."
  (push-to-stack interpreter
    (factorial
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; ,
(define-command-processor :prime (interpreter)
  "Pops the top element \"t\" from the INTERPRETER's stack, construes it
   as an index, calculates the t-th prime number, pushes the same unto
   the stack, and returns no value."
  (push-to-stack interpreter
    (get-prime interpreter
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; +
(define-command-processor :addition (interpreter)
  (let ((addend (pop-from-stack interpreter))
        (augend (pop-from-stack interpreter)))
    (declare (type integer addend))
    (declare (type integer augend))
    (push-to-stack interpreter
      (+ augend addend))))

;;; -------------------------------------------------------

;; -
(define-command-processor :subtraction (interpreter)
  (let ((subtrahend (pop-from-stack interpreter))
        (minuend    (pop-from-stack interpreter)))
    (declare (type integer subtrahend))
    (declare (type integer minuend))
    (push-to-stack interpreter
      (- minuend subtrahend))))

;;; -------------------------------------------------------

;; *
(define-command-processor :multiplication (interpreter)
  (push-to-stack interpreter
    (* (pop-from-stack interpreter)
       (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; /
(define-command-processor :integer-division (interpreter)
  (let ((divisor  (pop-from-stack interpreter))
        (dividend (pop-from-stack interpreter)))
    (declare (type integer divisor))
    (declare (type integer dividend))
    (push-to-stack interpreter
      (round dividend divisor))))

;;; -------------------------------------------------------

;; %
(define-command-processor :modulo (interpreter)
  (let ((divisor  (pop-from-stack interpreter))
        (dividend (pop-from-stack interpreter)))
    (declare (type integer divisor))
    (declare (type integer dividend))
    (push-to-stack interpreter
      (mod dividend divisor))))

;;; -------------------------------------------------------

;; ^
(define-command-processor :power (interpreter)
  (let ((exponent (pop-from-stack interpreter))
        (base     (pop-from-stack interpreter)))
    (declare (type integer exponent))
    (declare (type integer base))
    (push-to-stack interpreter
      (expt base exponent))))

;;; -------------------------------------------------------

(define-command-processor :push-0 (interpreter)
  "Pushes the integer number zero (0) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 0))

;;; -------------------------------------------------------

(define-command-processor :push-1 (interpreter)
  "Pushes the integer number one (1) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 1))

;;; -------------------------------------------------------

(define-command-processor :push-2 (interpreter)
  "Pushes the integer number two (2) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 2))

;;; -------------------------------------------------------

(define-command-processor :push-3 (interpreter)
  "Pushes the integer number three (3) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 3))

;;; -------------------------------------------------------

(define-command-processor :push-4 (interpreter)
  "Pushes the integer number four (4) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 4))

;;; -------------------------------------------------------

(define-command-processor :push-5 (interpreter)
  "Pushes the integer number five (5) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 5))

;;; -------------------------------------------------------

(define-command-processor :push-6 (interpreter)
  "Pushes the integer number six (6) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 6))

;;; -------------------------------------------------------

(define-command-processor :push-7 (interpreter)
  "Pushes the integer number seven (7) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 7))

;;; -------------------------------------------------------

(define-command-processor :push-8 (interpreter)
  "Pushes the integer number eight (8) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 8))

;;; -------------------------------------------------------

(define-command-processor :push-9 (interpreter)
  "Pushes the integer number nine (9) unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter 9))

;;; -------------------------------------------------------

;; ;
(define-command-processor :push-a[n-t] (interpreter)
  "Pops the top element \"t\" from the INTERPRETER's stack, queries the
   sequence element a(n-t), where n designates the current sequence
   index, pushes a(n-t) unto the stack, and returns no value."
  (let ((offset (pop-from-stack interpreter)))
    (declare (type integer offset))
    (push-to-stack interpreter
      (get-sequence-element interpreter
        (- (slot-value interpreter 'index) offset)))))

;;; -------------------------------------------------------

;; {
(define-command-processor :push-a[n-1] (interpreter)
  "Queries the prevenient cycle's sequence term, designated by a(n-1),
   pushes it unto the INTERPRETER's stack, and returns no value."
  (push-to-stack interpreter
    (get-sequence-element interpreter
      (1- (slot-value interpreter 'index)))))

;;; -------------------------------------------------------

;; }
(define-command-processor :push-a[n-2] (interpreter)
  "Queries the sequence element a(n-2), where n represents the current
   sequence index, pushes a(n-2) unto the INTERPRETER's stack, and
   returns no value."
  (push-to-stack interpreter
    (get-sequence-element interpreter
      (- (slot-value interpreter 'index) 2))))

;;; -------------------------------------------------------

;; (
(define-command-processor :push-a[n-3] (interpreter)
  "Queries the sequence element a(n-3), where n represents the current
   sequence index, pushes a(n-3) unto the INTERPRETER's stack, and
   returns no value."
  (push-to-stack interpreter
    (get-sequence-element interpreter
      (- (slot-value interpreter 'index) 3))))

;;; -------------------------------------------------------

;; :
(define-command-processor :push-a[n] (interpreter)
  "Pops the top stack element \"t\", employs it as an index in order to
   query the t-th sequence element a(t), pushes a(t) unto the stack, and
   returns no value."
  (push-to-stack interpreter
    (get-sequence-element interpreter
      (pop-from-stack interpreter))))

;;; -------------------------------------------------------

;; `
(define-command-processor :push-index (interpreter)
  "Pushes the current sequence index unto the INTERPRETER's stack and
   returns no value."
  (push-to-stack interpreter
    (slot-value interpreter 'index)))

;;; -------------------------------------------------------

(defun notify-cycle-handler (interpreter)
  "Apprizes the INTERPRETER's cycle handler, if such is registered,
   about the current iteration cycle's patration and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (cycle-handler) interpreter
    (declare (type (or null cycle-handler) cycle-handler))
    (when cycle-handler
      (funcall cycle-handler interpreter)))
  (values))

;;; -------------------------------------------------------

(defun perform-cycle (interpreter)
  "Executes a single cycle by avail of the INTERPRETER's context and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program index) interpreter
    (declare (type Osis-Program program))
    (declare (type index        index))
    
    ;; If no base case is defined for this INDEX, utilize the formula.
    (unless (has-sequence-element-p interpreter index)
      ;; Apply the formula.
      (dolist (command (osis-program-formula program))
        (declare (type command command))
        (process-command interpreter command))
      
      ;; If a formula exists, pop its result and append it to the
      ;; sequence; otherwise simply insert the default value of zero
      ;; (0).
      (memorize-term interpreter
        (if (stack-empty-p interpreter)
          0
          (pop-from-stack interpreter))))
    
    (notify-cycle-handler interpreter)
    (incf index))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates the Osis program consigned to the INTERPRETER's castaldy
   and, if finite in its perpetuation principle, returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (index end-index) interpreter
    (declare (type index           index))
    (declare (type (or null index) end-index))
    (symbol-macrolet
        ((has-next-cycle-p
          (the boolean
            (not (null
              (or (null end-index)
                  (<= index end-index)))))))
      (declare (type boolean has-next-cycle-p))
      (loop while has-next-cycle-p do
        (perform-cycle interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Osis (code
                       &key (start-index   0)
                            (end-index     NIL)
                            (cycle-handler NIL))
  "Interprets the piece of Osis source CODE the iteration commencing
   with the START-INDEX and terminating in the END-INDEX, if such is
   non-``NIL'', the latter case of which capacitates an infinite
   repetition, upon each cycle's patration, notifying the optional
   CYCLE-HANDLER about the progress, the operation, if limited in its
   extent, returning no value."
  (declare (type string                  code))
  (declare (type index                   start-index))
  (declare (type (or null index)         end-index))
  (declare (type (or null cycle-handler) cycle-handler))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))
      :start-index   start-index
      :end-index     end-index
      :cycle-handler cycle-handler))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of cycle handlers.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-printing-cycle-handler (delay)
  "Creates and returns a cycle handler callback function which outputs
   its input INTERPRETER's current cycle index and sequence vector,
   rests for the specified DELAY number of seconds, and returns no
   value."
  (declare (type (real 0 *) delay))
  (the function
    #'(lambda (interpreter)
        (declare (type Interpreter interpreter))
        (format T "~&n = ~d; a = ~s"
          (get-current-index interpreter)
          (get-sequence      interpreter))
        (finish-output)
        (sleep delay)
        (values))))

;;; -------------------------------------------------------

(declaim (type cycle-handler +DEFAULT-CYCLE-HANDLER+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CYCLE-HANDLER+
  (make-printing-cycle-handler 0.1)
  "The recommended cycle handler for an Osis program's execution.")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the Fibonacci sequence.
(interpret-Osis "{}+ 1 0"
  :cycle-handler +DEFAULT-CYCLE-HANDLER+)

;;; -------------------------------------------------------

;; Generate the factorials n! for 0 <= n <= +infinity.
(interpret-Osis "`{* 1"
  :cycle-handler +DEFAULT-CYCLE-HANDLER+)

;;; -------------------------------------------------------

;; Print the powers of two, that is:
;;   (1, 2, 4, 8, 16, 32, 64, ...)
(interpret-Osis "{\" 1"
  :cycle-handler +DEFAULT-CYCLE-HANDLER+)

;;; -------------------------------------------------------

;; Period [1,2]: Repeatedly print the numbers 1 and 2 in alternation:
;;   (1, 2, 1, 2, 1, 2, ...)
(interpret-Osis "`2%1+"
  :cycle-handler +DEFAULT-CYCLE-HANDLER+)

;;; -------------------------------------------------------

;; Period [1,3]: Repeatedly print the numbers 1 and 3 in alternation:
;;   (1, 3, 1, 3, 1, 3, ...)
(interpret-Osis "21_`^-"
  :cycle-handler +DEFAULT-CYCLE-HANDLER+)

;;; -------------------------------------------------------

;; Generate the sequence of the first ten prime numbers:
;;   (2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
(interpret-Osis "`,"
  :end-index     9
  :cycle-handler (make-printing-cycle-handler 0.0))
