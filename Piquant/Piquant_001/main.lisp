;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Piquant", presented by the Esolang user "Misterblue28" in
;; the year 2020, and founded upon an implicit iteration which tests a
;; series of switch-case statements, compact of a condition and zero or
;; more actions, the first match of which is executed, until the program
;; terminates with no eligibility ascertained.
;; 
;; 
;; Concept
;; =======
;; The Piquant programming language is founded upon an implicit loop
;; that tests a specified series of switch-case statements, each
;; delineated by a condition and a series of actions, executing the
;; first successfully confirmed candidate's body, or terminating the
;; program if none is present.
;; 
;; == PIQUANT: A NAME AS PRAGMATISM ==
;; The agnomination (P)(I)(Q)U(A)NT serves in a pragmatic fashion in
;; allusion to its four most conspicuous command tokens, "p", "i", "q",
;; and "A".
;; 
;; == PIQUANT PROGRAMS: A TWO-STAGE PROCESS ==
;; A dichotomy into two stages governs the programs:
;; 
;;  (A) MEMORY INITIALIZATION
;;      The incipient section is dedicated to the initialization of the
;;      program memory, executed once, thus preceding the repeating case
;;      tests.
;;  (B) CASE SELECTION
;;      Succeeding the initialization, the zero or more switch cases are
;;      tested, either executing the first eligible candidate, or, if
;;      none matches, immediately terminating the program.
;; 
;; == (A) MEMORY INITIALIZATION: POPULATING THE MEMORY ONCE ==
;; The initalization step, exercised once at the program's inchoation,
;; populates the memory's foremost cells.
;; 
;; The elements of this compartment are embraced in a jumelle of
;; brackets, "[" and "]", with zero or more comma-separated expressions
;; that are assigned to the consecutive memory cells starting with the
;; first index zero (0).
;; 
;; Given an initialization section conformant with the design
;; 
;;   [ initValue_1, ..., initValue_j, ..., initValue_N ]
;; 
;; and the program memory with the cells
;; 
;;   memory[i], with i >= 0
;; 
;; the following process, formulated in pseudocode, applies:
;; 
;;   for cellIndex from 0 to (N - 1) do
;;     memory[cellIndex] <- initValue[cellIndex + 1]
;;   end for
;; 
;; == (B) CASE SELECTION: REPEATEDLY TEST AND EXECUTE ==
;; Each case is ensconced in a twain of braces, "{" and "}", with its
;; body being a composition of one or more semicolon-separated
;; expressions.
;; 
;; The first expression provides the test criterion; the subsequent
;; constituents define the statements to execute upon the condition's
;; satisfaction, in the stated order.
;; 
;; An implicit loop iterates over all cases in their specification
;; ordonnance, testing each condition. The first satisfied case
;; predicate halts the probing, instigating the case actions' execution,
;; ere the cycle starts anew.
;; 
;; If at any given iteration run no matching case could be detected,
;; the program is terminated following the failed search.
;; 
;; An anatomy of the cases, inducing stringency in the specification,
;; shall now be adduced:
;; 
;; Given the rule set
;; 
;;   {condition[1]; actions[1][1]; ..., actions[1][i]; ...; actions[1][W]}
;;   {condition[2]; actions[2][1]; ..., actions[2][i]; ...; actions[2][X]}
;;   [...]
;;   {condition[i]; actions[i][1]; ..., actions[i][i]; ...; actions[i][Y]}
;;   [...]
;;   {condition[N]; actions[N][1]; ..., actions[N][i]; ...; actions[N][Z]}
;; 
;; which can be abbreviated by eliding the "actions" extension
;; concerning the subscripts to the more pleasing
;; 
;;   {condition[1]; actions[1]}
;;   {condition[2]; actions[2]}
;;   [...]
;;   {condition[i]; actions[i]}
;;   [...]
;;   {condition[N]; actions[N]}
;; 
;; the following pseudocode may be administered to explicate its
;; purpose:
;; 
;;   if condition[1] is satisfied then
;;     for each action in actions[1] do
;;       execute action
;;     end for
;;   
;;   else if condition[2] is satisfied then
;;     for each action in actions[2] do
;;       execute action
;;     end for
;;   
;;   [...]
;;   
;;   else if condition[i] is satisfied then
;;     for each action in actions[i] do
;;       execute action
;;     end for
;;   
;;   [...]
;;   
;;   else if condition[N] is satisfied then
;;     for each action in actions[N] do
;;       execute action
;;     end for
;;   
;;   else
;;     terminate program
;;   end if
;; 
;; An alternative formulation, invested with augmented compendiousness,
;; relies on the definition of "cases" as tuples of condition and
;; actions:
;; 
;;   case = (condition, actions)
;; 
;; Our less stringent definition maintains lealty to the Piquant syntax:
;; 
;;   case[1] := {condition[1]; actions[1]}
;;   case[2] := {condition[2]; actions[2]}
;;   [...]
;;   case[i] := {condition[i]; actions[i]}
;;   [...]
;;   case[N] := {condition[N]; actions[N]}
;; 
;; The ensuing pseudocode subsequently resolves to:
;; 
;;   let firstEligibleCase <- nil
;;   
;;   for each case in cases do
;;     if the case condition is satisfied then
;;       firstEligibleCase <- case
;;       terminate loop
;;     end if
;;   end for
;;   
;;   if firstEligibleCase is not nil then
;;     execute the actions of the firstEligibleCase
;;   else
;;     terminate the program
;;   end if
;; 
;; 
;; Architecture
;; ============
;; Piquant's data storage is described by a random-access memory whose
;; cells are amenable to non-negative integer indices, each such
;; amplecting a single signed integer datum.
;; 
;; == THE MEMORY: A VECTOR OF INTEGERS ==
;; The memory comprises a theoretically infinite tally of cells,
;; amenable to integer indices greater than or equal to zero, with no
;; upper bourne's imposition.
;; 
;; A cell stores a scalar signed integer of any magnitude, initially
;; assuming the default value of zero (0).
;; 
;; == ACCESS PROCEEDS BY SUBSCRIPTS ==
;; Memory cells are either addressed by mediation of a single subscript
;; or a range, the former variant manifests in the pattern
;; 
;;   Ax
;; 
;; for any non-negative integer position x; the range specifier, on the
;; other hand follows the forbisen
;; 
;;   Ax:y
;; 
;; for any twain of non-negative integer locations x and y, with x >= y,
;; where x designates the inclusive start index and y the inclusive end.
;; 
;; 
;; Data Types
;; ==========
;; Two species of objects contribute to a Piquant program's data
;; department: signed integers and ASCII characters, the former of which
;; are apportioned a paravaunt significance, whereas the latter issue
;; from a certain perspect on the numeric ilk's subset.
;; 
;; == INTEGERS: THE PROGRAM DATA'S FOUNDRY ==
;; The excellent echolon reserved for signed integers of any magnitude
;; is corroborated in their employment as the program memory cells'
;; contents. Additionally, all arithmetics, relational perquisition, and
;; the sole input facility, as well as the output's moiety, relays to
;; this category.
;; 
;; == CHARACTERS: CURRENCY ALONG THE COMMUNICATION SECTION ==
;; An assessment into the role of subordination occurs to the character
;; type, restricted to the ASCII standard's repertoire, and deployed
;; merely in the output of integers, either stated directly or extracted
;; from the memory cells, in the aspect of their ASCII code mappings.
;; 
;; 
;; Syntax
;; ======
;; Piquant's synactical design is founded upon expressions, arithmetic,
;; relational, and logical in their nature, and communicated by
;; adminiculum of infix operations. Two sections, delineated by their
;; bracketing symbols, subsume the program stages.
;; 
;; == INSTRUCTIONS ==
;; Two general categories of statements exist: the aefauld
;; initialization section, and the optional case blocks.
;; 
;; The initialization consists of an arbitrary tally of expressions
;; ensconced in a bracket pair "[" and "]", segregating any two items
;; via a single comma ",".
;; 
;; A case statement is lined by a jumelle of braces "{" and "}", with
;; one or more expressions, every twain separated by a semicolon (";"),
;; entailed in this body, the incipient element construed as the case
;; condition, the subsequent providing its actions.
;; 
;; == WHITESPACES ==
;; Whitespaces, encompassing in their diorism the space, tab, and
;; newline entities, do not bear any significant value. Their insertion
;; and absence are reckoned in the same mete.
;; 
;; == COMMENTS ==
;; Comments are incorporated in the language, instigated via the hash
;; sign "#" and extending to the end of the line.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation applies to
;; Piquant's syntax:
;; 
;;   program           := initialization , { caseStatement } ;
;;   
;;   initialization    := "["
;;                     ,  [ expression , { "," , expression } ]
;;                     ,  "]"
;;                     ;
;;   
;;   caseStatement     := "{" , condition , { ";" , action } , "}" ;
;;   condition         := logicalExpression ;
;;   action            := statement ;
;;   
;;   statement         := input
;;                     |  printNumber
;;                     |  printCharacter
;;                     |  expression
;;                     ;
;;   
;;   expression        := integer
;;                     |  memoryReference
;;                     |  arithmeticExpr
;;                     |  logicalExpression
;;                     ;
;;   
;;   logicalExpression := expression , relation        , expression
;;                     |  expression , logicalOperator , expression
;;                     ;
;;   arithmeticExpr    := expression , binaryOperator  , expression ;
;;   
;;   binaryOperator    := "+" | "-" | "*" | "/" | "%" ;
;;   relation          := "==" | "!=" | "<" | "<=" | ">" | ">=" ;
;;   logicalOperator   := "&&" | "||" ;
;;   
;;   input             := "i" , memoryReference ;
;;   printNumber       := "p" , expression ;
;;   printCharacter    := "q" , expression ;
;;   
;;   memoryReference   := scalarReference
;;                     |  rangeReference
;;                     |  indirectReference
;;                     ;
;;   indirectReference := "A" , ( indirectReference
;;                              | scalarReference
;;                              | rangeReference
;;                              )
;;                     ;
;;   scalarReference   := "A" , index ;
;;   rangeReference    := "A" , index , ":" , index ;
;;   
;;   index             := digit , { digit } ;
;;   integer           := optionalSign , digit , { digit } ;
;;   optionalSign      := [ "+" | "-" ] ;
;;   digit             := "0" | "1" | "2" | "3" | "4"
;;                     |  "5" | "6" | "7" | "8" | "9"
;;                     ;
;; 
;; 
;; Instructions
;; ============
;; Piquant's instruction set amplects unary and binary arithmetics,
;; relational and logical operations, as well as input and output
;; facilities.
;; 
;; == OVERVIEW ==
;; Piquant's instruction set differentiates into a ramose structure,
;; encompassing
;; 
;;   - Unary and binary arithmetic operations
;;   - Relational operations
;;   - Logical operations
;;   - Input and output facilities
;; 
;; == BINARY ARITHMETIC OPERATIONS ==
;; Entrusted with the provision of basic arithmetics, Piquant programs
;; may operate on integer literals, single memory cells, and ranges in
;; their pursuit of addition, subtraction, multiplication, division, and
;; remainder supputation.
;; 
;; All binary operations, including this arithmetic subspecies, conform
;; to the infix pattern
;; 
;;   <leftOperand> <operator> <rightOperand>
;; 
;; With both the <leftOperand> and the <rightOperand> expected to either
;; be stated in literal integers, references, or expressions that
;; resolves to equivalent values.
;; 
;; A tabular exposition is entrusted with the wike of explicating the
;; available operators:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Effect
;;   ---------+--------------------------------------------------------
;;   +        | Adds the right operand to the left one and returns the
;;            | sum.
;;   ..................................................................
;;   -        | Substracts the right operand from the left one and
;;            | returns the difference.
;;   ..................................................................
;;   *        | Multiplies the left operand by the right one and
;;            | returns the product.
;;   ..................................................................
;;   /        | Divides the left operand by the right one and returns
;;            | the quotient.
;;   ..................................................................
;;   %        | Divides the left operand by the right one and returns
;;            | the remainder.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONAL OPERATORS ==
;; Relational operators establish a special ilk of the binary species,
;; their commorancy being the junction of the arithmetic and logical
;; vales.
;; 
;; The following table provides an apercu concerning the rcognized
;; relations:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Effect
;;   ---------+--------------------------------------------------------
;;   ==       | True if the left operand equals the right operand,
;;            | otherwise false.
;;   ..................................................................
;;   !=       | True if the left operand does not equal the right
;;            | operand, otherwise false.
;;   ..................................................................
;;   <        | True if the left operand is strictly less than the
;;            | right operand, otherwise false.
;;   ..................................................................
;;   <=       | True if the left operand is less than or equal to the
;;            | right operand, otherwise false.
;;   ..................................................................
;;   >        | True if the left operand is strictly greater than the
;;            | right operand, otherwise false.
;;   ..................................................................
;;   >=       | True if the left operand is greater than or equal to
;;            | the right operand, otherwise false.
;;   ------------------------------------------------------------------
;; 
;; == LOGICAL OPERATIONS ==
;; Piquant's conditions may partake of complex interactions by
;; adminiculum of a logical combinator's twain, the AND ("&&")
;; conjunction and the OR ("||") disjunction.
;; 
;; Both operations belong to the binary species, designed in an infix
;; mode, and as such following the forbisen
;; 
;;   <leftOperand> <logicalOperation> <rightOperand>
;; 
;; These operands, the <leftOperand> and the <rightOperand>, are
;; restricted to be Boolean truth values, that is, either false or true,
;; producing upon their coeffiency a third member of this set.
;; 
;; The tabular exposition below shall treat of the pair in a cursor
;; manner:
;; 
;;   ------------------------------------------------------------------
;;   Operator | Effect
;;   ---------+--------------------------------------------------------
;;   &&       | True if both the left and the right operand are true;
;;            | false if any or both of these are false.
;;            |--------------------------------------------------------
;;            | The implementation might choose a shortcut realization
;;            | of this operator, that is, if the left operand is
;;            | already false, the operation's evaluation ceases with
;;            | immediacy, eliding the right operator's processing and
;;            | thus its contribution, as this second participant
;;            | cannot modify the compound expression in any way to
;;            | prevent the resolution to a Boolean false.
;;   ..................................................................
;;   ||       | True if either the left operand, the right operand, or
;;            | both are true; false if both are false.
;;            |--------------------------------------------------------
;;            | The implementation might choose a shortcut realization
;;            | of this operator, that is, if the left operand is
;;            | already true, the operation's evaluation ceases with
;;            | immediacy, eliding the right operator's processing and
;;            | thus its contribution, as this second participant
;;            | cannot modify the compound expression in any way to
;;            | prevent the resolution to a Boolean true.
;;   ------------------------------------------------------------------
;; 
;; == INPUT AND OUTPUT ==
;; The input and output reception capacitates the programmer to employ
;; signed integers in user requests and the same number type, enhanced
;; with characters from the ASCII repertoire, for issuing outputs.
;; 
;; A table shall educate about the operations across the input/output
;; conduits. Please note that the variable portions, signifying the
;; arguments, are underlined with asterisks ("*") in order to
;; distinguish them from the fixed keywords, and further segregated by
;; spaces, which are not mandated by the language standard.
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   i cell     | Queries the user for a signed integer input and
;;     ****     | stores the same in the referenced {cell}.
;;              | The {cell} may be a single cell reference or a cell
;;              | range, the value of the targeted cell or cells shall
;;              | be replaced by the input.
;;   ..................................................................
;;   p argument | Prints the {argument} in its verbatim numeric form to
;;     ******** | the standard output.
;;              | The {argument} may be a signed integer number, a
;;              | single cell reference, as well as a cell range. In
;;              | the latter two cases, the cell value or values will
;;              | be retrieved and printed.
;;   ..................................................................
;;   q argument | Prints the character whose ASCII code equals the
;;     ******** | numeric {argument} to the standard output.
;;              | The {argument} may be an unsigned integer number
;;              | corresponding to an ASCII code, a single cell
;;              | reference, as well as a cell range. In the latter two
;;              | cases, the cell value or values will be construed as
;;              | ASCII code(s) and printed as the corresponding
;;              | character or character sequence.
;;   ------------------------------------------------------------------
;; 
;; == MEMORY REFERENCES ==
;; A variable component's introduction in the language proceeds from the
;; capacitation to employ memory cell references in lieu of literal
;; integers.
;; 
;; Piquant's memory model specifies an infinite random-access vector of
;; cells, any of which stores a signed scalar with no bournes along each
;; lateral extremity, the storage component being entalented with an
;; amenability to non-negative integer indices.
;; 
;; A treble account of modes for the memory access:
;; 
;;   ------------------------------------------------------------------
;;   Reference type | Description
;;   ---------------+--------------------------------------------------
;;   scalar         | Selects a single cell, designated by an aefauld
;;                  | integer subscript.
;;   ..................................................................
;;   range          | Selects a potentially empty, finite subsequence
;;                  | of continuous cells, demarcated by an inclusive
;;                  | start index and an inclusive end index.
;;   ..................................................................
;;   indirect       | References a second reference, either scalar, a
;;                  | range, or another indirect specimen. Effectively,
;;                  | the values of the cell or cells selected by the
;;                  | ensconced target reference are interpreted as
;;                  | cell indices themselves, and thus used to in the
;;                  | ultimate selection.
;;   ------------------------------------------------------------------
;; 
;; The memory access trichotomy is modeled in the language as follows:
;; 
;;   ------------------------------------------------------------------
;;   Reference     | Effect
;;   --------------+---------------------------------------------------
;;   A index       | Establishes a scalar reference, that is, a
;;     *****       | reference to exactly one cell, amenable to the
;;                 | non-negative integer {index}.
;;   ..................................................................
;;   A start : end | Establishes a range reference, that is, a
;;     *****   *** | reference to zero or more cells occupying the
;;                 | closed index range [{start}, {end}], both posts
;;                 | being construed as inclusive.
;;                 | It must hold:
;;                 |   {start} >= {end}
;;   ..................................................................
;;   A reference   | Establishes an indirect reference, that is, a
;;     *********   | reference that targets another reference. The cell
;;                 | values selected by the incorporated locator are
;;                 | thus interpreted as cell indices and employed in
;;                 | the actual access operation.
;;   ------------------------------------------------------------------
;; 
;; == INTEGER SCALARS AND VECTORS INCUR COMPLEXITY ==
;; The three exclusive operand types applicable in conjunction with all
;; unary and binary operations enlist
;; 
;;   - integers
;;   - vectors of integers
;;   - Boolean truth values
;; 
;; The desinent species, borrowed from Boolean algebra, partakes of a
;; rather narrow involvement, being nexible only in its unassuming
;; true-false dichotomy, maintaining no interactions with the remaining
;; sets. The integer scalar and vector types remain encumbered with
;; augmented peccability for a more peisant complexity's introduction,
;; as their combinations might very well apply in arithmetic and
;; relational circumstances.
;; 
;; Cognate with the always ensuing production of a Boolean truth value
;; from the conjoined activities related to integer scalars and/or
;; vectors in the relational context, arithmetic operations, while
;; generating either a single or a sequential response, are inflicted
;; with the same problems of achieving a compatibility betwixt the
;; conflicting cardinalities.
;; 
;; Ere adducing our treatise on the scalar-vector interoperation
;; principles, the following table describes the result type of an
;; arithmetic operation in dependence upon its operands:
;; 
;;   ------------------------------------------
;;   Left operand | Right Operand | Result type
;;   -------------+---------------+------------
;;   integer      | integer       | integer
;;   ..........................................
;;   integer      | vector        | vector
;;   ..........................................
;;   vector       | integer       | vector
;;   ..........................................
;;   vector       | vector        | vector
;;   ------------------------------------------
;; 
;; == HOW INTEGER SCALARS AND VECTORS BREED ==
;; Finally, an elucidation will be provided describing the treatment
;; catered to by binary operations with respect to the integer scalar
;; and integer vector application.
;; 
;; Of a systatic value, please note that the symbol
;; 
;;   |x|
;; 
;; for a vector x returns its cardinality or length, thus being
;; equivalent to these formulations in popular programming languages:
;; 
;;   ------------------------------------------------------------
;;   Programming language          | Function or property request
;;   ------------------------------+-----------------------------
;;   Java "Collection" method      | x.size()
;;   ............................................................
;;   Java array property           | x.length
;;   ............................................................
;;   Common Lisp sequence function | (length x)
;;   ------------------------------------------------------------
;; 
;; The general pattern applicable to all binary operations is presented
;; in the below enumeration, comprehending pseudocode formulations:
;; 
;;   Case 1: <leftOperand : integer> <operator> <rightOperand : integer>
;;   
;;     let resultScalar <- leftOperand operator rightOperand
;;     
;;     return resultScalar
;;   
;;   ------------------------------------------------------------------
;;  
;;   Case 2: <leftOperand : integer> <operator> <rightOperand : vector>
;;   
;;     let resultVector <- empty vector of size |rightOperand|
;;     
;;     for i from 1 to |rightOperand| do
;;       resultVector[i] <- leftOperand operator rightOperand[i]
;;     end for
;;     
;;     return resultVector
;;   
;;   ------------------------------------------------------------------
;;  
;;   Case 3: <leftOperand : vector> <operator> <rightOperand : integer>
;;   
;;     let resultVector <- empty vector of size |leftOperand|
;;     
;;     for i from 1 to |leftOperand| do
;;       resultVector[i] <- leftOperand[i] operator rightOperand
;;     end for
;;     
;;     return resultVector
;;   
;;   ------------------------------------------------------------------
;;  
;;   Case 4: <leftOperand : vector> <operator> <rightOperand : vector>
;;   
;;     let maximumLength <- maximum(|leftOperand|, |rightOperand|)
;;     let resultVector  <- empty vector of size maximumLength
;;     
;;     for i from 1 to maximumLength do
;;       let leftOperandValue  <- 0
;;       let rightOperandValue <- 0
;;       
;;       if i <= |leftOperand| then
;;         leftOperandValue <- leftOperand[i]
;;       else
;;         leftOperandValue <- 0
;;       end if
;;       
;;       if i <= |rightOperand| then
;;         rightOperandValue <- rightOperand[i]
;;       else
;;         rightOperandValue <- 0
;;       end if
;;       
;;       resultVector[i] <- leftOperandValue operator rightOperandValue
;;     end for
;;     
;;     return resultResult
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The mickleness of details commorant in the original specification
;; expels the preponderance of conceivable ambiguities; a subset of the
;; few remnants shall be enumerated in the coming sections.
;; 
;; == MAY THE INITIALIZATION SECTION REFERENCE MEMORY CELLS? ==
;; The solitary section at the program's inchoation, demarcated by a
;; jumelle of brackets, "[" and "]", serves in the first memory cells'
;; initialization. The protolog's examples express merely integer
;; literals for this purpose. It is not explicitly sanctioned, nor
;; interdicted, to insert references to memory cells.
;; 
;; It has been adjudged, in the face of the fact that all cells in their
;; pristine state contain the value zero (0), to homologate any
;; references.
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
;;   (1) A lexer generates from the Var=Bar program string a sequence of
;;       tokens.
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
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-06-07
;; 
;; Sources:
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
;;   [esolang2020Piquant]
;;   The Esolang contributors, "Piquant", 2020
;;   URL: "https://esolangs.org/wiki/Piquant"
;;   
;;   [grand1997javalangref]
;;   Mark Grand, "Java Language Reference", 2nd Edition July 1997,
;;               "Chapter 4.14 Order of Operations"
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
;;   [sedgewick2022operatorprecedence]
;;   Robert Sedgewick, Kevin Wayne,
;;     "Appendix A: Operator Precedence in Java", 2022
;;   URL: "https://introcs.cs.princeton.edu/java/11precedence/"
;;   Notes:
;;     - Operator precedence in Java.
;;   
;;   [stackoverflow2011q2811319]
;;   The Stack Overflow contributors, "What is the difference between
;;     >>> and >> operators in Java?", 2011
;;   URL: "https://stackoverflow.com/questions/2811319/
;;         difference-between-and"
;;   Notes:
;;     - Describes the ">>>" operator in Java an unsigned right shift.
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, each indicator, or key, of which
   conforms to the INDICATOR-TYPE, affiliated with a value of the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype attribute-map ()
  "The ``attribute-map'' type defines an abstract syntax tree (AST)
   node's dictionary of attributes, associating keyword symbol names
   with arbitrary values in a hash table."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more abstract syntax
   tree (AST) nodes."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, without the claim of exhaustion, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the valid associativity
   options for an operator."
  '(member :left-to-right :right-to-left))

;;; -------------------------------------------------------

(deftype index ()
  "The ``index'' type defines a non-negative integer number greater than
   or equal to zero, utile as a memory cell index."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype piquant-object ()
  "The ``piquant-object'' type defines the valid species of data
   participating as tokens of currency in a Piquant program."
  '(or boolean integer Integer-Vector))

;;; -------------------------------------------------------

(deftype integer-function ()
  "The ``integer-function'' type defines a binary function on two input
   integers that responds with a third integer, all categorically signed
   in their nature.
   ---
   The signature thus conforms to:
   
     lambda (integer integer) => integer"
  '(function (integer integer) integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class represents a significant portion extracted from a
   piece of Piquant source code."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-token-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TOKEN-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (declare (type keyword expected-token-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-token-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of Piquant code to analyze.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The index into the SOURCE's current character.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class implements an entity responsible for the
     generation of tokens from a piece of Piquant source code."))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'', and
   ``character'' to eponymous symbol macros for general access, executes
   the BODY forms, and returns the desinent evaluated form's results.
   ---
   In addition to the LEXER slots, two local functions are defined as
   adminicles for a more efficient processing:
     
     advance ()
       Moves the LEXER's position cursor to the next character, if
       possible, updates the current character, and returns no value.
     
     move-to (new-position)
       Relocates the LEXER's position cursor to the NEW-POSITION,
       updates the current character, and returns no value."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (with-slots (source position character) ,evaluated-lexer
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         (flet ((advance ()
                  "Moves the LEXER's position cursor to the next, if
                   possible, updates the current character, and returns
                   no value."
                  (setf character
                    (when (array-in-bounds-p source (1+ position))
                      (char source (incf position))))
                  (values))
                (move-to (new-position)
                  "Relocates the LEXER's position cursor to the
                   NEW-POSITION, updates the current character, and
                   returns no value."
                  (setf position new-position)
                  (setf character
                    (when (array-in-bounds-p source position)
                      (char source position)))
                  (values)))
          ,@body)))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which analyzes the SOURCE."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-read-character (lexer token-type)
  "Consumes the LEXER's current character and returns a token
   representation thereof, with its type tantamount to the TOKEN-TYPE
   and the value being the character itself, advancing to the next
   position in the LEXER's source while doing so."
  (declare (type Lexer lexer))
  (declare (type keyword token-type))
  (with-lexer (lexer)
    (the Token
      (prog1
        (make-token token-type character)
        (advance)))))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the current position into the LEXER's source, skips a
   sequence of zero or more adjacent spaces and returns the modified
   LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (whitespace-character-p character)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position into the LEXER's source, reads an
   unsigned decimal integer and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :integer
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (advance))))))))

;;; -------------------------------------------------------

(defun lexer-string-follows-p (lexer expected-string)
  "Checks whether the characters starting at the current position into
   the LEXER's source match the EXPECTED-STRING, on confirmation
   relocating the LEXER's position cursor to first index following the
   congruent portion and returning a ``boolean'' value of ``T'',
   otherwise returning the cursor to the index assumed immediately
   before the invocation of this function, while returning ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-string))
  (with-lexer (lexer)
    (let ((return-position position))
      (declare (type fixnum return-position))
      (the boolean
        (loop
          for expected-character
            of-type character
            across  expected-string
          do
            (cond
              ((null character)
                (move-to return-position)
                (return NIL))
              ((char/= character expected-character)
                (move-to return-position)
                (return NIL))
              (T
                (advance)))
          finally
            (return T))))))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the current position into the LEXER's source, skips the
   comment characters until a linebreak or the end of the source has
   been encountered, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop until (or (null character) (char= character #\Newline)) do
      (advance)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((char= character #\#)
          (lexer-skip-comment lexer))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token lexer))
        
        ((char= character #\[)
          (lexer-read-character lexer :left-bracket))
        
        ((char= character #\])
          (lexer-read-character lexer :right-bracket))
        
        ((char= character #\{)
          (lexer-read-character lexer :left-brace))
        
        ((char= character #\})
          (lexer-read-character lexer :right-brace))
        
        ((char= character #\,)
          (lexer-read-character lexer :comma))
        
        ((char= character #\;)
          (lexer-read-character lexer :semicolon))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((char= character #\A)
          (lexer-read-character lexer :reference))
        
        ((char= character #\:)
          (lexer-read-character lexer :colon))
        
        ((char= character #\i)
          (lexer-read-character lexer :input))
        
        ((char= character #\p)
          (lexer-read-character lexer :print-number))
        
        ((char= character #\q)
          (lexer-read-character lexer :print-character))
        
        ((char= character #\+)
          (lexer-read-character lexer :plus))
        
        ((char= character #\-)
          (lexer-read-character lexer :minus))
        
        ((char= character #\*)
          (lexer-read-character lexer :multiplication))
        
        ((char= character #\/)
          (lexer-read-character lexer :division))
        
        ((char= character #\%)
          (lexer-read-character lexer :remainder))
        
        ((lexer-string-follows-p lexer "==")
          (make-token :equal-to "=="))
        
        ((char= character #\=)
          (lexer-read-character lexer :assignment))
        
        ((lexer-string-follows-p lexer "!=")
          (make-token :not-equal-to "!="))
        
        ((lexer-string-follows-p lexer ">=")
          (make-token :greater-or-equal ">="))
        
        ((char= character #\>)
          (lexer-read-character lexer :greater-than))
        
        ((lexer-string-follows-p lexer "<=")
          (make-token :less-or-equal "<="))
        
        ((char= character #\<)
          (lexer-read-character lexer :less-than))
        
        ((lexer-string-follows-p lexer "&&")
          (make-token :logical-and "&&"))
        
        ((lexer-string-follows-p lexer "||")
          (make-token :logical-or "||"))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class represents a node or subtree for the use in an
   abstract syntax tree (AST), yielded by parsing a piece of Piquant
   source code."
  (type       (error "Missing node type.") :type keyword)
  (attributes (make-hash-table :test #'eq) :type attribute-map))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' of the specified TYPE, optionally
   initialized with the INITIAL-ATTRIBUTES, supplied as a property list,
   or plist, whose keys constitute the attribute names, allied with the
   immediately succeeding attribute values."
  (declare (type keyword                      type))
  (declare (type (property-list-of keyword T) initial-attributes))
  (let ((node (initialize-node type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes node))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value of the attribute registered with the ATTRIBUTE-NAME
   at the NODE, or signals an error of an unspecified type upon its
   absence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-name)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-name))
    (the T
      (if contains-name
        attribute-value
        (error "Invalid attribute name \"~a\" for node ~s."
          attribute-name node)))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "Node(type=~s" (node-type node))
  (maphash
    #'(lambda (attribute-name attribute-value)
        (declare (type keyword attribute-name))
        (declare (type T       attribute-value))
        (format stream ", ~a=~s" attribute-name attribute-value))
    (node-attributes node))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (* * *) *)            std-parselet-invoke))
(declaim (ftype (function (Token) boolean)      std-token-p))
(declaim (ftype (function (Token) Std-Parselet) std))
(declaim (ftype (function (Token) boolean)      nud-token-p))
(declaim (ftype (function (Parser integer) Node)
                parser-parse-expression))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The lexer responsible for the token purveyance.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently acquired LEXER token."))
  (:documentation
    "The ``Parser'' class assembles an abstract syntax tree (AST) from
     a lexer's token stream."))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous symbol macros for general access, executes the BODY's
   forms, and returns the last evaluated form's results.
   ---
   Additionally to the symbol macros, two local functions are provided
   as adminicles for the token indagation and consumption:
     
     consume ()
       Returns the current token, while concomitantly querying the next
       one from the lexer and storing it in the returned token's stead.
     
     eat (expected-token-type)
       Checks whether the current token conforms to the
       EXPECTED-TOKEN-TYPE. Upon confirmation, the perquisited token is
       returned, while concomitantly the next one is loaded from the
       lexer and stored in its stead. Upon a mismatch, an error of an
       unspecified type is signaled."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (with-slots (lexer current-token) ,evaluated-parser
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         (labels
             ((consume ()
               "Returns the current token, while querying and storing
                the next one from the LEXER."
               (the Token
                 (prog1 current-token
                   (setf current-token
                     (lexer-get-next-token lexer)))))
              
              (eat (expected-token-type)
               "Checks whether the current token conforms to the
                EXPECTED-TOKEN-TYPE, on confirmation returning the just
                inspected token, while querying and storing the next one
                from the LEXER; otherwise an error of an unspecified
                type is signaled."
               (declare (type keyword expected-token-type))
               (the Token
                 (if (token-type-p current-token expected-token-type)
                   (consume)
                   (error "Expected a token of the type ~s, ~
                           but encountered the token ~s."
                     expected-token-type current-token)))))
           
           ,@body)))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-parser (parser)
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which assembles the tokens
   produced by the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-parse-integer (parser)
  "Expects the PARSER's current token to be constitute an integer and
   returns a node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Node
      (make-node :integer
        :value (token-value (eat :integer))))))

;;; -------------------------------------------------------

(defun parser-parse-initial-memory (parser)
  "Parses the initial memory configuration using the PARSER and returns
   a list of integer nodes representing the consumed integer objects."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat :left-bracket)
    (let ((initial-cells NIL))
      (declare (type node-list initial-cells))
      (flet ((collect-element ()
              "Parses an expression, inserts it at the INITIAL-CELLS
               list's front, and returns no value."
              (push (parser-parse-expression parser 0) initial-cells)
              (values)))
        
        (unless (token-type-p current-token :right-bracket)
          (collect-element)
          
          ;; Collect zero or more elements separated by commas.
          (loop while (token-type-p current-token :comma) do
            (eat :comma)
            (collect-element))))
      
      (eat :right-bracket)
      
      (the node-list
        (nreverse initial-cells)))))

;;; -------------------------------------------------------

(defun parser-parse-std (parser)
  "Expecting the PARSER's current token to constitute a statement
   denotation (std) token, invokes the respective parselet and returns
   a node representation of the extracted statement."
  (declare (type Parser parser))
  (with-parser (parser)
    (the Node
      (std-parselet-invoke
        (std current-token) parser (consume)))))

;;; -------------------------------------------------------

(defun parser-parse-actions (parser)
  "Employs the PARSER's tokens to parse zero or more actions and returns
   a list of node representations thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((actions NIL))
      (declare (type node-list actions))
      (loop while (token-type-p current-token :semicolon) do
        (eat :semicolon)
        (cond
          ((std-token-p current-token)
            (push (parser-parse-std parser) actions))
          ((nud-token-p current-token)
            (push (parser-parse-expression parser 0) actions))
          (T
            (error "Cannot parse the token ~s as an action."
              current-token))))
      (the node-list (nreverse actions)))))

;;; -------------------------------------------------------

(defun parser-parse-case (parser)
  "Employs the PARSER's tokens to parse a case statement and returns a
   ``:case'' node representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (eat :left-brace)
    (the Node
      (prog1
        (make-node :case
          :condition (parser-parse-expression parser 0)
          :actions   (parser-parse-actions    parser))
        (eat :right-brace)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Precedence".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Precedence
  (:constructor make-precedence (binding-power associativity)))
  "The ``Precedence'' class encapsulates the binding power and
   associativity information requisite to operator tokens partaking in
   Pratt's parser concept."
  (binding-power 0              :type integer)
  (associativity :left-to-right :type associativity))

;;; -------------------------------------------------------

(defun precedence-effective-binding-power (precedence)
  "Returns the PRECEDENCE's effective binding power, which is defined in
   terms of the nominal binding power contingently reduced in order to
   accommodate the associativity."
  (declare (type Precedence precedence))
  (the integer
    (case (precedence-associativity precedence)
      (:left-to-right
        (precedence-binding-power precedence))
      (:right-to-left
        (1- (precedence-binding-power precedence)))
      (otherwise
        (error "Invalid associativity: ~s."
          (precedence-associativity precedence))))))

;;; -------------------------------------------------------

(defun parse-with-precedence (parser precedence)
  "Parses an expression following the Pratt concept by operating on the
   PARSER's tokens in conjunction with the specified PRECEDENCE.
   ---
   The PRECENDE comprehends the current binding power and associativity
   information, the latter of which avails in determining the actual
   binding power to apply."
  (declare (type Parser     parser))
  (declare (type Precedence precedence))
  (the Node
    (parser-parse-expression parser
      (precedence-effective-binding-power precedence))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Parselet".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parselet ()
  ()
  (:documentation
    "The ``Parselet'' interface defines code generators used in the
     conversion from tokens to abstract syntax tree (AST) nodes."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Nud-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nud-Parselet (Parselet)
  ()
  (:documentation
    "The ``Nud-Parselet'' interface describes a null denotation (nud)
     parselet, a code generator for tokens representing terminal
     operands or prefix operators."))

;;; -------------------------------------------------------

(defgeneric nud-parselet-invoke (nud-parselet parser token)
  (:documentation
    "Applies the nud PARSELET to the PARSER and converts the nud TOKEN
     into an abstract syntax tree (AST) node, which is consequently
     returned."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Integer-Parselet".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Integer-Parselet (Nud-Parselet)
  ()
  (:documentation
    "The ``Integer-Parselet'' class models a code generator for
     converting integer tokens into abstract syntax tree (AST) nodes."))

;;; -------------------------------------------------------

(defun make-integer-parselet ()
  "Creates and returns a new ``Integer-Parselet''."
  (the Integer-Parselet
    (make-instance 'Integer-Parselet)))

;;; -------------------------------------------------------

(defmethod nud-parselet-invoke ((parselet Integer-Parselet)
                                (parser   Parser)
                                (token    Token))
  (declare (type Integer-Parselet parselet))
  (declare (type Parser           parser))
  (declare (type Token            token))
  (declare (ignore                parselet))
  (with-parser (parser)
    (the Node
      (make-node :integer
        :value (token-value token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Unary-Operator-Parselet".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Unary-Operator-Parselet (Nud-Parselet)
  ((operator
    :initarg       :operator
    :initform      (error "Missing operator.")
    :type          keyword
    :documentation "The prefix operator used to indicate the
                    ``:operator'' attribute.")
   (precedence
    :initarg       :precedence
    :initform      (error "Missing precedence.")
    :type          Precedence
    :documentation "The unary prefix operator's precedence
                    information."))
  (:documentation
    "The ``Unary-Operator-Parselet'' class furnishes a code generator
     which transforms prefix operators tokens into the respective
     abstract syntax tree (AST) nodes."))

;;; -------------------------------------------------------

(defun make-unary-operator-parselet (operator precedence)
  "Creates and returns a new ``Unary-Operator-Parselet'', which binds
   operands with the specified PRECEDENCE, and produces abstract syntax
   tree (AST) nodes whose ``:operator'' attribute name associates with
   the OPERATOR."
  (declare (type keyword    operator))
  (declare (type Precedence precedence))
  (the Unary-Operator-Parselet
    (make-instance 'Unary-Operator-Parselet
      :operator   operator
      :precedence precedence)))

;;; -------------------------------------------------------

(defmethod nud-parselet-invoke ((parselet Unary-Operator-Parselet)
                                (parser   Parser)
                                (token    Token))
  (declare (type Unary-Operator-Parselet parselet))
  (declare (type Parser                  parser))
  (declare (type Token                   token))
  (the Node
    (make-node :unary-operation
      :operator (slot-value parselet 'operator)
      :operand  (parse-with-precedence parser
                  (slot-value parselet 'precedence)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Reference-Parselet".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Reference-Parselet (Nud-Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing precedence.")
    :type          Precedence
    :documentation "The reference's precedence information."))
  (:documentation
    "The ``Reference-Parselet'' class furnishes a code generator which
     transforms memory reference tokens into the respective abstract
     syntax tree (AST) nodes."))

;;; -------------------------------------------------------

(defun make-reference-parselet ( precedence)
  "Creates and returns a new ``Reference-Parselet'', which binds
   operands with the specified PRECEDENCE."
  (declare (type Precedence precedence))
  (the Reference-Parselet
    (make-instance 'Reference-Parselet
      :precedence precedence)))

;;; -------------------------------------------------------

(defmethod nud-parselet-invoke ((parselet Reference-Parselet)
                                (parser   Parser)
                                (token    Token))
  (declare (type Reference-Parselet parselet))
  (declare (type Parser             parser))
  (declare (type Token              token))
  (the Node
    (make-node :reference
      :operand  (parse-with-precedence parser
                  (slot-value parselet 'precedence)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Led-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Led-Parselet (Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing led parselet precedence.")
    :type          Precedence
    :documentation "The led operator's precedence information."))
  (:documentation
    "The ``Led-Parselet'' interface describes a left denotation (led)
     parselet, a code generator for tokens representing infix or postfix
     operators."))

;;; -------------------------------------------------------

(defgeneric led-parselet-invoke (parselet parser left token)
  (:documentation
    "Applies the led PARSELET to the PARSER and the LEFT abstract syntax
     tree (AST) node and converts the led TOKEN into an AST node, which
     is consequently returned."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Binary-Operator-Parselet".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-Operator-Parselet (Led-Parselet)
  ()
  (:documentation
    "The ``Binary-Operator-Parselet'' class represents a code generator
     for a particular ilk of left denotation (led) tokens that resolve
     to infix operators."))

;;; -------------------------------------------------------

(defun make-binary-operator-parselet (precedence)
  "Creates and returns a ``Binary-Operator-Parselet'' which binds
   operands with the specified PRECEDENCE."
  (declare (type Precedence precedence))
  (the Binary-Operator-Parselet
    (make-instance 'Binary-Operator-Parselet :precedence precedence)))

;;; -------------------------------------------------------

(defmethod led-parselet-invoke ((parselet Binary-Operator-Parselet)
                                (parser   Parser)
                                (left     Node)
                                (token    Token))
  (declare (type Binary-Operator-Parselet parselet))
  (declare (type Parser                   parser))
  (declare (type Node                     left))
  (declare (type Token                    token))
  (the Node
    (make-node (token-type token)
      :left  left
      :right (parse-with-precedence parser
               (slot-value parselet 'precedence)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Range-Parselet".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Range-Parselet (Led-Parselet)
  ()
  (:documentation
    "The ``Binary-Operator-Parselet'' class represents a code generator
     for a particular ilk of left denotation (led) tokens that resolve
     to infix operators."))

;;; -------------------------------------------------------

(defun make-range-parselet (precedence)
  "Creates and returns a ``Range-Parselet'' which binds operands with
   the specified PRECEDENCE."
  (declare (type Precedence precedence))
  (the Range-Parselet
    (make-instance 'Range-Parselet :precedence precedence)))

;;; -------------------------------------------------------

(defmethod led-parselet-invoke ((parselet Range-Parselet)
                                (parser   Parser)
                                (left     Node)
                                (token    Token))
  (declare (type Range-Parselet parselet))
  (declare (type Parser         parser))
  (declare (type Node           left))
  (declare (type Token          token))
  (declare (ignore              token))
  
  (unless (eq (node-type left) :integer)
    (error "Cannot parse a range with the left node ~s." left))
  
  (with-parser (parser)
    (unless (token-type-p current-token :integer)
      (error "Expected the right expression to be an integer, ~
              but encountered ~a."
        current-token))
    
    (the Node
      (make-node :range
        :minimum left
        :maximum (make-node :integer
                   :value (token-value (eat :integer)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Std-Parselet".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Std-Parselet (Parselet)
  ()
  (:documentation
    "The ``Std-Parselet'' interface describes a statement denotation
     (std) parselet, a code generator for tokens representing
     statements."))

;;; -------------------------------------------------------

(defgeneric std-parselet-invoke (parselet parser token)
  (:documentation
    "Applies the std PARSELET to the PARSER and converts the std TOKEN
     into an abstract syntax tree (AST) node, which is consequently
     returned."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Standard-Std-Parselet".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Standard-Std-Parselet (Std-Parselet)
  ((precedence
    :initarg       :precedence
    :initform      (error "Missing precedence.")
    :type          Precedence
    :documentation "The statement's precedence information."))
  (:documentation
    "The ``Standard-Std-Parselet'' class implements a statement
     denotation (std) code generator for tokens representing
     statements."))

;;; -------------------------------------------------------

(defun make-standard-std-parselet (precedence)
  "Creates and returns a new ``Standard-Std-Parselet'' which binds
   tokens with the specified PRECEDENCE."
  (declare (type Precedence precedence))
  (the Standard-Std-Parselet
    (make-instance 'Standard-Std-Parselet :precedence precedence)))

;;; -------------------------------------------------------

(defmethod std-parselet-invoke ((parselet Standard-Std-Parselet)
                                (parser   Parser)
                                (token    Token))
  (declare (type Standard-Std-Parselet parselet))
  (declare (type Parser                parser))
  (declare (type Token                 token))
  (the Node
    (make-node (token-type token)
      :value (parse-with-precedence parser
               (slot-value parselet 'precedence)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of nud parselet registry.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Nud-Parselet) +NUD-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +NUD-PARSELETS+ (make-hash-table :test #'eq)
  "Associates token types with null denotation (nud) parselets.")

;;; -------------------------------------------------------

(defun register-nud-parselet (token-type parselet)
  "Registers the TOKEN-TYPE with the nud PARSELET and returns no value."
  (declare (type keyword      token-type))
  (declare (type Nud-Parselet parselet))
  (setf (gethash token-type +NUD-PARSELETS+)
        parselet)
  (values))

;;; -------------------------------------------------------

(defun nud-token-p (token)
  "Checks whether the TOKEN is associated with a nud parselet, that is,
   its represents either a terminal operand or a prefix operator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +NUD-PARSELETS+))))))

;;; -------------------------------------------------------

(defun nud (token)
  "Returns the parselet associated with the nud TOKEN, or signals an
   error of an unspecified type in the absence of any correspondence for
   the same."
  (declare (type Token token))
  (the Nud-Parselet
    (or (gethash (token-type token) +NUD-PARSELETS+)
        (error "No nud token: ~s." token))))

;;; -------------------------------------------------------

(register-nud-parselet :integer
  (make-integer-parselet))

(register-nud-parselet :reference
  (make-reference-parselet
    (make-precedence 100 :right-to-left)))

(register-nud-parselet :plus
  (make-unary-operator-parselet :plus
    (make-precedence 90 :right-to-left)))

(register-nud-parselet :minus
  (make-unary-operator-parselet :minus
    (make-precedence 90 :right-to-left)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of let parselet registry.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Led-Parselet) +LED-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +LED-PARSELETS+ (make-hash-table :test #'eq)
  "Associates token types with left denotation (led) parselets.")

;;; -------------------------------------------------------

(defun register-led-parselet (token-type parselet)
  "Registers the TOKEN-TYPE with the led PARSELET and returns no value."
  (declare (type keyword      token-type))
  (declare (type Led-Parselet parselet))
  (setf (gethash token-type +LED-PARSELETS+)
        parselet)
  (values))

;;; -------------------------------------------------------

(defun led-token-p (token)
  "Checks whether the TOKEN is associated with a led parselet, that is,
   its represents either an infix operator or a postfix operator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +LED-PARSELETS+))))))

;;; -------------------------------------------------------

(defun led-lbp (token)
  "Returns the binding power of the TOKEN, or signals an error of an
   unspecified type if the TOKEN does not represent a led token."
  (declare (type Token token))
  (the integer
    (precedence-binding-power
      (slot-value
        (gethash (token-type token) +LED-PARSELETS+)
        'precedence))))

;;; -------------------------------------------------------

(defun led (token)
  "Returns the parselet associated with the led TOKEN, or signals an
   error of an unspecified type in the absence of any correspondence for
   the same."
  (declare (type Token token))
  (the Led-Parselet
    (or (gethash (token-type token) +LED-PARSELETS+)
        (error "No led token: ~s." token))))

;;; -------------------------------------------------------

(register-led-parselet :multiplication
  (make-binary-operator-parselet
    (make-precedence 80 :left-to-right)))

(register-led-parselet :division
  (make-binary-operator-parselet
    (make-precedence 80 :left-to-right)))

(register-led-parselet :remainder
  (make-binary-operator-parselet
    (make-precedence 80 :left-to-right)))

(register-led-parselet :plus
  (make-binary-operator-parselet
    (make-precedence 70 :left-to-right)))

(register-led-parselet :minus
  (make-binary-operator-parselet
    (make-precedence 70 :left-to-right)))

(register-led-parselet :greater-than
  (make-binary-operator-parselet
    (make-precedence 60 :left-to-right)))

(register-led-parselet :greater-or-equal
  (make-binary-operator-parselet
    (make-precedence 60 :left-to-right)))

(register-led-parselet :less-than
  (make-binary-operator-parselet
    (make-precedence 60 :left-to-right)))

(register-led-parselet :less-or-equal
  (make-binary-operator-parselet
    (make-precedence 60 :left-to-right)))

(register-led-parselet :equal-to
  (make-binary-operator-parselet
    (make-precedence 50 :left-to-right)))

(register-led-parselet :not-equal-to
  (make-binary-operator-parselet
    (make-precedence 50 :left-to-right)))

(register-led-parselet :logical-and
  (make-binary-operator-parselet
    (make-precedence 40 :left-to-right)))

(register-led-parselet :logical-or
  (make-binary-operator-parselet
    (make-precedence 30 :left-to-right)))

(register-led-parselet :assignment
  (make-binary-operator-parselet
    (make-precedence 20 :right-to-left)))

(register-led-parselet :colon
  (make-range-parselet
    (make-precedence 120 :left-to-right)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of std parselet registry.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of keyword Std-Parselet) +STD-PARSELETS+))

;;; -------------------------------------------------------

(defparameter +STD-PARSELETS+ (make-hash-table :test #'eq)
  "Associates token types with statement denotation (std) parselets.")

;;; -------------------------------------------------------

(defun register-std-parselet (token-type parselet)
  "Registers the TOKEN-TYPE with the std PARSELET and returns no value."
  (declare (type keyword      token-type))
  (declare (type Std-Parselet parselet))
  (setf (gethash token-type +STD-PARSELETS+)
        parselet)
  (values))

;;; -------------------------------------------------------

(defun std-token-p (token)
  "Checks whether the TOKEN is associated with a std parselet, that is,
   its represents a statement, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (token-type token) +STD-PARSELETS+))))))

;;; -------------------------------------------------------

(defun std (token)
  "Returns the parselet associated with the std TOKEN, or signals an
   error of an unspecified type in the absence of any correspondence for
   the same."
  (declare (type Token token))
  (the Std-Parselet
    (or (gethash (token-type token) +STD-PARSELETS+)
        (error "No std token: ~s." token))))

;;; -------------------------------------------------------

(register-std-parselet :input
  (make-standard-std-parselet
    (make-precedence 110 :right-to-left)))

(register-std-parselet :print-number
  (make-standard-std-parselet
    (make-precedence 110 :right-to-left)))

(register-std-parselet :print-character
  (make-standard-std-parselet
    (make-precedence 110 :right-to-left)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Pratt parser routine.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parser-parse-expression (parser current-binding-power)
  "Parses an expression utilizing the PARSER's tokens in conjunction
   with the CURRENT-BINDING-POWER."
  (declare (type Parser  parser))
  (declare (type integer current-binding-power))
  
  (with-parser (parser)
    (let ((nud-token    NIL)
          (nud-parselet NIL)
          (left-node    NIL))
      (declare (type (or null Token)        nud-token))
      (declare (type (or null Nud-Parselet) nud-parselet))
      (declare (type (or null Node)         left-node))
      
      (setf nud-token    (consume))
      (setf nud-parselet (nud nud-token))
      (setf left-node    (nud-parselet-invoke nud-parselet parser nud-token))
      
      (loop do
        (cond
          ;; No further tokens available?
          ((token-type-p current-token :eof)
            (loop-finish))
          
          ;; The following token does not represent an operator?
          ((not (led-token-p current-token))
            (loop-finish))
          
          ;; The following token represents an operator, but it is
          ;; weaker than the current one?
          ((<= (led-lbp current-token) current-binding-power)
            (loop-finish))
          
          ;; The following token represents an operator which is
          ;; stronger than the current one?
          (T
            (let ((led-token    (consume))
                  (led-parselet NIL))
              (declare (type Token                  led-token))
              (declare (type (or null Led-Parselet) led-parselet))
              
              (setf led-parselet (led led-token))
              
              (setf left-node
                (led-parselet-invoke
                  led-parselet parser left-node led-token))))))
      
      (the Node left-node))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the program assembled by the PARSER and returns the root node
   of the resultant abstract syntax tree (AST)."
  (declare (type Parser parser))
  
  (with-parser (parser)
    (let ((initial-memory (parser-parse-initial-memory parser))
          (statements     NIL))
      (declare (type node-list initial-memory))
      (declare (type node-list statements))
      
      (loop until (token-type-p current-token :eof) do
        (push (parser-parse-case parser) statements))
      
      (the Node
        (make-node :program
          :initial-memory initial-memory
          :cases          (nreverse statements))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Integer-Vector".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Integer-Vector
  (:constructor make-integer-vector
    (&optional (initial-elements NIL)
     &aux      (elements
                 (coerce initial-elements
                   '(simple-array integer (*)))))))
  "The ``Integer-Vector'' class stores a fixed-size vector of integers."
  (elements
    (make-array 0
      :element-type    'integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)
    :type (simple-array integer (*))))

;;; -------------------------------------------------------

(defun integer-vector-element-at (vector index
                                  &optional
                                    (default NIL default-supplied-p))
  "Returns the element located at the INDEX in the integer VECTOR if the
   same obeys the VECTOR's bounds, or either responds with the DEFAULT
   value, if such is supplied, or, upon its absence, signals an error of
   an unspecified type."
  (declare (type Integer-Vector vector))
  (declare (type fixnum         index))
  (declare (type T              default))
  (declare (type T              default-supplied-p))
  (the T
    (cond
      ;; The INDEX constitutes a valid position in the VECTOR?
      ;; => Return the VECTOR element at the INDEX.
      ((array-in-bounds-p (integer-vector-elements vector) index)
        (aref (integer-vector-elements vector) index))
      ;; The INDEX violates the VECTOR bounds, but a DEFAULT value is
      ;; supplied?
      ;; => Return the DEFAULT.
      (default-supplied-p
        default)
      ;; The INDEX violates the VECTOR bounds, and no DEFAULT value is
      ;; specified?
      ;; => Signal an error.
      (T
        (error "The index ~d violates the vector's bounds [0, ~d]."
          index
          (1- (length (integer-vector-elements vector))))))))

;;; -------------------------------------------------------

(defun (setf integer-vector-element-at) (new-value vector index)
  "Stores the NEW-VALUE in the integer VECTOR at the specified INDEX and
   returns the modified VECTOR.
   ---
   Upon the INDEX' violation of the VECTOR bounds, an error of an
   unspecified type is signaled."
  (declare (type integer        new-value))
  (declare (type Integer-Vector vector))
  (declare (type fixnum         index))
  (setf (aref (integer-vector-elements vector) index)
        new-value)
  (the integer new-value))

;;; -------------------------------------------------------

(defun integer-vector-empty-p (vector)
  "Determines whether the integer VECTOR is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Integer-Vector vector))
  (the boolean
    (not (null
      (zerop (length (integer-vector-elements vector)))))))

;;; -------------------------------------------------------

(defun integer-vector-length (vector)
  "Returns the number of elements in the integer VECTOR."
  (declare (type Integer-Vector vector))
  (the fixnum
    (length (integer-vector-elements vector))))

;;; -------------------------------------------------------

(defun integer-vector-valid-index-p (vector index)
  "Determines whether the INDEX refers to a valid position in the
   integer VECTOR, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Integer-Vector vector))
  (declare (type fixnum         index))
  (the boolean
    (not (null
      (array-in-bounds-p
        (integer-vector-elements vector) index)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell-Range".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell-Range
  (:constructor make-cell-range (minimum maximum)))
  "The ``Cell-Range'' class represents a closed interval of memory cell
   indices, comprehending the inclusive minimum and inclusive maximum
   subscripts."
  (minimum 0 :type integer)
  (maximum 0 :type integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents the program memory as a sparse vector
   of signed integer-valued cells, amenable to non-negative integer
   indices, with each cell defaulting to zero (0)."
  (cells
    (make-hash-table :test #'eql)
    :type (hash-table-of index integer)))

;;; -------------------------------------------------------

(defgeneric memory-cell-at (memory location)
  (:documentation
    "Returns the cell value or values designated by teh MEMORY
     LOCATION."))

;;; -------------------------------------------------------

(defgeneric (setf memory-cell-at) (new-value memory location)
  (:documentation
    "Sets the MEMORY cell or cells specified by the LOCATION to the
     NEW-VALUE and returns no value."))

;;; -------------------------------------------------------

(defun memory-check-index (cell-index)
  "Determines whether the CELL-INDEX constitutes a valid subscript into
   a ``Memory'' object, on confirmation returning the unmodified
   CELL-INDEX, otherwise signaling an error of an unspecified type."
  (declare (type integer cell-index))
  (when (minusp cell-index)
    (error "Invalid cell index: ~d. Must be >= 0." cell-index))
  (the integer cell-index))

;;; -------------------------------------------------------

(defmethod memory-cell-at ((memory Memory) (cell-index integer))
  "Returns the value of the MEMORY cell located at the CELL-INDEX."
  (declare (type Memory memory))
  (declare (type index  cell-index)) 
  (memory-check-index cell-index)
  (the integer
    (gethash cell-index (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defmethod memory-cell-at ((memory       Memory)
                           (cell-indices Integer-Vector))
  "Returns a new ``Integer-Vector'' encompassing the MEMORY cells at the
   specified CELL-INDICES."
  (declare (type Memory         memory))
  (declare (type Integer-Vector cell-indices))
  (the Integer-Vector
    (make-integer-vector
      (loop
        for cell-index
          of-type integer
          across  (integer-vector-elements cell-indices)
        do
          (memory-check-index cell-index)
        collect
          (memory-cell-at memory cell-index)))))

;;; -------------------------------------------------------

(defmethod memory-cell-at ((memory     Memory)
                           (cell-range Cell-Range))
  "Returns a new ``Integer-Vector'' encompassing the MEMORY cells
   occupying the inclusive CELL-RANGE."
  (declare (type Memory     memory))
  (declare (type Cell-Range cell-range))
  (the Integer-Vector
    (make-integer-vector
      (loop
        for cell-index
          of-type integer
          from    (cell-range-minimum cell-range)
          to      (cell-range-maximum cell-range)
        do
          (memory-check-index cell-index)
        collect
          (memory-cell-at memory cell-index)))))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-value  integer)
                                  (memory     Memory)
                                  (cell-index integer))
  "Stores the NEW-VALUE in the MEMORY cell at the CELL-INDEX and returns
   the NEW-VALUE."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (declare (type index   cell-index))
  (memory-check-index cell-index)
  (setf (gethash cell-index (memory-cells memory) 0) new-value)
  (values))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-values Integer-Vector)
                                  (memory     Memory)
                                  (cell-index integer))
  "Stores the first item of the integer vector NEW-VALUES in the MEMORY
   cell at the CELL-INDEX, if the same is not empty, and returns the no
   value."
  (declare (type Integer-Vector new-values))
  (declare (type Memory         memory))
  (declare (type index          cell-index))
  (memory-check-index cell-index)
  (unless (integer-vector-empty-p new-values)
    (setf (memory-cell-at memory cell-index)
          (integer-vector-element-at new-values 0)))
  (values))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-value    integer)
                                  (memory       Memory)
                                  (cell-indices Integer-Vector))
  "Sets all MEMORY cells embraced in the CELL-INDEX to the NEW-VALUE and
   returns no value."
  (declare (type integer        new-value))
  (declare (type Memory         memory))
  (declare (type Integer-Vector cell-indices))
  (loop
    for cell-index
      of-type integer
      across  (integer-vector-elements cell-indices)
    do
      (memory-check-index cell-index)
      (setf (memory-cell-at memory cell-index) new-value))
  (values))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-values   Integer-Vector)
                                  (memory       Memory)
                                  (cell-indices Integer-Vector))
  "Sets the MEMORY cells embraced in the CELL-INDEX to the NEW-VALUES
   and returns no value.
   ---
   Supernumerary elements in the NEW-VALUES integer vector are ignored;
   unmatched indices in the CELL-INDICES commit to rejection similiter."
  (declare (type Integer-Vector new-values))
  (declare (type Memory         memory))
  (declare (type Integer-Vector cell-indices))
  (loop
    for cell-index
      of-type integer
      across  (integer-vector-elements cell-indices)
    and new-value
      of-type integer
      across  (integer-vector-elements new-values)
    do
      (memory-check-index cell-index)
      (setf (memory-cell-at memory cell-index) new-value))
  (values))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-value  integer)
                                  (memory     Memory)
                                  (cell-range Cell-Range))
  "Sets all MEMORY cells occupying the CELL-RANGE to the NEW-VALUE and
   returns no value."
  (declare (type integer    new-value))
  (declare (type Memory     memory))
  (declare (type Cell-Range cell-range))
  (loop
    for cell-index
      of-type integer
      from    (cell-range-minimum cell-range)
      to      (cell-range-maximum cell-range)
    do
      (memory-check-index cell-index)
      (setf (memory-cell-at memory cell-index)
            new-value))
  (values))

;;; -------------------------------------------------------

(defmethod (setf memory-cell-at) ((new-values Integer-Vector)
                                  (memory     Memory)
                                  (cell-range Cell-Range))
  "Sets the MEMORY cells occupying the CELL-RANGE to the NEW-VALUES and
   returns no value.
   ---
   Supernumerary elements in the NEW-VALUES integer vector are ignored;
   unmatched indices in the CELL-RANGE commit to rejection similiter."
  (declare (type Integer-Vector new-values))
  (declare (type Memory         memory))
  (declare (type Cell-Range     cell-range))
  (loop
    for cell-index
      of-type integer
      from    (cell-range-minimum cell-range)
      to      (cell-range-maximum cell-range)
    and new-cell-value
      of-type integer
      across  (integer-vector-elements new-values)
    do
      (memory-check-index cell-index)
      (setf (memory-cell-at memory cell-index)
            new-cell-value))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "Reference".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Reference
  "The ``Reference'' interface represents a designator utilized in the
   access of memory cells.")

;;; -------------------------------------------------------

(defstruct (Scalar-Reference
  (:include     Reference)
  (:constructor make-scalar-reference (index)))
  "The ``Scalar-Reference'' class implements the ``Reference'' interface
   in order to capacitate accessing a single memory cell by its index."
  (index 0 :type integer))

;;; -------------------------------------------------------

(defstruct (Range-Reference
  (:include     Reference)
  (:constructor make-range-reference (bounds)))
  "The ``Range-Reference'' class implements the ``Reference'' interface
   in order to capacitate accessing a range of memory cells by the
   inclusive minimum and inclusive maximum indices."
  (bounds (error "Missing range reference bounds.") :type Cell-Range))

;;; -------------------------------------------------------

(defstruct (Indirect-Reference
  (:include     Reference)
  (:constructor make-indirect-reference (target)))
  "The ``Indirect-Reference'' class implements the ``Reference''
   interface in its pursuit to represent a redirection to another
   ``Reference'' instance, its target."
  (target (error "Missing indirect reference target.") :type Reference))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of reference operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reference-query-memory (reference memory)
  (:documentation
    "Returns one or more cell values from the MEMORY in concord with the
     REFERENCE's nature.")
  
  (:method ((reference Scalar-Reference)
            (memory    Memory))
    "Returns the value of the MEMORY cell located at the scalar
     REFERENCE's index."
    (declare (type Scalar-Reference reference))
    (declare (type Memory           memory))
    (the integer
      (memory-cell-at memory (scalar-reference-index reference))))
  
  (:method ((reference Indirect-Reference)
            (memory    Memory))
    "Returns the cell value or values --- depending on the REFERENCE's
     target reference --- of the MEMORY cell or cells, either as a
     scalar integer or as an ``Integer-Vector''."
    (declare (type Indirect-Reference reference))
    (declare (type Memory             memory))
    (the (or integer Integer-Vector)
      (memory-cell-at memory
        (reference-query-memory
          (indirect-reference-target reference)
          memory))))
  
  (:method ((reference Range-Reference)
            (memory    Memory))
    "Returns a new ``Integer-Vector'' comprised of the MEMORY cell
     values occupying the closed range of the ``Range-Reference''
     REFERENCE."
    (declare (type Range-Reference reference))
    (declare (type Memory          memory))
    (the Integer-Vector
      (memory-cell-at memory
        (range-reference-bounds reference)))))

;;; -------------------------------------------------------

(defgeneric reference-update-memory (reference memory new-value)
  (:documentation
    "Modifies the MEMORY cell or cells amenable to the REFERENCE's
     location designator to embrace the NEW-VALUE and returns no
     value.")
  
  (:method ((reference Scalar-Reference)
            (memory    Memory)
            (new-value integer))
    "Sets the MEMORY cell amenable to the scalar REFERENCE's index to
     the scalar NEW-VALUE and returns no value."
    (declare (type Scalar-Reference reference))
    (declare (type Memory           memory))
    (declare (type integer          new-value))
    (setf (memory-cell-at memory (scalar-reference-index reference))
          new-value)
    (values))
  
  (:method ((reference  Scalar-Reference)
            (memory     Memory)
            (new-values Integer-Vector))
    "Sets the MEMORY cell amenable to the scalar REFERENCE's index to
     the first element of the integer vector NEW-VALUES, or abstains
     from any action if the same is empty, and returns in any case no
     value."
    (declare (type Scalar-Reference reference))
    (declare (type Memory           memory))
    (declare (type Integer-Vector   new-values))
    (setf (memory-cell-at memory (scalar-reference-index reference))
          new-values)
    (values))
  
  (:method ((reference Range-Reference)
            (memory    Memory)
            (new-value integer))
    "Sets the MEMORY cells amenable to the ranged reference's index
     range to the scalar NEW-VALUE and returns no value."
    (declare (type Range-Reference reference))
    (declare (type Memory          memory))
    (declare (type integer         new-value))
    (setf (memory-cell-at memory (range-reference-bounds reference))
          new-value)
    (values))
  
  (:method ((reference  Range-Reference)
            (memory     Memory)
            (new-values Integer-Vector))
    "Sets the MEMORY cells amenable to the ranged REFERENCE's index
     range to the integer vector NEW-VALUES' elements and returns no
     value.
     ---
     If the REFERENCE range's extent exceeds the NEW-VALUES vector's
     cardinality, the supernumerary indices remain unmodified. Siclike,
     a prepotent tally in the NEW-VALUES is curtailed to a smaller
     REFERENCE span."
    (declare (type Range-Reference reference))
    (declare (type Memory          memory))
    (declare (type Integer-Vector  new-values))
    (setf (memory-cell-at memory (range-reference-bounds reference))
          new-values)
    (values))
  
  (:method ((reference Indirect-Reference)
            (memory    Memory)
            (new-value integer))
    "Sets the MEMORY cells amenable to the REFERENCE's target reference
     to the scalar NEW-VALUE and returns no value."
    (declare (type Indirect-Reference reference))
    (declare (type Memory             memory))
    (declare (type integer            new-value))
    (let ((cell-locator
            (reference-query-memory reference memory)))
      (declare (type (or integer Integer-Vector) cell-locator))
      (setf (memory-cell-at memory cell-locator) new-value))
    (values))
  
  (:method ((reference  Indirect-Reference)
            (memory     Memory)
            (new-values Integer-Vector))
    "Sets the MEMORY cells amenable to the REFERENCE's target reference
     to the integer vector NEW-VALUES' elements and returns no value."
    (declare (type Indirect-Reference reference))
    (declare (type Memory             memory))
    (declare (type Integer-Vector     new-values))
    (let ((cell-locator
            (reference-query-memory reference memory)))
      (declare (type (or integer Integer-Vector) cell-locator))
      (setf (memory-cell-at memory cell-locator) new-values))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-whether-operands-are-booleans (left-operand right-operand)
  "Determines whether the LEFT-OPERAND and RIGHT-OPERAND both constitute
   ``boolean'' objects, on confirmation simply terminating without
   returning a value, otherwise signaling an error of an unspecified
   type."
  (declare (type T left-operand))
  (declare (type T right-operand))
  (cond
    ((not (typep left-operand 'boolean))
      (error "The left binary operand, ~s, is not a ``boolean''."
        left-operand))
    ((not (typep right-operand 'boolean))
      (error "The right binary operand, ~s, is not a ``boolean''."
        right-operand))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defgeneric map-integer-vector (operation
                                left-operand
                                right-operand
                                &key &allow-other-keys)
  (:documentation
    "Invokes the binary OPERATION on each element of the
     ``Integer-Vector'', which might be one or both of the LEFT-OPERAND
     and RIGHT-OPERAND, and returns a new ``Integer-Vector''
     comprehending the result.
     ---
     The OPERATION must be a callback function conforming to the
     signature
     
       lambda (integer integer) => integer")
  
  (:method ((operation     function)
            (left-operand  Integer-Vector)
            (right-operand Integer-Vector)
            &key (default 0)
            &allow-other-keys)
    (declare (type integer-function operation))
    (declare (type Integer-Vector   left-operand))
    (declare (type Integer-Vector   right-operand))
    (declare (type integer          default))
    (let ((maximum-index
            (max (integer-vector-length left-operand)
                 (integer-vector-length right-operand))))
      (declare (type fixnum maximum-index))
      (the Integer-Vector
        (make-integer-vector
          (loop
            for index of-type fixnum from 0 below maximum-index
            collect
              (funcall operation
                (integer-vector-element-at
                  left-operand index default)
                (integer-vector-element-at
                  right-operand index default)))))))
  
  (:method ((operation     function)
            (left-operand  Integer-Vector)
            (right-operand integer)
            &key &allow-other-keys)
    (declare (type integer-function operation))
    (declare (type Integer-Vector   left-operand))
    (declare (type integer          right-operand))
    (the Integer-Vector
      (make-integer-vector
        (loop
          for vector-element
            of-type integer
            across  (integer-vector-elements left-operand)
          collect
            (the integer
              (funcall operation vector-element right-operand))))))
  
  (:method ((operation     function)
            (left-operand  integer)
            (right-operand Integer-Vector)
            &key &allow-other-keys)
    (declare (type integer-function operation))
    (declare (type integer          left-operand))
    (declare (type Integer-Vector   right-operand))
    (the Integer-Vector
      (make-integer-vector
        (loop
          for vector-element
            of-type integer
            across  (integer-vector-elements right-operand)
          collect
            (the integer
              (funcall operation left-operand vector-element)))))))

;;; -------------------------------------------------------

(defgeneric apply-binary-operation (operator left right)
  (:documentation
    "Applies the binary OPERATOR to the LEFT and RIGHT operands in this
     exact order and returns a result appropriate for this argument
     combination."))

;;; -------------------------------------------------------

(defmacro define-binary-operation (operator (left-type right-type)
                                   &body body)
  "Defines an implementation of the generic function
   ``apply-binary-operation'', the first parameter of which is nevened
   automatically and dispatched on an ``eql''-equality with the
   OPERATOR, the second parameter is named ``left'' and specified by the
   LEFT-TYPE, the third parameter, fixed as ``right'', by the
   RIGHT-TYPE, its body being defined by the BODY forms.
   ---
   In a compendious apercu, the following argument list is specified:
   
     -----------------------------------------------------------
     Parameter no. | Name                      | Type
     --------------+---------------------------+----------------
     1             | (automatically generated) | (eql OPERATOR)
     ...........................................................
     2             | left                      | LEFT-TYPE
     ...........................................................
     3             | right                     | RIGHT-TYPE
     -----------------------------------------------------------"
  (let ((operator-variable (gensym)))
    (declare (type symbol operator-variable))
    `(defmethod apply-binary-operation
         ((,operator-variable (eql ,operator))
          (left               ,left-type)
          (right              ,right-type))
       (declare (type keyword     ,operator-variable))
       (declare (type ,left-type  left))
       (declare (type ,right-type right))
       (declare (ignore           ,operator-variable))
       (declare (ignorable        left))
       (declare (ignorable        right))
       ,@body)))

;;; -------------------------------------------------------

(define-binary-operation :plus (integer integer)
  (the integer
    (+ left right)))

;;; -------------------------------------------------------

(define-binary-operation :plus (integer Integer-Vector)
  (the integer-vector
    (map-integer-vector #'+ left right)))

;;; -------------------------------------------------------

(define-binary-operation :plus (Integer-Vector integer)
  (the integer-vector
    (map-integer-vector #'+ left right)))

;;; -------------------------------------------------------

(define-binary-operation :plus (Integer-Vector Integer-Vector)
  (the integer-vector
    (map-integer-vector #'+ left right :default 0)))

;;; -------------------------------------------------------

(define-binary-operation :minus (integer integer)
  (the integer
    (- left right)))

;;; -------------------------------------------------------

(define-binary-operation :minus (integer Integer-Vector)
  (the integer-vector
    (map-integer-vector #'- left right)))

;;; -------------------------------------------------------

(define-binary-operation :minus (Integer-Vector integer)
  (the integer-vector
    (map-integer-vector #'- left right)))

;;; -------------------------------------------------------

(define-binary-operation :minus (Integer-Vector Integer-Vector)
  (the integer-vector
    (map-integer-vector #'- left right :default 0)))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (integer integer)
  (the integer
    (* left right)))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (integer Integer-Vector)
  (the integer-vector
    (map-integer-vector #'* left right)))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (Integer-Vector integer)
  (the integer-vector
    (map-integer-vector #'* left right)))

;;; -------------------------------------------------------

(define-binary-operation :multiplication (Integer-Vector Integer-Vector)
  (the integer-vector
    (map-integer-vector #'* left right :default 0)))

;;; -------------------------------------------------------

(define-binary-operation :division (integer integer)
  (the integer
    (round left right)))

;;; -------------------------------------------------------

(define-binary-operation :division (integer Integer-Vector)
  (the integer-vector
    (map-integer-vector #'round left right)))

;;; -------------------------------------------------------

(define-binary-operation :division (Integer-Vector integer)
  (the integer-vector
    (map-integer-vector #'round left right)))

;;; -------------------------------------------------------

(define-binary-operation :division (Integer-Vector Integer-Vector)
  (the integer-vector
    (map-integer-vector #'round left right :default 1)))

;;; -------------------------------------------------------

(define-binary-operation :remainder (integer integer)
  (the integer
    (mod left right)))

;;; -------------------------------------------------------

(define-binary-operation :remainder (integer Integer-Vector)
  (the integer-vector
    (map-integer-vector #'mod left right)))

;;; -------------------------------------------------------

(define-binary-operation :remainder (Integer-Vector integer)
  (the integer-vector
    (map-integer-vector #'mod left right)))

;;; -------------------------------------------------------

(define-binary-operation :remainder (Integer-Vector Integer-Vector)
  (the integer-vector
    (map-integer-vector #'mod left right :default 1)))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (integer integer)
  (the boolean
    (not (null
      (= left right)))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (= left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (= vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (Integer-Vector Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'=
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :equal-to (T T)
  (check-whether-operands-are-booleans left right)
  (the boolean
    (not (null
      (eq left right)))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (integer integer)
  (the boolean
    (not (null
      (/= left right)))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (/= left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (/= vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (Integer-Vector Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'/=
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :not-equal-to (T T)
  (check-whether-operands-are-booleans left right)
  (the boolean
    (not (eq left right))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (integer integer)
  (the boolean
    (not (null
      (< left right)))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (< left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (< vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :less-than (Integer-Vector Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'<
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :less-or-equal (integer integer)
  (the boolean
    (not (null
      (<= left right)))))

;;; -------------------------------------------------------

(define-binary-operation :less-or-equal (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (<= left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :less-or-equal (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (<= vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :less-or-equal (Integer-Vector Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'<=
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (integer integer)
  (the boolean
    (not (null
      (> left right)))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (> left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (> vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-than (Integer-Vector Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'>
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-or-equal (integer integer)
  (the boolean
    (not (null
      (>= left right)))))

;;; -------------------------------------------------------

(define-binary-operation :greater-or-equal (integer Integer-Vector)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (>= left vector-element))
        (integer-vector-elements right))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-or-equal (Integer-Vector integer)
  (the boolean
    (not (null
      (every
        #'(lambda (vector-element)
            (declare (type integer vector-element))
            (>= vector-element right))
        (integer-vector-elements left))))))

;;; -------------------------------------------------------

(define-binary-operation :greater-or-equal (Integer-Vector
                                            Integer-Vector)
  (the boolean
    (not (null
      (and
        (= (integer-vector-length left)
           (integer-vector-length right))
        (every #'>=
          (integer-vector-elements left)
          (integer-vector-elements right)))))))

;;; -------------------------------------------------------

(define-binary-operation :logical-and (T T)
  (check-whether-operands-are-booleans left right)
  (the boolean
    (and left right)))

;;; -------------------------------------------------------

(define-binary-operation :logical-or (T T)
  (check-whether-operands-are-booleans left right)
  (the boolean
    (or left right)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of print operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-number (object destination)
  (:documentation
    "Prints an object as a single number or a sequence thereof and
     returns no value.")
  
  (:method ((object integer) destination)
    (declare (type integer     object))
    (declare (type destination destination))
    (format destination "~d " object)
    (values))
  
  (:method ((object Integer-Vector) destination)
    (declare (type Integer-Vector object))
    (declare (type destination    destination))
    (loop
      for element
        of-type integer
        across  (integer-vector-elements object)
      do
        (format destination "~d " element))
    (values)))

;;; -------------------------------------------------------

(defgeneric print-character (object destination)
  (:documentation
    "Prints an object as a single character or a sequence of such and
     returns no value.")
  
  (:method ((object integer) destination)
    (declare (type integer     object))
    (declare (type destination destination))
    (format destination "~c "
      (code-char object))
    (values))
  
  (:method ((object Integer-Vector) destination)
    (declare (type Integer-Vector object))
    (declare (type destination    destination))
    (loop
      for character-code
        of-type integer
        across  (integer-vector-elements object)
      do
        (format destination "~c"
          (code-char character-code))
      finally
        (format destination " "))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Interpreter Node) *) visit-node))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((tree
    :initarg       :tree
    :initform      (error "Missing interpreter AST.")
    :type          Node
    :documentation "The root of the abstract syntax tree (AST) to
                    evaluate.")
   (memory
    :initarg       :memory
    :initform      (make-memory)
    :type          Memory
    :documentation "The program memory represented by a sparse table
                    structuring.
                    ---
                    Any absent entry is conceited as zero-valued.")
   (output-conduit
    :initarg       :output-conduit
    :initform      T
    :type          destination
    :documentation "The channel to print output to."))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluation of an
     abstract syntax tree (AST) representing a Piquant program."))

;;; -------------------------------------------------------

(defun make-interpreter (tree &key (output-conduit T))
  "Creates and returns a new ``Interpreter'' assigned to the evaluation
   of the abstract syntax TREE."
  (declare (type Node        tree))
  (declare (type destination output-conduit))
  (the Interpreter
    (make-instance 'Interpreter
      :tree           tree
      :output-conduit output-conduit)))

;;; -------------------------------------------------------

(defun value-of (interpreter object)
  "Resolves the OBJECT in the INTERPRETER's context and returns its
   value."
  (declare (type Interpreter interpreter))
  (declare (type T           object))
  (the piquant-object
    (typecase object
      (integer
        object)
      (Reference
        (reference-query-memory object
          (slot-value interpreter 'memory)))
      (boolean
        object)
      (otherwise
        (error "Cannot retrieve the value of the object ~s." object)))))

;;; -------------------------------------------------------

(defun interpreter-apply-initial-memory (interpreter)
  "Applies the initial memory cell values, as specified by the abstract
   syntax tree (AST) stored in the INTERPRETER, and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (memory tree) interpreter
    (declare (type Memory memory))
    (declare (type Node   tree))
    (let ((initial-cell-values (node-attribute tree :initial-memory)))
      (declare (type node-list initial-cell-values))
      (loop
        for initial-value of-type Node  in   initial-cell-values
        and cell-index    of-type index from 0
        do  (setf (memory-cell-at memory cell-index)
              (value-of interpreter
                (visit-node interpreter initial-value))))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-select-case (interpreter)
  "Finds and returns the first case node in the INTERPRETER's abstract
   syntax tree whose condition subtree is satisfied and returns this
   node, or responds with ``NIL'' if none of the predicates are met."
  (declare (type Interpreter interpreter))
  (the (or null Node)
    (find-if
      #'(lambda (case)
          (declare (type Node case))
          (the boolean
            (not (null
              (visit-node interpreter
                (node-attribute case :condition))))))
      (node-attribute (slot-value interpreter 'tree) :cases))))

;;; -------------------------------------------------------

(defun interpreter-execute-actions (interpreter case)
  "Executes the CASE node's action nodes using the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Node        case))
  (dolist (action (node-attribute case :actions))
    (declare (type Node action))
    (visit-node interpreter action))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the abstract syntax tree (AST) using the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  
  ;; Apply the initial memory configuration.
  (interpreter-apply-initial-memory interpreter)
  
  ;; Repeatedly find the first matching case and execute it, until no
  ;; suitable cases can be ascertained anymore.
  (loop
    for matching-case
      of-type (or null Node)
      =       (interpreter-select-case interpreter)
    while matching-case
    do    (interpreter-execute-actions interpreter matching-case))
  
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE identified by the NODE-TYPE utilizing the
     INTERPRETER and returns a value appropriate for this
     combination."))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Visits the NODE using the INTERPRETER by invoking the generic
   function ``dispatch-node'' with the INTEPRETER and NODE in
   conjunction with the NODE's type as a dispatching criterion, and
   returns a value appropriate for the NODE type."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T (dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmacro define-node-processor (node-type
                                 (interpreter-variable node-variable)
                                 &body body)
  "Defines an implementation of the ``dispatch-node'' generic function
   whose first argument ``interpreter'' is designated by the
   INTERPRETER-VARIABLE of the ``Interpreter'' class, its third argument
   ``node'' by the NODE-VARIABLE of the ``Node'' class, and whose
   second argument ``node-type'' is established verbatim; while the
   function incorporates the BODY statements and returns the last
   evaluated form's results."
  `(defmethod dispatch-node ((,interpreter-variable Interpreter)
                             (node-type             (eql ,node-type))
                             (,node-variable        Node))
     (declare (type Interpreter ,interpreter-variable))
     (declare (type keyword     node-type))
     (declare (type Node        ,node-variable))
     (declare (ignorable        ,interpreter-variable))
     (declare (ignore           node-type))
     (declare (ignorable        ,node-variable))
     ,@body))

;;; -------------------------------------------------------

(defun resolve-reference (reference)
  "Resolves the REFERENCE node and returns a ``Reference''
   representation of its cell indices specification."
  (declare (type Node reference))
  (let ((target (node-attribute reference :operand)))
    (declare (type Node target))
    (the Reference
      (case (node-type target)
        (:integer
          (make-scalar-reference
            (node-attribute target :value)))
        (:reference
          (make-indirect-reference
            (resolve-reference target)))
        (:range
          (make-range-reference
            (make-cell-range
              (node-attribute
                (node-attribute target :minimum)
                :value)
              (node-attribute
                (node-attribute target :maximum)
                :value))))
        (otherwise
          (error "Invalid reference target: ~s." target))))))

;;; -------------------------------------------------------

(define-node-processor :equal-to (interpreter node)
  (the boolean
    (apply-binary-operation :equal-to
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :not-equal-to (interpreter node)
  (the boolean
    (apply-binary-operation :not-equal-to
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :less-than (interpreter node)
  (the boolean
    (apply-binary-operation :less-than
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :less-or-equal (interpreter node)
  (the boolean
    (apply-binary-operation :less-or-equal
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :greater-than (interpreter node)
  (the boolean
    (apply-binary-operation :greater-than
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :greater-or-equal (interpreter node)
  (the boolean
    (apply-binary-operation :greater-or-equal
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :logical-and (interpreter node)
  (the boolean
    (apply-binary-operation :logical-and
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :logical-or (interpreter node)
  (the boolean
    (apply-binary-operation :logical-or
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :integer (interpreter node)
  (the integer
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-processor :reference (interpreter node)
  (the Reference
    (resolve-reference node)))

;;; -------------------------------------------------------

(define-node-processor :input (interpreter node)
  (let ((target (node-attribute node :value)))
    (declare (type Node target))
    
    (format
      (slot-value interpreter 'output-conduit)
      "~&Please enter an integer: ")
    
    (reference-update-memory
      (resolve-reference target)
      (slot-value interpreter 'memory)
      (parse-integer (read-line))))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(define-node-processor :assignment (interpreter node)
  (let ((reference
          (resolve-reference
            (node-attribute node :left)))
        (right
          (node-attribute node :right)))
    (declare (type Reference reference))
    (declare (type Node      right))
    
    (reference-update-memory reference
      (slot-value interpreter 'memory)
      (value-of interpreter
        (visit-node interpreter right))))
  (values))

;;; -------------------------------------------------------

(define-node-processor :plus (interpreter node)
  (the piquant-object
    (apply-binary-operation :plus
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :minus (interpreter node)
  (the piquant-object
    (apply-binary-operation :minus
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :multiplication (interpreter node)
  (the piquant-object
    (apply-binary-operation :multiplication
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :division (interpreter node)
  (the piquant-object
    (apply-binary-operation :division
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :remainder (interpreter node)
  (the piquant-object
    (apply-binary-operation :remainder
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :left)))
      (value-of interpreter
        (visit-node interpreter
          (node-attribute node :right))))))

;;; -------------------------------------------------------

(define-node-processor :print-number (interpreter node)
  (print-number
    (value-of interpreter
      (visit-node interpreter
        (node-attribute node :value)))
    (slot-value interpreter 'output-conduit))
  (values))

;;; -------------------------------------------------------

(define-node-processor :print-character (interpreter node)
  (print-character
    (value-of interpreter
      (visit-node interpreter
        (node-attribute node :value)))
    (slot-value interpreter 'output-conduit))
  (values))

;;; -------------------------------------------------------

(define-node-processor :unary-operation (interpreter node)
  (the integer
    (case (node-attribute node :operator)
      (:plus
        (+ (value-of interpreter
             (visit-node interpreter
               (node-attribute node :operand)))))
      (:minus
        (- (value-of interpreter
             (visit-node interpreter
               (node-attribute node :operand)))))
      (otherwise
        (error "Invalid unary operator: ~s." node)))))

;;; -------------------------------------------------------

(defun interpret-Piquant (code &key (output-conduit T))
  "Interprets the piece of Piquant CODE and returns no value."
  (declare (type string      code))
  (declare (type destination output-conduit))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))
      :output-conduit output-conduit))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello world".
(interpret-Piquant
  "[72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]
   {A0 == 72; qA0:10; A0 = 0}")

;;; -------------------------------------------------------

;; Print "Hello world" by mediation of indirect referencing: The memory
;; cells A0:10 comprehend the "Hello world" ASCII codes, whereas those
;; spanning the memory portion A11:21 store the cell indices which, when
;; printing in this exact order as ASCII characters, replicate the
;; effect of A0:10, that is, the message "Hello world".
(interpret-Piquant
  "[72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100,
    0,  1,   2,   3,   4,   5,   6,  7,   8,   9,   10]
   {A0 == 72; qAA11:21; A0 = 0}")

;;; -------------------------------------------------------

;; Print "Hello world" by mediation of indirect referencing: Copy the
;; ASCII codes of the message from the memory cells A0:10 to the
;; locations A11:21, and subsequently print this new range in character
;; form, thus reproducing "Hello world".
(interpret-Piquant
  "[72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]
   {A0 == 72; A11:21 = A0:10; qA11:21; A0 = 0}")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program which terminates on a user
;; input equal to zero (0).
(interpret-Piquant
  "[-1]
   { A0 != 0; iA0; pA0 }")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which prints the ASCII characters
;; corresponding to user inputs and terminates if encountering the value
;; zero (0).
(interpret-Piquant
  "[-1]
   { A0 != 0; iA0; qA0 }")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The cell A0 stores the user input, which is concomitantly printed to
;; the standard output. The cell A1 serves in an adminicular purpose by
;; determining whether the user input has already been requested, with a
;; value of zero (0), being the cells' default, claiming no previous
;; input, whereas a value of one (1) designates a preceding query.
(interpret-Piquant
  "[]
   {A0 == 0 && A1 == 0; iA0; pA0; A1 = 1}
   {A0 == 1;            pA0}")

;;; -------------------------------------------------------

;; Print the number 1 (one).
(interpret-Piquant
  "[]
   {A0 == 0; p1; A0 = 1}")

;;; -------------------------------------------------------

;; Print the letter "A".
(interpret-Piquant
  "[]
   {A0 == 0; q65; A0 = 1}")

;;; -------------------------------------------------------

;; Print the integers from one (1) to ten (10).
(interpret-Piquant
  "[1]
   { A0 <= 10; pA0; A0 = A0 + 1 }")

;;; -------------------------------------------------------

;; Test negation (unary minus).
(interpret-Piquant
  "[1]
   { A0 != -1 ; pA0; A0 = -A0 ; pA0 }")

;;; -------------------------------------------------------

;; Test indirect references.
;; 
;; Outputs: 2 and 69, the latter representing the element at the cell
;; index 2.
(interpret-Piquant
  "[1, 2, 69]
   { A0 == 1 ; A0 = 0 ; pA1; pAA1 }")

;;; -------------------------------------------------------

;; Test indirect references.
;; 
;; Sets the first cell (index = 0) to the element in the second cell
;; (index = 1), that is, to the value 2, and prints the new value of
;; the cell at index 0 (= 2).
(interpret-Piquant
  "[1, 2]
   { A0 == 1 ; A0 = AA0 ; pA0 }")

;;; -------------------------------------------------------

;; Test indirect references.
;; 
;; Prints:
;;   5 6 7 0 1 2 1 65 5 
(interpret-Piquant
  "[1, 65, 5, 6, 7, 0, 1, 2]
   { A0 == 1 ; pA2:4; pAA2:4; pAAA2:4; A0 = -1 }")

;;; -------------------------------------------------------

;; Test indirect references.
;; 
;; Prints:
;;   5 6 7 8 9 10 0 1 2 
(interpret-Piquant
  "[1, 65, 5, 6, 7, 8, 9, 10, 0, 1, 2]
   { A0 == 1 ; pA2:4; pAA2:4; pAAA2:4; A0 = -1 }")

;;; -------------------------------------------------------

;; Test indirect references in a memory update context.
;; 
;; Prints:
;;   65 65 65 6 7 8 9 10 0 1 2 
(interpret-Piquant
  "[1, 65, 5, 6, 7, 8, 9, 10, 0, 1, 2]
   { A0 == 1 ; AAA2:4 = 65 ; pA0:10 }")

;;; -------------------------------------------------------

;; Fibonacci series.
(interpret-Piquant
  "[1]
   {A0 >= A1; pA0; A1 = A0 + A1}
   {A0 < A1; pA1; A0 = A0 + A1}")

;;; -------------------------------------------------------

;; Factorial.
;; 
;; Memory layout:
;;   A0 --- the current factorial value
;;   A1 --- the input N, for which the factorial N! shall be computed.
(interpret-Piquant
  "[]
   {A0 == 0; A0 = 1; iA1}
   {A1 > 1; A0 = A0 * A1; A1 = A1 - 1}
   {A1 == 1; pA0; A1 = 0}")

;;; -------------------------------------------------------

;; FizzBuzz program.
(interpret-Piquant
  "[1, 70, 105, 122, 122, 66, 117, 122, 122]
   {A0 > 100; A0 = -1}
   {A0 % 3 == 0 && A0 % 5 == 0; qA1:8; A0 = A0 + 1}
   {A0 % 3 == 0; qA1:4; A0 = A0 + 1}
   {A0 % 5 == 0; qA5:8; A0 = A0 + 1}
   {A0 != -1; pA0; A0 = A0 + 1}")
