;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainfuck", invented by Urban Mueller in the year 1993,
;; the haecceity of which wones in its restricted instruction set,
;; its circumference an octuple account, each designated by a single
;; character, whence yet a Turing-complete status is accomplished.
;; 
;; 
;; Concept
;; =======
;; All brainfuck programs operate on a tape of byte-valued cells, its
;; active entity designated by a mobile cell pointer. The instructions
;; are designated by a symbol each, an octuple membership whose
;; potentials entalent it with Turing-completeness.
;; 
;; == THE MEMORY: A TAPE OF BYTES ==
;; The brainfuck programming language is founded upon the manipulation
;; of a tape compact of a bilaterally infinite dispansion of unsigned
;; byte-valued cell, the currently active member among which, endowed
;; with the capacity for indagations and modifications, is selected by
;; a mobile cell pointer.
;; 
;; == OPERATIONS: AN OCTUPLE OF SINGLE-CHARACTER BEHESTS ==
;; All operations, amplecting a tally of eight, are expressed by an
;; identifier whose agnomination includes an aefauld symbol. These
;; facilities comprehend parvipotent arithmetics, memory management,
;; input, output, as well as a jump-based control flow.
;; 
;; == BRAINFUCK AND TURING-COMPLETENESS ==
;; brainfuck's kenspeckle parturition is peccant of bewraying its status
;; as a competent warklume and a polymechany. A programming language
;; imbued with the capacity for an equiparation to this specimen as a
;; consectary chevises its own Turing-completeness' attest, a proprium
;; desumed from brainfuck's personal potence.
;; 
;; 
;; Architecture
;; ============
;; brainfuck's architectural diorism amplects a tape enumerating an
;; infinite tally of cells along both of its axes, each such unit an
;; aefauld unsigned byte's salvatory. A designated and mobile marker,
;; the "cell pointer" applies itself to the current cell's selection,
;; the sole member entalented with a respondency to indagations and
;; manipulations.
;; 
;; == THE TAPE IS A BILATERALLY INFINITE LINE OF CELLS ==
;; A program's data castaldy is realized in a linear arrangement of
;; cells, a tape of infinite dispansion along both of its lateralities.
;; 
;; == EACH CELL COMPREHENDS AN UNSIGNED BYTE SCALAR ==
;; Every cell's amplectation is laid around a single unsigned byte
;; datum, commorant, proceeding from this consectary, in the closed
;; integral range spanning [0, 255].
;; 
;; Initially acquiring the minimum value of zero (0), stepwise
;; incrementations and decrementations may be imposed upon these units.
;; Any of its two bournes' transgressions conditions a wrapping around
;; towards the athwart extremum; that is, if the value descends alow the
;; bottom march of zero (0), the state assumes the maximum of 255;
;; obversely, an excess beyond the upper bourne of 255 relapses to the
;; minimum of zero (0).
;; 
;; == THE CELL POINTER SELECTS THE ACTIVE CELL ==
;; At any instant in the program only one cell may answer to
;; perquisition and modification requests, its status as such particular
;; member being a dependency upon the cell pointer's referral.
;; 
;; Empighted at the execution's inchoation at the first cell, its
;; capacitation for alternating the referred unit is realized in the
;; pointer's amenability to gradual relocations along any of the two
;; tape axes.
;; 
;; 
;; Data Types
;; ==========
;; A bifurcation governs brainfuck's type system and cleaves the same
;; into the paravaunt unsigned byte species and the parhedral character
;; complement, the former commits to the memory and its arithmetics,
;; while the latter's utility is restricted to the communication along
;; the input and output conduits.
;; 
;; == UNSIGNED INTEGERS: THE PREMIER DATA ITEMS ==
;; Its role in the program memory already intimates the unsigned byte
;; species' excellent impact, its gamut covering the extent of the
;; integer interval [0, 255].
;; 
;; == ASCII CHARACTERS: TOKENS OF COMMUNICATION ==
;; The paravail object of deliberation, the character type, appropriates
;; its sole deployment in the form of the ASCII repertoire, operating
;; on the input and output communication channels.
;; 
;; 
;; Instructions
;; ============
;; brainfuck's operational competences enumerate an octuple membership,
;; the ostensible parvipotence encumbering this tally serves to bewray
;; the the nimiety appertaining to its potentials, as the coefficiency
;; of its memory, the basic arithmetics, the input and output
;; infrastructure's provisions, and the jump-based control flow
;; mechanism impart upon it veridical Turing-completeness.
;; 
;; == OVERVIEW ==
;; An apercu shall serve in a cursory species of nortelry's adhibition
;; concerning the available instructions commorant in brainfuck:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one.
;;           | If the new value exceeds the upper bourne of 255, the
;;           | cell state is reset to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;           | If the new value descends below the lower bourne of zero
;;           | (0), the cell state is reset to the maximum of 255.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code equals the current
;;           | cell value to the standard output.
;;   ..................................................................
;;   ,       | Queries the standard input for an ASCII character and
;;           | stores its character code in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]"; otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "["; otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre maturity being a tenable imputation in regards to the
;; language's senescence, an account of unresolved inquisitions retain
;; their woning in brainfuck's specification, a subset therefrom shall
;; receive the following sections' perquisitions.
;; 
;; == WHAT CONCRETE MEMORY SPECIFICATIONS EXERCISE THEIR PURVIEW? ==
;; Urban Mueller's original brainfuck implementation expressed itself in
;; a particular program memory accommodation, enumerating 30,000 cells
;; of unsigned byte capacity each. An adherence's dever is, however,
;; neither declaimed nor dismissed.
;; 
;; Several different interpretations are allotted to the quesited
;; property of the memory, frequently imposing inconcinnities and
;; incongruencies betwixt implementations, without an explicit
;; disqualification's infliction, tallying variations upon the cell
;; count towards a bilateral infinity, as well as the admission of
;; signed octet or even integer gamuts for the cells.
;; 
;; It has been adjudged to resort to a prescription of an infinite
;; extent of cells, its apertures empighted on both lateralities, where
;; every cell acquires the castaldy over its personal unsigned byte
;; value.
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp applies itself to an input
;; brainfuck source code's reformulation as an abstract syntax tree
;; (AST), conducted in its pursuit by a shift-reduce parser. The
;; deliberately accepted avoirdupois encumbering the supererogative
;; nimiety invested into such a simple programming language as brainfuck
;; bears the auspice of its vindication by the didascalic fruition.
;; 
;; == SHIFT-REDUCE PARSING: BUILDING AN AST ON A STACK ==
;; Maugre the shift-reduce parsing concept's peisant involvement in this
;; project, its treatise's circumference would be a subject too nimious
;; in its cumbrance by convolutions in this locality, whence issues the
;; relocation of its exact nature to the "Appendices" section, the same
;; please consult. The following curtailed ilk of explication shall
;; accoutre the reader with a foundational acquaintance with the
;; subject only.
;; 
;; A shift-reduce parser constitutes a bottom-up parsing technique, its
;; realm accommodated to the ultimate desideratum's construction, a
;; processed program's abstract syntax tree representation as a
;; hierarchy of nodes, each such a utilized facility from the source
;; language.
;; 
;; The parser employs a triad of components: the input buffer, a stack,
;; and a set of zero or more production rules.
;; 
;; The input buffer's wike is delineated by a provision of the analyzed
;; source code's tokens. Its inquisition establishes the parsing's
;; incipiency, while its exhaustion, as an equipoise, serves as its
;; conclusion.
;; 
;; The stack maintains a last-in-first-out collection of tokens and
;; abstract syntax tree nodes. Its reception of an input buffer token
;; and the subsequent insertion unto the stack top realizes the "shift"
;; stage. Ensuing from this point, the stack repeatedly perquires the
;; production rules in a stage norned the "reduce" step, contingently
;; substituting its top elements by the matching rules' outputs until a
;; failure to accommodate such service redirects its attention to the
;; iterum "shift" and potentially following next "reduce" activities.
;; 
;; The production rules define a mapping of a produce from a particular
;; expression sequence's satisfaction. The produce is known as the
;; "antecedent" or the "left-hand side" (LHS), based upon its location
;; in the formal exposition; the expression sequence installing the
;; docimasy's predicate is often nevened the "handle" or the
;; "right-hand side" (RHS). The official nomenclature assigns a format
;; as this to a production rule:
;; 
;;   antecedent -> handle
;; 
;; As an example, an addition operation may be expressed in this
;; fashion:
;; 
;;   addition -> expression "+" expression
;; 
;; The antecedent "addition", such can be a binary operation node in the
;; AST, is generated from this rule if two expressions, concatenated via
;; a plus token ("+"), may be provided.
;; 
;; At the parsing process' inchoation, the "shift" phase obtains from
;; the input buffer the first token and pushes it unto the stack. As an
;; act of respondency, the stack queries its production rules' handles,
;; presenting to these candidates its top stack elements --- in the
;; incipient iteration a single specimen, later more potent in their
;; quantity ---, the most eligible among these, if any extant, is
;; selected. The stack's top elements requisite to the rule handle's
;; application are popped, and the returned antecedent is pushed back
;; unto the stack.
;; 
;; Given an exemplary multiplication production rule
;; 
;;   multiplication -> integer "*" expression
;; 
;; if the stack comprehends
;; 
;;    top    >  | expression     |   <- matches production rule handle
;;              | "*"            |   <- matches production rule handle
;;              | integer        |   <- matches production rule handle
;;              | .............. |
;;              | .............. |
;;              | .............. |
;;    bottom >  |________________|
;; 
;; The reversed top stack elements comport to the rule; as a corollary,
;; the triad is popped and substituted by the rule handle's antecedent,
;; the "multiplication" node:
;; 
;;    top    >  | multiplication |   <- production rule antecedent
;;              | ...............|
;;              | ...............|
;;              | .............. |
;;    bottom >  |________________|
;; 
;; A failure to locate further matching rules concludes this iteration's
;; reduction step.
;; 
;; While the input buffer provides more tokens, the shift and reduce
;; activities are perpetuated; upon the buffer's exhaustion, one of the
;; "success" or "error" stages is assumed.
;; 
;; The "success" stage's occupancy depends upon the stack comprehending
;; a single object, which concomitantly concords with a specially
;; designated start symbol --- commonly the abstract syntax tree's
;; expected root node. This desinent state's chevisance proves the
;; parsing as successfully completed.
;; 
;; The obverse final state, "error", emerges in its transpiration upon
;; the stack content either being incompatible in its cardinality, by
;; ensconcing more than one element, and/or failing to exhibit the start
;; symbol as its aefauld member, which signifies an abortive parsing.
;; 
;; Proceeding from this cursory administration of gnarity, that aspects
;; more trenchant to the project shall be communicated in the sections
;; to come.
;; 
;; == EXPRESSIONS: ENCAPSULATIONS OF TOKENS AND NODES ON THE STACK ==
;; The heterogeneity autochthonous to the shift-reduce parser's
;; confluence of the actually disparage token and AST node species as
;; contemporaneous compeers on the stack experiences an alleviation's
;; adhibition through their ensconcement in "Expression" objects.
;; 
;; These "Expression" instances receive a categorizing type in
;; conjunction with their actual payload, the token, AST node, or a list
;; maintaining the latter. A type specification of "increment", as a
;; forbisen, designates the brainfuck "+" token on the stack, whereas
;; the "node" identifier appertains to any of the available node
;; subclasses, such as "Increment-Node". Expressions may be utilized for
;; adminicular or temporary purposes as a secondary emolumental facet
;; desumed from their participation; a list of several nodes, as a
;; concrete use case adduced, is denoted via the signum "node-list", ere
;; its devourment by either a loop or its integration into the root
;; program node.
;; 
;; == PRODUCTION RULES: FUNCTIONS FROM EXPRESSIONS TO EXPRESSIONS ==
;; The regulative institution of the shift-reduce parser implementation,
;; the production rules, comprehends a list of approximately cognominant
;; "Production-Rule" instances.
;; 
;; Each example from this class comprehends two parcels of information:
;; the pattern, which expresses in a list of keyword symbols the
;; "Expression" types in their content, tally and order whence ensues a
;; recognition of this rule, and the handle, a function from an
;; equinumerant variadic input "Expression" argument list to a new
;; "Expression" instance.
;; 
;; == PRODUCTIONS RULE DETAILS ==
;; The incipient effort shall be an introduction's dation, enumerating
;; the various token, expression, and abstract syntax tree (AST) node
;; types whose participation conduces the parsing's ultimate
;; capacitation. An incession from this establishment, these elements'
;; interaction is indagated, first in a less formal tabular exposition,
;; and, in its desinence, as an equivalent formulation of elevated
;; stringency in discipline and diction.
;; 
;; == CONSTITUENTS: TOKENS, EXPRESSIONS, AND NODES ==
;; In the transformations' context that apply to the shift-reduce
;; parsing activities, a series of grammatical constituents ought to
;; enjoy their diorisms. These elements, to whom an involvement in any
;; of the architectural entities' twissel --- input buffer and stack ---
;; is apportioned, may experience a modulation in dependence upon the
;; shifting and/or reduction stage.
;; 
;;   ------------------------------------------------------------------
;;   Expr. type  | Description
;;   ------------+-----------------------------------------------------
;;   increment   | The "+" token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   decrement   | The "-" token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   moveLeft    | The "<" token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   moveRight   | The ">" token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   input       | The "," token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   output      | The "." token queried from the input buffer in its
;;               | verbatim form, ere a conversion into a "nodeList".
;;   ..................................................................
;;   jumpForward | The "[" token queried from the input buffer in its
;;               | verbatim form, ere its coalescence into a "loop"
;;               | node in conjunction with a matching "]" token.
;;   ..................................................................
;;   jumpBack    | The "]" token queried from the input buffer in its
;;               | verbatim form, ere its coalescence into a "loop"
;;               | node in conjunction with a preceding "[" token.
;;   ..................................................................
;;   node        | A parsed command token.
;;   ..................................................................
;;   nodeList    | A list composed of one or more "node" objects.
;;   ..................................................................
;;   program     | A program composed of zero or more nodes.
;;   ..................................................................
;;   eof         | The end-of-file token, a sentinel produced by the
;;               | input buffer upon its source's exhaustion.
;;   ------------------------------------------------------------------
;; 
;; == TOKEN, EXPRESSION, AND NODE TRANSFORMATIONS ==
;; The reduction stage, with its cynosure airted towards the
;; transformation applicable to the production rules, shall be the
;; following tabular illustration's cynosure. In this context, the
;; sinistral compartment maintains a rule handle's pattern, concomitant
;; to the dextral moeity's contribution of the result, its antecedent,
;; issuing from the former's evaluation.
;; 
;;   ------------------------------------------------------------------
;;   Handle pattern                  | Antecedent (rule output)
;;   --------------------------------+---------------------------------
;;   increment                       | node (increment node)
;;   ..................................................................
;;   decrement                       | node (decrement node)
;;   ..................................................................
;;   moveRight                       | node (move right AST node)
;;   ..................................................................
;;   moveLeft                        | node (move left AST node)
;;   ..................................................................
;;   input                           | node (input AST node)
;;   ..................................................................
;;   output                          | node (outpu AST node)
;;   ..................................................................
;;   jumpForward, jumpBack           | node (loop AST node)
;;   ..................................................................
;;   jumpForward, node, jumpBack     | node (loop AST node)
;;   ..................................................................
;;   jumpForward, nodeList, jumpBack | node (loop AST node)
;;   ..................................................................
;;   node, node                      | nodeList (list of AST nodes)
;;   ..................................................................
;;   nodeList, node                  | nodeList (list of AST nodes)
;;   ..................................................................
;;   nodeList, nodeList              | nodeList (list of AST nodes)
;;   ..................................................................
;;   EOF                             | program (empty program AST node)
;;   ..................................................................
;;   node, EOF                       | program (single progr. AST node)
;;   ..................................................................
;;   nodeList, EOF                   | program (multi. progr. AST node)
;;   ..................................................................
;;   program, EOF                    | program (identity)
;;   ------------------------------------------------------------------
;; 
;; == THE PRODUCTION RULE SET ==
;; A formal description of the production rules shall furnish the
;; separately introduced constituents' and the tabular ligation's
;; confluence:
;; 
;;   node     -> "+"
;;   node     -> "-"
;;   node     -> "<"
;;   node     -> ">"
;;   node     -> "."
;;   node     -> ","
;;   
;;   node     -> "[" "]"
;;   node     -> "[" node "]"
;;   node     -> "[" nodeList "]"
;;   
;;   nodeList -> node node
;;   nodeList -> nodeList node
;;   nodeList -> nodeList nodeList
;;   
;;   program  -> EOF
;;   program  -> node EOF
;;   program  -> nodeList EOF
;; 
;; 
;; Appendices
;; ==========
;; The following sections shall impart a comprehensive mete of nortelry
;; concerning various subjects cognate to this project, yet too remote
;; in its ultimate involvement to vindicate immediate inclusion in the
;; primary treatise's spatiality.
;; 
;; == APPENDIX A: SHIFT-REDUCE PARSING ==
;; The shift-reduce parser embraces the concept of bottom-up parsing,
;; realized through the efforts of a componency the same intrines an
;; input buffer for the provision of the programming language's tokens,
;; a stack endowed with the competence to assemble tokens and abstract
;; syntax tree (AST) nodes into a coherent unity, and a set of
;; production rules the conformance to whom conducts the stack's
;; assemblage.
;; 
;; == SHIFT-REDUCE PARSING: A BOTTOM-UP TECHNIQUE ==
;; Proceeding from the notion of the parsing enterprise's telos being an
;; abstract syntax tree (AST) representation of the evaluated program,
;; a tree-based structure whose nodes encapsulate the involved language
;; facilities, the shift-reduce parser, comporting to the bottom-up
;; operation order, assembles the hierarchy in an ascending airt, its
;; construction commencing from the leaves towards the root.
;; 
;; Adducing a forbisen, the arithmetic expression
;; 
;;   4 + 7
;; 
;; would construct the treble of AST nodes (4), (+), and (7), which, in
;; more pleasing graphical illustration, combine into the design
;; 
;;       (+)
;;        |
;;    +---+---+
;;    |       |
;;   (4)     (7)
;; 
;; A bottom-up parser, such as the shift-reduce specimen, would
;; recognize the terminal nodes (4) and (7) in its inchoation, and the
;; next higher level node (+) at the desinence.
;; 
;; A top-down counterpart, on the other hand, produces the root node
;; (+), ere its appendage of the lower elements (4) and (7).
;; 
;; == THE INPUT BUFFER: A TOKEN SUPPLIER ==
;; The ingress from the source program to the parser system manifests in
;; the input buffer, such that accepts the subject language's code in a
;; string form in a pursuit to detect, extract, and return the
;; significant objects, its tokens. Most commonly, the minimum amount of
;; information transferred in an object of this class amplects the
;; categorizing type and the detailing value.
;; 
;; == THE STACK: THE ABSTRACT SYNTAX TREE ASSEMBLER ==
;; The wike consumed by the effort of combining the tokens into an
;; abstract syntax tree (AST) hierarchy is delegated to the stack.
;; Administered the presentation of tokens, as well as AST nodes
;; begotten from already invested efforts from its personal
;; contribution, and mediated through the production rules, which please
;; sojourn alow, this storage gradually generates and assembles the
;; tree's components.
;; 
;; Conplying with a throughout eath diction, the stack's top elements
;; are juxtaposed with the available rules, the eligible one, if any
;; extant, consumes the requisite tally among these candidates, and
;; pushes its appertaining node unto the stack.
;; 
;; == PRODUCTION RULES: REGULATIONS FOR THE AST ASSEMBLAGE ==
;; The source language's donat ought to conform to a formulation molded
;; into one or more production rules, each such entity a tuple of an
;; antecedent, or left-hand side (LHS), and a handle, also nevened the
;; consequent, or the right-hand side (RHS).
;; 
;; A rule's general format adheres to the following norm:
;; 
;;   antecedent -> handle
;; 
;; The handle's role encompasses the diorism of tokens or nodes whose
;; combination admits to the rule's recognition. The antecedent, its
;; causatum, establishes the result of a rule's application unto the
;; stack's top elements, equinumerant its their appropration from the
;; salvatory to the handle's constituents.
;; 
;; The rule, for instance, limned by the specification
;; 
;;   addition -> number "+" expression
;; 
;; bears the twain of general components
;; 
;;   ----------------------------------
;;   Part       | Content
;;   -----------+----------------------
;;   Antecedent | addition
;;   ..................................
;;   Handle     | number "+" expression
;;   ----------------------------------
;; 
;; The handle imposes the existency of a number, the plus ("+") token,
;; and an expression on the stack, relating to its three topmost items
;; as indagated from lowest to the highest.
;; 
;; The exemplary case shall be developed in further detail by its
;; relationship to the stack's illustration:
;; 
;;   top    > | Node(expression) | <---- expression
;;            | Token("+")       | <---- "+"
;;            | Node(number)     | <---- number
;;   bottom > | Token("xyz")     |
;;            +------------------+
;;                   Stack               Handle
;; 
;; == SHIFT, REDUCE, SUCCESS, AND EROR: THE POSSIBLE PARSING STAGES ==
;; The two premier stages, "shift" and "reduce", as well as the
;; parhedral states "success" and "error", the desinent twissel imposing
;; alternatives, shall be enumerated alow:
;; 
;;   (a) Shift: Pushing an input buffer token to the stack.
;;       In the "shift" phase's dever subsumes the inquisition of the
;;       input buffer for the next token, the same is obtained and
;;       pushed unto the stack.
;;       This state constitutes the parser's introduction.
;;   
;;   (b) Reduce: Replacing the stack top by production rule results.
;;       A far more convoluted endeavor, the reduce stage's amplectation
;;       enumerates the application of the matching production rules
;;       until none is capacitated to express its eligibility, which
;;       either segues into the next shift or any of the success/error
;;       stages.
;;       The reduction proceeds by searching for a production rule
;;       whose handle matches the top stack elements, the tally of these
;;       candidates equinumerant to the handle's constituent count, and
;;       mandated to be form an immediate succession in the storage.
;;       It is of utmost significance to stress that the stack elements'
;;       arrangement reflects a perfect reversal of their probing order,
;;       which means that the last handle component must match the top
;;       stack element, and the first handle item the lowest candidate
;;       on the stack. That is, given a production rule
;;         antecedent -> handleComp[1], ..., handleComp[n]
;;       the stack must be fathomed in the order
;;         stack[top    ] => handleComp[1]
;;         stack[top - 1] => handleComp[2]
;;         ...
;;         stack[top - n] => handleComp[n]
;;       In a tabular format, the following equiparation is imposed:
;;         ---------------------------------
;;         Handle component | Stack element
;;         -----------------+---------------
;;         handleComp[1]    | stack[top]
;;         .................................
;;         handleComp[2]    | stack[top - 1]
;;         .................................
;;         handleComp[i]    | stack[top - i]
;;         .................................
;;         handleComp[n]    | stack[top - n]
;;         ---------------------------------
;;       If a production rule's handle matches, the respective top stack
;;       element sequence is removed, or "popped" from the stack, and
;;       the rule's antecedent is pushed unto the same. In the usual
;;       case, this new member constitutes an abstract syntax tree node.
;;       
;;       The process is repeated until no matching production can be
;;       detected anymore, whence the circumstances a furcation:
;;         
;;         - If the input buffer is not yet exhausted, another shift
;;           applies, requesting the next token from the buffer and
;;           pushing it unto the stack, which please see above.
;;         - If the input buffer is exhausted, the success or error
;;           stage is sojourned, which please perquire below.
;; 
;; While the shift and reduce activities obey a cyclic administration,
;; repeating until the input buffer's exhaustion, the accept and error
;; actions are imparted with a singular, competing exposure, applied
;; immediately as a consequence of the former twain's patration:
;; 
;;   (c) Success: The start symbol as the stack's sole resident.
;;       The parsing process, its cessation declaimed by the input
;;       buffer's completion, must, in order to enjoy a success in its
;;       supputation, demonstrate the stack as a singleton storage,
;;       composed of a single element only, this being the designated
;;       start symbol. For an abstract syntax tree, this commonly
;;       resolves to the root node's commorancy as the aefauld member on
;;       the stack.
;;   
;;   (d) Error: An anomalous stack remains.
;;       If the above implication, the "success" predicament, does not
;;       hold, as the stack either comprehends too many elements, or a
;;       single object of incompatible haecceity to the expected start
;;       symbol, an error occurs, which deems the parsing process a
;;       failure.
;; 
;; == SHIFT-REDUCE PARSING: PUSH TOKEN, CHOOSE RULE, BUILD NODE ==
;; A pseudocode formulation of the shift-reduce parsing concept shall
;; be offered for deliberation:
;; 
;;   { Queries the next token from the inputBuffer and pushes the same }
;;   { unto the stack.                                                 }
;;   procedure shift (inputBuffer, stack)
;;     Input:
;;       inputBuffer: The unit responsible for generating tokens from
;;                    a piece of source code.
;;       stack:       The stack comprehending the tokens and AST nodes.
;;     
;;     Output:
;;       None.
;;     
;;     Perform:
;;       let token <- get next token from inputBuffer
;;       
;;       push token unto stack
;;   end procedure
;;   
;;   
;;   { Searches for the first rule among the productionRules which }
;;   { matches the stack's top elements and returns the same.      }
;;   { Otherwise returns the "nil" sentinel.                       }
;;   function findEligibleProductionRule (stack, productionRules)
;;     Input:
;;       stack:           The stack comprehending the tokens and
;;                        AST nodes.
;;       productionRules: The set of specified production rules.
;;     
;;     Output:
;;       matchingRule:    The first matching rule among the
;;                        productionRules, or "nil" if none such could
;;                        be ascertained.
;;     
;;     Perform:
;;       let matchingRule <- nil
;;       
;;       for rule in productionRules do
;;         if rule matches top stack elements do
;;           matchingRule <- rule
;;           terminate loop
;;         end if
;;       end for
;;       
;;       return matchingRule
;;   end function
;;   
;;   
;;   { Repeatedly replaces the stack's top elements by the matching    }
;;   { rule among the productionRules until none such can be detected. }
;;   procedure reduce (stack, productionRules)
;;     Input:
;;       stack:           The stack comprehending the tokens and
;;                        AST nodes.
;;       productionRules: The set of specified production rules.
;;     
;;     Output:
;;       None.
;;     
;;     Perform:
;;       repeat
;;         let matchingRule <- findEligibleProductionRule(stack,
;;                                                      productionRules)
;;         
;;         if matchingRule != nil do
;;           let handle      <- handle(matchingRule)
;;           let handleSize  <- size(handle)
;;           let topElements <- pop the handleSize top elements from
;;                              the stack
;;           let antecedent  <- apply matchingRule handle to topElements
;;           
;;           push antecedent unto stack
;;         else if
;;           terminate loop
;;         end if
;;       end repeat
;;   end procedure
;;   
;;   
;;   { Determines whether the stack at its current state may be        }
;;   { considered as comprehending a valid abstract syntax tree (AST), }
;;   { which presupposes the existence of the startSymbol as its sole  }
;;   { element, returning on confirmation the Boolean "true" value,    }
;;   { otherwise "false".                                              }
;;   function hasSucceeded (stack, startSymbol)
;;     Input:
;;       stack:        The stack comprehending the tokens and AST nodes.
;;       startSymbol:  The identifier whose sole occurrency on the stack
;;                     designates a successful parsing.
;;     
;;     Output:
;;       isSuccessful: A Boolean value, resolving to "true" if the
;;                     parsing has succeeded, otherwise "false".
;;     
;;     Perform:
;;       let isSuccessful     <- false
;;       let isSingletonStack <- size(stack) = 1
;;       
;;       isSuccessful <- isSingletonStack
;;                       and (stack.peek() matches startSymbol)
;;       
;;       return isSuccessful
;;   end function
;;   
;;   
;;   { Executes several shift and reduce operations, nurtured by the   }
;;   { inputBuffer's tokens and helmed in its abstract syntax tree's   }
;;   { construction by the productionRules' manuduction, until the     }
;;   { stack either contains a valid state, or the parsing has failed, }
;;   { in the former case returning the assembled abstract syntax tree }
;;   { (AST), which must constitute the stack's aefauld element, as    }
;;   { expessed by the startSymbol. Otherwise an error is signaled.    }
;;   function shiftReduceParse (inputBuffer, stack, productionRules,
;;                              startSymbol)
;;     Input:
;;       inputBuffer:     The unit responsible for generating tokens
;;                        from a piece of source code.
;;       stack:           The stack comprehending the tokens and
;;                        AST nodes.
;;       productionRules: The set of specified production rules.
;;       startSymbol:     The identifier whose sole occurrency on the
;;                        stack designates a successful parsing.
;;     
;;     Output:
;;       tree:            The assembled AST.
;;     
;;     Perform:
;;       let tree <- nil
;;       
;;       while inputBuffer is not exhausted do
;;         shift(inputBuffer, stack)
;;         reduce(stack, productionRules)
;;       end while
;;       
;;       if hasSucceeded(stack, startSymbol) then
;;         tree <- stack.peek()
;;       else
;;         signal parse failure
;;       end if
;;   end function
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-12-05
;; 
;; Sources:
;;   [ahmedh2023shiftred]
;;   Izhan Ahmed H, "Shift Reduce Parser | Compiler Design",
;;     July 4th, 2023
;;   URL: "https://medium.com/@izhan_a1/
;;         shift-reduce-parser-compiler-design-41dbf88779ba"
;;   
;;   [esolang2023brainfuck]
;;   The Esolang contributors, "brainfuck", October 17th, 2023
;;   URL: "https://esolangs.org/wiki/Brainfuck"
;;   Notes:
;;     - Describes the brainfuck programming language.
;;   
;;   [rinard2013introshiftreduce]
;;   Martin Rinard,
;;     "MIT 6.035 --- Introduction to Shift-Reduce Parsing", 2013
;;   URL: "http://cons.mit.edu/sp13/slides/S13-lecture-03.pdf"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*)
                            (size         '*))
  "The ``list-of'' type defines a list compact of a SIZE tally of
   elements, defaulting to an arbitrary account, each member of which
   conforms to the ELEMENT-TYPE, the same defaults to the generic
   sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (and (symbolp size) (eq size '*))
              (= (length (the list candidate)) size))
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype size ()
  "The ``size'' type defines a non-negative integer compatible with the
   concept of a magnitude or tally."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype expression-type ()
  "The ``expression-type'' type enumerates the recognized variation on
   expression categories that may be assumed during the shift-reduce
   parsing process."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :input
    :output
    :jump-forward
    :jump-back
    :node
    :node-list
    :program
    :eof))

;;; -------------------------------------------------------

(deftype production-pattern ()
  "The ``production-pattern'' type defines the general conformation of
   a production rule's consequent or handle, enumerating its expression
   types in the correct arrangement by adminiculum of a list entailing
   one or more ``expression-type'' representations."
  '(list-of expression-type))

;;; -------------------------------------------------------

(deftype production-handle ()
  "The ``production-handle'' type defines that moiety of a production
   rule which produces the pattern, or \"left-hand side\" (LHS), from
   a sequence of one or more parser stack elements, realized as a
   variadic function, equinumerant in its argument list size to the
   pattern, whose one or more input ``Expressions'' map to an single
   output ``Expression'', the same is pushed unto the stack."
  '(function (&rest Expression) Expression))

;;; -------------------------------------------------------

(deftype expression-list (&optional (size '*))
  "The ``expression-list'' type defines a list composed of a SIZE tally
   of ``Expression'' instances, their amount defaults to an arbitrary
   account."
  `(list-of Expression ,size))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and allies
   with a value of the VALUE-TYPE, both defaulting to the generic
   sentinel ``*''."
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

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits, and thus a commorant of the closed integral range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   unsigned byte-valued cells, amenable to signed integer indices,
   emerging as a hash table whose keys represent the cell indices as
   integer objects and are affiliated with the cell values in the form
   of ``octet'' instances."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST).                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AST-Node)
  "The ``AST-Node'' interface accoutres the fundament for all classes
   pursuing the representation of a brainfuck program's facilities in
   the guise of an abstract syntax tree (AST) leaf or subtree.")

;;; -------------------------------------------------------

(defstruct (Increment-Node
  (:include     AST-Node)
  (:constructor make-increment-node ()))
  "The ``Increment-Node'' class implements the ``AST-Node'' interface in
   order encapsulates the notion of the brainfuck increment operation
   \"+\" as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Decrement-Node
  (:include     AST-Node)
  (:constructor make-decrement-node ()))
  "The ``Decrement-Node'' class implements the ``AST-Node'' interface in
   order encapsulates the notion of the brainfuck decrement operation
   \"-\" as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Move-Right-Node
  (:include     AST-Node)
  (:constructor make-move-right-node ()))
  "The ``Move-Right-Node'' class implements the ``AST-Node'' interface
   in order to encapsulate the notion of the brainfuck dextral cell
   pointer translation operation \">\" as an abstract syntax tree (AST)
   node.")

;;; -------------------------------------------------------

(defstruct (Move-Left-Node
  (:include     AST-Node)
  (:constructor make-move-left-node ()))
  "The ``Move-Left-Node'' class implements the ``AST-Node'' interface in
   order to encapsulate the notion of the brainfuck sinistral cell
   pointer translation operation \"<\" as an abstract syntax tree (AST)
   node.")

;;; -------------------------------------------------------

(defstruct (Input-Node
  (:include     AST-Node)
  (:constructor make-input-node ()))
  "The ``Input-Node'' class implements the ``AST-Node'' interface in
   order to encapsulate the notion of the brainfuck input operation
   \",\" as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Output-Node
  (:include     AST-Node)
  (:constructor make-output-node ()))
  "The ``Output-Node'' class implements the ``AST-Node'' interface in
   order to encapsulate the notion of the brainfuck output operation
   \".\" as an abstract syntax tree (AST) node.")

;;; -------------------------------------------------------

(defstruct (Loop-Node
  (:include     AST-Node)
  (:constructor make-loop-node (body)))
  "The ``Loop-Node'' class implements the ``AST-Node'' interface in
   order to encapsulate the notion of the brainfuck jump forward and
   back twain operation \"[\" ... \"]\" as an abstract syntax tree (AST)
   node."
  (body NIL :type (list-of AST-Node)))

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include     AST-Node)
  (:constructor make-program-node (statements)))
  "The ``Decrement-Node'' class implements the ``AST-Node'' interface in
   order to encapsulate a complete brainfuck program as an abstract
   syntax tree (AST) node."
  (statements NIL :type (list-of AST-Node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of expression.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Expression
  (:constructor make-expression (type value)))
  (type  (error "Missing type.")  :type expression-type)
  (value (error "Missing value.") :type T))

;;; -------------------------------------------------------

(defun expression-type-matches-p (expression expected-type)
  "Determines whether the EXPRESSION conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Expression      expression))
  (declare (type expression-type expected-type))
  (the boolean
    (not (null
      (eq (expression-type expression) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instruction-character-p (candidate)
  "Determines whether the CANDIDATE represents a brainfuck command
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "><+-.,[]" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input buffer.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Input-Buffer
  (:constructor make-input-buffer
    (source
     &aux (position  0)
          (character (when (array-in-bounds-p source position)
                       (char source position))))))
  (source    (error "Missing source.") :type string :read-only T)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defun input-buffer-advance (input-buffer)
  "Advances the INPUT-BUFFER's position cursor to the next character in
   its source, if possible, and returns no value."
  (declare (type Input-Buffer input-buffer))
  (setf (input-buffer-position input-buffer)
    (min (1+ (input-buffer-position input-buffer))
         (length (input-buffer-source input-buffer))))
  (setf (input-buffer-character input-buffer)
    (when (array-in-bounds-p
            (input-buffer-source   input-buffer)
            (input-buffer-position input-buffer))
      (char
        (input-buffer-source input-buffer)
        (input-buffer-position input-buffer))))
  (values))

;;; -------------------------------------------------------

(defun input-buffer-skip-comment (input-buffer)
  "Proceeding from the current position into the INPUT-BUFFER, skips a
   sequence of zero or more accolent comment characters, and returns no
   value."
  (declare (type Input-Buffer input-buffer))
  (setf (input-buffer-position input-buffer)
    (or
      (position-if
        #'instruction-character-p
        (input-buffer-source input-buffer)
        :start (input-buffer-position input-buffer))
      (length (input-buffer-source input-buffer))))
  (setf (input-buffer-character input-buffer)
    (when (array-in-bounds-p
            (input-buffer-source   input-buffer)
            (input-buffer-position input-buffer))
      (char
        (input-buffer-source input-buffer)
        (input-buffer-position input-buffer))))
  (values))

;;; -------------------------------------------------------

(defun input-buffer-exhausted-p (input-buffer)
  "Determines whether the INPUT-BUFFER is exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Input-Buffer input-buffer))
  (input-buffer-skip-comment input-buffer)
  (the boolean
    (not (null
      (>= (input-buffer-position input-buffer)
          (length (input-buffer-source input-buffer)))))))

;;; -------------------------------------------------------

(defun input-buffer-get-next-token (input-buffer)
  "Returns the next token, in the form an ``Expression'' from the
   INPUT-BUFFER."
  (declare (type Input-Buffer input-buffer))
  (input-buffer-skip-comment input-buffer)
  (the Expression
    (case (input-buffer-character input-buffer)
      ((NIL)
        (make-expression :eof NIL))
      (#\>
        (prog1
          (make-expression :move-right #\>)
          (input-buffer-advance input-buffer)))
      (#\<
        (prog1
          (make-expression :move-left #\<)
          (input-buffer-advance input-buffer)))
      (#\+
        (prog1
          (make-expression :increment #\+)
          (input-buffer-advance input-buffer)))
      (#\-
        (prog1
          (make-expression :decrement #\-)
          (input-buffer-advance input-buffer)))
      (#\.
        (prog1
          (make-expression :output #\.)
          (input-buffer-advance input-buffer)))
      (#\,
        (prog1
          (make-expression :input #\,)
          (input-buffer-advance input-buffer)))
      (#\[
        (prog1
          (make-expression :jump-forward #\[)
          (input-buffer-advance input-buffer)))
      (#\]
        (prog1
          (make-expression :jump-back #\])
          (input-buffer-advance input-buffer)))
      (otherwise
        (input-buffer-skip-comment   input-buffer)
        (input-buffer-get-next-token input-buffer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of stack.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Stack
  (:constructor make-stack ()))
  "The ``Stack'' class maintains a stack of expressions, its obtention's
   provenance usually an ``Input-Buffer'', while its own services'
   usufruction constitutes the subject of the ``Parser'' in conjunction
   with its ``Production-Rule''s."
  (elements NIL :type (list-of Expression)))

;;; -------------------------------------------------------

(defun stack-push (stack expression)
  "Pushes the EXPRESSION unto the STACK's top and returns no value."
  (declare (type Stack      stack))
  (declare (type Expression expression))
  (push expression (stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun stack-pop (stack &optional (number-of-elements 1))
  "Pops the NUMBEROF-ELEMENTS tally of topmost items from the STACK and
   returns a list comprehending these in the order of their removal,
   which duplicates the deleted section's arrangement ere this
   operation.
   ---
   If the STACK cannot accommodate the requisite amount of elements, an
   error of an unspecified type is signaled."
  (declare (type Stack stack))
  (declare (type size  number-of-elements))
  (the (list-of Expression)
    (loop repeat number-of-elements
      if (stack-elements stack)
        collect (pop (stack-elements stack))
      else do
        (error "Cannot pop from an empty stack."))))

;;; -------------------------------------------------------

(defun stack-peek (stack)
  "Returns without removing the element located at the STACK's top, or
   signals an error of an unspecified type upon its vacancy."
  (declare (type Stack stack))
  (the T
    (or (first (stack-elements stack))
        (error "Cannot peek into an empty stack."))))

;;; -------------------------------------------------------

(defun stack-size (stack)
  "Returns the number of elements on the STACK."
  (declare (type Stack stack))
  (the size
    (length (stack-elements stack))))

;;; -------------------------------------------------------

(defun stack-matches-pattern-p (stack probed-types)
  "Determines whether the STACK's top elements match the PROBED-TYPES
   in their tally, type, and arrangement, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack              stack))
  (declare (type production-pattern probed-types))
  (the boolean
    (not (null
      (and
        (>= (stack-size stack)
            (length probed-types))
        (loop
          for stack-element
            of-type Expression
            in      (stack-elements stack)
          for probed-type
            of-type expression-type
            in      (reverse probed-types)
          always
            (expression-type-matches-p stack-element probed-type)))))))

;;; -------------------------------------------------------

(defun singleton-stack-with-p (stack expected-expression-type)
  "Determines whether the STACK comprehends exactly one elements, this
   being an expression whose type matches the EXPECTED-EXPRESSION-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Stack           stack))
  (declare (type expression-type expected-expression-type))
  (the boolean
    (not (null
      (and
        (= (stack-size stack) 1)
        (expression-type-matches-p
          (first (stack-elements stack))
          expected-expression-type))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of production rule.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Production-Rule
  (:constructor make-production-rule (pattern handle)))
  "The ``Production-Rule'' class encapsulates the concept of a
   production rule, or simply \"production\", composed of a twissel of
   constituents, namely, the pattern, which describes the ``Expression''
   types of the handle, also known as the \"consequent\" or the
   \"right-hand side\" (RHS), and handle itself as a function whose
   arity complies with the pattern length, and to whom is assigned the
   wike of converting the top stack elements matching the pattern to the
   production rule's antecedent, or \"left-hand side\" (LHS), which must
   subsequently be pushed unto the stack in lieu of the removed and thus
   transformed items."
  (pattern (error "Missing pattern.") :type production-pattern)
  (handle  (error "Missing handle.")  :type production-handle))

;;; -------------------------------------------------------

(defun production-rule-can-apply-p (rule stack)
  "Determines whether the production RULE can be applied to the STACK's
   top elements, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Production-Rule rule))
  (declare (type Stack           stack))
  (the boolean
    (stack-matches-pattern-p stack
      (production-rule-pattern rule))))

;;; -------------------------------------------------------

(defun production-rule-apply (rule stack)
  "Applies the production RULE to the STACK, removing the number of
   elements requisite for its operation from the STACK's top, pushing
   the RULE's output expression unto the same, and returns no value."
  (declare (type Production-Rule rule))
  (declare (type Stack           stack))
  (stack-push stack
    (apply (production-rule-handle rule)
      (reverse
        (stack-pop stack
          (length
            (production-rule-pattern rule))))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (input-buffer)))
  "The ``Parser'' class furnishes a parser from the realm of the
   shift-reducer concept, founded upon a componency's triad, namely, the
   input buffer, a stack, and a set of production rules, the first among
   which provides the brainfuck source code tokens, which the second
   assembles into an abstract syntax tree (AST) by mediation of the
   third constituent's rules."
  (input-buffer (error "Missing input buffer.")
                :type      Input-Buffer
                :read-only T)
  (stack        (make-stack)
                :type      Stack
                :read-only T)
  (productions  NIL
                :type      (list-of Production-Rule)
                :read-only NIL))

;;; -------------------------------------------------------

(defun parser-define-production-rule (parser production-rule)
  "Adds the PRODUCTION-RULE to the PARSER and returns no value."
  (declare (type Parser          parser))
  (declare (type Production-Rule production-rule))
  (setf (parser-productions parser)
    (append
      (parser-productions parser)
      (list production-rule)))
  (values))

;;; -------------------------------------------------------

(defun parser-shift (parser)
  "Accompasses the \"shift\" step by consuming the next expression from
   the PARSER's internally managed input buffer and pushing the same
   unto its stack, and returns no value."
  (declare (type Parser parser))
  (stack-push
    (parser-stack parser)
    (input-buffer-get-next-token
      (parser-input-buffer parser)))
  (values))

;;; -------------------------------------------------------

(defun parser-find-applicable-production (parser)
  "Given PARSER's current stack state, searches for an eligible
   production rule in its productions list, upon success returning the
   first matching rule, otherwise responding with the ``NIL'' value.
   ---
   The production rules are probed in the order of their registration at
   the PARSER."
  (declare (type Parser parser))
  (flet ((production-rule-matches-p (pattern)
          "Determines whether the production pattern can be applied
           to the STACK, returning on confirmation a ``boolean'' value
           of ``T'', otherwise ``NIL''."
          (declare (type production-pattern pattern))
          (the boolean
            (not (null
              (stack-matches-pattern-p
                (parser-stack parser)
                pattern))))))
    (the (or null Production-Rule)
      (find-if #'production-rule-matches-p
        (parser-productions parser)
        :key #'production-rule-pattern))))

;;; -------------------------------------------------------

(defun parser-reduce (parser)
  "Repeatedly applies the reduction step while one of the PARSER's
   production rules is capacited its application unto the stack, and
   returns no value."
  (declare (type Parser parser))
  (loop
    for applicable-production
      of-type (or null Production-Rule)
      =       (parser-find-applicable-production parser)
    while applicable-production
    do (production-rule-apply applicable-production
         (parser-stack parser)))
  (values))

;;; -------------------------------------------------------

(defun parser-halted-p (parser)
  "Determines whether the PARSER's operations, that is, the shift and
   reduce steps, have completed, which is the case of an exhausted input
   buffer, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   Please heed that
     (a) This stage's confirmation precedes the PARSER's perquisition
         for a success or failure.
     (b) This operation's result does not serve to confirm the success
         or failure of the parsing process."
  (declare (type Parser parser))
  (the boolean
    (not (null
      (input-buffer-exhausted-p
        (parser-input-buffer parser))))))

;;; -------------------------------------------------------

(defun assign-production-rules (parser)
  "Generates the default production rules for the brainfuck programming
   language's procession, assigns the same to the PARSER, and returns no
   value."
  (declare (type Parser parser))
  
  ;; A "+" token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:increment)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-increment-node))))))
  
  ;; A "-" token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:decrement)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-decrement-node))))))
  
  ;; A ">" token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:move-right)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-move-right-node))))))
  
  ;; A "<" token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:move-left)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-move-left-node))))))
  
  ;; A "," token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:input)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-input-node))))))
  
  ;; A "." token produces a node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:output)
      #'(lambda (token)
          (declare (type Expression token))
          (declare (ignore          token))
          (the Expression
            (make-expression :node
              (make-output-node))))))
  
  ;; Two nodes in succession produce a node list.
  (parser-define-production-rule parser
    (make-production-rule
      '(:node :node)
      #'(lambda (first-node second-node)
          (declare (type Expression first-node))
          (declare (type Expression second-node))
          (the Expression
            (make-expression :node-list
              (list
                (expression-value first-node)
                (expression-value second-node)))))))
  
  ;; A node list succeeded by a node combines into a new node list.
  (parser-define-production-rule parser
    (make-production-rule
      '(:node-list :node)
      #'(lambda (node-list node)
          (declare (type Expression node-list))
          (declare (type Expression node))
          (the Expression
            (make-expression :node-list
              (append
                (expression-value node-list)
                (list (expression-value node))))))))
  
  ;; Two node lists in succession are coalesced into a single node list.
  (parser-define-production-rule parser
    (make-production-rule
      '(:node-list :node-list)
      #'(lambda (first-node-list second-node-list)
          (declare (type Expression first-node-list))
          (declare (type Expression second-node-list))
          (the Expression
            (make-expression :node-list
              (append
                (expression-value first-node-list)
                (expression-value second-node-list)))))))
  
  ;; An empty loop produces a single node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:jump-forward :jump-back)
      #'(lambda (loop-start loop-end)
          (declare (type Expression loop-start))
          (declare (ignore          loop-start))
          (declare (type Expression loop-end))
          (declare (ignore          loop-end))
          (the Expression
            (make-expression :node
              (make-loop-node NIL))))))
  
  ;; A singleton loop produces a single node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:jump-forward :node :jump-back)
      #'(lambda (loop-start loop-body loop-end)
          (declare (type Expression loop-start))
          (declare (ignore          loop-start))
          (declare (type Expression loop-body))
          (declare (type Expression loop-end))
          (declare (ignore          loop-end))
          (the Expression
            (make-expression :node
              (make-loop-node
                (list (expression-value loop-body))))))))
  
  ;; A non-empty loop produces a single node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:jump-forward :node-list :jump-back)
      #'(lambda (loop-start loop-body loop-end)
          (declare (type Expression loop-start))
          (declare (ignore          loop-start))
          (declare (type Expression loop-body))
          (declare (type Expression loop-end))
          (declare (ignore          loop-end))
          (the Expression
            (make-expression :node
              (make-loop-node
                (expression-value loop-body)))))))
  
  ;; A concluding single-instruction command yields a program node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:node :eof)
      #'(lambda (node end-of-file)
          (declare (type Expression node))
          (declare (type Expression end-of-file))
          (declare (ignore          end-of-file))
          (the Expression
            (make-expression :program
              (make-program-node
                (list (expression-value node))))))))
  
  ;; A concluding node list yields a program node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:node-list :eof)
      #'(lambda (node-list end-of-file)
          (declare (type Expression node-list))
          (declare (type Expression end-of-file))
          (declare (ignore          end-of-file))
          (the Expression
            (make-expression :program
              (make-program-node
                (expression-value node-list)))))))
  
  ;; The empty program yields a valid program node.
  (parser-define-production-rule parser
    (make-production-rule
      '(:eof)
      #'(lambda (end-of-file)
          (declare (type Expression end-of-file))
          (declare (ignore          end-of-file))
          (the Expression
            (make-expression :program
              (make-program-node NIL))))))
  
  (values))

;;; -------------------------------------------------------

(defun parser-succeed-or-fail (parser)
  "Determines whether the PARSER has succeeded or failed, the former
   case's emergence depends upon the PARSER stack comprehending an
   aefauld element, an expression of the ``:program'' type, whose value,
   a ``Program-Node'', is subsequently returned; otherwise the parsing
   has failed, and an error of an unspecified type is signaled."
  (declare (type Parser parser))
  (the AST-Node
    (if (singleton-stack-with-p (parser-stack parser) :program)
      (expression-value
        (stack-peek
          (parser-stack parser)))
      (error "The parser stack could not be reduced a singular ~
              \":program\" expression: ~s"
        (parser-stack parser)))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Applies the shift-reduce PARSER to its input buffer and returns the
   thus produced abstract syntax tree (AST)."
  (declare (type Parser parser))
  
  ;; Repeated shift and reduce until the input buffer is exhausted.
  (loop
    until (parser-halted-p parser) do
      (parser-shift  parser)
      (parser-reduce parser)
    finally
      (parser-shift  parser)
      (parser-reduce parser))
  
  ;; Determine whether the parser's stack comprehends exactly one
  ;; element, the same being an expression of the ``:program'' type.
  ;; Upon confirmation, the parsing has succeeded and the program
  ;; expression's value, a ``Program-Node'' is returned.
  ;; Otherwise the process has failed because of a syntactical anomaly,
  ;; which instigates an error signaling.
  (the AST-Node
    (parser-succeed-or-fail parser)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (tree)))
  "The ``Interpreter'' class constitutes that wike's recepient which
   compels its imbution of actual effect to a brainfuck program provided
   as an abstract syntax tree (AST)."
  (tree         (error "Missing AST.")        :type Program-Node)
  (memory       (make-hash-table :test #'eql) :type memory)
  (cell-pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun current-memory-cell (interpreter)
  "Returns the byte value stored in the INTERPRETER memory's current
   cell."
  (declare (type Interpreter interpreter))
  (the octet
    (gethash
      (interpreter-cell-pointer interpreter)
      (interpreter-memory       interpreter)
      0)))

;;; -------------------------------------------------------

(defun (setf current-memory-cell) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER memory's current cell,
   contingently preceded by a wrapping around of its value into the
   valid unsigned byte range [0, 255], and returns no value."
  (declare (type Interpreter interpreter))
  (setf
    (gethash
      (interpreter-cell-pointer interpreter)
      (interpreter-memory       interpreter)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defgeneric visit-node (interpreter node)
  (:documentation
    "Evaluates the abstract syntax tree (AST) node in the INTERPRETER's
     context and returns no value."))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Decrement-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Decrement-Node node))
  (declare (ignore              node))
  (decf (current-memory-cell interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Increment-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Increment-Node node))
  (declare (ignore              node))
  (incf (current-memory-cell interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Input-Node))
  (declare (type Interpreter interpreter))
  (declare (type Input-Node  node))
  (declare (ignore           node))
  (format T "~&>> ")
  (finish-output)
  (setf (current-memory-cell interpreter)
    (char-code
      (read-char)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Loop-Node))
  (declare (type Interpreter interpreter))
  (declare (type Loop-Node   node))
  (flet ((current-cell-zero-p ()
          "Determines whether the INTERPRETER memory's current cell
           value equals zero (0), returning on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (the boolean
            (not (null
              (zerop
                (current-memory-cell interpreter)))))))
    (loop until (current-cell-zero-p) do
      (dolist (statement (loop-node-body node))
        (declare (type AST-Node statement))
        (visit-node interpreter statement))))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Move-Left-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Move-Left-Node node))
  (declare (ignore              node))
  (decf (interpreter-cell-pointer interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Move-Right-Node))
  (declare (type Interpreter     interpreter))
  (declare (type Move-Right-Node node))
  (declare (ignore               node))
  (incf (interpreter-cell-pointer interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Output-Node))
  (declare (type Interpreter interpreter))
  (declare (type Output-Node node))
  (declare (ignore           node))
  (write-char
    (code-char
      (current-memory-cell interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (dolist (statement (program-node-statements node))
    (declare (type AST-Node statement))
    (visit-node interpreter statement))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the program stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-brainfuck (brainfuck-code)
  "Interprets the piece of BRAINFUCK-CODE and returns no value."
  (declare (type string brainfuck-code))
  ;; Build an input buffer which tokenizes the BRAINFUCK-CODE.
  (let ((input-buffer (make-input-buffer brainfuck-code)))
    (declare (type Input-Buffer input-buffer))
    
    ;; Build a shift-reduce parser which assembles an abstract syntax
    ;; tree (AST).
    (let ((parser (make-parser input-buffer)))
      (declare (type Parser parser))
      ;; Initialize the PARSER with the default production rules.
      (assign-production-rules parser)
      
      ;; Request the abstract syntax tree (AST) assembled by the parser.
      (let ((tree (parse-program parser)))
        (declare (type Program-Node tree))
        
        ;; Interpreter the abstract syntax tree (AST).
        (let ((interpreter (make-interpreter tree)))
          (declare (type Interpreter interpreter))
          (interpret-program interpreter)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-brainfuck "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-brainfuck ", [ . , ]")
