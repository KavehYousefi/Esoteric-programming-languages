;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Junk", invented by Mark Schad in 2009 and presented by the
;; Esolang user "StapleGun" on April 27th, 2009, its kenspeckle
;; proprium the castaldy of its instructions --- themselves composed of
;; tasks, or "elements", as the actual operative units ---, on a stack
;; whose processing and manipulation furnishes the warklumes for a
;; sophisticated control flow helming.
;; 
;; 
;; Concept
;; =======
;; The Junk programming language's foundry is edified upon a stack of
;; instructions, its gendrure the parsed instruction sequence's
;; collation in its reverse order, whose manipulation homologates the
;; control flow's duction.
;; 
;; == INSTRUCTIONS: COMPOSITIONS OF ID AND TASKS ==
;; An "instruction" in accord with Junk's terminology represents a
;; composition of a mandatory integral identification, or ID, and a
;; list of its actual operative units, the "elements" --- nevened in
;; this documentation, for the purpose of amplified expressiveness,
;; "tasks".
;; 
;; The task list's componency admits zero or more members, each such
;; delineated in its diorism to always include, besides the identifying
;; name, a single argument to operate upon.
;; 
;; == PROGRAM: CALL STACK POPULATION AND EVACUATION ==
;; A Junk program's execution bifurcates into a twissel of coarse
;; tiers, the incipiency relating of its call stack's population by
;; pushing the parsed instructions woning in the source code onto the
;; same. The second stage applies itself to the bailwick involving
;; the thus collated operation's execution.
;; 
;; A tantamount illustration entalented with an enhaused mete of
;; formality shall be the following enumeration's cynosure:
;; 
;;   (1) CALL STACK INITIALIZATION
;;       While still unparsed instructions exist in the source code,
;;       the following efforts are actuated:
;;       
;;       (1.1) INSTRUCTION PARSING
;;             The next instruction in the source code is parsed.
;;       
;;       (1.2) INSTRUCTION PUSHING ONTO THE CALL STACK
;;             The just parsed instruction is pushed onto the call
;;             stack's top position
;;   
;;   (2) PROGRAM EXECUTION
;;       While the call stck is not empty, these actions apply:
;;       
;;       (2.1) INSTRUCTION SELECTION
;;             The top element on the call stack is popped and forms
;;             the currently processed instruction.
;;       
;;       (2.2) INSTRUCTION EXECUTION
;;             The tasks partaking in the currently selected instruction
;;             are executed in their given order.
;; 
;; Extending a more structured diction, the principle shall be limned
;; by a pseudocode tmema:
;; 
;;   function initializeCallStack (instructions)
;;     Input:
;;       instructions: An ordered list of the parsed Junk instructions.
;;     
;;     Output:
;;       callStack:    A stack comprehending the entire INSTRUCTIONS in
;;                     their reverse order.
;;     
;;     Procedure:
;;       let callStack <- prepare empty stack
;;       
;;       for each currentInstruction in instructions do
;;         push currentInstruction onto callStack
;;       end for
;;     
;;     return callStack
;;   end function
;;   
;;   procedure executeProgram (callStack)
;;     Input:
;;       callStack: A stack maintaining the contemporaneously extant
;;                  Junk instructions.
;;     Output:
;;       None.
;;     
;;     Process:
;;       while callStack is not empty do
;;         let currentInstruction <- pop from callStack
;;         
;;         execute currentInstruction
;;       end while
;;   end procedure
;; 
;; == THE MEMORY: A CHAMPARTY OF ARRAY AND ACCUMULATOR ==
;; The Junk program memory lays its amplection around a twissel of
;; constituents: imprimis, an array enumerating 256 signed integer
;; cells, entalented with an amenability to integral addressed from the
;; range [0, 255]; and, secondly, but not parhedral, a signed integer
;; accumulator as a scalar salvatory.
;; 
;; 
;; Architecture
;; ============
;; Junk's architecture intrines two components for the data castaldy and
;; a single specimen dedicated to the execution governance: imprimis,
;; a random-access memory block composed of 256 signed integer numbers,
;; and addressed by mediation of integral subscripts commencing from
;; inclusive zero (0) upwards; secondly, but not disranked into a
;; paravail echolon, the accumulator as an aefauld signed integer
;; salvatory; and ultimately a call stack whose topmost member
;; designates the next instruction to execute.
;; 
;; == THE MEMORY BLOCK: 256 INTEGER-VALUED CELLS ==
;; Endowed with the most capacious circumference, a memory block
;; enumerating 256 continguous cells establishes one moeity of the data
;; department.
;; 
;; Each cell's capacity is meted with a signed integer number of any
;; magnitude, at the program's inchoation assigned the value zero (0),
;; and amenable to an integral address desumed from the closed interval
;; [0, 255].
;; 
;; == THE ACCUMULATOR: A SCALAR INTEGER ==
;; The second participant in the data calstaldy's champarty, the
;; accumulator's admission is restricted to a scalar signed integer
;; number with no natural march in its mickleness, commencing at the
;; incipiency with the value zero (0).
;; 
;; == CALL STACK: THE INSTRUCTION STORAGE ==
;; Forinsecal in its haecceity from the aforementioned twissel of memory
;; and accumulator, the call stack's wike establishes a direct vinculum
;; to the program execution by its maintenance of the instruction
;; sequence to process in the form of a last in, first out salvatory.
;; 
;; At the program's inchoation, the Junk source code's entailed
;; instructions experience a parsing effort, whence their are pushed
;; onto the call stack, effectively reversing the instruction order in
;; comparison to its actually stated arrangement.
;; 
;; Ensuing from the initializtion tier, the execution stage commences,
;; which continuously removes, or pops, the topmost element as the
;; currently active instruction, subsequently evaluating thilk.
;; 
;; The provenance of the control flow manipulations' capaciation
;; emerges from the ability to insert, or push, instructions by
;; adminiculum of their unique identifiers at the stack's top location.
;; Recursion is realized if an instruction pushes itself onto the call
;; stack.
;; 
;; Upon this storage's exhaustion, the program immediately ceases its
;; operations.
;; 
;; 
;; Data Types
;; ==========
;; The ramosity's incarnation in Junk's type system ostends a modest
;; niggardliness, wisting merely of signed integer numbers, these
;; disencumbered from any bournes' impositions, as the paravaunt
;; species, and the ASCII character repertoire's membership as a
;; parhedral contribution, thilk in its utility is not enhaused aboon a
;; token of currency's agency for the communication betwixt a program
;; and the input and output conduits.
;; 
;; 
;; Syntax
;; ======
;; A Junk program's syntactical conspection reveals a sequence
;; enumerating zero or more instructions, everichon among these a
;; compound amplected by a jumelle of opening bracket ("[") and closing
;; counterpart ("]"), inside of whose compass a signed integer
;; identifier resides, segregated by a vertical bar ("|") from the
;; optionally succeeding zero or more tasks, thilk incarnate as twains
;; of an identifying name and one argument.
;; 
;; == INSTRUCTIONS: BRACKETED TASKS ==
;; An instruction is edified upon a tmema introduced via an opening
;; bracket "[" and its closing compernage, "]", inwith whose boundaries
;; a segregation into an optional identifier and, separated therefrom
;; by a single vertical bar ("|"), a comma-separated sequence of tasks
;; is exercised.
;; 
;; == INSTRUCTION ID: AN INTEGRAL IDENTIFIER ==
;; The instruction identifier, or ID, assumes a signed or unsigned
;; integer number's format, posing as an imperative constituent as the
;; first component in the instruction's appropriated space.
;; 
;; == TASK LIST: A COMMA-SEPARATED SEQUENCE ==
;; An instruction's task list does not thole any cumbrance in its
;; mickleness, permitting zero or more items, or "elements".
;; 
;; If not empty, this sequence must be distinguished from the
;; instruction ID by a single vertical bar "|". For empty task lists,
;; the sepiment's presence or absence is supputated as a matter of
;; adiaphoracy.
;; 
;; Each twissel of accolent tasks is accommodated a spatial dispansion
;; demarcated from its compernage by a single comma (",").
;; Supererogative instalment of same symbol, however, do not enjoy the
;; benison of tolerance.
;; 
;; == TASKS: COMPOUNDS OF IDENTIFIER AND SINGLE ARGUMENTS ==
;; The tasks', nevened "elements" in the protolog, conformation
;; delivers, with an aefauld exemption,  a beau ideal of uniformity, its
;; designment ensues from a task identifier and a mandatory argument.
;; Merely the "acc" specimen's enjoyment of a more compendiousness
;; format, scilicet, the reduction to its argument without the task
;; identifier's involvement, provides a variation in the vista.
;; 
;; == ARGUMENTS: INTEGER LITERALS OR ACCUMULATOR REFERENCE ==
;; A twifold phenotype wones in the potential for a task argument's
;; expression; the attendance either that to the immediate influence of
;; literal integer numbers, desumed from the signed space of any
;; conceivable mickleness; or, otherwart in its resolution, the "@"
;; keyword, selfsame is substituted by the accumulator value
;; contemporaneous in its state during the argument's inquisition.
;; 
;; == COMMENTS ==
;; Scholia's dations are homologated at any position outside of an
;; instruction's demarcated segment by simply stating text without the
;; inclusion of the operative sentinel "[", the opening bracket.
;; 
;; == GRAMMAR ==
;; An augmentation of the formality commorant in the treatise on the
;; syntaxis shall be accompassed by the following mold into the
;; Extended Backus-Naur Form (EBNF):
;; 
;;   program       := { comment | instruction } ;
;;   
;;   comment       := commentChar , { commentChar } ;
;;   commentChar   := character - "[" ;
;;   
;;   instruction   := "[" , id , "|" , task , "]" ;
;;   
;;   id            := integer ;
;;   taskList      := { task } ;
;;   task          := ( command , spaces , argument ) | integer ;
;;   
;;   command       := "+"   | "-"   | "*"    | "/"
;;                 |  "="   | "~"   | "<"    | ">"
;;                 |  "in"  | "out" | "out$"
;;                 |  "sto" | "ret"
;;                 |  "acc"
;;                 |  "push"
;;                 ;
;;   argument      := integer | accumulator ;
;;   accumulator   := "@" ;
;;   
;;   spaces        := space , { space } ;
;;   space         := " " | "\t" | "\n" ;
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the mickleness in the material of the protolog, the same
;; supplements its theoretical treatise by a forbisen's dation, a few
;; aspects in the language attain a status clouded by a crepuscle. Their
;; most pertinent specimens shall acquire the following tmemata's
;; cynosure.
;; 
;; == HOW SHALL DUPLICATE INSTRUCTION IDENTIFIERS BE HANDLED? ==
;; An instruction's compass lays its amplectation, besides the task
;; segment, around an integral identifier, this datum's wike that of
;; a claviger for the reference by other or the same instruction for
;; contingent future executions.
;; 
;; Maugre the peisant role apportioned to this property, the standard
;; does not invest a verb anent an ID's duplication among distinct
;; instruction objects.
;; 
;; The potential for a treatment bifurcates into the following
;; alternatives:
;; 
;;   (1) AN ERROR IS SIGNALED
;;       Upon an already assigned ID's participation, an error is
;;       signaled at an apropos point in the program.
;;   
;;   (2) NEWER INSTRUCTIONS ACQUIRE PRECEDENCE
;;       The most recently parsed instruction appropriates the duplicate
;;       ID, retaining in the course of this policy the exclusive
;;       prerogative of addressing if its identification is invoked.
;;       Instructions endowed with this datum in a prevenient stage,
;;       as a kenspeckle consectary, thole an expiration of their
;;       referential potential.
;; 
;; The adjugment has been actuated to impute the first (1) option's
;; purview, inflicting a duplicate ID either during the parsing or the
;; interpretation process with a fatal and abortive error of the type
;; "DuplicateInstructionIdError".
;; 
;; == HOW SHALL UNDEFINED INSTRUCTION IDENTIFIERS BE HANDLED? ==
;; A cognate and complementary of the duplication predicate in the
;; instruction identifier, the lacuna of this datum's existency at the
;; instant of the instruction's intended invocation incurs a further
;; adumbration in the protolog.
;; 
;; Iterum, a twissel of mode in the nomothesia may be conceived:
;; 
;;   (1) AN ERROR IS SIGNALED
;;       At the moment of the illicit instruction ID's request, an error
;;       inflicts the execution.
;;   
;;   (2) NO CAUSATUM IS ACCOMPASSED
;;       The request and its epiphenomenal control flow redirection
;;       effect are deprived of efficacy, destitute of further causata.
;; 
;; It has been adjudged to pursue the first (1) concept, thilk
;; stipulates the signaling of an abortive "InstructionIdError" upon an
;; undefined instruction ID's referencing.
;; 
;; 
;; Instructions
;; ============
;; Junk's instrunction set tallies a cardinality of 25 members, the
;; thus amplected compass appertaining to basic arithmetics, conditional
;; execution, the intercourse betwixt the accumulator and the memory
;; block, as well as a control flow construct built upon the
;; manipulation of and navigation inside of the instruction call stack.
;; 
;; == OVERVIEW ==
;; The following tabulation's dation shall be fulfilled in an apercu's
;; adhibition concerning the language's operative competences.
;; 
;; Please heed the ensconcement of succedaneous tmemata in a jumelle of
;; braces, "{" and "}", their ultimate purpose the complete supersession
;; by actual Junk code in the program.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Description
;;   ------------+-----------------------------------------------------
;;   + {addr}    | Increments the accumulator value by the value stored
;;               | in the memory cell amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   accumulator <- accumulator + memory[addr]
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   - {addr}    | Decrements the accumulator value by the value stored
;;               | in the memory cell amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   accumulator <- accumulator - memory[addr]
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   * {addr}    | Multiplies the accumulator value by the value stored
;;               | in the memory cell amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   accumulator <- accumulator * memory[addr]
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   / {addr}    | Divides the accumulator value by the value stored
;;               | in the memory cell amenable to the address {addr}
;;               | and rounds the new accumulator state to the nearest
;;               | inetger.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   accumulator <- round(accumulator / memory[addr])
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ==================================================================
;;   = {addr}    | If the accumulator value is equal to the value
;;               | stored in the memory cell amenable to the address
;;               | {addr}, proceeds in executing the currently active
;;               | instruction's following tasks; otherwise immediately
;;               | ceases this instruction's execution.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   if not (accumulator = memory[addr]) then
;;               |     terminate current instruction
;;               |   end if
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   ~ {addr}    | If the accumulator value is not equal to the value
;;               | stored in the memory cell amenable to the address
;;               | {addr}, proceeds in executing the currently active
;;               | instruction's following tasks; otherwise immediately
;;               | ceases this instruction's execution.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   if not (accumulator != memory[addr]) then
;;               |     terminate current instruction
;;               |   end if
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   < {addr}    | If the accumulator value is strictly less than the
;;               | value stored in the memory cell amenable to the
;;               | address {addr}, proceeds in executing the currently
;;               | active instruction's following tasks; otherwise
;;               | immediately ceases this instruction's execution.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   if not (accumulator < memory[addr]) then
;;               |     terminate current instruction
;;               |   end if
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   > {addr}    | If the accumulator value is strictly greater than
;;               | the value stored in the memory cell amenable to the
;;               | address {addr}, proceeds in executing the currently
;;               | active instruction's following tasks; otherwise
;;               | immediately ceases this instruction's execution.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   if not (accumulator > memory[addr]) then
;;               |     terminate current instruction
;;               |   end if
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ==================================================================
;;   in {addr}   | Queries the standard input conduit for a signed or
;;               | unsigned integer number and stores thilk in the
;;               | memory cell amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the committed input cannot be interpreted as an
;;               | integral object, an error of the type
;;               | "NonNumericInputError" is signaled.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   in$ {addr}  | Queries the standard input conduit for a single
;;               | character and stores its ASCII code in the memory
;;               | cell amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   out {addr}  | Prints the value of the memory cell amenable to the
;;               | address {addr} in its verbatim numeric form to the
;;               | standard output.
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   out$ {addr} | Prints the character whose ASCII code corresponds to
;;               | the value of the memory cell amenable to the address
;;               | {addr} to the standard output conduit.
;;               |-----------------------------------------------------
;;               | If the referenced memory cell value transcends the
;;               | valid ASCII code interval of [0, 255], the actual
;;               | character code is obtained through a prevenient
;;               | modulus operation with the number 256; that is, in a
;;               | pseudocode diction:
;;               |   let asciiCode <- memory[addr] modulus 256
;;               |   print character with ASCII code asciiCode
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ==================================================================
;;   acc {value} | Stores the {value} in the accumulator.
;;               |-----------------------------------------------------
;;               | The {value} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;   ..................................................................
;;   {value}     | Stores the {value} in the accumulator.
;;               |-----------------------------------------------------
;;               | The {value} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;   ..................................................................
;;   ret {addr}  | Copies the value stored in the memory cell amenable
;;               | to the address {addr} to the accumulator.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   accumulator <- memory[argument]
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ..................................................................
;;   sto {addr}  | Copies the accumulator state to the memory cell
;;               | amenable to the address {addr}.
;;               |-----------------------------------------------------
;;               | Elucidated by adminiculum of a pseudocode tmema,
;;               | it holds:
;;               |   memory[argument] <- accumulator
;;               |-----------------------------------------------------
;;               | The {addr} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If the {addr} does not designate a valid memory
;;               | address, an error of the type
;;               | "InvalidMemoryAddressError" will be signaled.
;;   ==================================================================
;;   push {id}   | Retrieves the instruction amenable to the identifier
;;               | {id} and pushes thilk to the top of the call stack.
;;               |-----------------------------------------------------
;;               | The {id} must be either of:
;;               |   (1) A signed or unsigned literal integer number.
;;               |   (2) The "@" keyword, which is replaced by the
;;               |       accumulator's value.
;;               |-----------------------------------------------------
;;               | If no instruction with the {id} exists, an error of
;;               | the type "UnassignedInstructionIdError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's reification has been exercised in the programming
;; language Common Lisp, installing as a vinculum betwixt the Junk
;; source code string and the execution stage an adminicular tier whose
;; bailiwick involves the instructions' extraction and parsing, whence
;; is yielded an ordered list of dedicated object representations.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-06-02
;; 
;; Sources:
;;   [esolang2009Junk]
;;   The Esolang, "Junk", April 30th, 2009
;;   URL: "https://esolangs.org/wiki/Junk"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type, the agnomination of which is desumed from
   the TYPE-NAME, its formal arguments the LAMBDA-LIST's verbatim
   dation, and which assigns to the object to administer the docimasy to
   the CANDIDATE-NAME, evaluating the BODY forms, with the desinent
   form's primary result being construed as the assessment's conclusion,
   a \"generalized boolean\" value of \"true\" furnishing a
   vouchsafement of the candidate's compliance with the imposed
   stipulations, while a \"false\" output communicates its rejections.
   ---
   The first BODY form, upon its resolution to a string object, is
   imputed as the derived type's documentation string, and is
   subsequently reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   '*)
                                                 (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   admits zero or more entries, everichon among these a composite of a
   key that adheres to the KEY-TYPE and a value complying with the
   VALUE-TYPE, for both is stipulated the generic sentinel ``*'' as the
   default."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping betwixt Junk
   language keywords and their representative tokens, maintained by a
   hash table whose keys accommodate the identifier names as simple
   strings, each answering with a ``Token'' value."
  '(hash-table-of simple-string Token))

;;; -------------------------------------------------------

(deftype task-kind ()
  "The ``task-kind'' type enumerates the recognized variation on Junk
   task kinds, identifying the task's \"mnemonic\" in a symbolic guise."
  '(member
    :add
    :subtract
    :multiply
    :divide
    :equal
    :not-equal
    :less-than
    :greater-than
    :in
    :in$
    :out
    :out$
    :acc
    :ret
    :sto
    :push))

;;; -------------------------------------------------------

(deftype task-list ()
  "The ``task-list'' type defines an ordered list composed of zero or
   more ``Task'' objects."
  '(list-of Task))

;;; -------------------------------------------------------

(deftype instruction-list ()
  "The ``instruction-list'' type defines an ordered list composed of
   zero or more ``Instruction'' objects."
  '(list-of Instruction))

;;; -------------------------------------------------------

(deftype instruction-registry ()
  "The ``instruction-registry'' type defines an association betwixt a
   numeric instruction ID and its ensconcing Junk instruction, realized
   in the guise of a hash table, the keys of which assume signed integer
   numbers and ally with ``Instruction'' instances."
  '(hash-table-of integer Instruction))

;;; -------------------------------------------------------

(deftype call-stack ()
  "The ``call-stack'' type defines a last-in first-out collection of
   instructions, founded upon a list-based stack which admits
   ``Instruction'' objects only"
  '(list-of Instruction))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Junk memory block as a
   one-dimensional simple array of 256 signed integer-valued cells."
  '(simple-array integer (256)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which entails, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Evaluates the OBJECT in its agency as a \"generalized boolean\" and
   returns an actual Boolean equivalent thereof, responding with a
   ``boolean'' value of ``T'' for a non-``NIL'' input; otherwise, for
   a ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   including in its diorism the space, horizontal tab, and newline
   entities, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Linefeed)
          (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a character homologated
   to partake of an identifier's constitution, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not
      (or (whitespace-character-p candidate)
          (find candidate "[]|," :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class furnishes an encapsulation of a significant
   object extracted from a piece of Junk source code during the lexical
   analyzation stage, its componency a twissel of a categorizing type
   and a detailing value."
  (type  (error "Missing token type.")  :type T :read-only T)
  (value (error "Missing token value.") :type T :read-only T))

;;; -------------------------------------------------------

(defun token-is-of-type-p (token expected-type)
  "Determines whether the TOKEN complies to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (get-boolean-value-of
      (eq (token-type token)
          expected-type))))

;;; -------------------------------------------------------

(defun token-designates-task-p (candidate)
  "Determines whether the CANDIDATE represents a token representative of
   a task's identification, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (get-boolean-value-of
      (typep (token-type candidate) 'task-kind))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (let ((identifiers (make-hash-table :test #'equal)))
    (declare (type identifier-table identifiers))
    (flet ((register-identifier (name token-type token-value)
            "Associates the identifier NAME with a fresh token assembled
             from the categorizing TOKEN-TYPE and the detailing
             TOKEN-VALUE, stores the jumelle in the IDENTIFIERS table,
             and returns no value."
            (declare (type simple-string name))
            (declare (type keyword       token-type))
            (declare (type simple-string token-value))
            (setf (gethash name identifiers)
              (make-token token-type token-value))
            (values)))
      ;; Parameter signifiers.
      (register-identifier "@"    :at-sign      "@")
      ;; Binary arithmetic operators.
      (register-identifier "+"    :add          "+")
      (register-identifier "-"    :subtract     "-")
      (register-identifier "*"    :multiply     "*")
      (register-identifier "/"    :divide       "/")
      ;; Relational operators.
      (register-identifier "="    :equal        "=")
      (register-identifier "~"    :not-equal    "~")
      (register-identifier "<"    :less-than    "<")
      (register-identifier ">"    :greater-than ">")
      ;; Input and output commands.
      (register-identifier "in"   :in           "in")
      (register-identifier "in$"  :in$          "in$")
      (register-identifier "out"  :out          "out")
      (register-identifier "out$" :out$         "out$")
      ;; Accumulator access commands.
      (register-identifier "acc"  :acc          "acc")
      (register-identifier "ret"  :ret          "ret")
      (register-identifier "sto"  :sto          "sto")
      ;; Stack access commands.
      (register-identifier "push" :push         "push"))
    (the identifier-table identifiers))
  "Associates the recognized Junk language keywords as string with
   representative tokens.")

;;; -------------------------------------------------------

(defun get-identifier (source)
  "Returns for the SOURCE a covenable token representation, this either
   representing a Junk language keyword, or a generic ``:word''
   instance."
  (declare (type string source))
  (the Token
    (or (gethash source +IDENTIFIERS+)
        (make-token :word source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token evaluator operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attempt-to-parse-integer-token (source)
  "Attempts to parse the SOURCE string as an integer number, returning
   on success an ``:integer'' token representation of its thus obtained
   numeric value; otherwise produces ``NIL''."
  (declare (type string source))
  (the (or null Token)
    (ignore-errors
      (make-token :integer
        (parse-integer source)))))

;;; -------------------------------------------------------

(defun parse-word-as-token (source)
  "Returns a connable token representation of the SOURCE string,
   construed as an identifier."
  (declare (type string source))
  (the Token
    (or (attempt-to-parse-integer-token source)
        (get-identifier                 source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor prepare-lexer (source)))
  "The ``Lexer'' class applies itself to the furnishment of a lexical
   analyzer, thilk's dever manifests in the segregation of a piece of
   Junk source code into its pertainent objects and their delivery in
   the guise of tokens."
  (source   (error "Missing lexer source.")
            :type      string
            :read-only T)
  (position 0
            :type      fixnum
            :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slot ``source'' to the local symbol
   macro ``$source'', and its ``position'' to ``$position'', evaluates
   the BODY forms, and returns the desinent form's results.
   ---
   As a contribution of supererogation, a twissel of local symbol
   macro bindings not immediately consistent with the ``Lexer'' class
   componency partake of the ``with-lexer'' macro's circumference,
   scilicet, the character ``$character'' and the Boolean flag
   ``$source-is-exhauste-p''. The following apercu shall administer a
   cursory of nortelry concerning the quadruple diorisms' dation:
   --------------------------------------------------------------------
   Local symbol macro     | Role
   -----------------------+--------------------------------------------
   $source                | The piece of Junk source code string to
                          | analyze.
   ....................................................................
   $position              | The currently processed index into the
                          | $SOURCE.
   ....................................................................
   $character             | The character t the $POSITION into the
                          | $SOURCE.
   ....................................................................
   $source-is-exhausted-p | A Boolean flag which determines whether the
                          | $POSITION cursor has moved beyond the
                          | $SOURCE's bournes.
   --------------------------------------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer)
                (ignorable    ,evaluated-lexer))
       (symbol-macrolet
           (($source
             (the string
               (lexer-source ,evaluated-lexer)))
            ($position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            ($character
             (the character
               (char $source $position)))
            ($source-is-exhausted-p
             (the boolean
               (not (array-in-bounds-p $source $position)))))
         (declare (type string    $source)
                  (ignorable      $source))
         (declare (type fixnum    $position)
                  (ignorable      $position))
         (declare (type character $character)
                  (ignorable      $character))
         (declare (type boolean   $source-is-exhausted-p)
                  (ignorable      $source-is-exhausted-p))
         ,@body))))

;;; -------------------------------------------------------

(defun current-character-equals-p (lexer expected-character)
  "Determines whether the LEXER's current character equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Lexer     lexer))
  (declare (type character expected-character))
  (the boolean
    (with-lexer (lexer)
      (get-boolean-value-of
        (and
          (not $source-is-exhausted-p)
          (char= $character expected-character))))))

;;; -------------------------------------------------------

(defun current-character-is-whitespace-p (lexer)
  "Determines whether the LEXER's current character represents a
   whitespace, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (with-lexer (lexer)
      (and (not $source-is-exhausted-p)
           (whitespace-character-p $character)))))

;;; -------------------------------------------------------

(defun locate-end-of-current-word (lexer)
  "Proceeding from the current position into the LEXER's source and
   expected to reside in a word, returns the position immediately
   succeeding its occupied tmema."
  (declare (type Lexer lexer))
  (the fixnum
    (with-lexer (lexer)
      (or (position-if-not #'identifier-character-p $source
            :start $position)
          (length $source)))))

;;; -------------------------------------------------------

(defun extract-word (lexer)
  "Proceeding from the current position into the LEXER's source and
   expected to reside in a word, returns the same, while concomitantly
   advancing the LEXER's position cursor the location immediately
   succeeding the thus obtained word's occupied tmema."
  (declare (type Lexer lexer))
  (the string
    (with-lexer (lexer)
      (let ((end-of-word (locate-end-of-current-word lexer)))
        (declare (type fixnum end-of-word))
        (prog1
          (subseq $source $position end-of-word)
          (setf $position end-of-word))))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an identifier and returns a conable token representation of
   its content."
  (declare (type Lexer lexer))
  (the Token
    (parse-word-as-token
      (extract-word lexer))))

;;; -------------------------------------------------------

(defun read-symbol (lexer token-type)
  "Returns a fresh token as a composition of the TOKEN-TYPE and the
   LEXER's current character as the token's value, while concomitantly
   advancing the LEXER's position cursor to the character in its
   source."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (make-token token-type
        (prog1 $character
          (incf $position))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a catena composed of zero or more accolent whitespaces and returns no
   value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (current-character-is-whitespace-p lexer) do
      (incf $position)))
  (values))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (``:eof'') token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ($source-is-exhausted-p
          (make-token :eof NIL))
        
        ((whitespace-character-p $character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        
        ((current-character-equals-p lexer #\[)
          (read-symbol lexer :left-bracket))
        
        ((current-character-equals-p lexer #\])
          (read-symbol lexer :right-bracket))
        
        ((current-character-equals-p lexer #\|)
          (read-symbol lexer :vertical-bar))
        
        ((current-character-equals-p lexer #\,)
          (read-symbol lexer :comma))
        
        (T
          (read-identifier lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of task arguments.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Task-Argument)
  "The ``Task-Argument'' interface establishes a common foundry for all
   classes intent on the representation of task arguments.")

;;; -------------------------------------------------------

(defstruct (Literal-Argument
  (:include     Task-Argument)
  (:constructor make-literal-argument (value)))
  "The ``Literal-Argument'' class serves in the encapsulation of a task
   argument expressed in a literal integer number."
  (value (error "Missing value for literal integer argument.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Accumulator-Argument
  (:include     Task-Argument)
  (:constructor make-accumulator-argument ()))
  "The ``Accumulator-Argument'' class serves in the representation of a
   task argument referencing an inquisition of the accumulator state.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Task".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Task
  (:constructor make-task (kind argument)))
  "The ``Task'' class applies itself to the encapsulation of a task, an
   operative unit commorant inwith an instruction and entalented with
   epiphenomenal capacity.
   ---
   The ``Task'' diorism enumerates a categorizing kind, ostending the
   mnemonic in a symbol mode, and an argument upon thilk the action is
   adhibited."
  (kind     (error "Missing instruction kind.")
            :type      task-kind
            :read-only T)
  (argument (error "Missing instruction argument.")
            :type      Task-Argument
            :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor prepare-instruction (id tasks)))
  "The ``Instruction'' class' compass amplects the castaldy of a Junk
   instruction's pertinent attribute, scilicet, the numeric ID and a
   list of its comprising tasks."
  (id    (error "Missing instruction ID.")
         :type      integer
         :read-only T)
  (tasks (error "Missing instruction task list.")
         :type      task-list
         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The entity responsible for the token delivery.")
   (current-token
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Parser'' class is entalented with the wike of a Junk
     program's assemblage from a sequence of tokens."))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slot ``lexer'' to the local symbol
   macro ``$lexer'' and its slot ``current-token'' to
   ``$current-token'', evaluates the BODY forms, and returns the
   desinent form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (declare (ignorable   ,evaluated-parser))
       (symbol-macrolet
           (($lexer
             (the Lexer
               (slot-value ,evaluated-parser 'lexer)))
            ($current-token
             (the Token
               (slot-value ,evaluated-parser 'current-token))))
         (declare (type Lexer $lexer)
                  (ignorable  $lexer))
         (declare (type Token $current-token)
                  (ignorable  $current-token))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Queries the first token from the PARSER's underlying lexer, stores
   thilk in the PARSER itself, and returns no value."
  (declare (type Parser parser))
  (with-parser (parser)
    (setf $current-token
      (get-next-token $lexer)))
  (values))

;;; -------------------------------------------------------

(defun prepare-parser (lexer)
  "Creates and returns a fresh ``Parser'' whose requisite tokens'
   furnishment is assigned to the LEXER's bailiwick."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun consume-current-token (parser)
  "Returns the PARSER's current token, while concomitantly quering the
   next one from the underlying lexer and storing thilk in the PARSER
   in lieu of the just delivered instance."
  (declare (type Parser parser))
  (the Token
    (with-parser (parser)
      (prog1 $current-token
        (setf $current-token
          (get-next-token $lexer))))))

;;; -------------------------------------------------------

(defun expect-token (parser expected-token-type)
  "Determines whether the PARSER's current token complies with the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying the next instant from the underlying
   lexer and storing thilk in the PARSER; otherwise, upon a mismatch,
   an error of an unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
    (with-parser (parser)
      (if (token-is-of-type-p $current-token expected-token-type)
        (consume-current-token parser)
        (error "Expected a token of the type ~s, but encountered the ~
                token ~s."
          expected-token-type $current-token)))))

;;; -------------------------------------------------------

(defun skip-comment (parser)
  "Consumes a sequence of zero or more accolent tokens in the PARSER
   which form non-instruction content, as thus being attended with the
   consideration as descants, propagating until either an instruction
   signifier in the form of the \"[\" token or an end-of-file token
   is encountered, and returns no value."
  (declare (type Parser parser))
  (with-parser (parser)
    (symbol-macrolet
        ((comment-has-concluded-p
          (the boolean
            (get-boolean-value-of
              (or (token-is-of-type-p $current-token :left-bracket)
                  (token-is-of-type-p $current-token :eof))))))
      (declare (type boolean comment-has-concluded-p))
      (loop until comment-has-concluded-p do
        (consume-current-token parser))))
  (values))

;;; -------------------------------------------------------

(defun parse-task-argument (parser)
  "Parses a task argument utilizing the PARSER's services and returns a
   covenable ``Task-Argument'' representation thereof."
  (declare (type Parser parser))
  (the Task-Argument
    (with-parser (parser)
      (case (token-type $current-token)
        (:integer
          (make-literal-argument
            (token-value
              (consume-current-token parser))))
        (:at-sign
          (consume-current-token parser)
          (make-accumulator-argument))
        (otherwise
          (error "The token ~s does not designate a task argument."
            $current-token))))))

;;; -------------------------------------------------------

(defun parse-task (parser)
  "Attempts to parse an instruction task utilizing the PARSER, on
   success returning a conable ``Task'' representation of the thus
   produced element; otherwise returns the ``NIL'' value."
  (declare (type Parser parser))
  (the (or null Task)
    (with-parser (parser)
      (cond
        ((token-is-of-type-p $current-token :integer)
          (make-task :acc
            (make-literal-argument
              (token-value
                (consume-current-token parser)))))
        ((token-designates-task-p $current-token)
          (make-task
            (token-type
              (consume-current-token parser))
            (parse-task-argument parser)))
        (T
          NIL)))))

;;; -------------------------------------------------------

(defun parse-subsequent-member-in-task-list (parser)
  "Attempts to parse a task succeeding the first member of an
   instruction's task list, either immediately or distant, utilizing
   for this endeavor the PARSER's services, and returning upon a
   successful detection a covenable ``Task'' representation thereof;
   otherwise, upon its lacuna, responds with the ``NIL'' value."
  (declare (type Parser parser))
  (the (or null Task)
    (with-parser (parser)
      (when (token-is-of-type-p $current-token :comma)
        (consume-current-token parser)
        (or (parse-task parser)
            (error "Expected a task, but encountered the token ~s."
              $current-token))))))

;;; -------------------------------------------------------

(defun parse-task-list (parser)
  "Parses a sequence of instruction tasks utlizing the PARSER's
   services and returns an ordered list of its ``Task''
   representations."
  (declare (type Parser parser))
  (the task-list
    (with-parser (parser)
      (let ((first-task (parse-task parser)))
        (declare (type (or null Task) first-task))
        (when first-task
          (loop
            for next-task
              of-type (or null Task)
              =       (parse-subsequent-member-in-task-list parser)
            
            while next-task
              collect next-task
                into subsequent-tasks
            
            finally
              (return
                (cons first-task subsequent-tasks))))))))

;;; -------------------------------------------------------

(defun parse-instruction-id (parser)
  "Parses an instruction ID utilizing the PARSER's services and returns
   thilk as a signed integer number."
  (declare (type Parser parser))
  (the integer
    (token-value
      (expect-token parser :integer))))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Parses an aefauld instruction utilizing the PARSER's services and
   returns a conable ``Instruction'' representation thereof."
  (declare (type Parser parser))
  (the Instruction
    (with-parser (parser)
      (expect-token parser :left-bracket)
      
      (let ((id    (parse-instruction-id parser))
            (tasks NIL))
        (declare (type integer   id))
        (declare (type task-list tasks))
        
        ;; [id|...]
        (when (token-is-of-type-p $current-token :vertical-bar)
          (consume-current-token parser)
          (setf tasks
            (parse-task-list parser)))
        
        (expect-token parser :right-bracket)
        
        (prepare-instruction id tasks)))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a sequence of zero or more Junk instructions utilizing the
   PARSER's services and returns an ordered list of their
   ``Instruction'' representations."
  (declare (type Parser parser))
  (the instruction-list
    (with-parser (parser)
      (loop
        if (token-is-of-type-p $current-token :left-bracket)
          collect
            (parse-instruction parser)
        else if (token-is-of-type-p $current-token :eof) do
          (loop-finish)
        else do
          (skip-comment parser)
        end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction registry operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-instruction-registry ()
  "Creates and returns an initially empty ``instruction-registry''."
  (the instruction-registry
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun instruction-with-id-exists-p (registry instruction-id)
  "Determines whether an instruction amenable to the INSTRUCTION-ID
   exists in the instruction REGISTRY, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type instruction-registry registry))
  (declare (type integer           instruction-id))
  (the boolean
    (get-boolean-value-of
      (nth-value 1
        (gethash instruction-id registry)))))

;;; -------------------------------------------------------

(defun register-instruction (registry instruction)
  "Registers the INSTRUCTION at the instruction REGISTRY by adminiculum
   of its numeric ID and returns no value.
   ---
   If an instruction with the same ID already partakes of the REGISTRY's
   compass, an error of an unspecified type is signaled."
  (declare (type instruction-registry registry))
  (declare (type Instruction       instruction))
  (let ((instruction-id (instruction-id instruction)))
    (declare (type integer instruction-id))
    (if (instruction-with-id-exists-p registry instruction-id)
      (error 'Duplicate-Instruction-Id-Error
        :offending-id instruction-id)
      (setf (gethash instruction-id registry) instruction)))
  (values))

;;; -------------------------------------------------------

(defun retrieve-instruction-by-id (registry instruction-id)
  "Returns the instruction registered with the INSTRUCTION-ID at the
   instruction REGISTRY; or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type instruction-registry registry))
  (declare (type integer           instruction-id))
  (the Instruction
    (or (gethash instruction-id registry)
        (error 'Unassigned-Instruction-Id-Error
          :offending-id instruction-id))))

;;; -------------------------------------------------------

(defun build-instruction-registry (instructions)
  "Creates and returns a fresh ``instruction-registry'' which maps the
   instruction identifiers in the INSTRUCTIONS list to their instruction
   objects."
  (declare (type instruction-list instructions))
  (let ((instruction-registry (prepare-empty-instruction-registry)))
    (declare (type instruction-registry instruction-registry))
    (dolist (current-instruction instructions)
      (declare (type Instruction current-instruction))
      (register-instruction instruction-registry current-instruction))
    (the instruction-registry instruction-registry)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Junk-Error (error)
  ()
  (:documentation
    "The ``Junk-Error'' condition type applies itself to a common
     substratum's furnishment directed at those condition types
     nuncupated to the representation of anomalous situations arising
     during any stage of a Junk program's processing."))

;;; -------------------------------------------------------

(define-condition Duplicate-Instruction-Id-Error (Junk-Error)
  ((offending-id
    :initarg       :offending-id
    :initform      (error "Missing offending instruction ID.")
    :reader        duplicate-instruction-id-error-offending-id
    :type          integer
    :documentation "The instruction ID whose trial of redefinition has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Instruction-Id-Error condition))
      (declare (type destination                    stream))
      (format stream "An instruction with the ID ~d already exists."
        (duplicate-instruction-id-error-offending-id condition))))
  (:documentation
    "The ``Duplicate-Instruction-Id-Error'' serves in the communication
     of an anomalous situation whose etiology emerges from the trial to
     register define an instruction with an identification (ID) already
     assigned to a preveniently specified instruction."))

;;; -------------------------------------------------------

(define-condition Unassigned-Instruction-Id-Error (Junk-Error)
  ((offending-id
    :initarg       :offending-id
    :initform      (error "Missing offending instruction ID.")
    :reader        unassigned-instruction-id-error-offending-id
    :type          integer
    :documentation "The instruction ID whose trial of invocation has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Unassigned-Instruction-Id-Error condition))
      (declare (type destination                     stream))
      (format stream "No instruction with the ID ~d exists."
        (unassigned-instruction-id-error-offending-id condition))))
  (:documentation
    "The ``Unassigned-Instruction-Id-Error'' condition type serves in
     the apprizal about an anomalous situation whose etiology emerges
     from the trial to request an instruction by an identification (ID)
     having never been defined."))

;;; -------------------------------------------------------

(define-condition Non-Numeric-Input-Error (Junk-Error)
  ((offending-input
    :initarg       :offending-input
    :initform      (error "Missing offending input.")
    :reader        non-numeric-input-error-offending-input
    :type          string
    :documentation "The committed input whose illicit conformation is
                    peccant in this error's instigation."))
  (:report
    (lambda (condition stream)
      (declare (type Non-Numeric-Input-Error condition))
      (declare (type destination             stream))
      (format stream "The input ~s cannot be interpreted as an ~
                      integer number."
        (non-numeric-input-error-offending-input condition))))
  (:documentation
    "The ``Non-Numeric-Input-Error'' condition type serves in the
     apprizal about a "))

;;; -------------------------------------------------------

(define-condition Invalid-Memory-Address-Error (Junk-Error)
  ((offending-address
    :initarg       :offending-address
    :initform      (error "Missing offending address.")
    :reader        invalid-memory-address-error-offending-address
    :type          integer
    :documentation "The address whose incompatibility with the program
                    memory has encumbered the trial with this error."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Memory-Address-Error condition))
      (declare (type destination                  stream))
      (format stream "The address ~d is invalid for the memory block."
        (invalid-memory-address-error-offending-address condition))))
  (:documentation
    "The ``Invalid-Memory-Address-Error'' condition type serves in the
     communication of an anomalous circumstance whose gendrure
     constitutes an ultimity from the attempt to access the Junk program
     memory block, either in the purpose of a perquisition or
     modulation operation, via an ineligible address."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-numeric-input ()
  "Queries the standard input conduit for a signed or unsigned integer
   number and returns thilk in its parsed form.
   ---
   Upon an invalid input's commission, an error of the type
   ``Non-Numeric-Input-Error'' is signaled."
  (the integer
    (let ((user-input (read-line *standard-input* NIL #\Null)))
      (declare (type string  user-input))
      (handler-case
          (parse-integer user-input)
        (error ()
          (error 'Non-Numeric-Input-Error
            :offending-input user-input))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-memory-block ()
  "Creates and returns a fresh Junk program memory block as a
   one-dimensional simple array comprehending 256 signed integer
   elements."
  (the memory
    (make-array 256
      :element-type    'integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing instructions for interpreter.")
    :type          instruction-list
    :documentation "The Junk instructions to execute.")
   (instruction-registry
    :type          instruction-registry
    :documentation "Maps the instruction IDs to the respective
                    ``Instruction'' objects.")
   (call-stack
    :initform      NIL
    :type          call-stack
    :documentation "A last-in first-out structure which stores the
                    instructions to invoke, the current specimen always
                    residing on the top position.")
   (current-instruction
    :type          Instruction
    :documentation "The currently executing instruction, the same
                    constitutes the most recently removed element from
                    the CALL-STACK.")
   (executes-next-task-p
    :initform      T
    :type          boolean
    :documentation "Determines whether the current instruction's
                    following tasks shall be executed.")
   (memory
    :initform      (prepare-memory-block)
    :type          memory
    :documentation "The paravaunt memory as a random-access sequence
                    comprehending 256 signed integer cells.")
   (accumulator
    :initform      0
    :type          integer
    :documentation "The single register known as the \"accumulator\"."))
  (:documentation
    "The ``Interpreter'' class is apportioned the wike of accompassing
     actual efficacy to a Junk program communicated in the form of a
     complex instruction list."))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evalues the INTERPRETER, binds its slots to local symbol macros,
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   The following symbol macros, each equiparating with a slot in the
   INTERPRETER, establish this macro's furnishment:
     ------------------------------------------------------------------
     Symbol macro          | Role
     ----------------------+-------------------------------------------
     $program              | The parsed instructions in their original
                           | order.
     ..................................................................
     $instruction-registry | Maps the $PROGRAM instructions to their
                           | unique instruction IDs.
     ..................................................................
     $call-stack           | A stack maintaining the Junk instructions.
                           | At the interpreter's inchoation, this stack
                           | merely contains the $PROGRAM instructions
                           | in their reserved, as pushed, arrangement.
     ..................................................................
     $current-instruction  | The current instruction, being tantamount
                           | to the $CALL-STACK's most recently removed
                           | erstwhile top element.
     ..................................................................
     $executes-next-task-p | A Boolean flag which determines whether
                           | the current instruction's following tasks
                           | shall be executed.
     ..................................................................
     $memory               | The memory block, composed of 256 signed
                           | integer cells.
     ..................................................................
     $accumulator          | The signed integer accumulator.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter)
                (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($program
              (the instruction-list
                (slot-value ,evaluated-interpreter 'program)))
            ($instruction-registry
              (the instruction-registry
                (slot-value
                  ,evaluated-interpreter
                  'instruction-registry)))
            ($call-stack
              (the call-stack
                (slot-value ,evaluated-interpreter 'call-stack)))
            ($current-instruction
              (the Instruction
                (slot-value
                  ,evaluated-interpreter
                  'current-instruction)))
            ($executes-next-task-p
              (the boolean
                (slot-value
                  ,evaluated-interpreter
                  'executes-next-task-p)))
            ($memory
              (the memory
                (slot-value ,evaluated-interpreter 'memory)))
            ($accumulator
              (the integer
                (slot-value ,evaluated-interpreter 'accumulator))))
         (declare (type instruction-list     $program)
                  (ignorable                 $program))
         (declare (type instruction-registry $instruction-registry)
                  (ignorable                 $instruction-registry))
         (declare (type call-stack           $call-stack)
                  (ignorable                 $call-stack))
         (declare (type Instruction          $current-instruction)
                  (ignorable                 $current-instruction))
         (declare (type memory               $memory)
                  (ignorable                 $memory))
         (declare (type integer              $accumulator)
                  (ignorable                 $accumulator))
         (declare (type boolean              $executes-next-task-p)
                  (ignorable                 $executes-next-task-p))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Based upon the Junk program consigned to the INTERPRETER's castaldy,
   supputates the instruction registry and the call stack, stores both
   in the INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (psetf
      $instruction-registry (build-instruction-registry $program)
      $call-stack           (reverse                    $program)))
  (values))

;;; -------------------------------------------------------

(defun prepare-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Junk
   PROGRAM's execution."
  (declare (type instruction-list program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-has-completed-p (interpreter)
  "Determines whether the INTERPRETER has exhausted its call stack, in
   which case its Junk program is deemed as completed, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-interpreter (interpreter)
      (null $call-stack))))

;;; -------------------------------------------------------

(defun validate-memory-address (interpreter probed-address)
  "Determines whether the PROBED-ADDRESS constitutes a valid subscript
   into the INTERPRETER memory block, on confirmation returning no
   value; otherwise an error of the type
   ``Invalid-Memory-Address-Error'' will be instigated."
  (declare (type Interpreter interpreter))
  (declare (type integer     probed-address))
  (with-interpreter (interpreter)
    (unless (array-in-bounds-p $memory probed-address)
      (error 'Invalid-Memory-Address-Error
        :offending-address probed-address)))
  (values))

;;; -------------------------------------------------------

(defgeneric resolve-argument (interpreter argument)
  (:documentation
    "Returns the value referenced by the task ARGUMENT in the
     INTERPRETER's context.")
  
  (:method ((interpreter Interpreter)
            (argument    Literal-Argument))
    (declare (type Interpreter      interpreter)
             (ignore                interpreter))
    (declare (type Literal-Argument argument))
    (the integer
      (literal-argument-value argument)))
  
  (:method ((interpreter Interpreter)
            (argument    Accumulator-Argument))
    (declare (type Interpreter          interpreter))
    (declare (type Accumulator-Argument argument))
    (the integer
      (with-interpreter (interpreter)
        $accumulator))))

;;; -------------------------------------------------------

(defgeneric get-memory-cell-at (interpreter address)
  (:documentation
    "Returns the value stored in the INTERPRETER's memory block amenable
    to the ADDRESS.")
  
  (:method ((interpreter interpreter)
            (address     integer))
    (declare (type Interpreter interpreter))
    (declare (type integer     address))
    (validate-memory-address interpreter address)
    (the integer
      (with-interpreter (interpreter)
        (aref $memory address))))
  
  (:method ((interpreter interpreter)
            (address     Task-Argument))
    (declare (type Interpreter   interpreter))
    (declare (type Task-Argument address))
    (the integer
      (get-memory-cell-at interpreter
        (resolve-argument interpreter address))))
  
  (:method ((interpreter interpreter)
            (address     Task))
    (declare (type Interpreter interpreter))
    (declare (type Task        address))
    (the integer
      (get-memory-cell-at interpreter
        (task-argument address)))))

;;; -------------------------------------------------------

(defgeneric set-memory-cell-at (interpreter address new-value)
  (:documentation
    "Stores the NEW-VALUE in the INTERPRETER's memory block amenable
     to the ADDRESS and returns no value.")
  
  (:method ((interpreter Interpreter)
            (address     integer)
            (new-value   integer))
    (declare (type integer     new-value))
    (declare (type Interpreter interpreter))
    (declare (type integer     address))
    (validate-memory-address interpreter address)
    (with-interpreter (interpreter)
      (setf (aref $memory address)
            new-value))
    (values))
  
  (:method ((interpreter Interpreter)
            (address     Task-Argument)
            (new-value   integer))
    (declare (type Interpreter   interpreter))
    (declare (type Task-Argument address))
    (declare (type integer       new-value))
    (set-memory-cell-at interpreter
      (resolve-argument interpreter address)
      new-value)
    (values))
  
  (:method ((interpreter Interpreter)
            (address     Task)
            (new-value   integer))
    (declare (type Interpreter interpreter))
    (declare (type Task        address))
    (declare (type integer     new-value))
    (set-memory-cell-at interpreter
      (task-argument address)
      new-value)
    (values)))

;;; -------------------------------------------------------

(defun adjust-task-execution-flag (interpreter task predicate)
  "Determines whether the PREDICATE, receiving as its first operand the
   memory cell amenable to the TASK argument's specified address and as
   its second the INTERPRETER's accumulator state, is satisfied, setting
   the INTERPRETER's next task execution flag accordingly, and returns
   no value.
   ---
   The PREDICATE must be a dyadic function, accepting two integer inputs
   and producing an integer result; as a corollary, the signature
   adheres to:
     lambda (integer integer) => integer"
  (declare (type Interpreter                    interpreter))
  (declare (type Task                           task))
  (declare (type (function (integer integer) *) predicate))
  (with-interpreter (interpreter)
    (setf $executes-next-task-p
      (get-boolean-value-of
        (funcall predicate
          $accumulator
          (get-memory-cell-at interpreter task)))))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-task (task-kind task interpreter)
  (:documentation
    "Evaluates the TASK, dispatched on its TASK-KIND, in the
     INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defmacro define-task-dispatch (task-kind (task-name interpreter-name)
                                &body body)
  "Defines an implementation of the generic function ``dispatch-task'',
   assigning to its first formal parameter an automatically generated
   name, and dispatching on an ``eql''-equality to the TASK-KIND,
   appropriating for the second parameter the TASK-NAME, while
   dispatching on the ``Task'' class, and for the third parameter the
   INTERPRETER-NAME, dispatching on the ``Interpreter'' class,
   incorporates the BODY forms in the method viscerals, while returning
   no values with an appended ``(values)'' form."
  (let ((task-kind-name (gensym)))
    (declare (type symbol task-kind-name))
    `(defmethod dispatch-task
         ((,task-kind-name   (eql ,task-kind))
          (,task-name        Task)
          (,interpreter-name Interpreter))
       (declare (type task-kind   ,task-kind-name)
                (ignore           ,task-kind-name))
       (declare (type Task        ,task-name)
                (ignorable        ,task-name))
       (declare (type Interpreter ,interpreter-name)
                (ignorable        ,interpreter-name))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-task-dispatch :add (task interpreter)
  (with-interpreter (interpreter)
    (incf $accumulator
      (get-memory-cell-at interpreter task))))

;;; -------------------------------------------------------

(define-task-dispatch :subtract (task interpreter)
  (with-interpreter (interpreter)
    (decf $accumulator
      (get-memory-cell-at interpreter task))))

;;; -------------------------------------------------------

(define-task-dispatch :multiply (task interpreter)
  (with-interpreter (interpreter)
    (setf $accumulator
      (* $accumulator
         (get-memory-cell-at interpreter task)))))

;;; -------------------------------------------------------

(define-task-dispatch :divide (task interpreter)
  (with-interpreter (interpreter)
    (setf $accumulator
      (round $accumulator
             (get-memory-cell-at interpreter task)))))

;;; -------------------------------------------------------

(define-task-dispatch :equal (task interpreter)
  (adjust-task-execution-flag interpreter task #'=))

;;; -------------------------------------------------------

(define-task-dispatch :not-equal (task interpreter)
  (adjust-task-execution-flag interpreter task #'/=))

;;; -------------------------------------------------------

(define-task-dispatch :less-than (task interpreter)
  (adjust-task-execution-flag interpreter task #'<))

;;; -------------------------------------------------------

(define-task-dispatch :greater-than (task interpreter)
  (adjust-task-execution-flag interpreter task #'>))

;;; -------------------------------------------------------

(define-task-dispatch :in (task interpreter)
  (with-interpreter (interpreter)
    (format        *standard-output* "~&>> ")
    (finish-output *standard-output*)
    (set-memory-cell-at interpreter task
      (query-for-numeric-input))
    (clear-input *standard-input*)))

;;; -------------------------------------------------------

(define-task-dispatch :in$ (task interpreter)
  (with-interpreter (interpreter)
    (format        *standard-output* "~&>> ")
    (finish-output *standard-output*)
    (set-memory-cell-at interpreter task
      (char-code
        (read-char *standard-input* NIL #\Null)))
    (clear-input *standard-input*)))

;;; -------------------------------------------------------

(define-task-dispatch :out (task interpreter)
  (with-interpreter (interpreter)
    (format *standard-output* "~d"
      (get-memory-cell-at interpreter
        (task-argument task)))))

;;; -------------------------------------------------------

(define-task-dispatch :out$ (task interpreter)
  (with-interpreter (interpreter)
    (format *standard-output* "~c"
      (code-char
        (mod
          (get-memory-cell-at interpreter
            (task-argument task))
          256)))))

;;; -------------------------------------------------------

(define-task-dispatch :acc (task interpreter)
  (with-interpreter (interpreter)
    (setf $accumulator
      (resolve-argument interpreter
        (task-argument task)))))

;;; -------------------------------------------------------

(define-task-dispatch :sto (task interpreter)
  (with-interpreter (interpreter)
    (set-memory-cell-at interpreter task $accumulator)))

;;; -------------------------------------------------------

(define-task-dispatch :ret (task interpreter)
  (with-interpreter (interpreter)
    (setf $accumulator
      (get-memory-cell-at interpreter task))))

;;; -------------------------------------------------------

(define-task-dispatch :push (task interpreter)
  (with-interpreter (interpreter)
    (push
      (retrieve-instruction-by-id $instruction-registry
        (resolve-argument interpreter
          (task-argument task)))
      $call-stack)))

;;; -------------------------------------------------------

(defun process-task (interpreter task)
  "Processes the TASK in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Task        task))
  (dispatch-task
    (task-kind task)
    task
    interpreter)
  (values))

;;; -------------------------------------------------------

(defun select-next-instruction (interpreter)
  "Pops the top instruction from the INTERPRETER's call stack, memorizes
   the same in its context, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $current-instruction
      (pop $call-stack)))
  (values))

;;; -------------------------------------------------------

(defun execute-current-instruction (interpreter)
  "Executes the tasks comprising the INTERPRETER's current instruction
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (select-next-instruction interpreter)
    
    ;(format *standard-output* "****~&~a~%****~%" $current-instruction)
    
    (loop
      while $executes-next-task-p
      
      for next-task
        of-type Task
        in      (instruction-tasks $current-instruction)
      do
        (process-task interpreter next-task)
      
      finally
        (setf $executes-next-task-p T)))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Junk program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-completed-p interpreter) do
    (execute-current-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Junk (code)
  "Interprets the piece of Junk source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (prepare-interpreter
      (parse-program
        (prepare-parser
          (prepare-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A" to the standard output.
(interpret-Junk
  "[1 | out$ 0]
   [2 | acc 65, sto 0]")

;;; -------------------------------------------------------

;; Print "Hello World!" to the standard output.
;; 
;; The following memory layout applies to this program:
;; 
;;   ------------------------------------------------------------------
;;   Memory  | Initial | Role
;;   address | content | 
;;   --------+---------+-----------------------------------------------
;;   0       | 12      | Tally of remaining characters to print.
;;           |         | Decrements from 12 to inclusive zero (0), by
;;           |         | adminiculum of the subtrahend under the
;;           |         | address 1, until the content equals the
;;           |         | termination condition specified by the address
;;           |         | 15, that is, zero (0).
;;   ..................................................................
;;   1       | 1       | Subtrahend to decrement the remaining tally
;;           |         | of characters, located under the address 0,
;;           |         | by in order to ultimately terminate the
;;           |         | program.
;;   ..................................................................
;;   2       | 3       | Address of the next character to print.
;;           |         | Commences at 3 because the first message
;;           |         | character, "H", is located at this position;
;;           |         | which please see below.
;;   ..................................................................
;;   3       | 72      | ASCII code of character "H".
;;   ..................................................................
;;   4       | 101     | ASCII code of character "e".
;;   ..................................................................
;;   5       | 108     | ASCII code of character "l".
;;   ..................................................................
;;   6       | 108     | ASCII code of character "l".
;;   ..................................................................
;;   7       | 111     | ASCII code of character "o".
;;   ..................................................................
;;   8       | 32      | ASCII code of space character.
;;   ..................................................................
;;   9       | 87      | ASCII code of character "W".
;;   ..................................................................
;;   10      | 111     | ASCII code of character "o".
;;   ..................................................................
;;   11      | 114     | ASCII code of character "r".
;;   ..................................................................
;;   12      | 108     | ASCII code of character "l".
;;   ..................................................................
;;   13      | 100     | ASCII code of character "d".
;;   ..................................................................
;;   14      | 33      | ASCII code of character "!".
;;   ..................................................................
;;   15      | 0       | Value to juxtapose the tally of remaining
;;           |         | characters, stored in the address 0, with in
;;           |         | order to terminate the program if both values
;;           |         | conflate to zero (0).
;;   ------------------------------------------------------------------
(interpret-Junk
  "[0|ret 0, - 1, sto 0, 0, + 2, out$ @, + 1, sto 2, ret 0, > 15, push 0]
   [1|32, sto 8, 101, sto 4, 72, sto 3]
   [2|33, sto 14, 100, sto 13, 108, sto 12, sto 6, sto 5, 114, sto 11, 111, sto 10, sto 7, 87, sto 9]
   [3|12, sto 0, 1, sto 1, 3, sto 2, 0, sto 15]")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-Junk
  "[1 | in$ 1, ret 1, ~ 0, out$ 1, push 1]
   [2 | acc 0, sto 0]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Junk
  "[1 | out 2, ~ 0, push 1]
   [2 | in 2, ret 2]
   [3 | 1, sto 1]
   [4 | 0, sto 0]")
