;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Turing-machine", presented by the Esolang user "A" in the
;; year 2018, being designed as an explicit realization of the Turing
;; machine concept for programming purposes.
;; 
;; 
;; Concept
;; =======
;; The Turing-machine programming language specifies its programs in the
;; form of transition rules, directed from a source compound
;; (condition, symbol) to a target pair concording with the same
;; structure, concomitantly engaging in a translation of the memory
;; pointer, known as the "tape head".
;; 
;; == THE TURING MACHINE MODEL: STATE MACHINE, TAPE, AND RULES ==
;; One of the most significant aspects of computer science, the
;; computational model known as the "Turing machine" describes computer
;; programs in terms of a state machine and a tape with a head cursor,
;; which operate in champarty, governed by a rule set.
;; 
;; == THE TURING MACHINE MODEL: STATES ENUMERATE POSSIBLE SITUATIONS ==
;; The static component is realized in a state machine, an automaton
;; compact of zero or more stages, the states, supplying a description
;; of the machine's situation. At any instant, the machine resides at
;; exactly one state.
;; 
;; Two particular definitions appropriate a special role in this
;; context: Imprimis, a subset of the admissive states, known as the
;; "final states", serves as a discriminating criterion for the
;; termination and success evaluation. The arrival in a final state
;; halts the program and assesses it as successful.
;; 
;; The second entity, the initial state, must be provided in order to
;; choose the starting point in the state machine, active at the
;; program's inchoation.
;; 
;; == THE TURING MACHINE MODEL: TRANSITIONS CHANGE STATES ==
;; Each two states, or a state and itself, may be connected by a
;; transition, the motion of the same is instigated by a symbol's
;; adminiculum, serving as an event, the occasion of which produces the
;; effects.
;; 
;; == THE TURING MACHINE MODEL: AN ABSTRACT EVENT SOURCE REQUIRED ==
;; Whereas the state machine itself constitutes an independent warklume
;; of contemplation about a process, the means of its events' provision,
;; and, ensuing from this interface, the provenance of its effectuation,
;; relies upon the concrete context of the automaton's deployment.
;; 
;; For a coffee machine, as a forbisen, the human user's handling of the
;; device's button, as well as internal timing mechanisms, exert the
;; requisite forces. Other entities modeled by such a machine expect the
;; stimula for the responses' elicitation to derive aliunde. Siclike,
;; the modes and modalities of respondency --- or, the "output" ---
;; attend to the particular use case's criteria; for a coffee machine
;; this would resolve to the production of the beverage, or the
;; intelligence about an error's infliction.
;; 
;; The Turing machine, disencumbered from a physical incarnation, as
;; as well, a fortiori, conceptualized to be of the highest mete in
;; abstract expressiveness, is dependent upon a more recondite event
;; source and response propagation.
;; 
;; == THE TURING MACHINE MODEL: A TAPE ISSUES INPUT AND OUTPUT ==
;; The predicament involved in the reception of input and the
;; transmission of output is solved in a Turing machine by the memory's
;; arbitration: Capable of being initialized in its entirety with
;; "blank" cells, that is, empty symbols, the capability is extended to
;; prearrange the content in order to embrace meaningful symbols in lieu
;; of the vacancy placeholders; thus ensues the faculty for input.
;; 
;; Every transition being a composition of a source state-symbol to a
;; target state-symbol-movement, the participating symbol to transfer
;; unto the current tape cell provides the output equivalent.
;; 
;; == THE TURING MACHINE MODEL: A RULE SET GOVERNS THE BEHAVIOR ==
;; A mode similar to the static framework imposed by the state machine
;; and the tape appertains to the dynamic moeity commorant in the Turing
;; machine via its rule set.
;; 
;; A unordered sequence of zero or more rules governs the behavior, or
;; more concrete, the response to the configuration at the given
;; instant. A rule is defined as a function admissive to the current
;; state machine state and the symbol under the tape head, returning the
;; new state, the symbol to replace the cell under the tape head by, and
;; the direction into which the head shall move by a single step, if
;; any. Applying formality in a higher mete, we can define a rule r as:
;; 
;;   r := function (q[c], t[c]) -> (q[n], t[n], d)
;; 
;; where
;; 
;;   r    --- the rule to be defined
;;   q[c] --- the current state machine state
;;   t[c] --- the symbol in the cell under the tape head
;;   q[n] --- the new state machine state to assume
;;   t[n] --- the new symbol to write into the cell under the tape head
;;   d    --- the direction to move the tape head into, being either of
;;              L    for left
;;              R    for right
;;              none for no motion.
;; 
;; Enhanced stringency in the formulation will follow in the subsequent
;; section as a successor to the rather informal explications; the
;; following will procure an elucidation with more stringent lealty to
;; the mathematical expectancies.
;; 
;; == THE TURING MACHINE MODEL: A FORMAL DELINEATION ==
;; Deploying a formal diction, a Turing machine may be expressed as a
;; 7-tuple
;; 
;;   (Q, q0, F, T, b, I, d)
;; 
;; compact of the following constituents:
;; 
;;   ------------------------------------------------------------------
;;   Component | Role
;;   ----------+-------------------------------------------------------
;;   Q         | A finite set of the state machine's states.
;;   ..................................................................
;;   q0        | The initial state of the state machine.
;;             | q0 must be an element of the recognized state set Q.
;;   ..................................................................
;;   F         | The finite set of final states.
;;             | Upon the state machine's transition into any state of
;;             | F, the input string is considered accepted.
;;   ..................................................................
;;   T         | The tape alphabet.
;;             | Specifies the set of symbols that may be written to
;;             | the tape.
;;   ..................................................................
;;   b         | The blank symbol.
;;             | This is the default value resident in each tape cell
;;             | at the machine operation's start. It holds:
;;             |   b is an element of the tape alphabet T.
;;             | It resembles in purpose and function the "null" or
;;             | "NIL" value is some programming languages.
;;   ..................................................................
;;   I         | The input alphabet.
;;             | Determines the set of symbols that may be input, that
;;             | is, which may the present in the initial tape. It
;;             | holds:
;;             |   I is a subset of the tape alphabet T, excluding the
;;             |     blank symbol b.
;;   ..................................................................
;;   d         | A transition function which maps
;;             |   Q x T -> Q x T x {L, R}.
;;             | Depending on its present state (element of Q) and the
;;             | present tape alphabet (element of T), being determined
;;             | by the tape head location, the state machine will
;;             | change into the new state (element of Q), potentially
;;             | replace the symbol currently at the tape head (element
;;             | of T), and move the tape head either left (L), right
;;             | (R), or not at all (epsilon).
;;   ------------------------------------------------------------------
;; 
;; == THE TURING MACHINE MODEL: READ, WRITE, MOVE, REPEAT ==
;; A Turing machine proceeds by reading the current state and tape
;; symbol, transitioning into the new state, writing the new symbol, and
;; moving the tape head. If no transition is possible, the program has
;; failed; if a final state has been reached, the program succeeds,
;; otherwise the process repeats.
;; 
;; The concept shall be limned in more detailed alow:
;; 
;;   (1) The current state machine state q[c] is read.
;;       At the program's inchoation, q[c] equals the designated start
;;       state q0.
;;   (2) The symbol t[c] at the cell under the tape head is read.
;;   (3) The rule d[c] matching the source state-symbol tuple
;;       (q[c], t[c]) is searched.
;;   (4) If no such rule d[c] can be detected, the program must be
;;       reckoned as invalid, the machine subsequently failing.
;;   (5) If the rule d[c] for the current state q[c] and current tape
;;       symbol t[c] can be detected, its return value is obtained,
;;       comprehending the triple
;;         q[n] --- the new state for the state machine
;;         t[n] --- the symbol to replace the current cell value by
;;         m    --- the direction to move the tape head by one step,
;;                  being either left, right, or none at all.
;;   (6) Consequently,
;;         (a) the state machine transitions from the current state q[c]
;;             to the new state q[n],
;;         (b) the symbol t[n] is written to the cell under the tape
;;             head's current location, and
;;         (c) the tape head, following the writing action, depending
;;             upon the movement instruction m, either moves one cell to
;;             the left, one cell to the right, or not all all.
;;   (7) If the new state q[n] belongs to one of the designated final
;;       states F, the program halts successfully; otherwise, the
;;       process repeats with the step (1), which please see.
;; 
;; == THE TURING-MACHINE LANGUAGE: PROGRAMS SPECIFY TRANSITION RULES ==
;; The Turing-machine language employs four states and an infinite tape
;; of bits, driving programs by a programmer-provided rule set.
;; 
;; == THE TURING-MACHINE LANGUAGE: APPROPRIATING THE DESIGN ==
;; The Turing-machine language's architecture exhibits a strong
;; conformance to the model: operating on a finite state machine in
;; conjunction with an infinite tape.
;; 
;; == THE TURING-MACHINE LANGUAGE: LEAL TO THE RULE SET DESIGN ==
;; A consectary of its status as the Turing machine model's realization,
;; the Turing-machine programming language adheres to the basic
;; principles, while deviating in some formal aspects of its entheus.
;; 
;; Each statement's onus consists of the specification of the source
;; condition and symbol, followed by the new condition and symbol,
;; concluded by the head motion. Its fidelity to the abstract model
;; remains impeccable in this rule set accommodation.
;; 
;; == THE TURING-MACHINE LANGUAGE: FIXED STATES AND SYMBOLS ==
;; In counterdistinguishment from the automaton, the set of recognized
;; states, the final states, and the initial state, as well as the tape
;; alphabet, cannot be provided in a liberal fashion, imposing a fixture
;; by the language standard.
;; 
;; Only four states exist: "Q", "E", "O", and "F", with the desinent
;; allotted the construe as the final state, the arrival at which halts
;; the program successfully. The starting point is extracted from the
;; source state of the first rule definition.
;; 
;; The tape alphabet is exhausted by the binary symbol "0" and "1",
;; destitute of a blank symbol.
;; 
;; == THE TURING-MACHINE LANGUAGE: AN INFINITE LOOP OF RULE TESTS ==
;; Programs in the Turing-machine language are expressed as a sequence
;; of zero or more rules, specified by a quintuple of
;; 
;;   (1) the state machine's current state, or nevened "condition" in
;;       this language
;;   (2) the symbol at the tape's head location
;;   (3) the new state, or condition, to transmit the state machine
;;       into
;;   (4) the direction into to move the tape's head by one step
;;   (5) the new symbol to place into the tape's head location.
;; 
;; Upon settling into the final state "F", and the program's subsequent
;; halting, the tape is printed to the standard output.
;; 
;; == TURING-MACHINE AND TURING MACHINE: MAPPING LANGUAGE AND MODEL ==
;; The Turing-machine language's foundry registered at the eponymous
;; abstraction, reason redes to pursue the affiliation of its
;; components:
;; 
;;   ------------------------------------------------------------------
;;   Component | Turing-machine language equivalent
;;   ----------+-------------------------------------------------------
;;   Q         | The four possible states
;;             |   Q = {"Q", "E", "O", "F"}
;;   ..................................................................
;;   q0        | Resorts to the source condition (state) in the first
;;             | transition rule specified by the Turing-machine
;;             | program, that is:
;;             |   q0 = rules[0].sourceCondition
;;             | Unspecified, and otiose, for an empty program.
;;   ..................................................................
;;   F         | The dedicated singleton set
;;             |   F = {"F"}
;;   ..................................................................
;;   T         | The binary character set
;;             |   T = {"0", "1"}
;;   ..................................................................
;;   b         | Confluent with the default tape value "0".
;;   ..................................................................
;;   I         | Paregal to the binary character set T:
;;             |   I = {"0", "1"}
;;   ..................................................................
;;   d         | Determined by the Turing-machine program rules.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; Compatible with the Turing machine model, the language operates on a
;; twain of coefficient automata: a finite state machine and an infinite
;; tape, albeit specialized to the pragmatism of its deployment.
;; 
;; == A FINITE STATE MACHINE ==
;; The Turing-machine programming language employs a state machine
;; amplecting a fixed set of four states, "Q", "E", "O", and "F", the
;; desinent one of which acts as the final state, whose assumption
;; determines the program's successful termination.
;; 
;; Whereas the final state resolves to the fixed "F", the initial one
;; equals that of the first rule specified.
;; 
;; == AN INFINITE TAPE OF BITS ==
;; Deploying an infinite tape, siclike to the state machine, the symbol
;; set's circumference is exhausted by the language standard, potent
;; only to incorporate the bit digits "0" and "1". All tape cells are
;; initialized to zero (0), if not provided with a dedicated input tape.
;; 
;; A mobile tape head refers to the currently active cell, amenable to
;; indagations and modifications.
;; 
;; 
;; Data Types
;; ==========
;; The Turing-machine language's type system lays its compass merely
;; around a single type, the set of bit numbers zero (0) and one (1).
;; 
;; 
;; Syntax
;; ======
;; Turing-machine's syntactical design is established upon a conspicuous
;; homogeneity, with statements being compact of exactly six effective
;; characters, homologating whitespaces for augmented aesthetics.
;; 
;; == INSTRUCTIONS ==
;; Each instruction constitutes a compound of six characters, these
;; being recepients of a construe in accordance with their positional
;; instalment.
;; 
;; The following table shall be adhibited to obtain a deeper gnarity
;; anenst the positions, tokens, and their roles.
;; 
;;   ------------------------------------------------------------------
;;   Pos. | Role                        | Possible values
;;   -----+-----------------------------+------------------------------
;;     1  | Current condition (state)   | Q
;;        |                             |..............................
;;        |                             | E
;;        |                             |..............................
;;        |                             | O
;;        |                             |..............................
;;        |                             | F
;;   ==================================================================
;;     2  | Current symbol on tape      | 0
;;        |                             |..............................
;;        |                             | 1
;;   ==================================================================
;;     3  | Separator current--new      | :
;;   ==================================================================
;;     4  | New condition (state)       | Q
;;        |                             |..............................
;;        |                             | E
;;        |                             |..............................
;;        |                             | O
;;        |                             |..............................
;;        |                             | F
;;   ==================================================================
;;     5  | New symbol to write on tape | 0
;;        |                             |..............................
;;        |                             | 1
;;   ==================================================================
;;     6  | Tape head movement          | <
;;        |                             |..............................
;;        |                             | >
;;        |                             |..............................
;;        |                             | -
;;   ------------------------------------------------------------------
;; 
;; == CHARACTER REPERTOIRE ==
;; Turing-machine's very restricted syntactical compass shall be the
;; following purposeful illustration's cynosure, allocating to each of
;; the three categories --- conditions, symbols, and directions --- the
;; homologated character forms.
;; 
;;   ------------------------------------------------------------------
;;   Species   | Character | Remarks
;;   ----------+-----------+-------------------------------------------
;;   Condition | Q         | None.
;;             |.......................................................
;;             | E         | None.
;;             |.......................................................
;;             | O         | None.
;;             |.......................................................
;;             | F         | If this condition is assumed, the program
;;             |           | halts, and the tape state is printed to
;;             |           | the standard output.
;;   ==================================================================
;;   Symbol    | 0         | None.
;;             |.......................................................
;;             | 1         | None.
;;   ==================================================================
;;   Direction | <         | Moves the tape head left.
;;             |.......................................................
;;             | >         | Moves the tape head right.
;;             |.......................................................
;;             | -         | Does not move the tape head.
;;   ------------------------------------------------------------------
;; 
;; == WHITESPACES ==
;; Whitespaces, encompassing in their diorism the space, horizontal tab,
;; as well as the newline specimen, may be inserted liberally.
;; 
;; == GRAMMAR ==
;; The language's donat may be formulated in the Extended Backus-Naur
;; Form (EBNF) as follows:
;; 
;;   program   := { rule } ;
;;   rule      := condition , symbol , ":"
;;             ,  condition , direction , symbol
;;             ;
;;   condition := "Q" | "E" | "O" | "F" ;
;;   symbol    := "0" | "1" ;
;;   direction := "<" | ">" | "-" ;
;; 
;; 
;; Instructions
;; ============
;; All effect in the language ensues from a single statement, serving as
;; a Turing machine rule, the four parameters of which instil the
;; diversity in causata, which incorporates the transitioning betwixt
;; states, modifications to the tape, and a contingent halting.
;; 
;; A statement adheres to the following forbisen, which designates the
;; variable portions by an underline composed of carets ("^"):
;; 
;;   currentCondition currentSymbol : nextCondition newSymbol headMove
;;   ^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^   ^^^^^^^^^^^^^ ^^^^^^^^^ ^^^^^^^^
;; 
;; The quadruple parameter list is comprised of:
;; 
;;   ------------------------------------------------------------------
;;   Parameter        | Type      | Role
;;   -----------------+-----------+------------------------------------
;;   currentCondition | condition | The current state machine requisite
;;                    |           | for the rule to be match.
;;   ..................................................................
;;   currentCondition | symbol    | The symbol under the tape head
;;                    |           | requisite for the rule to be match.
;;   ..................................................................
;;   nextCondition    | condition | The state to set the state machine
;;                    |           | to, if the rule matches.
;;   ..................................................................
;;   newSymbol        | symbol    | The symbol to write into the cell
;;                    |           | under the tape head, replacing the
;;                    |           | current content, if the rule
;;                    |           | matches.
;;   ..................................................................
;;   headMove         | direction | The direction into to move the tape
;;                    |           | head by one step, if the rule
;;                    |           | matches.
;;   ------------------------------------------------------------------
;; 
;; The following options appertain to the parameters:
;; 
;;   ------------------------------------------------------------------
;;   Type      | Value     | Remarks
;;   ----------+-----------+-------------------------------------------
;;   Condition | Q         | None.
;;             |.......................................................
;;             | E         | None.
;;             |.......................................................
;;             | O         | None.
;;             |.......................................................
;;             | F         | If this condition is assumed, the program
;;             |           | halts, and the tape state is printed to
;;             |           | the standard output.
;;   ==================================================================
;;   Symbol    | 0         | None.
;;             |.......................................................
;;             | 1         | None.
;;   ==================================================================
;;   Direction | <         | Moves the tape head left.
;;             |.......................................................
;;             | >         | Moves the tape head right.
;;             |.......................................................
;;             | -         | Does not move the tape head.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A sparse set only of uncertain aspects can be implicated in the
;; protolog's state, with a selected portion exposed in the following
;; sections.
;; 
;; == HOW DOES THE PROGRAM RESPOND TO INVALID TRANSITIONS? ==
;; The direct statement of the expected behavior upon an attempted
;; transition into an undefined condition-symbol configuration lacks in
;; the specification. Natheless, in the face of the Turing-machine
;; language's consanguinity to the inspiring computational model, it has
;; been deemed that the occasion corresponds to the issuance of an error
;; of an unspecified type.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter has been implemented in Common Lisp, based upon a
;; rather emarginate correlation of the lexer and parser entities, but
;; a stark delineation betwixt this compound and the interpreter's
;; bailiwick.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-03-14
;; 
;; Sources:
;;   [esolang2020turingmachinelang]
;;   The Esolang contributors, "Turing-machine", 2020
;;   URL: "https://esolangs.org/wiki/Turing-machine"
;;   
;;   [krumins2023busybeaver]
;;   Peter Krumins, "The Busy Beaver Problem", 2023
;;   URL: "https://catonmat.net/busy-beaver"
;;   Notes:
;;     - Describes the busy beaver problem.
;;     - Enumerates the growth, listing exemplary results for the
;;       function B(n), with n being the number of states available to
;;       the Turing machine:
;;         B(1) = 1
;;         B(2) = 4
;;         B(3) = 6
;;         B(4) = 13
;;         B(5) = 4098
;;         B(6) = 4.6 * 10^1439
;;     - In our case, the entry B(3) = 6 is of significance.
;;   
;;   [mullins2012turingmachinexampleprogs]
;;   Robert Mullins, "Section 4: Turing Machine Example Programs", 2012
;;   URL: "https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/
;;         turing-machine/four.html"
;;   Notes:
;;     - Presents several Turing machine example programs, including a
;;       binary counter and a 3-state busy beaver.
;;   
;;   [mullins2012whatisturingmachine]
;;   Robert Mullins, "What is a Turing machine?", 2012
;;   URL: "https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/
;;         turing-machine/one.html"
;;   Notes:
;;     - Description of the Turing machine model, with a special focus
;;       on its binary tape design.
;;   
;;   [tuteja2023turingmachine]
;;   Sonal Tuteja, "Turing Machine in TOC", 22 Feb, 2023
;;   URL: "https://www.geeksforgeeks.org/turing-machine-in-toc/"
;;   Notes:
;;     - Description of the Turing machine model, encompassing a
;;       formal diorism.
;;   
;;   [tutorialspoint2023turingmachineintro]
;;   The Tutorials Point contributors, "Turing Machine Introduction",
;;                                     2023 
;;   URL: "https://www.tutorialspoint.com/automata_theory/
;;         turing_machine_introduction.htm"
;;   Notes:
;;     - Description of the Turing machine model, encompassing a
;;       formal diorism.
;;     - Presents a simple example.
;;   
;;   [wikipedia2023turingmachine]
;;   The Wikipedia contributors, "Turing machine", 2023
;;   URL: "https://en.wikipedia.org/wiki/Turing_machine"
;;   Notes:
;;     - Throughout description of the Turing machine model,
;;       encompassing a formal diorism.
;;     - Demonstrates the 3-state busy beaver problem as an example.
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
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype rule-set ()
  "The ``rule-set'' type defines a list of zero or more ``Rule''
   objects."
  '(list-of Rule))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype rule-map ()
  "The ``rule-map'' defines a mapping from a combination of source
   condition and symbol to a rule, comprehending, among others, the
   target condition, symbol to write, and tape head direction,
   manifesting in a hash table which associates the source
   condition-symbol twain in string form to a ``Rule'' instance."
  '(hash-table-of (string 2) Rule))

;;; -------------------------------------------------------

(deftype cell-map ()
  "The ``cell-map'' defines a mapping of tape cell indices to the
   symbols located at the respective positions, realized as a hash table
   of zero or more entries, each key of which specifies the cell index
   as a signed integer, amenable to a single character as the cell's
   symbol."
  '(hash-table-of integer character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun condition-name-p (candidate)
  "Determines whether the CANDIDATE represents a condition name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "QEOF" :test #'char=)))))

;;; -------------------------------------------------------

(defun symbol-name-p (candidate)
  "Determines whether the CANDIDATE represents a symbol name, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "01" :test #'char=)))))

;;; -------------------------------------------------------

(defun direction-name-p (candidate)
  "Determines whether the CANDIDATE represents a direction identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "<>-" :test #'char=)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate
        '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun eof-p (source position)
  "Determines whether the POSITION violates the SOURCE's boundaries,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (array-in-bounds-p source position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Rule".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Rule
  "The ``Rule'' class models a rule or transition defined on a Turing
   machine, translating the automaton from its current condition, or
   state, if encountered with a specified symbol, to a new condition,
   replacing the current tape cell symbol with a new one, and moving the
   tape head in the desiderated direction."
  (current-condition
    (error "Missing current condition.") :type character)
  (current-symbol
    (error "Missing current symbol.")    :type character)
  (next-condition
    (error "Missing next condition.")    :type character)
  (next-symbol
    (error "Missing next symbol.")       :type character)
  (direction
    (error "Missing direction.")         :type character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Commencing at the START position in the SOURCE, skips a sequence of
   zero or more whitespaces, and returns the position of the first
   non-whitespace character in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for position
        of-type fixnum
        from    start
        by      1
        below   (length source)
      while (and (not (eof-p source position))
                 (whitespace-character-p (char source position)))
      finally
        (return position))))

;;; -------------------------------------------------------

(defun read-condition (source start)
  "Commencing at the START position in the SOURCE, reads a condition
   name and returns two values:
     (1) the consumed condition name as a character
     (2) the position in the SOURCE immediately succeeding the consumed
         condition name."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (the (values character fixnum)
      (cond
        ((eof-p source position)
          (error "Expected a condition at position ~d, but encountered
                  EOF."
            start))
        ((condition-name-p (char source position))
          (values
            (char source position)
            (1+ position)))
        (T
          (error "Expected a condition at position ~d, ~
                  but encountered the character ~c."
            start (char source position)))))))

;;; -------------------------------------------------------

(defun read-symbol (source start)
  "Commencing at the START position in the SOURCE, reads a tape symbol
   name and returns two values:
     (1) the consumed symbol name as a character
     (2) the position in the SOURCE immediately succeeding the consumed
         symbol name."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (the (values character fixnum)
      (cond
        ((eof-p source position)
          (error "Expected a symbol at position ~d, but encountered ~
                  EOF."
            start))
        ((symbol-name-p (char source position))
          (values
            (char source position)
            (1+ position)))
        (T
          (error "Expected a symbol at position ~d, but encountered ~
                  the character ~c."
            start (char source position)))))))

;;; -------------------------------------------------------

(defun read-direction (source start)
  "Commencing at the START position in the SOURCE, reads a direction
   name and returns two values:
     (1) the consumed direction name as a character
     (2) the position in the SOURCE immediately succeeding the consumed
         direction name."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (the (values character fixnum)
      (cond
        ((eof-p source position)
          (error "Expected a direction at position ~d, ~
                  but encountered EOF."
            start))
        ((direction-name-p (char source position))
          (values
            (char source position)
            (1+ position)))
        (T
          (error "Expected a direction at position ~d, but ~
                  encountered the character ~c."
            start (char source position)))))))

;;; -------------------------------------------------------

(defun expect-colon (source start)
  "Expects a colon (\":\") at the START position in the SOURCE, on
   confirmation returning the position immediately suceeding the colon
   in the SOURCE, otherwise signaling an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (the fixnum
      (cond
        ((eof-p source position)
          (error "Expected a colon (\":\") at position ~d, ~
                  but encountered EOF."
            start))
        ((char= (char source position) #\:)
          (1+ position))
        (T
          (error "Expected a colon (\":\") at position ~d, but ~
                  encountered the character ~c."
            start (char source position)))))))

;;; -------------------------------------------------------

(defun parse-rule (source start)
  "Commencing at the START position in the SOURCE, parses a single rule
   and returns two values:
     (1) the detected rule as a ``Rule'' object
     (2) the position in the SOURCE immediately succeeding the rule's
         extent."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position start))
    (declare (type fixnum position))
    (let ((current-condition NIL)
          (current-symbol    NIL)
          (next-condition    NIL)
          (direction         NIL)
          (next-symbol       NIL))
      (declare (type (or null character) current-condition))
      (declare (type (or null character) current-symbol))
      (declare (type (or null character) next-condition))
      (declare (type (or null character) direction))
      (declare (type (or null character) next-symbol))
      
      (setf (values current-condition position)
        (read-condition source position))
      (setf (values current-symbol position)
        (read-symbol source position))
      (setf position (expect-colon source position))
      (setf (values next-condition position)
        (read-condition source position))
      (setf (values direction position)
        (read-direction source position))
      (setf (values next-symbol position)
        (read-symbol source position))
      
      (the (values Rule fixnum)
        (values
          (make-rule
            :current-condition current-condition
            :current-symbol    current-symbol
            :next-condition    next-condition
            :next-symbol       next-symbol
            :direction         direction)
          position)))))

;;; -------------------------------------------------------

(defun parse-rules (source)
  "Parses the SOURCE and returns a list of zero or more rules."
  (declare (type string source))
  (let ((rules    NIL)
        (position 0))
    (declare (type rule-set rules))
    (declare (type fixnum   position))
    (flet
        ((collect-rule (new-rule new-position)
          "Inserts the NEW-RULE at the front of the RULES list, assigns
           the POSITION cursor to the NEW-POSITION, and returns no
           value."
          (declare (type Rule   new-rule))
          (declare (type fixnum new-position))
          (push new-rule rules)
          (setf position new-position)
          (values)))
      (loop do
        (setf position (skip-whitespaces source position))
        (if (eof-p source position)
          (loop-finish)
          (multiple-value-call #'collect-rule
            (parse-rule source position)))))
    (the rule-set
      (nreverse rules))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Stage".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Stage
  (:constructor make-stage (condition symbol)))
  "The ``Stage'' class encapsulates a tuple compact of a condition
   (state) and a symbol."
  (condition NIL :type (or null character))
  (symbol    NIL :type (or null character)))

;;; -------------------------------------------------------

(defun stage-completely-specified-p (stage)
  "Determines whether the STAGE's information can be assayed as
   complete, that is, composed of a non-``NIL'' condition and a
   non-``NIL'' symbol, on confirmation returning a ``boolean'' value of
   ``T'', otherwise responding with ``NIL''."
  (declare (type Stage stage))
  (the boolean
    (not (null
      (and (stage-condition stage)
           (stage-symbol    stage))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Rule-Table".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-rule-key (rule)
  "Creates and returns a rule-table-compatible key from the RULE's
   source condition and symbol."
  (declare (type Rule rule))
  (the (string 2)
    (with-output-to-string (key)
      (declare (type string-stream key))
      (write-char (rule-current-condition rule) key)
      (write-char (rule-current-symbol    rule) key))))

;;; -------------------------------------------------------

(defun build-stage-key (stage)
  "Creates and returns a rule-table-compatible key from the STAGE's
   condition and symbol."
  (declare (type Stage stage))
  (the (string 2)
    (with-output-to-string (key)
      (declare (type string-stream key))
      (write-char (stage-condition stage) key)
      (write-char (stage-symbol    stage) key))))

;;; -------------------------------------------------------

(defclass Rule-Table ()
  ((rules
    :initarg       :rules
    :initform      (make-hash-table :test #'equal)
    :type          rule-map
    :documentation "Maps each (sourceCondition . sourceSymbol) pair,
                    designed as a two-character string, to the
                    represented rule.")
   (initial-rule
    :initarg       :initial-rule
    :initform      NIL
    :type          (or null Rule)
    :documentation "The first rule transmitted to the rule table,
                    serving as the initial condition."))
  (:documentation
    "The ``Rule-Table'' class serves in the castaldy of zero or more
     transition rules, each retrievable by a string representation of
     its source condition and symbol, maintained in conjunction with the
     initial rule, if extant."))

;;; -------------------------------------------------------

(defun rule-table-initial-rule (rule-table)
  "Returns the RULE-TABLE's initial condition in the form of its
   entailing rule, or ``NIL'' upon the RULE-TABLE's vacancy."
  (declare (type Rule-Table rule-table))
  (the (or null Rule)
    (slot-value rule-table 'initial-rule)))

;;; -------------------------------------------------------

(defun rule-table-rules (rule-table)
  "Returns a reference to the RULE-TABLE's rule map."
  (declare (type Rule-Table rule-table))
  (the rule-map
    (slot-value rule-table 'rules)))

;;; -------------------------------------------------------

(defun rule-table-has-rule-p (rule-table stage)
  "Determines whether the RULE-TABLE comprehends an entry amenable to
   the STAGE, the same specifies a source condition and current symbol,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Rule-Table rule-table))
  (declare (type Stage stage))
  (the boolean
    (not (null
      (nth-value 1
        (gethash (build-stage-key stage)
          (rule-table-rules rule-table)))))))

;;; -------------------------------------------------------

(defun rule-table-get-rule (rule-table stage)
  "Returns for the STAGE, specifying a source condition and a current
   symbol, the respective rule in the RULE-TABLE; or signals an error in
   the case of a disrespondency."
  (declare (type Rule-Table rule-table))
  (declare (type Stage stage))
  (the Rule
    (or
      (nth-value 0
        (gethash (build-stage-key stage)
          (rule-table-rules rule-table)))
      (error "No rule associated with the stage ~a." stage))))

;;; -------------------------------------------------------

(defun rule-table-empty-p (rule-table)
  "Determines whether the RULE-TABLE is empty, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Rule-Table rule-table))
  (the boolean
    (not (null
      (zerop
        (hash-table-count
          (slot-value rule-table 'rules)))))))

;;; -------------------------------------------------------

(defun build-rule-table (rule-set)
  "Creates and returns a new ``Rule-Table'' derived from the list of
   rules RULE-SET.
   ---
   If non-empty, the source condition in the RULE-SET's first rule is
   appropriated as an indicator of the Turing machine's initial state."
  (declare (type rule-set rule-set))
  (let ((rule-table (make-instance 'Rule-Table)))
    (declare (type Rule-Table rule-table))
    
    (when rule-set
      ;; Specify the initial rule for its initial condition (state).
      (setf (slot-value rule-table 'initial-rule)
            (first rule-set))
      
      ;; Register each RULE-SET rule by its source condition and symbol
      ;; in the RULE-TABLE's RULES map.
      (dolist (rule rule-set)
        (declare (type Rule rule))
        (setf (gethash (build-rule-key rule)
                       (slot-value rule-table 'rules))
              rule)))
    
    (the Rule-Table rule-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-empty-tape ()))
  "The ``Tape'' class represents a bilaterally infinite extent of cells,
   compact of binary symbols in character form, with a movable cursor,
   known as the \"head\", at any instant selecting the active
   component."
  (cells         (make-hash-table :test #'eql) :type cell-map)
  (head          0                             :type integer)
  (lowest-index  0                             :type integer)
  (highest-index 0                             :type integer))

;;; -------------------------------------------------------

(defmacro with-tape ((tape) &body body)
  "Evaluates the TAPE, binds its slots ``cells'', ``head'',
   ``lowest-index'' and ``highest-index'' to eponymous local symbol
   macros, processes the BODY forms, and returns the last evaluated
   form's results."
  (let ((evaluated-tape (gensym)))
    (declare (type symbol evaluated-tape))
    `(let ((,evaluated-tape ,tape))
       (declare (type Tape ,evaluated-tape)
                (ignorable ,evaluated-tape))
       (symbol-macrolet
           ((cells         (the cell-map
                             (tape-cells ,evaluated-tape)))
            (head          (the integer
                             (tape-head  ,evaluated-tape)))
            (lowest-index  (the integer
                             (tape-lowest-index ,evaluated-tape)))
            (highest-index (the integer
                             (tape-highest-index ,evaluated-tape))))
         (declare (type cell-map cells)
                  (ignorable     cells))
         (declare (type integer  head)
                  (ignorable     head))
         (declare (type integer  lowest-index)
                  (ignorable     lowest-index))
         (declare (type integer  highest-index)
                  (ignorable     highest-index))
         ,@body))))

;;; -------------------------------------------------------

(defun make-tape-of (start-index initial-symbols)
  "Creates and returns a new ``Tape'' whose cells, commencing with the
   START-INDEX, are assigned the INITIAL-SYMBOLS.
   ---
   The thus produced tape's head will point to the START-INDEX."
  (declare (type integer start-index))
  (declare (type string  initial-symbols))
  (let ((tape (make-empty-tape)))
    (declare (type Tape tape))
    (with-tape (tape)
      (setf head          start-index)
      (setf lowest-index  start-index)
      (setf highest-index start-index)
      (loop
        for current-symbol of-type character across initial-symbols
        for current-index  of-type integer   from   start-index by 1
        do
          (setf (gethash current-index cells) current-symbol)
          (setf highest-index                 current-index)))
    (the Tape tape)))

;;; -------------------------------------------------------

(defun tape-current-symbol (tape)
  "Returns the symbol located at the TAPE's head."
  (declare (type Tape tape))
  (the character
    (with-tape (tape)
      (gethash head cells #\0))))

;;; -------------------------------------------------------

(defun (setf tape-current-symbol) (new-symbol tape)
  "Replaces the symbol at the TAPE's head by the NEW-SYMBOL and returns
   the modified TAPE."
  (declare (type character new-symbol))
  (declare (type Tape      tape))
  (with-tape (tape)
    (setf (gethash head cells #\0) new-symbol))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Translates the TAPE head one cell to the left and returns the
   modified TAPE."
  (declare (type Tape tape))
  (with-tape (tape)
    (decf head)
    (setf lowest-index (min lowest-index head)))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Translates the TAPE head one cell to the right and returns the
   modified TAPE."
  (declare (type Tape tape))
  (with-tape (tape)
    (incf head)
    (setf highest-index (max highest-index head)))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-stay (tape)
  "Does not translate the TAPE's head, but returns the unmodified TAPE."
  (declare (type Tape tape))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-print (tape)
  "Prints to the standard output the TAPE's explicitly set symbols and
   returns the TAPE."
  (declare (type Tape tape))
  (fresh-line)
  (with-tape (tape)
    (loop
      for cell-index
        of-type integer
        from    lowest-index
        to      highest-index
      do
        (write-char (gethash cell-index cells #\0))))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((rules
    :initarg       :rules
    :initform      (error "Missing rule table.")
    :type          Rule-Table
    :documentation "The rules registered with their source
                    condition-symbol pairing.")
   (tape
    :initarg       :tape
    :initform      (make-empty-tape)
    :type          Tape
    :documentation "The Turing machine's tape.")
   (stage
    :initarg       :stage
    :initform      (make-stage NIL NIL)
    :type          Stage
    :documentation "The current condition (state) and symbol.")
   (halted-p
    :initarg       :halted-p
    :initform      NIL
    :type          boolean
    :documentation "Determines whether the Turing machine has entered
                    the terminating \"F\" condition (state)."))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluation of a
     set of Turing-machine rules, concomitantly assigned the onus of
     maintaining the requisite state for such an endeavour."))

;;; -------------------------------------------------------

(defun interpreter-update-stage (interpreter current-condition)
  "Updates the INTERPRETER's stage by appropriation of the
   CURRENT-CONDITION and concomitant affiliation with the internally
   maintained tape's current symbol, and returns the modified
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type character   current-condition))
  (with-slots (stage tape) interpreter
    (declare (type Stage stage))
    (declare (type Tape  tape))
    (setf (stage-condition stage) current-condition)
    (setf (stage-symbol    stage) (tape-current-symbol tape)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-check-for-termination (interpreter)
  "Determines whether the INTERPRETER's current stage designates the
   Turing-machine program as halted, which is only the case if the same
   has assumed the condition \"F\", on confirmation activating the
   respective INTERPRETER flag, otherwise remaining ineffectuous, and in
   any case returning the potentially modified INTEPRRETER."
  (declare (type Interpreter interpreter))
  (with-slots (stage halted-p) interpreter
    (declare (type Stage   stage))
    (declare (type boolean halted-p))
    (declare (ignorable    halted-p))
    (when (and (stage-condition stage)
               (char= (stage-condition stage) #\F))
      (setf halted-p T)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's stage information, determines whether
   the program is already halted, by being in the \"F\" condition, and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  
  ;; Set the stage to the rule table's initial rule.
  (with-slots (rules) interpreter
    (declare (type Rule-Table rules))
    (unless (rule-table-empty-p rules)
      (interpreter-update-stage interpreter
        (rule-current-condition
          (rule-table-initial-rule rules)))))
  
  ;; Check whether the state machine has reached the terminating "F"
  ;; condition (state)
  (interpreter-check-for-termination interpreter)
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (rules &optional (tape (make-empty-tape)))
  "Creates and returns a new ``Interpreter'' based upon the rule table
   RULES, with an optional TAPE that defaults to an empty instance.
   ---
   Please note that a custom TAPE constitutes the only possible
   provision for input into a Turing-machine program."
  (declare (type Rule-Table rules))
  (declare (type Tape       tape))
  (the Interpreter
    (make-instance 'Interpreter :rules rules :tape tape)))

;;; -------------------------------------------------------

(defun interpreter-apply-rule (interpreter rule)
  "Applies the RULE unto the INTERPRETER's current state and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type Rule        rule))
  
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    
    ;; Replace the current tape cell with the RULE's target symbol.
    (setf (tape-current-symbol tape) (rule-next-symbol rule))
    
    ;; Contingently move the tape head.
    (case (rule-direction rule)
      (#\< (tape-move-left  tape))
      (#\> (tape-move-right tape))
      (#\- (tape-stay       tape))
      (otherwise
        (error "Unrecognized tape direction: ~s."
          (rule-direction rule)))))
  
  ;; Set the STAGE to (rule.nextCondition, tape.currentSymbol).
  (interpreter-update-stage interpreter
    (rule-next-condition rule))
  
  ;; Check whether the state machine has reached the terminating "F"
  ;; condition (state)
  (interpreter-check-for-termination interpreter)
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the rules maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  
  ;; Operate the infinite program loop.
  (with-slots (rules stage halted-p) interpreter
    (declare (type Rule-Table rules))
    (declare (ignorable       rules))
    (declare (type Stage      stage))
    (declare (type boolean    halted-p))
    (declare (ignorable       halted-p))
    
    (when (stage-completely-specified-p stage)
      (loop until halted-p do
        (interpreter-apply-rule interpreter
          (rule-table-get-rule rules stage)))))
  
  ;; Conclude the halting condition with an output of the tape.
  (tape-print (slot-value interpreter 'tape))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Turing-machine (code
                                 &optional (tape (make-empty-tape)))
  "Interprets the piece of Turing-machine source CODE, optionally
   employing the specified TAPE for the symbol provision, and reutrns no
   value."
  (declare (type string code))
  (declare (type Tape   tape))
  (interpreter-interpret
    (make-interpreter
      (build-rule-table
        (parse-rules code))
      tape))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine, case of input = 0.
(interpret-Turing-machine
  "Q1:E>1
   Q0:O>0
   E0:E>1
   O0:F<0"
  (make-empty-tape))

;;; -------------------------------------------------------

;; Truth-machine, case of input = 1.
(interpret-Turing-machine
  "Q1:E>1
   Q0:O>0
   E0:E>1
   O0:F<0"
  (make-tape-of 0 "1"))

;;; -------------------------------------------------------

;; 3-state busy beaver.
;; 
;; This will print six 1-bits, as it holds:
;; 
;;   B(n) = 6, for n = 3,
;; 
;; with
;; 
;;   n --- the number of available states (= |{ Q, E, O }|).
;; 
;; The following rules apply:
;; 
;;   ------------------------------------------------------------------
;;   Curr. state | Curr. symbol | Next state | New symbol | Direction
;;   ------------+--------------+------------+------------+------------
;;        Q      |      0       |     E      |     1      |     >
;;        Q      |      1       |     O      |     1      |     <
;;        E      |      0       |     Q      |     1      |     <
;;        E      |      1       |     E      |     1      |     >
;;        O      |      0       |     E      |     1      |     <
;;        O      |      1       |     F      |     1      |     >
;;   ------------------------------------------------------------------
(interpret-Turing-machine
  "Q0:E>1
   Q1:O<1
   
   E0:Q<1
   E1:E>1
   
   O0:E<1
   O1:F>1")
