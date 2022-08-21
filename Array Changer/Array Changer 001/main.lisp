;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter and code generator for the
;; esoteric programming language "Array Changer", invented by the
;; Esolang user "A", and based upon the processing of a state table.
;; 
;; Concepts
;; ========
;; Array Changer can be subsumed into the cell-based species of esoteric
;; programming languages, with its dioristic attribute being a state
;; table whose entries respond to two transition values: zero (0) and
;; one (1).
;; 
;; == PROGRAMS OPERATE IN ACCORDANCE WITH A STATE MACHINE ==
;; The basic substratum of an Array Changer program is defined by a
;; fixed association of 256 states, agnominated by unsigned non-negative
;; integer values in the range [0, 255], and each such affiliated as the
;; source to its two destination states. An equinumerant count in events
;; or transitions appertains defined to any of these correspondences:
;; the first conflated with a transition value of zero (0), the second
;; with one (1).
;; 
;; The states are expressed as hexadecimal digit twains, begetting the
;; following exposition:
;; 
;;   Current state   | Next state on 0 | Next state on 1
;;   ----------------+-----------------+----------------
;;   0x00            | 0x2B            | 0xF7
;;   0x01            | 0x77            | 0x1D
;;   0x02            | 0x5F            | 0x78
;;   0x03            | 0x4E            | 0xEA
;;   0x04            | 0x9A            | 0xAE
;;   0x05            | 0x01            | 0x60
;;   0x06            | 0x75            | 0x46
;;   0x07            | 0x1A            | 0x1A
;;   0x08            | 0x73            | 0x68
;;   0x09            | 0x76            | 0x50
;;   0x0A            | 0x63            | 0x44
;;   0x0B            | 0xC1            | 0x82
;;   0x0C            | 0x5E            | 0x47
;;   0x0D            | 0xAA            | 0xC9
;;   0x0E            | 0xA6            | 0x07
;;   0x0F            | 0x15            | 0x27
;;   0x10            | 0x79            | 0x81
;;   0x11            | 0x38            | 0xB1
;;   0x12            | 0x5D            | 0xA2
;;   0x13            | 0x19            | 0xA5
;;   0x14            | 0xBC            | 0x55
;;   0x15            | 0x3F            | 0xE5
;;   0x16            | 0x7A            | 0x56
;;   0x17            | 0x8D            | 0x93
;;   0x18            | 0xFB            | 0x7F
;;   0x19            | 0xF9            | 0xAC
;;   0x1A            | 0x3B            | 0x16
;;   0x1B            | 0x91            | 0x2A
;;   0x1C            | 0xA8            | 0x57
;;   0x1D            | 0x68            | 0x51
;;   0x1E            | 0x04            | 0x9A
;;   0x1F            | 0x03            | 0xBD
;;   0x20            | 0x36            | 0xB2
;;   0x21            | 0x3E            | 0x5E
;;   0x22            | 0x35            | 0x6E
;;   0x23            | 0x52            | 0x37
;;   0x24            | 0x92            | 0x34
;;   0x25            | 0xC7            | 0x69
;;   0x26            | 0x87            | 0x52
;;   0x27            | 0x05            | 0x08
;;   0x28            | 0x96            | 0x04
;;   0x29            | 0x6A            | 0xD9
;;   0x2A            | 0xD8            | 0x77
;;   0x2B            | 0x8F            | 0x0B
;;   0x2C            | 0x95            | 0xB5
;;   0x2D            | 0x5B            | 0x1E
;;   0x2E            | 0x66            | 0x7D
;;   0x2F            | 0x69            | 0x1C
;;   0x30            | 0xE2            | 0x64
;;   0x31            | 0xCD            | 0x9D
;;   0x32            | 0x98            | 0xF4
;;   0x33            | 0x6E            | 0x75
;;   0x34            | 0x64            | 0xD4
;;   0x35            | 0x99            | 0xD6
;;   0x36            | 0xAC            | 0xCE
;;   0x37            | 0x18            | 0x4F
;;   0x38            | 0x5C            | 0x4E
;;   0x39            | 0x0D            | 0x28
;;   0x3A            | 0xEB            | 0x09
;;   0x3B            | 0xDD            | 0x7A
;;   0x3C            | 0x86            | 0x40
;;   0x3D            | 0xE4            | 0x99
;;   0x3E            | 0x0C            | 0xC5
;;   0x3F            | 0xC5            | 0x5F
;;   0x40            | 0xAE            | 0x9C
;;   0x41            | 0xAF            | 0x94
;;   0x42            | 0xAB            | 0x36
;;   0x43            | 0x6C            | 0x8B
;;   0x44            | 0x7C            | 0xBA
;;   0x45            | 0xD4            | 0xF3
;;   0x46            | 0x3D            | 0x10
;;   0x47            | 0x72            | 0xF6
;;   0x48            | 0xF3            | 0x66
;;   0x49            | 0xF5            | 0xB7
;;   0x4A            | 0xCE            | 0x4A
;;   0x4B            | 0x07            | 0x25
;;   0x4C            | 0x46            | 0x4C
;;   0x4D            | 0x37            | 0xB8
;;   0x4E            | 0x4B            | 0xDD
;;   0x4F            | 0x6B            | 0x14
;;   0x50            | 0x6D            | 0x03
;;   0x51            | 0x34            | 0x63
;;   0x52            | 0x82            | 0xC1
;;   0x53            | 0xCA            | 0xCC
;;   0x54            | 0xA0            | 0xF2
;;   0x55            | 0xEF            | 0xAD
;;   0x56            | 0x6F            | 0xAA
;;   0x57            | 0xED            | 0x54
;;   0x58            | 0xA5            | 0x7E
;;   0x59            | 0x2D            | 0xD7
;;   0x5A            | 0xAD            | 0x29
;;   0x5B            | 0x25            | 0x2D
;;   0x5C            | 0x50            | 0x3B
;;   0x5D            | 0xD3            | 0x12
;;   0x5E            | 0xC0            | 0x15
;;   0x5F            | 0x2A            | 0xCB
;;   0x60            | 0x74            | 0x43
;;   0x61            | 0x1E            | 0xF0
;;   0x62            | 0x2E            | 0x2E
;;   0x63            | 0xFE            | 0x1B
;;   0x64            | 0x9B            | 0xE2
;;   0x65            | 0x53            | 0x9E
;;   0x66            | 0x21            | 0x0C
;;   0x67            | 0x06            | 0x22
;;   0x68            | 0x60            | 0x45
;;   0x69            | 0x61            | 0x70
;;   0x6A            | 0xA7            | 0x42
;;   0x6B            | 0xBF            | 0xCF
;;   0x6C            | 0xFF            | 0x92
;;   0x6D            | 0x65            | 0x90
;;   0x6E            | 0x31            | 0xDB
;;   0x6F            | 0xB5            | 0xB6
;;   0x70            | 0xEC            | 0xC2
;;   0x71            | 0xE0            | 0x76
;;   0x72            | 0x00            | 0x2B
;;   0x73            | 0x1D            | 0xCD
;;   0x74            | 0x43            | 0x0F
;;   0x75            | 0xDE            | 0xC3
;;   0x76            | 0xB7            | 0xD5
;;   0x77            | 0x10            | 0x6C
;;   0x78            | 0x12            | 0x88
;;   0x79            | 0x33            | 0xFF
;;   0x7A            | 0xDB            | 0x97
;;   0x7B            | 0xD2            | 0x87
;;   0x7C            | 0x7D            | 0x21
;;   0x7D            | 0xA3            | 0xD8
;;   0x7E            | 0xF4            | 0x59
;;   0x7F            | 0x85            | 0xE6
;;   0x80            | 0x8B            | 0x24
;;   0x81            | 0x8C            | 0x79
;;   0x82            | 0x4D            | 0x4D
;;   0x83            | 0xCB            | 0x3E
;;   0x84            | 0xE9            | 0xE4
;;   0x85            | 0x20            | 0x20
;;   0x86            | 0x8A            | 0xEB
;;   0x87            | 0xEE            | 0xEE
;;   0x88            | 0x0B            | 0x8F
;;   0x89            | 0xC3            | 0xAB
;;   0x8A            | 0x2F            | 0x3A
;;   0x8B            | 0x93            | 0x39
;;   0x8C            | 0x27            | 0x80
;;   0x8D            | 0x4C            | 0x06
;;   0x8E            | 0xF2            | 0x11
;;   0x8F            | 0x7B            | 0xD2
;;   0x90            | 0xBE            | 0xEC
;;   0x91            | 0xBA            | 0xA6
;;   0x92            | 0x08            | 0x30
;;   0x93            | 0x81            | 0x31
;;   0x94            | 0xCF            | 0xBC
;;   0x95            | 0x22            | 0x8C
;;   0x96            | 0x97            | 0x8D
;;   0x97            | 0xC6            | 0xC8
;;   0x98            | 0x5A            | 0xFA
;;   0x99            | 0x56            | 0x35
;;   0x9A            | 0xD7            | 0x13
;;   0x9B            | 0xD0            | 0x3D
;;   0x9C            | 0x13            | 0xE3
;;   0x9D            | 0x51            | 0x01
;;   0x9E            | 0x3C            | 0x86
;;   0x9F            | 0xE1            | 0x41
;;   0xA0            | 0x8E            | 0xED
;;   0xA1            | 0x26            | 0xD3
;;   0xA2            | 0xA1            | 0x23
;;   0xA3            | 0x62            | 0x73
;;   0xA4            | 0xDF            | 0xF8
;;   0xA5            | 0x40            | 0x58
;;   0xA6            | 0x17            | 0x84
;;   0xA7            | 0x89            | 0xBB
;;   0xA8            | 0xEA            | 0x71
;;   0xA9            | 0xC9            | 0x8E
;;   0xAA            | 0xBD            | 0xDE
;;   0xAB            | 0xFC            | 0x61
;;   0xAC            | 0x4F            | 0xAF
;;   0xAD            | 0xB2            | 0x6A
;;   0xAE            | 0x42            | 0x2F
;;   0xAF            | 0xE8            | 0x18
;;   0xB0            | 0x0F            | 0x3F
;;   0xB1            | 0x29            | 0x5A
;;   0xB2            | 0xD9            | 0x89
;;   0xB3            | 0xB9            | 0xB9
;;   0xB4            | 0xF0            | 0x65
;;   0xB5            | 0x80            | 0x05
;;   0xB6            | 0x24            | 0x74
;;   0xB7            | 0x11            | 0x1F
;;   0xB8            | 0x47            | 0xA1
;;   0xB9            | 0x28            | 0x0D
;;   0xBA            | 0xF6            | 0xE7
;;   0xBB            | 0xDA            | 0x32
;;   0xBC            | 0xD1            | 0x85
;;   0xBD            | 0x90            | 0xE9
;;   0xBE            | 0xCC            | 0x67
;;   0xBF            | 0x14            | 0xF9
;;   0xC0            | 0x02            | 0x91
;;   0xC1            | 0xFD            | 0xFD
;;   0xC2            | 0x54            | 0xA9
;;   0xC3            | 0x7E            | 0xF1
;;   0xC4            | 0xF8            | 0xC7
;;   0xC5            | 0x0A            | 0x9B
;;   0xC6            | 0x84            | 0x5C
;;   0xC7            | 0x9E            | 0xA7
;;   0xC8            | 0x16            | 0x0E
;;   0xC9            | 0x1F            | 0xB3
;;   0xCA            | 0xD5            | 0xCA
;;   0xCB            | 0xE7            | 0xC0
;;   0xCC            | 0xB3            | 0x17
;;   0xCD            | 0xD6            | 0x6F
;;   0xCE            | 0x41            | 0x98
;;   0xCF            | 0xBB            | 0x6B
;;   0xD0            | 0x45            | 0xFE
;;   0xD1            | 0x4A            | 0xE1
;;   0xD2            | 0x78            | 0x72
;;   0xD3            | 0xA2            | 0x26
;;   0xD4            | 0x48            | 0x7C
;;   0xD5            | 0xA9            | 0x4B
;;   0xD6            | 0xB6            | 0xC6
;;   0xD7            | 0x1C            | 0xB4
;;   0xD8            | 0x1B            | 0x5D
;;   0xD9            | 0xDC            | 0xDA
;;   0xDA            | 0x7F            | 0x9F
;;   0xDB            | 0x67            | 0xBE
;;   0xDC            | 0x9C            | 0xA4
;;   0xDD            | 0x39            | 0x96
;;   0xDE            | 0x0E            | 0x95
;;   0xDF            | 0xC4            | 0x8A
;;   0xE0            | 0xB4            | 0x6D
;;   0xE1            | 0x94            | 0xD1
;;   0xE2            | 0xB0            | 0xB0
;;   0xE3            | 0x58            | 0x5B
;;   0xE4            | 0x2C            | 0x33
;;   0xE5            | 0x44            | 0x62
;;   0xE6            | 0x32            | 0xDC
;;   0xE7            | 0xB8            | 0x7B
;;   0xE8            | 0xE6            | 0xE8
;;   0xE9            | 0xC8            | 0x2C
;;   0xEA            | 0x57            | 0xA0
;;   0xEB            | 0x71            | 0x49
;;   0xEC            | 0xC2            | 0xF5
;;   0xED            | 0x3A            | 0x38
;;   0xEE            | 0x88            | 0xD0
;;   0xEF            | 0xB1            | 0xDF
;;   0xF0            | 0x70            | 0xE0
;;   0xF1            | 0xA4            | 0x19
;;   0xF2            | 0x49            | 0xA8
;;   0xF3            | 0x30            | 0x48
;;   0xF4            | 0xF1            | 0xC4
;;   0xF5            | 0x09            | 0x53
;;   0xF6            | 0xF7            | 0x83
;;   0xF7            | 0x83            | 0x00
;;   0xF8            | 0xE3            | 0x3C
;;   0xF9            | 0x55            | 0xEF
;;   0xFA            | 0x9F            | 0xFB
;;   0xFB            | 0xFA            | 0xBF
;;   0xFC            | 0x59            | 0xFC
;;   0xFD            | 0x23            | 0xA3
;;   0xFE            | 0xE5            | 0x02
;;   0xFF            | 0x9D            | 0x0A
;; 
;; These properties' coeffiency establishes for the foundational element
;; of the language a state machine, the initial location of which
;; positions it in the state 0x00. Instructions exist to change the
;; state by mediation of one of the binary transitions 0 and 1.
;; 
;; == THE MEMORY: AN INFINITE TALLY OF CELLS ==
;; While the operative department of the language resorts to a state
;; machine, the data related moeity subscribes to a linear memory
;; composed of an infinite number of cells. Any of these entities may
;; store a state value, that is, an integer in the range [0, 255] ---
;; or, tantmount to this notion, an eight-bit byte.
;; 
;; The tape-like storage is arranged in a linear fashion, functioning
;; with a reference to the currently selected cell with what is known as
;; the cell pointer, in order to receive state manipulations or output
;; the recently set value. The pointer may be translated sinistrally or
;; dextrally in stepwise motions; yet it is not capacitated to request a
;; random access.
;; 
;; == THE STATE MACHINE AND THE MEMORY ARE TIGHTLY COUPLED ==
;; As the state machine and the tape operate as complements, their
;; mutual receptiveness defines a piece of Array Changer code's effects.
;; The language lacks input facilities, and may only print the current
;; cell's state. The exclusive facilities entail the transition of the
;; current state using zero or one, while either moving the cell pointer
;; to the left, right, or not at all, and a repetition command in order
;; to multiply the effect of a succeeding instruction.
;; 
;; 
;; Architecture
;; ============
;; The language's construction may be bifurcated into two complementary
;; aspects: the programmatic instruments and the data management
;; compartment, the former of which incorporates a state machine, while
;; the latter employs a linear tape of cells.
;; 
;; == STATE MACHINE ==
;; The woning of its diorism, a state machine is accommodated a
;; paravaunt role in the execution of programs. Each state, represented
;; by an unsigned byte value in the range [0, 255], associates with
;; exactly two other of its peer, one reached by a transition event of
;; zero, the other upon the occasion of one. The program flow is
;; realized by changing betwixt the states using the offered commands.
;; 
;; Naturally designed in a tabular form, the concrete manifestation
;; depends upon the intended deployment. A puristic approach might favor
;; a vector or array, indexed by the integer states themselves, and
;; associated in some way with the destination twain. A more convolute,
;; yet faithful reproduction attends to a directed graph, modeling the
;; states as vertices, and the transitions as edges.
;; 
;; == MEMORY ==
;; A rather plain solution is assigned to the castaldy of the program
;; data, installed in a tape-like memory composed of an infinite account
;; of cells. Each such specimen stores a state value.
;; 
;; A cell pointer selects at any the instant the currently active cell,
;; being amenable to sinistral and dextral movements by various
;; instructions' adminicle.
;; 
;; 
;; Data Types
;; ==========
;; Two categories of data partake of the Array Changer's utility: states
;; and bits.
;; 
;; == BYTE-VALUED STATES ==
;; The chief elements of currency, a set of 256 states, each a
;; non-negative octet in the range [0, 255], supplies the most
;; significant rank's occupants. In a dual employment, these delineate
;; the program state, as well as participate as tenants in the memory
;; cells.
;; 
;; == BIT-VALUED TRANSITIONS ==
;; Transitions define events which impel the translation of a state into
;; another one. Any such endpoint pair is connected by exactly two
;; transition values: zero (0) and one (1) --- thus partaking of a
;; commorancy inside the bit type {0, 1}.
;; 
;; 
;; Syntax
;; ======
;; Array Changer's donat betokens a paragon of plainess, composed
;; chiefly of commands, the carriage of the same constitute single
;; characters. Solely whitespaces as sepiments encounter tolerance.
;; 
;; == INSTRUCTIONS ARE REPRESENTED BY A SINGLE CHARACTER ==
;; Any member of the octuple instruction set's representatives assigns
;; the onus to a single letter, discriminating betwixt minuscules and
;; the majuscular case.
;; 
;; == WHITESPACES ==
;; Whitespaces, enumerating in their roster the space, horizontal tab,
;; as well as the various linebreak variants, shall be permissive at
;; any location and in any quantity.
;; 
;; == COMMENTS ==
;; No provision for comments dwells among the language's accommodations.
;; 
;; == GRAMMAR ==
;; A formulation of the donat in concord with the Extended Backus-Naur
;; Form (EBNF) shall be adduced:
;; 
;;   program    := sepiments , { command , sepiments }  , sepiments ;
;;   command    := "c" | "C" | "F" | "l" | "L" | "O" | "r" | "R" ;
;;   sepiments  := { whitespace } ;
;;   whitespace := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The Array Changer instruction set's perimeter parauvant envelops
;; operations for the modification of the state by moving along a
;; transition, with the contingency of a concomitant translation
;; regarding the memory's cell pointer. An orra member applies itself as
;; a convenient warkloom to the repetition of a succeeding command, and
;; an output facility engages in the communication from the program to
;; the user, however, destitute of a withershins counterpart.
;; 
;; == OVERVIEW ==
;; The eight available commands shall be subjected to a cursory perusal:
;; 
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   c       | Changes the state by following the transition 0 without
;;           | moving the cell pointer.
;;   ..................................................................
;;   C       | Changes the state by following the transition 1 without
;;           | moving the cell pointer.
;;   ..................................................................
;;   l       | Changes the state by following the transition 0, and
;;           | moves the cell pointer one cell to the left.
;;   ..................................................................
;;   L       | Changes the state by following the transition 1, and
;;           | moves the cell pointer one cell to the left.
;;   ..................................................................
;;   r       | Changes the state by following the transition 0, and
;;           | moves the cell pointer one cell to the right.
;;   ..................................................................
;;   R       | Changes the state by following the transition 1, and
;;           | moves the cell pointer one cell to the right.
;;   ..................................................................
;;   F       | Repeats the next instruction a tally of times equal to
;;           | the value stored in the current cell.
;;   ..................................................................
;;   O       | Outputs the current cell's state.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The original specification, as consulted during this writ's genesis,
;; did bewray a modest quantity of gnarity regarding the language. This
;; reticence concludes in a rather large proportion of extrapolations'
;; necessity in order to actually realize a working implementation. This
;; abridged enumeration shall relate a few, but by far not all, objects
;; of calligation.
;; 
;; == WHICH ARCHITECTURE APPLIES TO THE MEMORY? ==
;; The memory, as a conjecture of the output instruction "O", and the
;; several other operations that mention a sinistral or dextral
;; movement, may be agnized as a cell-based multitude of constituents.
;; The tenability, however, is not veridically meted against the
;; author's ultimate intentions.
;; 
;; 
;; Implementation
;; ==============
;; The states and their interrelations are implemented in the form of a
;; directed graph, the vertices of which represent the states, whereas
;; the edges define the two possible bit-valued transitions.
;; 
;; == VECTOR-BASED STATE TABLES IMPEDE TRANSITION DETECTIONS ==
;; In its most simple variant, the stable table could manifest itself
;; in a static vector, assigning to any of the 256 states, assuming
;; values in the unsigned byte range [0, 255] and thus being eligible as
;; subscripts to a one-dimensional array, a tuple of states, one moeity
;; of the same responds to the transition event of zero (0), the other
;; to one (1). Other variations, probing extension among the dimensions
;; or a generalization into associate produces, do attain less
;; tenability.
;; 
;; This approach's appropriation in its raw incarnation would satisfy
;; the basic requirements regarding the state machine navigation. If
;; consigned to our more elaborate conspectuity, the replication of a
;; desired text in the memory as a the respective ASCII code sequence,
;; however, the direct structure perforce ought to proceed athwart the
;; subordinated telos to permit the supputation of state transitions for
;; the reproduction of a specified output. If, for instance, our
;; desideratum entertains the generation of the message "Hello" on the
;; standard output, the requisite commands for navigating from the
;; incipient state to those replicating the ASCII codes in their correct
;; order ought to be computed. A vector implementation would mandate a
;; brute force algorithm for a transition sequence's detection --- which
;; might not even be optimal anenst the criterion of the least count in
;; intermediate stages.
;; 
;; == GRAPHS PERMIT EFFICIENT SHORTEST PATH EXPLORATIONS ==
;; The graph structure recommends itself as most steadable in allaying
;; predicaments of this ilk. For difficulties involving the traversal of
;; connected constituents, several probed algorithms exist, including
;; their derivations for path detections, in particular the shortest
;; among these. In this project, the breadth first search (BFS) based
;; shortest path algorithm has been instituted, yielding for any two
;; states the minimum tally of transitions, and thus interstitial
;; sojourn points, necessary to arrive at the destination from a source.
;; 
;; == WE USE A DIRECTED GRAPH WITH INCIDENCE COLLECTIONS ==
;; In our concrete circumstances, the graph implementation proceeds by
;; a class' adminiculum, persisting as its vertices the states in the
;; form of a vector of cons cells. Indeed, the solution central to our
;; disquisition in this section's inchoation recurs as a significant
;; parcel. The properties affiliated with the state's haecceity, namely
;; its unsigned octet range of [0, 255], allows their deployment as
;; indices of the fixed-size one-dimesional array. With any state being
;; allied with exactly two others, one for a transition event of zero
;; (0), another for the second possible event of one (1), the cons
;; associated with the state posing as its index, and thus as the source
;; vertex, stores in its left compartment the destination state if
;; encountering a zero event in the source, and in the dextral parcel
;; the destination for a one-valued transition. This directed graph thus
;; most closely emulates a simplified adjacency list structure,
;; retaining the vertices' private incidence collection, while rendered
;; disencumbered from the explicitly operating edge list. A
;; visualization of this association may be
;;   
;;   sourceState => (destinationStateIfZero . destinationStateIfOne)
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-18
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Array_Changer"
;;   -> "https://pencilprogrammer.com/algorithms/shortest-path-in-unweighted-graph-using-bfs/"
;;       o Describes and implements a shortest path search algorithm
;;         in the context of an unweighted graph, based on the breadth
;;         first traversal or breadth first search (BFS).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with the VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, without a claim of exhaustion, the function ``format''
   and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype state ()
  "The ``state'' type defines a state as a non-negative integer in the
   range [0, 255]."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype state-table ()
  "The ``state-table'' type defines an association of a source state
   with exactly two destinations of the same type, harnessing the fact
   that states are represented by non-negative integers in the range
   [0, 255], thus employing the source as an index into a simple
   one-dimensional array, mapping this location to a cons composed of
   two further states."
  '(simple-array (cons state state) (256)))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Array Changer
   commands."
  '(member
    :change-to-0-and-move-right
    :change-to-1-and-move-right
    :change-to-0-and-move-left
    :change-to-1-and-move-left
    :repeat
    :change-to-0
    :change-to-1
    :output))

;;; -------------------------------------------------------

(deftype supply (result-type)
  "The ``supply'' type defines a function akin to an iterator, accepting
   no input and returning two values:
     (1) an object of the RESULT-TYPE
     (2) a ``boolean'' value which resolves to ``T'' if the current
         result has not exhausted this supply, or ``NIL'' if this
         invocation has transgressed beyond the supply's content."
  `(function () (values ,result-type boolean)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Graph".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Graph ()
  ((states
    :initarg       :states
    :initform      (make-array 256
                     :element-type    '(cons state state)
                     :initial-element (cons 0 0)
                     :adjustable      NIL
                     :fill-pointer    NIL)
    :type          state-table
    :documentation "Associates each source state with its two
                    destinations by employing the former as an index
                    into the array, mapping to a cons, the first element
                    of which describes the state if zero, the second the
                    state if one."))
  (:documentation
    "The ``Graph'' class represents the states and their interrelations
     in the form of vertices.
     ---
     Each state assumes the role of a vertex, with two neighbors of the
     same type. The bit-valued transitions are assigned the agency of
     edges.
     ---
     The graph data structure as a rather convolute choice for the
     modeling of the Array Changer state table is entertained by the
     telos to reproduce a desired sequence of result states via the
     minimum tally of intermediate stages. If one desiderates, as a
     forbisen, the generation of the ASCII character codes forming the
     message \"Hello\", starting at the state 0x00, an infinite number
     of possible transition paths may be followed. Given that any state
     refers to a twain of neighbor states, without an appropriate data
     structure a brute force algorithm may be a requisite to the
     determination of any potent transition sequence. The graph
     structure, on the other hand, naturally establishes an eligible
     solution, the same stems from a well known predicament: the
     exploration of the shortest path --- in our case, specifically
     applying to an unweighted directed graph. Among the probed
     approaches, the breadth first search (BFS) may be employed in this
     wike."))

;;; -------------------------------------------------------

(defun make-graph ()
  "Creates and returns a new empty ``Graph''."
  (the Graph (make-instance 'Graph)))

;;; -------------------------------------------------------

(defun graph-add-connection (graph source state-if-0 state-if-1)
  "Adds to the GRAPH two edges, or transitions, from the SOURCE vertex
   (state) to its two destination vertices (states), with STATE-IF-0
   being reached with a edge value of zero (0), and STATE-IF-1 with a
   value of one (1), and returns the modified GRAPH."
  (declare (type Graph graph))
  (declare (type state source))
  (declare (type state state-if-0))
  (declare (type state state-if-1))
  (setf (aref (slot-value graph 'states) source)
        (cons state-if-0 state-if-1))
  (the Graph graph))

;;; -------------------------------------------------------

(defun graph-get-outgoing-states (graph source)
  "Returns the two destination states reached by the SOURCE vertex
   (state) in the GRAPH in the form of a cons cell, the first element of
   which represents the state reached by the edge value (transition) of
   zero (0), the second by a transition of one (1)."
  (declare (type Graph graph))
  (declare (type state source))
  (the (cons state state)
    (aref (slot-value graph 'states) source)))

;;; -------------------------------------------------------

(defun graph-get-opposite-state (graph source transition)
  "Returns the destination state reached in the GRAPH BY following the
   SOURCE vertex (state) along the edge value TRANSITION."
  (declare (type Graph graph))
  (declare (type state source))
  (declare (type bit   transition))
  (the state
    (if (zerop transition)
      (car (aref (slot-value graph 'states) source))
      (cdr (aref (slot-value graph 'states) source)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-graph ()
  "Creates and returns a new ``Graph'' representing the Array Changer's
   state configurations."
  (let ((graph (make-graph)))
    (declare (type Graph graph))
    (graph-add-connection graph #x00 #x2B #xF7)
    (graph-add-connection graph #x01 #x77 #x1D)
    (graph-add-connection graph #x02 #x5F #x78)
    (graph-add-connection graph #x03 #x4E #xEA)
    (graph-add-connection graph #x04 #x9A #xAE)
    (graph-add-connection graph #x05 #x01 #x60)
    (graph-add-connection graph #x06 #x75 #x46)
    (graph-add-connection graph #x07 #x1A #x1A)
    (graph-add-connection graph #x08 #x73 #x68)
    (graph-add-connection graph #x09 #x76 #x50)
    (graph-add-connection graph #x0A #x63 #x44)
    (graph-add-connection graph #x0B #xC1 #x82)
    (graph-add-connection graph #x0C #x5E #x47)
    (graph-add-connection graph #x0D #xAA #xC9)
    (graph-add-connection graph #x0E #xA6 #x07)
    (graph-add-connection graph #x0F #x15 #x27)
    (graph-add-connection graph #x10 #x79 #x81)
    (graph-add-connection graph #x11 #x38 #xB1)
    (graph-add-connection graph #x12 #x5D #xA2)
    (graph-add-connection graph #x13 #x19 #xA5)
    (graph-add-connection graph #x14 #xBC #x55)
    (graph-add-connection graph #x15 #x3F #xE5)
    (graph-add-connection graph #x16 #x7A #x56)
    (graph-add-connection graph #x17 #x8D #x93)
    (graph-add-connection graph #x18 #xFB #x7F)
    (graph-add-connection graph #x19 #xF9 #xAC)
    (graph-add-connection graph #x1A #x3B #x16)
    (graph-add-connection graph #x1B #x91 #x2A)
    (graph-add-connection graph #x1C #xA8 #x57)
    (graph-add-connection graph #x1D #x68 #x51)
    (graph-add-connection graph #x1E #x04 #x9A)
    (graph-add-connection graph #x1F #x03 #xBD)
    (graph-add-connection graph #x20 #x36 #xB2)
    (graph-add-connection graph #x21 #x3E #x5E)
    (graph-add-connection graph #x22 #x35 #x6E)
    (graph-add-connection graph #x23 #x52 #x37)
    (graph-add-connection graph #x24 #x92 #x34)
    (graph-add-connection graph #x25 #xC7 #x69)
    (graph-add-connection graph #x26 #x87 #x52)
    (graph-add-connection graph #x27 #x05 #x08)
    (graph-add-connection graph #x28 #x96 #x04)
    (graph-add-connection graph #x29 #x6A #xD9)
    (graph-add-connection graph #x2A #xD8 #x77)
    (graph-add-connection graph #x2B #x8F #x0B)
    (graph-add-connection graph #x2C #x95 #xB5)
    (graph-add-connection graph #x2D #x5B #x1E)
    (graph-add-connection graph #x2E #x66 #x7D)
    (graph-add-connection graph #x2F #x69 #x1C)
    (graph-add-connection graph #x30 #xE2 #x64)
    (graph-add-connection graph #x31 #xCD #x9D)
    (graph-add-connection graph #x32 #x98 #xF4)
    (graph-add-connection graph #x33 #x6E #x75)
    (graph-add-connection graph #x34 #x64 #xD4)
    (graph-add-connection graph #x35 #x99 #xD6)
    (graph-add-connection graph #x36 #xAC #xCE)
    (graph-add-connection graph #x37 #x18 #x4F)
    (graph-add-connection graph #x38 #x5C #x4E)
    (graph-add-connection graph #x39 #x0D #x28)
    (graph-add-connection graph #x3A #xEB #x09)
    (graph-add-connection graph #x3B #xDD #x7A)
    (graph-add-connection graph #x3C #x86 #x40)
    (graph-add-connection graph #x3D #xE4 #x99)
    (graph-add-connection graph #x3E #x0C #xC5)
    (graph-add-connection graph #x3F #xC5 #x5F)
    (graph-add-connection graph #x40 #xAE #x9C)
    (graph-add-connection graph #x41 #xAF #x94)
    (graph-add-connection graph #x42 #xAB #x36)
    (graph-add-connection graph #x43 #x6C #x8B)
    (graph-add-connection graph #x44 #x7C #xBA)
    (graph-add-connection graph #x45 #xD4 #xF3)
    (graph-add-connection graph #x46 #x3D #x10)
    (graph-add-connection graph #x47 #x72 #xF6)
    (graph-add-connection graph #x48 #xF3 #x66)
    (graph-add-connection graph #x49 #xF5 #xB7)
    (graph-add-connection graph #x4A #xCE #x4A)
    (graph-add-connection graph #x4B #x07 #x25)
    (graph-add-connection graph #x4C #x46 #x4C)
    (graph-add-connection graph #x4D #x37 #xB8)
    (graph-add-connection graph #x4E #x4B #xDD)
    (graph-add-connection graph #x4F #x6B #x14)
    (graph-add-connection graph #x50 #x6D #x03)
    (graph-add-connection graph #x51 #x34 #x63)
    (graph-add-connection graph #x52 #x82 #xC1)
    (graph-add-connection graph #x53 #xCA #xCC)
    (graph-add-connection graph #x54 #xA0 #xF2)
    (graph-add-connection graph #x55 #xEF #xAD)
    (graph-add-connection graph #x56 #x6F #xAA)
    (graph-add-connection graph #x57 #xED #x54)
    (graph-add-connection graph #x58 #xA5 #x7E)
    (graph-add-connection graph #x59 #x2D #xD7)
    (graph-add-connection graph #x5A #xAD #x29)
    (graph-add-connection graph #x5B #x25 #x2D)
    (graph-add-connection graph #x5C #x50 #x3B)
    (graph-add-connection graph #x5D #xD3 #x12)
    (graph-add-connection graph #x5E #xC0 #x15)
    (graph-add-connection graph #x5F #x2A #xCB)
    (graph-add-connection graph #x60 #x74 #x43)
    (graph-add-connection graph #x61 #x1E #xF0)
    (graph-add-connection graph #x62 #x2E #x2E)
    (graph-add-connection graph #x63 #xFE #x1B)
    (graph-add-connection graph #x64 #x9B #xE2)
    (graph-add-connection graph #x65 #x53 #x9E)
    (graph-add-connection graph #x66 #x21 #x0C)
    (graph-add-connection graph #x67 #x06 #x22)
    (graph-add-connection graph #x68 #x60 #x45)
    (graph-add-connection graph #x69 #x61 #x70)
    (graph-add-connection graph #x6A #xA7 #x42)
    (graph-add-connection graph #x6B #xBF #xCF)
    (graph-add-connection graph #x6C #xFF #x92)
    (graph-add-connection graph #x6D #x65 #x90)
    (graph-add-connection graph #x6E #x31 #xDB)
    (graph-add-connection graph #x6F #xB5 #xB6)
    (graph-add-connection graph #x70 #xEC #xC2)
    (graph-add-connection graph #x71 #xE0 #x76)
    (graph-add-connection graph #x72 #x00 #x2B)
    (graph-add-connection graph #x73 #x1D #xCD)
    (graph-add-connection graph #x74 #x43 #x0F)
    (graph-add-connection graph #x75 #xDE #xC3)
    (graph-add-connection graph #x76 #xB7 #xD5)
    (graph-add-connection graph #x77 #x10 #x6C)
    (graph-add-connection graph #x78 #x12 #x88)
    (graph-add-connection graph #x79 #x33 #xFF)
    (graph-add-connection graph #x7A #xDB #x97)
    (graph-add-connection graph #x7B #xD2 #x87)
    (graph-add-connection graph #x7C #x7D #x21)
    (graph-add-connection graph #x7D #xA3 #xD8)
    (graph-add-connection graph #x7E #xF4 #x59)
    (graph-add-connection graph #x7F #x85 #xE6)
    (graph-add-connection graph #x80 #x8B #x24)
    (graph-add-connection graph #x81 #x8C #x79)
    (graph-add-connection graph #x82 #x4D #x4D)
    (graph-add-connection graph #x83 #xCB #x3E)
    (graph-add-connection graph #x84 #xE9 #xE4)
    (graph-add-connection graph #x85 #x20 #x20)
    (graph-add-connection graph #x86 #x8A #xEB)
    (graph-add-connection graph #x87 #xEE #xEE)
    (graph-add-connection graph #x88 #x0B #x8F)
    (graph-add-connection graph #x89 #xC3 #xAB)
    (graph-add-connection graph #x8A #x2F #x3A)
    (graph-add-connection graph #x8B #x93 #x39)
    (graph-add-connection graph #x8C #x27 #x80)
    (graph-add-connection graph #x8D #x4C #x06)
    (graph-add-connection graph #x8E #xF2 #x11)
    (graph-add-connection graph #x8F #x7B #xD2)
    (graph-add-connection graph #x90 #xBE #xEC)
    (graph-add-connection graph #x91 #xBA #xA6)
    (graph-add-connection graph #x92 #x08 #x30)
    (graph-add-connection graph #x93 #x81 #x31)
    (graph-add-connection graph #x94 #xCF #xBC)
    (graph-add-connection graph #x95 #x22 #x8C)
    (graph-add-connection graph #x96 #x97 #x8D)
    (graph-add-connection graph #x97 #xC6 #xC8)
    (graph-add-connection graph #x98 #x5A #xFA)
    (graph-add-connection graph #x99 #x56 #x35)
    (graph-add-connection graph #x9A #xD7 #x13)
    (graph-add-connection graph #x9B #xD0 #x3D)
    (graph-add-connection graph #x9C #x13 #xE3)
    (graph-add-connection graph #x9D #x51 #x01)
    (graph-add-connection graph #x9E #x3C #x86)
    (graph-add-connection graph #x9F #xE1 #x41)
    (graph-add-connection graph #xA0 #x8E #xED)
    (graph-add-connection graph #xA1 #x26 #xD3)
    (graph-add-connection graph #xA2 #xA1 #x23)
    (graph-add-connection graph #xA3 #x62 #x73)
    (graph-add-connection graph #xA4 #xDF #xF8)
    (graph-add-connection graph #xA5 #x40 #x58)
    (graph-add-connection graph #xA6 #x17 #x84)
    (graph-add-connection graph #xA7 #x89 #xBB)
    (graph-add-connection graph #xA8 #xEA #x71)
    (graph-add-connection graph #xA9 #xC9 #x8E)
    (graph-add-connection graph #xAA #xBD #xDE)
    (graph-add-connection graph #xAB #xFC #x61)
    (graph-add-connection graph #xAC #x4F #xAF)
    (graph-add-connection graph #xAD #xB2 #x6A)
    (graph-add-connection graph #xAE #x42 #x2F)
    (graph-add-connection graph #xAF #xE8 #x18)
    (graph-add-connection graph #xB0 #x0F #x3F)
    (graph-add-connection graph #xB1 #x29 #x5A)
    (graph-add-connection graph #xB2 #xD9 #x89)
    (graph-add-connection graph #xB3 #xB9 #xB9)
    (graph-add-connection graph #xB4 #xF0 #x65)
    (graph-add-connection graph #xB5 #x80 #x05)
    (graph-add-connection graph #xB6 #x24 #x74)
    (graph-add-connection graph #xB7 #x11 #x1F)
    (graph-add-connection graph #xB8 #x47 #xA1)
    (graph-add-connection graph #xB9 #x28 #x0D)
    (graph-add-connection graph #xBA #xF6 #xE7)
    (graph-add-connection graph #xBB #xDA #x32)
    (graph-add-connection graph #xBC #xD1 #x85)
    (graph-add-connection graph #xBD #x90 #xE9)
    (graph-add-connection graph #xBE #xCC #x67)
    (graph-add-connection graph #xBF #x14 #xF9)
    (graph-add-connection graph #xC0 #x02 #x91)
    (graph-add-connection graph #xC1 #xFD #xFD)
    (graph-add-connection graph #xC2 #x54 #xA9)
    (graph-add-connection graph #xC3 #x7E #xF1)
    (graph-add-connection graph #xC4 #xF8 #xC7)
    (graph-add-connection graph #xC5 #x0A #x9B)
    (graph-add-connection graph #xC6 #x84 #x5C)
    (graph-add-connection graph #xC7 #x9E #xA7)
    (graph-add-connection graph #xC8 #x16 #x0E)
    (graph-add-connection graph #xC9 #x1F #xB3)
    (graph-add-connection graph #xCA #xD5 #xCA)
    (graph-add-connection graph #xCB #xE7 #xC0)
    (graph-add-connection graph #xCC #xB3 #x17)
    (graph-add-connection graph #xCD #xD6 #x6F)
    (graph-add-connection graph #xCE #x41 #x98)
    (graph-add-connection graph #xCF #xBB #x6B)
    (graph-add-connection graph #xD0 #x45 #xFE)
    (graph-add-connection graph #xD1 #x4A #xE1)
    (graph-add-connection graph #xD2 #x78 #x72)
    (graph-add-connection graph #xD3 #xA2 #x26)
    (graph-add-connection graph #xD4 #x48 #x7C)
    (graph-add-connection graph #xD5 #xA9 #x4B)
    (graph-add-connection graph #xD6 #xB6 #xC6)
    (graph-add-connection graph #xD7 #x1C #xB4)
    (graph-add-connection graph #xD8 #x1B #x5D)
    (graph-add-connection graph #xD9 #xDC #xDA)
    (graph-add-connection graph #xDA #x7F #x9F)
    (graph-add-connection graph #xDB #x67 #xBE)
    (graph-add-connection graph #xDC #x9C #xA4)
    (graph-add-connection graph #xDD #x39 #x96)
    (graph-add-connection graph #xDE #x0E #x95)
    (graph-add-connection graph #xDF #xC4 #x8A)
    (graph-add-connection graph #xE0 #xB4 #x6D)
    (graph-add-connection graph #xE1 #x94 #xD1)
    (graph-add-connection graph #xE2 #xB0 #xB0)
    (graph-add-connection graph #xE3 #x58 #x5B)
    (graph-add-connection graph #xE4 #x2C #x33)
    (graph-add-connection graph #xE5 #x44 #x62)
    (graph-add-connection graph #xE6 #x32 #xDC)
    (graph-add-connection graph #xE7 #xB8 #x7B)
    (graph-add-connection graph #xE8 #xE6 #xE8)
    (graph-add-connection graph #xE9 #xC8 #x2C)
    (graph-add-connection graph #xEA #x57 #xA0)
    (graph-add-connection graph #xEB #x71 #x49)
    (graph-add-connection graph #xEC #xC2 #xF5)
    (graph-add-connection graph #xED #x3A #x38)
    (graph-add-connection graph #xEE #x88 #xD0)
    (graph-add-connection graph #xEF #xB1 #xDF)
    (graph-add-connection graph #xF0 #x70 #xE0)
    (graph-add-connection graph #xF1 #xA4 #x19)
    (graph-add-connection graph #xF2 #x49 #xA8)
    (graph-add-connection graph #xF3 #x30 #x48)
    (graph-add-connection graph #xF4 #xF1 #xC4)
    (graph-add-connection graph #xF5 #x09 #x53)
    (graph-add-connection graph #xF6 #xF7 #x83)
    (graph-add-connection graph #xF7 #x83 #x00)
    (graph-add-connection graph #xF8 #xE3 #x3C)
    (graph-add-connection graph #xF9 #x55 #xEF)
    (graph-add-connection graph #xFA #x9F #xFB)
    (graph-add-connection graph #xFB #xFA #xBF)
    (graph-add-connection graph #xFC #x59 #xFC)
    (graph-add-connection graph #xFD #x23 #xA3)
    (graph-add-connection graph #xFE #xE5 #x02)
    (graph-add-connection graph #xFF #x9D #x0A)
    (the Graph graph)))

;;; -------------------------------------------------------

(declaim (type Graph +STATE-GRAPH+))

;;; -------------------------------------------------------

(defparameter +STATE-GRAPH+ (build-graph)
  "The graph which represents the Array Changer state relations.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of Array Changer CODE its
   instructions and returns these in a one-dimensional simple array."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0
      do
        (case token
          (#\r (push :change-to-0-and-move-right instructions))
          (#\R (push :change-to-1-and-move-right instructions))
          (#\l (push :change-to-0-and-move-left  instructions))
          (#\L (push :change-to-1-and-move-left  instructions))
          (#\F (push :repeat                     instructions))
          (#\c (push :change-to-0                instructions))
          (#\C (push :change-to-1                instructions))
          (#\O (push :output                     instructions))
          ((#\Space #\Tab #\Newline) NIL)
          (otherwise
            (error "Invalid character \"~c\" at position ~d."
              token position))))
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "State-Machine".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass State-Machine ()
  ((graph
    :initarg       :graph
    :initform      +STATE-GRAPH+
    :type          Graph
    :documentation "A graph maintaining the states and their
                    interrelations.")
   (current-state
    :initarg       :current-state
    :initform      0
    :type          state
    :documentation "The currently selected state in the GRAPH."))
  (:documentation
    "The ``State-Machine'' class implements an automaton which operates
     on a graph composed of states as its vertices and transitions as
     the edges, maintaining a reference to the current state.
     ---
     Effectively, a state machine augments the state graph by a pointer
     to its active state."))

;;; -------------------------------------------------------

(defun make-state-machine ()
  "Creates and returns a new ``State-Machine'', with its selected state
   being set to that with the value zero (0)."
  (the State-Machine (make-instance 'State-Machine)))

;;; -------------------------------------------------------

(defun state-machine-current-state (state-machine)
  "Returns the STATE-MACHINE's currently selected state."
  (declare (type State-Machine state-machine))
  (the state (slot-value state-machine 'current-state)))

;;; -------------------------------------------------------

(defun state-machine-change-to (state-machine transition)
  "Changes the STATE-MACHINE from its current state along the TRANSITION
   to the new state and returns the modified STATE-MACHINE."
  (declare (type State-Machine state-machine))
  (declare (type bit           transition))
  (with-slots (graph current-state) state-machine
    (declare (type Graph graph))
    (declare (type state current-state))
    (setf current-state
      (graph-get-opposite-state graph current-state transition)))
  (the State-Machine state-machine))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer state)
    :documentation "Maps to a signed integer index a state.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "Stores the index (key) of the current selected cell
                    among the CELLS."))
  (:documentation
    "The ``Memory'' class realizes a tape-like linear memory, composed
     of ``state''-typed cells, unbounded in their tally, upon which
     operates a pointer as the index into the currently active
     specimen."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new empty ``Memory'' whose cell at the index
   zero is selected."
  (the Memory (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the state stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer state) cells))
    (declare (type integer                       pointer))
    (the state
      (gethash pointer cells #x00))))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-state memory)
  "Stores the NEW-STATE into the MEMORY's current cell and returns the
   modified MEMORY."
  (declare (type state  new-state))
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer state) cells))
    (declare (type integer                       pointer))
    (setf (gethash pointer cells #x00) new-state))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY pointer to the previous cell and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (slot-value memory 'pointer))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY pointer to the next cell and returns the modified
   MEMORY."
  (declare (type Memory memory))
  (incf (slot-value memory 'pointer))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing interpreter instructions.")
    :type          (vector command *)
    :documentation "The Array Changer instructions to process.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer's current index in the
                    INSTRUCTIONS vector.")
   (current-instruction
    :initarg       :current-instruction
    :initform      NIL
    :type          (or null command)
    :documentation "The instruction at the instruction pointer position
                    IP in the INSTRUCTIONS vector.")
   (state-machine
    :initarg       :state-machine
    :initform      (make-state-machine)
    :type          State-Machine
    :documentation "The state machine which contains a graph
                    representation of the state relations, in
                    conjunction with a pointer to the currently active
                    state.")
   (memory
    :initarg       :memory
    :initform      (make-memory)
    :type          Memory
    :documentation "The tape of cells which store states as their
                    values."))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluations of a
     instruction sequence for the manifestation of effects.
     ---
     In order to fulfil its onus, an interpreter contains several pieces
     of data:
       - An instruction vector defining the operations to process.
       - An instruction pointer (IP) responsible for selecting the
         current instruction to apply.
       - A state machine operating on a graph whose vertices contain the
         states, whereas its edges define the transitions from one state
         to exactly two of its neighbors.
       - A memory composed of cells, each a salvatory to exactly one
         state."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector command *) instructions))
    (declare (type fixnum             ip))
    (declare (type (or null command)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions ip)
        (aref instructions ip))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' responsible for processing
   the sequence of INSTRUCTIONS."
  (declare (type (vector command *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer (IP) to the next
   instruction, if possible, and returns the modified INTERPRETER."
  (with-slots (instructions ip current-instruction) interpreter
    (declare (type (vector command *) instructions))
    (declare (type fixnum             ip))
    (declare (type (or null command)  current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p instructions (1+ ip))
        (aref instructions (incf ip)))))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-move-left (interpreter)
  "Moves the INTERPRETER's memory pointer one cell to the left and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (memory-move-left (slot-value interpreter 'memory))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-move-right (interpreter)
  "Moves the INTERPRETER's memory pointer one cell to the right and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (memory-move-right (slot-value interpreter 'memory))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-change-state-to (interpreter transition)
  "Changes the INTERPRETER's internally managed state machine from its
   current state to that one reached by following the TRANSITION and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type bit         transition))
  (with-slots (state-machine memory) interpreter
    (declare (type State-Machine state-machine))
    (declare (type Memory        memory))
    (state-machine-change-to state-machine transition)
    (setf (memory-current-cell memory)
      (state-machine-current-state state-machine)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defgeneric interpreter-process-instruction (interpreter
                                             instruction-type)
  (:documentation
    "Processes the INSTRUCTION-TYPE using the INTERPRETER and returns an
     arbitrary value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    ((interpreter-variable instruction-type)
     &body body)
  "Implements a method for the generic function
   ``interpreter-process-instruction'' by which utilizes an
   ``Interpreter'' bound to the argument INTERPRETER-VARIABLE and
   dispatches on the INSTRUCTION-TYPE, evaluating the BODY forms, and
   returning the underlying interpreter itself, as no command in the
   Array Changer language is expected to produce a value."
  (let ((instruction-type-variable (gensym)))
    (declare (type symbol instruction-type-variable))
    `(defmethod interpreter-process-instruction
         ((,interpreter-variable      Interpreter)
          (,instruction-type-variable (eql ,instruction-type)))
       (declare (type Interpreter ,interpreter-variable))
       (declare (type command     ,instruction-type-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (ignore           ,instruction-type-variable))
       ,@body
       (the Interpreter ,interpreter-variable))))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-0-and-move-right)
  (interpreter-change-state-to interpreter 0)
  (interpreter-move-right      interpreter))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-1-and-move-right)
  (interpreter-change-state-to interpreter 1)
  (interpreter-move-right      interpreter))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-0-and-move-left)
  (interpreter-change-state-to interpreter 0)
  (interpreter-move-left interpreter))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-1-and-move-left)
  (interpreter-change-state-to interpreter 1)
  (interpreter-move-left interpreter))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :repeat)
  (with-slots (current-instruction memory) interpreter
    (declare (type (or null command) current-instruction))
    (declare (type Memory            memory))
    (declare (ignorable              memory))
    (interpreter-advance-ip interpreter)
    ;; Advance to next instruction in order to repeat it.
    (if current-instruction
      (let ((number-of-repetitions (memory-current-cell memory)))
        (declare (type state number-of-repetitions))
        (loop repeat number-of-repetitions do
          (interpreter-process-instruction
            interpreter current-instruction)))
      ;; No instruction to repeat?
      ;; => Error.
      (error "No instruction to repeat exists."))))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-0)
  (interpreter-change-state-to interpreter 0))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :change-to-1)
  (interpreter-change-state-to interpreter 1))

;;; -------------------------------------------------------

(define-instruction-processor (interpreter :output)
  (format T "~d "
    (memory-current-cell
      (slot-value interpreter 'memory))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions stored in the INTERPRETER and returns the
   modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (current-instruction) interpreter
    (declare (type (or null command) current-instruction))
    (loop while current-instruction do
      (interpreter-process-instruction interpreter current-instruction)
      (interpreter-advance-ip interpreter)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-Array-Changer (code)
  "Interprets the piece of Array Changer CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (extract-instructions code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Queue".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Queue
  "The ``Queue'' class implements the queue abstract data structure,
   a first-in first-out (FIFO) collection, which answers to insertions
   at its tail and removals from its front.
   ---
   In the context of this program, the queue is exclusively employed
   during the shortest path algorithm based on the graph traversal
   method \"breadth first search\" (BFS)."
  (elements NIL :type (list-of T)))

;;; -------------------------------------------------------

(defun queue-enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the QUEUE's tail and returns the modified
   QUEUE."
  (declare (type Queue queue))
  (declare (type T     new-element))
  (setf (queue-elements queue)
        (append (queue-elements queue)
                (list new-element)))
  (the Queue queue))

;;; -------------------------------------------------------

(defun queue-dequeue (queue)
  "Removes and returns the element at the QUEUE front, or ``NIL'' if the
   QUEUE is empty."
  (declare (type Queue queue))
  (the T (pop (queue-elements queue))))

;;; -------------------------------------------------------

(defun queue-clear (queue)
  "Removes all elements from the QUEUE and returns the modified QUEUE."
  (declare (type Queue queue))
  (setf (queue-elements queue) NIL)
  (the Queue queue))

;;; -------------------------------------------------------

(defun queue-empty-p (queue)
  "Checks whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (not (null
      (zerop (length (queue-elements queue)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Edge".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Edge
  (:constructor make-edge (source transition destination)))
  "The ``Edge'' class represents a connection betwixt two states in a
   graph, known as the source and the destination, comprehending also
   the transition from the former to the latter.
   ---
   Please note that this implementation wists of no use for the
   DESTINATION slot, yet, in an effort to afford completion, the same
   has been retained."
  (source      0 :type state)
  (transition  0 :type bit)
  (destination 0 :type state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Shortest-Path-Finder".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Shortest-Path-Finder ()
  ((vertex-queue
    :initarg       :vertex-queue
    :initform      (make-queue)
    :type          Queue
    :documentation "The queue used to store the vertices (states) to
                    perquire regarding unvisited neighboring vertices
                    (states).")
   (visitations
    :initarg       :visitations
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of state boolean)
    :documentation "Associates visited states with a Boolean true
                    flag.")
   (predecessors
    :initarg       :predecessors
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of state Edge)
    :documentation "Associates a destination state with an edge which
                    encapsulates its source state and the transition
                    bit.
                    ---
                    By starting with the specified end vertex (state),
                    and following its edge back to the source vertex
                    (state), the discovered transition from the start to
                    the end can be reproduced. The respective algorithm
                    may be expressed in the following.
                    
                      Given:
                        endVertex    --- the destination state
                        predecessors --- a mapping of states to the
                                         ingoing edges
                      
                      Calculation:
                        let currentVertex <- endVertex
                        let edgeToCurrent <- predecessors(currentVertex)
                        
                        while edgeToCurrent != null
                          currentVertex <- edgeToCurrent.sourceVertex
                          edgeToCurrent <- predecessors(currentVertex)
                        end while"))
  (:documentation
    "The ``Shortest-Path-Finder'' class implements a routine based upon
     the breadth first search (BFS) or breadth first traversal algorithm
     for detecting the shortest path between two vertices in an
     unweighted graph."))

;;; -------------------------------------------------------

(defun make-shortest-path-finder ()
  "Creates and returns a new ``Shortest-Path-Finder''."
  (the Shortest-Path-Finder
    (make-instance 'Shortest-Path-Finder)))

;;; -------------------------------------------------------

(defun reset-shortest-path-finder (path-finder)
  "Resets the PATH-FINDER's internal state and returns the modified
   object."
  (declare (type Shortest-Path-Finder path-finder))
  (queue-clear (slot-value path-finder 'vertex-queue))
  (clrhash     (slot-value path-finder 'visitations))
  (clrhash     (slot-value path-finder 'predecessors))
  (the Shortest-Path-Finder path-finder))

;;; -------------------------------------------------------

(defun find-predecessors (path-finder graph start end)
  "Determines the shortest path in the GRAPH from the START vertex to
   the END vertex, creates a predecessor table, which maps to each
   vertex in the role of a destination the edge from its source to it,
   stores this table in the PATH-FINDER, and returns the latter.
   ---
   The thus computed predecessor table associates, starting with the
   specified END node, each destination vertex with the edge which was
   followed by the respective source vertex in order to reach it. By
   traversing the table from the END to the START, the reverse order of
   transitions (edge values) necessary for the opposite direction, from
   the START to the END, can be received."
  (declare (type Shortest-Path-Finder path-finder))
  (declare (type Graph                graph))
  (declare (type state                start))
  (declare (type state                end))
  
  (with-slots (vertex-queue visitations predecessors) path-finder
    (declare (type Queue                         vertex-queue))
    (declare (type (hash-table-of state boolean) visitations))
    (declare (type (hash-table-of state Edge)    predecessors))
    
    (labels
        ((set-state-visited (state)
          "Marks the STATE as visited and returns no value."
          (declare (type state state))
          (setf (gethash state visitations) T)
          (values))
         
         (state-visited-p (state)
          "Checks whether the STATE is marked as visited, returning on
           confirmation a ``boolean'' value of ``T'', otherwise
           ``NIL''."
          (declare (type state state))
          (the boolean (gethash state visitations)))
         
         (store-predecessor (state transition predecessor)
          "Associates the STATE with the PREDECESSOR and returns no
           value."
          (declare (type state state))
          (declare (type bit   transition))
          (declare (type state predecessor))
          (setf (gethash state predecessors)
                (make-edge predecessor transition state))
          (values))
         
         (visit-neighbor (current-state transition neighbor)
          "Visits the NEIGHBOR state, being an opposite of the
           CURRENT-STATE, and returns a ``boolean'' value of ``T'' if
           the NEIGHBOR represents the END state, otherwise ``NIL''."
          (declare (type state current-state))
          (declare (type state neighbor))
          (unless (state-visited-p neighbor)
            (set-state-visited neighbor)
            (queue-enqueue     vertex-queue neighbor)
            (store-predecessor neighbor transition current-state)
            (the boolean
              (when (= neighbor end)
                (queue-clear vertex-queue)
                T)))))
      
      ;; Visit and add the START state to the VERTEX-QUEUE.
      (set-state-visited start)
      (queue-enqueue     vertex-queue start)
      
      (loop
        until (queue-empty-p vertex-queue)
        for   current-state of-type state = (queue-dequeue vertex-queue)
        do
          (destructuring-bind (state-if-0 . state-if-1)
              (graph-get-outgoing-states graph current-state)
            (declare (type state state-if-0))
            (declare (type state state-if-1))
            (cond
              ((visit-neighbor current-state 0 state-if-0)
                (loop-finish))
              ((visit-neighbor current-state 1 state-if-1)
                (loop-finish))
              (T
                NIL))))))
  
  (the Shortest-Path-Finder path-finder))

;;; -------------------------------------------------------

(defun build-path (path-finder end)
  "Returns a list containing the necessary transitions for reaching the
   END state using the PATH-FINDER's precomputed predecessors states."
  (declare (type Shortest-Path-Finder path-finder))
  (declare (type state                end))
  (flet ((get-predecessor (state)
          "Returns the edge arriving into this state, or ``NIL'' if no
           such association exists among the PREDECESSORS."
          (declare (type state state))
          (the (or null Edge)
            (gethash state
              (slot-value path-finder 'predecessors)))))
    
    (let ((path         NIL)
          (current-edge (get-predecessor end)))
      (declare (type (list-of bit)  path))
      (declare (type (or null Edge) current-edge))
      
      (loop while current-edge do
        (push (edge-transition current-edge) path)
        (setf current-edge
          (get-predecessor
            (edge-source current-edge))))
    
    (the (list-of bit) path))))

;;; -------------------------------------------------------

(defun shortest-path-finder-get-path (path-finder graph start end)
  "Employs the PATH-FINDER in order to detect the shortest path in the
   GRAPH from the START to the END state, returning a list of bit-valued
   transitions.
   ---
   If, starting at the GRAPH's START vertex (state), the detected
   transitions building the shortest path are followed, this motion
   culminates in the arrival at the END vertex (state) "
  (declare (type Shortest-Path-Finder path-finder))
  (declare (type Graph                graph))
  (declare (type state                start))
  (declare (type state                end))
  (reset-shortest-path-finder path-finder)
  (find-predecessors          path-finder graph start end)
  (the (list-of state) (build-path path-finder end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Array Changer text generator.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Shortest-Path-Finder +SHORTEST-PATH-FINDER+))

;;; -------------------------------------------------------

(defparameter +SHORTEST-PATH-FINDER+ (make-shortest-path-finder)
  "The default transition calculator for the detection of paths inside
   of a state machine.")

;;; -------------------------------------------------------

(defun get-transitions-for-text
    (graph text
     &key (start-state #x00)
          (path-finder +SHORTEST-PATH-FINDER+))
  "Given a GRAPH, and an optional START-START designating its initially
   selected vertex (state), creates a ``supply'' of transitions, each
   such a list of bits which, starting from the current state, may be
   followed to arrive at the state tantamount to the next TEXT
   character's ASCII code by using the PATH-FINDER for the transition
   generation."
  (declare (type string               text))
  (declare (type state                start-state))
  (declare (type Shortest-Path-Finder path-finder))
  
  (let ((current-character-index 0)
        (current-character       NIL)
        (current-state           start-state)
        (next-state              start-state))
    (declare (type fixnum              current-character-index))
    (declare (type (or null character) current-character))
    (declare (type state               current-state))
    (declare (type state               next-state))
    
    (labels
        ((has-more-characters-p ()
          "Checks whether at least one unprocessed character remains in
           the TEXT, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (the boolean
            (not (null
              (< current-character-index (length text))))))
         
         (update-state ()
          "Updates the CURRENT-CHARACTER, CURRENT-STATE, and NEXT-STATE
           based on the value of the CURRENT-CHARACTER-INDEX, and
           returns no value."
          (when (has-more-characters-p)
            (setf current-character
              (char text current-character-index))
            (shiftf current-state next-state
              (char-code current-character)))
          (values)))
      
      (update-state)
      
      (the function
        #'(lambda ()
            (when (has-more-characters-p)
              (multiple-value-prog1
                (values
                  (shortest-path-finder-get-path
                    path-finder graph current-state next-state)
                  T)
                (incf current-character-index)
                (update-state))))))))

;;; -------------------------------------------------------

(defun generate-text-program
    (graph text
     &key (destination NIL)
          (start-state #x00)
          (path-finder +SHORTEST-PATH-FINDER+))
  "Given a GRAPH, and an optional START-START designating its initially
   selected vertex (state), creates an Array Changer program capable of
   reproducing and printing the TEXT's ASCII character codes, using the
   PATH-FINDER for the traversals from one state to another, and writes
   the thus generated code to the DESTINATION, producing a value of
   ``NIL'' for a non-``NIL'' DESTINATION, otherwise creating and
   returning a fresh string containing the result.
   ---
   Please note that, in order to output the ASCII character codes, this
   function operates on a single cell only, applying upon it the
   sedentary Array Changer commands \"c\", \"C\", and \"O\" only."
  (declare (type string               text))
  (declare (type destination          destination))
  (declare (type state                start-state))
  (declare (type Shortest-Path-Finder path-finder))
  
  (the (or null string)
    (if destination
      (flet
          ((write-transitions (transitions)
            "Writes the instructions requisite for navigating using
             the TRANSITIONS and printing the terminating state to the
             DESTINATION, and returns no value."
            (declare (type (list-of bit) transitions))
            (loop for transition of-type bit in transitions do
              (format destination "~c"
                (case transition
                  (0 #\c)
                  (1 #\C)
                  (otherwise
                    (error "Invalid transition: ~d. ~
                            Expected either 0 or 1."
                      transition)))))
            (format destination "O")
            (values)))
        
        (let ((transition-generator
                (get-transitions-for-text graph text
                  :start-state start-state
                  :path-finder path-finder)))
          (declare (type (supply (list-of bit)) transition-generator))
          (loop do
            (multiple-value-bind (transitions has-transitions-p)
                (funcall transition-generator)
              (declare (type (list-of bit) transitions))
              (declare (type boolean       has-transitions-p))
              (if has-transitions-p
                (write-transitions transitions)
                (loop-finish))))))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program graph text
          :destination output
          :start-state start-state
          :path-finder path-finder)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transitions:
;;   (0 0 0 1 0 1 0 0 0)
(interpret-Array-Changer "rrrRrRrrrO")

;;; -------------------------------------------------------

;; Change the current cell's value to the state 247 (= 0xF7) and print
;; it an equinumerant tally of times.
(interpret-Array-Changer "CFO")

;;; -------------------------------------------------------

;; Generate the Array Changer code for assigning to the first cell in
;; turn the ASCII codes for the text "Hello, World!", that is,
;;   72 101 108 108 111 44 32 87 111 114 108 100 33
;; and printing them, resulting in the program
;;   cccCcCcccOccCcCCCCCCcccOcCCCCCCcCOOCccCCOCcCccCccOCccCCCcCcCcCcCcO
;;   CCccCCcCOcCCcccCCcOCcCCcCCCcOcCccCcccCCOCCCOccccCCO
(generate-text-program +STATE-GRAPH+ "Hello, World!")

;;; -------------------------------------------------------

;; Generate the Array Changer code for assigning to the first cell in
;; turn the ASCII codes for the text "Hello, World!", that is,
;;   72 101 108 108 111 44 32 87 111 114 108 100 33
;; and printing them, and execute the program.
(interpret-Array-Changer
  (generate-text-program +STATE-GRAPH+ "Hello, World!"))

;;; -------------------------------------------------------

;; Generate a supply of transitions for visiting the states
;; corresponding to the ASCII codes of the text "Hello, World!", namely
;;   72 101 108 108 111 44 32 87 111 114 108 100 33
;; and print each character's transition sequence on a separate line.
(let ((transition-generator
        (get-transitions-for-text +STATE-GRAPH+ "Hello, World!")))
  (declare (type (supply (list-of bit)) transition-generator))
  (loop do
    (multiple-value-bind (transitions has-transitions-p)
        (funcall transition-generator)
      (declare (type (list-of bit) transitions))
      (declare (type boolean       has-transitions-p))
      (if has-transitions-p
        (print transitions)
        (loop-finish)))))
