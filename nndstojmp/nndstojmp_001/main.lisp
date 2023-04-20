;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "nndstojmp", presented by the Esolang user "Jan Gamecuber"
;; in the year 2022, and founded upon the application of a single
;; four-parameter instruction to pursuit the purposes of calculating the
;; NAND-combination of memory cells, governing input and output, as well
;; as navigating across the program.
;; 
;; 
;; Concept
;; =======
;; nndstojmp is founded upon the manipulation of a random-access memory
;; of unsigned bytes, lending a commorancy to both the program and the
;; data, manipulated by NAND-combinations only, and providing all
;; services --- including input, output, and control flow --- in a
;; single instruction whose non-negative integer parameters determine
;; the desiderated facilities.
;; 
;; == nndstojmp: {N}A{ND}, {STO}RE, {J}U{MP} ==
;; Concinnously, the language "nndstojmp" is nevened in such a manner as
;; to already bewray its foundational concepts and capacities, being a
;; curtailed intimation to "nand", "store", "jump".
;; 
;; == nndstojmp: AN OISC ==
;; The nndstojmp programming language constitutes a specimen adhering to
;; the "One Instruction Set Computer" (OISC) concept, the instructions
;; of the same class are represented by a single variant, with the
;; discriminating criterion being either the tally of operands, their
;; concrete values, or a combination therefrom. nndstojmp subscribes to
;; the second mechanism: all facilities, including the NAND-combination,
;; input, output, and control flow, represent variables of the actual
;; parameters received.
;; 
;; == ONE INSTRUCTION, FOUR PARAMETERS, MANIFOLD CAPACITIES ==
;; Each instruction is defined as a tetrad of four non-negative integer
;; numbers, with the element zero (0) appropriated as a sentinel for
;; particular causata.
;; 
;; == NAND: THE ONLY MANIPULATIVE WARKLUME ==
;; The singular element of memory manipulation, the logical NAND
;; operation is applied to a twain of memory cell values --- or
;; alternatively to one or two input bytes ---, with the result being
;; stored in a desired location, or printed to the standard output. No
;; other facilities for data manipulation exist.
;; 
;; == ONE MEMORY SPACE FOR PROGRAM AND DATA ==
;; The memory space definition proceeds from a one-based indexing of
;; random-access cells, each a salvatory to a single byte, and intended
;; a target deployment target for the code and data as concomitants.
;; 
;; Incipiently at the first location, one (1), at the memory, the
;; responsible pointer consumes a tetrad of adjacent cells values, forms
;; an instruction, contingently modifying a cell at some index, and, if
;; not mandated to cease, relocates to the next location for a
;; subsequent iteration. In corollary, any memory datum may be at any
;; time an instruction pointer (IP), operation parameter, or a
;; combination thereof.
;; 
;; == COLLECT PARAMETERS, EXECUTE, REPEAT ==
;; The following iterative procedure applies to the program execution:
;; 
;;   (1) Commencing with the element at the current memory pointer
;;       position, collect the four subsequent cell values into a single
;;       instruction, designated as the parameter quadruple
;;         (a, b, c, d)
;;       At the program's inchoation, the memory pointer resides at the
;;       first memory location one (1).
;;   (2) Access the memory cell m[a] at location a.
;;   (3) Access the memory cell m[b] at location b.
;;   (4) NAND-combine the cell values m[a] and m[b] to x, with
;;         x = m[a] NAND m[b].
;;   (5) If c denotes a non-zero location, store x in the memory cell
;;       m[c]. Otherwise, only print x to the standard output.
;;   (6) If d denotes a non-zero location, relocate the memory pointer
;;       to d, and repeat at step (1). Otherwise, terminate the program
;;       immediately.
;; 
;; 
;; Architecture
;; ============
;; Programs in nndstojmp rely on an infinite random-access memory
;; compact of unsigned bytes. Ensuing from the zero (0) byte's sentinel
;; role in all instruction parameters --- designating either input,
;; output, or program termination --- the consectary redes that the
;; memory cells are enumerated starting with the inclusive location
;; one (1).
;; 
;; The memory lends contemporanoues harborage to the instruction
;; parameters as well as data.
;; 
;; In the inchoate program state resident upon the first cell at the
;; index one (1), the memory pointer at any execution cycle refers to
;; the four subsequent items as the current operation parameters in the
;; form of indices into the memory containing the acutal operand values.
;; The ensuing jump command relocates the pointer to the head of the
;; parameter tetrad.
;; 
;; These values, in the memory's second aspect, provide the data
;; portion.
;; 
;; 
;; Data Types
;; ==========
;; The data castaldy is restricted to the memory's currency: unsigned
;; bytes that occupy the integer range [0, 255].
;; 
;; 
;; Syntax
;; ======
;; Its status as an OISC apportions to nndstojmp throughout homogeneity
;; in the syntactial expression, with all instructions composed of
;; exactly four non-negative integer numbers, separated by one or more
;; whitespaces, and demarcated from the peers in their surroundings via
;; the same ilk of sepiments.
;; 
;; == INSTRUCTIONS ==
;; Every instruction is communicated by a quadruple sequence of
;; non-negative integer literals, the intersticial positions being the
;; wonings of whitespaces. These sepiments also apply themselves to the
;; subsequent instructions' demarcation.
;; 
;; == WHITESPACES ==
;; Whitespaces, subsuming in the terminology the space, tab, and newline
;; character, impose an imperative betwixt an instruction's operands, as
;; as well betwixt two adjacent instructions. At any other occasion in
;; the code their allotment resolves to one's own deliberations.
;; 
;; == COMMENTS ==
;; No provisions for comments partake of the current language iteration.
;; 
;; == GRAMMAR ==
;; The language's syntactical aspect can be expressed in the Extended
;; Backus-Naur Form (EBNF):
;; 
;;   program     := [ [ separator ]
;;               ,    instruction
;;               ,    { separator , [ instruction ] } ]
;;               ;
;;   instruction := location , separator
;;               ,  location , separator
;;               ,  location , separator
;;               ,  location
;;               ;
;;   location    := digit , [ digit , [ digit ] ] ;
;;   digit       := "0" | "1" | "2" | "3" | "4"
;;               |  "5" | "6" | "7" | "8" | "9"
;;               ;
;;   separator   := whitespace , { whitespace } ;
;;   whitespace  := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; nndstojmp's instruction set is already exhausted by an aefauld
;; member, the variegated causata of which emanate from the
;; configuration of the quadruple non-negative integer parameters.
;; 
;; == FOUR PARAMETERS DEFINE THE INSTRUCTION ==
;; The sole instruction expects a tetrade of non-negative integers,
;; nevened as the positional parameters "a", "b", "c", and "d".
;; 
;; The following tabular illustration shall administer a compendious
;; nortelry concerning the four parameters and their roles:
;; 
;;   ------------------------------------------------------------------
;;   Pos. | Name | Role
;;   -----+------+-----------------------------------------------------
;;     1  |  a   | First (left) NAND operand. Can be a user input.
;;   ..................................................................
;;     2  |  b   | Second (right) NAND operand. Can be a user input.
;;   ..................................................................
;;     3  |  c   | NAND result receiver: Either stores the same in the
;;        |      | memory, or prints it to the standard output.
;;   ..................................................................
;;     4  |  d   | Control flow: Either jumps to a specified position
;;        |      | in the memory, or terminates the program.
;;   ------------------------------------------------------------------
;; 
;; == PARAMETER BEHAVIOR DEPENDS ON VALUE ==
;; As entailed in the statements adduced above, the value induction
;; renders the parameter's effect, its ramosity bifurcated into the
;; sentinel zero (0) and the Procrustean haecceity applicative to the
;; positive integers.
;; 
;; The following table shall educate about the respondency of the
;; parameter values in deportment according to their concrete value:
;; 
;;   ------------------------------------------------------------------
;;   Parameter | Value | Effect
;;   ----------+-------+-----------------------------------------------
;;   a         |  > 0  | Retrieves the byte at the memory location a
;;             |       | and memorizes it as the first operand for
;;             |       | further processing.
;;             |.......................................................
;;             |  = 0  | Queries the user for an input byte and
;;             |       | memorizes it as the first operand for futher
;;             |       | processing.
;;   ------------------------------------------------------------------
;;   b         |  > 0  | Retrieves the byte at the memory location b
;;             |       | and memorizes it as the second operand for
;;             |       | further processing.
;;             |.......................................................
;;             |  = 0  | Queries the user for an input byte and
;;             |       | memorizes it as the second operand for futher
;;             |       | processing.
;;   ------------------------------------------------------------------
;;   c         |  > 0  | Stores the result produced by the parameters a
;;             |       | and b through a NAND-combination at the memory
;;             |       | location c.
;;             |.......................................................
;;             |  = 0  | Prints the result produced by the parameters a
;;             |       | and b through a NAND-combination to the
;;             |       | standard output.
;;   ------------------------------------------------------------------
;;   d         |  > 0  | Relocates the memory pointer to the one-based
;;             |       | position that equals the location d.
;;             |.......................................................
;;             |  = 0  | Terminates the program immediately.
;;   ------------------------------------------------------------------
;; 
;; The nndstojmp execution model shall be subject to a replication from
;; the "Concept" section:
;; 
;;   (1) Commencing with the element at the current memory pointer
;;       position, collect the four subsequent cell values into a single
;;       instruction, designated as the parameter quadruple
;;         (a, b, c, d)
;;       At the program's inchoation, the memory pointer resides at the
;;       first memory location one (1).
;;   (2) Access the memory cell m[a] at location a.
;;   (3) Access the memory cell m[b] at location b.
;;   (4) NAND-combine the cell values m[a] and m[b] to x, with
;;         x = m[a] NAND m[b].
;;   (5) If c denotes a non-zero location, store x in the memory cell
;;       m[c]. Otherwise, only print x to the standard output.
;;   (6) If d denotes a non-zero location, relocate the memory pointer
;;       to d, and repeat at step (1). Otherwise, terminate the program
;;       immediately.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The compendiousness adhibited to the protolog inflicts the nndstojmp
;; programming language with a certain mete of ambiguities, a subset of
;; the same shall be the following sections' cynosure.
;; 
;; == WHAT ILK OF INPUT IS EXPECTED? ==
;; A zero-sentinel in the parameters "a" or "b" incites the query for a
;; user input in lieu of the direct memory index interpretation. It is,
;; however, not stated whether the input shall supply a byte value or a
;; memory index.
;; 
;; Owing to the already extant capacitation to provide an index in a
;; literal mode, it has been adjudged that the user input must assume
;; the form of an unsigned byte in the range [0, 255]. Any communication
;; deviating from this imposition shall inflict an error of an
;; unspecified type upon the program in operation.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's materialization is fulfilled in the programming
;; language Common Lisp.
;; 
;; 
;; Appendices
;; ==========
;; Topics whose extent or material is reckoned beyond the eligibility to
;; partake of the main document body shall at this place be explicated
;; as subsidiaries.
;; 
;; == APPENDIX A: NAND TABLE ==
;; The Boolean NAND operator derives from the nomenclature *N*ot *AND*,
;; signifying its reversal of the AND logic. Counterdistinguished from
;; its cleronomy, which produces "true" only for two "true" inputs,
;; otherwise always responding with "false",  the NAND function deems
;; any combination "true" as long as not both inputs assume the "true"
;; state.
;; 
;; The NAND truth table, accompanied by its AND base, shall be adduced
;; below:
;; 
;;   -----------------------------
;;    A | B | NAND(A,B) | AND(A,B)
;;   ---+---+-----------+---------
;;    0 | 0 | 1         | 0
;;    0 | 1 | 1         | 0
;;    1 | 0 | 1         | 0
;;    1 | 1 | 0         | 1
;;   -----------------------------
;; 
;; == APPENDIX B: BYTE RESTORATION USING A NAND-COMBINATION ==
;; A desiderated unsigned byte value's restoration from the
;; NAND-combination of two operands borrowed from the same type can be
;; accomplished in several ways; a structed solution can be extracted
;; with the gnarity that through combinations with 255 one can yield any
;; value in the range [0, 255], presupposing a correct second operand.
;; 
;; Based upon the following equation, which, given a byte value in the
;; range [0, 255] to restore, a variable y in conjunction with the
;; potent constant 255 can be utilized to forumulate
;; 
;;   y = (255 - x) NAND 255
;; 
;; With the NAND operation's commutative nature, we obtain the
;; additional gnarity
;; 
;;   y = (255 - x) NAND 255
;;     = 255       NAND (255 - x)
;; 
;; A forbisen of this, if pursuing to obtain the value x = 4 via NAND,
;; we supputate the variable operand y as
;; 
;;   y = (255 - x) NAND 255
;;     = (255 - 4) NAND 255
;;     = 251
;;       ===
;; 
;; This means that 251 must be NAND-combined with 255 in order to yield
;; 4:
;; 
;;   4 = 251 NAND 255
;;     = 255 NAND 251.
;; 
;; Despite the contingency to evaluate the desideratum by the just
;; adduced formula's adminculum, the following table will extend a
;; compendious collocation of target byte values and their potential
;; operand combinations, the first specimen of which contributes the
;; variable moeity, with concomitant fixation of the second one. Please
;; note again that, ensuing from the NAND operation's commutative
;; nature, the two inputs may be exchanged without the output byte's
;; corruption.
;; 
;;   -----------------------
;;   NAND(A, B) |  A  |  B
;;   -----------+-----+-----
;;         0    | 255 | 255
;;   .......................
;;         1    | 254 | 255
;;   .......................
;;         2    | 253 | 255
;;   .......................
;;         3    | 252 | 255
;;   .......................
;;         4    | 251 | 255
;;   .......................
;;         5    | 250 | 255
;;   .......................
;;         6    | 249 | 255
;;   .......................
;;         7    | 248 | 255
;;   .......................
;;         8    | 247 | 255
;;   .......................
;;         9    | 246 | 255
;;   .......................
;;        10    | 245 | 255
;;   .......................
;;        11    | 244 | 255
;;   .......................
;;        12    | 243 | 255
;;   .......................
;;        13    | 242 | 255
;;   .......................
;;        14    | 241 | 255
;;   .......................
;;        15    | 240 | 255
;;   .......................
;;        16    | 239 | 255
;;   .......................
;;        17    | 238 | 255
;;   .......................
;;        18    | 237 | 255
;;   .......................
;;        19    | 236 | 255
;;   .......................
;;        20    | 235 | 255
;;   .......................
;;        21    | 234 | 255
;;   .......................
;;        22    | 233 | 255
;;   .......................
;;        23    | 232 | 255
;;   .......................
;;        24    | 231 | 255
;;   .......................
;;        25    | 230 | 255
;;   .......................
;;        26    | 229 | 255
;;   .......................
;;        27    | 228 | 255
;;   .......................
;;        28    | 227 | 255
;;   .......................
;;        29    | 226 | 255
;;   .......................
;;        30    | 225 | 255
;;   .......................
;;        31    | 224 | 255
;;   .......................
;;        32    | 223 | 255
;;   .......................
;;        33    | 222 | 255
;;   .......................
;;        34    | 221 | 255
;;   .......................
;;        35    | 220 | 255
;;   .......................
;;        36    | 219 | 255
;;   .......................
;;        37    | 218 | 255
;;   .......................
;;        38    | 217 | 255
;;   .......................
;;        39    | 216 | 255
;;   .......................
;;        40    | 215 | 255
;;   .......................
;;        41    | 214 | 255
;;   .......................
;;        42    | 213 | 255
;;   .......................
;;        43    | 212 | 255
;;   .......................
;;        44    | 211 | 255
;;   .......................
;;        45    | 210 | 255
;;   .......................
;;        46    | 209 | 255
;;   .......................
;;        47    | 208 | 255
;;   .......................
;;        48    | 207 | 255
;;   .......................
;;        49    | 206 | 255
;;   .......................
;;        50    | 205 | 255
;;   .......................
;;        51    | 204 | 255
;;   .......................
;;        52    | 203 | 255
;;   .......................
;;        53    | 202 | 255
;;   .......................
;;        54    | 201 | 255
;;   .......................
;;        55    | 200 | 255
;;   .......................
;;        56    | 199 | 255
;;   .......................
;;        57    | 198 | 255
;;   .......................
;;        58    | 197 | 255
;;   .......................
;;        59    | 196 | 255
;;   .......................
;;        60    | 195 | 255
;;   .......................
;;        61    | 194 | 255
;;   .......................
;;        62    | 193 | 255
;;   .......................
;;        63    | 192 | 255
;;   .......................
;;        64    | 191 | 255
;;   .......................
;;        65    | 190 | 255
;;   .......................
;;        66    | 189 | 255
;;   .......................
;;        67    | 188 | 255
;;   .......................
;;        68    | 187 | 255
;;   .......................
;;        69    | 186 | 255
;;   .......................
;;        70    | 185 | 255
;;   .......................
;;        71    | 184 | 255
;;   .......................
;;        72    | 183 | 255
;;   .......................
;;        73    | 182 | 255
;;   .......................
;;        74    | 181 | 255
;;   .......................
;;        75    | 180 | 255
;;   .......................
;;        76    | 179 | 255
;;   .......................
;;        77    | 178 | 255
;;   .......................
;;        78    | 177 | 255
;;   .......................
;;        79    | 176 | 255
;;   .......................
;;        80    | 175 | 255
;;   .......................
;;        81    | 174 | 255
;;   .......................
;;        82    | 173 | 255
;;   .......................
;;        83    | 172 | 255
;;   .......................
;;        84    | 171 | 255
;;   .......................
;;        85    | 170 | 255
;;   .......................
;;        86    | 169 | 255
;;   .......................
;;        87    | 168 | 255
;;   .......................
;;        88    | 167 | 255
;;   .......................
;;        89    | 166 | 255
;;   .......................
;;        90    | 165 | 255
;;   .......................
;;        91    | 164 | 255
;;   .......................
;;        92    | 163 | 255
;;   .......................
;;        93    | 162 | 255
;;   .......................
;;        94    | 161 | 255
;;   .......................
;;        95    | 160 | 255
;;   .......................
;;        96    | 159 | 255
;;   .......................
;;        97    | 158 | 255
;;   .......................
;;        98    | 157 | 255
;;   .......................
;;        99    | 156 | 255
;;   .......................
;;       100    | 155 | 255
;;   .......................
;;       101    | 154 | 255
;;   .......................
;;       102    | 153 | 255
;;   .......................
;;       103    | 152 | 255
;;   .......................
;;       104    | 151 | 255
;;   .......................
;;       105    | 150 | 255
;;   .......................
;;       106    | 149 | 255
;;   .......................
;;       107    | 148 | 255
;;   .......................
;;       108    | 147 | 255
;;   .......................
;;       109    | 146 | 255
;;   .......................
;;       110    | 145 | 255
;;   .......................
;;       111    | 144 | 255
;;   .......................
;;       112    | 143 | 255
;;   .......................
;;       113    | 142 | 255
;;   .......................
;;       114    | 141 | 255
;;   .......................
;;       115    | 140 | 255
;;   .......................
;;       116    | 139 | 255
;;   .......................
;;       117    | 138 | 255
;;   .......................
;;       118    | 137 | 255
;;   .......................
;;       119    | 136 | 255
;;   .......................
;;       120    | 135 | 255
;;   .......................
;;       121    | 134 | 255
;;   .......................
;;       122    | 133 | 255
;;   .......................
;;       123    | 132 | 255
;;   .......................
;;       124    | 131 | 255
;;   .......................
;;       125    | 130 | 255
;;   .......................
;;       126    | 129 | 255
;;   .......................
;;       127    | 128 | 255
;;   .......................
;;       128    | 127 | 255
;;   .......................
;;       129    | 126 | 255
;;   .......................
;;       130    | 125 | 255
;;   .......................
;;       131    | 124 | 255
;;   .......................
;;       132    | 123 | 255
;;   .......................
;;       133    | 122 | 255
;;   .......................
;;       134    | 121 | 255
;;   .......................
;;       135    | 120 | 255
;;   .......................
;;       136    | 119 | 255
;;   .......................
;;       137    | 118 | 255
;;   .......................
;;       138    | 117 | 255
;;   .......................
;;       139    | 116 | 255
;;   .......................
;;       140    | 115 | 255
;;   .......................
;;       141    | 114 | 255
;;   .......................
;;       142    | 113 | 255
;;   .......................
;;       143    | 112 | 255
;;   .......................
;;       144    | 111 | 255
;;   .......................
;;       145    | 110 | 255
;;   .......................
;;       146    | 109 | 255
;;   .......................
;;       147    | 108 | 255
;;   .......................
;;       148    | 107 | 255
;;   .......................
;;       149    | 106 | 255
;;   .......................
;;       150    | 105 | 255
;;   .......................
;;       151    | 104 | 255
;;   .......................
;;       152    | 103 | 255
;;   .......................
;;       153    | 102 | 255
;;   .......................
;;       154    | 101 | 255
;;   .......................
;;       155    | 100 | 255
;;   .......................
;;       156    |  99 | 255
;;   .......................
;;       157    |  98 | 255
;;   .......................
;;       158    |  97 | 255
;;   .......................
;;       159    |  96 | 255
;;   .......................
;;       160    |  95 | 255
;;   .......................
;;       161    |  94 | 255
;;   .......................
;;       162    |  93 | 255
;;   .......................
;;       163    |  92 | 255
;;   .......................
;;       164    |  91 | 255
;;   .......................
;;       165    |  90 | 255
;;   .......................
;;       166    |  89 | 255
;;   .......................
;;       167    |  88 | 255
;;   .......................
;;       168    |  87 | 255
;;   .......................
;;       169    |  86 | 255
;;   .......................
;;       170    |  85 | 255
;;   .......................
;;       171    |  84 | 255
;;   .......................
;;       172    |  83 | 255
;;   .......................
;;       173    |  82 | 255
;;   .......................
;;       174    |  81 | 255
;;   .......................
;;       175    |  80 | 255
;;   .......................
;;       176    |  79 | 255
;;   .......................
;;       177    |  78 | 255
;;   .......................
;;       178    |  77 | 255
;;   .......................
;;       179    |  76 | 255
;;   .......................
;;       180    |  75 | 255
;;   .......................
;;       181    |  74 | 255
;;   .......................
;;       182    |  73 | 255
;;   .......................
;;       183    |  72 | 255
;;   .......................
;;       184    |  71 | 255
;;   .......................
;;       185    |  70 | 255
;;   .......................
;;       186    |  69 | 255
;;   .......................
;;       187    |  68 | 255
;;   .......................
;;       188    |  67 | 255
;;   .......................
;;       189    |  66 | 255
;;   .......................
;;       190    |  65 | 255
;;   .......................
;;       191    |  64 | 255
;;   .......................
;;       192    |  63 | 255
;;   .......................
;;       193    |  62 | 255
;;   .......................
;;       194    |  61 | 255
;;   .......................
;;       195    |  60 | 255
;;   .......................
;;       196    |  59 | 255
;;   .......................
;;       197    |  58 | 255
;;   .......................
;;       198    |  57 | 255
;;   .......................
;;       199    |  56 | 255
;;   .......................
;;       200    |  55 | 255
;;   .......................
;;       201    |  54 | 255
;;   .......................
;;       202    |  53 | 255
;;   .......................
;;       203    |  52 | 255
;;   .......................
;;       204    |  51 | 255
;;   .......................
;;       205    |  50 | 255
;;   .......................
;;       206    |  49 | 255
;;   .......................
;;       207    |  48 | 255
;;   .......................
;;       208    |  47 | 255
;;   .......................
;;       209    |  46 | 255
;;   .......................
;;       210    |  45 | 255
;;   .......................
;;       211    |  44 | 255
;;   .......................
;;       212    |  43 | 255
;;   .......................
;;       213    |  42 | 255
;;   .......................
;;       214    |  41 | 255
;;   .......................
;;       215    |  40 | 255
;;   .......................
;;       216    |  39 | 255
;;   .......................
;;       217    |  38 | 255
;;   .......................
;;       218    |  37 | 255
;;   .......................
;;       219    |  36 | 255
;;   .......................
;;       220    |  35 | 255
;;   .......................
;;       221    |  34 | 255
;;   .......................
;;       222    |  33 | 255
;;   .......................
;;       223    |  32 | 255
;;   .......................
;;       224    |  31 | 255
;;   .......................
;;       225    |  30 | 255
;;   .......................
;;       226    |  29 | 255
;;   .......................
;;       227    |  28 | 255
;;   .......................
;;       228    |  27 | 255
;;   .......................
;;       229    |  26 | 255
;;   .......................
;;       230    |  25 | 255
;;   .......................
;;       231    |  24 | 255
;;   .......................
;;       232    |  23 | 255
;;   .......................
;;       233    |  22 | 255
;;   .......................
;;       234    |  21 | 255
;;   .......................
;;       235    |  20 | 255
;;   .......................
;;       236    |  19 | 255
;;   .......................
;;       237    |  18 | 255
;;   .......................
;;       238    |  17 | 255
;;   .......................
;;       239    |  16 | 255
;;   .......................
;;       240    |  15 | 255
;;   .......................
;;       241    |  14 | 255
;;   .......................
;;       242    |  13 | 255
;;   .......................
;;       243    |  12 | 255
;;   .......................
;;       244    |  11 | 255
;;   .......................
;;       245    |  10 | 255
;;   .......................
;;       246    |   9 | 255
;;   .......................
;;       247    |   8 | 255
;;   .......................
;;       248    |   7 | 255
;;   .......................
;;       249    |   6 | 255
;;   .......................
;;       250    |   5 | 255
;;   .......................
;;       251    |   4 | 255
;;   .......................
;;       252    |   3 | 255
;;   .......................
;;       253    |   2 | 255
;;   .......................
;;       254    |   1 | 255
;;   .......................
;;       255    |   0 | 255
;;   -----------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-04-19
;; 
;; Sources:
;;   [esolang2020NRefine]
;;   The Esolang contributors, "N Refine", 2020
;;   URL: "https://esolangs.org/wiki/N_Refine"
;;   Notes:
;;     - An esoteric programming language similar to nndstojmp.
;;     - Its original author's interpreter can be obtained from
;;       "https://github.com/PythonshellDebugwindow/esolangs/blob/
;;        master/n-refine/n-refine.py".
;;   
;;   [esolang2022Subleq]
;;   The Esolang contributors, "Subleq", 2022
;;   URL: "https://esolangs.org/wiki/Subleq"
;;   Notes:
;;     - An esoteric programming language similar to nndstojmp.
;;   
;;   [esolang2022nndstojmp]
;;   The Esolang contributors, "nndstojmp", 2022
;;   URL: "https://esolangs.org/wiki/Nndstojmp"
;;   
;;   [esolang2023oisc]
;;   The Esolang contributors, "OISC", 2023
;;   URL: "https://esolangs.org/wiki/OISC"
;;   Notes:
;;     - Describes the OISC ("One Instruction Set Computer").
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype location ()
  "The ``location'' type defines a memory location as a non-negative
   integer of no intrinsic upper bound, that is, a commorant in the
   range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

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
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight
   consecutive bits and thus an occupant of the integer sub-range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, defaulting to the comprehensive
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

(deftype cell-table ()
  "The ``cell-table'' type defines the contingently infinite, non-negative
   integer-index program memory of byte cells in a sparse fashion by
   mediation of a hash table, mapping ``non-negative-integer'' keys to
   ``octet'' values."
  '(hash-table-of location octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, as
   employed in the invocation of the functions ``format'' and
   ``write-char'', among others."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype location-table ()
  "The ``location-table'' type defines an adminicular associative
   structure for associating characters with locations in a piece of
   nndstojmp code's data segment, manifesting as a hash table which maps
   character keys to location values."
  '(hash-table-of character location))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (a b c d)))
  "The ``Instruction'' class encapsulates the four parameters requisite
   for an nndstojmp instruction's representation."
  (a 0 :type location)
  (b 0 :type location)
  (c 0 :type location)
  (d 0 :type location))

;;; -------------------------------------------------------

(defun instruction-requests-input-for-a-p (instruction)
  "Determines whether the INSTRUCTION involves user input for the first
   parameter \"a\", returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (zerop (instruction-a instruction))))))

;;; -------------------------------------------------------

(defun instruction-requests-input-for-b-p (instruction)
  "Determines whether the INSTRUCTION involves user input for the second
   parameter \"b\", returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (zerop (instruction-b instruction))))))

;;; -------------------------------------------------------

(defun instruction-commits-output-p (instruction)
  "Determines whether the INSTRUCTION commits output, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (zerop (instruction-c instruction))))))

;;; -------------------------------------------------------

(defun instruction-terminates-program-p (instruction)
  "Determines whether the INSTRUCTION terminates the program, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (zerop (instruction-d instruction))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-empty-memory ()))
  "The ``Memory'' class represents an nndstojmp virtual machine's
   memory as a theoretically infinite space of bytes, amenable to random
   access by integer addresses, or \"locations\"."
  (cells         (make-hash-table :test #'eql) :type cell-table)
  (minimum-index 1                             :type location)
  (maximum-index 1                             :type location))

;;; -------------------------------------------------------

(defmacro with-memory ((memory) &body body)
  "Evaluates the MEMORY, binds its slots ``cells'', ``minimum-index''
   and ``maximum-index'' to eponymous local symbol macros, evaluates the
   BODY forms, and returns the last evaluated form's results."
  (let ((evaluated-memory (gensym)))
    (declare (type symbol evaluated-memory))
    `(let ((,evaluated-memory ,memory))
       (declare (type Memory ,evaluated-memory)
                (ignorable   ,evaluated-memory))
       (symbol-macrolet
           ((cells
             (the cell-table
               (memory-cells ,evaluated-memory)))
            (minimum-index
             (the location
               (memory-minimum-index ,evaluated-memory)))
            (maximum-index
             (the location
               (memory-maximum-index ,evaluated-memory))))
         (declare (type cell-table cells)
                  (ignorable       cells))
         (declare (type location   minimum-index)
                  (ignorable       minimum-index))
         (declare (type location   maximum-index)
                  (ignorable       maximum-index))
         ,@body))))

;;; -------------------------------------------------------

(defun memory-cell-at (memory location)
  "Returns the byte value of the MEMORY cell at the LOCATION."
  (declare (type Memory   memory))
  (declare (type location location))
  (with-memory (memory)
    (the octet
      (gethash location cells 0))))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory location)
  "Stores the NEW-VALUE into the MEMORY cell at the LOCATION and returns
   no value."
  (declare (type octet    new-value))
  (declare (type Memory   memory))
  (declare (type location location))
  
  (when (minusp location)
    (error "Location must be >= 0, but equals ~d." location))
  
  (with-memory (memory)
    (setf (gethash location cells 0) new-value)
    (setf maximum-index (max maximum-index location)))
  
  (values))

;;; -------------------------------------------------------

(defun memory-extract-instruction (memory start)
  "Commencing at the START location in the MEMORY, embraces the four
   successive bytes, and returns an ``Instruction'' employing the tetrad
   as the parameters \"a\", \"b\", \"c\", and \"d\", in this order."
  (declare (type Memory   memory))
  (declare (type location start))
  (with-memory (memory)
    (the Instruction
      (make-instruction
        (memory-cell-at memory (+ start 0))
        (memory-cell-at memory (+ start 1))
        (memory-cell-at memory (+ start 2))
        (memory-cell-at memory (+ start 3))))))

;;; -------------------------------------------------------

(defun build-memory (bytes)
  "Creates and returns a new ``Memory'' object, initialized at its
   incipent positions with the BYTES."
  (declare (type (list-of octet) bytes))
  (let ((memory (make-empty-memory)))
    (declare (type Memory memory))
    (loop
      for byte     of-type octet    in   bytes
      for location of-type location from 1 by 1
      do  (setf (memory-cell-at memory location) byte))
    (the Memory memory)))

;;; -------------------------------------------------------

(defun print-octet (destination
                    octet
                    colon-modifier-supplied-p
                    at-sign-modifier-supplied-p
                    &rest prefix-parameters)
  "Prints to the DESTINATION an OCTET, expecting the flags
   COLON-MODIFIER-SUPPLIED-P, AT-SIGN-MODIFIER-SUPPLIED-P, and the
   PREFIX-PARAMETERS all to resolve to ``NIL'', being not tolerated in
   this context, and returns ``NIL'' for a non-``NIL'' DESTINATION,
   otherwise responding with a fresh string comprehending the output.
   ---
   This function is compatible with the custom directive protocol in the
   Common Lisp standard function ``format'', amenable to an invocation
   via
     ~/print-octet/"
  (declare (type destination destination))
  (declare (type octet       octet))
  (declare (type T           colon-modifier-supplied-p))
  (declare (type T           at-sign-modifier-supplied-p))
  (declare (type (list-of T) prefix-parameters))
  (when colon-modifier-supplied-p
    (error "PRINT-OCTET does not accept the colon modifier \":\"."))
  (when at-sign-modifier-supplied-p
    (error "PRINT-OCTET does not accept the at-sign modifier \"@\"."))
  (when prefix-parameters
    (error "PRINT-OCTET does not accept prefix parameters, ~
            but received ~s."
      prefix-parameters))
  (format destination "~3,' d" octet))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) stream)
  (declare (type Memory      memory))
  (declare (type destination stream))
  (with-memory (memory)
    (let ((number-of-rows (ceiling (hash-table-count cells) 4)))
      (declare (type (integer 0 *) number-of-rows))
      (loop
        for row of-type (integer 0 *) from 0 below number-of-rows
        do
          (format stream "~&~/print-octet/ ~
                            ~/print-octet/ ~
                            ~/print-octet/ ~
                            ~/print-octet/"
            (memory-cell-at memory (+ (* row 4) 1))
            (memory-cell-at memory (+ (* row 4) 2))
            (memory-cell-at memory (+ (* row 4) 3))
            (memory-cell-at memory (+ (* row 4) 4)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun eof-p (source position)
  "Checks whether the POSITION designates a location outside of the
   SOURCE's boundaries, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (array-in-bounds-p source position))))

;;; -------------------------------------------------------

(defun character-at (source position)
  "Returns the character at the POSITION in the SOURCE, or ``NIL'' upon
   the location designator's transgression of the SOURCE's bounds."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun digit-at-p (source position)
  "Checks whether the POSITION in the SOURCE refers to a decimal digit,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (and
        (array-in-bounds-p source position)
        (digit-char-p (char source position)))))))

;;; -------------------------------------------------------

(defun whitespace-at-p (source position)
  "Checks whether the character at the POSITION in the SOURCE represents
   a whitespace, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (and
      (array-in-bounds-p source position)
      (whitespace-character-p (char source position)))))

;;; -------------------------------------------------------

(defun expect-digit (source position)
  "Determines whether the character at the POSITION in the SOURCE
   represents a decimal digit, on confirmation returning no value,
   otherwise signaling an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum position))
  (unless (digit-at-p source position)
    (error "Expected a decimal digit at position ~d, ~
            but encountered ~s."
      position
      (character-at source position)))
  (values))

;;; -------------------------------------------------------

(defun read-location (source start)
  "Proceeding from the START position in the SOURCE, reads a sequence of
   zero or more adjacent decimal digits, and returns two values:
     (1) the consumed digits as a non-negative integer number
     (2) the position in the SOURCE immediately succeeding the last
         consumed digit character."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position start))
    (declare (type fixnum position))
    (expect-digit source position)
    (the (values location fixnum)
      (values
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (digit-at-p source position) do
              (write-char (char source position) digits)
              (incf position))))
        position))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position in the SOURCE, skips a sequence of
   zero or more adjacent whitespaces and returns the location of the
   first non-whitespace character in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (loop
    for     position of-type fixnum from start
    while   (whitespace-at-p source position)
    finally (return position)))

;;; -------------------------------------------------------

(defun parse-memory (source)
  "Extracts the byte values from the SOURCE and returns a new ``Memory''
   object encapsulating the extricated items."
  (declare (type string source))
  (let ((locations NIL)
        (position  0))
    (declare (type (list-of location) locations))
    (declare (type fixnum             position))
    (flet
        ((extract-location (location new-position)
          "Inserts the LOCATION at the front of the locations list,
           updates the POSITION to the NEW-POSITION, and returns no
           value."
          (declare (type location location))
          (declare (type fixnum    new-position))
          (push location locations)
          (setf position new-position)
          (values))
         
         (source-exhausted-p ()
          "Determines whether the SOURCE is exhausted, which means that
           the POSITION cursor is located outside of the SOURCE's
           bounds, returning on confirmation a ``boolean'' of ``T'',
           otherwise ``NIL''."
          (setf position (skip-whitespaces source position))
          (the boolean
            (eof-p source position))))
      
      (loop until (source-exhausted-p) do
        (multiple-value-call #'extract-location
          (read-location source position))))
    
    (the Memory
      (build-memory
        (nreverse locations)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nand-bit (first-bit second-bit)
  "Returns the bit resulting from the NAND combination of the FIRST-BIT
   and the SECOND-BIT."
  (declare (type bit first-bit))
  (declare (type bit second-bit))
  (the bit
    (if (= first-bit second-bit 1)
      0
      1)))

;;; -------------------------------------------------------

(defun nand-bytes (first-byte second-byte)
  "Returns the byte resulting from the NAND combination of the
   FIRST-BYTE and the SECOND-BYTE."
  (declare (type octet first-byte))
  (declare (type octet second-byte))
  (let ((result #b00000000))
    (declare (type octet result))
    (dotimes (bit-position 8)
      (declare (type (integer 0 8) bit-position))
      (setf (ldb (byte 1 bit-position) result)
        (nand-bit
          (ldb (byte 1 bit-position) first-byte)
          (ldb (byte 1 bit-position) second-byte))))
    (the octet result)))

;;; -------------------------------------------------------

(defun ascertain-byte-value (candidate)
  "Determines whether the CANDIDATE represents an unsigned byte value in
   the range [0, 255], on confirmation returning the CANDIDATE,
   otherwise signaling an error of an unspecified type."
  (declare (type integer candidate))
  (the octet
    (if (typep candidate 'octet)
      candidate
      (error "No unsigned byte value: ~d." candidate))))

;;; -------------------------------------------------------

(defun parse-byte (source)
  "Parses the SOURCE as an unsigned byte value in decimal form and
   returns the thus produced octet.
   ---
   If the SOURCE violates the expected unsigned decimal format, an error
   of an unspecified type is signaled."
  (declare (type string source))
  (the octet
    (ascertain-byte-value
      (parse-integer source))))

;;; -------------------------------------------------------

(defun query-for-byte (&optional (message "~&>> "))
  "Queries the user for an unsigned byte input, optionally issuing to
   the standard output a MESSAGE in lieu of the default prompt text, and
   returns the parsed octet value.
   ---
   An input which cannot be construed as an integer in the range
   [0, 255] will instigate an error of an unspecified type.
   ---
   The MESSAGE constitutes a control string, compatible with the
   ``format'' function."
  (declare (type string message))
  (format T message)
  (the octet
    (prog1
      (parse-byte (read-line))
      (clear-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (memory)))
  "The ``Interpreter'' class encapsulates the context and state of an
   nndstojmp program's execution, embracing the memory and instruction
   sequence, among other pieces of data."
  (memory       (error "Missing memory.") :type Memory)
  ;; Determines whether the program has halted, either by mediation of
  ;; the parameter "d", or by exhaustion of the INSTRUCTIONS sequence.
  (halted-p     NIL                       :type boolean)
  ;; The instruction pointer (IP), responsible for designating the
  ;; currently active operation in the INSTRUCTIONS sequence.
  (ip           1                         :type fixnum)
  ;; The most recently generated byte, inchoately produced by the
  ;; parameter "a", then modified through a NAND-combination by the
  ;; parameter "b", and finally consumed by the "c" parameter to either
  ;; be stored in the memory or printed to the standard output.
  (current-byte 0                         :type octet))

;;; -------------------------------------------------------

(defun interpreter-cell-at (interpreter location)
  "Returns the value of the INTERPRETER's memory cell at the LOCATION."
  (declare (type Interpreter interpreter))
  (declare (type integer     location))
  (the octet
    (memory-cell-at (interpreter-memory interpreter) location)))

;;; -------------------------------------------------------

(defun (setf interpreter-cell-at) (new-value interpreter location)
  "Stores the NEW-VALUE into the INTERPRETER's memory cell at the
   LOCATION and returns no value."
  (declare (type octet       new-value))
  (declare (type Interpreter interpreter))
  (declare (type integer     location))
  (setf (memory-cell-at (interpreter-memory interpreter) location)
        new-value)
  (values))

;;; -------------------------------------------------------

(defun interpreter-current-instruction (interpreter)
  "Returns the instruction corresponding to the INTERPRETER's current
   instruction pointer (IP) location, or ``NIL'' if the same violates
   the instruction sequence's boundaries."
  (declare (type Interpreter interpreter))
  (the Instruction
    (memory-extract-instruction
      (interpreter-memory interpreter)
      (interpreter-ip     interpreter))))

;;; -------------------------------------------------------

(defun interpreter-jump-to (interpreter new-position)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   NEW-POSITION and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-position))
  (setf (interpreter-ip interpreter) new-position)
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-parameter-a (interpreter instruction)
  "Processes the INSTRUCTION's \"a\" parameter in the INTERPRETER's
   context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (setf (interpreter-current-byte interpreter)
    (if (instruction-requests-input-for-a-p instruction)
      (query-for-byte
        "~&Please input an unsigned byte for parameter \"a\": ")
      (interpreter-cell-at interpreter
        (instruction-a instruction))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-parameter-b (interpreter instruction)
  "Processes the INSTRUCTION's \"b\" parameter in the INTERPRETER's
   context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (setf (interpreter-current-byte interpreter)
    (nand-bytes
      (interpreter-current-byte interpreter)
      (if (instruction-requests-input-for-b-p instruction)
        (query-for-byte
          "~&Please input an unsigned byte for parameter \"b\": ")
        (interpreter-cell-at interpreter
          (instruction-b instruction)))))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-parameter-c (interpreter instruction)
  "Processes the INSTRUCTION's \"c\" parameter in the INTERPRETER's
   context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (if (instruction-commits-output-p instruction)
    (format T "~&~d"
      (interpreter-current-byte interpreter))
    (setf (interpreter-cell-at interpreter (instruction-c instruction))
      (interpreter-current-byte interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-parameter-d (interpreter instruction)
  "Processes the INSTRUCTION's \"d\" parameter in the INTERPRETER's
   context and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  (if (instruction-terminates-program-p instruction)
    (setf (interpreter-halted-p interpreter) T)
    (interpreter-jump-to interpreter
      (instruction-d instruction)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-current-instruction (interpreter)
  "Processes the INTERPRETER's current instruction and returns no
   value."
  (declare (type Interpreter interpreter))
  (let ((current-instruction
          (interpreter-current-instruction interpreter)))
    (declare (type Instruction current-instruction))
    (interpreter-process-parameter-a interpreter current-instruction)
    (interpreter-process-parameter-b interpreter current-instruction)
    (interpreter-process-parameter-c interpreter current-instruction)
    (interpreter-process-parameter-d interpreter current-instruction))
  (values))

;;; -------------------------------------------------------

(defun interpreter-process-instructions (interpreter)
  "Processes the instruction sequence maintained by the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-halted-p interpreter) do
    (interpreter-process-current-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-nndstojmp (code)
  "Interprets the piece of nndstojmp source CODE and returns no value."
  (declare (type string code))
  (interpreter-process-instructions
    (make-interpreter
      (parse-memory code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-data-offset (text)
  "Calculates and returns for the TEXT the location in an nndstojmp
   program's memory at which the data segment shall start."
  (declare (type string text))
  (the (integer 0 *)
    (1+ (* (length text) 4))))

;;; -------------------------------------------------------

(defun build-location-table (data-offset text)
  "Calculates for the TEXT, intended to start at the memory location
   DATA-OFFSET, the location table, and returns two values:
     (1) the location table, a hash table mapping each distinct TEXT
         character to the memory location in the order of its occurrence
         in the TEXT, with respect to the DATA-OFFSET
     (2) a string comprehending the location table keys in the order of
         their occurrence in the TEXT."
  (declare (type fixnum data-offset))
  (declare (type string text))
  (let ((data-table         (make-hash-table :test #'eql))
        (next-free-location data-offset))
    (declare (type location-table data-table))
    (declare (type location       next-free-location))
    (with-open-stream (keys (make-string-output-stream))
      (declare (type string-stream keys))
      (loop for character of-type character across text do
        (let ((contains-character-p
                (nth-value 1
                  (gethash character data-table))))
          (declare (type T contains-character-p))
          (unless contains-character-p
            (setf (gethash character data-table) next-free-location)
            (write-char character keys)
            (incf next-free-location))))
      (the (values location-table string)
        (values data-table
          (get-output-stream-string keys))))))

;;; -------------------------------------------------------

(defun generate-text-program (text &key (destination NIL))
  "Generates an nndstojmp program capable of printing the the TEXT's
   ASCII codes to the standard output and writes the thus produced
   source code to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string comprehending the result."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((number-of-characters (length text))
            (data-offset          (calculate-data-offset text)))
        (multiple-value-bind (location-table keys)
            (build-location-table (1+ data-offset) text)
          (declare (type fixnum         number-of-characters))
          (declare (type (integer 0 *)  data-offset))
          (declare (type location-table location-table))
          (declare (type string         keys))
          
          (flet
              ((print-instruction-row (character character-index)
                "Prints an instruction row, composed of the four
                 requisite parameters, capacitated to print the
                 CHARACTER, constituting the CHARACTER-INDEX-th element
                 of the TEXT, to the DESTINATION, and returns no value.
                 ---
                 The parameters comprehend the following:
                 (a) The location of the constant NAND parameter 255.
                 (b) The location of the variable NAND operand
                     corresponding to the CHARACTER's ASCII code.
                 (c) The output sentinel 0.
                 (d) Either the location of the next print row, or the
                     sentinel 0 to terminate the program in the case of
                     the last TEXT character."
                (declare (type character character))
                (declare (type fixnum    character-index))
                (format destination "~&~d ~d 0 ~d"
                  data-offset
                  (gethash character location-table)
                  (if (= character-index number-of-characters)
                    0
                    (prog1 (1+ (* character-index 4))
                      (incf character-index))))
                (values))
               
               (print-data-segment ()
                "Prints the data segment to the DESTINATION.
                 ---
                 This segment starts with the constant NAND parameter
                 255, followed by the unique bytes that, when
                 NAND-combined with the constant 255, yields the ASCII
                 code of a TEXT character."
                (format destination "~&255")
                (loop for character of-type character across keys do
                  (format destination " ~d"
                    (nand-bytes 255 (char-code character))))
                (values)))
            
            (loop
              for     character of-type character across text
              for     index     of-type fixnum    from   1
              do      (print-instruction-row character index)
              finally (print-data-segment)))))
        
        (with-output-to-string (output)
          (declare (type string-stream output))
          (generate-text-program text :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query for two bytes, NAND-combine them, print the result, and
;; terminate the program.
(interpret-nndstojmp "0 0 0 0")

;;; -------------------------------------------------------

;; Repeatedly query for two bytes, NAND-combine them, and print the
;; result, ere restarting the process.
(interpret-nndstojmp "0 0 0 1")

;;; -------------------------------------------------------

;; Print the ASCII character codes corresponding to the message
;; "Hello, World!".
(interpret-nndstojmp
  "
  53 54 0 5
  53 55 0 9
  53 56 0 13
  53 56 0 17
  53 57 0 21
  53 58 0 25
  53 59 0 29
  53 60 0 33
  53 57 0 37
  53 61 0 41
  53 56 0 45
  53 62 0 49
  53 63 0 0
  255 183 154 147 144 211 223 168 141 155 222
  ")

;;; -------------------------------------------------------

;; Generate an nndstojmp capacitated with the printing the ASCII
;; character codes for the message "Hello, World!", and print the same
;; to the standard output.
;; 
;; The thus produced code resolves to:
;; 
;;   53 54 0 5
;;   53 55 0 9
;;   53 56 0 13
;;   53 56 0 17
;;   53 57 0 21
;;   53 58 0 25
;;   53 59 0 29
;;   53 60 0 33
;;   53 57 0 37
;;   53 61 0 41
;;   53 56 0 45
;;   53 62 0 49
;;   53 63 0 0
;;   255 183 154 147 144 211 223 168 141 155 222
(generate-text-program "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Generate an nndstojmp capacitated with the printing the ASCII
;; character codes for the message "Hello, World!", and interpret the
;; same.
(interpret-nndstojmp
  (generate-text-program "Hello, World!"))
