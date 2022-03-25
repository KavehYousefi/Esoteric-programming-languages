;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a compiler and an interpreter for the
;; esoteric programming language "LayerASM", designed by the Esolang
;; user "Ashtons", and intended to permit operations on the
;; conspicuously low level of machine code.
;; 
;; Concepts
;; ========
;; The "LayerASM" programming language establishes a commorant of an
;; utterly low tier, involved in the realization on the level of bits
;; and bytes. Elements are provided to furnish a vinculum to an assembly
;; language whose exact delineations escaped the original
;; specification's circumference, but germinate by a concoction of their
;; own in this document. The language maintains its data in four layers,
;; each a 16x16 cells grid of a particular data type, into the same a
;; pointer acts as a selector of its active cell, while at any time a
;; single member of the quartet may be chosen as the active layer.
;; 
;; == TERMINOLOGY ==
;; Indulgence in the rather complicated realm of low-level programming,
;; as appertains to the haecceity of the LayerASM language, mandates at
;; least a modicum of comprehension regarding this ambit's foundry. The
;; following sections are ordained to the onus of supplying the absolute
;; minimum of the trenchant gnarity. For more sophisticated exposure,
;; please consult the sources located in this documentation's desinence.
;; 
;; Machine code describes a programming language located at a low tier,
;; including in its compass bytecode as well as assembly languages, and
;; being itself a composite of fine-grained instructions. Its ramosity
;; manifests especially in two expressions: the mostly numeric bytecode
;; and the more human-friendly assembly.
;; 
;; Bytecode represents the most primitive variant of a machine language,
;; in that its content is restricted to a series of numeric literals and
;; predefined constants, intended to render upon a machine in particular
;; efficient manner, with little regard to the programmer's comfort.
;; 
;; Assembly language issues as the stark opposite of bytecode from the
;; machine language's furcation. Its target being to a high mete
;; amenities vouchsafed to the programmer, and secondarily only
;; efficiency in execution, symbols and mnemonics apply themselves to
;; the stead or coefficiency of instructions' and operands' numeric
;; codes.
;; 
;; In the context of LayerASM, a bytecode instruction targeted at
;; prompting the user for an input would assume the direct octet value
;;   00101100
;; whereas its assembly language variant corresponds to the more lucid
;; and expressive, yet, because of a further compilation step's
;; necessitation to accomplish the aforementioned form, less efficient
;;   do i
;; 
;; == LAYERASM IS IN ITS ULTIMATE FORM BYTECODE ==
;; LayerASM's basic tenet comprises a resolution to bytecode, that is,
;; a sequence of zero or more bytes carrying instructions and their
;; operands. A particular format serves as a discriminating criterion
;; in the apperception and processing of operations.
;; 
;; == LAYERASM ACCEPTS ASSEMBLY ==
;; Commorant in a higher stratum than the bytecode, a rudimentary
;; assembler language is superimposed upon the bit level, its
;; consumption ultimately justified by the transcription into the lower
;; tier.
;; 
;; == THE MEMORY EMBRACES FOUR GRIDS ==
;; A kenspeckle aspect of the language, the architecture is defined
;; rather meticulously to appear as a layer of four congruent grids,
;; each tallying 16 columns and 16 rows, but distinguished by the
;; constraint in the compatible data through the regulations of their
;; respective layer: The first level contains 8-bit ASCII character,
;; the second 8-bit signed integers, the third extending the previous
;; data type to 16 bits, while the fourth and last admits 32-bit
;; floating-point numbers.
;; 
;; At each instant in a program one layer constitutes the active entity,
;; and a cursor, the data cell pointer, designates its active cell, in
;; regard to which all operations are construed.
;; 
;; 
;; Architecture
;; ============
;; LayerASM arranges its memory in four layers composed of 16x16 cells
;; each, with every tier identified by a particular data type. A data
;; cell pointer per stratum designates the currently selected cell. At
;; any time exactly one layer bears the designation as the active unit.
;; 
;; The environment apportioned a realistic venue, the memory submits to
;; exactness in specification and circumscription, involving a lucid
;; exposition of the available data types and their respective extension
;; in the memory.
;; 
;; == LAYERS ==
;; The LayerASM language accords a program a set of four congruent
;; layers, the distinguishing mark of which resides in the data type.
;; A layer is comprised of cells shaped in a Cartesian arrangement of
;; 16 columns and 16 rows, tallying a total of 256 units per such grid,
;; with any cell adhering in its datum to the data type of its layer.
;; The four layers enumerate in this form:
;;   
;;   Layer ID | Data type             | Bytes per cell | Size in bytes
;;   ---------+-----------------------+----------------+--------------
;;    0       | ASCII character       | 1              | 256
;;    1       | 8-bit signed integer  | 1              | 256
;;    2       | 16-bit signed integer | 2              | 512
;;    3       | 32-bit float          | 4              | 1024
;; 
;; A function of its byte allocation and purpose, a data type and its
;; range shall be produced in the next figure:
;;   
;;   Layer ID | Data type             | Minimum value  | Maximum value
;;   ---------+-----------------------+----------------+--------------
;;    0       | ASCII character       |  0             | +255
;;    1       | 8-bit signed integer  | -128           | +127
;;    2       | 16-bit signed integer | -32768         | +32767
;;    3       | 32-bit float          | -3.4028233E+38 | +3.4028233E+38
;; 
;; == LAYER 0: ASCII CHARACTERS ==
;; The first layer, located at the depth zero (0), accommodates its
;; 16x16 cells arrangement to one ASCII character per unit, entailing
;; the corollary of eight bits, or one byte, residing in a cell, and
;; spanning the range [0, 255].
;; 
;; == LAYER 1: 8-BIT SIGNED INTEGER ==
;; The second layer, identified by the subscript one (1), is assigned
;; eight bit (one byte) signed integers. This concept resembles Java's
;; primitive "byte" type, and might participate in the consanguinity by
;; the contingence of space and time efficiency in storage and
;; operation. The range [-128, +127] is occupied.
;; 
;; == LAYER 2: 16-BIT SIGNED INTEGER ==
;; The third layer, indexed with two (2), provides the larger of the
;; integer twain, proffering with its 16 bits (2 bytes) a signed
;; structure similar to the C and Java programming languages' "short"
;; types. Its marches similarly resolve to [-32768, +32767].
;; 
;; == LAYER 3: 32-BIT FLOAT ==
;; The desinent layer, located at a depth of three (3), imparts to the
;; programmer the sole instance invading into the floating-point realm.
;; Each cell maintains one 32-bit (4 bytes) single-precision float
;; value, conforming to the respective IEEE-754 standard for such
;; objects. The occupied interval results from the same specification
;; when partaking of the symmetric designation
;; [-3.4028233E+38, +3.4028233E+38].
;; 
;; == DATA CELL POINTER ==
;; Every data layer maintains a single, independent marker designating
;; at any instant its selected cell: the data cell pointer. Initialized
;; at the left-upper corner, this cursor is amenable to stepwise
;; navigation.
;; 
;; 
;; Data Types
;; ==========
;; LayerASM, maugre its low-level appearance, employs a rather luxurious
;; amalgam of data types utible for computations, the compass being only
;; exhausted by ASCII characters, 8-bit and 16-bit integers, as well as
;; a 32-bit floating-point type. Interactions betwixt these realms are,
;; by distribution across disjunct layers, eloigned from the language's
;; capacity.
;; 
;; == CHARACTER ==
;; The language dedicates the incipient layer to the castaldy of an
;; ASCII character grid. A consectary of its standardized repertoire,
;; characters are encoded by 256 different values, enumerated within the
;; contingency of a single byte, occupying the integer range [0, 255].
;; 
;; == 8-BIT SIGNED INTEGER ==
;; A very restricted variation of the numeric type, and representable by
;; one octet, the 8-bit signed integer covers the range [-128, +127].
;; The value is agnominated in a euonymous manner as "byte" in the Java
;; programming language.
;; 
;; == 16-BIT SIGNED INTEGER ==
;; A more generous numeric specimen, two bytes comprehend the 16-bit
;; signed integer type, with a range extending inside [-32768, +32767].
;; In the C and Java programming languages this type is referred to as
;; "short".
;; 
;; == 32-BIT FLOAT ==
;; The only non-integer numeric type, the 32-bit single-precision
;; floating-point is a construction of two four bytes. A manifestation
;; of the IEEE-754 specification for the eponymous category, its
;; symmetric range spans [-3.4028233E+38, +3.4028233E+38].
;; 
;; == LAYER ==
;; A layer describes an internally connected 16x16 cell network, its
;; layout a Cartesian grid. While no imposition of random access finds
;; its expression, the requirement holds that, given a currently
;; selected cell, its immediate neighbor in any of the cardinal
;; directions, left, right, top, and bottom, must be retrievable. The
;; cell itself adheres in its respondency to the entailing layer's data
;; type, storing a scalar datum issuing from the same prescription.
;; 
;; The initial values correlate in the following fashion with the layer
;; data types:
;;   
;;   Layer ID | Data type      | Default
;;   ---------+----------------+------------------------------------
;;    0       | Character      | '\0' (the NULL character, code = 0)
;;    1       | 8-bit integer  | 0
;;    2       | 16-bit integer | 0
;;    3       | 32-bit float   | 0.0
;; 
;; 
;; Syntax
;; ======
;; With its two manifestations as (a) bytecode and (b) assembly,
;; LayerASM's syntax bifurcates into a twofold capacitation. The tight
;; consanguinity may yet avail as a recommendation to reproduce the
;; nexus in the forthcoming treatise.
;; 
;; == BYTECODE ==
;; LayerASM subscribes to the byte order philosophy of big endian.
;; 
;; == BYTCODE AND ASSEMBLY: CONSTANTS ==
;; Its dispensation along the various topics tangent to its utility
;; justifies a compact summary of the mnemonics, or symbolic constants,
;; representive of commonly encountered bit patterns, and preserved in
;; the below figure:
;;   
;;   Mnemonic | Full name    | Role        | Bit pattern
;;   ---------+--------------+-------------+------------
;;    cl      | change layer | instruction | 11001001
;;    do      | do           | instruction | 00101000
;;    jp      | jump         | instruction | 10001000
;;    mv      | move         | instruction | 01000010
;;   ...................................................
;;    db      | double       | argument    | 010
;;    de      | decrement    | argument    | 001
;;    hl      | halve        | argument    | 011
;;    i       | input        | argument    | 100
;;    in      | increment    | argument    | 000
;;    o       | output       | argument    | 101
;;   ...................................................
;;    dn      | down         | argument    | 01
;;    lf      | left         | argument    | 11
;;    rt      | right        | argument    | 10
;;    up      | up           | argument    | 00
;;   ...................................................
;;    a       | always       | argument    | 000
;;    gt      | greater than | argument    | 100
;;    lt      | less than    | argument    | 011
;;    nz      | not zero     | argument    | 010
;;    z       | zero         | argument    | 001
;; 
;; == ASSEMBLY: INSTRUCTIONS ==
;; An instruction is delineated by a composite of an operation
;; identifier and zero or more operands, acting as arguments to its
;; duty's fulfilment. The instruction name must be provided in the form
;; of its mnemonic, separated by one or more spaces from its operands,
;; which may be either specified as symbolic constants or alternatively
;; as literal numbers of any apropos form. Each two operands themselves
;; are separated by exactly one comma, optionally ensconced by zero or
;; more spaces. Other content in the interstices, including newlines,
;; infringes upon the syntax' tolerance.
;; 
;; Each instruction occupies a line of its own; in order to divide a
;; program into a sequence of such, at least one newline must apply
;; itself to the intermittent region. A more generous tally of newlines
;; is permitted, including as a prelude to the first operation as well
;; as a conclusion to the final.
;; 
;; == ASSEMBLY: COMMENTS ==
;; Comments are introduced via a single semicolon ";", extending to, but
;; not absorbing, the end of the line. Any character, regardless of its
;; conformance to the ASCII or any other convention, is homologated
;; occurrence.
;; 
;; == ASSEMBLY: SEPARATORS ==
;; Spaces, which entail in their rank the simple space character " " as
;; well as the horizontal tab "\t", are encountered with tolerance in
;; all places around tokens, and mandated as divisions for an
;; instruction and its operand. The division betwixt operands follows by
;; mediation of the comma "," symbol.
;; 
;; Newlines, manifesting in the "\n" character or any equivalent entity
;; or combination of entities in such agency as depending upon the
;; respective execution platform, are tolerated at any place, and
;; mandated as divisions for instructions among themselves, as well as
;; terminators for comments.
;; 
;; == ASSEMBLY: NUMBER LITERALS ==
;; The only literals anticipated in a piece of assembly code is
;; established in signed integers. These objects may be stated in a
;; variety of ways, the expression being not only a notational facility
;; but also founded upon the contingencies of number bases. Four bases
;; are recognized:
;;   
;;   - binary      (base-2)
;;   - octal       (base-8)
;;   - decimal     (base-10)
;;   - hexadecimal (base-16)
;; 
;; The predicament applying to the manifold variegations's discernment
;; imposes the bailiwick of a particular account of syntactical
;; diorisms; concretely, three formats of literals are distinguished:
;;   
;;   Format        | Description                             | Examples
;;   --------------+-----------------------------------------+---------
;;    Direct form  | An optional sign ("+" or "-")  followed | 15
;;                 | by a sequence of one or more decimal    | +15
;;                 | digits.                                 | -15
;;   ..................................................................
;;    Decimal form | The dollar symbol "$" followed by an    | $15
;;                 | optional sign ("+" or "-") and          | $+15
;;                 | succeeded by a sequence  of one or more | $-15
;;                 | decimal digits.                         | $0
;;   ..................................................................
;;    Radix form   | The dollar symbol "$" followed by a     | $b-1010
;;                 | radix code, succeeded by an optional    | $d+55
;;                 | sign ("+" or "-"), and terminated by a  | $o07
;;                 | sequence of one or more digits in       | $x+AH
;;                 | concord with the signified radix.       | $x9FB21
;; 
;; Each number base (radix) dictates a certain set of admittable digits.
;; A listing of the available radix codes and the digits defined in
;; their context shall be the coming table's responsibility:
;;   
;;   Radix code | Radix | Name        | Native digit | Decimal value
;;   -----------+-------+-------------+--------------+--------------
;;    b         | 2     | binary      | 0            | 0
;;              |       |             | 1            | 1
;;   -----------+-------+-------------+--------------+--------------
;;    d         | 10    | decimal     | 0            | 0
;;              |       |             | 1            | 1
;;              |       |             | 2            | 2
;;              |       |             | 3            | 3
;;              |       |             | 4            | 4
;;              |       |             | 5            | 5
;;              |       |             | 6            | 6
;;              |       |             | 7            | 7
;;              |       |             | 8            | 8
;;              |       |             | 9            | 9
;;   ...............................................................
;;    o         | 8     | octal       | 0            | 0
;;              |       |             | 1            | 1
;;              |       |             | 2            | 2
;;              |       |             | 3            | 3
;;              |       |             | 4            | 4
;;              |       |             | 5            | 5
;;              |       |             | 6            | 6
;;              |       |             | 7            | 7
;;   ...............................................................
;;    x         | 16    | hexadecimal | 0            | 0
;;              |       |             | 1            | 1
;;              |       |             | 2            | 2
;;              |       |             | 3            | 3
;;              |       |             | 4            | 4
;;              |       |             | 5            | 5
;;              |       |             | 6            | 6
;;              |       |             | 7            | 7
;;              |       |             | 8            | 8
;;              |       |             | 9            | 9
;;              |       |             | A            | 10
;;              |       |             | B            | 11
;;              |       |             | C            | 12
;;              |       |             | D            | 13
;;              |       |             | E            | 14
;;              |       |             | F            | 15
;; 
;; The SHORT FORM, which attends exclusively to the decimal system
;; (base-10), and nemned the "direct form", simply expects one or more
;; decimal digits; leading zeroes are homologated. An optional sign,
;; either a "+" or "-", may precede the magnitude, restricted to a
;; single occurrence at most. This variant exploits the abstinence of
;; the LayerASM mnemonics from digits, particularly at the first
;; identifier position.
;; 
;; In pursuit of disambiguity regarding the definition of mnemonics and
;; literals permissive of letters, such as hexadecimal objects, a
;; general number is introduced by the dollar sign "$", a symbol
;; forensecal to the named constants appertaining to instructions and
;; arguments. An optional sign, either "+" or "-", and at most in one
;; instance attached to the datum, may avail in the determination or
;; emphasis of the number line's moeity occupied by the value. A
;; discrepancy now ensues for the sake of identifying the base or radix
;; of the declared literal, ramifying into two variations: the decimal
;; form and the radix form.
;; 
;; An intermediary design, the DECIMAL FORM, more expressive than
;; the aforementioned conception by the direct form, activated by means
;; of the "$" and optional sign combination, yet destitute of the base's
;; introduction, expects one or more decimal digit, again administering
;; tolerance to leading zeroes.
;; 
;; If the radix comprehends an objects of significance for one's
;; purposes, the RADIX FORM requires instantiation: Following the dollar
;; symbol "$" and an optional sign, the value must be preceded by one of
;; four "radix codes", single-character identifiers with the potential
;; of declaring the intended number system, and enumerated below:
;;   
;;   Radix code | System      | Radix | Example
;;   -----------+-------------+-------+--------------
;;    b         | binary      | 2     | $b1010101010
;;    d         | decimal     | 10    | $d0123456789
;;    o         | octal       | 8     | $o0123457801
;;    x         | hexadecimal | 16    | $x0123456AEF
;; 
;; Leading zeroes, in this final form, are encountered with leniency
;; anew.
;; 
;; In any mode of notation, no distinguishment is apportioned concerning
;; positive and negative zeroes.
;; 
;; == ASSEMBLY: FURTHER CHARACTERS ==
;; Any character not consigned to the identification of an instruction,
;; the diorism of operands, the introduction of a comment, or embraced
;; in the latter's bailiwick, and not part of the separator set, is
;; inflicted with prohibition and will provoke an error in the lexing or
;; parsing process.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) description applies to
;; the LayerASM syntax:
;;   
;;   instruction     := instructionName , [ argument , { "," , argument } ] , "\n" ;
;;   instructionName := "cl" | "do" | "jp" | "mv" ;
;;   argument        := integerLiteral
;;                   |  constant ;
;;   constant        := letter , { letter } ;
;;   integerLiteral  := directForm
;;                   |  decimalForm
;;                   |  radixForm ;
;;   directForm      := [ "+" | "-" ] , decimalNumber ;
;;   decimalForm     := "$" , [ "+" | "-" ] , decimalNumber ;
;;   radixForm       := "$" , radixCode , [ "+" | "-" ], number ;
;;   radixCode       := "b" | "d" | "o" | "x" ; 
;;   number          := digit , { digit } ;
;;   decimalNumber   := decimalDigit , { decimalDigit } ;
;;   digit           := decimalDigit
;;                   |  letter ;
;;   letter          := "a" | ... | "z" | "A" | ... | "Z" ;
;;   decimalDigit    := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; The LayerASM instruction set is exhausted by a tally of four members,
;; each represented by a particular composition of bits in the first
;; byte, carrying simultaneously the operation identity and either all
;; or a portion of the operands.
;; 
;; The coming apercu concerns the four instructions, their byte count,
;; and their effect:
;;   
;;   Instruction | No. bytes | Effect
;;   ------------+-----------+-----------------------------------------
;;    cl         | 1         | Changes the layer.
;;   ..................................................................
;;    do         | 1         | Performs a unary operation on the
;;               |           | current cell.
;;   ..................................................................
;;    jp         | 2         | Jumps forward/backward in the code
;;               |           | if a condition involving the current
;;               |           | cell and a cell in a specified direction
;;               |           | is satisfied.
;;   ..................................................................
;;    mv         | 1         | Moves the data cell pointer into the
;;               |           | given direction.
;; 
;; The first octet always includes in its two most significant bits
;; (MSBs) a pattern utible in discriminating any of the four
;; instructions; hence, by indagating the pair as a sentinel, one might
;; determine the type. Gnarity of this proves especially significant if
;; the program's bytecode is minimal in that only the jump command "jp"
;; appropriates two bytes, while all other three instructions subsist on
;; a single octet. If the indagated sentinel bits match a jump
;; instruction, a second byte must be consumed, otherwise the single
;; extant byte suffices for a processing.
;;   
;;   Instruction | Sentinel bits (the two most significant bits)
;;   ------------+----------------------------------------------
;;    do         | 00xxxxx
;;    mv         | 01xxxxx
;;    jp         | 10xxxxx
;;    cl         | 11xxxxx
;; 
;; The subsequent six bits contain one or more requisite arguments. If
;; a second octet is necessitated to complete the instruction --- as is
;; contemporarily solely the case with "jp" ---, its information
;; restricts to this purpose only. A correlation betwixt the
;; instructions, their bit pattern, and argument count is adduced below:
;;   
;;   Instruction | Binary representation | Number of arguments
;;   ------------+-----------------------+--------------------
;;    cl         | 11xx1001              | 1
;;    do         | 00101ppp              | 1
;;    jp         | 10dd1ccc bbbbbbbb     | 3
;;    mv         | 01dd0010              | 1
;; 
;; The following tabular exposition is dedicated to a more detailed
;; treatise of the arguments as embedded structures in the bits:
;;   
;;   Instruction | Bit pattern       | Effect
;;   ------------+-------------------+---------------------------------
;;    cl         | 11yy1001          | Changes to the layer with the
;;               |                   | ID {yy}.
;;               |                   |---------------------------------
;;               |                   | {yy} may assume:
;;               |                   |   00 = layer 0
;;               |                   |   01 = layer 1
;;               |                   |   10 = layer 2
;;               |                   |   11 = layer 3
;;   ..................................................................
;;    do         | 00101ppp          | Performs the unary operation
;;               |                   | {ppp} on the current cell.
;;               |                   |---------------------------------
;;               |                   | {ppp} may assume:
;;               |                   |   000 = increment by one
;;               |                   |   001 = decrement by one
;;               |                   |   010 = double
;;               |                   |   011 = halve
;;               |                   |   100 = input
;;               |                   |   101 = output
;;   ..................................................................
;;    jp         | 10dd1ccc bbbbbbbb | Compares the current cell value
;;               |                   | with the cell located in the
;;               |                   | direction {dd} using the
;;               |                   | condition {ccc}, and, if this
;;               |                   | condition is satisfied, moves
;;               |                   | the instruction pointer
;;               |                   | {bbbbbbbb} entries relative to
;;               |                   | the instruction pointer position.
;;               |                   |---------------------------------
;;               |                   | {dd} may assume:
;;               |                   |   00 = up
;;               |                   |   01 = down
;;               |                   |   10 = right
;;               |                   |   11 = left
;;               |                   |---------------------------------
;;               |                   | {ccc} may assume:
;;               |                   |   000 = always jump
;;               |                   |   001 = jump if equal or zero
;;               |                   |   010 = jump if not equal or
;;               |                   |         not zero
;;               |                   |   011 = jump if less than
;;               |                   |   100 = jump if greater than
;;               |                   |---------------------------------
;;               |                   | {bbbbbbbb} is interpreted as the
;;               |                   | an 8-bit signed integer encoded
;;               |                   | in two's complement.
;;   ..................................................................
;;    mv         | 01dd0010          | Moves the data cell pointer in
;;               |                   | the direction {dd}.
;;               |                   |---------------------------------
;;               |                   | {dd} may assume:
;;               |                   |   00 = up
;;               |                   |   01 = down
;;               |                   |   10 = right
;;               |                   |   11 = left
;; 
;; == "CL" : CHANGE LAYER ==
;; Changes the active layer, consequentially also updating the active
;; cell to the new tier's.
;; 
;; Bytecode signature:
;;   11yy1001
;; 
;; Assembly signature:
;;   cl {yy}
;; 
;; Interface:
;;   cl (yy : integer in [0, 3])
;; 
;; Arguments:
;;   
;;   Argument | Description
;;   ---------+--------------------------------------------------------
;;    yy      | A two-bit unsigned integer in the range [0, 3] which
;;            | determines the ID of the layer to set as the active
;;            | one.
;;   
;;   The layer specifier {yy} may assume one of these four values:
;;     
;;     Bytecode | Description
;;     ---------+-------------------------------------
;;      00      | Set the layer 0 as the active layer.
;;      01      | Set the layer 1 as the active layer.
;;      10      | Set the layer 2 as the active layer.
;;      11      | Set the layer 3 as the active layer.
;; 
;; Description:
;;   Changes the active layer to that designated by the layer ID {yy}.
;; 
;; Side effects:
;;   - The active layer will be changed.
;;   - If the new active layer differs from the current one, the active
;;     cell will be set to the active cell of new active layer.
;; 
;; Exceptional situations:
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     layer ID {yy} assumes an invalid value.
;; 
;; == "DO": PERFORM UNARY OPERATION ==
;; Performs a unary operation on the active cell, changing its value.
;; 
;; Bytecode signature:
;;   00101ppp
;; 
;; Assembly signature:
;;   do {ppp}
;; 
;; Interface:
;;   do (ppp : integer in [0, 5])
;; 
;; Arguments:
;;   
;;   Argument | Description
;;   ---------+--------------------------------------------------------
;;    ppp     | A three-bit code of a unary operation to perform on the
;;            | active cell.
;;   
;;   The operation specifier {ppp} may assume one of these four values:
;;     
;;     Bytecode | Mnemonic | Description
;;     ---------+----------+----------------------------------------
;;      000     | in       | Increment the active cell value by one.
;;     .............................................................
;;      001     | de       | Decrement the active cell value by one.
;;     .............................................................
;;      010     | db       | Double the active cell value.
;;     .............................................................
;;      011     | hl       | Halve the active cell value.
;;     .............................................................
;;      100     | i        | Set the active cell value to the user
;;              |          | input.
;;     .............................................................
;;      101     | o        | Output to the active cell value to the
;;              |          | standard output.
;; 
;; Description:
;;   Applies the unary operation {ppp} on the active cell, updating its
;;   value to the resulting datum.
;;   
;;   Despite the lack of requirements imposed upon its concrete design,
;;   the input operation (code {ppp} = "100") should be preceded by an
;;   elucidating prompt message printed to the output conduit. In the
;;   optimal case, the admittance of the user input data type will be
;;   an equivalent of the active layer's type.
;; 
;; Side effects:
;;   - The value of the active cell will be modified.
;; 
;; Exceptional situations:
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     unary operation {ppp} assumes an invalid value.
;; 
;; == "JP": JUMP ==
;; Relocates the instruction pointer either unconditionally or depending
;; on the relationship betwixt the active cell and one of its neighbors.
;; 
;; Bytecode signature:
;;   10dd1ccc bbbbbbbb
;; 
;; Assembly signature:
;;   jp {dd} {ccc} {bbbbbbbb}
;; 
;; Interface:
;;   jp (dd       : integer in [0, 3],
;;       ccc      : integer in [0, 4],
;;       bbbbbbbb : integer in [-128, +127])
;; 
;; Arguments:
;;   
;;   Argument  | Description
;;   ----------+-------------------------------------------------------
;;    dd       | A two-bit direction code which specifies which
;;             | neighbor, measured from the active cell, shall be
;;             | compared to the active cell's value.
;;   ..................................................................
;;    ccc      | A three-bit condition code which specifies the
;;             | prerequisite under which a jump, that is, a relocation
;;             | of the instruction pointer, shall eventuate.
;;   ..................................................................
;;    bbbbbbbb | The displacement byte. It encodes a signed byte in the
;;             | range [-128, 127] by which the instruction pointer
;;             | shall be moved relative from its current position in
;;             | the instruction sequence.
;;   
;;   The direction specifier {dd} may assume one of these four values:
;;     
;;     Bytecode | Mnemonic | Direction
;;     ---------+----------+----------
;;      00      | up       | up
;;      01      | dn       | down
;;      10      | rt       | right
;;      11      | lf       | left
;;   
;;   The condition specifier {ccc} may assume one of these five values:
;;     
;;     Bytecode | Mnemonic | Effect
;;     ---------+----------+-------------------------------------------
;;      000     | a        | Always jumps.
;;     ................................................................
;;      001     | z        | Jumps if the active cell value equals
;;              |          | zero, or if the active cell value equals
;;              |          | the value of the neighbor cell value in
;;              |          | the direction {dd}.
;;     ................................................................
;;      010     | nz       | Jumps if the active cell value does not
;;              |          | equal zero, or if the active cell value
;;              |          | does not equal the neighbor cell value in
;;              |          | the direction {dd}.
;;     ................................................................
;;      011     | lt       | Jumps if the active cell value is less
;;              |          | than the neighbor cell value in the
;;              |          | direction {dd}.
;;     ................................................................
;;      100     | gt       | Jumps if the active cell value is greater
;;              |          | than the neighbor cell value in the
;;              |          | direction {dd}.
;; 
;; Description:
;;   Indagates the condition {ccc}, which either represents a
;;   relationship betwixt the active cell and its neighbor in the
;;   direction {dd}, or an unconditional expression. If the condition is
;;   satisfied, the instruction pointer moves relative from its current
;;   position in the instruction sequence by the value of the
;;   displacement byte {bbbbbbbb}, construed as a signed integer in the
;;   range [-128, +127]. Otherwise the instruction pointer advances in
;;   the usual manner.
;; 
;; Side effects:
;;   - The instruction pointer may be relocated relative to its current
;;     position in the instruction sequence.
;; 
;; Exceptional situations:
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     condition {dd} assumes an invalid value.
;;   - An error of the type "CellPointerOutOfBoundsError" is thrown if
;;     the direction {dd} refers to a neighbor cell outside of the
;;     active layer's boundaries.
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     condition {ccc} assumes an invalid value.
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     displacement byte {bbbbbbbb} assumes an invalid value.
;;   - An error of the type "InstructionPointerOutOfBoundsError" is
;;     thrown if the displacement byte {bbbbbbbb} attempts to relocate
;;     the instruction pointer to a position violating the bounds of the
;;     instruction sequence.
;; 
;; == "MV": MOVE DATA CELL POINTER ==
;; Moves the data cell pointer in the specified direction.
;; 
;; Bytecode signature:
;;   01dd0010
;; 
;; Assembly signature:
;;   mv {dd}
;; 
;; Interface:
;;   mv (dd : integer in [0, 3])
;; 
;; Arguments:
;;   
;;   Argument  | Description
;;   ----------+-------------------------------------------------------
;;    dd       | A two-bit direction code which specifies the direction
;;             | into which the data cell pointer of the active layer
;;             | shall be moved by one step.
;;   
;;   The direction specifier {dd} may assume one of these four values:
;;     
;;     Bytecode | Mnemonic | Direction
;;     ---------+----------+----------
;;      00      | up       | up
;;      01      | dn       | down
;;      10      | rt       | right
;;      11      | lf       | left
;; 
;; Description:
;;   Moves the active layer's data cell pointer one step in the
;;   direction specified by {dd}.
;; 
;; Side effects:
;;   - The data cell pointer will be relocated by one cell.
;; 
;; Exceptional situations:
;;   - An error of the type "InvalidOperandError" is thrown if the
;;     condition {dd} assumes an invalid value.
;;   - An error of the type "CellPointerOutOfBoundsError" is thrown if
;;     the direction {dd} attempts to relocate the data cell pointer to
;;     a position which violates the active layer's boundaries.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Commensurable upon the project's extensive compass and demanding
;; design, the inflicting porosity does not afford an object of
;; mazement. A subset shall however be consigned to disquisition.
;; 
;; == HOW DOES THE GRID RESPOND ON ITS BOUNDARIES? ==
;; Gnarity about the finitude inherent to the 16x16 cells grids involves
;; an inquest into the expected behavior upon collisions with their
;; edges --- concretely: Which kind of respondency does a grid exhibit
;; in the case of a pointer motion that infringes on a boundary? At
;; least three options are admitted some mete of tenable conclusion:
;;   
;;   (1) STRINGENCY: ERROR
;;       The attempted transgression is construed as an adit to
;;       contingent corruption and thus being replied to with an error.
;;   (2) LENIENCE: NEGLIGENCE
;;       The motation is administered tolerance, but not effectiveness:
;;       The pointer simply halts.
;;   (3) ENCOURAGEMENT: WRAPPING
;;       The motion experiences a wrapping around and advance of its
;;       region, akin to a "linebreak" on a terminal:
;;         (i.1)   LEFT MOTION IN THE TOP-LEFT CORNER
;;                 If the pointer is located at the leftmost cell of the
;;                 top row, and moves sinistrally, it is relocated to
;;                 the rightmost cell of the bottom row.
;;         (i.2)   LEFT MOTION IN THE CENTER OR BOTTOM
;;                 If the pointer is located at the leftmost cell of any
;;                 row not at the top, it is relocated to the rightmost
;;                 cell one row aboon it.
;;         (ii.1)  RIGHT MOTION IN THE BOTTOM-RIGHT CORNER
;;                 If the pointer is located at the rightmost cell of
;;                 the bottom row, and moves dextrally, it is relocated
;;                 to the leftmost cell of the top row.
;;         (ii.2)  RIGHT MOTION IN THE CENTER OR TOP
;;                 If the pointer is located at the rightmost cell of
;;                 any row except for that at the bottom, and moves
;;                 dextrally, it is relocated to the leftmost cell of
;;                 immediately alow it.
;;         (iii.1) UPWARD MOTION IN THE TOP-RIGHT CORNER
;;                 If the pointer is located at the rightmost cell of
;;                 the top row, and moves upward, it is relocated to the
;;                 leftmost cell of the bottom row.
;;         (iii.2) UPWARD MOTION IN THE CENTER OR LEFT COLUMN
;;                 If the pointer is located at any column except for
;;                 the rightmost one of the top row, and moves
;;                 upward, it is relocated to the next column of the
;;                 bottom row.
;;         (iv.1)  DOWNWARD MOTION IN THE BOTTOM-LEFT CORNER
;;                 If the pointer is located at the leftmost column of
;;                 the bottom row, and moves downward, it is relocated
;;                 to the rightmost cell of the top row.
;;         (iv.2)  DOWNWARD MOTION IN THE CENTER OR RIGHT COLUMN
;;                 If the pointer is located any column except for the
;;                 leftmost one of the bottom row, and moves downward,
;;                 it is relocated to the previous column of the top
;;                 row.
;; 
;; Accounting for the significance and ramifications inherent to the
;; behavioral specification, and origination from apperception in regard
;; to array-like data structures in general application, the first
;; alternative is assumed effectivity. It impacts the ecosystem with
;; some loss in comfort and usability, but accords more veridically with
;; the nature of a suitable empirema to impute stringency on the
;; marches.
;; 
;; == WHICH SYNTAX DOES APPLY TO THE ASSEMBLY FORM? ==
;; In counterdinstinction from the binary form --- laden with some mete
;; of liberty through its concrete representation, either as a single
;; stream of bits, an integer encoding the binary digits, or a string
;; of that equivalency ---, the higher-level assembly enjoys nevening,
;; yet no substantial specification apart from its mnemonics. No further
;; treatment targets this subject, failing specially to inspect the
;; following points:
;;   
;;   (a) Does significance inhere in whitespaces, including linebreaks?
;;   (b) How shall numeric literals be represented?
;; 
;; Concerning the first aspect (a), conventions preside over popular
;; assembly languages, like NASM, to apportion importance to spaces and
;; linebreaks, with the former intervening to separate commands and
;; arguments, and the latter applying themselves to the division of
;; commands. It shall thus be declaimed a conformance to this proven
;; concept, in that commands and their arguments shall enclose at lease
;; one space or tab, whereas arguments among each other require exactly
;; one comma and tolerate without prescription spaces. Further, each two
;; commands must be discriminated by at least one linebreak.
;; 
;; The topic (b) involved with the design of literal values remains
;; requisite of a short elucidation. LayerASM expects data in this form
;; solely in regard of signed integer values, with no stricture in
;; the occupied gamut. The furnishment of literals thus pursues the
;; question of integer objects. A variety in the representation exists
;; which only the marches of one's conception may immure. Please consult
;; to the disquisition of this manner the web source
;; [stackoverflow2012hexassembly].
;; 
;; LayerASM's requirements shall be iterum aligned with the features
;; afforded by existing produces, yet instantiated by a dioristic
;; aptitude. Integer literals may be stated in the following four bases:
;;   
;;   - binary      (base-2)
;;   - octal       (base-8)
;;   - decimal     (base-10)
;;   - hexadecimal (base-16)
;; 
;; Magnamity in the representation coerces syntactical deliberations,
;; which are expressed by the following grammar:
;;   
;;   integerLiteral := directForm
;;                  |  decimalForm
;;                  |  radixForm ;
;;   directForm     := [ "+" | "-" ] , decimalNumber ;
;;   decimalForm    := "$" , [ "+" | "-" ] , decimalNumber ;
;;   radixForm      := "$" , [ "+" | "-" ] , radixCode , number ;
;;   radixCode      := "b" | "d" | "o" | "x" ; 
;;   number         := digit , { digit } ;
;;   decimalNumber  := decimalDigit , { decimalDigit } ;
;;   digit          := decimalDigit
;;                  |  "a" | ... | "z" | "A" | ... | "Z" ;
;;   decimalDigit   := "0" | "1" | "2" | "3" | "4"
;;                  |  "5" | "6" | "7" | "8" | "9" ;
;; 
;; == WHENCE DOES THE JUMP DISPLACEMENT TALLY? ==
;; The "jp" instruction constitutes the surrogate for a traditional
;; label and goto conjunction, its nature deviating by an immediate
;; relocation of the instruction pointer relative to the locality of
;; its causatum. With reference to the twofold quantity of bytes
;; allocated to this instruction, the latter constituing the actual
;; vehicle of the motion information, an inquiry arises about the
;; concrete construe applicable to the displacement byte. Designating
;; the current instruction pointer position as i, the current byte
;; index, pertaining to the second byte of the operating "jump"
;; instruction at i, as (i+1), and the received displacement byte as d,
;; the following possibilities are open to conception:
;;   
;;   (a) DISPLACEMENT BY BYTES:
;;       The instruction pointer and byte index are relocated in a
;;       manner such that i becomes i + d. This interpretation liberates
;;       the code from the instruction compounds' coarser granularity
;;       by encroaching into separate bytes; however, with "jump"
;;       operations being composites, the consequence would be in
;;       such cases a corruption of a "jump" displacement byte in the
;;       forensecal agency of an instruction.
;;   (b) DISPLACEMENT BY INSTRUCTIONS:
;;       The instruction pointer i is itself relocated in a manner to
;;       yield i + d. This homologates an exclusive navigation across
;;       operations, not bytes, but does not encumber a program with the
;;       vitiations innate to byte portion exploits.
;; 
;; With respect to the ascertainment of sanity by the second conception
;; (b), its application has been adjuded the canonical solution.
;; 
;; 
;; Implementation
;; ==============
;; This LayerASM implementation partakes of no aspiration in the ambit
;; of patration, rather being a vehicle of simplistic pragmatism. It
;; incoporates the two autochthonous tiers involved in the conjoined
;; expression of this language, the bytecode and assembly aspects, the
;; latter moeity administered, commensurately with the innate
;; complexity, a more convoluted realization, while the lower level
;; constituent resides in a lighter department.
;; 
;; == THE ASSEMBLER AS A CONSTRUCTION OF LEXER AND PARSER ==
;; The assembler language constitutes a coefficiency of a lexer and a
;; parser, the former produces a stream of tokens for the latter's
;; assemblage. The parser's produce is realized in a sequence of
;; instructions as LayerASM command representatives.
;; 
;; == THE LEXER: A FACTOR OF TOKENS ==
;; The lexer analyzes a piece of LayerASM code, producing from its
;; significant portions tokens, an encapsulating unit compact of a
;; categorizating type and a specifying datum.
;; 
;; == THE PARSER COMBINES TOKENS INTO INSTRUCTIONS ==
;; The thus generated token stream is consigned to the custody of the
;; parser, the entity responsible for ascertaining the validity of the
;; program represented by the tokens' arrangement. Issuing from this
;; premise, these objects are assembled into a vector of instructions,
;; the arguments of the same born as operands.
;; 
;; == INSTRUCTIONS REPRESENT LAYERASM COMMANDS ==
;; An instruction, identified by a type designator as the vinculum to
;; the correlated command, maintains a list of zero or more operands,
;; the tally thereof a dependency upon the LayerASM command whose
;; delegation it is burdened to assume.
;; 
;; An operand either carries a literal decimal integer or a constant
;; name in the form of a string, the latter must subsequently be
;; resolved to its numeric value in the course of the interpretation
;; stage.
;; 
;; == THE COMPILATION STEP: INSTRUCTIONS BECOME BYTECODE ==
;; The descent into the lower-leved bytecode realm of LayerASM is
;; designed by a desinent compilation step, a conversion of the
;; instruction vector into a vector of bytes as the binary information
;; known as the bytecode.
;; 
;; Aspiring the accomplishment of this objective, each instruction is
;; in turn subjected to the indagation of its type and, in the context
;; of the same, the evaluation of its operands, producing per such input
;; one or two bytes for the ultimate binary data.
;; 
;; == BYTECODE IS PROCESSED BY AN INTERPRETER =
;; The onus of the bytecode consumption, and the resulting induction of
;; effect into the analyzed code, is achieved by the efforts of an
;; interpreter, maintaining the program state in conjunction with its
;; work on the bit and byte level.
;; 
;; The necessity of the jump instruction "jp" to navigate across the
;; instructions on their level as units in lieu of the more basic byte
;; tier justifies a dedicated ``Chunk'' structure's services to be
;; harnessed, coalescing one or two bytes into a single entity. Apart
;; from its identifying opcode designator, availing in the recognition
;; of the represented operation, the tally of bits --- 16 for "jp", 8
;; for any other instruction ---, and an equinumerant bit sequence apply
;; themselves to its definition. In consectary, a chunk constitutes
;; the binary analogue of the assembly language's ``Instruction'' class,
;; which please peruse aboon.
;; 
;; The interpreter, after its receipt of a byte sequence, converts the
;; same into a vector of ``Chunk'' instances, on which it ultimately
;; operates.
;; 
;; 
;; APPENDIX A: Binary Conventions
;; ==============================
;; Bytecode is expressed as sequence of zero or more octets, that is,
;; eight adjacent bits comprising a byte, and usually encoded in a
;; numeric datum. The bits, enumerated from the least significant (LSB)
;; to the most significant (MSB) position, act as a function of the
;; bit's index, rising proportionally to this subscript's value:
;;   
;;   bit[0] --- least significant bit (LSB)
;;   bit[1]
;;   bit[2]
;;   bit[3]
;;   bit[4]
;;   bit[5]
;;   bit[6]
;;   bit[7] --- most  significant bit (MSB)
;; 
;; When introduced into textual descriptions, the notation by popular
;; convention aligns the most significant bit to the left, decreasing in
;; capacity while traversing dextrally:
;;   
;;   bit[7], bit[6], bit[5], bit[4], bit[3], bit[2], bit[1], bit[0]
;; 
;; Given a binary pattern
;;   
;;   10001110
;; 
;; its anatomy is subject to the unambiguous construe that
;;   
;;   10001110
;;   ^      ^
;;   MSB    LSB
;; 
;; When enumerating its bits, we yield
;;   1 0 0 0 1 1 1 0 <- bit sequence
;;   7 6 5 4 3 2 1 0 <- bit indices from bit[7] to bit[0]
;; 
;; which is tantamount to the explicit, and nimious, explication
;;   
;;   bit[0] = 0 --- least significant bit (LSB)
;;   bit[1] = 1
;;   bit[2] = 1
;;   bit[3] = 1
;;   bit[4] = 0
;;   bit[5] = 0
;;   bit[6] = 0
;;   bit[7] = 1 --- most  significant bit (MSB)
;; 
;; If entailed in a sequence of more than one member, the combination of
;; octets with the purpose of forming one unit --- regardless of
;; intention and context --- poses a field of inquiry and much
;; contention. Two alternatives are usually proffered:
;;   
;;   (a) Big endian
;;       The first byte provides the "most valuable" eight bits, with
;;       any subsequent octet gradually discounting in value. The
;;       highest bit of the first byte thus forms the most significant
;;       bit (MSB) of the combined bit pattern, while the last bit of
;;       the desinent byte supplies the least significant bit (LSB).
;;       An incoming sequence of three bytes S = (s[1], s[2], s[3]),
;;       with
;;         s[1] = 11111111
;;         s[2] = 01010101
;;         s[3] = 00000000
;;       is construed to manufacture the following 24-bit pattern:
;;         11111111 01010101 00000000
;;         |-s[1]-| |-s[2]-| |-s[3]-|
;;   (b) Little endian
;;       The first byte provides the "least valuable" eight bits, with
;;       any subsequent octet gradually accruing in value. The lowest
;;       bit of the first byte thus forms the least significant bit
;;       (LSB) of the combined bit pattern, while the highest bit of
;;       the desinent byte supplies the most significant bit (MSB).
;;       An incoming sequence of three bytes S = (s[1], s[2], s[3]),
;;       with
;;         s[1] = 11111111
;;         s[2] = 01010101
;;         s[3] = 00000000
;;       is construed to manufacture the following 24-bit pattern:
;;         00000000 01010101 11111111
;;         |-s[3]-| |-s[2]-| |-s[1]-|
;; 
;; 
;; Appendix B: Two's Complement
;; ============================
;; A select of topics significant or interesting in the context of the
;; LayerASM specification shall be elucidated in the next sections.
;; 
;; == TWO'S COMPLEMENT ==
;; The establishment of a convention regarding the encoding of signed
;; integer numbers limns a challenge encountered with several
;; distinguished solutions, amidst which are tallied, without the
;; contingency's exhaustion:
;;   - Signed magnitude
;;   - One's complement
;;   - Two's complement
;; The LayerASM programming language resorts to the third alternative.
;; 
;; The interpretation of a bit sequence as a signed integer expands as
;; follows: Given a sequence of N bits B, with
;;   B = (b[N], b[N-1], ..., b[i] ..., b[1]),
;; where b[i] is in {0, 1}, and b[N] is assigned the role of the most
;; significant bit (MSB), the following rules hold:
;;   
;;   (a) If the most significant bit b[N] equals 1, the represented
;;       integer is negative, in which case its value is computed by
;;       subtracting from the nonnegative, unsigned integer value
;;       ABSOLUTEBITS of its N bits the maximum integer value MAXIMUM
;;       representable by N bits:
;;         let maximum      <- 2^N
;;         let absoluteBits <- (b[N-1], ..., b[i], ..., b[1])
;;         let result       <- absolute - maximum.
;;   (b) If the most significant bit b[N] equals 0, the represented
;;       integer is positive or zero, in which case its value is simply
;;       computed as the nonnegative integer corresponding to the
;;       lowest (N-1) bit from B:
;;         let result <- (b[N-1], ..., b[i] ..., b[1]).
;;       Note that, in the face of b[N] being zero and thus
;;       ineffectuous, the equivalent definition might entail in the
;;       result all of B's N bits:
;;         let result <- (b[N], b[N-1], ..., b[i] ..., b[1]).
;; 
;; The athwart application, yielding for an extant signed integer I the
;; two's complement bit sequence B, requires as an additional datum the
;; number of bits intended for the latter's encoding. Given with tally
;; as N, the bit sequence B as
;;   B = (b[N], b[N-1], ..., b[i] ..., b[1])
;; can be obtained from the integer I by the following process:
;;   
;;   (a) If the integer I is negative, the most significant bit b[N] in
;;       B is set to 1, while the remaining (N-1) bits assume the binary
;;       representation of I's absolute value:
;;         let b[N] <- 1
;;         let (b[N-1], ..., b[i], ..., b[1]) <- asBinary(I).
;;   (b) If the inter I is positive or zero, B simply assumes the binary
;;       representation of I:
;;         let (b[N], ..., b[i], ..., b[1]) <- asBinary(I).
;; 
;; A patent ramification capable of derivation from the sign bit imposes
;; that, if the largest positive integer possible can be represented by
;; M bits, an additional bit, that is, N = M + 1 bits, must be allotted
;; to the two's complement in order to encode and decode negative values
;; as well. If, for example, one optates to encode signed integers in
;; the two's complement, despite the maximum of +5 being reliant upon
;; three (3) bits, the encoding mandates at least four (4) binary
;; digits, as the most significant would assume the value 1 as a
;; sentinel for the negative moeity. +5 no longer resolves to a binary
;; 101, but actually to 0101. The former notation may still be utilized,
;; of course, but the remembrance of the N = 4 bit tally must be
;; implemented.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-02-08
;; 
;; Sources:
;;   [esolang2020layerasm]
;;   -> "https://esolangs.org/wiki/LayerASM"
;;       o Original specification of the LayerASM programming language.
;;   
;;   [stackoverflow2012hexassembly]
;;   -> "https://stackoverflow.com/questions/11733731/how-to-represent-hex-value-such-as-ffffffbb-in-x86-assembly-programming"
;;       o Disquisition of the literal number notations across different
;;         assemblers.
;;   
;;   [stackoverflow2012ieee754max]
;;   -> "https://stackoverflow.com/questions/10233444/max-float-represented-in-ieee-754"
;;       o Maximum value of IEEE-754 single float: 3.4028235*10^{38}.
;;       o In Common Lisp notation: 3.4028233E+38.
;;   
;;   [stackoverflow2013csignedbyte]
;;   -> "https://stackoverflow.com/a/18013782"
;;       o Describes two's complement for representing signed integer
;;         numbers.
;;   
;;   [stackoverflow2014chunk]
;;   -> "https://stackoverflow.com/questions/22008273/what-do-chunk-block-offset-buffer-and-sector-mean"
;;       o Furnishes definitions for the terms "chunk", "block",
;;         "offset", "buffer", and "sector".
;;   
;;   [stackoverflow2015ieee754range]
;;   -> "https://stackoverflow.com/questions/32193791/single-precision-floating-point-format-range"
;;       o Describes the range of the IEEE-754 single float type.
;;       o Mentions that this type is symmetric for positive and
;;         negative numbers.
;;   
;;   [venners1996underthehood]
;;   -> "https://www.infoworld.com/article/2077233/bytecode-basics.html"
;;       o Describes the role of bytecode in the context of the Java
;;         virtual machine.
;;   
;;   [wikipedia2021bytecode]
;;   -> "https://en.wikipedia.org/wiki/Bytecode"
;;       o Definition of bytecode.
;;   
;;   [wikipedia2021machinecode]
;;   -> "https://en.wikipedia.org/wiki/Machine_code"
;;       o Definition of machine code.
;;   
;;   [wikipedia2021opcode]
;;   -> "https://en.wikipedia.org/wiki/Opcode"
;;       o Definition of opcode.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, the same defaulting to ``T''."
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
   entries, each key-value pair's key conforming to the KEY-TYPE and
   the associated value to the VALUE-TYPE, both of which default to the
   comprehensive ``T''."
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
    `(satisfies, predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a communication conduit for input
   and output operations, including ``format'', ``write'', and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype format-control ()
  "The ``format-control'' type defines an object capable of representing
   a target for manipulations applied by formatting functions, such as
   the ``format'' and ``error'' operations."
  '(or string
       (function (destination &rest T) *)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines a byte composed of eight adjacent bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype layer-data-type ()
  "The ``layer-data-type'' type enumerates the possible data types
   utilized by the LayerASM layers."
  '(member :character :8-bit-integer :16-bit-integer :32-bit-float))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class furnishes an encapsulation applied to a
   significant portion of a piece of LayerASM source code."
  (type  (error "No token type specified.") :type keyword)
  (value NIL                                :type T))

;;; -------------------------------------------------------

(defun token-has-type-of (token expected-type)
  "Checks whether the TOKEN is of the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (token-type token) expected-type)))))

;;; -------------------------------------------------------

(defun token-is-argument (token)
  "Checks whether the TOKEN represents a LayerASM instruction argument,
   returning a ``boolean'' value of ``T'' on confirmation, or ``NIL''
   on mismatch."
  (declare (type Token token))
  (the boolean
    (not (null
      (or (token-has-type-of token :number)
          (token-has-type-of token :constant))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "No source for the lexer specified.")
    :type          string
    :documentation "The LayerASM source code analyzed by this lexer.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION."))
  (:documentation
    "The ``Lexer'' class analyzes a piece of LayerASM code and produces
     tokens from its significant portions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (< position (length source))
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' operating on the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean (not (null (char= character #\Space)))))

;;; -------------------------------------------------------

(defun instruction-identifier-p (identifier)
  "Checks whether the IDENTIFIER represents an instruction name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string identifier))
  (the boolean
    (not (null
      (member identifier '("cl" "do" "jp" "mv") :test #'string=)))))

;;; -------------------------------------------------------

(defun get-instruction-type (identifier)
  "Expects the IDENTIFIER to be a reserved instruction name, and returns
   a keyword symbol in the agency of the instruction type."
  (declare (type string identifier))
  (the keyword (intern (string-upcase identifier) :keyword)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next character in the source, if possible,
   and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (< position (1- (length source)))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-number-literal (lexer radix)
  "Starting at the current LEXER position, reads an integer literal
   represented in the RADIX, and returns its decimal (base-10)
   value in a ``Token''."
  (declare (type Lexer          lexer))
  (declare (type (integer 2 36) radix))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (when (find character "+-" :test #'char=)
              (write-char character digits)
              (lexer-advance lexer))
            (loop while (and character (digit-char-p character radix)) do
              (write-char character digits)
              (lexer-advance lexer)))
          :radix radix)))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position in the source, reads an
   alphabetic identifier and returns a token representing either an
   instruction or a constant."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (let ((identifier ""))
      (declare (type string identifier))
      (setf identifier
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop while (and character (alpha-char-p character)) do
            (write-char character content)
            (lexer-advance lexer))))
      (the Token
        (if (instruction-identifier-p identifier)
          (make-token :instruction (get-instruction-type identifier))
          (make-token :constant    identifier))))))

;;; -------------------------------------------------------

(defun lexer-skip-spaces (lexer)
  "Starting at the LEXER's current position in the source, skips zero
   or more adjacent spaces and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (space-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Starting at the LEXER's current position in the source, skips the
   comment up to, but not including, a newline or end of file, and
   returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (loop while (and character (char/= character #\Newline)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon the LEXER source's exhaustion, this function constantly returns
   a new ``Token'' instance of the ``:eof'' (end of file) type."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (cond
        ;; End of code?
        ((null character)
          (make-token :eof NIL))
        
        ;; Semicolon found?
        ;; => Skip comment.
        ((char= character #\;)
          (lexer-skip-comment   lexer)
          (lexer-get-next-token lexer))
        
        ;; Space found?
        ;; => Skip it.
        ((space-character-p character)
          (lexer-skip-spaces    lexer)
          (lexer-get-next-token lexer))
        
        ;; Newline found?
        ((char= character #\Newline)
          (lexer-advance lexer)
          (make-token :newline #\Newline))
        
        ;; Comma found?
        ((char= character #\,)
          (lexer-advance lexer)
          (make-token :comma ","))
        
        ;; "$" found?
        ;; => An integer literal, contingently with a radix.
        ((char= character #\$)
          (lexer-advance lexer)
          (let ((radix-identifier character))
            (declare (type (or null character) radix-identifier))
            (case radix-identifier
              ;; No radix identifier found?
              ((NIL)
                (error "Expected a radix for the number literal, ~
                        but encountered EOF."))
              ;; Binary number?
              (#\b
                (lexer-advance lexer)
                (lexer-read-number-literal lexer 2))
              ;; Decimal number?
              (#\d
                (lexer-advance lexer)
                (lexer-read-number-literal lexer 10))
              ;; Octal number?
              (#\o
                (lexer-advance lexer)
                (lexer-read-number-literal lexer 8))
              ;; Hexadecimal number?
              (#\x
                (lexer-advance lexer)
                (lexer-read-number-literal lexer 16))
              ;; Decimal number?
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)
                (lexer-read-number-literal lexer 10))
              ;; Invalid radix identifier?
              (otherwise
                (error "Invalid radix identifier: ~s."
                  radix-identifier)))))
        
        ;; Digit or sign found?
        ;; => Decimal integer literal.
        ((find character "0123456789+-" :test #'char=)
          (lexer-read-number-literal lexer 10))
        
        ;; Alphabetic character found?
        ;; => Mnemonic or constant.
        (T
          (lexer-read-identifier lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type value)))
  "An ``Operand'' acts an an argument to an instruction."
  (type  NIL :type (or null keyword))
  (value NIL :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (operands NIL))))
  "An ``Instruction'' encapsulates an assembler operation and its
   appertaining operands in a form that permits its subsequent
   application."
  (type     NIL :type (or null keyword))
  (operands NIL :type (list-of Operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "The parser requires a lexer.")
    :type          Lexer
    :documentation "The lexer responsible for furnishing the tokens.")
   (current-token
    :initarg       :current-token
    :initform      NIL
    :type          (or null Token)
    :documentation "The most recent token queried from the LEXER."))
  (:documentation
    "The ``Parser'' class generates from a stream of tokens a sequence
     of instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' receiving its tokens from the
   LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on success superseding it by the next token
   queried from the PARSER's internal lexer and returning the modified
   PARSER, while on failure signaling an error."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer           lexer))
    (declare (type (or null Token) current-token))
    (if (token-has-type-of current-token expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Invalid token type: Expected ~s but found token ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-operand (parser)
  "Parses and returns an ``Operand''."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (the Operand
      (case (token-type current-token)
        (:number
          (prog1
            (make-operand :number (token-value current-token))
            (parser-eat parser :number)))
        (:constant
          (prog1
            (make-operand :constant (token-value current-token))
            (parser-eat parser :constant)))
        (T
          (error "Expected a number or a constant as an operand token, ~
                  but encountered ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-operands (parser)
  "Parses and returns a list of zero or more instruction operands using
   the PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((operands NIL))
      (declare (type (list-of Operand) operands))
      ;; Start operands collection if at least one operand is found.
      (when (token-is-argument current-token)
        (push (parser-parse-operand parser) operands)
        ;; Each further operand is introduced by a comma ",".
        (loop while (token-has-type-of current-token :comma) do
          (parser-eat parser :comma)
          (if (token-is-argument current-token)
            (push (parser-parse-operand parser) operands)
            (error "Expected an operand to follow the comma, but ~
                    encountered the token ~s."
              current-token))))
      (the (list-of Operand) (nreverse operands)))))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses and returns an instruction using the PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((instruction-token current-token))
      (declare (type Token instruction-token))
      (parser-eat parser :instruction)
      (the Instruction
        (make-instruction
          (token-value instruction-token)
          (parser-parse-operands parser))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the source code using the PARSER and returns a vector of
   instructions."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type (or null Token) current-token))
    (let ((instructions NIL))
      (declare (type (list-of Instruction) instructions))
      
      (loop do
        (cond
          ;; End of source?
          ((null current-token)
            (loop-finish))
          
          ;; End of source?
          ((token-has-type-of current-token :eof)
            (loop-finish))
          
          ;; Empty line?
          ((token-has-type-of current-token :newline)
            (parser-eat parser :newline))
          
          ;; Instruction?
          ((token-has-type-of current-token :instruction)
            (let ((instruction (parser-parse-instruction parser)))
              (declare (type Instruction instruction))
              (push instruction instructions)
              
              ;; Check for end of instruction (newline or EOF).
              (cond
                ((null current-token)
                  (loop-finish))
                ((token-has-type-of current-token :eof)
                  (loop-finish))
                ((token-has-type-of current-token :newline)
                  (parser-eat parser :newline))
                (T
                  (error "Expected a newline or end of file following ~
                          the instruction, but encountered the token ~
                          ~s."
                    current-token)))))
          
          ;; Invalid token?
          (T
            (error "Invalid token encountered during parsing: ~s."
              current-token))))
      
      (the (simple-array Instruction (*))
        (coerce (nreverse instructions)
          '(simple-array Instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Invalid-Operand-Error".         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Operand-Error (simple-condition)
  ()
  (:documentation
    "An error of the ``Invalid-Operand-Error'' type signals that an
     instruction operand must be deemed inappropriate in the current
     situation based upon its type or value."))

;;; -------------------------------------------------------

(defun throw-invalid-operand-error (format-control
                                    &rest format-arguments)
  "Signals an error of the type ``Invalid-Operand-Error'' the report
   message of which is produced by formatting the FORMAT-CONTROL using
   the FORMAT-ARGUMENTS."
  (declare (type format-control format-control))
  (declare (type (list-of T)    format-arguments))
  (error 'Invalid-Operand-Error
    :format-control   format-control
    :format-arguments format-arguments))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of assembler to bytecode compiler.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string octet) +SYMBOLIC-CONSTANTS+))

(defparameter +SYMBOLIC-CONSTANTS+
  (let ((constants (make-hash-table :test #'equal)))
    (declare (type (hash-table-of string octet) constants))
    ;; Instructions.
    (setf (gethash "cl"    constants) #b11001001)
    (setf (gethash "do"    constants) #b00101000)
    (setf (gethash "jp"    constants) #b10001000)
    (setf (gethash "mv"    constants) #b01000010)
    ;; Directions.
    (setf (gethash "up"    constants) #b00)
    (setf (gethash "down"  constants) #b01)
    (setf (gethash "right" constants) #b10)
    (setf (gethash "left"  constants) #b11)
    ;; Operations.
    (setf (gethash "in"    constants) #b000)
    (setf (gethash "de"    constants) #b001)
    (setf (gethash "db"    constants) #b010)
    (setf (gethash "hl"    constants) #b011)
    (setf (gethash "i"     constants) #b100)
    (setf (gethash "o"     constants) #b101)
    ;; Conditions.
    (setf (gethash "a"     constants) #b000)
    (setf (gethash "z"     constants) #b001)
    (setf (gethash "nz"    constants) #b010)
    (setf (gethash "lt"    constants) #b011)
    (setf (gethash "gt"    constants) #b100)
    (the (hash-table-of string octet) constants))
  "Defines the symbolic constants or mnemonics, which associate
   identifiers with integer values.
   ---
   The table is composed of entries, each of which constitutes the
   following mapping:
     string => (unsigned-byte 8)")

;;; -------------------------------------------------------

(defun get-bytecode-for (mnemonic)
  "Returns the integer bytecode associated with the MNEMONIC if such
   exists, throwing an error on failure to locate the same."
  (declare (type string mnemonic))
  (multiple-value-bind (bytecode contains-mnemonic)
      (gethash mnemonic +SYMBOLIC-CONSTANTS+)
    (declare (type (or null octet) bytecode))
    (declare (type T               contains-mnemonic))
    (unless contains-mnemonic
      (error "No bytecode for mnemonic ~s found." mnemonic))
    (the octet bytecode)))

;;; -------------------------------------------------------

(defun decode-twos-complement (bits &optional (bit-count 8))
  "Interprets the BITS as a two's complement representation allotted the
   BIT-COUNT tally of binary digits and returns the signed integer
   obtained therefrom."
  (declare (type unsigned-byte bits))
  (declare (type (integer 0 *) bit-count))
  (the integer
    (if (logbitp (1- bit-count) bits)
      (- (abs bits) (expt 2 bit-count))
      bits)))

;;; -------------------------------------------------------

(defun encode-in-twos-complement (number &optional (bit-count 8))
  "Encodes the signed integer NUMBER as an integer-encoded unsigned
   binary value in the two's complement, allotting to the complete
   encoded bit pattern at most a BIT-COUNT tally of bits."
  (declare (type integer       number))
  (declare (type (integer 0 *) bit-count))
  (the unsigned-byte
    (if (minusp number)
      (let ((maximum (expt 2 bit-count)))
        (declare (type (integer 0 *) maximum))
        (- maximum (abs number)))
      number)))

;;; -------------------------------------------------------

(defun binary-encode-number (number)
  "Encodes the signed integer NUMBER as an integer-encoded unsigned
   binary value in the two's complement."
  (declare (type integer number))
  (the unsigned-byte (encode-in-twos-complement number)))

;;; -------------------------------------------------------

(defun operand-resolve (operand)
  "Resolves the OPERAND's value if not already a literal, and returns
   the thus produced datum."
  (declare (type Operand operand))
  (the integer
    (case (operand-type operand)
      (:constant
        (get-bytecode-for (operand-value operand)))
      (:number
        (operand-value operand))
      (otherwise
        (error "The operand ~s exhibits an invalid type." operand)))))

;;; -------------------------------------------------------

(defun check-operands (instruction expected-number-of-operands)
  "Checks whether the INSTRUCTION contains exactly the
   EXPECTED-NUMBER-OF-OPERANDS, on ascertainment returning its operands
   list, otherwise signaling an error."
  (declare (type Instruction   instruction))
  (declare (type (integer 0 *) expected-number-of-operands))
  (let ((operands (instruction-operands instruction)))
    (declare (type (list-of Operand) operands))
    (unless (= expected-number-of-operands (length operands))
      (error "The instruction '~a' accepts ~d operands, but ~d have ~
              been provided."
        (instruction-type instruction)
        expected-number-of-operands
        (length operands)))
    (the (list-of Operand) operands)))

;;; -------------------------------------------------------

(defun check-range (subject minimum maximum subject-description)
  "Checks whether the SUBJECT is contained in the inclusive range of
   [MINIMUM, MAXIMUM], on confirmation returning the SUBJECT itself,
   otherwise signaling an ``Invalid-Operand-Error'' whose report message
   nevens the SUBJECT-DESCRIPTION as the offending datum."
  (declare (type integer subject))
  (unless (<= minimum subject maximum)
    (throw-invalid-operand-error
      "The ~a ~d is outside of the range [~d, ~d]"
      subject-description subject minimum maximum))
  (the integer subject))

;;; -------------------------------------------------------

(defun check-layer-id (layer-id)
  "Checks whether the LAYER-ID resides in the tolerated range of [0, 3],
   returning on confirmation the LAYER-ID, otherwise signaling an
   ``Invalid-Operand-Error''."
  (declare (type integer layer-id))
  (the integer (check-range layer-id 0 3 "layer ID")))

;;; -------------------------------------------------------

(defun check-direction (direction)
  "Checks whether the DIRECTION resides in the tolerated range of
   [0, 3], returning on confirmation the DIRECTION, otherwise signaling
   an ``Invalid-Operand-Error''."
  (declare (type integer direction))
  (the integer (check-range direction 0 3 "direction")))

;;; -------------------------------------------------------

(defun check-condition (condition)
  "Checks whether the CONDITION resides in the tolerated range of
   [0, 4], returning on confirmation the CONDITION, otherwise signaling
   an ``Invalid-Operand-Error''."
  (declare (type integer condition))
  (the integer (check-range condition 0 4 "condition")))

;;; -------------------------------------------------------

(defun check-displacement (displacement)
  "Checks whether the DISPLACEMENT resides in the tolerated range of
   [-128, +127], returning on confirmation the DISPLACEMENT, otherwise
   signaling an ``Invalid-Operand-Error''."
  (declare (type integer displacement))
  (the integer (check-range displacement -128 +127 "displacement")))

;;; -------------------------------------------------------

(defun check-operation (operation)
  "Checks whether the OPERATION resides in the tolerated range of
   [0, 5], returning on confirmation the OPERATION, otherwise signaling
   an ``Invalid-Operand-Error''."
  (declare (type integer operation))
  (the integer (check-range operation 0 5 "operation")))

;;; -------------------------------------------------------

(defun compile-to-bytecode (instructions)
  "Compiles the assembler INSTRUCTIONS and returns the corresponding
   bytecode instructions."
  (declare (type (simple-array Instruction (*)) instructions))
  (let ((bytecodes NIL))
    (declare (type (list-of octet) bytecodes))
    (loop for instruction of-type Instruction across instructions do
      (case (instruction-type instruction)
        ;; "cl": 11yy1001
        (:cl
          (destructuring-bind (layer-id)
              (check-operands instruction 1)
            (declare (type Operand layer-id))
            (check-layer-id (operand-resolve layer-id))
            (let ((layer-id-bits (operand-resolve layer-id)))
              (declare (type integer layer-id-bits))
              (if (<= 0 layer-id-bits 3)
                (let ((bits #b11001001))
                  (declare (type octet bits))
                  (setf (ldb (byte 2 4) bits) layer-id-bits)
                  (push bits bytecodes))
                (error "Invalid layer ID ~d." layer-id-bits)))))
        
        ;; "do": b00101ppp
        (:do
          (destructuring-bind (operation)
              (check-operands instruction 1)
            (declare (type Operand operation))
            (check-operation (operand-resolve operation))
            (let ((operation-bits (operand-resolve operation)))
              (declare (type integer operation-bits))
              (if (<= 0 operation-bits 5)
                (let ((bits #b00101000))
                  (declare (type octet bits))
                  (setf (ldb (byte 3 0) bits) operation-bits)
                  (push bits bytecodes))
                (error "Invalid operation: ~d." operation-bits)))))
        
        ;; "jp": 10dd1ccc bbbbbbbb
        (:jp
          (destructuring-bind (direction condition displacement)
              (check-operands instruction 3)
            (declare (type Operand direction))
            (declare (type Operand condition))
            (declare (type Operand displacement))
            
            (check-direction    (operand-resolve direction))
            (check-condition    (operand-resolve condition))
            (check-displacement (operand-resolve displacement))
            
            (let ((direction-bits    (operand-resolve direction))
                  (condition-bits    (operand-resolve condition))
                  (displacement-bits (binary-encode-number
                                       (operand-resolve displacement))))
              (declare (type integer direction-bits))
              (declare (type integer condition-bits))
              (declare (type integer displacement-bits))
              
              (let ((first-byte  #b10001000)
                    (second-byte #b00000000))
                (declare (type octet first-byte))
                (declare (type octet second-byte))
                
                (setf (ldb (byte 2 4) first-byte) direction-bits)
                (setf (ldb (byte 3 0) first-byte) condition-bits)
                (setf second-byte                 displacement-bits)
                
                (push first-byte  bytecodes)
                (push second-byte bytecodes)))))
        
        ;; "mv": 01dd0010
        (:mv
          (destructuring-bind (direction)
              (check-operands instruction 1)
            (declare (type Operand direction))
            (check-direction (operand-resolve direction))
            (let ((bits           #b01000010)
                  (direction-bits (operand-resolve direction)))
              (declare (type octet   bits))
              (declare (type integer direction-bits))
              (setf (ldb (byte 2 4) bits) direction-bits)
              (push bits bytecodes))))
        
        ;; Invalid operation?
        (otherwise
          (error "Invalid instruction name: ~s."
            (instruction-type instruction)))))
    
    (the (simple-array octet (*))
      (coerce (nreverse bytecodes)
        '(simple-array octet (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Pointer".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pointer
  (:constructor make-pointer (x y)))
  "The ``Pointer'' class models a cursor into a ``Grid''."
  (x 0 :type (integer 0 15))
  (y 0 :type (integer 0 15)))

;;; -------------------------------------------------------

(defmacro with-pointer ((x-variable y-variable) pointer-expression
                        &body body)
  "Evaluates the POINTER-EXPRESSION to a ``Pointer'' object, binds its
   x-coordinate to the X-VARIABLE and its y-coordinate to the,
   Y-VARIABLE, evaluated the BODY forms, and returns the result of the
   last processed form."
  (let ((evaluated-pointer (gensym)))
    (declare (type symbol evaluated-pointer))
    `(let ((,evaluated-pointer ,pointer-expression))
       (declare (type Pointer ,evaluated-pointer))
       (symbol-macrolet
           ((,x-variable
              (the (integer 0 15)
                (pointer-x ,evaluated-pointer)))
            (,y-variable
              (the (integer 0 15)
                (pointer-y ,evaluated-pointer))))
         ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Layer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Layer ()
  ((data-type
    :initarg       :data-type
    :initform      (error "No layer data type specified.")
    :accessor      layer-data-type
    :type          layer-data-type
    :documentation "The nominal data type of this layer's grid.")
   (cells
    :initarg       :cells
    :initform      (make-array '(16 16) :adjustable NIL :fill-pointer NIL)
    :accessor      layer-cells
    :type          (simple-array * (16 16))
    :documentation "A 16x16 grid of data items in accord with the
                    DATA-TYPE.")
   (pointer
    :initarg       :pointer
    :initform      (make-pointer 0 0)
    :accessor      layer-pointer
    :type          Pointer
    :documentation "A pointer to the currently selected cell, construed
                    in Cartesian coordinates order as ``(x . y)''."))
  (:documentation
    "The ``Layer'' class represents a data layer as a grid of 16x16
     cells, each element of which conforms to the same data type.
     ---
     A pointer is defined holding a reference to the currently selected
     cell."))

;;; -------------------------------------------------------

(defun get-array-element-type-for (layer-data-type)
  "Returns an element type compatible with the ``make-array'' function's
   eponymous option which corresponds to the LAYER-DATA-TYPE."
  (declare (type layer-data-type layer-data-type))
  (the (or symbol list)
    (case layer-data-type
      ((:character :8-bit-integer) '(unsigned-byte  8))
      (:16-bit-integer             '(unsigned-byte 16))
      (:32-bit-float               '(real -3.4028233E+38 3.4028233E+38))
      (otherwise
        (error "Invalid LayerASM data type: ~s." layer-data-type)))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((layer Layer) &key)
  (declare (type Layer layer))
  (with-slots (data-type cells) layer
    (declare (type layer-data-type          data-type))
    (declare (type (simple-array * (16 16)) cells))
    (setf cells
      (make-array '(16 16)
        :element-type    (get-array-element-type-for data-type)
        :initial-element 0
        :adjustable      NIL
        :fill-pointer    NIL)))
  (the Layer layer))

;;; -------------------------------------------------------

(defun make-layer (data-type)
  "Creates and returns a new ``Layer'' of the DATA-TYPE."
  (declare (type layer-data-type data-type))
  (the Layer (make-instance 'Layer :data-type data-type)))

;;; -------------------------------------------------------

(defun layer-cell-at (layer x y)
  "Returns the value of the LAYER cell at the column X and row Y."
  (declare (type Layer          layer))
  (declare (type (integer 0 15) x))
  (declare (type (integer 0 15) y))
  (the T (aref (slot-value layer 'cells) y x)))

;;; -------------------------------------------------------

(defun clamp (value minimum maximum)
  "Ensures that the VALUE occupies the closed range delineated by the
   MINIMUM and MAXIMUM, and returns the adjusted VALUE."
  (declare (type real value))
  (declare (type real minimum))
  (declare (type real maximum))
  (the real (max minimum (min value maximum))))

;;; -------------------------------------------------------

(defun (setf layer-cell-at) (new-value layer x y)
  "Sets the value of the LAYER cell at designated by the column X and
   the row Y to the NEW-VALUE, and modified LAYER."
  (declare (type real           new-value))
  (declare (type Layer          layer))
  (declare (type (integer 0 15) x))
  (declare (type (integer 0 15) y))
  (with-slots (cells data-type) layer
    (declare (type (simple-array * (16 16)) cells))
    (declare (type layer-data-type          data-type))
    (setf (aref cells y x)
      (case data-type
        (:character
          (clamp new-value 0 255))
        (:8-bit-integer
          (clamp new-value -128 +127))
        (:16-bit-integer
          (clamp new-value -32768 +32767))
        (:32-bit-float
          (clamp new-value -3.4028233E+38 3.4028233E+38))
        (otherwise
          (error "Invalid layer data type: ~s." data-type)))))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-current-cell (layer)
  "Returns the value of the LAYER's active cell."
  (declare (type Layer layer))
  (with-pointer (x y) (slot-value layer 'pointer)
    (declare (type (integer 0 15) x))
    (declare (type (integer 0 15) y))
    (the T (aref (slot-value layer 'cells) y x))))

;;; -------------------------------------------------------

(defun (setf layer-current-cell) (new-value layer)
  "Sets the value of the LAYER's active cell to the NEW-VALUE, and
   returns the modified LAYER."
  (declare (type T     new-value))
  (declare (type Layer layer))
  (with-pointer (x y) (slot-value layer 'pointer)
    (declare (type (integer 0 15) x))
    (declare (type (integer 0 15) y))
    (setf (layer-cell-at layer x y) new-value))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-cell-in-direction (layer direction)
  "Returns the value of the cell neighboring the LAYER's active cell in
   the DIRECTION."
  (declare (type Layer   layer))
  (declare (type (mod 4) direction))
  (with-slots (pointer) layer
    (declare (type Pointer pointer))
    (with-pointer (x y) pointer
      (declare (type (integer 0 15) x))
      (declare (type (integer 0 15) y))
      (the T
        (case direction
          (#b00 (layer-cell-at layer     x  (1- y)))
          (#b01 (layer-cell-at layer     x  (1+ y)))
          (#b10 (layer-cell-at layer (1+ x)     y))
          (#b11 (layer-cell-at layer (1- x)     y))
          (T    (error "Invalid layer direction: ~d." direction)))))))

;;; -------------------------------------------------------

(defun layer-move-into (layer direction)
  "Moves the LAYER's data cell pointer one step into the DIRECTION, and
   returns the modified LAYER."
  (declare (type Layer   layer))
  (declare (type (mod 4) direction))
  (with-slots (pointer) layer
    (declare (type Pointer pointer))
    (case direction
      (#b00      (decf (pointer-x pointer)))
      (#b01      (incf (pointer-x pointer)))
      (#b10      (incf (pointer-y pointer)))
      (#b11      (decf (pointer-y pointer)))
      (otherwise (error "Invalid direction: ~s." direction))))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-has-cell-in-direction (layer direction)
  "Checks whether the LAYER contains a neighbor to its active cell in
   the DIRECTION, returning a ``boolean'' value of ``T'' on
   confirmation, otherwise ``NIL''."
  (declare (type Layer   layer))
  (declare (type (mod 4) direction))
  (with-slots (pointer cells) layer
    (declare (type Pointer pointer))
    (with-pointer (x y) pointer
      (declare (type (integer 0 15) x))
      (declare (type (integer 0 15) y))
      (the boolean
        (not (null
          (case direction
            (#b00      (array-in-bounds-p cells     x  (1- y)))
            (#b01      (array-in-bounds-p cells     x  (1+ y)))
            (#b10      (array-in-bounds-p cells (1+ x)     y))
            (#b11      (array-in-bounds-p cells (1- x)     y))
            (otherwise (error "Invalid layer direction: ~d."
                         direction)))))))))

;;; -------------------------------------------------------

(defun layer-increment (layer)
  "Increments the value of the LAYER's current cell, and returns the
   modified LAYER."
  (declare (type Layer layer))
  (incf (layer-current-cell layer))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-decrement (layer)
  "Decrements the value of the LAYER's current cell, and returns the
   modified LAYER."
  (declare (type Layer layer))
  (decf (layer-current-cell layer))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-double (layer)
  "Doubles the value of the LAYER's current cell, and returns the
   modified LAYER."
  (declare (type Layer layer))
  (with-slots (data-type) layer
    (declare (type layer-data-type data-type))
    (setf (layer-current-cell layer)
          (* (layer-current-cell layer) 2)))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-halve (layer)
  "Halves the value of the LAYER's current cell, and returns the
   modified LAYER."
  (declare (type Layer layer))
  (with-slots (data-type) layer
    (declare (type layer-data-type data-type))
    (setf (layer-current-cell layer)
      (case data-type
        ((:character :8-bit-integer :16-bit-integer)
          (round (layer-current-cell layer) 2))
        ((:32-bit-float)
          (float (/ (layer-current-cell layer) 2)))
        (otherwise
          (error "Invalid layer data type encountered while halving: ~s."
            data-type)))))
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-input (layer &key (output T))
  "Queries from the user an input suitable for the LAYER's data type,
   printing the prompt text to the OUTPUT, sets the LAYER's current cell
   value to the received input, and returns the modified LAYER."
  (declare (type Layer       layer))
  (declare (type destination output))
  (with-slots (data-type) layer
    (declare (type layer-data-type data-type))
    (case data-type
      (:character
        (format output "~&Please enter a character: ")
        (setf (layer-current-cell layer)
              (char-code (read-char))))
      ((:8-bit-integer :16-bit-integer)
        (format output "~&Please enter an integer number: ")
        (setf (layer-current-cell layer)
              (parse-integer (read-line))))
      (:32-bit-float
        (format output "~&Please enter a floating-point number: ")
        (setf (layer-current-cell layer)
              (read-from-string (read-line))))
      (otherwise
        (error "Invalid layer data type encountered during input: ~s."
          data-type))))
  (clear-input)
  (the Layer layer))

;;; -------------------------------------------------------

(defun layer-output (layer &key (destination T))
  "Outputs the LAYER' current clel to the DESTINATION, and returns the
   LAYER."
  (declare (type Layer       layer))
  (declare (type destination destination))
  (with-slots (data-type) layer
    (declare (type layer-data-type data-type))
    (case data-type
      (:character
        (write-char (code-char (layer-current-cell layer)) destination))
      ((:8-bit-integer :16-bit-integer :32-bit-float)
        (write (layer-current-cell layer) :stream destination))
      (otherwise
        (error "Invalid layer data type encountered during ouput: ~s."
          data-type))))
  (the Layer layer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Chunk".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Chunk
  (:constructor make-chunk (opcode bit-width bits)))
  "A ``Chunk'' unites a sequence of one or two bytes which in the
   compound represent a machine instruction's bytecode."
  (opcode    NIL :type (or null keyword))
  (bit-width 0   :type (integer 0 *))
  (bits      0   :type unsigned-byte))

;;; -------------------------------------------------------

(defun chunk-bits-at (chunk bit-count start-position)
  "Returns an integer-encoded portion of bits in the CHUNK delineated
   by the BIT-COUNT tally of bits extracted beginning from the
   START-POSITION."
  (declare (type Chunk         chunk))
  (declare (type (integer 0 *) bit-count))
  (declare (type (integer 0 *) start-position))
  (the unsigned-byte
    (ldb (byte bit-count start-position) (chunk-bits chunk))))

;;; -------------------------------------------------------

(defun extract-opcode (byte)
  "Checks whether the two most significant bits (MSBs), or
   'sentinel bits', of the CURRENT-BYTE equal the BITS,
   returning a ``boolean'' result of ``T'' on a match,
   otherwise ``NIL''."
  (declare (type octet byte))
  (let ((sentinel (ldb (byte 2 6) byte)))
    (declare (type (unsigned-byte 2) sentinel))
    (the keyword
      (case sentinel
        (#b00      :do)
        (#b01      :mv)
        (#b10      :jp)
        (#b11      :cl)
        (otherwise (error "Invalid sentinel: ~2,'0b." sentinel))))))

;;; -------------------------------------------------------

(defun convert-bytecode-to-chunks (bytecode)
  "Converts the byte sequence BYTECODE into a vector of ``Chunk''
   objects and returns the resulting collection."
  (declare (type (vector octet *) bytecode))
  (when (plusp (length bytecode))
    (let ((byte-index   0)
          (current-byte (aref bytecode 0)))
      (declare (type fixnum          byte-index))
      (declare (type (or null octet) current-byte))
      (flet ((read-next-byte ()
              "Reads from the BYTECODE the next byte and stores it into
               the CURRENT-BYTE, returning no value."
              (if (< byte-index (1- (length bytecode)))
                (setf current-byte (aref bytecode (incf byte-index)))
                (setf current-byte NIL))
              (values)))
        (the (vector Chunk *)
          (coerce
            (loop
              while current-byte
              collect
                (let ((instruction-type (extract-opcode current-byte)))
                  (declare (type keyword instruction-type))
                  (case instruction-type
                    ;; The instruction "cl", "do", and "mv" are
                    ;; constructed of a single byte only.
                    ((:cl :do :mv)
                      (prog1
                        (make-chunk instruction-type 8 current-byte)
                        (read-next-byte)))
                    ;; The instruction "jp" is composed of two bytes,
                    ;; with the second holding the displacement.
                    (:jp
                      (let ((first-byte current-byte))
                        (declare (type octet first-byte))
                        ;; Read in the displacement byte.
                        (read-next-byte)
                        (prog1
                          (make-chunk instruction-type 16
                            (dpb current-byte
                              (byte 8 0)
                              (ash first-byte 8)))
                          (read-next-byte))))
                    ;; Invalid instruction type?
                    (otherwise
                      (error "Invalid instruction type: ~s."
                        instruction-type)))))
            '(vector Chunk *)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-layers ()
  "Builds the four layers which comprise the LayerASM architecture and
   returns these in a vector."
  (the (simple-array Layer (4))
    (make-array 4
      :element-type 'Layer
      :initial-contents
        (list
          (make-layer :character)
          (make-layer :8-bit-integer)
          (make-layer :16-bit-integer)
          (make-layer :32-bit-float)))))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((layers
    :initarg       :layers
    :initform      (build-layers)
    :type          (simple-array Layer (4))
    :documentation "The four layer comprising the memory.")
   (active-layer-index
    :initarg       :active-layer-index
    :initform      0
    :type          (integer 0 3)
    :documentation "The index of the currently selected layer.")
   (active-layer
    :initarg       :active-layer
    :initform      NIL
    :type          (or null Layer)
    :documentation "The currently selected layer.")
   (bytecode
    :initarg       :bytecode
    :initform      (error "The interpreter must be initialized with ~
                           a chunk vector.")
    :type          (vector Chunk *)
    :documentation "The bytecode instructions to execute."))
  (:documentation
    "The ``Interpreter'' class is responsible for executing bytecode
     instructions with the aim of imbuing effect to the represented
     program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (layers active-layer-index active-layer) interpreter
    (declare (type (simple-array Layer (4)) layers))
    (declare (type (integer 0 3)            active-layer-index))
    (declare (type (or null Layer)          active-layer))
    (setf active-layer (aref layers active-layer-index)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun make-interpreter (bytecode)
  "Creates and returns a new ``Interpreter'' operating on the BYTECODE."
  (declare (type (vector octet *) bytecode))
  (the Interpreter
    (make-instance 'Interpreter
      :bytecode (convert-bytecode-to-chunks bytecode))))

;;; -------------------------------------------------------

(defun interpret-LayerASM (interpreter)
  "Interprets the LayerASM code maintained by the INTERPRETER, and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (bytecode layers active-layer-index active-layer)
      interpreter
    (declare (type (vector Chunk *)         bytecode))
    (declare (type (simple-array Layer (4)) layers))
    (declare (type (integer 0 3)            active-layer-index))
    (declare (type (or null Layer)          active-layer))
    
    (when (plusp (length bytecode))
      (let ((pc            0)
            (current-chunk (aref bytecode 0)))
        (declare (type fixnum          pc))
        (declare (type (or null Chunk) current-chunk))
        
        (flet
            ((read-next-chunk ()
              "Reads from the BYTECODE the next chunk and stores it into
               the current-chunk, returning no value."
              (if (< pc (1- (length bytecode)))
                (setf current-chunk (aref bytecode (incf pc)))
                (setf current-chunk NIL))
              (values)))
          
          (loop while current-chunk do
            (case (chunk-opcode current-chunk)
              ;; "cl"
              (:cl
                (let ((new-layer (chunk-bits-at current-chunk 2 4)))
                  (declare (type (unsigned-byte 2) new-layer))
                  (setf active-layer-index new-layer)
                  (setf active-layer       (aref layers new-layer)))
                (read-next-chunk))
              
              ;; "do"
              (:do
                (let ((operation (chunk-bits-at current-chunk 3 0)))
                  (declare (type (unsigned-byte 3) operation))
                  (case operation
                    ;; increment
                    (#b000
                      (layer-increment active-layer))
                    
                    ;; decrement
                    (#b001
                      (layer-decrement active-layer))
                    
                    ;; double
                    (#b010
                      (layer-double active-layer))
                    
                    ;; halve
                    (#b011
                      (layer-halve active-layer))
                    
                    ;; input
                    (#b100
                      (layer-input active-layer))
                    
                    ;; output
                    (#b101
                      (layer-output active-layer))
                    
                    (otherwise
                      (error 'Invalid-Operand-Error
                        :format-control
                          "Invalid 'do' operation: ~3,'0b."
                        :format-arguments
                          (list operation)))))
                
                (read-next-chunk))
              
              ;; "mv"
              (:mv
                (let ((direction (chunk-bits-at current-chunk 2 4)))
                  (declare (type (unsigned-byte 2) direction))
                  (layer-move-into active-layer direction))
                (read-next-chunk))
              
              ;; "jp"
              (:jp
                (let ((condition    (chunk-bits-at current-chunk 3  8))
                      (direction    (chunk-bits-at current-chunk 2 12))
                      (displacement (chunk-bits-at current-chunk 8  0)))
                  (declare (type (unsigned-byte 3) condition))
                  (declare (type (unsigned-byte 2) direction))
                  (declare (type octet             displacement))
                  
                  (let ((comparison-cell  NIL)
                        (is-condition-met NIL))
                    (declare (type T       comparison-cell))
                    (declare (type boolean is-condition-met))
                    
                    (when (layer-has-cell-in-direction active-layer direction)
                      (setf comparison-cell
                        (layer-cell-in-direction active-layer direction)))
                    
                    (case condition
                      ;; Always.
                      (#b000
                        (setf is-condition-met T))
                      
                      ;; Equal or zero.
                      (#b001
                        (setf is-condition-met
                          (or
                            (zerop (layer-current-cell active-layer))
                            (and comparison-cell
                                 (eql (layer-current-cell active-layer)
                                      comparison-cell)))))
                      
                      ;; Not equal or not zero.
                      (#b010
                        (setf is-condition-met
                          (or
                            (not (zerop (layer-current-cell active-layer)))
                            (and comparison-cell
                                 (not (eql (layer-current-cell active-layer)
                                           comparison-cell))))))
                      
                      ;; Less than.
                      (#b011
                        (setf is-condition-met
                          (< (layer-current-cell active-layer)
                             comparison-cell)))
                      
                      ;; Greater than.
                      (#b100
                        (setf is-condition-met
                          (> (layer-current-cell active-layer)
                             comparison-cell)))
                      
                      (otherwise
                        (error 'Invalid-Operand-Error
                          :format-control
                            "Invalid 'jp' condition: ~3,'0b."
                          :format-arguments
                            (list condition))))
                    
                    (cond
                      (is-condition-met
                        (incf pc (decode-twos-complement displacement))
                        (setf current-chunk (aref bytecode pc)))
                      (T
                        (read-next-chunk))))))
              
              (T
                (error "Invalid chunk: ~s."
                  current-chunk))))))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-LayerASM-bytecode (bytecode)
  "Interprets the LayerASM BYTECODE instructions and returns no value."
  (declare (type (vector octet) bytecode))
  (interpret-LayerASM (make-interpreter bytecode))
  (values))

;;; -------------------------------------------------------

(defun interpret-LayerASM-assembly (assembly-code)
  "Interprets the LayerASM ASSEMBLY-CODE instructions and returns no
   value."
  (declare (type string assembly-code))
  (interpret-LayerASM-bytecode
    (compile-to-bytecode
      (parser-parse
        (make-parser
          (make-lexer assembly-code)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of auxiliary functions.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-byte-vector (&rest bytes)
  "Creates and returns a one-dimensional simple array composed of the
   BYTES and appropriate for the employment as an argument to the
   function ``interpret-LayerASM-bytecode''."
  (declare (type (list-of octet) bytes))
  (the (simple-array octet (*))
    (make-array (length bytes)
      :element-type     'octet
      :initial-contents bytes
      :adjustable       NIL
      :fill-pointer     NIL)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program:
;;   [1] do i         ;; Read and store input          (= do 100          => 00101 100).
;;   [2] do o         ;; Output the current cell value (= do 101          => 00101 101).
;;   [3] jp 0, a, -2  ;; Unconditionally repeat [1].   (= jp 000 11111110 => 10 00 1 000 11111110).
(interpret-LayerASM-bytecode
  (make-byte-vector
    #b00101100
    #b00101101
    #b10001000
    #b11111110))

;;; -------------------------------------------------------

;; Infinitely repeating cat program expressed in assembly code.
(interpret-LayerASM-assembly
  "do i
   do o
   jp 0, a, -2")

;;; -------------------------------------------------------

;; Truth-machine:
;;   [1] cl 1           ;; Change to 8-bit layer (#1).   (= cl 01           => 11 01 1001).
;;   [2] do i           ;; Read and store input          (= do 100          => 00101 100).
;;   [3] do o           ;; Output the current cell value (= do 101          => 00101 101).
;;   [4] jp 0, nz, -1   ;; Repeat if not zero.           (= jp 010 11111101 => 10 00 1 010 11111101).
(interpret-LayerASM-bytecode
  (make-byte-vector
    #b11011001
    #b00101100
    #b00101101
    #b10001010
    #b11111101))

;;; -------------------------------------------------------

;; Truth-machine expressed in assembly code.
(interpret-LayerASM-assembly
  "cl 1           ;; Change to the 8-bit layer (#1).
   do i           ;; Input an integer number.
   do o           ;; Output the current cell value.
   jp 0, nz, -1   ;; Repeat 'do o' if active cell is not zero.
  ")
