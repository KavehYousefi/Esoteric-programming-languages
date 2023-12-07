;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Turi ip ip ip", invented by the Esolang user "LEOMOK" and
;; presented on July 13th, 2022, the syntaxis of which derives from the
;; cognominant meme, an acquisition appropriated for the encheson of
;; manipulating an infinite sequence of real-valued stacks.
;; 
;; 
;; Concept
;; =======
;; The "Turi ip ip ip" programming language desumes its donet's design
;; from the eponymous meme, operating on a bournless series of stacks,
;; the current instance among which establishes at any moment the active
;; unit, amenable to perquisitions and modifications, and aided in their
;; telos by an adminicle, the "backup stack".
;; 
;; == "Turi ip ip ip": FROM SONG TO MEME TO LANGUAGE ==
;; The entheus of the language emanates from the an eponymous meme,
;; presented on May 12th, 2022 by the TikTok user "@cappeman1", the
;; same proceeds from the song "Symbolism" by the artist Electro-Light,
;; published in the year 2014, transferring the phonetic lyrics of the
;; work as "turi ip ip ip" into the context of several images involving
;; the canine species members' suggested communication in speech
;; bubbles.
;; 
;; == "ip" SEQUENCES ENCODE DECIMAL DIGITS ==
;; "Turi ip ip ip" is ligated in its basic concepts to the unary numeral
;; system, which, as counterdistinguished from the acquainted decimal
;; species that deploys ten digits, from inclusive zero (0) to inclusive
;; nine (9), represents such an atomic numeric component by the tally of
;; its singular unary digit --- in this case, the "ip" keyword.
;; 
;; The number of consecutive "ip" tokens, segregated by spaces, and
;; reduced by an amount of one (1), designates the encoded decimal
;; digit. A sensible ramification begotten by this rule imposes the
;; requisitum of at least one "ip" and at most ten (10) such for this
;; transliteration wike, the former bourne answers to a decimal zero
;; (0), while the latter equiparates to the upper extremum of ten (10).
;; 
;; Several decimal digits in succession are the production of these
;; unary sequences, but each encoded digit, or "ip" sequence", separates
;; from its successor digit by a single comma ",". A consectary of this
;; notion shall be limned in a diction of compendiousness:
;;   (a) A sequence of space-separated "ip" tokens accumulates into an
;;       aefauld decimal digit. Such a concurrence shall be nevened a
;;       "digit code".
;;   (b) A comma-separated sequence of these "digit codes" is
;;       concatenated into a single integer object, forming a decimal
;;       digit catena.
;; 
;; A more conspicable apercu shall be an express treatise on the "ip"
;; series's cardinality and its represented decimal digit:
;; 
;;   ------------------------------------------------------------
;;   No. of "ip"s | Illustration                  | Decimal value
;;   -------------+-------------------------------+--------------
;;    1           | ip                            | 0
;;   ............................................................
;;    2           | ip ip                         | 1
;;   ............................................................
;;    3           | ip ip ip                      | 2
;;   ............................................................
;;    4           | ip ip ip ip                   | 3
;;   ............................................................
;;    5           | ip ip ip ip ip                | 4
;;   ............................................................
;;    6           | ip ip ip ip ip ip             | 5
;;   ............................................................
;;    7           | ip ip ip ip ip ip ip          | 6
;;   ............................................................
;;    8           | ip ip ip ip ip ip ip ip       | 7
;;   ............................................................
;;    9           | ip ip ip ip ip ip ip ip ip    | 8
;;   ............................................................
;;   10           | ip ip ip ip ip ip ip ip ip ip | 9
;;   ------------------------------------------------------------
;; 
;; == "tura": SIGN OR DECIMAL POINT --- DEPENDING ON ITS POSITION ==
;; The homologation of negative numbers and their bifurcation into
;; integers and floating-point objects derives both from an aefauld
;; keyword, "tura".
;; 
;; If empighted at a number's front, preceding any digit encoding, the
;; sentinel designates a negative number, contingently integral or
;; floating-point in its nature.
;; 
;; An occurrency betwixt digits associates the same "tura" keyword with
;; a floating-point object; specifically, its position designates the
;; decimal point's locality.
;; 
;; == NUMBER COMPOSITION RULES ==
;; An integer number must be composed of one or more digits. Siclike,
;; for a floating-point datum, it holds that each part, integral and
;; fractional, imposes at least one digit.
;; 
;; == THE NUMBER ENCODING'S COMPONENTS ==
;; Ere the actual table's presentation, an abbozzo shall demonstrate its
;; general conformation:
;; 
;;   +----------------------------------------------------------------+
;;   | Enumeration point (please see description below table)         |
;;   |................................................................|
;;   | Language keyword                                               |
;;   |................................................................|
;;   | Effect or role                                                 |
;;   |................................................................|
;;   | Optional or mandatory for the decoding process                 |
;;   +----------------------------------------------------------------+
;; 
;; Proceeding from this piece of gnarity's communication, an encoded
;; number in the "Turi ip ip ip" language is compact of these elements:
;; 
;;   +----------------------------------------------------------------+
;;   |      (1)      |     (2)      |      (3)      |       (4)       |
;;   |---------------+--------------+---------------+-----------------|
;;   | tura          | ip[1..10]    | tura          | ip[1..10]       |
;;   |...............|..............|...............|.................|
;;   | Negative sign | Integer part | Decimal point | Fractional part |
;;   |...............|..............|...............|.................|
;;   | optional      | imperative   | optional      | imperative for  |
;;   |               |              |               | decimal point   |
;;   +----------------------------------------------------------------+
;; 
;; == THE NUMBER DECODING PROCESS ==
;; Entalented with a fundamental mete of acquaintance concerning the
;; number representation's components, a decoding and assemblage into
;; a plaintext integer or floating-point number by mediation of the
;; vincula atwixt its parts shall be exposed.
;; 
;; For a "Turi ip ip ip" number's decoding, the following principles
;; impose their obedience:
;; 
;;   (1) If the keyword "tura" is stated at the front, the number bears
;;       a negative sign, otherwise it constitutes a positive or neutral
;;       value. At most one "tura" prefixion may be attributed.
;;   
;;   (2) A sequence of one or more digits follows, the same provides the
;;       ultimate number's integral part.
;;       
;;       (2.1) Each digit is represented by a space-separated series of
;;             one to ten "ip" tokens, where the digit's value, an
;;             integer in the range [0, 9], derives from this tally of
;;             "ip" keywords deducted by one.
;;             As a forbisen, the sequence
;;               ip ip ip ip
;;             amplects four (4) "ip" tokens, the thus represented
;;             decoded digit constitute 3:
;;               4 - 1 = 3
;;       (2.2) Each two consecutive digits in the encoding are
;;             segregated by a single comma, any such compartment
;;             defined in lealty to the process ostended under the point
;;             (2.1).
;;             For example, the composition
;;               ip ip ip ip, ip ip
;;             comprehends two digits:
;;               ip ip ip ip = 3
;;               ip ip       = 1
;;             Their concatenation begets the plaintext integer
;;               31
;;             forecause it holds:
;;               ip ip ip ip, ip ip
;;               -----------  -----
;;                    3         1
;;   
;;   (3) If a comma (",") followed by the "tura" keyword is present, the
;;       encoded object represents a floating-point number rather than
;;       an integer, the "tura" sentinel thus acting, in this instance,
;;       as the decimal point (".").
;;   
;;   (4) In the case of the floating-point format's participation,
;;       introduced by the "tura" keyword in the encoding's
;;       interstition, one or more digits must be appended in order to
;;       specify the floating-point number's fractional parts. In this
;;       circumstance the same rules as adduced by the point (2), and,
;;       as a corollary, its subsections (2.1) and (2.2), apply;
;;       concretely, appropriated approximately ipsissima verba:
;;       
;;       (4.1) Each digit is represented by a space-separated series of
;;             one to ten "ip" tokens, where the digit's value, an
;;             integer in the range [0, 9], derives from this tally of
;;             "ip" keywords deducted by one.
;;             As a forbisen, the sequence
;;               ip ip ip ip
;;             amplects four (4) "ip" tokens, the thus represented
;;             decoded digit constitute 3:
;;               4 - 1 = 3
;;       (4.2) Each two consecutive digits in the encoding are
;;             segregated by a single comma, any such compartment
;;             defined in lealty to the process ostended under the point
;;             (2.1).
;;             For example, the composition
;;               ip ip ip ip, ip ip
;;             comprehends two digits:
;;               ip ip ip ip = 3
;;               ip ip       = 1
;;             Their concatenation begets the plaintext fractional
;;             digits
;;               31
;;             forecause it holds:
;;               ip ip ip ip, ip ip
;;               -----------  -----
;;                    3         1
;;       
;;       A more comprehensive example, governing a circumambiency over
;;       anenst the fractional entirety, shall be offered: The encoding
;;         ip ip ip, tura ip ip ip ip, ip ip
;;       acts in the agency of a vehicle to the componency
;;         ip ip ip, tura ip ip ip ip, ip ip
;;         --------  ---- -----------  -----
;;             2       .      3          1
;;       which assembles into the floating-point number
;;         2.31
;;       
;;       As an adminiculum for the "tura" token's twifaced wike, the
;;       following exposition shall present a negative float:
;;         tura ip ip ip, tura ip ip ip ip, ip ip
;;         ---- --------  ---- -----------  -----
;;           -      2       .      3          1
;;       Its decoded object equiparation being the tantamount
;;         -2.31
;; 
;; == EXAMPLES FOR NUMBER ENCODINGS ==
;; A listing of exemplary number encodings, both from the sets of
;; integers and floating-point numbers, shall be adduced as forbisens:
;; 
;;   ------------------------------------------------------------------
;;   Decoded value | Encoding
;;   --------------+---------------------------------------------------
;;   0             | ip
;;   ..................................................................
;;   1             | ip ip
;;   ..................................................................
;;   25            | tura ip ip ip, ip ip ip ip ip ip
;;   ..................................................................
;;   123           | ip ip, ip ip ip, ip ip ip ip
;;   ..................................................................
;;   -1            | tura ip
;;   ..................................................................
;;   -25           | tura ip ip ip, ip ip ip ip ip ip
;;   ..................................................................
;;   0.0           | ip, tura ip
;;   ..................................................................
;;   1.0           | ip ip, tura ip
;;   ..................................................................
;;   2.5           | ip ip ip, tura ip ip ip ip ip ip
;;   ..................................................................
;;   1.23          | ip ip, tura ip ip ip, ip ip ip ip
;;   ..................................................................
;;   -1.0          | tura ip ip, tura ip
;;   ..................................................................
;;   -2.5          | tura ip ip ip, tura ip ip ip ip ip ip
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; "Turi ip ip ip"'s architectural aspect constitutes a twissel
;; composition, that moiety imparted with superior mickleness and
;; paravaunt effect administered by a bilaterally bournless catena of
;; linearly arranged stacks of real-valued elements, and a parhedral
;; factotum kithed in the mold of a backup stack, the specifications of
;; which comply with its manifold consanguines.
;; 
;; == THE MAIN STORAGE: AN INFINITE SERIES OF STACKS ==
;; The architecture's curule component attends to the services of a
;; seriatim arrangement of stacks, bournless in its dispansion along
;; both marches.
;; 
;; Each stack accommodates an infinite tally of integer or
;; floating-point numbers, both species neither in their sign nor their
;; amplitude impounded.
;; 
;; A mobile cursor, or pointer, tivers, empighted at the program's
;; inchoation at the first stack, at any instant the currently active
;; member, among the series the sole instance entalented with a
;; respondency to queries and modifications.
;; 
;; == THE BACKUP STACK: AN AEFAULD ADMINICLE ==
;; A parergal constituent's dedication kithes in the guise of the
;; "backup stack", participating in the differentia of its cognates'
;; hirsel by admitting integers and floating-point numbers in any
;; quantity.
;; 
;; Its agency and purpose designate this parhedral salvatory in its
;; chief competence with a provisional adminicular role, such as the
;; temporary reception of elements from the current stack and the
;; transfer of its personal appurtenances in the athwart airt.
;; 
;; 
;; Data Types
;; ==========
;; A rather mickle amalgam of species serves to comprise the type system
;; commorant in "Turi ip ip ip", its amplectation's circumambience
;; extending across signed integers, signed floating-point numbers, and
;; ASCII characters.
;; 
;; == NUMBERS = INTEGERS + FLOATS ==
;; Those objects of paravaunt influence in a "Turi ip ip ip" program
;; are beteemed by the ilk of integers and floating-point numbers, both
;; neither impounded with respect to their polarity, nor by their
;; precision.
;; 
;; Slightly less endowed with a peisant contribution, natheless
;; essential to the language's capabilities, characters desumed from the
;; ASCII repertoire partake of a rather communicative wike, operating
;; mainly on the interfaces atwixt the machine and its environment --- a
;; diorism whose incarnation rines the standard input and output
;; conduits.
;; 
;; 
;; Syntax
;; ======
;; A syntactical conspection reveals a "Turi ip ip ip" program's
;; conformation as a produce of zero or more instruction, each twain's
;; segregation accompassed by at least one linebreak or semicolon (";").
;; 
;; == INSTRUCTIONS ==
;; "Turi ip ip ip"'s dictionary is desumed from the eponymous meme, an
;; acquisition which permeates both the instruction designators and the
;; arguments.
;; 
;; An instruction, if reliant upon an operand, expects its purveyance by
;; mediation of immediate contribution, destitute of any particular
;; sentinels for their introduction nor delimitation.
;; 
;; The only composite construct, the "while" loop, amplects its zero or
;; more statements with the keyword twissel "eugh eugh" and "turisha".
;; 
;; Each two accolent instructions are separated by one or more sepiment
;; in the form of a newline or semicolon (";").
;; 
;; == ARGUMENTS ==
;; Arguments succeed their instructions in direct immediacy, always
;; assuming the form of integer or floating-point numbers encoded in
;; "ip" and "tura" keyword occurrences, the former of which provide the
;; digits' diorisms, while the latter, depending upon the position,
;; either employ their prefixion to contribute a negative sign, or wone
;; in the intermede for the specification of a floating-point number's
;; fractional point.
;; 
;; For the concrete and detailed rules of a number's assemblage from the
;; "tura" and "ip" keywords, please consult the "Concept" subsections
;; "THE NUMBER ENCODING'S COMPONENTS" and "THE NUMBER DECODING PROCESS".
;; 
;; == WHITESPACES ==
;; Spaces, a species exhausted by the space character and the horizontal
;; tab, constitute an imperative component betwixt two accolent tokens,
;; but may be distributed liberally in their tally in this instant, and
;; dispersed in both magnitude and occasion at any other.
;; 
;; The particular specimen of linebreaks or newlines, on the other hand,
;; serves a more peisant and stringent wike by its capability to
;; demarcate consecutive instructions, equiparated therein with the
;; semicolon (";"). The account of such objects, however, does not
;; incorporate further significance, as empty lines are homologated at
;; any rate.
;; 
;; == COMMENTS ==
;; The language's provision for comments kithes itself in the form of
;; segments ensconced in brackets, introduced by an opening instance
;; "[" and concluding with the closing counterpart "]", inwith which
;; spatiality zero or more ignored characters may reside.
;; 
;; Comments blocks elude the ability of nesting, that is, opening
;; brackets ("[") inside of such an apostille are not recognized as the
;; request for another pairing, but as a sequestered and independent
;; symbol.
;; 
;; == GRAMMAR ==
;; The language's donet shall be the following Extended Backus-Naur
;; Form (ENBF) formulation's subject:
;; 
;;   program       := commands ;
;;   commands      := [ optSeparators
;;                 ,    command
;;                 ,    { separators , command , optSeparators }
;;                 ,    optSeparators
;;                    ]
;;                 ;
;;   optSeparators := [ separators ] ;
;;   separators    := separator , { separator } ;
;;   separator     := ";" | "\n" ;
;;   command       := push | operation | whileLoop ;
;;   push          := "turi" , number ;
;;   operation     := "eugh" , number ;
;;   whileLoop     := "eugh" , "eugh" , commands , "turisha" ;
;;   
;;   number        := [ tura ] , digits , [ "," , tura , digits ] ;
;;   digits        := digit , { "," , digit } ;
;;   digit         := ip , { ip } ;
;;   tura          := "tura" ;
;;   ip            := "ip" ;
;; 
;; 
;; Instruction
;; ===========
;; The "Turi ip ip ip" instruction set is composed of a treble
;; membership, the purposes of which enlists memory management in form
;; of stack selection and manipulation, basic arithmetics, and an
;; aefauld "while"-based iteration construct.
;; 
;; A preponderance among these competences derives from the "eugh"
;; specimen, its sole argument specifying a numeric operation key,
;; whence the miscellany of faculties emanates.
;; 
;; == OVERVIEW ==
;; The following tabular exposition's dation shall capacitate a cursory
;; mete of acquaintance with the language's operational aspect.
;; 
;; Please heed that placeholder sections are underlined via a catena of
;; asterisks ("*"), the thus demarcated spatiality intended to be
;; superseded by valid "Turi ip ip ip" code.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Effect
;;   ------------+-----------------------------------------------------
;;   turi number | Pushes the {number} unto the current stack.
;;        ****** | 
;;   ..................................................................
;;   eugh key    | Executes the operation answering to the numeric
;;        ***    | {key}.
;;               |-----------------------------------------------------
;;               | For an exhaustive listing of the available keys and
;;               | their causata, please consult the subsection
;;               | '"eugh eugh" OPERATION KEYS' alow.
;;   ..................................................................
;;   eugh eugh   | Peeks the current stack's top element and executes
;;               | the zero or more statements atwixt this command and
;;               | the matching "turisha" instruction while the probed
;;               | element does not equal zero (0).
;;               |-----------------------------------------------------
;;               | Please heed that the current stack is probed anew
;;               | upon each iteration cycle's commencement; in
;;               | consectary, the stack's current state is always
;;               | perquired. This probed element, as stated aboon, is
;;               | indagated but not removed from its stack.
;;               |-----------------------------------------------------
;;               | If the current stack is empty, an error of the type
;;               | "EmptyStackError" is signaled.
;;               |-----------------------------------------------------
;;               | If the "eugh eugh" command is not concluded with a
;;               | "turisha" counterpart, a parsing error is signaled.
;;   ..................................................................
;;   turisha     | Terminates a "while" loop instigated by "eugh eugh".
;;               |-----------------------------------------------------
;;               | If the "turisha" command does not correspond to a
;;               | "eugh eugh" counterpart, a parsing error is
;;               | signaled.
;;   ------------------------------------------------------------------
;; 
;; == "eugh eugh" OPERATION KEYS ==
;; An enumeration of the operation keys affiliated with the "eugh"
;; instruction shall kythe in the following table. Any argument
;; committed to the instruction but no present in this list is inflicted
;; with an interdiction and will instigate an error of the type
;; "InvalidOperationKeyError".
;; 
;;   ------------------------------------------------------------------
;;   Key  | Causatum
;;   -----+------------------------------------------------------------
;;   1    | Addition:
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | sum
;;        |   secondPoppedElement + firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let sum                 <-   secondPoppedElement
;;        |                              + firstPoppedElement
;;        |   push sum unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   2    | Subtraction:
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | difference
;;        |   secondPoppedElement - firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let difference          <-   secondPoppedElement
;;        |                              - firstPoppedElement
;;        |   push difference unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   3    | Multiplication:
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | product
;;        |   secondPoppedElement * firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let product             <-   secondPoppedElement
;;        |                              * firstPoppedElement
;;        |   push product unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   4    | Division:
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | quotient
;;        |   secondPoppedElement / firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let quotient            <-   secondPoppedElement
;;        |                              / firstPoppedElement
;;        |   push quotient unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   5    | Negation:
;;        | Pops the top element from the current stack, negates it,
;;        | and pushes the result unto the stack.
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   10   | Remainder (modulo):
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | remainder of the division
;;        |   secondPoppedElement mod firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let remainder           <-     secondPoppedElement
;;        |                              mod firstPoppedElement
;;        |   push remainder unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   20   | Power, exponentiation:
;;        | Pops the first element, firstPoppedElement, from the
;;        | current stack, pops the new top element,
;;        | secondPoppedElement, from the same stack, calculates the
;;        | power
;;        |   expt(secondPoppedElement, firstPoppedElement)
;;        | or
;;        |   secondPoppedElement ^ firstPoppedElement
;;        | and pushes the result unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   let power               <- expt(secondPoppedElement,
;;        |                                   firstPoppedElement)
;;        |   push power unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   30   | Pops the top element from the current stack and prints it
;;        | in its verbatim numeric form to the standard output.
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   40   | Pops the top element from the current stack and prints the
;;        | character whose ASCII code equals its value to the standard
;;        | output.
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;        |------------------------------------------------------------
;;        | If the popped element does not affiliate with a valid ASCII
;;        | code, an error of the type "IncompatibleValuError" is
;;        | signaled.
;;   ..................................................................
;;   50   | Queries the standard input for a string line, iterates its
;;        | characters from the rear to the front, and pushes each
;;        | character's ASCII code unto the current stack.
;;        |------------------------------------------------------------
;;        | Expressed in pseudocode diction, it holds:
;;        |   let input       <- query line from standard input
;;        |   let inputLength <- length(input)
;;        |   for charIndex from (inputLength - 1) down to 0 do
;;        |     let currentChar     <- input[charIndex]
;;        |     let currentCharCode <- ASCII code of currentChar
;;        |     push currentCharCode unto current stack
;;        |   end for
;;        |------------------------------------------------------------
;;        | The reversed traversal mechanism ascertains that the
;;        | character codes transferred to the stack, if reading the
;;        | storage from top to bottom, exactly replicate the message
;;        | symbols in their correct order. For instance, the input
;;        |   abc
;;        | if pushed unto the current stack in the athwart order
;;        |   c, b, a
;;        | produces the ASCII codes of
;;        |   | a  | << top of current stack
;;        |   | b  |
;;        |   | c  |
;;        | For the concept's patration, the character codes amount to:
;;        |   | 97 | << top of current stack
;;        |   | 98 |
;;        |   | 99 |
;;   ..................................................................
;;   100  | Duplicates the current stack's top element, that is, pushes
;;        | a copy of the same unto the stack without the original's
;;        | removal.
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   200  | Swaps the current stack's two top elements, that is,
;;        | exchanges their positions in the stack.
;;        | Expressed in pseudocode diction, it holds:
;;        |   let firstPoppedElement  <- pop from current stack
;;        |   let secondPoppedElement <- pop from current stack
;;        |   push firstPoppedElement  unto current stack
;;        |   push secondPoppedElement unto current stack
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   300  | Substitutes each element in the current stack which
;;        | represents a decimal digit's ASCII code by the
;;        | corresponding digit in its numeric form, pops these,
;;        | concatenates the digits in the order of their conversion
;;        | into a single integer number, and pushes this new datum
;;        | unto the current stack.
;;        | 
;;        | The following affiliations hold for stack objects and their
;;        | resulting productions:
;;        | 
;;        |   -------------------------------
;;        |   Stack element | Surrogate digit
;;        |   --------------+----------------
;;        |         48      |       0
;;        |         49      |       1
;;        |         50      |       2
;;        |         51      |       3
;;        |         52      |       4
;;        |         53      |       5
;;        |         54      |       6
;;        |         55      |       7
;;        |         56      |       8
;;        |         57      |       9
;;        |   -------------------------------
;;        | 
;;        | Stack elements not admitted to eligibility for this purpose
;;        | experience no alteration.
;;        | 
;;        | For example, the stack of four elements
;;        |   top> 49, 50, 51, -77.8 <bottom
;;        |        ^^  ^^  ^^
;;        | transliterates into a cardinality of halve the size as:
;;        |   top> 123,        -77.8 <bottom
;;   ..................................................................
;;   400  | Sets the stack pointer to the next stack.
;;   ..................................................................
;;   1000 | Sets the stack pointer to the previous stack.
;;   ..................................................................
;;   2000 | Pops the top element from the current stack and pushes it
;;        | unto the backup stack.
;;        |------------------------------------------------------------
;;        | If the current stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   3000 | Pops the top element from the backup stack and pushes it
;;        | unto the current stack.
;;        |------------------------------------------------------------
;;        | If the backup stack is empty or insufficiently populated
;;        | to attend to the required data's provision, an error of the
;;        | type "EmptyStackError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The rather compendious treatise vouchsafed by the "Turi ip ip ip"
;; protolog encumbers the same with a certain mete of ambiguities, a
;; subset of which shall be extracted for further scrutiny.
;; 
;; == HOW DOES THE ASCII-TO-NUMBER CONVERSION HANDLE INCONCINNITY? ==
;; The instruction "eugh {300}" serves in the conversion of the current
;; stack's elements into their ASCII counterparts digits and their
;; concatenation into a single numeric amalgam. Maugre its forbisen in
;; the specification, it cannot without patration in certainty the late
;; derived by which ineligible elements influence the result.
;; 
;; Items inwith which is commorant such inconcinnnity's potential
;; enumerate:
;; 
;;   (a) Integer numbers outside of the ASCII code range [0, 255].
;;   (b) Floating-point numbers.
;; 
;; It has been adjudged to assign to these particular objects a
;; distinguishing agency as separators: Upon their encounter, the
;; hitherto collected valid digit sequence experiences the mandated
;; treatment, while the incompatible item concludes this concatenation,
;; with the sentinel value's verbatim state retention. The probing and
;; conversion process perpetuates anew with the subsequent stack
;; element.
;; 
;; 
;; Implementation
;; ==============
;; This project is implemented in the programming language Common Lisp,
;; its enterprise intrines the stages of token generation, assemblage
;; into abstract syntax tree (AST) nodes, and their traversal by the
;; interpreter entity.
;; 
;; A parhedral investment, didascalic in its ultimate haecceity, rines
;; the accommodation of a macro entalented with the wike of hoisting
;; Common Lisp's ``defstruct'' structure definition facility in order to
;; provide augmented convenience in such a composite's slots' access.
;; 
;; == PROGRAM EXECUTION: A PAIR ROYAL OF TIERS ==
;; A "Turi ip ip ip" source code's evaluation is capacitated by a
;; triplicate coefficiency:
;; 
;;   (1) The source code, supplied as a string, is transformed, by a
;;       lexical analyzer's, or lexer's, efforts into a catena of
;;       significant objects, the tokens.
;;   
;;   (2) The token stream experiences its assemblage into a hierarchical
;;       node model, the abstract syntax tree (AST), by a parser's
;;       adminiculum.
;;   
;;   (3) The actual accompassing of effect is assigned to the
;;       interpreter's bailiwick, the same traverses the abstract syntax
;;       tree (AST) and processes each encountered node.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-11-04
;; 
;; Sources:
;;   [esolang2022Turiipipip]
;;   The Esolang contributors, "Turi ip ip ip", July 14th, 2022
;;   URL: "https://esolangs.org/wiki/Turi_ip_ip_ip"
;; 
;;   [knowyourmeme2022TuriIpIpIp]
;;   The Know Your Meme contributors, "Turi Ip Ip Ip | Know Your Meme",
;;     June 10th, 2022
;;   URL: "https://knowyourmeme.com/memes/turi-ip-ip-ip"
;;   Notes:
;;     - Describes the meme "Turi Ip Ip Ip".
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tuple-of (&rest element-types)
  "The ``tuple-of'' type defines a n-tuple as a list whose cardinality
   equals the tally of ELEMENT-TYPES, each element at the i-th position
   in the tuple conforming to the i-th type specifier in the
   ELEMENT-TYPES."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (= (length (the list candidate))
               (length element-types))
            (loop
              for    element       of-type T in (the list candidate)
              and    expected-type of-type T in element-types
              always (typep element expected-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the generic sentinel ``*''."
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

(deftype option ()
  "The ``option'' type defines a structure or slot option in terms of
   either a symbol or a list of arbitrary composition."
  '(or symbol list))

;;; -------------------------------------------------------

(deftype slot-attributes ()
  "The ``slot-attributes'' type defines an encapsulation of a structure
   slots pertinent properties, which amplects the pair royal of its
   name, type, and accessor function, in terms of a 3-tuple, compact of
   the symbolic name, an arbitrary type specificier, and a symbol
   communicating the accessor function's agnomination."
  '(tuple-of symbol T symbol))

;;; -------------------------------------------------------

(deftype structure-slots ()
  "The ``structure-slots'' type defines a collection of a structure's
   slot information, comprehending the name, type, and accessor function
   supplied by the structure, as a list of zero or more
   ``slot-attributes'' objects."
  '(list-of slot-attributes))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
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

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of \"Turi ip ip ip\"
   identifier names to representative tokens, kithing in a hash table's
   mold, the keys of which comprehends the string identifier name, each
   affiliated with a ``Token'' equivalent."
  '(hash-table-of string Token))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list composed of zero or more
   ``AST-Node'' objects."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream NIL))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based stack compact of zero or more
   real-valued elements."
  '(list-of real))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "defstruct" analyzer operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-symbol (&rest name-components)
  "Generates a symbol composed of the NAME-COMPONENTS, each twissel of
   which is connected via a single hyphen (\"-\"), and the complete
   identifier converted into majuscules, concomitantly interning the
   thus produced symbol in the current package, ere returning it."
  (declare (type list name-components))
  (the symbol
    (intern (format NIL "~{~:@(~a~)~^-~}" name-components))))

;;; -------------------------------------------------------

(defun concatenate-into-symbol (&rest name-components)
  "Generates a symbol composed of the NAME-COMPONENTS, each connected
   without any advenient vinculum, and the complete identifier converted
   into majuscules, concomitantly interning the thus produced symbol in
   the current package, ere returning it."
  (declare (type list name-components))
  (the symbol
    (intern (format NIL "~{~:@(~a~)~}" name-components))))

;;; -------------------------------------------------------

(defun extract-structure-name (name-and-options)
  "Extracts from the ``defstruct''-comptabile structured type's
   NAME-AND-OPTIONS, either an aefauld symbol or a list of one or more
   elements, the incipient member of which bears the agnomination,
   the structure name."
  (declare (type (or symbol list) name-and-options))
  (the symbol
    (typecase name-and-options
      (symbol    name-and-options)
      (list      (first name-and-options))
      (otherwise (error "Invalid defstruct name and options: ~s."
                   name-and-options)))))

;;; -------------------------------------------------------

(defun search-defstruct-option (options searched-option)
  "Searches a ``defstruct'' structure option designated by the
   SEARCH-OPTION key (indicator) in the OPTIONS, which must constitute
   a property list, similar to the ``defstruct'' name-and-options
   compound when curtailed of its head, the structure name, and returns
   two values:
     (1) The value associated with the SEARCH-OPTION key, or, upon its
         absence, ``NIL''.
     (2) A ``boolean'' value which amounts to ``T'' if the
         SEARCHED-OPTION has been detected, otherwise ``NIL''. This
         flag homologates a distinguishment betwixt an actually present
         entry with the SEARCH-OPTION key and an explicitly stated
         ``NIL'' value from a missing SEARCH-OPTION, which would also
         return ``NIL'' as a default."
  (declare (type list    options))
  (declare (type keyword searched-option))
  (the (values (or null keyword) boolean)
    (loop
      for option-entry of-type T in options
      ;; Raw optional keyword symbol?
      if (eq option-entry searched-option)
        do (return (values NIL T))
      ;; Option as two-element list (key-symbol-name value)?
      else if (and (listp option-entry)
                   (eq (car option-entry) searched-option))
        do (return (values (second option-entry) T))
      ;; SEARCHED-OPTION not detected:
      finally
        (return (values NIL NIL)))))

;;; -------------------------------------------------------

(defun extract-structure-conc-name-prefix (name-and-options)
  "Extract and returns from the ``defstruct''-compatible
   NAME-AND-OPTIONS the slot accessor prefix, or ``conc-name'', as a
   symbol."
  (declare (type option name-and-options))
  (the symbol
    (typecase name-and-options
      ;; No ``defstruct'' options specified?
      ;; => Assume the structure name.
      (symbol
        (assemble-symbol name-and-options))
      ;; Any of
      ;;   :conc-name
      ;;   (:conc-name)
      ;;   (:conc-name conc-name-value)
      (list
        (multiple-value-bind (conc-name-value conc-name-found-p)
            (search-defstruct-option (rest name-and-options)
                                     :conc-name)
          (declare (type (or null keyword) conc-name-value))
          (declare (type boolean           conc-name-found-p))
          (cond
            ;; (:conc-name non-nil-value)
            ((and conc-name-found-p conc-name-value)
              (assemble-symbol conc-name-value))
            ;; (:conc-name NIL)
            ;; --or--
            ;; :conc-name
            ((and conc-name-found-p (null conc-name-value))
              (assemble-symbol ""))
            ;; No :conc-name option at all?
            (T
              (assemble-symbol (first name-and-options))))))
      (otherwise
        (error "Invalid defstruct name and options: ~s."
          name-and-options)))))

;;; -------------------------------------------------------

(defun extract-slot-name-and-type (slot-description)
  "Extracts from the ``defstruct''-compatible SLOT-DESCRIPTION, either
   an aefauld symbol or a list composed of one, two, four or six
   elements, the slot name and type information, and returns two values:
     (1) The mandatorily extant extracted slot name as a symbol.
     (2) The optional slot type, specified, if at all, by the ``:type''
         option, and defaulting upon its omission to the comprehensive
         ``T''."
  (declare (type option slot-description))
  (the (values symbol T)
    (typecase slot-description
      (symbol
        (values slot-description T))
      (list
        (values (first slot-description)
                (getf (cddr slot-description) :type T)))
      (otherwise
        (error "Invalid slot description: ~s." slot-description)))))

;;; -------------------------------------------------------

(defun extract-slot-information (slot-descriptions conc-name)
  "Returns from the SLOT-DESCRIPTIONS a list of ``slot-attributes''
   objects, comprehending each slot name, type, and accessor function
   name, the latter is derived from the CONC-NAME, as either supplied by
   the foundational ``defstruct'' declaration's ``:conc-name'' structure
   option or the structure name itself."
  (declare (type (list-of T) slot-descriptions))
  (declare (type symbol      conc-name))
  (the structure-slots
    (loop
      for slot-description of-type (list-of T) in slot-descriptions
      collect
        (multiple-value-bind (slot-name slot-type)
            (extract-slot-name-and-type slot-description)
          (declare (type symbol slot-name))
          (declare (type T      slot-type))
          (list slot-name slot-type
            (assemble-symbol conc-name slot-name))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Structure-Analyzer".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Structure-Analyzer
  (:constructor make-structure-analyzer
    (defstruct-declaration
     &aux
      (name-and-options  (first (cdr  defstruct-declaration)))
      (structure-name    (extract-structure-name name-and-options))
      (conc-name-prefix  (extract-structure-conc-name-prefix
                           name-and-options))
      (slot-descriptions (cddr defstruct-declaration))
      (documentation     (if (stringp (first slot-descriptions))
                           (pop slot-descriptions)
                           (format NIL "Implements the structure ~s."
                             structure-name)))
      (slot-information  (extract-slot-information
                           slot-descriptions conc-name-prefix)))))
  "The ``Structure-Analyzer'' class' responsibility is delineated by the
   analyzation of a ``defstruct'' in a pursuit to extract is pertinent
   components, such as the name, the ``conc-name'' structure option, an
   optional documentation string for the same, and its slot names and
   types in their entirety."
  (name-and-options  (error "Missing structure name and options.")
                     :type option)
  (slot-descriptions (error "Missing slot descriptions.")
                     :type list)
  (documentation     ""
                     :type string)
  (structure-name    (error "Missing structure name.")
                     :type symbol)
  (conc-name-prefix  (error "Missing conc-name-prefix.")
                     :type symbol)
  (slot-information  (error "Missing slot-information.")
                     :type structure-slots))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-bindings (structure-analyzer
                                       instance-name
                                       &optional (slot-prefix NIL))
  "Returns for the structure description entailed in the STRUCTURE-INFO
   a list of symbol macrolet name-value bindings for each of the
   structure's slots, the same enjoys the services of a structure
   instance nevened by the INSTANCE-NAME, with any such binding
   optionally prefixed via the SLOT-PREFIX, which, upon its omission or
   a ``NIL'' value, retains the verbatim slot name, the thus generated
   code being conformant with a local symbol macro's
   (``symbol-macrolet'') declarations."
  (declare (type Structure-Analyzer structure-analyzer))
  (declare (type symbol             instance-name))
  (declare (type (or null symbol)   slot-prefix))
  (the list
    (loop
      for slot-information
        of-type list
        in      (structure-analyzer-slot-information structure-analyzer)
      collect
        (destructuring-bind (slot-name slot-type slot-accessor)
            slot-information
          (declare (type symbol slot-name))
          (declare (type T      slot-type))
          (declare (type symbol slot-accessor))
          `(,(if slot-prefix
               (concatenate-into-symbol slot-prefix slot-name)
               slot-name)
             (the ,slot-type
               (,slot-accessor ,instance-name)))))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-declarations (structure-analyzer
                                           &optional (slot-prefix NIL))
  "Returns for the structure description entailed in the STRUCTURE-INFO
   a list of ``type'' and ``ignorable'' for each of the structure's
   slots, with any such binding optionally prefixed via the SLOT-PREFIX,
   which, upon its omission or a ``NIL'' value, retains the verbatim
   slot name, the thus generated code being conformant with a local
   symbol macro's (``symbol-macrolet'') declarations."
  (declare (type Structure-Analyzer structure-analyzer))
  (declare (type (or null symbol)   slot-prefix))
  (the list
    (loop
      for slot-information
        of-type list
        in      (structure-analyzer-slot-information structure-analyzer)
      append
        (destructuring-bind (slot-name slot-type slot-accessor)
            slot-information
          (declare (type symbol slot-name))
          (declare (type T      slot-type))
          (declare (type symbol slot-accessor))
          (declare (ignore      slot-accessor))
          (let ((symbol-macro-name
                  (if slot-prefix
                    (concatenate-into-symbol slot-prefix slot-name)
                    slot-name)))
            (declare (type symbol symbol-macro-name))
            `((declare (type ,slot-type ,symbol-macro-name))
              (declare (ignorable       ,symbol-macro-name))))))))

;;; -------------------------------------------------------

(defun build-with-macro-name (structure-analyzer)
  "Returns a symbol representing the name of the \"with-\" macro that
   grants convenient access to the structure referred to by the
   STRUCTURE-INFO."
  (declare (type Structure-Analyzer structure-analyzer))
  (the symbol
    (assemble-symbol "WITH"
      (structure-analyzer-structure-name structure-analyzer))))

;;; -------------------------------------------------------

(defmacro define-enhanced-structure (defstruct-declaration)
  "Analyzes a ``defstruct'' declaration, extracts its pertinent
   components, evaluates the declaration to a structure, and adds a
   ``with-'' macro for more convenient access to the structure's slots,
   returning the latter."
  (let* ((structure-analyzer
          (make-structure-analyzer defstruct-declaration))
         (structure-name
          (structure-analyzer-structure-name structure-analyzer))
         (with-macro-name
          (build-with-macro-name structure-analyzer)))
    (declare (type Structure-Analyzer structure-analyzer))
    (declare (type symbol             structure-name))
    (declare (type symbol             with-macro-name))
    `(progn
       (defstruct ,(structure-analyzer-name-and-options
                     structure-analyzer)
         
         ;; Insert the contingent documentation string.
         ,(structure-analyzer-documentation structure-analyzer)
         
         ;; Insert the slots.
         ,@(structure-analyzer-slot-descriptions structure-analyzer))
       
       (defmacro ,with-macro-name
           ((subject
             &optional (subject-name ',structure-name)
                       (slot-prefix  (concatenate-into-symbol
                                       subject-name
                                       "-")))
             &body body)
         "Establishes a convenience macro for accessing the slots of the
          structure SUBJECT, the same itself agnominated by the
          SUBJECT-NAME, which defaults to the STRUCTURE identifier
          itself, the slots of which are prefixed by the SLOT-PREFIX,
          upon omission deriving from the SUBJECT-NAME, while a ``NIL''
          value prevents any prefixion in favor the verbatim slot
          names.
          ---
          A trifurcation of configurations governs the possibilities for
          the ultimate name of a slot {slot} in dependency upon the
          SLOT-PREFIX and, potential, SUBJECT-NAME:
            -------------------------------------------------------
            SLOT-PREFIX | Symbol macro binding name for slot {slot}
            ------------+------------------------------------------
            specified   | {SLOT-PREFIX}-{slot}
            .......................................................
            NIL         | {slot}
            .......................................................
            omitted     | {SUBJECT-NAME}-{slot}
            -------------------------------------------------------"
         `(let ((,subject-name ,subject))
            (declare (type ,(quote ,structure-name) ,subject-name))
            (declare (ignorable                     ,subject-name))
            (symbol-macrolet
                (,@(build-symbol-macrolet-bindings
                     ,structure-analyzer
                     subject-name
                     `,slot-prefix))
              ,@(build-symbol-macrolet-declarations
                  ,structure-analyzer
                  `,slot-prefix)
              ,@body))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-enhanced-structure
  (defstruct (Token
    (:constructor make-token (type value)))
    "The ``Token'' class serves in the encapsulation of all information
     requisite for a significant object's replication, obtained during
     the lexical analyzation of a piece of \"Turi ip ip ip\" source
     code."
    (type  (error "Missing type.")  :type keyword :read-only T)
    (value (error "Missing value.") :type T       :read-only T)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (with-token (token $this my->)
    (the boolean
      (not (null
        (eq my->type expected-type))))))

;;; -------------------------------------------------------

(defun instruction-token-p (candidate)
  "Determines whether the CANDIDATE represents a token associated with
   an instruction, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (null (null
      (or (token-type-p candidate :eugh)
          (token-type-p candidate :turi))))))

;;; -------------------------------------------------------

(defun separator-token-p (candidate)
  "Determines whether the CANDIDATE represents an instruction separator,
   that is, a token ensconcing the newline (\"\\n\") or semicolon
   (\";\") character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Token candidate))
  (the boolean
    (not (null
      (or (token-type-p candidate :semicolon)
          (token-type-p candidate :newline))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Ordained to the castaldy of the \"Turi ip ip ip\" identifers strings
   and their token representations.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token-type)
        "Associates the NAME identifier NAME with a freshly created
         token bearing the TOKEN-TYPE and the NAME as its value, and
         returns no value."
        (declare (type string  name))
        (declare (type keyword token-type))
        (setf (gethash name +IDENTIFIERS+)
              (make-token token-type name))
        (values)))
  (declare (ftype (function (string keyword) (values))
                  register-identifier))
  (register-identifier "eugh"    :eugh)
  (register-identifier "ip"      :ip)
  (register-identifier "tura"    :tura)
  (register-identifier "turi"    :turi)
  (register-identifier "turisha" :turisha)
  (values))

;;; -------------------------------------------------------

(defun get-identifier (name)
  "Returns the token representing the identifier with the NAME, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type string name))
  (the Token
    (multiple-value-bind (identifier-token contains-name-p)
        (gethash name +IDENTIFIERS+)
      (declare (type (or null Token) identifier-token))
      (declare (type T               contains-name-p))
      (if contains-name-p
        identifier-token
        (error "Invalid identifier: ~s." name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a space character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Space)
          (char= candidate #\Tab))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-enhanced-structure
  (defstruct (Lexer
    (:constructor make-lexer
      (source
       &aux (position  0)
            (character (when (array-in-bounds-p source position)
                         (char source position))))))
    "The ``Lexer'' class furnishes a lexical analyzer, the wike of which
     is delineated by the extraction and delivery of tokens from a piece
     of \"Turi ip ip ip\" source code."
    (source    (error "Missing source.")
               :type      string
               :read-only T)
    (position  (error "Missing position.")
               :type      fixnum)
    (character (error "Missing character.")
               :type      (or null character))))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer $this)
    (setf $this-character
      (when (array-in-bounds-p $this-source (1+ $this-position))
        (char $this-source
          (incf $this-position)))))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a sequence of zero or more accolent spaces and returns no value."
  (with-lexer (lexer $this NIL)
    (loop while (and character (space-character-p character)) do
      (advance-lexer lexer)))
  (values))

;;; -------------------------------------------------------

(defun skip-comment (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   a comment block and returns no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer $this NIL)
    (loop do
      (case character
        ((NIL)
          (error "Unterminated comment section at position ~d."
            position))
        (#\]
          (advance-lexer $this)
          (loop-finish))
        (otherwise
          (advance-lexer $this)))))
  (values))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source,
   consumes an identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier
      (with-output-to-string (name)
        (declare (type string-stream name))
        (with-lexer (lexer this its-)
          (loop
            while (and its-character (alphanumericp its-character))
            do
              (write-char its-character name)
              (advance-lexer this)))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer lexer NIL)
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (skip-spaces    lexer)
          (get-next-token lexer))
        
        ((char= character #\[)
          (skip-comment   lexer)
          (get-next-token lexer))
        
        ((char= character #\,)
          (prog1
            (make-token :comma character)
            (advance-lexer lexer)))
        
        ((char= character #\;)
          (prog1
            (make-token :semicolon character)
            (advance-lexer lexer)))
        
        ((char= character #\Newline)
          (prog1
            (make-token :newline character)
            (advance-lexer lexer)))
        
        ((alphanumericp character)
          (read-identifier lexer))
        
        (T
          (error "Invalid character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract syntax tree (AST) nodes.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct AST-Node
  "The ``AST-Node'' class furnishes a common foundry for all classes
   pursuing the modeling of \"Turi ip ip ip\" language facilities in
   the form of abstract syntax tree (AST) nodes.")

;;; -------------------------------------------------------

(defstruct (Program-Node
  (:include     AST-Node)
  (:constructor make-program-node (statements)))
  "The ``Program-Node'' class contributes the root level of the
   abstract syntax tree (AST) representation of a parsed
   \"Turi ip ip ip\" program, encapsulating the program's statements."
  (statements (error "Missing statements.") :type node-list))

;;; -------------------------------------------------------

(defstruct (Number-Node
  (:include     AST-Node)
  (:constructor make-number-node (value)))
  "The ``Number-Node'' class encapsulates an integer or floating-point
   number literal."
  (value (error "Missing value.") :type real))

#|
;;; -------------------------------------------------------

(define-enhanced-structure
  (defstruct (NOP-Node
    (:include     AST-Node)
    (:constructor make-nop-node ()))
    "The ``NOP-Node'' class serves in the representation of a
     no-operation, or NOP, node, also, in some provenances, norned an
     \"empty node\"."))
|#

;;; -------------------------------------------------------

(defstruct (Operation-Node
  (:include     AST-Node)
  (:constructor make-operation-node (operation-key)))
  "The ``Operation-Node'' furnishes a representation of the
   \"Turi ip ip ip\" command \"eugh {integer}\", responsible for the
   furnishment of a miscellany of competences."
  (operation-key (error "Missing operation key.") :type Number-Node))

;;; -------------------------------------------------------

(defstruct (Push-Node
  (:include     AST-Node)
  (:constructor make-push-node (value)))
  "The ``Push-Node'' serves in the encapsulation of a
   \"Turi ip ip ip\" \"push\" command."
  (value (error "Missing value.") :type Number-Node))

;;; -------------------------------------------------------

(defstruct (While-Loop-Node
  (:include     AST-Node)
  (:constructor make-while-loop-node (statements)))
  "The ``While-Loop-Node'' class establishes a \"while\"-based
   iteration construct, corresponding to the \"Turi ip ip ip\"
   facility
     eugh eugh
       {body}
     turisha"
  (statements (error "Missing statements.") :type node-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) node-list)
                parse-instruction-sequence))

;;; -------------------------------------------------------

(define-enhanced-structure
  (defstruct (Parser
    (:constructor make-parser
      (lexer
       &aux (current-token (get-next-token lexer))
            (next-token    (get-next-token lexer)))))
    "The ``Parser'' class appropriates the wike of assembling from a
     series of tokens an abstract syntax tree (AST) representation of a
     \"Turi ip ip ip\" program."
    (lexer         (error "Missing lexer.")         :type Lexer)
    (current-token (error "Missing current token.") :type Token)
    (next-token    (error "Missing next token.")    :type Token)))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly querying the next from the underlying lexer and
   storing it in the PARSER; upon a mismatch an error of an unspecified
   type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (the Token
    (with-parser (parser this my-)
      (prog1 my-current-token
        (if (token-type-p my-current-token expected-token-type)
          (shiftf my-current-token my-next-token
            (get-next-token my-lexer))
          (error "Expected a token of the type ~s, but encountered ~s."
            expected-token-type my-current-token))))))

;;; -------------------------------------------------------

(defun parse-digit (parser)
  "Parses a sequence of one to inclusive ten \"ip\" tokens utilizing the
   PARSER, assembles (decodes) the decimal digit from the integral range
   [0, 9], and returns the same."
  (declare (type Parser parser))
  (the (integer 0 9)
    (with-parser (parser this NIL)
      (loop
        while (token-type-p current-token :ip)
        do    (eat-token parser :ip)
        count 1
        into  digit-value
        finally
          (return
            (cond
              ((zerop digit-value)
                (error "Expected a sequence of one or more \"ip\", ~
                        but encountered none."))
              ((> digit-value 10)
                (error "Expected a sequence of one to inclusive ten ~
                        \"ip\", but encountered ~d."
                  digit-value))
              (T
                (1- digit-value))))))))

;;; -------------------------------------------------------

(defun parse-digit-sequence (parser destination)
  "Parses a sequence of one or more digits utilizing the PARSER, writes
   these as characters to the DESTIANTION, and returns no value."
  (declare (type Parser      parser))
  (declare (type destination destination))
  (flet ((collect-digit ()
          "Parses a single decimal digit, writes its character form to
           the DESTINATION, and returns no value."
          (write-char
            (digit-char
              (parse-digit parser))
            destination)
          (values)))
    ;; Collect the first digit.
    (collect-digit)
    ;; Collect zero or more subsequent digits, separated by commas.
    (with-parser (parser this NIL)
      (loop
        while (and (token-type-p current-token :comma)
                   (token-type-p next-token    :ip))
        do
          (eat-token parser :comma)
          (collect-digit))))
  (values))

;;; -------------------------------------------------------

(defun parse-number (parser)
  "Parses an integer or floating-point number utilizing the PARSER and
   returns a ``Number-Node'' representation thereof."
  (declare (type Parser parser))
  (the Number-Node
    (with-parser (parser this NIL)
      (make-number-node
        (read-from-string
          (with-output-to-string (number)
            (declare (type string-stream number))
            ;; Parse mandatory integral part.
            (when (token-type-p current-token :tura)
              (eat-token parser :tura)
              (write-char #\- number))
            (parse-digit-sequence parser number)
            ;; Parse optional fractional part.
            (when (and (token-type-p current-token :comma)
                       (token-type-p next-token    :tura))
              (eat-token parser :comma)
              (eat-token parser :tura)
              (write-char #\. number)
              (parse-digit-sequence parser number))))))))

;;; -------------------------------------------------------

(defun expect-end-of-instruction (parser)
  "Determines whether the PARSER's current token represents one eligible
   for the termination of an instruction, for a non-end-of-file token
   consuming the same, in any affirmative case returning no value,
   otherwise responding with an error of an unspecified type."
  (declare (type Parser parser))
  (with-parser (parser $this)
    (cond
      ((token-type-p $this-current-token :eof)
        NIL)
      ((separator-token-p $this-current-token)
        (eat-token parser
          (token-type $this-current-token)))
      (T
        (error "Expected an instruction terminator, but encountered ~s."
          $this-current-token))))
  (values))

;;; -------------------------------------------------------

(defun skip-separator-tokens (parser)
  "Parses a sequence of zero or more accolent separator tokens using the
   PARSER and returns no value."
  (declare (type Parser parser))
  (with-parser (parser this NIL)
    (loop while (separator-token-p current-token) do
      (eat-token parser
        (token-type current-token))))
  (values))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Parses a \"Turi ip ip ip\" instruction utilizing the PARSER and
   produces a ``Node'' representation thereof."
  (declare (type Parser parser))
  (the AST-Node
    (with-parser (parser this NIL)
      (case (token-type current-token)
        ;; Either a "while" loop or a general operation.
        (:eugh
          (eat-token parser :eugh)
          (case (token-type current-token)
            ;; "eugh "eugh"? => While loop.
            (:eugh
              (eat-token parser :eugh)
              (expect-end-of-instruction parser)
              (prog1
                (make-while-loop-node
                  (parse-instruction-sequence parser))
                (eat-token parser :turisha)))
            
            ;; "eugh {number}"? => General operation.
            ((:ip :tura)
              (make-operation-node
                (parse-number parser)))
            
            (otherwise
              (error "Unexpected token following \"eugh\": ~s."
                current-token))))
        
        ;; "Push" instruction.
        (:turi
          (eat-token parser :turi)
          (make-push-node
            (parse-number parser)))
        
        (otherwise
          (error "Invalid command token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parse-instruction-sequence (parser)
  "Parses a sequence of zero or more \"Turi ip ip ip\" instructions
   utilizing the PARSER and returns these in a list."
  (declare (type Parser parser))
  (the node-list
    (with-parser (parser this NIL)
      (let ((instructions NIL))
        (declare (type node-list instructions))
        (skip-separator-tokens parser)
        (loop while (instruction-token-p current-token) do
          (push (parse-instruction parser) instructions)
          (cond
            ;; No more tokens?
            ((token-type-p current-token :eof)
              (loop-finish))
            ;; No separator betwixt the previous and this instruction?
            ((instruction-token-p current-token)
              (error "Missing separator betwixt two commands."))
            ;; Separator succeeding the previous command?
            ((separator-token-p current-token)
              (skip-separator-tokens parser)
              (unless (instruction-token-p current-token)
                (loop-finish)))
            ;; Any other token?
            (T
              (loop-finish))))
        (nreverse instructions)))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a \"Turi ip ip ip\" program utilizing the PARSER and produces
   a ``Program-Node'' representation thereof."
  (declare (type Parser parser))
  (skip-separator-tokens parser)
  (the Program-Node
    (prog1
      (make-program-node
        (parse-instruction-sequence parser))
      (skip-separator-tokens parser)
      (eat-token parser :eof))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape cell.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell (previous next)))
  "The ``Cell'' class implements a tape cell as a doubly linked node,
   compatible with the requisites of a doubly linked list."
  (stack    NIL :type stack)
  (previous NIL :type (or null Cell))
  (next     NIL :type (or null Cell)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-tape
    (&aux (first-cell
            (make-cell NIL NIL))
          (header
            (make-cell NIL first-cell))
          (trailer
            (let ((trailer-sentinel (make-cell first-cell NIL)))
              (declare (type Cell trailer-sentinel))
              (setf (cell-previous first-cell) header)
              (setf (cell-next     first-cell) trailer-sentinel)
              (the cell trailer-sentinel))))))
  "In the ``Tape'' class a linear collection of cells, each latreutical
   as stack's salvory, kythes, realized as a doubly linked list of
   ``Cell'' instances."
  (header  (error "Missing header.")  :type Cell)
  (trailer (error "Missing trailer.") :type Cell))

;;; -------------------------------------------------------

(defun tape-first-cell (tape)
  "Returns the first cell participating in the TAPE."
  (declare (type Tape tape))
  (the Cell
    (cell-next
      (tape-header tape))))

;;; -------------------------------------------------------

(defun tape-insert-betwixt (tape predecessor successor)
  "Creates a new ``Cell'' in the TAPE, inserts it betwixt the
   PREDECESSOR and SUCCESSOR cells, and returns the thus accommodated
   cell."
  (declare (type Tape tape))
  (declare (ignore    tape))
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (let ((new-cell (make-cell predecessor successor)))
    (declare (type Cell new-cell))
    (setf (cell-next     predecessor) new-cell)
    (setf (cell-previous successor)   new-cell)
    (the Cell new-cell)))

;;; -------------------------------------------------------

(defun tape-prepend-cell (tape)
  "Adds a new cell to the TAPE's left bourne and returns the thus
   accommodated cell."
  (declare (type Tape tape))
  (the Cell
    (tape-insert-betwixt tape
      (tape-header tape)
      (cell-next (tape-header tape)))))

;;; -------------------------------------------------------

(defun tape-append-cell (tape)
  "Adds a new cell to the TAPE's right bourne and the thus
   accommodated cell."
  (declare (type Tape tape))
  (the Cell
    (tape-insert-betwixt tape
      (cell-previous (tape-trailer tape))
      (tape-trailer tape))))

;;; -------------------------------------------------------

(defun tape-previous-cell (tape current-cell)
  "Returns the cell in the TAPE preceding the CURRENT-CELL, or inserts
   and returns a new one if such is hitherto absent."
  (declare (type Tape tape))
  (declare (type Cell current-cell))
  (the Cell
    (if (eq (cell-previous current-cell)
            (tape-header tape))
      (tape-prepend-cell tape)
      (cell-previous current-cell))))

;;; -------------------------------------------------------

(defun tape-next-cell (tape current-cell)
  "Returns the cell in the TAPE succeeding the CURRENT-CELL, or
   inserts and returns a new one if such is hitherto absent."
  (declare (type Tape tape))
  (declare (type Cell current-cell))
  (the Cell
    (if (eq (cell-next current-cell)
            (tape-trailer tape))
      (tape-append-cell tape)
      (cell-next current-cell))))

;;; -------------------------------------------------------

(defmethod print-object ((tape Tape) stream)
  (declare (type Tape        tape))
  (declare (type destination stream))
  (format stream "(Tape [")
  (loop
    for current-cell
      of-type Cell
      =       (cell-next (tape-header tape))
      then    (cell-next current-cell)
    and first-cell-p
      of-type boolean
      =       T
      then    NIL
    until
      (eq current-cell (tape-trailer tape))
    do
      (unless first-cell-p
        (format stream ", "))
      (format stream "~a"
        (cell-stack current-cell)))
  (format stream "])"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory (&aux (tape    (make-tape))
                                  (pointer (tape-first-cell tape)))))
  "The ``Memory'' class serves in the castaldy of the program memory by
   its purview over the tape stacks, a pointer into the currently active
   member thereof, and a \"backup stack\" as an adminicular warkloom."
  (tape         (error "Missing tape.")    :type Tape)
  (pointer      (error "Missing pointer.") :type Cell)
  (backup-stack NIL                        :type stack))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY cell located at the cell pointer."
  (declare (type Memory memory))
  (the Cell
    (memory-pointer memory)))

;;; -------------------------------------------------------

(defun memory-current-stack (memory)
  "Returns the MEMORY stack located at the cell pointer."
  (declare (type Memory memory))
  (the stack
    (cell-stack
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun (setf memory-current-stack) (new-stack memory)
  "Sets the MEMORY stack located at the cell pointer to the NEW-STACK and
   returns no value."
  (declare (type Memory memory))
  (setf (cell-stack (memory-pointer memory))
        new-stack)
  (values))

;;; -------------------------------------------------------

(defun memory-move-to-previous-stack (memory)
  "Translates the MEMORY's cell pointer to the preceding stack and returns
   no value."
  (declare (type Memory memory))
  (setf (memory-pointer memory)
    (tape-previous-cell
      (memory-tape    memory)
      (memory-pointer memory)))
  (values))

;;; -------------------------------------------------------

(defun memory-move-to-next-stack (memory)
  "Translates the MEMORY's cell pointer to the succeeding stack and
   returns no value."
  (declare (type Memory memory))
  (setf (memory-pointer memory)
    (tape-next-cell
      (memory-tape    memory)
      (memory-pointer memory)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Turi-ip-ip-ip-Error (error)
  ()
  (:documentation
    "The ``Turi-ip-ip-ip-Error'' condition type furnishes a substrate
     for all conditions pursuing to accommodate the communication of
     anomalous situations in respect to a \"Turi ip ip ip\" program's
     lexical analyzation, parsing, or interpretation."))

;;; -------------------------------------------------------

(define-condition Invalid-Operation-Key-Error (Turi-ip-ip-ip-Error)
  ((offending-key
    :initarg       :offending-key
    :initform      (error "Missing offending key.")
    :reader        invalid-operation-key-error-offending-key
    :type          real
    :documentation "The key not affiliated with any action in the
                    \"eugh\" instruction."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Operation-Key-Error condition))
      (declare (type destination                 stream))
      (format stream "The value ~a does not designate a valid \"eugh\" ~
                      operation key."
        (invalid-operation-key-error-offending-key condition))))
  (:documentation
    "The ``Invalid-Operation-Key-Error'' condition type serves in the
     apprizal about an invalid numeric key's consignment to the \"eugh\"
     instruction."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Turi-ip-ip-ip-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :type          stack
    :documentation "The stack which was, at the instant of the invalid
                    operation, empty."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (ignore                 condition))
      (declare (type destination       stream))
      (format stream "Cannot peek or pop from an empty stack.")))
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the
     communication of an anomalous situation instigated by the attempt
     to request or remove an element from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Interpreter" class.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-enhanced-structure
  (defstruct (Interpreter
    (:constructor make-interpreter (tree)))
    "The ``Interpreter'' class is assigned the wike of accompassing
     actual effect to a \"Turi ip ip ip\" program submitted as an
     abstract syntax tree (AST)."
    (tree   (error "Missing AST.") :type Program-Node)
    (memory (make-memory)          :type Memory)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of foundational interpreter operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-stack (interpreter)
  "Returns the INTERPRETER's currently active stack."
  (declare (type Interpreter interpreter))
  (the stack
    (memory-current-stack
      (interpreter-memory interpreter))))

;;; -------------------------------------------------------

(defun (setf current-stack) (new-stack interpreter)
  "Substitutes the INTERPRETER's currently active stack by the
   NEW-STACK and returns no value."
  (declare (type stack       new-stack))
  (declare (type Interpreter interpreter))
  (setf (memory-current-stack
          (interpreter-memory interpreter))
    new-stack)
  (values))

;;; -------------------------------------------------------

(defun push-to-current-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's current stack and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type real        new-value))
  (push new-value (current-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-current-stack (interpreter)
  "Removes and returns the top element located on the INTERPRETER's
   current stack."
  (declare (type Interpreter interpreter))
  (the real
    (or (pop (current-stack interpreter))
        (error 'Empty-Stack-Error :offended-stack
          (current-stack interpreter)))))

;;; -------------------------------------------------------

(defun peek-current-stack (interpreter)
  "Returns without removing the top element located on the INTERPRETER's
   current stack."
  (declare (type Interpreter interpreter))
  (the real
    (or (first (current-stack interpreter))
        (error 'Empty-Stack-Error :offended-stack
          (current-stack interpreter)))))

;;; -------------------------------------------------------

(defun push-to-backup-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's backup stack and returns
   no value."
  (declare (type Interpreter interpreter))
  (declare (type real        new-value))
  (push new-value
    (memory-backup-stack
      (interpreter-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defun pop-from-backup-stack (interpreter)
  "Removes and returns the top element located at the INTERPRETER's
   backup stack."
  (the real
    (or (pop (memory-backup-stack
               (interpreter-memory interpreter)))
        (error 'Empty-Stack-Error :offended-stack
          (memory-backup-stack
            (interpreter-memory interpreter))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "eugh" operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric execute-operation (interpreter operation-key)
  (:documentation
    "Executes the \"eugh\" operation amenable to the OPERATION-KEY in
     the INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defmacro define-operation (operation-key (interpreter-variable)
                            &body body)
  "Furnishes a commodity for the definition of an implementation of the
   generic function ``execute-operation'', employing for the first
   parameter name the INTERPRETER-VARIABLE, while automatically
   generating a name for the second parameter, upon which the method
   dispatched by an ``eql''-equality to the evaluated OPERATION-KEY,
   filling the method with the BODY forms, and concluding the same with
   no values to return."
  (let ((operation-key-name (gensym)))
    (declare (type symbol operation-key-name))
    `(defmethod execute-operation
         ((,interpreter-variable Interpreter)
          (,operation-key-name   (eql ,operation-key)))
       (declare (type Interpreter interpreter))
       (declare (ignorable        interpreter))
       (declare (type real        ,operation-key-name))
       (declare (ignore           ,operation-key-name))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-operation 1 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the sum (b + a), pushes this result unto the stack, and
   returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (+ second-popped-element
         first-popped-element))))

;;; -------------------------------------------------------

(define-operation 2 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the difference (b - a), pushes this result unto the stack,
   and returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (- second-popped-element
         first-popped-element))))

;;; -------------------------------------------------------

(define-operation 3 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the product (b * a), pushes this result unto the stack,
   and returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (* second-popped-element
         first-popped-element))))

;;; -------------------------------------------------------

(define-operation 4 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the quotient (b / a), pushes this result unto the stack,
   and returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (float
        (/ second-popped-element
           first-popped-element)))))

;;; -------------------------------------------------------

(define-operation 5 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   negates its value to -a, pushes the result unto the same stack, and
   returns no value."
  (push-to-current-stack interpreter
    (- (pop-from-current-stack interpreter))))

;;; -------------------------------------------------------

(define-operation 10 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the remainder (b mod a), pushes this result unto the
   stack, and returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (mod second-popped-element
           first-popped-element))))

;;; -------------------------------------------------------

(define-operation 20 (interpreter)
  "Pops the first element, \"a\", from the INTERPRETER's current stack,
   subsequently pops the new top element, \"b\", from the same stack,
   calculates the power (b ^ a), pushes this result unto the stack, and
   returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter
      (expt second-popped-element
            first-popped-element))))

;;; -------------------------------------------------------

(define-operation 30 (interpreter)
  "Pops the top element from the INTERPRETER's current stack, print its
   verbatim numeric form to the standard output, and returns no value."
  (format T "~&~d"
    (pop-from-current-stack interpreter)))

;;; -------------------------------------------------------

(define-operation 40 (interpreter)
  "Pops the top element from the INTERPRETER's current stack,
   transcripts it into the character whose ASCII code matches its value,
   prints this character to the standard output,  and returns no value."
  (write-char
    (code-char
      (pop-from-current-stack interpreter))))

;;; -------------------------------------------------------

(define-operation 50 (interpreter)
  "Queries the user for a line of input, pushes the response's
   characters, proceeding from right to left, unto the stack --- thus
   retaining the input's actual order if reading the stack from top to
   bottom ---, and returns no value.
   ---
   A contrieved forbisen shall be the latreutical agent in the
   principle's illustration: Confronted with a user response of
     abc
   the prial of characters is pushed in this order:
     (1) c
     (2) b
     (3) a
   and thus, following stack, if hitherto vacant, is produced, limining
   the layout in its verbatim top-to-bottom format:
     | a |
     | b |
     | c |
   while, if transcripted into the ASCII character codes, amounts to:
     | 97 |
     | 98 |
     | 99 |"
  (format T "~&>> ")
  (finish-output)
  (let ((input (read-line)))
    (declare (type string input))
    (clear-input)
    (loop for character of-type character across (nreverse input) do
      (push-to-current-stack interpreter
        (char-code character)))))

;;; -------------------------------------------------------

(define-operation 100 (interpreter)
  "Duplicates the top stack elements on the INTERPRETER's current stack
   and returns no value."
  (push-to-current-stack interpreter
    (peek-current-stack interpreter)))

;;; -------------------------------------------------------

(define-operation 200 (interpreter)
  "Swaps the positions of the current stack's two top elements and
   returns no value."
  (let ((first-popped-element  (pop-from-current-stack interpreter))
        (second-popped-element (pop-from-current-stack interpreter)))
    (declare (type real first-popped-element))
    (declare (type real second-popped-element))
    (push-to-current-stack interpreter first-popped-element)
    (push-to-current-stack interpreter second-popped-element)))

;;; -------------------------------------------------------

(define-operation 300 (interpreter)
  "Converts those elements of the INTERPRETER's current stack that
   correspond to the ASCII codes of decimal digit characters into the
   same, concatenates these digits into integers, substitutes the
   converted items by the thus assembled object, and returns no value."
  (let ((digit-buffer (make-array 0
                        :element-type    'character
                        :initial-element #\Null
                        :adjustable      T
                        :fill-pointer    0)))
    (declare (type string digit-buffer))
    
    (flet ((encodes-decimal-digit-p (stack-element)
            "Determines whether the STACK-ELEMENT represents the ASCII
             code of a decimal digit character, returning on
             confirmation a ``boolean'' value of ``T'', otherwise
             ``NIL''."
            (declare (type real stack-element))
            (the boolean
              (not (null
                (and (integerp stack-element)
                     (<= 48 stack-element 57))))))
           
           (collect-digit (ascii-code)
            "Converts the ASCII-CODE into its character form, appends
             the thus represented decimal digit to the DIGIT-BUFFER, and
             returns no value."
            (declare (type fixnum ascii-code))
            (format digit-buffer "~c"
              (code-char ascii-code))
            (values))
           
           (digit-buffer-empty-p ()
            "Determines whether the DIGIT-BUFFER is empty, returning on
             confirmation a ``boolean'' value of ``T'', otherwise
             ``NIL''."
            (the boolean
              (not (null
                (zerop (fill-pointer digit-buffer))))))
           
           (parse-digit-buffer ()
            "Parses the DIGIT-BUFFER's contents as an unsigned integer,
             resets its fill pointer to the zero (0) location, and
             returns the parsed integer value."
            (the (integer 0 *)
              (prog1
                (parse-integer digit-buffer)
                (setf (fill-pointer digit-buffer) 0)))))
      
      (loop
        for element of-type real in (current-stack interpreter)
        
        ;; The ELEMENT is a digit's ASCII code?
        ;; => Collect its decimal digit form in the DIGIT-BUFFER.
        if (encodes-decimal-digit-p element)
          do (collect-digit element)
        ;; The ELEMENT is not a digit's ASCII code, but the
        ;; DIGIT-BUFFER contains content?
        ;; => Parse and collect the DIGIT-BUFFER's integer value,
        ;;    and reset the buffer.
        ;; => Collect the ELEMENT in its verbatim form.
        else if (not (digit-buffer-empty-p))
          collect (parse-digit-buffer) into new-stack
          and collect element          into new-stack
        ;; The ELEMENT is not a digit's ASCII code, and the
        ;; DIGIT-BUFFER is empty?
        ;; => Simply collect the ELEMENT in its verbatim form.
        else
          collect element into new-stack
        
        finally
          ;; If the DIGIT-BUFFER contains digits, parse and collect
          ;; them.
          (unless (digit-buffer-empty-p)
            (setf new-stack
              (nconc new-stack
                (list (parse-digit-buffer)))))
          (setf (current-stack interpreter)
                new-stack)))))

;;; -------------------------------------------------------

(define-operation 400 (interpreter)
  "Moves the stack pointer maintained by INTERPRETER's memory to the
   next stack and returns no value."
  (memory-move-to-next-stack
    (interpreter-memory interpreter)))

;;; -------------------------------------------------------

(define-operation 1000 (interpreter)
  "Moves the stack pointer maintained by INTERPRETER's memory to the
   previous stack and returns no value."
  (memory-move-to-previous-stack
    (interpreter-memory interpreter)))

;;; -------------------------------------------------------

(define-operation 2000 (interpreter)
  "Pops the top element of the INTERPRETER's current stack, pushes it
   unto the backup stack, and returns no value."
  (push-to-backup-stack interpreter
    (pop-from-current-stack interpreter)))

;;; -------------------------------------------------------

(define-operation 3000 (interpreter)
  "Pops the top element of the INTERPRETER's backup stack, pushes the
   same unto the current stack, and returns no value."
  (push-to-current-stack interpreter
    (pop-from-backup-stack interpreter)))

;;; -------------------------------------------------------

(defmethod execute-operation ((interpreter   Interpreter)
                              (operation-key T))
  "Furnishes a default case for an undefined or invalid OPERATION-KEY in
   ignoring the INTERPRETER and signaling an error of the type
   ``Invalid-Operation-Key-Error'' which communicates the OPERATION-KEY
   as its culprit."
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type T           operation-key))
  (error 'Invalid-Operation-Key-Error :offending-key operation-key))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter operatins.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric visit-node (interpreter node)
  (:documentation
    "Traverses the NODE in the INTERPRETER's context and returns a value
     apropos for this combination."))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Program-Node))
  (declare (type Interpreter  interpreter))
  (declare (type Program-Node node))
  (loop
    for instruction of-type AST-Node in (program-node-statements node)
    do  (visit-node interpreter instruction))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Number-Node))
  (declare (type Interpreter interpreter))
  (declare (ignore           interpreter))
  (declare (type Number-Node node))
  (the real (number-node-value node)))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Operation-Node))
  (declare (type Interpreter    interpreter))
  (declare (type Operation-Node node))
  (execute-operation interpreter
    (visit-node interpreter
      (operation-node-operation-key node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        Push-Node))
  (declare (type Interpreter interpreter))
  (declare (type Push-Node   node))
  (push-to-current-stack interpreter
    (visit-node interpreter
      (push-node-value node)))
  (values))

;;; -------------------------------------------------------

(defmethod visit-node ((interpreter Interpreter)
                       (node        While-Loop-Node))
  (declare (type Interpreter     interpreter))
  (declare (type While-Loop-Node node))
  (loop while (not (zerop (peek-current-stack interpreter))) do
    (dolist (statement (while-loop-node-statements node))
      (declare (type AST-Node statement))
      (visit-node interpreter statement)))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates and executes the program stored in the INTERPRETER in the
   form of an abstract syntax tree and returns no value."
  (declare (type Interpreter interpreter))
  (visit-node interpreter
    (interpreter-tree interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Turi-ip-ip-ip (code)
  "Interprets the piece of \"Turi ip ip ip\" source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Turi-ip-ip-ip
  "turi ip ip ip ip ip ip ip ip, ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip, ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip, ip ip ip ip ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip, ip ip ip ip ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip ip, ip ip; eugh ip ip ip ip ip, ip
   turi ip ip ip ip ip, ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip ip ip, ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip ip ip ip ip ip ip ip, ip ip ip ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip ip, ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip ip, ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip, ip ip ip ip ip ip ip ip ip; eugh ip ip ip ip ip, ip
   turi ip ip, ip, ip; eugh ip ip ip ip ip, ip
   turi ip ip ip ip, ip ip ip ip; eugh ip ip ip ip ip, ip")

;;; -------------------------------------------------------

;; One-time cat program which expects a single character.
(interpret-Turi-ip-ip-ip
  "eugh ip ip ip ip ip ip, ip
   eugh ip ip ip ip ip, ip")

;;; -------------------------------------------------------

;; Line-based Repeating cat program which terminates on a user input of
;; the "null character" or a refusal to input.
(interpret-Turi-ip-ip-ip
  "[Push 0 for both repeatingly printing the input line inwith the     ]
   [inner 'eugh eugh' loop, and ascertaining that, for an empty input, ]
   [the outer 'eugh eugh' does not query an empty stack.               ]
   turi ip
   
   [Query for the first input line.]
   eugh ip ip ip ip ip ip, ip
   
   [Repeat until no input, or a single 'null character', has been      ]
   [committed.                                                         ]
   eugh eugh
     [Print all characters of the most recent input line.]
     eugh eugh
       eugh ip ip ip ip ip, ip
     turisha
     
     [Query for the next input line.]
     eugh ip ip ip ip ip ip, ip
   turisha")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Turi-ip-ip-ip
  "eugh ip ip ip ip ip ip, ip                                      [get input]
   turi ip ip ip ip ip, ip ip ip ip ip ip ip ip ip; eugh ip ip ip  [t=t-48]
   eugh eugh; eugh ip ip, ip, ip; eugh ip ip ip ip, ip; turisha    [print '1' forever]
   eugh ip ip ip ip, ip                                            [print '0']")

;;; -------------------------------------------------------

;; Demonstrate the instruction "eugh 300", which concatenates the
;; consecutive digits on the current stack to a single integer number.
(interpret-Turi-ip-ip-ip
  "[Push the ASCII codes of the digits '3', '2', and '1' unto the stack.]
   turi ip ip ip ip ip ip, ip ip
   turi ip ip ip ip ip ip, ip
   turi ip ip ip ip ip,    ip ip ip ip ip ip ip ip ip ip
   
   [Convert the stack to a number]
   eugh ip ip ip ip, ip, ip
   
   [Pop and print the thus generated number.]
   eugh ip ip ip ip, ip")

;;; -------------------------------------------------------

;; Looping counter whose upper margin is specified by the user input.
(interpret-Turi-ip-ip-ip
  "[Query for a numeric user input: The tally of repetitions N.]
   eugh ip ip ip ip ip ip, ip
   
   [Convert user input N into integer number.]
   eugh ip ip ip ip, ip, ip
   
   [Push initial counter value, zero (0), to backup stack.]
   turi ip
   eugh ip ip ip, ip, ip, ip
   
   eugh eugh
     [Decrement repetition count N by one.]
     turi ip ip
     eugh ip ip ip
     
     [Copy counter value from backup stack.]
     eugh ip ip ip ip, ip, ip, ip
     [Increment counter value by one.]
     turi ip ip
     eugh ip ip
     [Duplicate copied counter value.]
     eugh ip ip, ip, ip
     [Push one copy of the counter back to the backup stack.]
     eugh ip ip ip, ip, ip, ip
     
     [Print the counter value as a catena of asterisks ('*').]
     eugh eugh
       [Push the ASCII code of the symbol '*' (= 42) unto the stack and print it.]
       turi ip ip ip ip ip, ip ip ip
       eugh ip ip ip ip ip, ip
       
       [Decrement the counter value for the next iteration.]
       turi ip ip
       eugh ip ip ip
     turisha
     
     [Remove the zero-valued top element by adding it to the repetition count.]
     eugh ip ip
     
     [Push the ASCII code of a newline (= 10) unto the stack and print it.]
     turi ip ip, ip
     eugh ip ip ip ip ip, ip
     
   turisha")
