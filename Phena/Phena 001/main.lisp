;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Phena", invented by the Esolang user "Cinnamony" and
;; presented on June 16th, 2023, the diorism of which resides in its
;; pseudonatural syntaxis, whence are words formed by combination
;; pursuing the competences of basic arithmetics, input, output, and
;; control flow helming.
;; 
;; 
;; Concept
;; =======
;; The Phena language's indicium resides in a natural language's mimicry
;; with concomitant derivation from a distinguished vocabulary, every
;; member desumed from its circumference apportioned either the role of
;; an instruction designator or a variable name.
;; 
;; == A PHENA PROGRAM COMPRISES A SENTENCE ==
;; Endowed with its personal dictions, enumerated by 28 items, a Phena
;; program assimilates the sentential design of the English language,
;; every program rendered the composition of a single sentence, and
;; governed by a lealty to the respective regulations:
;; 
;;   (§1) The first program (sentence) letter must assume a majuscular
;;        format; all subsequent letters of the first word ought to
;;        be rendered in minuscles.
;;   
;;   (§2) All words following the incipient one must be adduced in
;;        minuscular form.
;;   
;;   (§3) Every two words must be segregated by one or more whitespaces.
;;        (§3.1) The diorism of whitespaces embraces and is exhausted by
;;               the entities
;;               - the space (" ")
;;               - the horizontal tab ("\t")
;;               - the newline ("\n")
;;   (§4) Every program (sentence) must be terminated by exactly one
;;        period ("."), contingently preceded by zero or more
;;        whitespaces.
;;   
;;   (§5) Succeeding the concluding period ("."), the sentence is
;;        considered as terminated, and the only admissive characters
;;        in its aftermath constitute whitespaces; no other content is
;;        homlogated.
;; 
;; == EVERY WORD CARRIES ITS OWN PRONUNCIATION ==
;; The complete vocabulary inherent to Phena in conjunction with its
;; assigned role in a program, and its pronunciation shall be adduced in
;; the table alow.
;; 
;; Please heed that, its acquisition originating from the Phena
;; language's protolog, unspecified entries are perforce designated as
;; of unknown vocalization.
;; 
;;   ----------------------------------
;;   Word | Role        | Pronunciation
;;   -----+-------------+--------------
;;   ho   | instruction | /hoʊ/
;;   ..................................
;;   i    | instruction | /ɪ/
;;   ..................................
;;   jei  | instruction | /dʒeɪ/
;;   ..................................
;;   ma   | variable    | /mæ/
;;   ..................................
;;   mae  | variable    | /meɪ/
;;   ..................................
;;   me   | variable    | /mi/
;;   ..................................
;;   mea  | variable    | (unknown)
;;   ..................................
;;   mei  | variable    | (unknown)
;;   ..................................
;;   meo  | variable    | /mijoʊ/
;;   ..................................
;;   mo   | variable    | /moʊ/
;;   ..................................
;;   mou  | variable    | /moʊ/
;;   ..................................
;;   mu   | variable    | /mu/
;;   ..................................
;;   my   | variable    | /mi/
;;   ..................................
;;   na   | variable    | /næ/
;;   ..................................
;;   nae  | variable    | /neɪ/
;;   ..................................
;;   ne   | variable    | /ni/
;;   ..................................
;;   nea  | variable    | (unknown)
;;   ..................................
;;   nei  | variable    | (unknown)
;;   ..................................
;;   neo  | variable    | /nijoʊ/
;;   ..................................
;;   no   | variable    | /noʊ/
;;   ..................................
;;   nou  | variable    | /noʊ/
;;   ..................................
;;   nu   | variable    | /nu/
;;   ..................................
;;   ny   | variable    | /ni/
;;   ..................................
;;   pa   | instruction | /pa/
;;   ..................................
;;   phe  | instruction | /fi/
;;   ..................................
;;   sha  | instruction | /ʃa/
;;   ..................................
;;   te   | instruction | /tɜ/
;;   ..................................
;;   tho  | instruction | /θoʊ/
;;   ----------------------------------
;; 
;; == VARIABLES ==
;; Phena's allowance of variables amplects a tally of 20 members,
;; everichon's designation chosen as a nasal betwixt two and three
;; characters in size, and entalented with the capacity to store at any
;; instant either a unsigned byte number or a string composed of zero or
;; more character entities.
;; 
;; A tabular listing shall endow the reader with further acquaintance
;; regarding the variable names and their pronunciations as stated by
;; the original Phena author:
;; 
;;   ------------------------
;;   Variable | Pronunciation
;;   ---------+--------------
;;   ma       | /mæ/
;;   ........................
;;   mae      | /meɪ/
;;   ........................
;;   me       | /mi/
;;   ........................
;;   mea      | (unknown)
;;   ........................
;;   mei      | (unknown)
;;   ........................
;;   meo      | /mijoʊ/
;;   ........................
;;   mo       | /moʊ/
;;   ........................
;;   mou      | /moʊ/
;;   ........................
;;   mu       | /mu/
;;   ........................
;;   my       | /mi/
;;   ........................
;;   na       | /næ/
;;   ........................
;;   nae      | /neɪ/
;;   ........................
;;   ne       | /ni/
;;   ........................
;;   nea      | (unknown)
;;   ........................
;;   nei      | (unknown)
;;   ........................
;;   neo      | /nijoʊ/
;;   ........................
;;   no       | /noʊ/
;;   ........................
;;   nou      | /noʊ/
;;   ........................
;;   nu       | /nu/
;;   ........................
;;   ny       | /ni/
;;   ------------------------
;; 
;; 
;; Architecture
;; ============
;; In the ambitus of its architectural composition, Phena employs a set
;; of 20 variables, identified by a twain or triplet of Latin letters,
;; each capacitated to store at an instant either an unsigned byte datum
;; or a string of arbitrary extent.
;; 
;; With respect to the impotence of any ordering scheme, the variables'
;; castaldy should be aligned with an associative warklume, among which
;; a hash-based mapping should contribute sufficiency in its services.
;; 
;; 
;; Data Types
;; ==========
;; A bifurcation applies to Phena's type system, with unsigned octets
;; commorant in the range [0, 255], occupying the paravaunt moeity,
;; while the complement is furnished via strings admitting an arbitrary
;; size and single characters from the ASCII repertoire.
;; 
;; == OCTETS: FOR ARITHMETICS, CONTROL FLOW, AND INPUT/OUTPUT ==
;; The integral species, a unsigned byte value spanning the range
;; [0, 255], is vested with superiority in its role, as all manipulating
;; warklumes, which comprehends basic arithmetics, as well as the
;; jump-based control flow predicate's foundry, relay to objects desumed
;; from this set.
;; 
;; Upon any of its bourne's transgressions, the variable state wraps
;; around, returning from beyond the maximum of 255 to zero (0), or,
;; when descending below the minimum, to 255.
;; 
;; The interface facilities, entailing input and output, constitute the
;; bailiwick shared by all three available types, integers, strings, and
;; characters.
;; 
;; == STRINGS: FOR INPUT/OUTPUT AND "HELLO WORLD" ==
;; The commission of strings is accompassed merely in a twissel of
;; occasions: imprimis, the input and output conduits' involvement,
;; where users may enter any object for a variable's assignment, or
;; print such a placeholder's content, and, in a parhedral segment, the
;; transmission into these salvatories of the "Hello World" string by
;; the "_ho" instruction's mediation.
;; 
;; == CHARACTERS: FOR INPUT/OUTPUT ==
;; Characters, proceeding in consilience with the string type's
;; forbisen, are homologated their admission only though the
;; input/output conduits, governed by a siclike destitution regarding
;; instruments for their manipulation.
;; 
;; 
;; Syntax
;; ======
;; A natural language simulation, a Phena program subscribes to the
;; acquainted structure of an English-language sentence, producing zero
;; or more whitespace-separated tokens, or "words", the incipient member
;; among these capitalized, while all others assume lowercase symbols,
;; and concluding with a period (".").
;; 
;; == EACH PROGRAM IS A SENTENCE ==
;; A Phena program constitutes a tantamount of a sentence in a natural
;; language, commencing, if not blank, with a Latin majuscle, and
;; terminating in a period ("."), the intermede of which is composed of
;; zero or more words in concord with the specified dictionary.
;; 
;; == INSTRUCTIONS ==
;; The componency of the instructions in preponderance imposes a
;; sequence of one to three letters, succeeded by zero or one argument
;; embedded into the composition in order to form a complete word.
;; Merely the "ho" command deviates from the prefixed ordonnance by
;; expecting the argument to precede the operation designator.
;; 
;; In conformation with Phena's sentential regulations, the program's
;; inicipient letter, whether a command or variable identifier, must be
;; rendered in majuscles, while all subsequent tokens adhere to a
;; minuscular design.
;; 
;; == ARGUMENTS ==
;; If admitted to an instruction, the argument forms an inextricable
;; constituent, through coalescence molded into a single token with its
;; operation name, and inspired with the same amenability to express, if
;; located at the program's inchoation, the incipient letter in
;; upper-case form, at any other position assuming the lower-case
;; standard.
;; 
;; All operands perforce constitute one of the 20 defined variable
;; names.
;; 
;; == WHITESPACES ==
;; A requisitum betwixt two words, in the form of zero or more spaces,
;; horizontal tabs, or newlines, whitespaces constitute an optional
;; piece of ornamentation at any other occasion.
;; 
;; == COMMENTS ==
;; The current language rendition does not afford any commentary
;; warklumes.
;; 
;; == GRAMMAR ==
;; The Phena donet shall be administered a more formal exposition by
;; the Extended Backus-Naur Form as its vehicle:
;; 
;;   sentence       := padding
;;                  ,  [ firstCommand , { commandTail } ]
;;                  ,  padding
;;                  ,  "."
;;                  ,  padding
;;                  ;
;;   
;;   loopStart      := "Pa" , loopBody , thoInside ;
;;   loopInside     := "pa" , loopBody , thoInside ;
;;   loopBody       := commandTail ;
;;   
;;   firstCommand   := startCommand ;
;;   commandTail    := separator , innerCommand
;;                  ,  { separator , innerCommand }
;;                  ;
;;   
;;   startCommand   := pheStart
;;                  |  iStart
;;                  |  jeiStart
;;                  |  shaStart
;;                  |  hoStart
;;                  |  loopStart
;;                  ;
;;   innerCommand   := pheInside
;;                  |  iInside
;;                  |  jeiInside
;;                  |  shaInside
;;                  |  hoInside
;;                  |  loopInside
;;                  ;
;;   
;;   pheStart       := "Phe" , variableInside ;
;;   pheInside      := "phe" , variableInside ;
;;   
;;   iStart         := "I" , variableInside ;
;;   iInside        := "i" , variableInside ;
;;   
;;   jeiStart       : "Jei" , variableInside ;
;;   jeiInside      : "jei" , variableInside ;
;;   
;;   shaStart       : "Sha" , variableInside ;
;;   shaInside      : "sha" , variableInside ;
;;   
;;   hoStart        := variableStart  , "ho" ;
;;   hoInside       := variableInside , "ho" ;
;;   
;;   teStart        := "Te" ;
;;   teInside       := "te" ;
;;   
;;   paStart        := "Pa" ;
;;   paInside       := "pa" ;
;;   
;;   thoInside      := "tho" ;
;;   
;;   variableStart  := "Ma"
;;                  |  "Mae"
;;                  |  "Me"
;;                  |  "Mea"
;;                  |  "Mei"
;;                  |  "Meo"
;;                  |  "Mo"
;;                  |  "Mou"
;;                  |  "Mu"
;;                  |  "My"
;;                  |  "Na"
;;                  |  "Nae"
;;                  |  "Ne"
;;                  |  "Nea"
;;                  |  "Nei"
;;                  |  "Neo"
;;                  |  "No"
;;                  |  "Nou"
;;                  |  "Nu"
;;                  |  "Ny"
;;                  ;
;;   variableInside := "ma"
;;                  |  "mae"
;;                  |  "me"
;;                  |  "mea"
;;                  |  "mei"
;;                  |  "meo"
;;                  |  "mo"
;;                  |  "mou"
;;                  |  "mu"
;;                  |  "my"
;;                  |  "na"
;;                  |  "nae"
;;                  |  "ne"
;;                  |  "nea"
;;                  |  "nei"
;;                  |  "neo"
;;                  |  "no"
;;                  |  "nou"
;;                  |  "nu"
;;                  |  "ny"
;;                  ;
;;   
;;   padding        := { whitespace } ;
;;   separator      := whitespace , { whitespace } ;
;;   whitespace     := " " | "\n" | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; An octuple cardinality governs the perimeter of Phena's instruction
;; set, the ambitus' extent subsuming basic arithmetics, input, output,
;; conditional goto redirections, as well as an unconditional cessation
;; mechanism.
;; 
;; == OVERVIEW ==
;; An apercu shall educate about the eight available instructions.
;; 
;; Please heed the following regulations:
;; 
;;   (1) Placeholder segments are ensconced in a twissel of braces "{"
;;       and "}", the expected to be omitted in the actual program,
;;       while their content must be substituted by valid Phena code.
;;   (2) Any identifier, comprehending both command and variable names,
;;       when empighted at the program's beginning, must be capitalized,
;;       that is, its first letter ought to assume a majuscular form,
;;       while the remaining portion retains its minuscles. At any other
;;       location, all alphabetic constituents are to be rendered in
;;       lower-case letters.
;; 
;; The basic command listing thus assumes:
;; 
;;   ------------------------------------------------------------------
;;   Command       | Effect
;;   --------------+---------------------------------------------------
;;   phe{variable} | Prints the {variable} to the standard output.
;;                 |---------------------------------------------------
;;                 | {variable} must be one of the 20 recognized
;;                 | variable names.
;;                 |---------------------------------------------------
;;                 | If the {variable} value constitutes an integer,
;;                 | the character whose ASCII equals the same is
;;                 | printed.
;;                 | If the {variable} value constitutes a string, its
;;                 | verbatim form is displayed.
;;   ..................................................................
;;   i{variable}   | Queries the user for an ASCII character and stores
;;                 | its character code in the {variable}.
;;                 |---------------------------------------------------
;;                 | {variable} must be one of the 20 recognized
;;                 | variable names.
;;   ..................................................................
;;   jei{variable} | Increments the value of the {variable} by one.
;;                 |---------------------------------------------------
;;                 | {variable} must be one of the 20 recognized
;;                 | variable names.
;;                 |---------------------------------------------------
;;                 | If the {variable} contains a string object, its
;;                 | value is substituted by an integer assuming the
;;                 | default of zero (0), ere applying this operation.
;;   ..................................................................
;;   sha{variable} | Decrements the value of the {variable} by one.
;;                 |---------------------------------------------------
;;                 | {variable} must be one of the 20 recognized
;;                 | variable names.
;;                 |---------------------------------------------------
;;                 | If the {variable} contains a string object, its
;;                 | value is substituted by an integer assuming the
;;                 | default of zero (0), ere applying this operation.
;;   ..................................................................
;;   {variable}ho  | Sets the value of the {variable} to the string
;;                 | "Hello World".
;;                 |---------------------------------------------------
;;                 | {variable} must be one of the 20 recognized
;;                 | variable names.
;;   ..................................................................
;;   pa            | If the value of the variable "nu" equals zero (0),
;;                 | moves the instruction pointer (IP) forward to the
;;                 | position immediately succeeding the matching "tho"
;;                 | instruction. Otherwise proceeds as usual.
;;                 | A string value for "nu" mimics the case of a
;;                 | non-zero integer, that is, a simple progression
;;                 | without jumping to the obverse bourne.
;;   ..................................................................
;;   tho           | If the value of the variable "nu" does not equal
;;                 | zero (0), moves the instruction pointer (IP) back
;;                 | to the position immediately succeeding the
;;                 | matching "pa" instruction. Otherwise proceeds as
;;                 | usual.
;;                 | A string value for "nu" mimics the case of a
;;                 | non-zero integer, that is, a return to the obverse
;;                 | bourne.
;;   ..................................................................
;;   te            | Immediately terminates the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its meticulousness in the provision of commands and examples,
;; the Phena protolog is inflicted with a few vices that render its
;; treatise ambiguous, whence a significant subset shall be ostended in
;; this location.
;; 
;; == HOW ARE UNINITIALIZED VARIABLES HANDLED? ==
;; The enumerable nature apportioned to variables, accounting for 20
;; members exclusively, incites an inquisition into their default state,
;; given a special vista on their contingency to be admissible to both
;; unsigned bytes and strings.
;; 
;; It has been adjudged to impute that any of the 20 variables to the
;; programmer's avail is initialized to the integer value of zero (0).
;; 
;; == WHAT TYPE OF INPUT SHALL BE EXPECTED AND ADMITTED? ==
;; Phena's accoutrement in regards of input transmissions manifests in
;; the "i..." operation; the treatise, however, does not project beyond
;; its ascertainment's fact, failing, especially, to install its
;; expectancies and marches' circumference.
;; 
;; A treble of possible interpretations may be adduced:
;; 
;;   (1) EXCLUSIVE CHARACTER INPUT:
;;       Consilient with a wide range of acquainted esoteric programming
;;       languages, the paragon's rank among the same should be credited
;;       to brainfuck, single character input, usually transliterated
;;       into the respective ASCII code or Unicode codepoint format, is
;;       reserved a conscious tenability.
;;       A corroborating fact, Phena partakes to some palpable mete of
;;       brainfuck's diorisms, most conspicuosly manifested in the
;;       forward/back jump facility for control flows.
;;   
;;   (2) EXCLUSIVE NUMERIC INPUT:
;;       A subset among the esoteric specimens applies itself to the
;;       reception of direct numeric input, the common case of which
;;       resolves to signed or unsigned integer definitions.
;;       This species of specialization attends to one moiety of Phena's
;;       type system in particular, while the textual component lacks
;;       its due attention.
;;   
;;   (3) ARBITRARY INPUT:
;;       A few contingency from the esolang realm assumes a more
;;       refined ilk of input delivery, requiring the dation to be a
;;       line whose interpretation into raw string or numeric format,
;;       in the latter circumstance again most frequently an integer
;;       exponent, ought to be fulfilled by the implementation itself.
;;       A certain potential for ambiguities accounts for the admitted
;;       convenience.
;;       Phena's type system conflates the two potent moeity of string
;;       and integral objects, and thus might very well answer to the
;;       dichotomy.
;; 
;; It has been adjudged to impute the incipient option (1), the
;; consumption of an ASCII character, realizing that the docimasy's
;; particular conclusion serves in obviating many ambiguities that may
;; arise in the homologation of an arbitrary string input; namely, if an
;; integer number is committed, the specification still prescribes
;; its output as the corresponding ASCII character, which in all
;; circumstances would render the induction of numbers via inputs an
;; impossibility.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is realized in Common Lisp, employing the
;; parser combinator concept for the assemblage of abstract syntax tree
;; (AST) nodes from tokens.
;; 
;; == PARSERS AND COMBINATORS ARE FUNCTIONS ==
;; In eath diction, the parser combinator approach constructs a complete
;; parser entity from a sequence of interoperating smaller parsers,
;; their coefficiency enabled through combinators.
;; 
;; Both parsers and combinators are, in their pristine diorism,
;; represented by functions, accepting a source to parse and returning
;; in the case of a successful application a composition apprehending at
;; least
;; 
;;   - The remaining portion of the source, curtailed by the consumed
;;     items.
;;     If, for instance, the source represents a string, the first
;;     characters matching the parsing predicate will be removed; for
;;     tokens in lieu of this direct input, the residue following the
;;     accepted token objects are delivered.
;;   - An object designating the parser's or combinator's contribution
;;     to the encompassing whole, that is, usually an AST node.
;; 
;; A failure in the parser's or combinator's operations usually
;; concludes either with a communicative flag or an error signaling.
;; 
;; Conforming to an augmentation in formality, the following signature
;; may be proffered for parsers and combinators:
;; 
;;   function (source : any) -> (newSource : any, output : any)
;; 
;; == PARSERS AND COMBINATORS ARE INTERWOVEN IN SERIES ==
;; Considering the successful case, the modified parser or combinator
;; source is utilized as an input to the subsequent parser/combinator,
;; chaining these into a series of processors that, in concluding in an
;; ultimately empty source, build the output structure, for instance,
;; the abstract syntax tree.
;; 
;; == PARSERS EQUAL COMBINATORS ==
;; The discrepancy betwixt parsers and combinators constitutes a rather
;; puisne question of terminology for most objectives, as both partake
;; of a functional commonality. Parsers are usually "stand-alone"
;; components, responsible for the actual modification of the source,
;; whereas combinators ligate zero or more parsers, or other
;; combinators, in order to accompass a result.
;; 
;; If we have, as an example, a parser "characterOf", defined as
;; 
;;   function characterOf (expectedCharacter : character)
;;     let characterParser <- function (source : string)
;;       if source[0] = expectedCharacter then
;;         return (source.substring (1, source.length),
;;                 makeNode(NODE_TYPE_CHARACTER, source[0])
;;       else
;;         return null
;;       end if
;;     end function
;;     
;;     return characterParser
;;   end function
;; 
;; the requisitum involved in parsing more than one character coerces us
;; to discover a chaining of mandatorily matching "characterOf"
;; invocations. To this end, we define the following combinator:
;; 
;;   function allMatch (parsers : parserFunction[0..*])
;;     let allCombinator <- function (source : string)
;;       let newSource <- source
;;       let nodes     <- empty node list
;;       for every parser currentParser in parsers do
;;         let parserResult <- currentParser(source)
;;         
;;         if parserResult is null then
;;           return null
;;         else
;;           newSource <- parserResult[0]
;;           append parserResult[1] to nodes
;;         end if
;;       end for
;;       
;;       return (newSource, nodes)
;;     end function
;;     
;;     return allCombinator
;;   end function
;; 
;; An exemplary invocation of the combinator "allMatch" with several
;; instances of the "characterOf" parser could involve:
;; 
;;   parse (allMatch (characterOf ('h'),
;;                    characterOf ('e'),
;;                    characterOf ('l'),
;;                    characterOf ('l'),
;;                    characterOf ('o')),
;;          "hello")
;; 
;; == A PARSER COMBINATOR IN AN OBJECT-ORIENTED CONTEXT ==
;; The principal and onomastic substrate derives from Jeffrey Massung's
;; "parse" package for Common Lisp, which please see under
;; [massung2020parse]. A diverging aspect is apportioned its commorancy
;; in the object-oriented variation, substituting the functional notions
;; in order to emphasize the coefficacy partaken of by the several
;; components.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-20
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [devanla2021minimalparsecomb]
;;   Guru Devanla, "Minimal Parser Combinator in Python",
;;                 26th October 2021
;;   URL: "https://gdevanla.github.io/posts/
;;         write-a-parser-combinator-in-python.html"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in Python.
;;   
;;   [elouafi2018gentleintroparscomb]
;;   Yassine Elouafi, "A gentle introduction to parser combinators",
;;                    2018
;;   URL: "https://dev.to/yelouafi/
;;         a-gentle-introduction-to-parser-combinators-21a0"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [elouafi2021introparsercomb]
;;   Yassine Elouafi, "introduction-to-parser-combinators.md",
;;                    June 28, 2021 
;;   URL: "https://gist.github.com/yelouafi/
;;         556e5159e869952335e01f6b473c4ec1"
;;   Notes:
;;     - Describes parser combinators.
;;     - Demonstrates an implementation in JavaScript.
;;   
;;   [esolang2023Phena]
;;   The Esolang contributors, "Phena", June 16th, 2023
;;   URL: "https://esolangs.org/wiki/Phena"
;;   
;;   [goodrich214datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", sixth edition, 2014,
;;     pages 122--127
;;   Notes:
;;     - Describes the concept and an implementation of the singly
;;       linked list in the Java programming language.
;;     - The pages 276 through 280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;;   
;;   [massung2020parse]
;;   Jeffrey Massung, "The PARSE Package", 2020
;;   URL: "https://github.com/massung/parse"
;;   Notes:
;;     - GitHub repository of the "parse" package, a Common Lisp library
;;       for token parsing which employs parser combinators.
;;   
;;   [mulligan2023unlocking]
;;   Rory Mulligan, "Unlocking the Power of Parser Combinators: A
;;                   Beginner's Guide", February 9, 2023
;;   URL: "https://www.sitepen.com/blog/
;;         unlocking-the-power-of-parser-combinators-a-beginners-guide"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype parser-processor ()
  "The ``parser-processor'' type defines a function ordained to the wike
   of a parse state's processing in order to produce a parse result."
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype optional-character ()
  "The ``optional-character'' type defines a \"nullable\" character
   entity, that is, an object admissible either to represent a
   ``character'' or the ``NIL'' value."
  '(or null character))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T) (size '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   which obey to the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (and (symbolp size)
                     (eq      size '*))
                (= (length (the list candidate))
                   size))
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype parser-generator ()
  "The ``parser-generator'' type defines a function which, receiving a
   parse result output, produces a ``Parser'' itself, affording by such
   motive a consequence."
  '(function (*) Parser))

;;; -------------------------------------------------------

(deftype phena-program ()
  "The ``phena-program'' type defines an executable Phena program as a
   vector embracing zero or more ``Instruction'' instances."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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
  "The ``octet'' type defines an unsigned byte compact of eight adjacent
   bits, and thus an occupant of the integral range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype pobject ()
  "The ``pobject'' type defines a species of object admissive to a Phena
   program, concretely signed integers of any magnitude and arbitrary
   strings."
  '(or octet string))

;;; -------------------------------------------------------

(deftype variable-table ()
  "The ``variable-table'' type defines a registry for the maintenance
   of the 20 defined variables, implemented as a hash table whose keys
   comprehend the identifiers, each such associated with either a signed
   integer or a string datum as the variable value."
  '(hash-table-of string pobject))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Variable-Error (error)
  ((offending-identifier
    :initarg       offending-identifier
    :initform      (error "Missing identifier.")
    :reader        variable-error-offending-identifier
    :type          string
    :documentation "The invalid variable name whose utilization has
                    instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Variable-Error condition))
      (declare (type destination    stream))
      (format stream "The identifier ~s does not designate a valid ~
                      variable name."
        (variable-error-offending-identifier condition))))
  (:documentation
    "The ``Variable-Error'' condition serves to signal an anomalous
     situation involving a request for a variable whose identifier does
     not belong to the twenty recognized names in Phena."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of variable names.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of string 20) +VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +VARIABLE-NAMES+
  '("nae" "na" "nea" "nei" "neo" "ne" "nou" "no" "nu" "ny"
    "mae" "ma" "mea" "mei" "meo" "me" "mou" "mo" "mu" "my")
  "Defines the 20 variable names recognized by Phena.")

;;; -------------------------------------------------------

(defun variable-name-p (identifier)
  "Determines whether the IDENTIFIER represents a valid variable name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string identifier))
  (the boolean
    (not (null
      (find identifier +VARIABLE-NAMES+ :test #'string-equal)))))

;;; -------------------------------------------------------

(defun check-variable-name (identifier)
  "Determines whether the IDENTIFIER represents a valid variable name,
   returning on confirmation the IDENTIFIER itself, otherwise signaling
   an error of the type ``Variable-Error''."
  (declare (type string identifier))
  (the string
    (if (variable-name-p identifier)
      identifier
      (error 'Variable-Error :offending-identifier identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface encapsulates the notion of a Phena
   instruction.")

;;; -------------------------------------------------------

(defstruct (Unary-Instruction
  (:include Instruction))
  "The ``Unary-Instruction'' class contributes a foundry for
   instructions dependent upon a single operand, defined in terms of a
   variable name."
  (operand (error "Missing operand.") :type string))

;;; -------------------------------------------------------

(defstruct (Phe-Instruction
  (:include     Unary-Instruction)
  (:constructor make-phe-instruction (operand)))
  "The ``Phe-Instruction'' class replicates the \"phe\" instruction,
   responsible for the printing of a numeric or string argument.")

;;; -------------------------------------------------------

(defstruct (I-Instruction
  (:include     Unary-Instruction)
  (:constructor make-i-instruction (operand)))
  "The ``I-Instruction'' class replicates the \"i\" instruction,
   responsible for the storage of user input in its variable argument.")

;;; -------------------------------------------------------

(defstruct (Jei-Instruction
  (:include     Unary-Instruction)
  (:constructor make-jei-instruction (operand)))
  "The ``Jei-Instruction'' class replicates the \"jei\" instruction,
   responsible for a variable's incrementation.")

;;; -------------------------------------------------------

(defstruct (Sha-Instruction
  (:include     Unary-Instruction)
  (:constructor make-sha-instruction (operand)))
  "The ``Sha-Instruction'' class replicates the \"sha\" instruction,
   responsible for a variable's decrementation.")

;;; -------------------------------------------------------

(defstruct (Ho-Instruction
  (:include     Unary-Instruction)
  (:constructor make-ho-instruction (operand)))
  "The ``Ho-Instruction'' class replicates the \"ho\" instruction,
   responsible for the assignment of the string \"Hello World\" to its
   variable argument.")

;;; -------------------------------------------------------

(defstruct (Pa-Instruction
  (:include     Instruction)
  (:constructor make-pa-instruction ()))
  "The ``Pa-Instruction'' class replicates the \"pa\" instruction,
   responsible for a conditional forward jump to the matching \"tho\"
   companion.")

;;; -------------------------------------------------------

(defstruct (Tho-Instruction
  (:include     Instruction)
  (:constructor make-tho-instruction ()))
  "The ``Tho-Instruction'' class replicates the \"tho\" instruction,
   responsible for a conditional back jump to the matching \"pa\"
   companion.")

;;; -------------------------------------------------------

(defstruct (Te-Instruction
  (:include     Instruction)
  (:constructor make-te-instruction ()))
  "The ``Te-Instruction'' class replicates the \"te\" instruction,
   responsible for a program's immediate termination.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse state.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-initial-parse-State
    (source
     &aux (position 0)))
  (:constructor advance-parse-state
    (template
     &aux (source   (parse-state-source template))
          (position (1+ (parse-state-position template))))))
  "The ``Parse-State'' class encapsulates the progress in the parsing
   process, subsuming in its compass the shared Phena source code and
   the currently perquired location in the same, the latter of which
   captures the effective advancement."
  (source   (error "Missing source.") :type string)
  (position 0                         :type fixnum))

;;; -------------------------------------------------------

(defun parse-state-character (state)
  "Returns the character referenced by the parse STATE's cursor in the
   shared Phena source, or ``NIL'' if the location transcends the valid
   code bounds."
  (declare (type Parse-State state))
  (the optional-character
    (when (array-in-bounds-p (parse-state-source state)
            (parse-state-position state))
      (char (parse-state-source state)
        (parse-state-position state)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse result.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class encapsulates a parser's response to a
   parse state's probing, encompassing a success-or-failure flag, the
   ensuing parse state, which might be advanced to the next position in
   the parsed Phena source, and an output object which might contribute
   to the assembled program's entirety."
  (succeeded-p (error "Missing success flag.") :type boolean)
  (state       (error "Missing state.")        :type Parse-State)
  (output      (error "Missing output.")       :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class implements a parser or combinator, the foundry
   of which is afforded by a processor function."
  (processor (error "Missing processor.") :type parser-processor))

;;; -------------------------------------------------------

(defun parse-state (parser state)
  "Applies the PARSER to the parse STATE, returning a parse result, the
   same defines its response, encompassing the success flag, the
   begotten state, and an output that might contribute to the assembled
   program representation."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall (parser-processor parser) state)))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Creates and returns a new ``Parser'' in a more convenient avenue by
   building its processor function utilizing the STATE-VARIABLE as the
   aefauld input, with the BODY forms providing the implementation."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun probe-character (predicate)
  "Creates and returns a new ``Parser'' which succeeds if its probed
   parse state's character satisfies the predicate, on confirmation
   returning the matching character."
  (declare (type (function (optional-character) *) predicate))
  (the Parser
    (build-parser (state)
      (let ((probed-character (parse-state-character state)))
        (declare (type optional-character probed-character))
        (the Parse-Result
          (if (funcall predicate probed-character)
            (make-parse-result T
              (advance-parse-state state)
              probed-character)
            (make-parse-result NIL state probed-character)))))))

;;; -------------------------------------------------------

(defmacro build-character-parser ((probed-character-variable)
                                  &body body)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   character, committed as the PROBED-CHARACTER-VARIABLE, matches,
   signified by the desinent BODY form producing a non-``NIL'' primary
   value, on confirmation returning in its parse result's output the
   probed character, which might be ``NIL''.
   ---
   This macro accommodates a convenience operation for the official
   constructor ``make-parser'', providing the processor in the form of
   an anonymous function whose aefauld argument is denoted by the
   PROBED-CHARACTER-VARIABLE, concomitantly establishing the argument's
   type declaration and marking the same as ignorable."
  `(the Parser
     (probe-character
       #'(lambda (,probed-character-variable)
           (declare (type      optional-character
                               ,probed-character-variable))
           (declare (ignorable ,probed-character-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun character-of (expected-character)
  "Returns a new ``Parser'' which succeeds if its input parse state's
   probed character equals the EXPECTED-CHARACTER, returning in its
   parse result's output the matching character."
  (declare (type character expected-character))
  (the Parser
    (probe-character
      #'(lambda (probed-character)
          (declare (type optional-character probed-character))
          (and probed-character
               (char= probed-character expected-character))))))

;;; -------------------------------------------------------

(defun eof ()
  "Returns a new ``Parser'' which succeeds if its input parser state's
   probed character constitutes the ``NIL'' value, on confirmation
   returning in its parse result's output the ``NIL'' object."
  (the Parser
    (build-character-parser (probed-character)
      (null probed-character))))

;;; -------------------------------------------------------

(defun bind (antecedent consequent-generator)
  "Returns a new ``Parser'' realizing a monadic binding by probing the
   ANTECEDENT parser and, if the same matches, querying the
   CONSEQUENT-GENERATOR for a parser to apply to the ANTECEDENT result's
   state, succeeding if both, the ANTECEDENT and the newly produced
   parser, match, and on confirmation returning the consequent parser's
   result."
  (declare (type Parser           antecedent))
  (declare (type parser-generator consequent-generator))
  (the Parser
    (build-parser (state)
      (let ((antecedent-result (parse-state antecedent state)))
        (declare (type Parse-Result antecedent-result))
        (the Parse-Result
          (if (parse-result-succeeded-p antecedent-result)
            (parse-state
              (funcall consequent-generator
                (parse-result-output antecedent-result))
              (parse-result-state antecedent-result))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defmacro bindlet ((antecedent-output-variable antecedent)
                   &body body)
  "Accommodates a convenient warklume for monadic binding by returning a
   new ``Parser'' which probes whether the ANTECEDENT parser matches,
   on confirmation binding its output to the ANTECEDENT-RESULT-OUTPUT
   variable, executing the BODY forms, and expecting the desinent form
   to produce a ``Parser'' itself, the same is subsequently invoked with
   the ANTECEDENT's result state in order to affirm or negate the
   matching."
  `(the Parser
     (bind ,antecedent
       #'(lambda (,antecedent-output-variable)
           (declare (type T    ,antecedent-output-variable))
           (declare (ignorable ,antecedent-output-variable))
           ,@body))))

;;; -------------------------------------------------------

(defun return-output (output)
  "Returns a new ``Parser'' which always succeeds, returning, besides
   its input state in a verbatim form, the OUTPUT as its output datum."
  (declare (type T output))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (make-parse-result T state output)))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if all of its input PARSERS,
   in the specified order, match, on confirmation returning the desinent
   parser's parse result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          for parser
            of-type Parser
            in      parsers
          for result
            of-type Parse-Result
            =       (parse-state parser new-state)
          unless (parse-result-succeeded-p result) do
            (return
              (make-parse-result NIL state NIL))
          finally
            (return result))))))

;;; -------------------------------------------------------

(defun sequence-of (&rest parsers)
  "Returns a new ``Parser'' which succeeds if all of the PARSERS, in
   the specified order, match, on confirmation returning in its parse
   result's output a list of the PARSERS' outputs reflecting their given
   ordonnance."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          for parser
            of-type Parser
            in      parsers
          for result
            of-type Parse-Result
            =       (parse-state parser new-state)
          if (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          else do
            (return
              (make-parse-result NIL state NIL))
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun many-of (parser)
  "Returns a new ``Parser'' which always succeeds, attempting to match
   the PARSER zero or more times, finally returning in its parse
   result's output a list comprehending the gathered PARSER outputs in
   their correct order."
  (declare (type Parser parser))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          for result
            of-type Parse-Result
            =       (parse-state parser new-state)
          while (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun one-or-more-of (parser)
  "Returns a new ``Parser'' which succeeds if the input PARSER matches
   one or more times, on confirmation returning in its parse result's
   output a list comprehending the PARSER's gathered outputs in their
   correct order."
  (declare (type Parser parser))
  (the Parser
    (bindlet (first-output parser)
      (declare (type T first-output))
      (build-parser (state)
        (the Parse-Result
          (loop
            for new-state
              of-type Parse-State
              =       state
              then    (parse-result-state result)
            for result
              of-type Parse-Result
              =       (parse-state parser new-state)
            while (parse-result-succeeded-p result)
              collect (parse-result-output result)
              into    subsequent-outputs
            finally
              (return
                (make-parse-result T new-state
                  (cons first-output subsequent-outputs)))))))))

;;; -------------------------------------------------------

(defun any-of (&rest choices)
  "Returns a new ``Parser'' which succeeds if any of its CHOICES match,
   on confirmation returning in its parse result's output the first
   eligible parser's output.
   ---
   The CHOICES are probed in their specified order, and the process
   ceases immediately with the first positive match."
  (declare (type parser-list choices))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for parser
            of-type Parser
            in      choices
          for result
            of-type Parse-Result
            =       (parse-state parser state)
          when (parse-result-succeeded-p result) do
            (return result)
          finally
            (return
              (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun letters-of (letters
                   &optional (start-of-sentence-p NIL
                              start-of-sentence-supplied-p))
  "Returns a new ``Parser'' which succeeds if the LETTERS' characters
   capitalized if START-OF-SENTENCE-P is ``T'', in minuscles if ``NIL'',
   and in its verbatim form if this argument is omitted, follows, on
   confirmation returning in its parse result's output a string
   containing the probed characters."
  (declare (type string  letters))
  (declare (type boolean start-of-sentence-p))
  (declare (type T       start-of-sentence-supplied-p))
  (the Parser
    (bindlet (characters
              (apply #'sequence-of
                (map 'list #'character-of
                  ;; Determine whether the LETTERS' case shall be
                  ;; adjusted to the position in a sentence.
                  (cond
                    ;; No adjustment necessary? => Verbatim LETTERS.
                    ((not start-of-sentence-supplied-p)
                      letters)
                    (start-of-sentence-p
                      (string-capitalize letters))
                    (T
                      (string-downcase letters))))))
      (declare (type (list-of character) characters))
      (return-output
        (coerce characters 'string)))))

;;; -------------------------------------------------------

(defun variable-name (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a variable name,
   capitalized if START-OF-SENTENCE-P is ``T'', otherwise in minuscles,
   follows, on confirmation returning in its parse result's output the
   retrieved identifier."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (bindlet
        (variable-name
          (apply #'any-of
            (mapcar
              #'(lambda (variable-name)
                  (declare (type string variable-name))
                  (the Parser
                    (letters-of variable-name start-of-sentence-p)))
              +VARIABLE-NAMES+)))
      (declare (type string variable-name))
      (return-output
        (string-downcase variable-name)))))

;;; -------------------------------------------------------

(defun phe_ (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"phe\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "phe" start-of-sentence-p)
      ;; Argument is a variable name?
      (bindlet (variable (variable-name start-of-sentence-p))
        (declare (type string variable))
        (return-output
          (make-phe-instruction variable))))))

;;; -------------------------------------------------------

(defun i_ (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if an \"i\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "I" start-of-sentence-p)
      (bindlet (variable (variable-name NIL))
        (declare (type string variable))
        (return-output
          (make-i-instruction variable))))))

;;; -------------------------------------------------------

(defun te (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"te\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "te" start-of-sentence-p)
      (return-output
        (make-te-instruction)))))

;;; -------------------------------------------------------

(defun jei_ (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"jei\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "jei" start-of-sentence-p)
      (bindlet (variable (variable-name NIL))
        (declare (type string variable))
        (return-output
          (make-jei-instruction variable))))))

;;; -------------------------------------------------------

(defun sha_ (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"sha\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "sha" start-of-sentence-p)
      (bindlet (variable (variable-name NIL))
        (declare (type string variable))
        (return-output
          (make-sha-instruction variable))))))

;;; -------------------------------------------------------

(defun pa (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"pa\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "pa" start-of-sentence-p)
      (return-output
        (make-pa-instruction)))))

;;; -------------------------------------------------------

(defun tho (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"tho\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (chain-of
      (letters-of "tho" start-of-sentence-p)
      (return-output
        (make-tho-instruction)))))

;;; -------------------------------------------------------

(defun _ho (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a \"ho\" command
   invocation, capitalized if START-OF-SENTENCE-P is ``T'', otherwise in
   minuscles, follows, on confirmation returning in its parse result's
   output an ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (bindlet (variable (variable-name start-of-sentence-p))
      (declare (type string variable))
      (chain-of
        (letters-of "ho")
        (return-output
          (make-ho-instruction variable))))))

;;; -------------------------------------------------------

(defun whitespace ()
  "Returns a new ``Parser'' which succeeds if its input parse state's
   character constitutes a whitespace, on confirmation returning in its
   parse result's output the probed character."
  (the Parser
    (build-character-parser (probed-character)
      (and probed-character
           (member probed-character
             '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun separator ()
  "Returns a new ``Parser'' which succeeds if a sequence of one or more
   accolent whitespaces follows, on confirmation returning in its parse
   result's output the gathered characters in their correct order."
  (the Parser
    (one-or-more-of
      (whitespace))))

;;; -------------------------------------------------------

(defun padding ()
  "Returns a new ``Parser'' which always succeeds, skipping a sequence
   of zero or more accolent whitespaces, and returning its parse
   result's output the matching characters."
  (the Parser
    (many-of
      (whitespace))))

;;; -------------------------------------------------------

(defun period ()
  "Returns a new ``Parser'' which succeeds if its input state's
   character constitutes a period (\".\"), on confirmation returning in
   its parse result's output the probed character."
  (the Parser
    (build-character-parser (probed-character)
      (and probed-character
           (char= probed-character #\.)))))

;;; -------------------------------------------------------

(defun coda ()
  "Returns a new ``Parser'' which succeeds if the end of the source
   follows, contingently preceded by zero or more whitespaces, on
   confirmation returning in its parse result's output the end-of-file
   designator ``NIL''."
  (the Parser
    (chain-of
      (padding)
      (eof))))

;;; -------------------------------------------------------

(defun fail (datum &rest arguments)
  "Returns a new ``Parser'' which always succeeds by signaling an error
   whose concrete nature is delineated by the DATUM's and ARGUMENTS'
   champarty, ostending a perfect consilience to the ``error''
   function's regulations, which please see."
  (declare (type (or symbol string) datum))
  (declare (type (list-of T)        arguments))
  (the Parser
    (build-parser (state)
      (apply #'error datum arguments))))

;;; -------------------------------------------------------

(defun command (start-of-sentence-p)
  "Returns a new ``Parser'' which succeeds if a command invocation,
   capitalized if START-OF-SENTENCE-P is ``T'', otherwise in minuscles,
   follows, on confirmation returning in its parse result's output an
   ``Instruction'' representation thereof."
  (declare (type boolean start-of-sentence-p))
  (the Parser
    (any-of
      (phe_ start-of-sentence-p)
      (i_   start-of-sentence-p)
      (te   start-of-sentence-p)
      (jei_ start-of-sentence-p)
      (sha_ start-of-sentence-p)
      (pa   start-of-sentence-p)
      (tho  start-of-sentence-p)
      (_ho  start-of-sentence-p))))

;;; -------------------------------------------------------

(defun sentence-fragment ()
  "Returns a new ``Parser'' which succeeds if zero or more commands,
   preceded by at least one whitespace, follow, on confirmation
   returning in its parse result's output a list of the gleaned
   ``Instruction'' representation in their correct order."
  (the Parser
    (many-of
      (chain-of
        (separator)
        (command NIL)))))

;;; -------------------------------------------------------

(defun sentence ()
  "Returns a new ``Parser'' which succeeds if a sentence, conforming to
   the Phena language's syntactical regulations, follows, on
   confirmation returning in its parse result's output a one-dimensional
   simple array of ``Instruction''s that represent the entailed
   commands."
  (the Parser
    (chain-of
      (padding)
      (bindlet (first-command (command T))
        (declare (type Instruction first-command))
        (bindlet (further-commands (sentence-fragment))
          (declare (type (list-of Instruction) further-commands))
          (any-of
            (chain-of
              (padding)
              (period)
              (coda)
              (return-output
                (coerce
                  (cons first-command further-commands)
                  '(simple-array Instruction (*)))))
            (fail "The sentence did not conclude in a period ~
                   (\".\")")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Table
  (:constructor make-jump-table ()))
  "The ``Jump-Table'' class applies itself to the connection of forward
   jump (\"pa\") and back jump (\"tho\") instructions in a Phena program
   by adminiculum of their indices in the same."
  (connections (make-hash-table :test #'eql)
               :type (hash-table-of fixnum fixnum)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Associates a forward jump (\"pa\") instruction via its START-POINT
   in a program to a back jump (\"tho\") through its END-POINT, stores
   the vinculum in the JUMP-TABLE, and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (setf (gethash start-point
          (jump-table-connections jump-table))
        end-point)
  (setf (gethash end-point
          (jump-table-connections jump-table))
        start-point)
  (values))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table source-point)
  "Returns the opposite jump instruction's position to the SOURCE-POINT
   registered at the JUMP-TABLE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (or (gethash source-point
          (jump-table-connections jump-table))
        (error "No end point associated with the jump source ~d."
          source-point))))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Creates and returns for the Phena PROGRAM a ``Jump-Table'' which
   bilaterally connects its forward jump (\"pa\") and back jump
   (\"tho\") instructions via their positions in the PROGRAM."
  (declare (type phena-program program))
  (let ((jump-table          (make-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for instruction of-type Instruction across program
      and position    of-type fixnum      from   0 by 1
      
      if (pa-instruction-p instruction) do
        (push position forward-jump-points)
      else if (tho-instruction-p instruction) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (connect-jump-points jump-table start-point end-point))
          (error "Unmatched \"tho\" instruction at position ~d."
            position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched \"pa\" instructions at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-variable-table ()
  "Creates and returns a new ``variable-table'' whose entries are all
   set to the default value of zero (0)."
  (let ((variables (make-hash-table :test #'equal)))
    (declare (type variable-table variables))
    (dolist (variable-name +VARIABLE-NAMES+)
      (declare (type string variable-name))
      (setf (gethash variable-name variables) 0))
    (the variable-table variables)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-input ()
  "Queries the standard input for an ASCII character and returns its
   character code."
  (format T "~&>> ")
  (finish-output)
  (let ((input (read-char)))
    (declare (type character input))
    (clear-input)
    (the octet
      (char-code input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          phena-program
    :documentation "The Phena program to process.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP) location inside of the
                    PROGRAM.")
   (current-instruction
    :initform      NIL
    :type          (or null Instruction)
    :documentation "The instruction at the instruction pointer (IP)
                    position in the PROGRAM, or ``NIL'', if the
                    location transcends the PROGRAM's bournes.")
   (jump-table
    :initform      (make-jump-table)
    :type          Jump-Table
    :documentation "Connects the forward jump and back jump points in
                    the PROGRAM.")
   (variables
    :initform      (build-variable-table)
    :type          variable-table
    :documentation "Maintains a table of the variables."))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluation of a
     Phena program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  (declare (type Interpreter interpreter))
  (with-slots (program ip current-instruction jump-table) interpreter
    (declare (type phena-program         program))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (declare (type Jump-Table            jump-table))
    (setf current-instruction
      (when (array-in-bounds-p program ip)
        (aref program ip)))
    (setf jump-table
      (build-jump-table program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' which operates on the Phena
   PROGRAM."
  (declare (type phena-program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip current-instruction) interpreter
    (declare (type phena-program         program))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (setf current-instruction
      (when (array-in-bounds-p program (1+ ip))
        (aref program
          (incf ip)))))
  (values))

;;; -------------------------------------------------------

(defun move-to (interpreter new-position)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   NEW-POSITION and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip current-instruction) interpreter
    (declare (type phena-program         program))
    (declare (type fixnum                ip))
    (declare (type (or null Instruction) current-instruction))
    (setf ip new-position)
    (setf current-instruction
      (when (array-in-bounds-p program ip)
        (aref program ip))))
  (values))

;;; -------------------------------------------------------

(defun jump-to (interpreter)
  "Expecting to reside at a jump instruction, relocates the
   INTERPRETER's instruction pointer (IP) to the opposite jump end
   point, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip jump-table) interpreter
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (move-to interpreter
      (get-jump-destination jump-table ip)))
  (values))

;;; -------------------------------------------------------

(defun terminate-program (interpreter)
  "Terminates the INTERPRETER's program and returns no value."
  (declare (type Interpreter interpreter))
  (move-to interpreter
    (length
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun program-terminated-p (interpreter)
  "Determines whether the INTERPRETER's program is exhausted, which
   constitutes a tantamount of its instruction pointer's transgression
   of the program boundaries, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (null (slot-value interpreter 'current-instruction))))

;;; -------------------------------------------------------

(defgeneric variable-value (interpreter name)
  (:documentation
    "Returns the value of the variable registered with the NAME at the
     INTERPRETER.")
  
  (:method ((interpreter Interpreter)
            (name        string))
    "Returns the octet or string value of the variable registered with
     the NAME at the INTERPRETER."
    (declare (type Interpreter interpreter))
    (declare (type string      name))
    (the pobject
      (gethash
        (check-variable-name name)
        (slot-value interpreter 'variables)))))

;;; -------------------------------------------------------

(defgeneric (setf variable-value) (new-value interpreter name)
  (:documentation
    "Stores the NEW-VALUE in the variable registered with the NAME at
     the INTERPRETER and returns no value.")
  
  (:method ((new-value   integer)
            (interpreter Interpreter)
            (name        string))
    "Stores the numeric NEW-VALUE in the variable registered with the
     NAME at the INTERPRETER, contingently wrapping it around in order
     to accommodate the valid unsigned byte range [0, 255], and returns
     no value."
    (declare (type integer     new-value))
    (declare (type Interpreter interpreter))
    (declare (type string      name))
    (setf (gethash
            (check-variable-name name)
            (slot-value interpreter 'variables))
          (mod new-value 256))
    (values))
    
    (:method ((new-value   string)
              (interpreter Interpreter)
              (name        string))
    "Stores the string NEW-VALUE in the variable registered with the
     NAME at the INTERPRETER and returns no value."
    (declare (type string      new-value))
    (declare (type Interpreter interpreter))
    (declare (type string      name))
    (setf (gethash
            (check-variable-name name)
            (slot-value interpreter 'variables))
          new-value)
    (values)))

;;; -------------------------------------------------------

(defun ensure-numeric-variable-value (interpreter name)
  "Ascertains that the variable registered with the NAME at the
   INTERPRETER contains an integer value, upon necessity setting it to
   the default numeric state of zero (0), in any case returning no
   value."
  (declare (type Interpreter interpreter))
  (declare (type string      name))
  (unless (integerp (variable-value interpreter name))
    (setf (variable-value interpreter name) 0))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Phe-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Phe-Instruction instruction))
  (let ((variable-value
          (variable-value interpreter
            (unary-instruction-operand instruction))))
    (declare (type pobject variable-value))
    (typecase variable-value
      (integer
        (format T "~c"
          (code-char variable-value)))
      (string
        (format T "~a" variable-value))
      (otherwise
        (error "Invalid variable value: ~s." variable-value))))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction I-Instruction))
  (declare (type Interpreter   interpreter))
  (declare (type I-Instruction instruction))
  (setf (variable-value interpreter
          (unary-instruction-operand instruction))
        (read-input))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Jei-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Jei-Instruction instruction))
  (let ((variable (unary-instruction-operand instruction)))
    (declare (type string variable))
    (ensure-numeric-variable-value interpreter variable)
    (incf (variable-value interpreter variable)))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Sha-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Sha-Instruction instruction))
  (let ((variable (unary-instruction-operand instruction)))
    (declare (type string variable))
    (ensure-numeric-variable-value interpreter variable)
    (decf (variable-value interpreter variable)))
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Ho-Instruction))
  (declare (type Interpreter    interpreter))
  (declare (type Ho-Instruction instruction))
  (setf (variable-value interpreter
          (unary-instruction-operand instruction))
        "Hello World")
  (advance-ip interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Pa-Instruction))
  (declare (type Interpreter    interpreter))
  (declare (type Pa-Instruction instruction))
  (declare (ignore              instruction))
  (let ((nu-value (variable-value interpreter "nu")))
    (declare (type pobject nu-value))
    (if (and (integerp nu-value)
             (zerop    nu-value))
      (jump-to interpreter)
      (advance-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Tho-Instruction))
  (declare (type Interpreter     interpreter))
  (declare (type Tho-Instruction instruction))
  (declare (ignore               instruction))
  (let ((nu-value (variable-value interpreter "nu")))
    (declare (type pobject nu-value))
    (if (and (integerp nu-value)
             (zerop nu-value))
      (advance-ip interpreter)
      (jump-to    interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((interpreter Interpreter)
                                (instruction Te-Instruction))
  (declare (type Interpreter    interpreter))
  (declare (type Te-Instruction instruction))
  (declare (ignore              instruction))
  (terminate-program interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Phena program maintained by the INTERPRETER and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-terminated-p interpreter) do
    (process-instruction interpreter
      (slot-value interpreter 'current-instruction)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Phena (code)
  "Interprets the piece of Phena source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (let ((parse-result
              (parse-state
                (sentence)
                (make-initial-parse-state code))))
        (declare (type Parse-Result parse-result))
        (if (parse-result-succeeded-p parse-result)
          (parse-result-output parse-result)
          (error "Parsing failed.")))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the ASCII characters, covering the character code range
;; [0, 255] infinitely.
(interpret-Phena "Jeinu pa jeineo pheneo tho.")

;;; -------------------------------------------------------

;; Print the ASCII characters, covering the character code range
;; [0, 255] once.
(interpret-Phena "Jeinu pa jeineo pheneo jeinu tho.")

;;; -------------------------------------------------------

;; Print "Hello World" utilizing the dedicated command.
(interpret-Phena "Neho phene.")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Phena "Ima phema.")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a user input of the
;; "null character".
(interpret-Phena "Inu pa phenu inu tho.")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; A pseudocode formulation shall limn the concept:
;; 
;;   nu <- input        { Either 48 (= "0") or 49 (= "1"). }
;;   
;;   nu <- nu - 48      { = 0 or 1. }
;;   na <- 49           { = "1" }.
;;   
;;   while nu != 0 do
;;     print na
;;   end while
;;   
;;   na <- na - 1       { = "0". }
;;   print na
(interpret-Phena
  "Inu
   
   shanu shanu shanu shanu shanu shanu shanu shanu
   shanu shanu shanu shanu shanu shanu shanu shanu
   shanu shanu shanu shanu shanu shanu shanu shanu
   shanu shanu shanu shanu shanu shanu shanu shanu
   shanu shanu shanu shanu shanu shanu shanu shanu
   shanu shanu shanu shanu shanu shanu shanu shanu
   
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina jeina jeina jeina jeina jeina jeina jeina
   jeina
   
   pa
     phena
   tho
   
   shana
   phena.")
