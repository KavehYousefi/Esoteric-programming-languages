;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "llec", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 30th, 2023, the dioristic element of its haecceity
;; wones in its similitude to an assembly language, as well as the
;; faculty to helm the control flow via labels in order to manipulate an
;; infinite memory of integer values.
;; 
;; 
;; Concept
;; =======
;; The llec programming language assumes a guise akin to an assembler
;; language, capacitated to apply basic arithmetics on an infinite tape
;; of cells, these being amenable to indices starting with zero (0),
;; performing conditional command omission (skipping), as well as
;; unconditional goto operations via labels, and issuing character-based
;; input and output.
;; 
;; 
;; Architecture
;; ============
;; llec's memory model employs an infinite tally of cells amenable to
;; integer indices greater than or equal to zero (0), but not impounded
;; towards the upper march.
;; 
;; Each cell, at its inchoation set to zero (0), may store a integer
;; scalar of any sign and magnitude.
;; 
;; 
;; Data Types
;; ==========
;; The type system ostends a bivial exercise, supplemented to the
;; paravant integer type's compernage the parhedral ASCII character
;; repertoire for communication purposes.
;; 
;; == SIGNED INTEGERS OCCUPY A PARAVANT ECHOLON ==
;; The integer type admits both negative and positive values, without
;; a constraint's imposition upon their mickleness.
;; 
;; == CHARACTERS OPERATE ALONG THE INPUT/OUTPUT CONDUITS ==
;; The character species, its reification accounting for the ASCII
;; repertoire, occupies the communiation channels.
;; 
;; 
;; Syntax
;; ======
;; A llec program is compact of zero or more instructions, the sames'
;; introduction proceeds via a single identifying character,
;; complemented by one or two inputs.
;; 
;; == INSTRUCTIONS ==
;; For specimens whose dependency allocates a single operand, this piece
;; of information immediately succeeds the command identifier token;
;; such in relations with a twain extends the parameter list with a
;; dioristic sepiment, ere the second argument's statement follows.
;; 
;; == GRAMMAR ==
;; llec's donat shall be subjected to a more formal species of
;; description by adminiculum of the Extended Backus-Naur Form (EBNF):
;; 
;;   program          := padding
;;                    ,  [ command ]
;;                    ,  padding
;;                    ,  { separator , command }
;;                    ,  padding
;;                    ;
;;   
;;   command          := increment
;;                    |  decrement
;;                    |  add
;;                    |  input
;;                    |  output
;;                    |  skipIfEqual
;;                    |  labelDeclaration
;;                    |  labelGoto
;;                    ;
;;   
;;   increment        := "+" , separator , cellOperand ;
;;   decrement        := "-" , separator , cellOperand ;
;;   add              := '"'
;;                    ,  separator , cellOperand
;;                    ,  separator , "+"
;;                    ,  separator , cellOperand
;;                    ;
;;   input            := "'" , separator , cellOperand ;
;;   output           := ":" , separator , cellOperand ;
;;   skipIfEqual      := "?"
;;                    ,  separator , cellOperand
;;                    ,  separator , "="
;;                    ,  separator , numericOperand
;;                    ;
;;   labelDeclaration := "/" , separator , labelName ;
;;   labelGoto        := "\" , separator , labelName ;
;;   
;;   numericOperand   := integer   | cellOperand ;
;;   cellOperand      := cellIndex | cellReference ;
;;   cellReference    := "*" , digit , { digit } ;
;;   cellIndex        := digit , { digit } ;
;;   labelName        := integer | string ;
;;   
;;   string           := firstStringChar , { subseqStringChar } ;
;;   firstStringChar  := letter | underline ;
;;   subseqStringChar := letter | underline | digit ;
;;   underline        := "_" ;
;;   integer          := [ "+" | "-" ] , digit , { digit } ;
;;   digit            := "0" | "1" | "2" | "3" | "4"
;;                    |  "5" | "6" | "7" | "8" | "9"
;;                    ;
;;   
;;   padding          := [ separator ] ;
;;   separator        := whitespace , { whitespace } ;
;;   whitespace       := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A octuple tally of members exhausts llecs's instruction set,
;; incorporating basic arithmetics, conditional skipping, label-based
;; control flow, as well as character input and output.
;; 
;; == OPERANDS ==
;; The various operand types' excellent role vindicates a section of its
;; own in order to communicate a sufficently designed intelligence.
;; 
;; Please heed that placeholder sections are underlined with a double
;; underline ("="), expected to be supplanted by valid llec code:
;; 
;;   ------------------------------------------------------------------
;;   Operand   | Syntax  | Description
;;   ----------+---------+---------------------------------------------
;;   Integer   | digits  | An integer literal, optionally preceded by a
;;             | ======  | sign, "+" or "-", and composed of one or
;;             |         | more decimal {digits}.
;;             |         |---------------------------------------------
;;             |         | Depending on the context, the value may
;;             |         | either be construed as a literal number, a
;;             |         | cell index --- imposing a strictly positive
;;             |         | value greater than or equal to one (1) ---,
;;             |         | or a label identifier.
;;   ..................................................................
;;   Reference | *digits | An integer value, unsigned and composed of
;;             |  ====== | one or more decimal {digits}, and always
;;             |         | employed in the agency of a memory cell
;;             |         | reference.
;;             |         |---------------------------------------------
;;             |         | If used in the context of an integer
;;             |         | literal, the value instead selects the cell
;;             |         | at the index {digits}.
;;             |         |---------------------------------------------
;;             |         | If employed in a cell index situation, an
;;             |         | indirect reference is assumed, that is, the
;;             |         | value of the cell at the index {digits} is
;;             |         | queried and used itself as the ultimate cell
;;             |         | cell index to obtain, namely:
;;             |         |   let actualIndex <- memory[{digits}]
;;             |         |   let result      <- memory[actualIndex]
;;             |         | Or, more compendious:
;;             |         |   let result <- memory[memory[{digits}]]
;;   ..................................................................
;;   String    | chars   | A string literal of one or more characters,
;;             | =====   | the first of which must constitute a Latin
;;             |         | or underscore ("_"), followed by zero or
;;             |         | more letters, underscores, or decimal
;;             |         | digits.
;;             |         |---------------------------------------------
;;             |         | String literals are exclusively utilized for
;;             |         | label identifiers.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW ==
;; The eights commands servicable to the programmer are listed in the
;; apercu below:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   + cell         | Increments the value of the {cell} by one.
;;     ****         |--------------------------------------------------
;;                  | {cell} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   - cell         | Decrements the value of the {cell} by one.
;;     ****         |--------------------------------------------------
;;                  | {cell} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   " left + right | Increments the value of the cell {left} by the
;;     ****   ***** | value of the cell {right}, modifying only the
;;                  | {left} cell.
;;                  |--------------------------------------------------
;;                  | {left} must be an integer literal or cell
;;                  | reference.
;;                  |--------------------------------------------------
;;                  | {right} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   ' cell         | Queries the standard input for an ASCII character
;;     ****         | character and stores its character code in the
;;                  | {cell}.
;;                  |--------------------------------------------------
;;                  | {cell} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   : cell         | Prints the ASCII character whose code corresponds
;;     ****         | to the value of the {cell} to the standard
;;                  | output.
;;                  |--------------------------------------------------
;;                  | {cell} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   ? left = right | If the value of the cell {left} equals {right},
;;     ****   ***** | skips the next command; otherwise proceeds as
;;                  | usual.
;;                  |--------------------------------------------------
;;                  | {left} must be an integer literal or cell
;;                  | reference.
;;                  |--------------------------------------------------
;;                  | {right} must be an integer literal or cell
;;                  | reference.
;;   ..................................................................
;;   /name          | Declares a label identified by the {name}.
;;    ****          |--------------------------------------------------
;;                  | The {name} must an integer or string literal.
;;   ..................................................................
;;   \name          | Jumps to the position of the label identified by
;;    ****          | the {name}.
;;                  |--------------------------------------------------
;;                  | {x} must be an integer or string literal.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the eath nature of its subject, the llec protolog is inflicted
;; with a few elements of ambiguity, a subset of which shall be the
;; following treatise's cynosure.
;; 
;; == WHICH REGULATIONS APPLY TO LABEL IDENTIFIERS? ==
;; The original specification desists from an accoutrement regarding the
;; goto label's designation; a consectary of this, the requirements for
;; its agnomination remain ensconced in a form of crepuscle.
;; 
;; It has been adjudged, ensuing from experience and the absence of
;; ambiguous inflictions, to homologate both integer numbers and strings
;; composed of letters, decimal digits, and the underscore ("_") to
;; partake in such a composition. The first and third of species may be
;; produced at a name's inchoation, whereas the zero or more subsequent
;; elements will be desumed from the entire treble.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation, reified in the programming language
;; Common Lisp, pursues an epideictic species of telos, conducting its
;; cynosure in an airt accommodated to macros, in particular such whose
;; competences communicate a rudimentary object-oriented alternative,
;; in the guise of "records", and a second forbisen that includes the
;; diorism of typed functions.
;; 
;; == RECORDS: LEIGHTWEIGHT AND SIMPLE STRUCTURES OR CLASSES ==
;; This project's twifaced purpose as both an implementation of the
;; esoteric programming language llec, as well as a vehicle for the
;; loring anenst the handling of macros, and especially that species
;; which capacitates the Common Lisp language's extension by an
;; alternative to structures and classes, appears in the specification
;; of records, such are lightweigth and simplistic simulations of
;; the structured objects commorant in this Lisp dialect's natural
;; circumference.
;; 
;; This record concept amplects the following constituents:
;; 
;;   (1) DISTINGUISHMENT:
;;       Each record type is inhabited by the faculty to be recognized
;;       as such, equipollent to the power of distinguishment from other
;;       record types.
;;   (2) SLOTS:
;;       A record may contain any number of slots, also known as fields
;;       in other programming languages, the same comprehends a treble
;;       componency:
;;         (a) A mandatory name, serving to identity the slot.
;;         (b) The optional type, defaulting to a comprehensive ``T''.
;;         (c) The optional initial value, defaulting to ``NIL''.
;;       Slots may queried as well as modified by their names only.
;;   (3) CONSTRUCTOR:
;;       Every record offers a single constructor based upon keyword
;;       arguments eponymous to the defiend slot nomenclature, enabling
;;       the creation of an arbitrary account of record instances. The
;;       nevening resorts to:
;;         make-<recordName>
;;   (2) ACCESSORS:
;;       For every slot in the record template exist two functions:
;;         (a) A reader, serving to indagate the instance's slot value.
;;         (b) A writer, intended to modify the slot state.
;;       The union of reader and writer is norned an "accessor", and
;;       agnominated by the forbisen
;;         <recordName>-<slotName>
;;       for the reader, and
;;         (setf <recordName>-<slotName>)
;;       in the writer's case.
;; 
;; == RECORD TYPE REQUIREMENTS ==
;; The record species subscribes to a structure or class'
;; approximation, curtailed, however, concerning several significant
;; aspects:
;; 
;;   (1) NO INHERITANCE:
;;       Consanguinous to the concept of structures, but starkly
;;       segregated from that of classes in object-oriented contexts,
;;       records offer no avenue for inheritance relationships.
;;       As such, a record's diorism proceeds from its personal
;;       properties, and none other, without advenient amplification.
;;   (2) NO REFLECTION:
;;       Reflection, as the capacitation of submitting an object's
;;       internal state to indagations, and sometimes even
;;       modifications, embues some object-oriented language's
;;       implements, but shall not be pursued in the record.
;; 
;; A consectary of these identified peculiarities, the competences
;; apportioned to the record as a palpable object of deliberation do
;; not project beyond nominal and formal testaments.
;; 
;; Concretely, the following information ought to be ligated into the
;; such a compound:
;; 
;;   ------------------------------------------------------------------
;;   Datum               | Purpose
;;   --------------------+---------------------------------------------
;;   Name                | Identifies a record type in a unique way,
;;                       | especially in circumstances involving the
;;                       | declaration and ascertainment of a
;;                       | particular species.
;;   ..................................................................
;;   Slot specifications | Enumerates the valid slot names, potentially
;;                       | augmented by type information and an initial
;;                       | value.
;;   ------------------------------------------------------------------
;; 
;; An implication obtained from the above statements, the following
;; definition holds for a record type:
;; 
;;   A record type defines the template for record instances, itself
;;   being composed of its identifying name and a sequences of zero or
;;   more slots, each such mandatorily agnominated by a unique
;;   identifier, an optional type specificier, which resolves to the
;;   comprehensive ``T'' species, and an initial value, the same bears
;;   upon its omission ``NIL''.
;;   
;;   An arbitrary tally of objects, the instances, may be begotten from
;;   the record as the template, requied to at least propagate its name,
;;   for purposes of identification and association, and a leal
;;   respondency to the record type's slot requirements.
;; 
;; == RECORD INSTANCE REQUIREMENTS ==
;; The scion of a record type is denoted as its instance, a tangible and
;; utible object that conforms to the forbisen's nominal and structural
;; regulations, while being described by its personal state.
;; 
;; Maugre its superior proximity to pragmatism, when juxtaposed to the
;; abstract nature of the record type, the instance's circumference does
;; not produce a more intricate outward exhibition, bearing in its
;; official expressions again a twain of constituents:
;; 
;;   ------------------------------------------------------------------
;;   Datum             | Purpose
;;   ------------------+-----------------------------------------------
;;   Record type name  | Equals the record type identifier to whom
;;                     | his instance established a legacy.
;;   ..................................................................
;;   Slot values       | Associates with each slot name from the record
;;                     | type's slot specification a current value,
;;                     | amenable to perquisitions and modifications,
;;                     | and thus already establishing the record
;;                     | instance's state.
;;   ------------------------------------------------------------------
;; 
;; A kenspeckle insight, a record instance's identity does not dependent
;; on any piece of knowledge besides its slot values.
;; 
;; == RECORD INSTANCES STORE ALL TYPE INFORMATION ==
;; The segregative partitioning of classes and their instances into
;; dedicated and independent actors, maugre the sensibility commorant in
;; the augmented coherence and ability for management, would rather tend
;; to accloy the project at hand, the simplistic record type and its
;; didascalic cynosure.
;; 
;; In lieu of this, a conflation of both the record template and its
;; progency is accompassed, approximately kindred to a prototype-based
;; object-oriented flavor. A record instance's germination ensues from
;; the storage of its record type's information, which encompasses only
;; a name and the slot specifications, into the reified object,
;; concluded with the constructor arguments' appilcation in order to
;; build the instance state, that is, the values per slot name.
;; 
;; Proceeding the this abstract conspectuity on the record concept, and
;; the practical simplification adhibited by its template and
;; reification lateralities though their coalescence, the approach of
;; the implementation's manifestation ought to be elucidated.
;; 
;; == LISTS: NATURAL CODE/DATA REPRESENTATIVES ==
;; A paravaunt airt of rumination would aiblins incline the Lisp
;; programmer towards the essential list data structure as the
;; predicament's solution. This mental apercu does not carry a betise's
;; imposition, verily, as Common Lisp's dioristic homoiconicity resides
;; in the selfsame concept.
;; 
;; Homoiconicity, in general diction, appertains to the employment of a
;; particular data structure in order to represent the dependent
;; programming language's programs. For Common Lisp, the requisite
;; reifies in the list structure. A nortelry's cursory application
;; shall serve in educating about this haecceity.
;; 
;; A list of elements can be expressed according to this forbisen:
;; 
;;   '(a 2 "hello")
;; 
;; Please heed the apostrophe which acts to "quote" the list, and thus
;; obviate its evaluation --- the list remains *data*, and does not
;; resolve to *code* in this example.
;; 
;; The following shall be an endeictic vista on a function invocation:
;; 
;;   (format T "hello, ~a" 'world)
;; 
;; Please remark the absence of the quoting apostrophe: This list is
;; evaluated, and thus produces a *code* in lieu of static *data*.
;; 
;; A purlicue of this, code and data are stored both in lists, the
;; common data structure. Common Lisp, thus, is homoiconic.
;; 
;; == LISTS RESPOND IN A KENSBACK MODE AS PARAMETERS ==
;; This gnarity supposedly communicated by the list as a language
;; substrate experiences an attenuation if its docimasy is assayed in
;; respect with the response to applications as a function argument.
;; 
;; In addition to the common expectation of lists to retain their
;; original state succeeding an induction into a function, certain
;; alterations, such as the destructive reversion, may beget perversions
;; in the provenance. For instance, the function
;; 
;;   (defun reverse-the-list (my-list)
;;     (declare (type list my-list))
;;     (setf my-list
;;       (nreverse my-list))
;;     (values))
;; 
;; when applied as
;; 
;;   (let ((my-list (list 1 2 3)))
;;     (declare (type list my-list))
;;     (reverse-the-list my-list)
;;     (the list my-list))
;; 
;; responds with the singleton list
;; 
;;   (1)
;; 
;; instead of the expected
;; 
;;   (3 2 1)
;; 
;; For the didactive purposes of this project, the manifold apertures
;; for such inconsistencies have been supputated as too peisant a
;; cumbrance, and their potential to accable the progress redes the
;; utilization of a different foundational entity in the Lisp family.
;; 
;; == SYMBOLS: IDENTIFIERS AND MAPS ==
;; A further basic component of Lisp accounts for the ``symbol'', a
;; a class of objects accommodated for entities that relate to
;; identifiers in a program, namely, capacitating the declaration of
;; variables and operations, but extended in their avails to
;; incorporate, among others, a property list as a storage for arbitrary
;; information.
;; 
;; The symbols' acquisition for a custom record type's representation,
;; despite its desistence from the list's vantages, conduces several
;; beneficial implications:
;; 
;;   (1) PASSED BY REFERENCE INTO PARAMETER LISTS:
;;       The symbols' induction into a parameter always proceeds in
;;       concord with the by-reference principles, the same serves to
;;       disencumber from the perpensions that a list's peculiar
;;       handling would address.
;;   (2) NATURAL COMPOUNDS:
;;       The symbol as an inherent composite equiparates with the list;
;;       its endowment with an associative structure in the property
;;       list supplements the sanity that slot name-value pairs as
;;       record states would presuppose.
;; 
;; A cursory statement concerning symbols and their --- insufficient ---
;; roles in the fashioning of objects, in counterdistinguishment to
;; CLOS, can be extracted from [stackoverflow2017q42405009].
;; 
;; == SYMBOLS CONTAIN COMPONENTS (CELLS) ==
;; Symbols in Common Lisp permeate the concept of variables and
;; functions and imbue a more generalized perspective on identifiers
;; than in many popular programming language. In concrete diction,
;; symbols establish objects of their own, enjoying a quintuple
;; partition into personal components, or "cells":
;; 
;;   ------------------------------------------------------------------
;;   Symbol component | Description
;;   -----------------+------------------------------------------------
;;   Print name       | A string representing the symbol's name.
;;   ..................................................................
;;   Property list    | The symbol's property list, which comprehends
;;                    | attributes as name-value pairs.
;;   ..................................................................
;;   Package          | The symbol's home package.
;;   ..................................................................
;;   Value binding    | The symbol's value attribute, if bound.
;;   ..................................................................
;;   Function binding | The symbol's function attribute, if bound.
;;   ------------------------------------------------------------------
;; 
;; == THE SYMBOL COMPONENTS MAY BE ACCESSED ==
;; A conclusion of rationality, the components forming a symbol may be
;; accessed following avenues whose mete in dedication collaborates with
;; the restrictions commorant in their diorism; namely, some items
;; homologate a read-only access, which designates a "reader" function,
;; whereas other, in their amplified competence, act in a bivial mode as
;; setters, compounding to "accessor" operations.
;; 
;; This table shall provide the interested party with an overview that
;; juxtaposes the components and their pertinent functions:
;; 
;;   ------------------------------------------------------------------
;;   Symbol component | Access operation | Accessor type
;;   -----------------+------------------+-----------------------------
;;   Print name       | symbol-name      | reader
;;   ..................................................................
;;   Property list    | symbol-plist     | accessor
;;   ..................................................................
;;   Package cell     | symbol-package   | reader
;;   ..................................................................
;;   Value binding    | symbol-value     | accessor
;;   ..................................................................
;;   Function binding | symbol-function  | accessor
;;   ------------------------------------------------------------------
;; 
;; == THE PROPERTY LIST: OUR PROJECT'S CHIEF WARKLOOM ==
;; For our project, the cynosure is occupied by the property list
;; --- a rather parergal tangency appertains to the name, whose wike
;; contributes its affiliation with an automatization in the production
;; of names for variables and functions.
;; 
;; The union of record type and instance information produces the
;; following set of key-value pairs in a symbol appropriated for its
;; employment as a record:
;; 
;;   ------------------------------------------------------------------
;;   Property name     | Purpose
;;   ------------------+-----------------------------------------------
;;   :record-p         | A sentinel, or Boolean flag, which for any
;;                     | record instance is set to ``T'' in order to
;;                     | signify that the instance, being a general
;;                     | symbol in its very haecceity, shall be
;;                     | construed as a record.
;;   ..................................................................
;;   :record-type      | A symbol designating the name of the record
;;                     | type specified during the ``define-record''
;;                     | invocation.
;;   ..................................................................
;;   :record-signature | The slot specifications contributed to the
;;                     | ``define-record'' macro in their verbatim
;;                     | design as one-, two-, or three-element tuples.
;;                     |-----------------------------------------------
;;                     | This piece of information, in conjunction with
;;                     | the ``record-type'' represents a record's
;;                     | blueprint, akin to a class' wike for any of
;;                     | its instances.
;;   ..................................................................
;;   :record-state     | An association list comprehending the record
;;                     | instance's slot information, that is, the
;;                     | slot names and their current values for the
;;                     | instance.
;;                     |-----------------------------------------------
;;                     | Initially, these value assume the defaults
;;                     | provided by the ``:record-signature'' which
;;                     | please see.
;;                     |-----------------------------------------------
;;                     | These slots' castaldy, including indagation
;;                     | manipulation, vindicate the record concept's
;;                     | existence.
;;   ------------------------------------------------------------------
;; 
;; == MACROS AS RECORD DEFINITIONS ==
;; A epitome of this wike's satisfication, this implementation promotes
;; macros to the record definition's ordainment.
;; 
;; The macro facility's participation in Common Lisp accommodates a
;; particularly competent warklume for the assemblage of code which
;; fadges with a specific induction syntax. A concrete diction would
;; relate of its potential to define extensions for the language whose
;; verisimilitude with the inherent traits escape the recognition of
;; the bournes betwixt the Common Lisp standard's repertoire and the
;; user-defined enhancements.
;; 
;; In our case, a macro ``define-record'' is supplied, the same mimics
;; the autochthonous ``defclass'' and ``defstruct'' facilities in its
;; pursuit to establish a record type, originating at the same instant
;; its type specifier, a constructor, getter and setter functions for
;; all of its slots, as well as a dedicated ``with-'' macro commodity
;; for the latter constituents' access in the guise of variables.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-22
;; 
;; Sources:
;;   [esolang2023Llec]
;;   The Esolang contributors, "Llec", May 30th, 2023
;;   URL: "https://esolangs.org/wiki/Llec"
;;   
;;   [LispWorks2005CLHSSymbolsDict]
;;   LispWorks Ltd.,
;;     "The Symbols Dictionary", in "The Common Lisp HyperSpec",
;;     1996--2005
;;   URL: "http://www.lispworks.com/documentation/HyperSpec/Body/
;;         c_symbol.htm"
;;   Notes:
;;     -> Overview of the symbol operations.
;;   
;;   [LispWorks2005CLHSSymbolClass]
;;   LispWorks Ltd.,
;;     "System Class SYMBOL", in "The Common Lisp HyperSpec",
;;     1996--2005
;;   URL: "http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm"
;;   Notes:
;;     - Specification of the system class "symbol".
;;     - Enumerates and describes a symbol's components, also known as
;;       attributes or "cells".
;;   
;;   [stackoverflow2017q42405009]
;;   The Stackoverflow contributors, "The copy-symbol function",
;;     February 23rd, 2017
;;   URL: "https://stackoverflow.com/questions/42405009/
;;         the-copy-symbol-function"
;;   Notes:
;;     - Discusses the "copy-symbol" function.
;;     - Mentions the vinculum between symbols as erstwhile class or
;;       structure representatives and object-oriented programming (OOP)
;;       with CLOS.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (name (candidate-variable &rest parameters)
     &body body)
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,name (,@parameters)
       ,(when (stringp (first body))
          (pop body))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(defmacro define-enumerated-type (name doc-string (&rest members))
  "Defines a derived type, fonded upon the ``member'' predicate, whose
   membership is exhausted by the MEMBERS, and whose intentions may be
   further delineated via a DOC-STRING."
  `(deftype ,name ()
     ,(or doc-string
          (format NIL "The ``~a'' type defines an enumeration." name))
     '(member ,@members)))

;;; -------------------------------------------------------

(define-predicated-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a tuple based upon a list, the
   cardinality of which matches the ELEMENT-TYPES', with each tuple
   element e[i] conforming to the type t[i] from the ELEMENT-TYPES, for
   all i in the range [0, |element-types| - 1]."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length (the list element-types)))
    (loop
      for    element       of-type T in (the list candidate)
      and    expected-type of-type T in (the list element-types)
      always (typep element expected-type))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(define-predicated-type association-list-of
    (candidate &optional (indicator-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, as a list composed of zero or more entries, each such a cons
   whose first compartment conforms to the INDICATOR-TYPE, and whose
   second moeity obeys the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element `(cons ,indicator-type ,value-type)))
      (the list candidate))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(define-predicated-type record (candidate)
  (and
    (symbolp candidate)
    (get candidate :record-p)))

;;; -------------------------------------------------------

(define-predicated-type record-of-type (candidate record-type)
  (and
    (symbolp candidate)
    (get candidate :record-p)
    (eq (get candidate :record-type) record-type)))

;;; -------------------------------------------------------

(deftype slot-specification ()
  '(or
     (tuple-of symbol T T)
     (tuple-of symbol T)
     (tuple-of symbol)))

;;; -------------------------------------------------------

(deftype record-signature ()
  "The ``record-signature'' type defines a record's slot information,
   composed of the slot's name, its type, and an optional default value,
   as a list of zero or more ``slot-specification'' instances."
  '(list-of slot-specification))

;;; -------------------------------------------------------

(deftype record-state ()
  "The ``record-state'' type defines a record's state as an mapping of
   its slot names to their current values, realized as an association
   list (alist) whose key store the slot names, associated with the slot
   values."
  '(association-list-of symbol T))

;;; -------------------------------------------------------

(deftype record-slot ()
  "The ``record-slot'' type defines an entry from a ``record-state'',
   that is, a single slot name-value pair for a record, the entirety of
   which defines the record's state."
  '(cons symbol T))

;;; -------------------------------------------------------

(deftype function-parameter ()
  "The ``function-parameter'' type defines a ``define-function'' input
   as a name-type pair, realized by a tuple of a symbol name and an
   arbitrary type specifier."
  '(tuple-of symbol T))

;;; -------------------------------------------------------

(deftype function-parameter-list ()
  "The ``function-parameter-list'' type defines a parameter list for the
   ``define-function'' macro as a list of ``function-parameter''
   objects."
  '(list-of function-parameter))

;;; -------------------------------------------------------

(define-enumerated-type command-type
  "The ``command-type'' type enumerates the recognized variants of llec
   commands."
  (:increment
   :decrement
   :add
   :declare-label
   :goto-label
   :skip-if-equal
   :input
   :output))

;;; -------------------------------------------------------

(define-enumerated-type operand-type
  "The ``operand-type'' type enumerates the recognized variants of llec
   command operands."
  (:name
   :number
   :reference))

;;; -------------------------------------------------------

(deftype llec-program ()
  "The ``llec-program'' type defines an executable llec program as a
   vector of zero or more ``Command'' objects."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines cell indices an non-negative integer
   numbers in the range [0, +infinity], eligible for referencing memory
   cells."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype llec-object ()
  "The ``llec-object'' type defines the species of objects admissive to
   a llec program's participation, namely, integers and strings, the
   latter of which exclusively relate to label designators."
  '(or integer string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "record" type.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-slot-name (slot)
  "Returns the SLOT specification's name."
  (declare (type slot-specification slot))
  (the symbol
    (first slot)))

;;; -------------------------------------------------------

(defun get-slot-type (slot)
  "Returns the SLOT specification's type."
  (declare (type slot-specification slot))
  (the T
    (or (second slot) 'T)))

;;; -------------------------------------------------------

(defun get-slot-initial-value (slot)
  "Returns the SLOT specification's initial value, or ``NIL'' if none
   such is provided."
  (declare (type slot-specification slot))
  (the T
    (third slot)))

;;; -------------------------------------------------------

(defun build-initial-record-state (slots)
  "Generates and returns the initial state for a record instance
   specified by the SLOTS slot specifications."
  (declare (type record-signature slots))
  (the record-state
    (loop
      for slot of-type slot-specification in slots
      collect
        (cons
          (get-slot-name          slot)
          (get-slot-initial-value slot)))))

;;; -------------------------------------------------------

(defun build-record-instance (record-name slots)
  "Creates and returns an instance of the record type designated by the
   RECORD-NAME and specified in its state by the SLOTS slot
   specifications."
  (declare (type symbol           record-name))
  (declare (type record-signature slots))
  (let ((new-instance (gensym)))
    (declare (type symbol new-instance))
    (setf (get new-instance :record-p)         T)
    (setf (get new-instance :record-type)      record-name)
    (setf (get new-instance :record-signature) slots)
    (setf (get new-instance :record-state)
      (build-initial-record-state slots))
    (the symbol new-instance)))

;;; -------------------------------------------------------

(defun get-record-state (record)
  "Returns the RECORD's state, composed of its slots, maintained in an
   association list that maps the slot names to their current values."
  (declare (type record record))
  (the record-state
    (get record :record-state)))

;;; -------------------------------------------------------

(defun get-record-state-entry (record slot-name)
  "Returns RECORD's state entry for the SLOT-NAME, a cons embracing in
   its sinistral moiety the SLOT-NAME itself, accompanied by its value
   in the RECORD to the right.
   ---
   The thus produced cons is supplied as a direct reference, that is,
   modifications to it propagate into the RECORD's state.
   ---
   An error of an unspecified type is signaled if no entry in the RECORD
   answers to the SLOT-NAME."
  (declare (type record record))
  (declare (type symbol slot-name))
  (the record-slot
    (or
      (assoc slot-name
        (get-record-state record)
        :test #'eq)
      (error "No slot ~s defined for the record." slot-name))))

;;; -------------------------------------------------------

(defun record-slot-value (record slot-name)
  "Returns the value of the slot corresponding to the SLOT-NAME in the
   RECORD's state, or signals an error of an unspecified type if no such
   association exists."
  (declare (type record record))
  (declare (type symbol slot-name))
  (the T
    (cdr (get-record-state-entry record slot-name))))

;;; -------------------------------------------------------

(defun (setf record-slot-value) (new-value record slot-name)
  "Stores the NEW-VALUE in the RECORD's state specified by the
   SLOT-NAME and returns the value itself, or signals an error of an
   unspecified type if no such SLOT-NAME is defined for the RECORD."
  (declare (type T      new-value))
  (declare (type record record))
  (declare (type symbol slot-name))
  (let ((slot-entry (get-record-state-entry record slot-name)))
    (declare (type record-slot slot-entry))
    (setf (cdr slot-entry) new-value))
  (the T new-value))

;;; -------------------------------------------------------

(defun set-record-slots-by-keys (record slot-values)
  "Sets the RECORD's slots as specified by the SLOT-VALUE, each member
   of the latter assumes a two-element list of the slot's name in a
   symbol form, accompanied by the new value, and returns the modified
   RECORD."
  (declare (type record                        record))
  (declare (type (list-of (tuple-of symbol T)) slot-values))
  (loop
    for (slot-name slot-value)
      of-type (symbol T)
      in      slot-values
    do
      (setf (record-slot-value record slot-name) slot-value))
  (the record record))

;;; -------------------------------------------------------

(defun print-record (record &optional (destination T))
  "Prints the RECORD's state to the DESTINATION and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responds with a
   fresh string comprehending the output."
  (declare (type record      record))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        initially
          (format destination "(~:@(~a~)"
            (get record :record-type))
        for (slot-name . slot-value)
          of-type (symbol . T)
          in      (get-record-state record)
        do
          (format destination " (~s ~s)" slot-name slot-value)
        finally
          (format destination ")"))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (print-record record output)))))

;;; -------------------------------------------------------

(defun assemble-symbol (&rest components)
  "Interns and returns a new symbol whose name is assembled from the
   COMPONENTS' concatenated string representations, molded into
   upper-case ere their application."
  (declare (type (list-of T) components))
  (the symbol
    (intern
      (format NIL "~:@(~{~a~}~)" components))))

;;; -------------------------------------------------------

(defun build-constructor-parameters (slots)
  "Generates and returns for the SLOTS a keyword parameter list composed
   of the ``&key'' sentinel succeeded by a two-element list per slot,
   the first member of which bears the slot name, the second its initial
   value."
  (declare (type record-signature slots))
  (the (list-of T)
    (loop
      for     slot of-type slot-specification in slots
      collect (list (get-slot-name          slot)
                    (get-slot-initial-value slot))
      into    parameters
      finally (return (cons '&key parameters)))))

;;; -------------------------------------------------------

(defun get-slots-as-arguments (slots)
  "Returns the SLOTS slot specifications as a list of two-element lists,
   the first moeity of each twain comprehends the quoted slot name,
   associated with the unquoted slot name in its compernage."
  (declare (type record-signature slots))
  (the (list-of T)
    (loop for slot of-type slot-specification in slots collect
      `(list (quote ,(get-slot-name slot))
             ,(get-slot-name slot)))))

;;; -------------------------------------------------------

(defun build-constructor-declarations (slots)
  "Creates and returns a list of ``type'' and ``ignorable'' declarations
   for the constructor parameter list involving the SLOTS."
  (declare (type (list-of T) slots))
  (the (list-of T)
    (loop
      for slot of-type slot-specification in slots
      collect
        `(declare (type ,(or (second slot) 'T) ,(first slot)))
      collect
        `(declare (ignorable ,(first slot))))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-bindings (record-name subject-name slots)
  "Generates and returns the symbol macrolet bindings for the record
   whose type is designated by the RECORD-NAME, the record instance's
   agnomination being defined by the SUBJECT-NAME, and the available
   slot specifications issued via the SLOTS."
  (declare (type symbol                       record-name))
  (declare (type symbol                       subject-name))
  (declare (type (list-of slot-specification) slots))
  (the (list-of T)
    (loop
      for slot
        of-type slot-specification
        in      slots
      for binding-name
        of-type symbol
        =       (assemble-symbol subject-name "-"
                  (get-slot-name slot))
      for accessor-name
        of-type symbol
        =       (assemble-symbol record-name "-"
                  (get-slot-name slot))
      collect
        `(,binding-name
           (the ,(get-slot-type slot)
             (,accessor-name ,subject-name))))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-declarations (subject-name slots)
  "Generates and returns for the record, designated by the SUBJECT-NAME,
   and its slot specifications SLOTS, the symbol macrolets' ``declare''
   declarations."
  (declare (type symbol                       subject-name))
  (declare (type (list-of slot-specification) slots))
  (the (list-of T)
    (loop
      for slot
        of-type slot-specification
        in      slots
      for binding-name
        of-type symbol
        =       (assemble-symbol subject-name "-"
                  (get-slot-name slot))
      collect `(declare (type ,(get-slot-type slot) ,binding-name))
      collect `(declare (ignorable                  ,binding-name)))))

;;; -------------------------------------------------------

(defmacro define-record (name doc-string (&rest slots))
  "Defines a new record type, identified by the NAME, and elucidated by
   mediation of the DOC-STRING, the template of which is founded upon
   the SLOTS, with each such item being addressable for indagation and
   modification by a dedicated accessor, while an instantiation of this
   record proceeds by an also generated constructor function.
   ---
   In addition to the derived type, its constructor, and slot accessors,
   a print operation for output pursuits, and a ``with-'' macro for
   facilitated slot operations is contributed."
  (let ((with-macro-name (assemble-symbol "WITH-" name)))
    (declare (type symbol with-macro-name))
    `(progn
       ;; Define a new derived type for the record NAME.
       (deftype ,name ()
         ,(or doc-string
              (format NIL "A record of the type ``~a''." name))
         '(record-of-type ,name))
       
       ;; Build the record's constructor function.
       (defun ,(assemble-symbol "MAKE-" name)
              (,@(build-constructor-parameters slots))
         ,(format NIL "Creates and returns a fresh instance of the ~
                       ~:@(~a~) record type."
            name)
         ,@(build-constructor-declarations slots)
         (the ,name
           (set-record-slots-by-keys
             (build-record-instance ',name ',slots)
             (list ,@(get-slots-as-arguments slots)))))
       
       ;; Build the record's print function.
       (defun ,(assemble-symbol "PRINT-" name)
              (record &optional (destination T))
         "Prints the RECORD's state to the DESTINATION and returns for a
          non-``NIL'' DESTINATION the ``NIL'' value, otherwise responds
          with a fresh string comprehending the output."
         (declare (type ,name       record))
         (declare (type destination destination))
         (the (or null string)
           (print-record record destination)))
       
       ;; Build the record's slot getter and setter functions.
       ,@(loop
          for slot of-type slot-specification in slots
          collect
            ;; Build the SLOT's getter function.
            `(defun ,(assemble-symbol name "-" (first slot))
                    (record)
               (declare (type ,name record))
               (the ,(get-slot-type slot)
                 (record-slot-value record
                   ',(first slot))))
          collect
            ;; Build the SLOT's setter function.
            `(defun (setf ,(assemble-symbol name "-" (first slot)))
                    (new-value record)
               (declare (type ,(get-slot-type slot) new-value))
               (declare (type ,name                 record))
               (the ,(get-slot-type slot)
                 (setf (record-slot-value record ',(first slot))
                       new-value))))
       
       ;; Build the record's "with-" macro.
       (defmacro ,with-macro-name
           ((subject &optional (subject-name ',name))
            &body body)
         `(let ((,subject-name ,subject))
            (declare (type ,(quote ,name) ,subject-name))
            (declare (ignorable           ,subject-name))
            (symbol-macrolet
                (,@(build-symbol-macrolet-bindings
                     (quote ,name)
                     `,subject-name
                     '(,@slots)))
              ,@(build-symbol-macrolet-declarations
                  `,subject-name
                  '(,@slots))
              ,@body))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of custom function generator.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-parameter-names (parameters)
  "Extracts from the PARAMETERS the parameter names and returns a list
   comprehending these."
  (declare (type function-parameter-list parameters))
  (the (list-of symbol)
    (mapcar #'first parameters)))

;;; -------------------------------------------------------

(defun extract-parameter-types (parameters)
  "Extracts from the PARAMETERS the parameter types and returns a list
   comprehending these."
  (declare (type function-parameter-list parameters))
  (the (list-of T)
    (mapcar #'second parameters)))

;;; -------------------------------------------------------

(defun build-parameter-declarations (parameters)
  "Generates and returns for the PARAMETERS the ``declare''
   declarations."
  (declare (type function-parameter-list parameters))
  (the (list-of T)
    (mapcan
      #'(lambda (parameter)
          (declare (type function-parameter parameter))
          (list
            `(declare (type ,(second parameter) ,(first parameter)))
            `(declare (ignorable                ,(first parameter)))))
      parameters)))

;;; -------------------------------------------------------

(defmacro define-function (name parameters &body body)
  "Defines a ``defun''-based function, nevend by the NAME, the inputs
   of which are derived by the PARAMETERS, each such a two-element list
   of name and type, the function forms being specified by the BODY,
   the desinent form's results of which are being returned."
  `(defun ,name (,@(extract-parameter-names parameters))
     ,(if (stringp (first body))
        (pop body)
        "")
     ,@(build-parameter-declarations parameters)
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of command type table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function get-command-type ((identifier character))
  "Returns the command type allied with the IDENTIFIER, or signals an
   error of an unspecified type upon its disrespondency."
  (the command-type
    (case identifier
      (#\+ :increment)
      (#\- :decrement)
      (#\" :add)
      (#\/ :declare-label)
      (#\\ :goto-label)
      (#\? :skip-if-equal)
      (#\' :input)
      (#\: :output)
      (otherwise
        (error "Unrecognized command identifier: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function whitespace-character-p ((candidate character))
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(define-function sign-character-p ((candidate character))
  "Determines whether the CANDIDATE represents a mathematical sign, that
   is, either plus (\"+\") or minus (\"-\"), returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(define-function identifier-first-character-p ((candidate character))
  "Determines whether the CANDIDATE represents an identifier's first
   constituent, permissive either in a label name or an integer number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (find candidate "+-_$%" :test #'char=))))))

;;; -------------------------------------------------------

(define-function identifier-character-p ((candidate character))
  "Determines whether the CANDIDATE represents an identifier
   constituent, permissive either in a label name or an integer number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (find candidate "-_$%" :test #'char=))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-word (word)
  "Parses the WORD and returns a value of the most eligible type, either
   a signed integer number or, if failing this attempt, the WORD in its
   verbatim string form."
  (declare (type string word))
  (the (or integer string)
    (handler-case
      (parse-integer word)
      (error ()      word))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Operand
  "The ``Operand'' record type serves in the representation of a llec
   instruction operand, categorized according to its nature by its type,
   and detailed through its value."
  ((type  keyword (error "Missing operand type."))
   (value T       (error "Missing operand value."))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Command
  "The ``Command'' record type serves in the representaiton of a llec
   instruction, compact of its type and one or two operands."
  ((type           command-type      (error "Missing command type."))
   (first-operand  (or null Operand) NIL)
   (second-operand (or null Operand) NIL)))

;;; -------------------------------------------------------

(defun ascertain-numeric-operand (operand)
  "Determines whether the OPERAND represents a numeric specimen, that
   is, either an integer literal or a cell reference, on confirmation
   returning the probed OPERAND itself, otherwise signaling an error of
   an unspecified type."
  (declare (type Operand operand))
  (the Operand
    (case (operand-type operand)
      ((:number :reference) operand)
      (otherwise
        (error "The object ~s does not constitute ~
                a numeric operand."
          (print-operand operand NIL))))))

;;; -------------------------------------------------------

(defun ascertain-label-operand (operand)
  "Determines whether the OPERAND represents label identifier conformant
   specimen, that is, either an integer literal or a name, on
   confirmation returning the probed OPERAND itself, otherwise signaling
   an error of an unspecified type."
  (declare (type Operand operand))
  (the Operand
    (case (operand-type operand)
      ((:name :number) operand)
      (otherwise
        (error "The object ~s does not constitute ~
                a label-conformant operand."
          (print-operand operand NIL))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Scanner
  "The ``Scanner'' record type provides an entity responsible for a
   piece of llec source code string's partition into significant
   objects, most significantly commands and operands."
  ((source    string              (error "Missing source."))
   (position  fixnum              0)
   (character (or null character) NIL)))

;;; -------------------------------------------------------

(defun initialize-scanner (source)
  "Creates and returns a new ``Scanner'' which operates on the SOURCE."
  (declare (type string source))
  (let ((scanner (make-scanner :source source)))
    (declare (type Scanner scanner))
    (with-scanner (scanner)
      (setf scanner-character
        (when (array-in-bounds-p scanner-source scanner-position)
          (char scanner-source scanner-position))))
    (the Scanner scanner)))

;;; -------------------------------------------------------

(defun scanner-advance (scanner)
  "Advances the SCANNER's position cursor to the next character in its
   source, if possible, and returns the modified SCANNER."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (setf scanner-character
      (when (array-in-bounds-p scanner-source (1+ scanner-position))
        (char scanner-source
          (incf scanner-position)))))
  (values))

;;; -------------------------------------------------------

(defun scanner-skip-whitespaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more accolent whitespaces, and returns the
   modified SCANNER."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop
      while (and scanner-character
                 (whitespace-character-p scanner-character))
      do    (scanner-advance scanner)))
  (values))

;;; -------------------------------------------------------

(defun scanner-read-number (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a signed or unsigned integer number and returns its parsed value."
  (declare (type Scanner scanner))
  (scanner-skip-whitespaces scanner)
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (with-scanner (scanner my)
          (loop
            initially
              (when (and my-character
                         (sign-character-p my-character))
                (write-char my-character digits)
                (scanner-advance scanner))
            while
              (and my-character
                   (digit-char-p my-character))
            do
              (write-char my-character digits)
              (scanner-advance scanner)))))))

;;; -------------------------------------------------------

(defun scanner-read-word (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a single word, demarcated by whitespaces or the end of the source,
   and returns a string representation thereof."
  (declare (type Scanner scanner))
  (the string
    (with-output-to-string (word)
      (declare (type string-stream word))
      (with-scanner (scanner $my)
        (loop
          while
            (and $my-character
                 (identifier-character-p $my-character))
          do
            (write-char $my-character word)
            (scanner-advance scanner))))))

;;; -------------------------------------------------------

(defun scanner-read-operand (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a command operand and returns an ``Operand'' record representation
   thereof."
  (declare (type Scanner scanner))
  (scanner-skip-whitespaces scanner)
  (the Operand
    (with-scanner (scanner)
      (cond
        ((null scanner-character)
          (error "Expected an operand at position ~d, but found the ~
                  source exhausted."
            scanner-position))
        ((char= scanner-character #\*)
          (scanner-advance scanner)
          (make-operand :type :reference :value
            (scanner-read-number scanner)))
        ((identifier-first-character-p scanner-character)
          (let ((word (parse-word
                        (scanner-read-word scanner))))
            (declare (type (or integer string) word))
            (typecase word
              (integer
                (make-operand :type :number :value word))
              (string
                (make-operand :type :name :value word))
              (otherwise
                (error "Invalid operator word: ~s." word)))))
        (T
          (error "The character \"~c\", encountered at the position ~d,
                  does not introduce an operand."
            scanner-character scanner-position))))))

;;; -------------------------------------------------------

(defun scanner-expect-character (scanner expected-character)
  "Determines whether the SCANNER's current character equals the
   EXPECTED-CHARACTER, on confirmation advancing to the next position in
   its source, otherwise signaling an error of an unspecified type."
  (declare (type Scanner   scanner))
  (declare (type character expected-character))
  (scanner-skip-whitespaces scanner)
  (with-scanner (scanner $this)
    (cond
      ((null $this-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but found the source exhausted."
          expected-character $this-position))
      ((char/= $this-character expected-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-character $this-position $this-character))
      (T
        (scanner-advance scanner))))
  (values))

;;; -------------------------------------------------------

(defun scanner-read-add-command (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   an addition instruction and returns a ``Command'' representation
   thereof."
  (declare (type Scanner scanner))
  (let ((left-operand (scanner-read-operand scanner)))
    (declare (type Operand left-operand))
    (scanner-expect-character scanner #\+)
    (let ((right-operand (scanner-read-operand scanner)))
      (declare (type Operand right-operand))
      (the Command
        (make-command
          :type           :add
          :first-operand  (ascertain-numeric-operand left-operand)
          :second-operand (ascertain-numeric-operand right-operand))))))

;;; -------------------------------------------------------

(defun scanner-read-skip-if-equal-command (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a conditional skip instruction and returns a ``Command''
   representation thereof."
  (declare (type Scanner scanner))
  (let ((left-operand (scanner-read-operand scanner)))
    (declare (type Operand left-operand))
    (scanner-expect-character scanner #\=)
    (let ((right-operand (scanner-read-operand scanner)))
      (declare (type Operand right-operand))
      (the Command
        (make-command
          :type           :skip-if-equal
          :first-operand  (ascertain-numeric-operand left-operand)
          :second-operand (ascertain-numeric-operand right-operand))))))

;;; -------------------------------------------------------

(defun scanner-read-unary-command (scanner command-type)
  "Proceeding from the current position into the SCANNER's source, reads
   a unary, non-label llec instruction, distinguished on its
   COMMAND-TYPE, and returns a ``Command'' representation thereof."
  (declare (type Scanner      scanner))
  (declare (type command-type command-type))
  (the Command
    (make-command
      :type          command-type
      :first-operand (ascertain-numeric-operand
                       (scanner-read-operand scanner)))))

;;; -------------------------------------------------------

(defun scanner-read-label-command (scanner command-type)
  "Proceeding from the current position into the SCANNER's source, reads
   a label-based llec instruction, distinguished on its COMMAND-TYPE,
   and returns a ``Command'' representation thereof."
  (declare (type Scanner      scanner))
  (declare (type command-type command-type))
  (the Command
    (make-command
      :type          command-type
      :first-operand (ascertain-label-operand
                       (scanner-read-operand scanner)))))

;;; -------------------------------------------------------

(defun scanner-read-command (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a llec instruction and returns a ``Command'' representation thereof."
  (declare (type Scanner scanner))
  (scanner-skip-whitespaces scanner)
  (the Command
    (with-scanner (scanner this)
      (let ((command-type (get-command-type this-character)))
        (declare (type command-type command-type))
        (case command-type
          (:add
            (scanner-advance scanner)
            (scanner-read-add-command scanner))
          (:skip-if-equal
            (scanner-advance scanner)
            (scanner-read-skip-if-equal-command scanner))
          ((:declare-label :goto-label)
            (scanner-advance scanner)
            (scanner-read-label-command scanner command-type))
          ((:increment :decrement :input :output)
            (scanner-advance scanner)
            (scanner-read-unary-command scanner command-type))
          (otherwise
            (error "Unrecognized command identifier \"~c\" ~
                    encountered at position ~d."
              this-character
              this-position)))))))

;;; -------------------------------------------------------

(defun scanner-extract-commands (scanner)
  "Extracts the Llec commands from the SCANNER's source and returns a
   one-dimensional simple array entailing the same."
  (declare (type Scanner scanner))
  (the llec-program
    (coerce
      (with-scanner (scanner)
        (loop
          initially
            (scanner-skip-whitespaces scanner)
          while scanner-character
          collect
            (prog1
              (scanner-read-command scanner)
              (scanner-skip-whitespaces scanner))))
      '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Label-Table
  "The ``Label-Table'' record type defines a mapping of label names to
   their positions in a llec command sequence."
  ((labels (hash-table-of T fixnum)
           (make-hash-table :test #'equal))))

;;; -------------------------------------------------------

(define-function label-table-register ((label-table Label-Table)
                                       (label-name  T)
                                       (position    fixnum)) 
  "Associates the LABEL-NAME with its command representation's POSITION
   in a llec program, stores the compound in the LABEL-TABLE, and
   returns no value."
  (setf (gethash label-name
          (label-table-labels label-table))
        position)
  (values))

;;; -------------------------------------------------------

(define-function build-label-table ((program llec-program))
  "Generates and returns for the llec PROGRAM a ``Label-Table'' which
   maps its label names to the declaration positions in the PROGRAM."
  (let ((labels (make-label-table)))
    (declare (type Label-Table labels))
    (loop
      for command  of-type Command across program
      and position of-type fixnum  from   0 by 1
      when (eq (command-type command) :declare-label) do
        (label-table-register labels
          (operand-value
            (command-first-operand command))
          position))
    (the Label-Table labels)))

;;; -------------------------------------------------------

(define-function label-table-get-destination ((label-table Label-Table)
                                              (label-name  T))
  "Returns the position associated with the LABEL-NAME in the
   LABEL-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (the fixnum
    (with-label-table (label-table my)
      (or (gethash label-name my-labels)
          (error "No destination defined for the label ~s."
            label-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Memory
  "The ``Memory'' record type defines the program memory as an expanse
   of integer-valued cells, amenable to zero-based indices, and infinite
   towards the subscripts' upper march."
  ((cells (hash-table-of cell-index integer)
          (make-hash-table :test #'eql))))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the value stored in the MEMORY cell at the INDEX."
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (the integer
    (gethash index
      (memory-cells memory)
      0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell at the INDEX and returns no
   value."
  (declare (type integer    new-value))
  (declare (type Memory     memory))
  (declare (type cell-index index))
  (setf (gethash index
          (memory-cells memory)
          0)
        new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record Interpreter
  "The ``Interpreter'' record type describes a entity responsible for
   the application of effect to a parsed llec program."
  ((program llec-program (error "Missing program."))
   (ip      fixnum       0)
   (labels  Label-Table  (make-label-table))
   (memory  Memory       (make-memory))))

;;; -------------------------------------------------------

(defun initialize-interpreter (program)
  "Creates and returns an ``Interpreter'' which operates on the llec
   PROGRAM."
  (declare (type llec-program program))
  (the Interpreter
    (make-interpreter
      :program program
      :labels  (build-label-table program))))

;;; -------------------------------------------------------

(define-function interpreter-exhausted-p ((interpreter Interpreter))
  "Determines whether the INTERPRETER's program is exhausted, that is,
   its instruction pointer (IP) has bestridden the desinent command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (with-interpreter (interpreter my)
      (>= my-ip
          (length my-program)))))

;;; -------------------------------------------------------

(define-function interpreter-current-command ((interpreter Interpreter))
  "Returns the INTERPRETER's currently processed command."
  (the Command
    (with-interpreter (interpreter my)
      (aref my-program my-ip))))

;;; -------------------------------------------------------

(define-function interpreter-advance ((interpreter Interpreter))
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program and returns no value."
  (with-interpreter (interpreter)
    (unless (interpreter-exhausted-p interpreter)
      (incf interpreter-ip)))
  (values))

;;; -------------------------------------------------------

(define-function interpreter-go-to ((interpreter  Interpreter)
                                    (new-position fixnum))
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   NEW-POSITION in its program and returns no value."
  (setf (interpreter-ip interpreter)
        new-position)
  (values))

;;; -------------------------------------------------------

(define-function interpreter-resolve-operand ((interpreter Interpreter)
                                              (operand     Operand))
  "Obtains and returns the OPERAND's value in the INTERPRETER's
   context."
  (the llec-object
    (case (operand-type operand)
      (:number
        (operand-value operand))
      (:reference
        (memory-cell-at (interpreter-memory interpreter)
          (operand-value operand)))
      (:name
        (operand-value operand))
      (otherwise
        (error "Cannot get the value of the operand ~s." operand)))))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-command (interpreter
                                          command-type
                                          command)
  (:documentation
    "Processes the COMMAND, identified by and dispatched on its
     COMMAND-TYPE, in the INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(define-function interpreter-process-command ((interpreter Interpreter)
                                              (command     Command))
  "Processes the COMMAND in the INTERPRETER's context and returns no
   value.
   ---
   Internally, this operation invokes the most eligible implementation
   of the generic function ``interpreter-dispatch-command'', dispatching
   on the COMMAND's type."
  (interpreter-dispatch-command interpreter
    (command-type command)
    command)
  (values))

;;; -------------------------------------------------------

(define-function interpreter-on-label-declaration-p
    ((interpreter interpreter))
  "Determines whether the INTERPRETER's current command constitutes a
   label declaration, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (and (not (interpreter-exhausted-p interpreter))
           (eq  (command-type
                  (interpreter-current-command interpreter))
                :declare-label))))))

;;; -------------------------------------------------------

(define-function interpreter-skip-label-declarations
    ((interpreter Interpreter))
  "Proceeding from the INTERPRETER's current command, skips a sequence
   of zero or more accolent label declaration instructions, and returns
   no value."
  (loop
    while (interpreter-on-label-declaration-p interpreter)
    do    (interpreter-advance interpreter))
  (values))

;;; -------------------------------------------------------

(define-function interpreter-skip-next-command
    ((interpreter Interpreter))
  "Skips the next command in the INTERPRETER's maintained program,
   omitting label declarations from this account, and returns no value."
  (interpreter-advance interpreter)
  (when (interpreter-on-label-declaration-p interpreter)
    (interpreter-skip-label-declarations interpreter))
  (values))

;;; -------------------------------------------------------

(defmacro define-command-dispatch
    (command-type (interpreter-variable command-variable)
     &body body)
  "Defines an implementation of the generic function
   ``interpreter-dispatch-command'', with the first parameter norned by
   the INTERPRETER-VARIABLE, the second provided automatically, and the
   third tantamount to the COMMAND-VARIABLE, empighting the BODY forms
   for its operations, concluding no no return value."
  (let ((command-type-variable (gensym)))
    (declare (type symbol command-type-variable))
    `(defmethod interpreter-dispatch-command
         ((,interpreter-variable  symbol)
          (,command-type-variable (eql ,command-type))
          (,command-variable      symbol))
       (declare (type Interpreter  ,interpreter-variable))
       (declare (ignorable         ,interpreter-variable))
       (declare (type command-type ,command-type-variable))
       (declare (ignore            ,command-type-variable))
       (declare (type Command      ,command-variable))
       (declare (ignorable         ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-command-dispatch :increment (interpreter command)
  (incf
    (memory-cell-at (interpreter-memory interpreter)
      (interpreter-resolve-operand interpreter
        (command-first-operand command)))))

;;; -------------------------------------------------------

(define-command-dispatch :decrement (interpreter command)
  (decf
    (memory-cell-at (interpreter-memory interpreter)
      (interpreter-resolve-operand interpreter
        (command-first-operand command)))))

;;; -------------------------------------------------------

(define-command-dispatch :add (interpreter command)
  (with-interpreter (interpreter)
    (incf (memory-cell-at interpreter-memory
            (interpreter-resolve-operand interpreter
              (command-first-operand command)))
          (memory-cell-at interpreter-memory
            (interpreter-resolve-operand interpreter
              (command-second-operand command))))))

;;; -------------------------------------------------------

(define-command-dispatch :input (interpreter command)
  (format T "~&>> ")
  (finish-output)
  (with-interpreter (interpreter)
    (setf (memory-cell-at interpreter-memory
            (interpreter-resolve-operand interpreter
              (command-first-operand command)))
          (char-code
            (read-char NIL NIL #\Null))))
  (clear-input))

;;; -------------------------------------------------------

(define-command-dispatch :output (interpreter command)
  (with-interpreter (interpreter)
    (format T "~C"
      (code-char
        (memory-cell-at interpreter-memory
          (interpreter-resolve-operand interpreter
            (command-first-operand command)))))))

;;; -------------------------------------------------------

(define-command-dispatch :skip-if-equal (interpreter command)
  (when (= (memory-cell-at
             (interpreter-memory interpreter)
             (interpreter-resolve-operand interpreter
               (command-first-operand command)))
           (interpreter-resolve-operand interpreter
             (command-second-operand command)))
    (interpreter-skip-next-command interpreter)))

;;; -------------------------------------------------------

(define-command-dispatch :declare-label (interpreter command)
  NIL)

;;; -------------------------------------------------------

(define-command-dispatch :goto-label (interpreter command)
  (interpreter-go-to interpreter
    (label-table-get-destination
      (interpreter-labels interpreter)
      (interpreter-resolve-operand interpreter
        (command-first-operand command)))))

;;; -------------------------------------------------------

(define-function interpreter-execute ((interpreter Interpreter))
  (with-interpreter (interpreter my)
    (loop until (interpreter-exhausted-p interpreter) do
      (interpreter-process-command interpreter
        (interpreter-current-command interpreter))
      (interpreter-advance interpreter)))
  (values))

;;; -------------------------------------------------------

(define-function interpret-llec ((code string))
  "Interprets the piece of llec source CODE and returns no value."
  (interpreter-execute
    (initialize-interpreter
      (scanner-extract-commands
        (initialize-scanner code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-llec
  "/loop
   '0
   :0
   \\loop")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; The following procedure, limned in pseudocode, holds:
;; 
;;   memory[0] <- input           { = 48 or 49   }
;;   memory[1] <- memory[0]       { = 48 or 49   }
;;   memory[1] <- memory[1] - 48  { = 0 or 1     }
;;   
;;   declare label "print_1"      { Used for repeated display of "1" }
;;   
;;   print memory[0]              { = "0" or "1" }
;;   
;;   if memory[1] = 0 then
;;     skip upcoming goto command
;;   end if
;;   
;;   go to label "print_1"
(interpret-llec
  "'0
   \"1+0
   -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1
   /print_1
   :0
   ?1=0
   \\print_1
   ")
