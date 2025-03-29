;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Nybblang", invented by the Esolang user
;; "PythonshellDebugwindow" and presented on February 11th, 2020, the
;; commorancy of its kenspeckle proprium maintained in a stack composed
;; of four-bit composites, or nybbles, on which, in the language's
;; standard variation, programs of four bits' capacity, resident in the
;; respective source files, or, in the putatively Turing-complete
;; rendition, an arbitrary binary program, operate.
;; 
;; 
;; Concept
;; =======
;; The Nybblang programming language constitutes a specimen operating
;; on four-bit compounds, or nybbles, by adminiculum of a stack and a
;; buffer whose efficacy applies to an intermediate state betwixt the
;; bits' assemblage into a nybble and the latter's admission to the
;; stack, operated upon by commands encoded in twains of two bits.
;; 
;; == STANDARD NYBBLANG: MANY PROGRAMS IN COMPOUNDS OF FOUR BITS ==
;; A standard Nybblang program's conformation does not apply tolerance
;; to any other content than an exact quadruple tally of binary digits,
;; each jumelle of subsequent symbols coalescing into an aefauld
;; instruction.
;; 
;; A corollary ensuing from this restriction, a separate Nybblang
;; program enumerates exactly two commands, an imposition alleviated by
;; the contingency to request an import of the next program via a
;; dedicated operation.
;; 
;; The standard Nybblang interpreter is expected to accept either an
;; ordered sequence of Nybblang files or a directory comprehend such,
;; their occurrency's embodiment that of a circular list, with an
;; ultimity of the desinent program's import an iterum commencement from
;; the first specified member.
;; 
;; The interpreter invocation's forbisen thus is limned in the
;; following, with the succedaneous segments' demarcation installed in
;; a jumelle of braces ("{...}"):
;; 
;;   nybblang {file-1} {file-2} ... {file-N}
;; 
;; where {file-1} establishes the entry point into the application.
;; 
;; A concrete example illustrates the concept:
;; 
;;   nybblang main.nyb sub1.nyb sub2.nyb
;; 
;; The provision of a directory in the file listing's stead assumes the
;; guise:
;; 
;;   nybblang {directory}
;; 
;; An example adduced:
;; 
;;   nybblang my/directory
;; 
;; == "JUMPING NYBBLANG": GOTO AS AN ATTEMPT AT TURING-COMPLETENESS ==
;; An extended variant of Nybblang, a derivation whose formation targets
;; the desiderated, yet not proven, contingency for Turing-completeness,
;; exists --- here apportioned the agnomination "Jumping Nybblang",
;; educed by its kenspeckle modulation from the original designment.
;; 
;; This language's reliance upon a single program rather than a sequence
;; thereof incorporates as a concomitant the homologation of a
;; bourneless amount of instructions to furnish its constituents, again,
;; of course, expressed as binary symbols.
;; 
;; The now superfluous program import facility transforms through a
;; supersession into a conditional goto mechanism, selecting the next
;; command based upon a one-indexed target designator.
;; 
;; == THE NYBBLANG MEMORY: A NYBBLE-VALUED STACK AND A BUFFER ==
;; An adminicle to the paravaunt memory component, a stack capacitated
;; to maintain nybbles as its elements, an intermediate and provisional
;; storage applies itself to the four requisite bits' collation, ere
;; the compound's patration is conveyed to the stack.
;; 
;; == THE TEMPORARY VARIABLE: A NYBBLE BUFFER FOR THE STACK ==
;; The insertion into the paravaunt memory component, the stack,
;; constitutes in a most abstract application of one's conspectuity,
;; a twiformed process: Imprimis, a four-bit buffer's nurture ought to
;; be driven to surfeiture, ere the thus formed nybble transfers onto
;; the stack top, administering as the obbligatio the epiphenomenon of
;; the buffer's restoration to the inchoate state.
;; 
;; The appertaining principle's notion shall enjoy, for the sake of an
;; enhanced mete of nortelry, the following listing's illustration:
;; 
;;   (0) The temporary variable starts in the decimal state of
;;       zero (0), tantamount to four zero-valued bits, or one
;;       zero-nybble:
;;         0000
;;       For our illustrations, we designate this incomplete, or any
;;       partially defined state, for this matter, by hyphens ("-"), as
;;       a distinguishment betwixt a completed zero-valued variable and
;;       an yet depleted state carries essential ponderance; hence, our
;;       initial variable nybble bears:
;;         ----
;;   
;;   (1) If a "push" instruction is issued, that is, either "00" or
;;       "01", the committed binary digit, zero (0) or one (1), is
;;       transferred into the temporary variable's least significant
;;       position.
;;       Given a digit twain "a" to push, this first behest sets the
;;       initially blank variable nybble
;;         ----
;;       to
;;         ---a
;;   
;;   (2) If a second "push" instruction is imposed, the new binary digit
;;       now occupies the next higher variable location.
;;       As a forbisen, the digit "b" renders the hitherto variable
;;       state
;;         ---a
;;       to
;;         --ba
;;   
;;   (3) The process continues for the next "push", here with a
;;       succedaneum nevened "c", that occupies the next higher bit
;;       position, extending
;;         --ba
;;       to
;;         -cba
;;   
;;   (4) A fourth "push" instruction serves to specify the highest
;;       nybble location. Given the bit "d" to insert, we arrive from
;;         -cba
;;       at the surfeited
;;         dcba
;;   
;;   (5) Immediately ensuing from this act of patration in point (4),
;;       the variable value, now composed of an entire four-bit nybble
;;       datum, is pushed unto the stack of nybbles. As a consequence,
;;       the variable itself is reset to its blank inchoate form
;;         0000
;;       or, as limned by us:
;;         ----
;;       This returns us to the conceptual situation specified in
;;       step (0), as a parasceuastic administration for a contingent
;;       future cycle.
;; 
;; == THE STACK: A THEORETICALLY INFINITE COLLECTION OF NYBBLES ==
;; The stack serves as the ultimate receiver of the respective behest's
;; bits in the form of a nybbles, their introduction, proceeding
;; serelepes until a quadruple whole's edification, consigned to the
;; parhedral nybble buffer's bailiwick.
;; 
;; == NYBBLANG CAN MERELY PRINT 16 DIFFERENT SYMBOLS ==
;; Nybblang's printable alphabet is composed of sixteen (16) symbols,
;; enumerated via a zero-based indexing scheme that, as a corollary,
;; covers the closed integer range [0, 15].
;; 
;; The following table enumerates the symbols in juxtaposition with
;; their locations in the alphabet:
;; 
;;   --------------------
;;   Position | Symbol
;;   ---------+----------
;;    0       | E
;;   ....................
;;    1       | T
;;   ....................
;;    2       | A
;;   ....................
;;    3       | O
;;   ....................
;;    4       | I
;;   ....................
;;    5       | N
;;   ....................
;;    6       | S
;;   ....................
;;    7       | R
;;   ....................
;;    8       | H
;;   ....................
;;    9       | D
;;   ....................
;;   10       | L
;;   ....................
;;   11       | (space)
;;   ....................
;;   12       | W
;;   ....................
;;   13       | U
;;   ....................
;;   14       | .
;;   ....................
;;   15       | (newline)
;;   --------------------
;; 
;; 
;; Instructions
;; ============
;; The sole criterion of this distinguishment, Nybblang's instruction
;; set bifurcates into the quadruple standard contingency at an
;; equinumerant alternative to the "jumping" variant, the latter of
;; whose divergence reified merely in the "11" command, dedicated, in
;; lieu of the original program import, to a conditional goto facility.
;; 
;; == STANDARD NYBBLANG: OVERVIEW ==
;; The following tabular exposition's onus shall be the standard
;; Nybblang variation operations' presentation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   00      | Pushes the number zero (0) to the memory.
;;           |---------------------------------------------------------
;;           | Please note that, as a first step, the number is
;;           | transferred into the temporary nybble-valued variable as
;;           | the bit zero (0), occupying the next free position in
;;           | nybble buffer, which proceeds from the least to the most
;;           | significant bit.
;;           | Upon this buffer's patration to four bits, the thus
;;           | resulting nybble is actually pushed onto the stack top,
;;           | purging and reseting the buffer in a epiphenomenal step.
;;           | Please consult the section "Architecture" for further
;;           | information on this subject.
;;   ..................................................................
;;   01      | Pushes the number one (1) to the memory.
;;           |---------------------------------------------------------
;;           | Please note that, as a first step, the number is
;;           | transferred into the temporary nybble-valued variable as
;;           | the bit one (1), occupying the next free position in
;;           | nybble buffer, which proceeds from the least to the most
;;           | significant bit.
;;           | Upon this buffer's patration to four bits, the thus
;;           | resulting nybble is actually pushed onto the stack top,
;;           | purging and reseting the buffer in a epiphenomenal step.
;;           | Please consult the "Concept" subsection "THE TEMPORARY
;;           | VARIABLE: A NYBBLE BUFFER FOR THE STACK" for further
;;           | information on this subject.
;;   ..................................................................
;;   10      | Pops the topmost element, here designated as "N", from
;;           | the stack and prints the the N-th character from the
;;           | 16-member Nybblang alphabet
;;           |   ------------------
;;           |   Index | Character
;;           |   ------+-----------
;;           |   0     | E
;;           |   ..................
;;           |   1     | T
;;           |   ..................
;;           |   2     | A
;;           |   ..................
;;           |   3     | O
;;           |   ..................
;;           |   4     | I
;;           |   ..................
;;           |   5     | N
;;           |   ..................
;;           |   6     | S
;;           |   ..................
;;           |   7     | R
;;           |   ..................
;;           |   8     | H
;;           |   ..................
;;           |   9     | D
;;           |   ..................
;;           |   10    | L
;;           |   ..................
;;           |   11    | (space)
;;           |   ..................
;;           |   12    | W
;;           |   ..................
;;           |   13    | U
;;           |   ..................
;;           |   14    | .
;;           |   ..................
;;           |   15    | (newline) 
;;           |   ------------------
;;           | whose zero-based index matches "N".
;;           |---------------------------------------------------------
;;           | Please consult the "Concept" subsection
;;           | "NYBBLANG CAN MERELY PRINT 16 DIFFERENT SYMBOLS" for
;;           | further information.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   11      | Imports the next program in the file sequence, wrapping
;;           | around to the first member upon its desinent element's
;;           | transgression.
;;   ------------------------------------------------------------------
;; 
;; == STANDARD NYBBLANG: OVERVIEW ==
;; The following tabular exposition's onus shall be the
;; "Jumping Nybblang" variation operations' presentation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   00      | Pushes the number zero (0) to the memory.
;;           |---------------------------------------------------------
;;           | Please note that, as a first step, the number is
;;           | transferred into the temporary nybble-valued variable as
;;           | the bit zero (0), occupying the next free position in
;;           | nybble buffer, which proceeds from the least to the most
;;           | significant bit.
;;           | Upon this buffer's patration to four bits, the thus
;;           | resulting nybble is actually pushed onto the stack top,
;;           | purging and reseting the buffer in a epiphenomenal step.
;;           | Please consult the section "Architecture" for further
;;           | information on this subject.
;;   ..................................................................
;;   01      | Pushes the number one (1) to the memory.
;;           |---------------------------------------------------------
;;           | Please note that, as a first step, the number is
;;           | transferred into the temporary nybble-valued variable as
;;           | the bit one (1), occupying the next free position in
;;           | nybble buffer, which proceeds from the least to the most
;;           | significant bit.
;;           | Upon this buffer's patration to four bits, the thus
;;           | resulting nybble is actually pushed onto the stack top,
;;           | purging and reseting the buffer in a epiphenomenal step.
;;           | Please consult the "Concept" subsection "THE TEMPORARY
;;           | VARIABLE: A NYBBLE BUFFER FOR THE STACK" for further
;;           | information on this subject.
;;   ..................................................................
;;   10      | Pops the topmost element, here designated as "N", from
;;           | the stack and prints the the N-th character from the
;;           | 16-member Nybblang alphabet
;;           |   ------------------
;;           |   Index | Character
;;           |   ------+-----------
;;           |   0     | E
;;           |   ..................
;;           |   1     | T
;;           |   ..................
;;           |   2     | A
;;           |   ..................
;;           |   3     | O
;;           |   ..................
;;           |   4     | I
;;           |   ..................
;;           |   5     | N
;;           |   ..................
;;           |   6     | S
;;           |   ..................
;;           |   7     | R
;;           |   ..................
;;           |   8     | H
;;           |   ..................
;;           |   9     | D
;;           |   ..................
;;           |   10    | L
;;           |   ..................
;;           |   11    | (space)
;;           |   ..................
;;           |   12    | W
;;           |   ..................
;;           |   13    | U
;;           |   ..................
;;           |   14    | .
;;           |   ..................
;;           |   15    | (newline) 
;;           |   ------------------
;;           | whose zero-based index matches "N".
;;           |---------------------------------------------------------
;;           | Please consult the "Concept" subsection
;;           | "NYBBLANG CAN MERELY PRINT 16 DIFFERENT SYMBOLS" for
;;           | further information.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ..................................................................
;;   11      | Peeks without removing the stack's bottom element:
;;           |   (1) If this element is greater than zero (0),
;;           |       supputates the jump target, here norned "n", as
;;           |       follows:
;;           |       (a) If an element above the stack bottom exists,
;;           |           sets "n" to the value of this element above
;;           |           the bototm.
;;           |       (b) If the stack bottom constitutes the only
;;           |           element on the stack, sets "n" to its value.
;;           |       If "n" is greater than zero (0) and less than or
;;           |       equal to the tally of instructions in the program,
;;           |       relocates the instruction pointer (IP) to "n";
;;           |       otherwise immediately terminates the program.
;;           |   (2) If the stack's bottom element equals zero (0),
;;           |       advances to the next instruction.
;;           |---------------------------------------------------------
;;           | In a pseudocode it holds:
;;           |   if stack.bottomElement > 0 then
;;           |     let jumpTarget <- nil
;;           |     if stack.size >= 2 then
;;           |       jumpTarget <- element above stack.bottom
;;           |     else
;;           |       jumpTarget <- stack.bottom
;;           |     end if
;;           |     if 1 <= jumpTarget <= program.instructionCount then
;;           |       jump to instruction at index jumpTarget
;;           |     else
;;           |       terminate program
;;           |     end if
;;           |   else
;;           |     advance to next instruction
;;           |   end if
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | invocation, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the generous mete of lucidity adhibited to its protolog, the
;; Nybblang language's infliction with a few inroads of ambivalency
;; retains is present status. A subset among these lacunae shall be the
;; following sections' cynosure.
;; 
;; == IN WHICH ORDER ARE BUFFERED NYBBLE BITS ASSEMBLED? ==
;; An aspect of conspicuous designment, the Nybblang program memory's
;; componency lays its amplectation around a twissel: a stack that
;; maintains nybbles and a buffer for the assemblage of such four-bit
;; compounds in a stillatim fashion; the latter's efforts contributing
;; the prerequisite to the former's nourishment.
;; 
;; While the necessity of the nybble buffer's patration into a quadruple
;; unity derives in an express manner from the protolog's diction, the
;; mode of the gradual edification, whether siccan pursues an airt from
;; the least significant bit (LSB) to the most significant position
;; (MSB), or athwart, eludes an equipollence in lucidity. The lacuna in
;; the specification is compounded by a conflicting account woning in
;; the examples, thilk at one instance adhibits the former, ascending,
;; order, then in other locations apply themselves to the widdershins
;; procession.
;; 
;; It has been adjudged, with lealty to the first unambiguously
;; discernable example, to impute an assemblage of bits in the nybble
;; buffer increasing from the least significant position (LSB) towards
;; the most significant one (MSB), that is, each bit "pushed" onto the
;; same occupies a higher position than its predecessor. This mode, as
;; an interesting concomitant, more closely emulates the stack's last
;; in, first out principle, which, if the bits were pushed immediately
;; instead of consigned to a buffer's provisional castaldy, would
;; construct the received binary digits in such a way as to reverse
;; their admission order.
;; 
;; == DO IMPORTS ABORT THE CURRENT PROGRAM? ==
;; The kenspeckle proprium commorant in the standard Nybblang variation
;; relates to the necessity to concatenate programs, impounded to
;; admit merely an exact account of two instruction each, in order to
;; accompass complex functionality. All examples enjoying their
;; adduction in the protolog ostend the import operation as the desinent
;; among the operative twissel, extending a parasceve to the next
;; actions.
;; 
;; However, the adhibition of the expectancy anent the import as the
;; first instruction member is wanting of a lucid treatise.
;; 
;; Three contingencies' proffer are entalented with tenability:
;; 
;;   (1) The current program is immediately aborted succeeding this
;;       first import instruction's issuance, circumventing the second
;;       member, and, as a corollary, selecting the next program's
;;       inchoate operation.
;;   
;;   (2) The import behest experiences an induction into a temporary
;;       retention, ensuring the second instruction's continuance,
;;       whence ensues the segue into the subsequent program.
;;   
;;   (3) The existency of an import request on the first position
;;       constitutes a subject of interdiction, whose causatum's
;;       embodiment emerges in an abortive error's propagation.
;; 
;; It has been adjudged to comply with the first (1) option, thilk
;; bears both the potential and entelechy of a Procrustean attendance
;; to all instructions as immediately effective warklumes.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's implementation constitutes an endeavor in the
;; programming language Common Lisp, the Nybblang programs' evaluation
;; being exercised immediately on the code string.
;; 
;; Destitute of the polymechany that nemns itself an executable program
;; in the language standard, the resulting Nybblang interpreter does
;; not comply in a mete of veridicous patration with the protolog's
;; stipulated mode of actuation. In lieu of the invocation
;; 
;;   nybblang {files-or-directory}
;; 
;; the interpreter at hand seeks amendments in the homologation of
;; so called "Program-Loader" instances, their bailiwick an abstract
;; adit mechanism's furnishment for the reception of the respective next
;; program commencing from an inchoation point.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-23
;; 
;; Sources:
;;   [esolang2022Nybblang]
;;   The Esolang contributors, "Nybblang", May 21st, 2022
;;   URL: "https://esolangs.org/wiki/Nybblang"
;;   
;;   [goodrich2006datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 120--127 describe the doubly linked list.
;;       o The page 120 presents an implementation of a doubly linked
;;         list node in Java, norned "DNode".
;;       o The pages 125--127 present an implementation of a doubly
;;         linked list in Java.
;;     
;;     - The pages 213--216 describe the double-ended queue, or deque,
;;       abstract data type (ADT).
;;       o The pages 215--216 present a partial implementation in Java
;;         utilizing a doubly linked list.
;;     
;;     - The pages 231-241 describe the node list abstract data type
;;       (ADT).
;;       o This data type utilizes the notion of "positions" in order
;;         to furnish an abstraction of nodes for its elements' access.
;;       o The pages 234--235 describe an interface for the node list
;;         ADT, nevened "PositionList".
;;       o The page 235 mentions the equivalency of the node list
;;         operations and the deque counterparts.
;;       o The pages 236--241 present an implementation of the node list
;;         ADT via a doubly linked list, the product being yclept the
;;         "NodePositionList".
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 132--137 describe the concept and an implementation
;;       of the doubly linked list in the Java programming language.
;;       o The pages 135--137 furnish the implementation.
;;     
;;     - The pages 248--251 describe the concept and implementation of
;;       the double-ended queue, or deque, abstract data type (ADT).
;;       o The pages 250--251 describe the implementation of a deque via
;;         a doubly linked list.
;;     
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;;   
;;   [martyanoff2023countlines]
;;   Nicolas Martyanoff, "Counting lines with Common Lisp",
;;     March 17th, 2023
;;   URL: "https://www.n16f.net/blog/counting-lines-with-common-lisp/"
;;   Notes:
;;     - Demonstrates the implementation of a file content line counter.
;;     - To this end, creates operations to list files in a directory.
;;   
;;   [stackoverflow2009a1403758]
;;   The Stack Overflow contributors,
;;     "How do I iterate through a directory in Common Lisp?",
;;     September 10th, 2009
;;   URL: "https://stackoverflow.com/a/1403758"
;;   Notes:
;;     - Demonstrates the obtention of the files in a specified
;;       directory.
;;   
;;   [weisstein2023crumb]
;;   Eric W. Weisstein, "Crumb", From MathWorld--A Wolfram Web Resource,
;;     2023
;;   URL "https://mathworld.wolfram.com/Crumb.html"
;;   Notes:
;;     - Definition of the term "crumb", used to denote a compound of
;;       two bits in computer science.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type the agnomination of which is appropriated from
   the TYPE-NAME, its formal parameters registering the LAMBDA-LIST's
   dation, stevening the object to probe with the CANDIDATE-NAME,
   evaluates the BODY forms, the desinent form's primary result
   furnishing the docimasy's result, with a \"generalized boolean\"
   value of \"true\" administered the construe as the candidate's
   eligibility, while \"false\" accounts for a failure in the
   compatibility."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name)
                        (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype crumb ()
  "The ``crumb'' type defines a sequence of two consecutive bits, thus
   being a commorant of the integer range [0, 3]."
  '(unsigned-byte 2))

;;; -------------------------------------------------------

(deftype nybble ()
  "The ``nybble'' type defines a sequence of four consecutive bits, thus
   being a commorant of the integer range [0, 15]."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype standard-nybblang-command ()
  "The ``standard-nybblang-command'' type enumerates the recognized
   variation of operations commorant in the standard version of the
   Nybblang language."
  '(member
    :push-0
    :push-1
    :output-character
    :import-next-program))

;;; -------------------------------------------------------

(deftype jumping-nybblang-command ()
  "The ``jumping-nybblang-command'' type enumerates the recognized
   variation of operations commorant in the extended version of the
   Nybblang language, by its author assessed to comply with the basic
   covenants of Turing-completeness in a conditional jumping
   instruction's establishment as a surrogate for the next program's
   induction."
  '(member
    :push-0
    :push-1
    :output-character
    :jump-to))

;;; -------------------------------------------------------

(deftype source ()
  "The ``source'' type defines a provenance for the obtention of data."
  '(or pathname stream string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing, among others, the function ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   which is defined the generic sentinel ``*'' as the default."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (every
        #'(lambda (current-element)
            (declare (type T current-element))
            (typep current-element element-type))
        (the list candidate)))))

;;; -------------------------------------------------------

(define-predicated-type circular-list-of
    (candidate &optional (element-type '*))
  "The ``circular-list-of'' type defines a circular list composed of
   zero or more elements, each member of which complies with the
   ELEMENT-TYPE, for thilk holds the generic sentinel ``*'' as the
   default.
   ---
   Please heed that, faced with the complexity commorant in the circular
   nature of a list, this type's paravaunt services appertain to a
   significative exercise, rather than a veridicous attendance to the
   probed candidate's conformation. Pursuing a concrete diction, the
   following inquisitions exhaust the docimasy:
     (1) Does the candidate object represent a list?
     (2) Does the ELEMENT-TYPE represent the ``*'' sentinel?
     (3) If the question (2) does not hold, does the first element of
         the candidate, hitherto confirmed to establish a list, comply
         with the ELEMENT-TYPE?"
  (and
    (listp candidate)
    (or
      (null  candidate)
      (eq    element-type      '*)
      (typep (first candidate) element-type))))

;;; -------------------------------------------------------

(deftype file-enumerator ()
  "The ``file-enumerator'' type defines a function whose capacitation
   is dedicated to the generation of a file path for a file specified
   by the twissel of its ensconcing directory and its one-based file
   number, as such complying with the following signature:
     lambda (pathname file-number) => new-pathname
   where the PATHNAME specifies the ``pathname'' location of the
   amplecting directory, the FILE-NUMBER represents an integer value
   greater than or equal to one (1), and the NEW-PATH imposes a
   ``pathname'' to accommodate for a datum's location."
  '(function (pathname (integer 1 *)) pathname))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 4 4)     +VALID-PROGRAM-SIZE+))
(declaim (type (simple-string 3) +NYBBLANG-FILE-EXTENSION+))

;;; -------------------------------------------------------

(defparameter +VALID-PROGRAM-SIZE+ 4
  "Specifies the exact tally of characters admissible in a standard
   Nybblang program.")

(defparameter +NYBBLANG-FILE-EXTENSION+ "nyb"
  "Specifies the file extension naited in the recognition of Nybblang
   files.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\"
   signification and produces a veridicous Boolean equivalency thereof,
   responding for a non-``NIL'' OBJECT with a ``boolean'' value of
   ``T''; otherwise, for a ``NIL'' input, returns ``NIL'' verbatim."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-lines (&rest lines)
  "Creates and returns a fresh string which lists the LINES in their
   encountered order, each twissel's intermede conjoined with a single
   newline character."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))

;;; -------------------------------------------------------

(defun concatenate-strings (&rest sources)
  "Creates and returns a fresh string which lists the LINES in their
   encountered order, each twissel's intermede conjoined with a single
   newline character."
  (declare (type (list-of string) sources))
  (the string
    (format NIL "~{~a~}" sources)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Nybble-Buffer".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nybble-Buffer ()
  ((nybble
    :initform      #b0000
    :reader        get-buffered-nybble
    :type          nybble
    :documentation "The nybble to be constructed from the most
                    significant position to the least significant one.")
   (size
    :initform      0
    :reader        get-nybble-buffer-size
    :type          (integer 0 4)
    :documentation "The number of bits added to the buffer, and
                    consequently set in the nybble.
                    ---
                    Building a nybble from its least significant bit
                    (LSB) position to its most significant bit (MSB)
                    location, that is, from the bit position zero (0) up
                    to three (3), the following relationship betwixt the
                    SIZE slot and the next bit position p to set in the
                    nybble holds:
                      p = size
                    ---
                    If greater or equal to four (4), the buffer is
                    considered complete or full, and must be reset for
                    further manipulations."))
  (:documentation
    "The ``Nybble-Buffer'' class capacitates the stillatim construction
     of a nybble, from its least significant to the most significant
     position, by insertion of a maximum of four bits."))

;;; -------------------------------------------------------

(defun make-nybble-buffer ()
  "Creates and returns a new ``Nybble-Buffer'' instance."
  (the Nybble-Buffer
    (make-instance 'Nybble-Buffer)))

;;; -------------------------------------------------------

(defun nybble-buffer-is-complete-p (buffer)
  "Determines whether the nybble BUFFER is complete or full, that is,
   all four bits of the internally managed nybble are set, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Nybble-Buffer buffer))
  (the boolean
    (get-boolean-value-of
      (>= (slot-value buffer 'size)
          +VALID-PROGRAM-SIZE+))))

;;; -------------------------------------------------------

(defun push-bit-to-nybble-buffer (buffer bit)
  "Inserts the BIT at the nybble BUFFER's position cursor, and returns
   the modified BUFFER.
   ---
   If the BUFFER is already complete prior to this function's
   invocation, an error of an unspecified type is signaled.
   ---
   Please remember that the nybble construction proceeds from the most
   significant to the least significant position."
  (declare (type Nybble-Buffer buffer))
  (declare (type bit           bit))
  (if (nybble-buffer-is-complete-p buffer)
    (error 'Full-Nybble-Buffer-Error :buffer buffer)
    (with-slots (nybble size) buffer
      (declare (type nybble        nybble))
      (declare (type (integer 0 4) size))
      (setf (ldb (byte 1 size) nybble) bit)
      (incf size)))
  (the Nybble-Buffer buffer))

;;; -------------------------------------------------------

(defun reset-nybble-buffer (buffer)
  "Resets the nybble BUFFER, thus purging its bits, and returns the
   modified BUFFER."
  (declare (type Nybble-Buffer buffer))
  (psetf (slot-value buffer 'nybble) #b0000
         (slot-value buffer 'size)   0)
  (the Nybble-Buffer buffer))

;;; -------------------------------------------------------

(defmethod print-object ((buffer Nybble-Buffer) (stream T))
  (declare (type Nybble-Buffer buffer))
  (declare (type destination   stream))
  (format stream "(Nybble-Buffer nybble=~4,'0b index=~d)"
    (slot-value buffer 'nybble)
    (slot-value buffer 'size)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of vector operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-nybble-vector ()
  "Creates and returns a fresh dynamic vector, at its inchoations
   entirely vacant and dedicated to the castaldy of ``nybble'' elements
   only."
  (the (vector nybble *)
    (make-array 0
      :element-type    'nybble
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "DLNode".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (DLNode
  (:constructor make-dlnode (element previous next)))
  "The ``DLNode'' class serves in the encapsulation of a doubly linked
   node's attributes, the componency of its diorism exhausted by
   intrining the ensconced nybble-valued element, an optional reference
   to a predecessor node, and equivalent potential successor pointer."
  (element  (error "Missing node element.")
            :type      nybble
            :read-only T)
  (previous (error "Missing node predecessor.")
            :type      (or null DLNode)
            :read-only NIL)
  (next     (error "Missing node successor.")
            :type      (or null DLNode)
            :read-only NIL))

;;; -------------------------------------------------------

(defmethod print-object ((node DLNode) (stream T))
  (declare (type DLNode      node))
  (declare (type destination stream))
  (format stream "(DLNode ~4,'0b)"
    (dlnode-element node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Stack".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ((header
    :initform      (make-dlnode 0 NIL NIL)
    :reader        stack-header
    :type          DLNode
    :documentation "The header sentinel node, which precedes any actual
                    element node, as well as the TRAILER.")
   (trailer
    :initform      (make-dlnode 0 NIL NIL)
    :reader        stack-trailer
    :type          DLNode
    :documentation "The trailer sentinel node, which succeeds any actual
                    element node, as well as the HEADER.")
   (size
    :initform      0
    :accessor      stack-size
    :type          (integer 0 *)
    :documentation "The number of elements subsumed into the stack's
                    castaldy."))
  (:documentation
    "The ``Stack'' class furnishes an implementation of the stack
     abstract data type (ADT) whose substratum's provision originates
     from a doubly linked list, this very potent principle's employment
     capacitated to vouch for the adscititious \"Jumping Nybblang\"
     language requirements of the indagation and removal of the two
     stack bottom positions, thilk for the nature of this last in,
     first out storage species usually register a forinsecal service."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((stack Stack) &key)
  "Connects the STACK's header and trailer nodes in a bilateral fashion
   and returns no value."
  (declare (type Stack stack))
  (with-slots (header trailer) stack
    (declare (type DLNode header))
    (declare (type DLNode trailer))
    (psetf
      (dlnode-next     header)  trailer
      (dlnode-previous trailer) header))
  (values))

;;; -------------------------------------------------------

(defun make-empty-stack ()
  "Creates and returns a stack in an incipial state of vacancy."
  (the Stack
    (make-instance 'Stack)))

;;; -------------------------------------------------------

(defun push-to-stack (stack element)
  "Pushes the ELEMENT onto the STACK's top position and returns no
   value."
  (declare (type Stack  stack))
  (declare (type nybble element))
  (with-slots (header) stack
    (declare (type DLNode header))
    (let ((new-node
            (make-dlnode element header
              (dlnode-next header))))
      (declare (type DLNode new-node))
      (setf (dlnode-previous (dlnode-next header)) new-node)
      (setf (dlnode-next     header)               new-node)))
  (incf (stack-size stack))
  (values))

;;; -------------------------------------------------------

(defun stack-is-empty-p (stack)
  "Determines whether the STACK is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (get-boolean-value-of
      (zerop
        (stack-size stack)))))

;;; -------------------------------------------------------

(defun stack-is-singleton-p (stack)
  "Determines whether the STACK constitutes a singleton storage, that
   is, accommodates a woning to an aefauld element only, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (get-boolean-value-of
      (= (stack-size stack)
         1))))

;;; -------------------------------------------------------

(defun peek-into-stack-top (stack)
  "Returns without removoal the top element from the STACK, or, upon its
   vacancy, signals an error of the type ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the nybble
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error)
      (dlnode-element
        (dlnode-next
          (stack-header stack))))))

;;; -------------------------------------------------------

(defun remove-node (stack node-to-remove)
  "Deletes the NODE-TO-REMOVE from the STACK and returns its ensconced
   element."
  (declare (type Stack  stack))
  (declare (type DLNode node-to-remove))
  (let ((predecessor (dlnode-previous node-to-remove))
        (successor   (dlnode-next     node-to-remove)))
    (declare (type DLNode predecessor))
    (declare (type DLNode successor))
    (psetf (dlnode-next     predecessor) successor
           (dlnode-previous successor)   predecessor)
    (decf (stack-size stack))
    (the nybble
      (dlnode-element node-to-remove))))

;;; -------------------------------------------------------

(defun pop-from-stack (stack)
  "Removes and returns the top element from the STACK, or, upon its
   vacancy, signals an error of the type ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the nybble
    (prog1
      (peek-into-stack-top stack)
      (remove-node stack
        (dlnode-next
          (stack-header stack))))))

;;; -------------------------------------------------------

(defun peek-into-stack-bottom (stack)
  "Returns without removal the element at the STACK's bottom, or, upon
   its vacancy, signals an error of the type ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the nybble
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error)
      (dlnode-element
        (dlnode-previous
          (stack-trailer stack))))))

;;; -------------------------------------------------------

(defun peek-above-stack-bottom (stack)
  "Returns without removal the element immediately aboon the STACK's
   bottom, or, upon its vacancy, signals an error of the type
   ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the nybble
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error)
      (dlnode-element
        (dlnode-previous
          (dlnode-previous
            (stack-trailer stack)))))))

;;; -------------------------------------------------------

(defun peek-above-or-into-stack-bottom (stack)
  "Returns without removal either the element immediately aboon the
   STACK's bottom, or, if a singleton stack, the STACK bottom itself,
   or, upon its vacancy, signals an error of the type
   ``Empty-Stack-Error''."
  (declare (type Stack stack))
  (the nybble
    (cond
      ((stack-is-empty-p stack)
        (error 'Empty-Stack-Error))
      ((stack-is-singleton-p stack)
        (peek-into-stack-bottom stack))
      (T
        (peek-above-stack-bottom stack)))))

;;; -------------------------------------------------------

(defmethod print-object ((stack Stack) (stream T))
  (declare (type Stack       stack))
  (declare (type destination stream))
  (with-slots (header trailer) stack
    (declare (type DLNode header))
    (declare (type DLNode trailer))
    (loop
      initially
        (format stream "(Stack [top>")
      for current-node
        of-type DLNode
        =       (dlnode-next header)
        then    (dlnode-next current-node)
      until (eq current-node trailer) do
        (format stream " ~4,'0b"
          (dlnode-element current-node))
      finally
        (format stream " <bottom])"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((stack
    :initform      (make-empty-stack)
    :accessor      memory-stack
    :type          Stack
    :documentation "A stack of nybbles.")
   (buffer
    :initform      (make-nybble-buffer)
    :reader        memory-buffer
    :type          Nybble-Buffer
    :documentation "The buffer dedicated to the assemblage of four bits,
                    provided serelepes and seriatim, into a single
                    nybble, ere thilk's insertion on the STACK's top."))
  (:documentation
    "The ``Memory'' class realizes the Nybblang program memory as a
     champarty of a nybble-valued stack and a buffer whose dever redes
     the construction of such a four-bit composition as a prerequisite
     to its insertion on the stack."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns an initially empty Nybblang program memory,
   edified upon the twissel of a nybble-valued stack and a buffer
   nuncupated to such a four-bit construct's assemblage."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun push-bit-to-memory (memory new-bit)
  "Pushes the NEW-BIT onto the MEMORY's nybble buffer, upon its state's
   patration transferring the assembled nybble onto the stack component,
   in any case returning no value."
  (declare (type Memory memory))
  (declare (type bit    new-bit))
  (with-slots (buffer stack) memory
    (declare (type Nybble-Buffer buffer))
    (declare (type Stack         stack)
             (ignorable          stack))
    (push-bit-to-nybble-buffer buffer new-bit)
    (when (nybble-buffer-is-complete-p buffer)
      (push-to-stack stack
        (get-buffered-nybble buffer))
      (reset-nybble-buffer buffer)))
  (values))

;;; -------------------------------------------------------

(defun pop-nybble-from-memory (memory)
  "Removes and returns the nybble commorant on the MEMORY stack's top
   position, if possible; otherwise, upon the stack's vacancy, signals
   an error of an unspecified type."
  (declare (type Memory memory))
  (the nybble
    (pop-from-stack
      (memory-stack memory))))

;;; -------------------------------------------------------

(defun peek-into-memory-bottom (memory)
  "Returns without removal the element at the MEMORY stack's bottom; or,
   upon its vacancy, signals an error of the type
   ``Empty-Stack-Error''."
  (declare (type Memory memory))
  (the nybble
    (peek-into-stack-bottom
      (memory-stack memory))))

;;; -------------------------------------------------------

(defun peek-above-or-into-memory-bottom (memory)
  "Returns without removal the element immediately aboon the MEMORY
   stack's bottom, or, if the storage constitutes a singleton
   collection, responds with the bottom element; for an empty stack, an
   error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Memory memory))
  (the nybble
    (peek-above-or-into-stack-bottom
      (memory-stack memory))))

;;; -------------------------------------------------------

(defun memory-is-empty-p (memory)
  "Determines whether the MEMORY stack is empty, returning on
   confirmation a ``boolean'' avlue of ``T'', otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (stack-is-empty-p
      (memory-stack memory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of list operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-circular-list (elements)
  "Returns a circular variant of the ELEMENTS list."
  (declare (type (list-of T) elements))
  (setf (cdr (last elements))
        elements)
  (the (circular-list-of *) elements))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-file-input-buffer (input-stream)
  "Prepares and returns a fresh buffer whose capacity conflates with the
   file INPUT-STREAM's supputated length."
  (declare (type file-stream input-stream))
  (the simple-string
    (make-array
      (file-length input-stream)
      :element-type    'character
      :initial-element #\Null
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun load-file-content (source)
  "Obtains the content of the SOURCE and returns thilk as a fresh
   string."
  (declare (type source source))
  (with-open-file (input-stream source
                   :element-type      'character
                   :direction         :input
                   :if-does-not-exist :error)
    (declare (type file-stream input-stream))
    (let ((content-buffer (prepare-file-input-buffer input-stream)))
      (declare (type simple-string content-buffer))
      (read-sequence content-buffer input-stream)
      (the simple-string content-buffer))))

;;; -------------------------------------------------------

(defun path-designates-directory-p (path)
  "Determines whether the PATH references a directory rather than a
   specific file, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type source path))
  (the boolean
    (and (not (pathname-name path))
         (not (pathname-type path)))))

;;; -------------------------------------------------------

(defun list-files-in-directory (source
                                &optional (admitted-file-type :wild))
  "Returns a fresh list comprehending all files directly entailed in
   the SOURCE directory and complying with the extension imposed by the
   ADMITTED-FILE-TYPE, thilk defaults to the comprehensive ``:wild''.
   ---
   Please heed that the order of the thus yielded files constitutes a
   dependency upon the underlying operating system's specifications."
  (declare (type source                  source))
  (declare (type (or string (eql :wild)) admitted-file-type))
  (the (list-of pathname)
    (directory
      (make-pathname
        :defaults source
        :type     admitted-file-type
        :name     :wild))))

;;; -------------------------------------------------------

(defun list-nybblang-files-in-directory (source)
  "Returns a fresh list comprehending all files directly entailed in
   the SOURCE directory and complying with the Nybblang file extension
   \"nyb\".
   ---
   Please heed that the order of the thus yielded files constitutes a
   dependency upon the underlying operating system's specifications."
  (declare (type source source))
  (the (list-of pathname)
    (list-files-in-directory source +NYBBLANG-FILE-EXTENSION+)))

;;; -------------------------------------------------------

(defun collate-nybblang-files-across-sources (sources)
  "Returns a fresh list comprehending all Nybblang files, administered
   their identification by adminiculum of the \"nyb\" extension,
   commorant in all of the queried SOURCES, everichon of these
   participants represented by a ``pathname'' object.
   ---
   The thus produced collection constitutes a flat list of the detected
   Nybblang files, extracted in concord with the SOURCES' assignment.
   ---
   Please heed that the order of the thus yielded files constitutes a
   dependency upon the underlying operating system's specifications."
  (declare (type (list-of source) sources))
  (the (list-of pathname)
    (mapcan
      #'(lambda (current-source)
          (declare (type source current-source))
          (if (path-designates-directory-p current-source)
            (list-nybblang-files-in-directory current-source)
            (list (parse-namestring current-source))))
      sources)))

;;; -------------------------------------------------------

(defun write-to-text-file (output-path content)
  "Writes the CONTENT to the text file designated by the OUTPUT-PATH,
   contingently creating such if not yet extant, and returns no value.
   ---
   Please heed the following stipulations:
     (a) If no file designated by the OUTPUT-PATH can be located, the
         entire amplecting directory structure as well as the file
         itself will be created in the operating system's file system.
     (b) If a file designated by the OUTPUT-PATH can be located, its
         entire content is purged ere its supersession by the output
         generated via this operation."
  (declare (type source output-path))
  (declare (type T      content))
  (with-open-file (output-stream output-path
                   :direction         :output
                   :element-type      'character
                   :if-exists         :supersede
                   :if-does-not-exist :create)
    (declare (type file-stream output-stream))
    (format output-stream "~a" content))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Nybblang-Program".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Nybblang-Program ()
  ((source
    :initarg       :source
    :initform      NIL
    :reader        get-program-source
    :type          (or null source)
    :documentation "The location whence the program has been obtained.")
   (content
    :initarg       :content
    :initform      (error "Missing Nybble program content.")
    :reader        get-program-content
    :type          string
    :documentation "The content stored in the SOURCE."))
  (:documentation
    "The ``Nybblang-Program'' class applies itself to the encapsulation
     of a separate Nybblang program, usually, but not necessarily,
     obtained from an external provenance."))

;;; -------------------------------------------------------

(defun make-sourced-nybblang-program (source content)
  "Creates and returns a fresh ``Nybblang-Program'' whose CONTENT is
   obtained from the SOURCE."
  (declare (type (or null source) source))
  (declare (type string           content))
  (the Nybblang-Program
    (make-instance 'Nybblang-Program
      :source  source
      :content content)))

;;; -------------------------------------------------------

(defun make-sourceless-nybblang-program (content)
  "Creates and returns a fresh ``Nybblang-Program'' with the given
   CONTENT, destitute, however, of its provenance's specification."
  (declare (type string content))
  (the Nybblang-Program
    (make-instance 'Nybblang-Program
      :source  NIL
      :content content)))

;;; -------------------------------------------------------

(defun load-nybblang-program-from-path (path)
  "Creates and returns a fresh sourced ``Nybblang-Program'' whose
   provenance is desumed from the PATH and whose content is obtained
   from thilk's data."
  (declare (type source path))
  (the Nybblang-Program
    (make-sourced-nybblang-program path
      (load-file-content path))))

;;; -------------------------------------------------------

(defun get-nybblang-program-size (program)
  "Returns the tally of characters comprising the Nybblang PROGRAM."
  (declare (type Nybblang-Program program))
  (the fixnum
    (length
      (slot-value program 'content))))

;;; -------------------------------------------------------

(defmethod print-object ((program Nybblang-Program) (stream T))
  (declare (type Nybblang-Program program))
  (declare (type destination      stream))
  (format stream "(Nybblang-Program :source ~s :content ~s)"
    (slot-value program 'source)
    (slot-value program 'content)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Nybblang program list operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-nybblang-programs-from-files (files)
  "Consumes the FILES and returns a fresh list of Nybblang programs
   produced from these sources."
  (declare (type (list-of source) files))
  (the (list-of Nybblang-Program)
    (mapcar #'load-nybblang-program-from-path files)))

;;; -------------------------------------------------------

(defun load-nybblang-programs-in-directory (directory)
  "Detects all Nybblang program files commorant directly inside of the
   DIRECTORY and returns a fresh list comprehending these."
  (declare (type source directory))
  (the (list-of Nybblang-Program)
    (load-nybblang-programs-from-files
      (list-nybblang-files-in-directory directory))))

;;; -------------------------------------------------------

(defun load-nybblang-programs-from-sources (sources)
  "Collates all Nybblang programs specified by the SOURCE, thilk may
   constitute both file or directories ensconcing Nybblang files, and
   returns a fresh list comprehending the loaded programs."
  (declare (type (list-of source) sources))
  (the (list-of Nybblang-Program)
    (load-nybblang-programs-from-files
      (collate-nybblang-files-across-sources sources))))

;;; -------------------------------------------------------

(defun merge-nybblang-programs (programs)
  "Creates and returns a fresh sourceless ``Nybblang-Program'' whose
   content is obtained by a concatenation of the input PROGRAMS'
   contents in their specified order."
  (declare (type (list-of Nybblang-Program) programs))
  (the Nybblang-Program
    (make-sourceless-nybblang-program
      (with-output-to-string (combined-content)
        (declare (type string-stream combined-content))
        (dolist (current-program programs)
          (declare (type Nybblang-Program current-program))
          (format combined-content "~a"
            (get-program-content current-program)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Program-Loader".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program-Loader ()
  ()
  (:documentation
    "The ``Program-Loader'' interface establishes a common foundry for
     all classes whose telos' woning appertains to the import of
     Nybblang programs."))

;;; -------------------------------------------------------

(defgeneric import-next-program (loader)
  (:documentation
    "Queries the next Nybblang program from the program LOADER and
     returns a ``Nybblang-Program'' encapsulation thereof.
     ---
     Upon the available program sequence's exhaustion, the retrieval
     commences anew with the incipient program instance."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Buffered-Program-Loader".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Buffered-Program-Loader (Program-Loader)
  ((programs
    :initarg       :programs
    :initform      (error "Missing programs.")
    :accessor      buffered-program-loader-programs
    :type          (circular-list-of Nybblang-Program)
    :documentation "A circular ordered list of the program files'
                    ``Nybblang-Program'' representations."))
  (:documentation
    "The ``Buffered-Program-Loader'' class accoutres a Nybblang program
     loader whose firmament proceeds from the notion of zero or more
     file contents' obtention and static castaldy, which means that the
     thus received data is stored in a fixed manner and does not react
     to modifications applied to the provenances after the loader's
     instantiation.
     ---
     Educed from this mode of stewardship remains the fact that even
     after a loaded file's deletion, thilk's content remains unaltered
     in the loader."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((loader Buffered-Program-Loader)
                                       &key)
  "Converts the program LOADER's initial Nybblang program list into a
   circular variant, stores thilk in the LOADER, supplanting the
   original sequence, and returns no value."
  (declare (type Buffered-Program-Loader loader))
  (setf (buffered-program-loader-programs loader)
    (make-circular-list
      (buffered-program-loader-programs loader)))
  (values))

;;; -------------------------------------------------------

(defun make-buffered-program-loader-for-files (files)
  "Creates and returns a fresh ``Buffered-Program-Loader'' whose
   programs are obtained from the FILES, respecting their specified
   ordering."
  (declare (type (list-of source) files))
  (the Buffered-Program-Loader
    (make-instance 'Buffered-Program-Loader :programs
      (load-nybblang-programs-from-files files))))

;;; -------------------------------------------------------

(defun make-buffered-program-loader-for-directory (directory)
  "Creates and returns a fresh ``Buffered-Program-Loader'' whose
   programs are obtained by a retrieval of all files constituting
   immediate commorants in the DIRECTORY and being designated with the
   extension \".nyb\".
   ---
   Please heed that the order of the thus yielded files constitutes a
   dependency upon the underlying operating system's specifications."
  (declare (type source directory))
  (the Buffered-Program-Loader
    (make-instance 'Buffered-Program-Loader :programs
      (load-nybblang-programs-in-directory directory))))

;;; -------------------------------------------------------

(defun make-buffered-program-loader-for-sources (sources)
  "Creates and returns a fresh ``Buffered-Program-Loader'' whose
   programs are obtained by a retrieval of all SOURCES constituting
   either concrete files or immediate commorants in a directory and
   being designated with the extension \".nyb\".
   ---
   Please heed that the order of the thus yielded files constitutes a
   dependency upon the underlying operating system's specifications."
  (declare (type (list-of source) sources))
  (the Buffered-Program-Loader
    (make-instance 'Buffered-Program-Loader :programs
      (load-nybblang-programs-from-sources sources))))

;;; -------------------------------------------------------

(defmethod import-next-program ((loader Buffered-Program-Loader))
  (declare (type Buffered-Program-Loader loader))
  (the Nybblang-Program
    (pop (buffered-program-loader-programs loader))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Listing-Program-Loader".            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Listing-Program-Loader (Program-Loader)
  ((programs
    :initarg       :programs
    :initform      (error "Missing programs.")
    :accessor      listing-program-loader-programs
    :type          (circular-list-of Nybblang-Program)
    :documentation "A circular ordered list of the input programs."))
  (:documentation
    "The ``Listing-Program-Loader'' class accoutres a Nybblang program
     loader whose programs' provenance constitutes a direct ordered
     sequence of Nybblang programs, fashioned into a perpetually
     repeating mold."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((loader Listing-Program-Loader)
                                       &key)
  "Converts the program LOADER's initial Nybblang program list into a
   circular variant, stores thilk in the LOADER, supplanting the
   original sequence, and returns no value."
  (declare (type Listing-Program-Loader loader))
  (setf (listing-program-loader-programs loader)
    (make-circular-list
      (listing-program-loader-programs loader)))
  (values))

;;; -------------------------------------------------------

(defun make-listing-program-loader (&rest programs)
  "Creates and returns a fresh ``Listing-Program-Loader'' whose programs
   are desumed from the PROGRAMS, respecting their specified ordering."
  (declare (type (list-of Nybblang-Program) programs))
  (the Listing-Program-Loader
    (make-instance 'Listing-Program-Loader :programs programs)))

;;; -------------------------------------------------------

(defmethod import-next-program ((loader Listing-Program-Loader))
  (declare (type Listing-Program-Loader loader))
  (the Nybblang-Program
    (pop (listing-program-loader-programs loader))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Concatenative-Program-Loader".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Concatenative-Program-Loader (Program-Loader)
  ((assembled-program
    :initarg       :assembled-program
    :initform      (error "Missing assembled Nybblang program.")
    :type          Nybblang-Program
    :documentation "The Nybblang program whose content is derived from
                    a mergence of several input programs."))
  (:documentation
    "The ``Concatenative-Program-Loader'' class serves in the
     furnishment of a Nybblang program loader which forms from an
     ordered sequence of zero or more programs a unity."))

;;; -------------------------------------------------------

(defun make-concatenative-program-loader-from-file (file)
  "Creates and returns a fresh ``Concatenative-Program-Loader'' whose
   sole Nybblang program is received from the FILE."
  (declare (type source file))
  (the Concatenative-Program-Loader
    (make-instance 'Concatenative-Program-Loader :assembled-program
      (load-nybblang-program-from-path file))))

;;; -------------------------------------------------------

(defun make-concatenative-program-loader-from-directory (directory)
  "Creates and returns a fresh ``Concatenative-Program-Loader'' whose
   sole Nybblang program constitutes an assemblage of the Nybblang
   files commorant in the DIRECTORY."
  (declare (type source directory))
  (the Concatenative-Program-Loader
    (make-instance 'Concatenative-Program-Loader :assembled-program
      (merge-nybblang-programs
        (load-nybblang-programs-in-directory directory)))))

;;; -------------------------------------------------------

(defun make-concatenative-program-loader-from-source (sources)
  "Creates and returns a fresh ``Concatenative-Program-Loader'' whose
   sole Nybblang program constitutes an assemblage of the Nybblang
   files obtained by an inquisition into the SOURCE, the same may
   specify direct files or directories comprehending such."
  (declare (type (list-of source) sources))
  (the Concatenative-Program-Loader
    (make-instance 'Concatenative-Program-Loader :assembled-program
      (merge-nybblang-programs
        (load-nybblang-programs-from-sources sources)))))

;;; -------------------------------------------------------

(defun make-concatenative-program-loader-from-programs (programs)
  "Creates and returns a ``Concatenative-Program-Loader'' whose aefauld
   program constitutes a concatenation of the input Nybblang PROGRAMS."
  (declare (type (list-of Nybblang-Program) programs))
  (the Concatenative-Program-Loader
    (make-instance 'Concatenative-Program-Loader :assembled-program
      (merge-nybblang-programs programs))))

;;; -------------------------------------------------------

(defun make-concatenative-program-loader-from-program (program)
  "Creates and returns a ``Concatenative-Program-Loader'' whose aefauld
   program conflates with the PROGRAM."
  (declare (type Nybblang-Program program))
  (the Concatenative-Program-Loader
    (make-instance 'Concatenative-Program-Loader
      :assembled-program program)))

;;; -------------------------------------------------------

(defmethod import-next-program ((loader Concatenative-Program-Loader))
  (declare (type Concatenative-Program-Loader loader))
  (the Nybblang-Program
    (slot-value loader 'assembled-program)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary and bit operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-crumb (source start-position)
  "Parses a crumb, that is, a twissel of two consecutive bits, from the
   SOURCE, commencing at the inclusive START-POSITION and terminating
   after the requisite two digits in its wake, and returns the thus
   produced integral number from the closed interval [0, 3]."
  (declare (type string source))
  (declare (type fixnum start-position))
  (the crumb
    (nth-value 0
      (parse-integer source
        :start start-position
        :end   (+ start-position 2)
        :radix 2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of standard Nybblang command decoder.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-standard-command (command-code)
  "Returns the standard Nybblang command allied with the two-bit
   COMMAND-CODE, or signals an error of an unspecified upon its
   disrespondency."
  (declare (type crumb command-code))
  (the standard-nybblang-command
    (case command-code
      (#b00 :push-0)
      (#b01 :push-1)
      (#b10 :output-character)
      (#b11 :import-next-program)
      (otherwise
        (error "Unrecognized standard Nybblang command code: ~2,'0b."
          command-code)))))

;;; -------------------------------------------------------

(defun decode-standard-code (program-code)
  "Extracts the twain of standard Nybblang commands ensconced in the
   PROGRAM-CODE string and return thilk as two values:
     (1) The first  contained ``standard-nybblang-command''.
     (2) The second contained ``standard-nybblang-command''."
  (declare (type string program-code))
  (the (values standard-nybblang-command standard-nybblang-command)
    (values
      (decode-standard-command (parse-crumb program-code 0))
      (decode-standard-command (parse-crumb program-code 2)))))

;;; -------------------------------------------------------

(defun decode-standard-program (program)
  "Extracts the twain of standard Nybblang commands ensconced in the
   PROGRAM and return thilk as two values:
     (1) The first  contained ``standard-nybblang-command''.
     (2) The second contained ``standard-nybblang-command''."
  (declare (type Nybblang-Program program))
  (the (values standard-nybblang-command standard-nybblang-command)
    (decode-standard-code
      (get-program-content program))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of alphabet.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 16) +ALPHABET+))

;;; -------------------------------------------------------

(defparameter +ALPHABET+ "ETAOINSRHDL WU.
"
  "States the Nybblang alphabet in string form.")

;;; -------------------------------------------------------

(defun decode-nybblang-character (character-code)
  "Returns the symbol in the alphabet at the zero-based CHARACTER-CODE,
   or signals an error of an unspecified type if the same violates the
   admissive subscript range."
  (declare (type (integer 0 15) character-code))
  (the character
    (schar +ALPHABET+ character-code)))

;;; -------------------------------------------------------

(defun decode-nybblang-character-to (character-code
                                     &optional (destination T))
  "Writes the Nybblang character amenable to the CHARACTER-CODE to the
   DESTINATION and returns no value."
  (declare (type (integer 0 15) character-code))
  (declare (type destination    destination))
  (format destination "~c"
    (decode-nybblang-character character-code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program validator operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bit-character-p (candidate)
  "Determines whether the CANDIDATE represents a binary digit in its
   character form, that is, either \"0\" or \"1\", returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (digit-char-p candidate 2))))

;;; -------------------------------------------------------

(defun binary-string-p (source)
  "Determines whether the SOURCE string entails merely the bit
   characters \"0\" and \"1\", returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (every #'bit-character-p source))))

;;; -------------------------------------------------------

(defun program-code-has-correct-size-p (code)
  "Determines whether the piece of Nybblang source CODE comprehends the
   valid extent of exactly four (4) constituents, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string code))
  (the boolean
    (get-boolean-value-of
      (= (length code) 4))))

;;; -------------------------------------------------------

(defun validate-program-composition (program)
  "Determines whether the Nybblang PROGRAM's content represents a pure
   binary string, returning on confirmation the probed PROGRAM;
   otherwise an error of the type ``Invalid-Program-Code-Error'' is
   signaled."
  (declare (type Nybblang-Program program))
  (the Nybblang-Program
    (or
      (and (binary-string-p (get-program-content program))
           program)
      (error 'Invalid-Program-Code-Error :program program))))

;;; -------------------------------------------------------

(defun validate-program-size (program)
  "Determines whether the Nybblang PROGRAM's content enumerates an exact
   quadruple of symbols, as imposed by the standard Nybblang covenant,
   returning on confirmation the probed PROGRAM; otherwise an error of
   the type ``Invalid-Program-Size-Error'' is signaled."
  (declare (type Nybblang-Program program))
  (the Nybblang-Program
    (or
      (and (program-code-has-correct-size-p
             (get-program-content program))
           program)
      (error 'Invalid-Program-Size-Error :program program))))

;;; -------------------------------------------------------

(defun validate-standard-nybblang-program (program)
  "Determines whether the Nybblang PROGRAM's content complies with the
   standard Nybblang language stipulations, scilicet, the representation
   as a pure binary string compact of exactly four digits, returning on
   confirmation the probed PROGRAM; otherwise either an error of the
   type ``Invalid-Program-Code-Error'' or ``Invalid-Program-Size-Error''
   is signaled."
  (declare (type Nybblang-Program program))
  (validate-program-composition program)
  (validate-program-size        program)
  (the Nybblang-Program program))

;;; -------------------------------------------------------

(defun validate-jumping-nybblang-program (program)
  "Determines whether the Nybblang PROGRAM's content complies with the
   \"jumping Nybblang\" language stipulation, scilicet, the
   representation as a pure binary string, returning on confirmation the
   probed PROGRAM; otherwise an error of the type
   ``Invalid-Program-Code-Error'' is signaled."
  (declare (type Nybblang-Program program))
  (the Nybblang-Program
    (validate-program-composition program)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Nybblang-Error (error)
  ()
  (:documentation
    "The ``Nybblang-Error'' condition type serves as a firmament to all
     conditions realized in a pursuit to encapsulate an anomalous
     situation's notion whose transpiration locates them in a nexus with
     a Nybblang program's analyzation or execution."))

;;; -------------------------------------------------------

(define-condition Invalid-Program-Code-Error (Nybblang-Error)
  ((program
    :initarg       :program
    :initform      (error "Missing Nybblang program.")
    :reader        invalid-program-code-error-program
    :type          Nybblang-Program
    :documentation "The Nybblang program whose viscerals thole an
                    adulterated species of data."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Program-Code-Error condition))
      (declare (type destination                stream))
      (format stream "The program ~s contains invalid characters."
        (get-program-content
          (invalid-program-code-error-program condition)))))
  (:documentation
    "The ``Invalid-Program-Code-Error'' condition type serves to signal
     an anomalous situation whose etiology registers the attempt to
     read a Nybblang program file composed of a character not desumed
     from the valid set {\"0\", \"1\"}."))

;;; -------------------------------------------------------

(define-condition Invalid-Program-Size-Error (Nybblang-Error)
  ((program
    :initarg       :program
    :initform      (error "Missing Nybblang program.")
    :reader        invalid-program-size-error-program
    :type          Nybblang-Program
    :documentation "The Nybblang program whose viscerals thole an
                    invalid character tally."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Program-Size-Error condition))
      (declare (type destination                stream))
      (format stream "The program ~s contains an incorrect number of ~
                      characters."
        (get-program-content
          (invalid-program-size-error-program condition)))))
  (:documentation
    "The ``Invalid-Program-Size-Error'' condition type serves to signal
     an anomalous situation whose etiology registers the attempt to
     read a Nybblang program file composed of less than or more than
     four (4) characters."))

;;; -------------------------------------------------------

(define-condition Full-Nybble-Buffer-Error (Nybblang-Error)
  ((buffer
    :initarg       :buffer
    :initform      (error "Missing full nybble buffer.")
    :reader        full-nybble-buffer-error-buffer
    :type          Nybble-Buffer
    :documentation "The nybble buffer whose attempt of extension while
                    in a surfeited state has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Full-Nybble-Buffer-Error condition))
      (declare (type destination              stream))
      (format stream "The nybble buffer ~s is surfeited and, as a ~
                      consectary, cannot accept further bits."
        (full-nybble-buffer-error-buffer condition))))
  (:documentation
    "The ``Full-Nybble-Buffer-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is educed
     from the attempt to insert a bit into an already surfeited
     ``Nybble-Buffer'' target."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Nybblang-Error simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous situation ensuing from the attempt to peek into
     or pop from an empty stack.")
  (:default-initargs
    :format-control "Cannot peek into or pop from an empty stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ()
  (:documentation
    "The ``Interpreter'' interface establishes a foundry entreparted
     by all varieties of Nybblang interpreters."))

;;; -------------------------------------------------------

(defgeneric get-program-memory (interpreter)
  (:documentation
    "Returns the Nybblang INTERPRETER's memory object."))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Processes the Nybblang COMMAND in the INTERPRETER's context and
     returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of abstract class "Abstract-Interpreter".     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Abstract-Interpreter (Interpreter)
  ((memory
    :initform      (make-memory)
    :reader        get-program-memory
    :type          Memory
    :documentation "The program memory as a composition of a
                    nybble-valued stack and a nybble buffer."))
  (:documentation
    "The ``Abstract-Interpreter'' abstract class accoutres a firmament
     inside of which are conjoined the properties partaken of by all
     Nybblang interpreters."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Standard-Nybblang-Interpreter".     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Standard-Nybblang-Interpreter (Abstract-Interpreter)
  ((program-loader
    :initarg       :program-loader
    :initform      (error "Missing program loader.")
    :reader        get-program-loader
    :type          Program-Loader
    :documentation "Responsible for importing Nybblang programs ensuing
                    from an explicit request.
                    ---
                    The thus obtained program is subsequently stored in
                    the CURRENT-PROGRAM slot.")
   (current-program
    :accessor      current-program
    :type          Nybblang-Program
    :documentation "The most recently acquired Nybblang program from
                    the PROGRAM-LOADER.")
   (shall-import-next-program-p
    :initform      NIL
    :accessor      shall-import-next-program-p
    :type          boolean
    :documentation "A Boolean flag which determines whether the
                    CURRENT-PROGRAM's first or second command has
                    requested the next program's import."))
  (:documentation
    "The ``Standard-Nybblang-Interpreter'' class implements an interpreter
     dedicated to the standard Nybblang programming language
     specification, its kenspeckle proprium the necessity to amend the
     curtailed circumference of its separate programs, each such
     homologated to accommodate exactly two instructions encoded in
     four bits, or a nybble, by adminiculum of an import facility that
     requests the subsequent program in a circular list."))

;;; -------------------------------------------------------

(defun request-next-program (interpreter)
  "Queries the next program from the INTERPRETER's program loader,
   stores thilk in the INTERPRETER, and returns no value."
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (setf (current-program interpreter)
    (validate-standard-nybblang-program
      (import-next-program
        (get-program-loader interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after
    ((interpreter Standard-Nybblang-Interpreter) &key)
  "Queries the first program from the INTERPRETER's program loader,
   stores thilk in the INTERPRETER, and returns no value."
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (request-next-program interpreter)
  (values))

;;; -------------------------------------------------------

(defun make-standard-nybblang-interpreter (program-loader)
  "Creates and returns a fresh ``Standard-Nybblang-Interpreter'' whose
   programs' provenance is incarnated in the PROGRAM-LOADER's
   contingency."
  (declare (type Program-Loader program-loader))
  (the Standard-Nybblang-Interpreter
    (make-instance 'Standard-Nybblang-Interpreter
      :program-loader program-loader)))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Standard-Nybblang-Interpreter)
                            (command     (eql :push-0)))
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (declare (type standard-nybblang-command     command)
           (ignore                             command))
  (push-bit-to-memory
    (get-program-memory interpreter)
    0)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Standard-Nybblang-Interpreter)
                            (command     (eql :push-1)))
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (declare (type standard-nybblang-command     command)
           (ignore                             command))
  (push-bit-to-memory
    (get-program-memory interpreter)
    1)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Standard-Nybblang-Interpreter)
                            (command     (eql :output-character)))
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (declare (type standard-nybblang-command     command)
           (ignore                             command))
  (decode-nybblang-character-to
    (pop-nybble-from-memory
      (get-program-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Standard-Nybblang-Interpreter)
                            (command     (eql :import-next-program)))
  (declare (type Standard-Nybblang-Interpreter interpreter))
  (declare (type standard-nybblang-command     command)
           (ignore                             command))
  (setf (shall-import-next-program-p interpreter) T)
  (values))

;;; -------------------------------------------------------

(defun execute-standard-nybblang-program (interpreter)
  "Executes the standard Nybblang program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Standard-Nybblang-Interpreter interpreter))
  
  (loop do
    (setf (shall-import-next-program-p interpreter) NIL)
    
    (multiple-value-bind (first-command second-command)
        (decode-standard-program
          (current-program interpreter))
      (declare (type standard-nybblang-command first-command))
      (declare (type standard-nybblang-command second-command))
      
      (process-command interpreter first-command)
      
      (if (shall-import-next-program-p interpreter)
        (request-next-program interpreter))
        (process-command interpreter second-command))
    
    (unless (shall-import-next-program-p interpreter)
      (loop-finish))
    
    (request-next-program interpreter))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-standard-nybblang-sources (&rest sources)
  "Collates all Nybblang files commorant in the SOURCES, admitting both
   concrete files and directories, interprets the thus resulting
   program sequence, and returns no value."
  (declare (type (list-of source) sources))
  (execute-standard-nybblang-program
    (make-standard-nybblang-interpreter
      (make-buffered-program-loader-for-sources sources)))
  (values))

;;; -------------------------------------------------------

(defun interpret-standard-nybblang-program (program)
  "Interprets the standard Nybblang PROGRAM and returns no value."
  (declare (type Nybblang-Program program))
  (execute-standard-nybblang-program
    (make-standard-nybblang-interpreter
      (make-concatenative-program-loader-from-program program)))
  (values))

;;; -------------------------------------------------------

(defun interpret-standard-nybblang-code (code)
  "Interprets the standard Nybblang program communicated in the CODE
   and returns no value."
  (declare (type string code))
  (execute-standard-nybblang-program
    (make-standard-nybblang-interpreter
      (make-concatenative-program-loader-from-program
        (make-sourceless-nybblang-program code))))
  (values))

;;; -------------------------------------------------------

(defun interpret-standard-nybblang-loader (program-loader)
  "Interprets the standard Nybblang program communicated by the
   PROGRAM-LOADER."
  (declare (type Program-Loader program-loader))
  (execute-standard-nybblang-program
    (make-standard-nybblang-interpreter program-loader))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Jumping Nybblang" command decoder.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-jumping-command (command-code)
  "Returns the \"Jumping Nybblang\" command allied with the two-bit
   COMMAND-CODE, or signals an error of an unspecified upon its
   disrespondency."
  (declare (type crumb command-code))
  (the jumping-nybblang-command
    (case command-code
      (#b00 :push-0)
      (#b01 :push-1)
      (#b10 :output-character)
      (#b11 :jump-to)
      (otherwise
        (error "Unrecognized \"Jumping Nybblang\" command code: ~2,'0b."
          command-code)))))

;;; -------------------------------------------------------

(defun decode-jumping-code (program-code start-point)
  "Extracts the \"Jumping Nybblang\" command commencing in the
   PROGRAM-CODE string at the START-POINT and return thilk as a
   ``jumping-nybblang-command'' object."
  (declare (type string program-code))
  (declare (type fixnum start-point))
  (the jumping-nybblang-command
    (decode-jumping-command
      (parse-crumb program-code start-point))))

;;; -------------------------------------------------------

(defun decode-jumping-program (program start-point)
  "Extracts the \"Jumping Nybblang\" command commencing in the Nybblang
   PROGRAM at the START-POINT and return thilk as a
   ``jumping-nybblang-command'' object."
  (declare (type Nybblang-Program program))
  (declare (type fixnum           start-point))
  (the jumping-nybblang-command
    (decode-jumping-code
      (get-program-content program)
      start-point)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Jumping-Nybblang-Interpreter".      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jumping-Nybblang-Interpreter (Abstract-Interpreter)
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          Nybble-Program
    :documentation "The \"Jumping Nybblang\" program to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position into
                    the PROGRAM's content.")
   (has-jumped-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the
                    prevenient instruction has requested a goto-based
                    redirection."))
  (:documentation
    "The ``Jumping-Nybblang-Interpreter'' class is apportioned the dever
     of executing a \"Jumping Nybblang\" program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after
    ((interpreter Jumping-Nybblang-Interpreter) &key)
  "Validates the \"Jumping Nybblang\" program consigned to the
   INTERPRETER's castaldy and returns no value."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (validate-jumping-nybblang-program
    (slot-value interpreter 'program))
  (values))

;;; -------------------------------------------------------

(defun make-jumping-nybblang-interpreter (program-loader)
  "Creates and returns a fresh ``Jumping-Nybblang-Interpreter'' whose
   sole Nybblang program is educed by an incipial request airted
   towards the PROGRAM-LOADER."
  (declare (type Program-Loader program-loader))
  (the Jumping-Nybblang-Interpreter
    (make-instance 'Jumping-Nybblang-Interpreter :program
      (import-next-program program-loader))))

;;; -------------------------------------------------------

(defun get-program-code (interpreter)
  "Returns the source code string comprising the content of the
   \"Jumping Nybblang\" INTERPRETER's program."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the string
    (get-program-content
      (slot-value interpreter 'program))))

;;; -------------------------------------------------------

(defun get-program-code-length (interpreter)
  "Returns the number of characters comprising the \"Jumping Nybblang\"
   INTERPRETER's program code."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the fixnum
    (length
      (get-program-code interpreter))))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the \"Jumping Nybblang\" INTERPRETER's program
   has been processed in its entirety, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (slot-value interpreter 'ip)
          (get-program-code-length interpreter)))))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the \"Jumping Nybblang\" INTERPRTER's instruction pointer
   (IP) to the next operation in its maintained program, if possible,
   and returns no value."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (with-slots (ip) interpreter
    (declare (type fixnum ip))
    (setf ip
      (min
        (+ ip 2)
        (get-program-code-length interpreter))))
  (values))

;;; -------------------------------------------------------

(defun select-next-instruction (interpreter)
  "Advances the \"Jumping Nybblang\" INTERPRTER's instruction pointer
   (IP) to the next operation in its maintained program, if possible,
   and returns no value."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (with-slots (ip has-jumped-p) interpreter
    (declare (type fixnum  ip))
    (declare (type boolean has-jumped-p))
    (if has-jumped-p
      (setf has-jumped-p NIL)
      (advance-to-next-instruction interpreter)))
  (values))

;;; -------------------------------------------------------

(defun valid-jump-target-p (interpreter target)
  "Determines whether the TARGET represents a valid one-based index into
   the \"Jumping Nybblang\" INTERPRETER's program, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type fixnum                       target))
  (the boolean
    (get-boolean-value-of
      (and
        (plusp target)
        (<= target (get-program-code-length interpreter))))))

;;; -------------------------------------------------------

(defun jump-to-instruction-at (interpreter target)
  "Relocates the INTERPRETER's instruction pointer (IP) to the one-based
   TARGET operation position and returns no value.
   ---
   Please heed that the relocation applies to the stratum of
   instructions rather than separate symbols. A TARGET equal to three
   (3), as a forbisen, references the third operation, which, with any
   such composed of two characters always, is tantamount to the sixth
   (3 * 2 = 6) character in the underlying Nybblang program code."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type fixnum                       target))
  (with-slots (ip has-jumped-p) interpreter
    (declare (type fixnum  ip))
    (declare (type boolean has-jumped-p))
    (psetf
      ip
        (if (valid-jump-target-p interpreter target)
          (* (1- target) 2)
          (get-program-code-length interpreter))
      has-jumped-p T))
  (values))

;;; -------------------------------------------------------

(defun extract-current-jumping-command (interpreter)
  "Returns the ``jumping-nybblang-command'' located at the INTERPRETER's
   instruction pointer (IP) position."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the jumping-nybblang-command
    (decode-jumping-program
      (slot-value interpreter 'program)
      (slot-value interpreter 'ip))))

;;; -------------------------------------------------------

(defun jump-antecedent-is-satisfied-p (interpreter)
  "Determines whether, founded upon the INTERPRETER's program memory,
   concretely its stack, a jump action merits its activation's
   homologation, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (plusp
        (peek-into-memory-bottom
          (get-program-memory interpreter))))))

;;; -------------------------------------------------------

(defun retrieve-jump-target (interpreter)
  "Based upon the INTERPRETER stack's bottom element or that aboon the
   same, determines the jump target for a successfully activated
   \"Jumping Nybblang\" instruction of the type \"11\" and returns
   thilk."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (the nybble
    (peek-above-or-into-memory-bottom
      (get-program-memory interpreter))))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Jumping-Nybblang-Interpreter)
                            (command     (eql :push-0)))
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type jumping-nybblang-command     command)
           (ignore                            command))
  (push-bit-to-memory
    (get-program-memory interpreter)
    0)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Jumping-Nybblang-Interpreter)
                            (command     (eql :push-1)))
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type jumping-nybblang-command     command)
           (ignore                            command))
  (push-bit-to-memory
    (get-program-memory interpreter)
    1)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Jumping-Nybblang-Interpreter)
                            (command     (eql :output-character)))
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type jumping-nybblang-command     command)
           (ignore                            command))
  (decode-nybblang-character-to
    (pop-nybble-from-memory
      (get-program-memory interpreter)))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Jumping-Nybblang-Interpreter)
                            (command     (eql :jump-to)))
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (declare (type jumping-nybblang-command     command)
           (ignore                            command))
  (when (jump-antecedent-is-satisfied-p interpreter)
    (jump-to-instruction-at interpreter
      (retrieve-jump-target interpreter)))
  (values))

;;; -------------------------------------------------------

(defun execute-jumping-nybblang-program (interpreter)
  "Executes the \"Jumping Nybblang\" program consigned to the
   INTERPRETER's castaldy and returns no value."
  (declare (type Jumping-Nybblang-Interpreter interpreter))
  (loop until (program-is-completed-p interpreter) do
    (process-command interpreter
      (extract-current-jumping-command interpreter))
    (select-next-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-jumping-nybblang-program-loader (program-loader)
  "Interprets the first \"Jumping Nybblang\" program queried from the
   PROGRAM-LOADER and returns no value."
  (declare (type Program-Loader program-loader))
  (execute-jumping-nybblang-program
    (make-jumping-nybblang-interpreter program-loader))
  (values))

;;; -------------------------------------------------------

(defun interpret-jumping-nybblang-sources (&rest sources)
  "Interprets the \"Jumping Nybblang\" program obtained by the
   concatenation of the Nybblang programs extracted from the SOURCES,
   thilk's compass might amplect both concrete files and directories
   comprehending covenable resources, and returns no value."
  (declare (type (list-of source) sources))
  (execute-jumping-nybblang-program
    (make-jumping-nybblang-interpreter
      (make-concatenative-program-loader-from-source sources)))
  (values))

;;; -------------------------------------------------------

(defun interpret-jumping-nybblang-code (code)
  "Interprets the piece of \"Jumping Nybblang\" source CODE and returns
   no value."
  (declare (type string code))
  (execute-jumping-nybblang-program
    (make-jumping-nybblang-interpreter
      (make-concatenative-program-loader-from-program
        (make-sourceless-nybblang-program code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Nybblang program writer.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-default-file-enumerator
    (&key (field-width 1 field-width-supplied-p))
  "Creates and returns a fresh ``file-enumerator'' function which
   produces for the first input a file nevened \"main.nyb\" in the
   program directory, and for any subsequent one appends to the
   prefixion of \"sub\" the respective line number reduced by an amount
   of one (1), woning in its service a supererogation in the ability
   to specify a fixed size, the FIELD-WIDTH, according to which the
   assigned file number is padded along the sinistral margin by zero
   (0) characters.
   ---
   In a formulaic guise of expression, given the function arguments
     programDirectory: The program's entailing directory.
     lineNumber:       The current line number, with lineNumber >= 1.
   the following output principle holds:
     if lineNumber = 1 then
       return concatenate (programDirectory, \"main.nyb\")
     else
       return concatenate (programDirectory,
                           \"sub\", (lineNumber - 1), \".nyb\")
     end if
   ---
   As a forbisen's adduction, the following shall impute a program
   directory located by the path \"C:/Nybblang/\", waiving the provision
   of a fixed-size field:
     -----------------------------------
     Line number | Generated file name
     ------------+----------------------
     1           | C:/Nybblang/main.nyb
     ...................................
     2           | C:/Nybblang/sub1.nyb
     ...................................
     3           | C:/Nybblang/sub2.nyb
     ...................................
     4           | C:/Nybblang/sub3.nyb
     ...................................
     [...]       | [...]
     -----------------------------------"
  (declare (type (integer 0 *) field-width))
  (declare (type T             field-width-supplied-p))
  (the function
    #'(lambda (program-directory file-number)
        (declare (type pathname      program-directory))
        (declare (type (integer 1 *) file-number))
        (the pathname
          (merge-pathnames program-directory
            (if (= file-number 1)
              "main.nyb"
              (format NIL "sub~v,'0d.nyb"
                (or (and field-width-supplied-p field-width)
                    1)
                (1- file-number))))))))

;;; -------------------------------------------------------

(defun write-nybblang-files
    (code
     program-directory
     &key (name-enumerator
            (make-default-file-enumerator)))
  "Interprets the piece of CODE as a standard Nybblang program, each
   logical line therein construed as a separate program, write the
   representative physical files, whose shared commorancy is
   accommodated by the PROGRAM-DIRECTORY, the ultimate paths, however,
   obtained by an inquisition into the NAME-ENUMERATOR for each
   program, and returns no value.
   ---
   As a forbisen's adduction, the following shall impute a piece of
   standard Nybble source CODE spanning a quadruple line set
     0111
     0011
     1011
     0011
   with the desiderated the PROGRAM-DIRECTORY being located by the
   path \"C:/Nybblang/\", and utilizing the default NAME-ENUMERATOR:
     ---------------------------------------------
     Line number | Program | Generated file
     ------------+---------+----------------------
     1           | 0111    | C:/Nybblang/main.nyb
     .............................................
     2           | 0011    | C:/Nybblang/sub1.nyb
     .............................................
     3           | 1011    | C:/Nybblang/sub2.nyb
     .............................................
     4           | 0011    | C:/Nybblang/sub3.nyb
     ---------------------------------------------"
  (declare (type string          code))
  (declare (type pathname        program-directory))
  (declare (type file-enumerator name-enumerator))
  (with-input-from-string (input-stream code)
    (declare (type string-stream input-stream))
    (loop
      for current-line
        of-type (or null string)
        =       (read-line input-stream NIL NIL)
      and line-number
        of-type (integer 1 *)
        from    1
        by      1
      while current-line do
        (write-to-text-file
          (funcall name-enumerator program-directory line-number)
          current-line)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases for Nybblang files writer.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the "HELLO" printing program.
(write-nybblang-files
"0011
0011
0011
0111
1011
0011
0011
0011
0011
1011
0011
0111
0011
0111
1011
0011
0111
0011
0111
1011
0111
0111
0011
0010"
  (make-pathname
    :directory '(:relative
                 "resources"
                 "nybblang-program-001"))
  :name-enumerator (make-default-file-enumerator :field-width 2))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases for standard Nybblang.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HELLO".
(interpret-standard-Nybblang-sources
  (make-pathname
    :directory '(:relative
                 "resources"
                 "nybblang-program-001")))

;;; -------------------------------------------------------

;; Print "HELLO".
(interpret-standard-Nybblang-loader
  (make-listing-program-loader
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "1011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "1011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "1011")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "1011")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "0111")
    (make-sourceless-nybblang-program "0011")
    (make-sourceless-nybblang-program "0010")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases for "Jumping Nybblang".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine which simulates an input of "0", printing the letter
;; "E" in lieu of an aefauld "0".
(interpret-jumping-nybblang-code "00000000000000001011")

;;; -------------------------------------------------------

;; Truth-machine which simulates an input of "1", perpetually printing
;; the letter "T" in lieu of the digit "1".
(interpret-jumping-nybblang-code "01000000010000001011")
