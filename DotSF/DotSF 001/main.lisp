;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "DotSF", invented by the Esolang user "ArthroStar11" and
;; presented on July 11th, 2021, its data castaldy realized in a signed
;; integer-valued stack of a capacity that permits at most 30,000
;; members, the language's paravaunt haecceity, however, commorant in
;; the necessity to furnish "tips" to the interpreter, "Agatha", as a
;; mandatory parasceve in obviating a program's failure.
;; 
;; 
;; Concept
;; =======
;; The DotSF programming language constitutes specimen founded upon a
;; 32-bit signed integer stack's manipulation by adminicle of one-symbol
;; instructions; concomitant being a recipient of its entheus from the
;; languages brainfuck, Befunge, and Intercal. A kenspeckle element of
;; its haecceity, the interpreter, whose agnomination is assigned as
;; "Agatha", must be attended to by instances of a dedicated "tip"
;; instruction; a violation in this act of propriety instigates an
;; error and a subsequent program cessation.
;; 
;; == DOTSF: A STACK MANIPULATION PROGRAMMING LANGUAGE ==
;; DotSF's foundry proceeds from the handling of signed integer numbers,
;; restricted in their gamut to 32 bits, and accommodated a commorancy
;; in a stack whose mickleness may not exceed the tally of 30,000
;; members.
;; 
;; == INSTRUCTIONS: REPRESENTED BY SINGLETON IDENTIFIERS ==
;; A cognate in many senses to brainfuck's construe of syntactical
;; syntomy, all behests ligated to a participation in a DotSF program
;; enumerate exactly one symbol in expansion.
;; 
;; Any content whose involvement amounts to a state of mateotechny shall
;; a tolerance's dation in a grade tantamount to its inefficacy.
;; Natheless, its presence still ostends a contribution to the entire
;; source code length, whose supputation into the "tip" calculation for
;; the interpreter, which please consult alow in the subsection
;; "AGATHA: AN INTERPRETER EXPECTANT OF EMOLUMENTS", entalents the
;; entity with a, parvipotent yet peisant, impact.
;; 
;; == THE MEMORY: A STACK OF AT MOST 30,000 32-BIT INTEGERS ==
;; The aefauld participant in the language's data department is realized
;; in a finite stack of 32-bit signed integer numbers, the same in its
;; mickleness wists of a maximum of 30,000 members' admission at any
;; time --- an imposition whose provenance accompts for the original
;; brainfuck programming language's implementation in terms of the
;; salvatory's vector design.
;; 
;; == AGATHA: AN INTERPRETER EXPECTANT OF EMOLUMENTS ==
;; A kenspeckle component of its haecceity, DotSF attends to an
;; embodiment's vouchsafement to its interpreter, yclept "Agatha", and
;; whose operations may with the best approximation be construed as a
;; clerk's wike.
;; 
;; This personification's paravaunt epiphenomenal expressions holds its
;; woning in the necessity to propine a vail, proportional to the
;; complete source code length in characters, to Agatha, intended as a
;; conceived "emolument" for her diligence, and in its conception
;; emerging from a feature of the "Intercal" programming language, whose
;; geniculation to polite conduct engenders a "PLEASE" command.
;; 
;; Augumented by an singular tacit, and thus hid, instances, such "tips"
;; are afforded in the guise of the symbol "$". Their tally, in
;; conjunction with the implicit one, requires to mete betwixt inclusive
;; 1/10 and inclusive 1/5 of the complete code size. Upon this gamut's
;; transgression, along any of its bournes, a fatal and abortive error
;; of the type "InappropriateTipError" will be signaled.
;; 
;; A consectary begotten from this poietic approach, no program of a
;; size less than or equal to five (5) bytes may exist.
;; 
;; == CONTROL FLOW: CONDITIONAL SKIPPING AND LABEL-BASED JUMPS
;; Of its control flow facilities, DotSF's potentials bifurcate into
;; a membership's twissel: a conditional skipping construct and a
;; label-based navigation infrastructure thilk entertains no stipulation
;; in its redirection principles.
;; 
;; == LABELS ARE IDENTIFIED BY SINGLE UPPER-CASE LETTERS ==
;; Anenst the latter contingency, and engaged in a consanguinity with
;; all further operative warklumes, a label identifier's dispansion
;; does not rise aboon an aefauld symbol's commitment, levying an
;; imposition for the label definition to adduce majuscular Latin
;; letters, as a proprium of counterdistinguishing potence from the
;; label sojourn instructions, for whom the selfsame set's minuscules
;; ought to be delivered to adhibition.
;; 
;; == ERROR: VISITING THE SAME LABEL MORE THAN 1,000 TIMES IN A ROW ==
;; A desinence in dioristic formalization relates to the actual go-to
;; realization in the constraint's enlisting that the same label, if
;; visited more than 1,000 times in immediate succession, will inflict
;; the program with an error whose consequence is weighted in a mode
;; as peisant as to inchoate the execution's abortion.
;; 
;; 
;; Instructions
;; ============
;; DotSF's instruction set accompts for a cardinality of 24 members,
;; their ambits' distribution such in diversity as to engage in the
;; tendence to arithmetics, input and output communications, control
;; flow duction, and a peculiar intercourse with the personified
;; interpreter itself.
;; 
;; == OVERVIEW ==
;; The following apercu's cynosure shall be a sufficient mete of
;; nortelry's adhibition concerning the language's operative comptences.
;; 
;; Please heed the ensconcement of succedaneous tmemata by a jumelle of
;; braces, "{" and "}", their entirety, including these markers, require
;; a substitution by actual DotSF code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command          | Effect
;;   -----------------+------------------------------------------------
;;   0                | Pushes the number zero (0) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   1                | Pushes the number one (1) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   2                | Pushes the number two (2) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   3                | Pushes the number three (3) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   4                | Pushes the number four (4) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   5                | Pushes the number five (5) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   6                | Pushes the number six (6) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   7                | Pushes the number seven (7) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   9                | Pushes the number eight (8) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   9                | Pushes the number nine (9) onto the memory
;;                    | stack's top.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   +                | Pops the first element, here nevened "a", from
;;                    | the memory stack's top, pops the new top
;;                    | element, "b", and pushes the sum (a + b) onto
;;                    | the stack's top.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction, the following holds:
;;                    |   let a <- pop from stack
;;                    |   let b <- pop from stack
;;                    |   let c <- a + b
;;                    |   push c onto stack
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   -                | Pops the first element, here nevened "a", from
;;                    | the memory stack's top, pops the new top
;;                    | element, "b", and pushes the difference (a - b)
;;                    | onto the stack's top.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction, the following holds:
;;                    |   let a <- pop from stack
;;                    |   let b <- pop from stack
;;                    |   let c <- a - b
;;                    |   push c onto stack
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   *                | Pops the first element, here nevened "a", from
;;                    | the memory stack's top, pops the new top
;;                    | element, "b", and pushes the product (a * b)
;;                    | onto the stack's top.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction, the following holds:
;;                    |   let a <- pop from stack
;;                    |   let b <- pop from stack
;;                    |   let c <- a * b
;;                    |   push c onto stack
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   /                | Pops the first element, here nevened "a", from
;;                    | the memory stack's top, pops the new top
;;                    | element, "b", and pushes the integer quotient
;;                    | (a / b) onto the stack's top.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction, the following holds:
;;                    |   let a <- pop from stack
;;                    |   let b <- pop from stack
;;                    |   let c <- a / b
;;                    |   push c onto stack
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   %                | Pops the first element, here nevened "a", from
;;                    | the memory stack's top, pops the new top
;;                    | element, "b", and pushes the remainder
;;                    | (a modulo b) onto the stack's top.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction, the following holds:
;;                    |   let a <- pop from stack
;;                    |   let b <- pop from stack
;;                    |   let c <- a modulo b
;;                    |   push c onto stack
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   _                | Duplicates the memory stack's top element by
;;                    | popping the same and pushing it twice onto the
;;                    | stack.
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ==================================================================
;;   .                | Queries the standard input conduit for a signed
;;                    | or unsigned integer number and pushes the same
;;                    | onto the top of the memory stack.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   ,                | Queries the standard input conduit for a
;;                    | character and pushes its Unicode code point
;;                    | onto the top of the memory stack.
;;                    |------------------------------------------------
;;                    | If the memory stack's capacity of 30,000
;;                    | positions is exceeded by the new element's
;;                    | introduction, an error of the type
;;                    | "FullStackError" is signaled.
;;   ..................................................................
;;   :                | Pops the top element from the memory stack and
;;                    | prints its verbatim numeric form to the
;;                    | standard output conduit, the displayed content
;;                    | commencing on a fresh line and terminating in
;;                    | a single newline character.
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;   ..................................................................
;;   ;                | Pops the top element from the memory stack and
;;                    | prints the character whose Unicode code point
;;                    | corresponds to this element to the standard
;;                    | output conduit, the displayed content neither
;;                    | preceded nor prefixed by any advenient
;;                    | insertions.
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyStackError" is signaled.
;;   ==================================================================
;;   [ {body} ]       | Pops the top element from the memory stack; if
;;                    | the same equals zero (0), skips the {body}
;;                    | tmema and relocates the instruction pointer
;;                    | (IP) to the position immediately succeeding the
;;                    | matching "]" token; otherwise proceeds as
;;                    | usual.
;;                    |------------------------------------------------
;;                    | {body} must be a sequence composed of zero or
;;                    | more instructions.
;;                    |------------------------------------------------
;;                    | If the memory stack is empty at the instant of
;;                    | the "[" token's encounter, an error of the type
;;                    | "EmptyStackError" is signaled.
;;   ..................................................................
;;   {latinMajuscule} | Defines a label agnominated by the identifier
;;                    | {latinMajuscule}.
;;                    |------------------------------------------------
;;                    | {latinMajuscule} must be a upper-case Latin
;;                    | letter, scilicet, a character from the finite
;;                    | set { "A", "B", "C", ..., "Z" }.
;;                    |------------------------------------------------
;;                    | If a label amenable to the {latinMajuscule} is
;;                    | defined more than once in a program, an error
;;                    | of the type "DuplicateLabelError" is signaled.
;;   ..................................................................
;;   {latinMinuscule} | Relocates the instruction pointer (IP) to the
;;                    | position of the label whose identifier
;;                    | corresponds to the upper-case variant of the
;;                    | {latinMinuscule}.
;;                    |------------------------------------------------
;;                    | {latinMinuscule} must be a lower-case Latin
;;                    | letter, scilicet, a character from the finite
;;                    | set { "a", "b", "c", ..., "z" }.
;;                    |------------------------------------------------
;;                    | If no label amenable to the majuscular form of
;;                    | the lower-case {latinMinuscule} is defined in
;;                    | the program, an error of the type
;;                    | "MissingLabelError" is signaled.
;;                    |------------------------------------------------
;;                    | If the same label is visited more than 1,000
;;                    | times in immediate succesion, an error of the
;;                    | type "TooManyJumpsError" is signaled.
;;   ==================================================================
;;   $                | Propines a tip to the interpreter Agatha, once
;;                    | for each occurrency of this token in the
;;                    | program, serving as an "emolument" for the
;;                    | operative moil.
;;                    |------------------------------------------------
;;                    | The tally of "$" symbols in the source code,
;;                    | incremented by an implicitly vouchsafed,
;;                    | "hidden" instance, in proportion to the entire
;;                    | source code must describe a ratio of at least
;;                    | 1/10 and at most 1/5. Otherwise an error of the
;;                    | type "InappropriateTipError" is signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre senescence and circumference as two beneficial factors, the
;; DotSF specification in its autochthonous design retains a destitution
;; of several disambiguating diorisms. A subset desumed from these
;; lacunae shall serve as the hyle to the following sections.
;; 
;; == WHICH HOMOLOGATION POLICY APPLIES TO NON-OPERATIVE CHARACTERS? ==
;; The DotSF protolog's treatise on those symbols to whom an operation
;; representation's competence is assigned desists from a complementing
;; epexegesis' vouchsafement concerning adscititious content.
;; 
;; A twifold gravity's governance applies to this participation scheme's
;; denouement:
;; 
;;   (a) The contingency for apostilles, akin to brainfuck's
;;       magnanamity, will be endorsed or eloigned.
;;   
;;   (b) The supputation of the interpreter's, Agatha's, nuncupated
;;       vail might be affected by this choice.
;; 
;; Deprived of an official enunciation, it has been adjudged to resolve
;; to the following imputations:
;; 
;;   (1) All non-operative tokens' involvement constitutes a thing of
;;       tolerance, whence a consectary's emergence ensues that these
;;       in effect present no-operations (NOPs). This deportment's
;;       provenance and its vindication accompt for a derivation from
;;       the respective brainfuck notion.
;;   
;;   (2) Non-operative characters count towards the entire source code
;;       length's formal conformation, installing a requisitum for
;;       Agatha's tip's proportional accrementation.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization has been exercised in the programming
;; language Common Lisp, the actual evaluation tier wisting of a
;; prevenience in its single symbols' transcription into dedicated
;; instruction objects, attending to no-operative content as
;; no-operation (NOP) specializations.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-09-04
;; 
;; Sources:
;;   [esolang2025DotSF]
;;   The Esolang contributors, "DotSF", June 28th, 2025
;;   URL: "https://esolangs.org/wiki/DotSF"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination ensues from the TYPE-NAME,
   and whose formal parameters accompt for the LAMBDA-LIST's ipsissima
   verba dation, the implementation being the BODY forms' furnishment,
   the same are granted access to the docimasy's subject by name of the
   CANDIDATE-VARIABLE, with the desinent BODY form's primary result
   contributing the probing effort's conclusion, a \"generalized
   boolean\" value of \"true\" being administered the construe as the
   candidate's compatibility with the type diorism, while a \"false\"
   output serves in its rejection.
   ---
   The first BODY form, upon its resolution to a string object, will be
   interpreted as the type definition's documentation string, whence
   is actuated its exclusive appropriation for this exact cause."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop     body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   comprehends zero or more entries, each such a twissel edified from
   a key adhering to the KEY-TYPE and an associated value of the
   VALUE-TYPE, for both is stipulated the generic sentinel ``*'' as a
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
   elements, each member of thilk complies with the ELEMENT-TYPE, for
   the same is specified the generic sentinel ``*'' as a default."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(define-predicated-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a list-based tuple whose conformation
   ostends an equinumerant membership to the ELEMENT-TYPES, and whose
   element at the i-th index subsumes into the species imposed by the
   i-th type specifier commorant among the ELEMENT-TYPES."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length element-types))
    (every #'typep
      (the list candidate)
      element-types)))

;;; -------------------------------------------------------

(deftype dotsf-program ()
  "The ``dotsf-program'' type defines an executable DotSF program as a
   one-dimensional simple array of ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype 32-bit-integer ()
  "The ``32-bit-integer'' type defines a signed integer number compact
   of 32 bits."
  '(signed-byte 32))

;;; -------------------------------------------------------

(deftype integer-stack ()
  "The ``integer-stack'' type defines the DotSF program memory stack as
   an adjustable vector enumerating zero or more numeric elements of the
   type ``32-bit-integer''."
  '(vector 32-bit-integer *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   ambit of which amplects, without the claim of exhaustion, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype skip-table ()
  "The ``skip-table'' type defines a unilateral affiliation betwixt a
   parsed DotSF program's skip start and end points, from the former
   airted towards the latter, by adminiculum of their zero-based
   positions into the program's instruction sequence.
   ---
   Please be not distracted by the ambivalency in the agnomination, as
   the \"skip table\" nomenclature concomitantly, and more popularly,
   also appertains to an implementation of the abstract data type (ADT)
   \"map\", which in our particular case does not register any meaning."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines a unilateral mapping betwixt a
   label name and its zero-based position into the ensconcing DotSF
   program; its manifestation that of a hash table, the keys of which
   allocate the agnominations as ``standard-char'' instances, responding
   with ``fixnum'' indices."
  '(hash-table-of standard-char fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition DotSF-Error (error)
  ()
  (:documentation
    "The ``DotSF-Error'' condition type serves as a firmament
     conceptually ligating those conditions dedicated to the apprizal
     about anomalous situations emerging during a DotSF program's
     evaluation."))

;;; -------------------------------------------------------

(define-condition Empty-Program-Error (DotSF-Error)
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        empty-program-error-program
    :type          dotsf-program
    :documentation "The parsed DotSF program whose state of destitution
                    in commands has instigated this anomaly."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Program-Error condition))
      (declare (ignore                   condition))
      (declare (type destination         stream))
      (format stream "Based upon the tipping policy, empty programs ~
                      are not homologated.")))
  (:documentation
    "The ``Empty-Program-Error'' type serves in the apprizal about an
     anomalous situation whose etiology emerges from the provision of a
     piece of DotSF source code devoid of any commands."))

;;; -------------------------------------------------------

(define-condition Inappropriate-Tip-Error (DotSF-Error)
  ((minimum-tip-ratio
    :initarg       :minimum-tip-ratio
    :initform      (error "Missing minimum tip ratio.")
    :reader        inappropriate-tip-error-minimum-tip-ratio
    :type          (rational 0 100)
    :documentation "The inclusive minimum ratio of tip in relation to a
                    program's command count expected.")
   (maximum-tip-ratio
    :initarg       :maximum-tip-ratio
    :initform      (error "Missing maximum tip ratio.")
    :reader        inappropriate-tip-error-maximum-tip-ratio
    :type          (rational 0 100)
    :documentation "The inclusive maximum ratio of tip in relation to a
                    program's command count expected.")
   (actual-tip-ratio
    :initarg       :actual-tip-ratio
    :initform      (error "Missing tip ratio.")
    :reader        inappropriate-tip-error-actual-tip-ratio
    :type          (rational 0 *)
    :documentation "The actual, contingently rounded, ratio of tips in
                    relation to a program's command count.")
   (number-of-commands
    :initarg       :number-of-commands
    :initform      (error "Missing number of commands.")
    :reader        inappropriate-tip-error-number-of-commands
    :type          fixnum
    :documentation "The tally of commands comprising the erroneous
                    program."))
  (:report
    (lambda (condition stream)
      (declare (type Inappropriate-Tip-Error condition))
      (declare (type destination             stream))
      (format stream "The tip to propine to Agatha must be betwixt ~
                      ~a and ~a of the code; but your dation of ~a ~
                      for ~d command~:p is too ~:[low~;high~]."
        (inappropriate-tip-error-minimum-tip-ratio  condition)
        (inappropriate-tip-error-maximum-tip-ratio  condition)
        (inappropriate-tip-error-actual-tip-ratio   condition)
        (inappropriate-tip-error-number-of-commands condition)
        (>= (inappropriate-tip-error-actual-tip-ratio  condition)
            (inappropriate-tip-error-maximum-tip-ratio condition)))))
  (:documentation
    "The ``Inappropriate-Tip-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology wones in the
     provision of a piece of DotSF source code whose ratio on tip
     commands violates the admissible gamut at either of its
     lateralities' twissel."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (DotSF-Error)
  ((name
    :initarg       :name
    :initform      (error "Missing label name.")
    :reader        duplicate-label-error-name
    :type          standard-char
    :documentation "The label name whose existency during the attempt
                    of a definition has instigated this error.")
   (position
    :initarg       :position
    :initform      (error "Missing label position.")
    :reader        duplicate-label-error-position
    :type          fixnum
    :documentation "The zero-based index into the parsed DotSF program
                    of the label NAME's first definition."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type destination           stream))
      (format stream "The label name \"~c\" has already been defined ~
                      at the position ~d."
        (duplicate-label-error-name     condition)
        (duplicate-label-error-position condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology is begotten in the
     illicit trial to define a new label by a name already assigned on
     a prior encheson during a DotSF program."))

;;; -------------------------------------------------------

(define-condition Missing-Label-Error (DotSF-Error)
  ((name
    :initarg       :name
    :initform      (error "Missing label name.")
    :reader        missing-label-error-name
    :type          standard-char
    :documentation "The optated label's name whose absence has inflicted
                    the program with this error."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Label-Error condition))
      (declare (type destination         stream))
      (format stream "No label with the name \"~c\" could be retrieved."
        (missing-label-error-name condition))))
  (:documentation
    "The ``Missing-Label-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology is elicited from the
     trial to request or navigate to a label by a name not defined in
     the ensconcing DotSF program."))

;;; -------------------------------------------------------

(define-condition Too-Many-Jumps-Error (DotSF-Error)
  ((label-name
    :initarg       :label-name
    :initform      (error "Missing label name.")
    :reader        too-many-jumps-error-label-name
    :type          standard-char
    :documentation "The name of the label whose attempt to sojourn to
                    a tally of times greater than the THRESHOLD has
                    instigated this erroneous state.")
   (threshold
    :initarg       :threshold
    :initform      (error "Missing threshold.")
    :reader        too-many-jumps-error-threshold
    :type          (integer 0 *)
    :documentation "The inclusive maximum number of consecutive jumps
                    to the LABEL-NAME the same have been attempted to
                    transgress."))
  (:report
    (lambda (condition stream)
      (declare (type Too-Many-Jumps-Error condition))
      (declare (type destination          stream))
      (format stream "Cannot jump to the label \"~c\" more than ~d ~
                      times in immediate succession."
        (too-many-jumps-error-label-name condition)
        (too-many-jumps-error-threshold  condition))))
  (:documentation
    "The ``Too-Many-Jumps-Error'' condition type serves in the apprizal
     about an anomalous situation whose etiology emerges from the trial
     to navigate to the same label in a consecutive succession by an
     inadmissibly high cerebritude."))

;;; -------------------------------------------------------

(define-condition Empty-Integer-Stack-Error (DotSF-Error)
  ((stack
    :initarg       :stack
    :initform      (error "Missing stack.")
    :reader        empty-integer-stack-error-stack
    :type          integer-stack
    :documentation "The empty stack whose indagation or modulation has
                    instigated this error.")
   (attempted-operation
    :initarg       :attempted-operation
    :initform      "peek into or pop from"
    :reader        empty-integer-stack-error-attempted-operation
    :type          simple-string
    :documentation "A string representing the attempted operation
                    having instigated this erroneous situation."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Integer-Stack-Error condition))
      (declare (type destination               stream))
      (format stream "Cannot ~a the empty stack ~a."
        (empty-integer-stack-error-attempted-operation condition)
        (empty-integer-stack-error-stack               condition))))
  (:documentation
    "The ``Empty-Integer-Stack-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is realized
     in the attempt indagate or remove from an empty stack."))

;;; -------------------------------------------------------

(define-condition Full-Integer-Stack-Error (DotSF-Error)
  ((stack
    :initarg       :stack
    :initform      (error "Missing stack.")
    :reader        full-integer-stack-error-stack
    :type          integer-stack
    :documentation "The stack which, during its state of surfeiture, has
                    been ordered to receive further elements.")
   (maximum-size
    :initarg       :maximum-size
    :initform      (error "Missing maximum size.")
    :reader        full-integer-stack-error-maximum-size
    :type          fixnum
    :documentation "The maximum admissible size of the STACK which
                    through an attempted operation has been subject to
                    a violation."))
  (:report
    (lambda (condition stream)
      (declare (type Full-Integer-Stack-Error condition))
      (declare (type destination              stream))
      (format stream "The stack ~a may not contain more than ~d ~
                      elements."
        (full-integer-stack-error-stack        condition)
        (full-integer-stack-error-maximum-size condition))))
  (:documentation
    "The ``Full-Integer-Stack-Error'' condition type serves in the
     apprizal about an anomalous situation whose etiology is realized
     in the attempt to add an element to an already surfeited stack."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its facette as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount for the same, returning for
   a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction class generator operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-constructor-signature (class-name)
  "Generates for the CLASS-NAME a constructor following the forbisen
   ``make-CLASS-NAME'', its sole argument being the mandatory
   ``position-in-source'', and returns the result as a list of two
   elements:
   
     (1) The symbolic constructor name.
     (2) The parameter list specified as ``(position-in-source)''.
   ---
   The thus begotten list ostends the weftage:
     (make-{CLASS-NAME} (position-in-source))"
  (declare (type symbol class-name))
  (the (tuple-of T T)
    (list
      `,(intern (format NIL "~:@(make-~a~)" class-name))
      `(position-in-source))))

;;; -------------------------------------------------------

(defmacro define-instruction-subclass
    (class-name
     &optional (documentation-string ""))
  "Defines a new subclass of the ``Instruction'' class, receiving an
   optional description via the DOCUMENTATION-STRING, and returns the
   thus established structure class' name as a symbol."
  `(defstruct (,class-name
     (:include     Instruction)
     (:constructor ,@(build-constructor-signature class-name)))
     ,documentation-string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' abstract class' vindication is realized in a
   common foundry's establishment, upon which shall be edified all
   concrete classes in their pursuit to model DotSF operations."
  (position-in-source (error "Missing instruction position.")
                      :type      fixnum
                      :read-only T))

;;; -------------------------------------------------------

(define-instruction-subclass Push-0-Instruction
  "The ``Push-0-Instruction'' class serves in the representation of the
   DotSF \"0\" operation, the same pushes the digit zero (0) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-1-Instruction
  "The ``Push-1-Instruction'' class serves in the representation of the
   DotSF \"1\" operation, the same pushes the digit one (1) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-2-Instruction
  "The ``Push-2-Instruction'' class serves in the representation of the
   DotSF \"2\" operation, the same pushes the digit two (2) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-3-Instruction
  "The ``Push-3-Instruction'' class serves in the representation of the
   DotSF \"3\" operation, the same pushes the digit three (3) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-4-Instruction
  "The ``Push-4-Instruction'' class serves in the representation of the
   DotSF \"4\" operation, the same pushes the digit four (4) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-5-Instruction
  "The ``Push-5-Instruction'' class serves in the representation of the
   DotSF \"5\" operation, the same pushes the digit five (5) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-6-Instruction
  "The ``Push-6-Instruction'' class serves in the representation of the
   DotSF \"6\" operation, the same pushes the digit six (6) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-7-Instruction
  "The ``Push-7-Instruction'' class serves in the representation of the
   DotSF \"7\" operation, the same pushes the digit seven (7) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-8-Instruction
  "The ``Push-8-Instruction'' class serves in the representation of the
   DotSF \"8\" operation, the same pushes the digit eight (8) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Push-9-Instruction
  "The ``Push-9-Instruction'' class serves in the representation of the
   DotSF \"9\" operation, the same pushes the digit nine (9) onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Add-Instruction
  "The ``Add-Instruction'' class serves in the representation of the
   DotSF \"+\" operation, the same pops two elements from the program
   memory's stack and pushes their sum.")

;;; -------------------------------------------------------

(define-instruction-subclass Subtract-Instruction
  "The ``Subtract-Instruction'' class serves in the representation of
   the DotSF \"-\" operation, the same pops two elements from the
   program memory's stack and pushes their difference.")

;;; -------------------------------------------------------

(define-instruction-subclass Multiply-Instruction
  "The ``Multiply-Instruction'' class serves in the representation of
   the DotSF \"*\" operation, the same pops two elements from the
   program memory's stack and pushes their product.")

;;; -------------------------------------------------------

(define-instruction-subclass Divide-Instruction
  "The ``Divide-Instruction'' class serves in the representation of
   the DotSF \"/\" operation, the same pops two elements from the
   program memory's stack and pushes their quotient.")

;;; -------------------------------------------------------

(define-instruction-subclass Remainder-Instruction
  "The ``Remainder-Instruction'' class serves in the representation of
   the DotSF \"%\" operation, the same pops two elements from the
   program memory's stack and pushes their remainder.")

;;; -------------------------------------------------------

(define-instruction-subclass Duplicate-Instruction
  "The ``Duplicate-Instruction'' class serves in the representation of
   the DotSF \"_\" operation, the same duplicates the stack's top
   element.")

;;; -------------------------------------------------------

(define-instruction-subclass Input-Integer-Instruction
  "The ``Input-Integer-Instruction'' class serves in the representation
   of the DotSF \".\" operation, the same queries the standard input
   conduit for a signed or unsigned integer number, ere pushing thilk
   onto the program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Input-Character-Instruction
  "The ``Input-Character-Instruction'' class serves in the
   representation of the DotSF \",\" operation, the same queries the
   standard input conduit for a character, ere pushing thilk onto the
   program memory's stack.")

;;; -------------------------------------------------------

(define-instruction-subclass Output-Integer-Instruction
  "The ``Output-Integer-Instruction'' class serves in the
   representation of the DotSF \":\" operation, the same prints the
   program memory's top stack element in its verbatim numeric form to
   the standard output conduit.")

;;; -------------------------------------------------------

(define-instruction-subclass Output-Character-Instruction
  "The ``Output-Character-Instruction'' class serves in the
   representation of the DotSF \";\" operation, the same prints the
   character whose ASCII code concurs with the program memory's top
   stack element to the standard output conduit.")

;;; -------------------------------------------------------

(define-instruction-subclass Skip-Start-Instruction
  "The ``Skip-Start-Instruction'' class serves in the representation
   of the DotSF \"[\" operation, the same skips the program tmema
   ensconced by this token and the matching \"]\" operation in the case
   of a zero-valued program memory's top stack element.")

;;; -------------------------------------------------------

(define-instruction-subclass Skip-End-Instruction
  "The ``Skip-End-Instruction'' class serves in the representation
   of the DotSF \"]\" operation, the same returns to the matching \"[\"
   operation in the case of a non-zero-valued program memory's top
   stack element.")

;;; -------------------------------------------------------

(defstruct (Define-Label-Instruction
  (:include Instruction)
  (:constructor make-define-label-instruction (name
                                               position-in-source)))
  "The ``Define-Label-Instruction'' class serves in the representation
   of the DotSF operation communicated in a Latin majuscule, the same
   serves in a program label's definition."
  (name (error "Missing label name.")
        :type      standard-char
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Visit-Label-Instruction
  (:include Instruction)
  (:constructor make-visit-label-instruction (name
                                              position-in-source)))
  "The ``Visit-Label-Instruction'' class serves in the representation
   of the DotSF operation communicated in a Latin minuscule, the same
   serves in the navigation to a program label's location."
  (name (error "Missing label name.")
        :type      standard-char
        :read-only T))

;;; -------------------------------------------------------

(define-instruction-subclass Tip-Instruction
  "The ``Tip-Instruction'' class serves in the representation of the
   DotSF \"$\" operation, the same affords a propine to the interpreter,
   \"Agatha\", for its moil.")

;;; -------------------------------------------------------

(define-instruction-subclass NOP-Instruction
  "The ``NOP-Instruction'' class serves in the representation of a
   no-operation (NOP, no-op), a succedaneum for a non-operative token's
   participation in a DotSF program.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun majuscular-letter-p (candidate)
  "Determines whether the CANDIDATE represents a majuscular Latin
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (alpha-char-p candidate)
        (upper-case-p candidate)))))

;;; -------------------------------------------------------

(defun minuscular-letter-p (candidate)
  "Determines whether the CANDIDATE represents a minuscular Latin
   letter, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and
        (alpha-char-p candidate)
        (lower-case-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of DotSF program operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dotsf-program (instructions)
  "Creates and returns a fresh ``dotsf-program'' comprehending the
   INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the dotsf-program
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tip calculation and probing operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (rational 1/10 1/10) +MINIMUM-TIP-RATIO+))
(declaim (type (rational 1/5  1/5)  +MAXIMUM-TIP-RATIO+))

;;; -------------------------------------------------------

(defparameter +MINIMUM-TIP-RATIO+ 1/10
  "The inclusive minimum share of tip commands in a DotSF program.")

(defparameter +MAXIMUM-TIP-RATIO+ 1/5
  "The inclusive maximum share of tip commands in a DotSF program.")

;;; -------------------------------------------------------

(defun count-tips (program)
  "Returns the total number of tips amplected in the DotSF PROGRAM, the
   same also includes in this tally the implicit instance."
  (declare (type dotsf-program program))
  (the fixnum
    (1+
      (count-if #'tip-instruction-p program))))

;;; -------------------------------------------------------

(defun tip-is-appropriate-p (tip-ratio)
  "Determines whether the TIP-RATIO constitutes a valid percentage of
   the DotSF commands, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type (rational 0 *) tip-ratio))
  (the boolean
    (get-boolean-value-of
      (<= +MINIMUM-TIP-RATIO+
          tip-ratio
          +MAXIMUM-TIP-RATIO+))))

;;; -------------------------------------------------------

(defun collate-tip-statistics (program)
  "Collates the statistics of most pertinence from the DotSF PROGRAM in
   the subject of the tip supputation and returns the result as three
   values:
     (1) The number of commands comprising the PROGRAM.
     (2) If the PROGRAM amplects more than zero (0) commands, the ratio
         of tip commands to the total instruction tally; otherwise the
         sentinel zero (0).
     (3) A Boolean flag which determines whether the tip ratio accompts
         for a valid quantity, resolving on conformation to a
         ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type dotsf-program program))
  (let ((number-of-commands (length     program))
        (number-of-tips     (count-tips program)))
    (declare (type fixnum number-of-commands))
    (declare (type fixnum number-of-tips))
    (let ((tip-ratio
            (if (plusp number-of-commands)
              (/ number-of-tips number-of-commands)
              0)))
      (declare (type (rational 0 *) tip-ratio))
      (the (values fixnum (rational 0 *) boolean)
        (values
          number-of-commands
          tip-ratio
          (tip-is-appropriate-p tip-ratio))))))

;;; -------------------------------------------------------

(defun validate-tip (program)
  "Determines whether the DotSF PROGRAM's tipping behavior complies with
   the stipulated policy, returning on confirmation the unmodified
   PROGRAM itself; otherwise signals a connable error.
   ---
   The contingency for abortive deportments trifurcates into the
   following trajectories:
     (1) If the PROGRAM is empty, an error of the type
         ``Empty-Program-Error'' is signaled.
     (2) If the ratio of tip instructions, incremented by the tacit
         tip amount, to the total command tally violates the required
         minimum, an error of the type ``Inappropriate-Tip-Error'' is
         signaled.
     (3) If the ratio of tip instructions, incremented by the tacit
         tip amount, to the total command tally violates the homologated
         maximum, an error of the type ``Inappropriate-Tip-Error'' is
         signaled."
  (declare (type dotsf-program program))
  (multiple-value-bind
      (number-of-commands tip-ratio tip-is-appropriate-p)
      (collate-tip-statistics program)
    (declare (type fixnum         number-of-commands))
    (declare (type (rational 0 *) tip-ratio))
    (declare (type boolean        tip-is-appropriate-p))
    (the dotsf-program
      (cond
        ((zerop number-of-commands)
          (error 'Empty-Program-Error :program program))
        ((not tip-is-appropriate-p)
          (error 'Inappropriate-Tip-Error
            :number-of-commands number-of-commands
            :minimum-tip-ratio  +MINIMUM-TIP-RATIO+
            :maximum-tip-ratio  +MAXIMUM-TIP-RATIO+
            :actual-tip-ratio   tip-ratio))
        (T
          program)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-dotsf-program (code)
  "Parses the piece of DotSF source CODE and returns a covenable
   ``dotsf-program'' representation of its ensconced instructions."
  (declare (type string code))
  (the dotsf-program
    (validate-tip
      (make-dotsf-program
        (loop
          for current-token    of-type character across code
          and current-position of-type fixnum    from   0 by 1
          collect
            (case current-token
              (#\0 (make-push-0-instruction           current-position))
              (#\1 (make-push-1-instruction           current-position))
              (#\2 (make-push-2-instruction           current-position))
              (#\3 (make-push-3-instruction           current-position))
              (#\4 (make-push-4-instruction           current-position))
              (#\5 (make-push-5-instruction           current-position))
              (#\6 (make-push-6-instruction           current-position))
              (#\7 (make-push-7-instruction           current-position))
              (#\8 (make-push-8-instruction           current-position))
              (#\9 (make-push-9-instruction           current-position))
              (#\+ (make-add-instruction              current-position))
              (#\- (make-subtract-instruction         current-position))
              (#\* (make-multiply-instruction         current-position))
              (#\/ (make-divide-instruction           current-position))
              (#\% (make-remainder-instruction        current-position))
              (#\_ (make-duplicate-instruction        current-position))
              
              (#\. (make-input-integer-instruction    current-position))
              (#\, (make-input-character-instruction  current-position))
              (#\: (make-output-integer-instruction   current-position))
              (#\; (make-output-character-instruction current-position))
              
              (#\[ (make-skip-start-instruction       current-position))
              (#\] (make-skip-end-instruction         current-position))
              
              (#\$ (make-tip-instruction              current-position))
              
              (otherwise
                (cond
                  ((majuscular-letter-p current-token)
                    (make-define-label-instruction
                      current-token
                      current-position))
                  
                  ((minuscular-letter-p current-token)
                    (make-visit-label-instruction
                      current-token
                      current-position))
                  
                  (T
                    (make-nop-instruction current-position))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of skip table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-skip-table ()
  "Creates and returns a fresh, initially vacant ``skip-table''."
  (the skip-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-skip-points (table start-point end-point)
  "Connects the skip START-POINT with the respective END-POINT in the
   skip TABLE and returns no value."
  (declare (type skip-table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (setf (gethash start-point table) end-point)
  (values))

;;; -------------------------------------------------------

(defun build-skip-table-for (program)
  "Creates and returns a fresh ``skip-table'' which ligates the DotSF
   program's skip start points with the respective end posts by
   adminiculum of their zero-based indices into the instruction
   sequence."
  (declare (type dotsf-program program))
  (let ((skip-table   (prepare-empty-skip-table))
        (start-points NIL))
    (declare (type skip-table       skip-table))
    (declare (type (list-of fixnuM) start-points))
    (loop
      for current-instruction of-type Instruction across program
      and current-position    of-type fixnum      from   0 by 1
      do
        (typecase current-instruction
          (Skip-Start-Instruction
            (push current-position start-points))
          (Skip-End-Instruction
            (if start-points
              (connect-skip-points skip-table
                (pop start-points)
                current-position)
              (error "Unmatched skip end instruction at position ~d."
                (instruction-position-in-source current-instruction))))
          (otherwise
            NIL)))
    (the skip-table skip-table)))

;;; -------------------------------------------------------

(defun locate-skip-end-point (table start-point)
  "Returns the zero-based position of the end point associated with the
   skip instruction's START-POINT as registered in the skip TABLE; or,
   upon its disrespondency, signals an error of an unspecified type."
  (declare (type skip-table table))
  (declare (type fixnum     start-point))
  (the fixnum
    (or (gethash start-point table)
        (error "No end point defined for the skip start instruction ~
                at position ~d."
          start-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-label-table ()
  "Creates and returns a fresh and initially vacant ``label-table''."
  (the label-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun register-label (labels name position)
  "Associates the label NAME with its zero-based POSITION into the
   ensconcing DotSF program, stores this affiliation in the LABELS
   table, and returns no value."
  (declare (type label-table   labels))
  (declare (type standard-char name))
  (declare (type fixnum        position))
  (multiple-value-bind (extant-position name-is-already-in-use-p)
      (gethash name labels)
    (declare (type (or null fixnum) extant-position))
    (declare (type T                name-is-already-in-use-p))
    (if name-is-already-in-use-p
      (error 'Duplicate-Label-Error
             :name     name
             :position extant-position)
      (setf (gethash name labels) position)))
  (values))

;;; -------------------------------------------------------

(defun build-label-table-for (program)
  "Creates and returns a fresh ``label-table'' lippened with the
   castaldy over the DotSF PROGRAM's label definition, by affiliating
   each such occurrency with its zero-based index into the PROGRAM.
   ---
   Upon the discovery of a label name's twifold definition, an error of
   the type ``Duplicate-Label-Error'' will be signaled."
  (declare (type dotsf-program program))
  (let ((labels (prepare-empty-label-table)))
    (declare (type label-table labels))
    (loop
      for current-instruction of-type Instruction across program
      and current-position    of-type fixnum      from   0 by 1
      
      when (define-label-instruction-p current-instruction) do
        (register-label labels
          (char-downcase
            (define-label-instruction-name current-instruction))
          current-position))
    (the label-table labels)))

;;; -------------------------------------------------------

(defun locate-label (labels name)
  "Returns the zero-based position of the label amenable to the NAME, as
   registered in the LABELS table; or, upon its disrespondency, signals
   an error of the type ``Missing-Label-Error''."
  (declare (type label-table   labels))
  (declare (type standard-char name))
  (the fixnum
    (or (gethash name labels)
        (error 'Missing-Label-Error :name name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label jump counter operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 1000 1000)
               +MAXIMUM-NUMBER-OF-CONSECUTIVE-JUMPS+))

;;; -------------------------------------------------------

(defparameter +MAXIMUM-NUMBER-OF-CONSECUTIVE-JUMPS+ 1000
  "The inclusive maximum number of consecutive times that the same label
   may be sojourned.")

;;; -------------------------------------------------------

(defstruct (Jump-Counter
  (:constructor make-jump-counter ()))
  "The ``Jump-Counter'' class applies itself to the registration of the
   tally of times that a label is sojourned in a consecutive
   succession, contingently producing an error upon the maximum
   accompt's violation."
  (last-label-name NIL
                   :type      (or null standard-char)
                   :read-only NIL)
  (number-of-jumps 1
                   :type      (integer 1 1000)
                   :read-only NIL))

;;; -------------------------------------------------------

(defun check-if-current-label-can-be-visited (counter)
  "Determines whether the most recently label name, as stored in the
   jump COUNTER, may be visited at least one more time, returning on
   confirmation the COUNTER itself; otherwise signals an error of the
   type ``Too-Many-Jumps-Error''."
  (declare (type Jump-Counter counter))
  (when (>= (jump-counter-number-of-jumps counter)
            +MAXIMUM-NUMBER-OF-CONSECUTIVE-JUMPS+)
    (error 'Too-Many-Jumps-Error
      :label-name (jump-counter-last-label-name counter)
      :threshold  +MAXIMUM-NUMBER-OF-CONSECUTIVE-JUMPS+))
  (the Jump-Counter counter))

;;; -------------------------------------------------------

(defun count-jump (counter label-name)
  "Apprizes the jump COUNTER about a navigation to the label designated
   by the LABEL-NAME and returns no value."
  (declare (type Jump-Counter  counter))
  (declare (type standard-char label-name))
  (if (and (jump-counter-last-label-name counter)
           (char= label-name (jump-counter-last-label-name counter)))
    (incf
      (jump-counter-number-of-jumps
        (check-if-current-label-can-be-visited counter)))
    (psetf (jump-counter-last-label-name counter) label-name
           (jump-counter-number-of-jumps counter) 1))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer stack operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum +MAXIMUM-INTEGER-STACK-SIZE+))

;;; -------------------------------------------------------

(defparameter +MAXIMUM-INTEGER-STACK-SIZE+ 30000
  "Specifies the maximum number of elements admissible to the castaldy
   of an integer stack.")

;;; -------------------------------------------------------

(defun prepare-empty-integer-stack ()
  "Creates and returns an initially empty stack of 32-bit signed integer
   numbers."
  (the integer-stack
    (make-array 0
      :element-type    '32-bit-integer
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))

;;; -------------------------------------------------------

(defun integer-stack-size (stack)
  "Returns the tally of elements partaking of the integer STACK."
  (declare (type integer-stack stack))
  (the fixnum
    (fill-pointer stack)))

;;; -------------------------------------------------------

(defun integer-stack-is-empty-p (stack)
  "Determines whether the integer STACK is exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type integer-stack stack))
  (the boolean
    (get-boolean-value-of
      (zerop
        (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun integer-stack-is-full-p (stack)
  "Determines whether the integer STACK's capacity is exhausted,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type integer-stack stack))
  (the boolean
    (get-boolean-value-of
      (>= (integer-stack-size stack)
          +MAXIMUM-INTEGER-STACK-SIZE+))))

;;; -------------------------------------------------------

(defun push-onto-integer-stack (stack new-element)
  "Pushes the NEW-ELEMENT onto the stack and returns no value.
   ---
   If the STACK has reached its maximum admissible capacity ere this
   operation's invocation, an error of the type
   ``Full-Integer-Stack-Error'' will be signaled."
  (declare (type integer-stack  stack))
  (declare (type 32-bit-integer new-element))
  (if (integer-stack-is-full-p stack)
    (error 'Full-Integer-Stack-Error :stack stack)
    (vector-push-extend new-element stack))
  (values))

;;; -------------------------------------------------------

(defun query-top-integer-stack-element (stack)
  "Returns without removing the integer STACK's top element, expecting
   this salvatory to contain at least one item."
  (declare (type integer-stack stack))
  (the 32-bit-integer
    (aref stack
      (1-
        (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun peek-into-integer-stack (stack)
  "Returns without removing the integer STACK's top element.
   ---
   If the stack is empty ere this operation's invocation, an error of
   the type ``Empty-Integer-Stack-Error'' is signaled."
  (declare (type integer-stack stack))
  (the 32-bit-integer
    (if (integer-stack-is-empty-p stack)
      (error 'Empty-Integer-Stack-Error
        :stack               stack
        :attempted-operation "peek into")
      (query-top-integer-stack-element stack))))

;;; -------------------------------------------------------

(defun pop-from-integer-stack (stack)
  "Removes and returns the integer STACK's top element.
   ---
   If the stack is empty ere this operation's invocation, an error of
   the type ``Empty-Integer-Stack-Error'' is signaled."
  (declare (type integer-stack stack))
  (the 32-bit-integer
    (if (integer-stack-is-empty-p stack)
      (error 'Empty-Integer-Stack-Error
        :stack               stack
        :attempted-operation "pop from")
      (prog1
        (query-top-integer-stack-element stack)
        (decf (fill-pointer stack))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of (tuple-of symbol symbol))
               +AGATHA-SLOT-ATTRIBUTES+))

;;; -------------------------------------------------------

(defparameter +AGATHA-SLOT-ATTRIBUTES+
  '((program       dotsf-program)
    (skip-table    skip-table)
    (labels        label-table)
    (jump-counter  Jump-Counter)
    (ip            fixnum)
    (memory        integer-stack))
  "The ``+AGATHA-SLOT-ATTRIBUTES+'' global constant enumerates the
   slot names and values comprising the ``Agatha'' class, thilk are
   accommodated in a paravaunt exercise of purpose for local symbol
   macro code generations.")

;;; -------------------------------------------------------

(defclass Agatha ()
  ((program
    :initarg       :program
    :initform      (error "Missing DotSF program.")
    :type          dotsf-program
    :documentation "The DotSF commands to evaluate.")
   (skip-table
    :type          skip-table
    :documentation "Maps the skip points (\"[\" and \"]\") by their
                    zero-based indices' mediation.")
   (labels
    :type          label-table
    :documentation "Associates the label definitions with their
                    zero-based indices into the PROGRAM.")
   (jump-counter
    :initform      (make-jump-counter)
    :type          Jump-Counter
    :documentation "Memorizes the hitherto actuated sojourns to the
                    same label name in consecution.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current instruction pointer (IP) position as a
                    zero-based index into the PROGRAM.")
   (memory
    :initform      (prepare-empty-integer-stack)
    :type          integer-stack
    :documentation "The program memory as a stack of at most 30,000
                    32-bit signed integer numbers."))
  (:documentation
    "The ``Agatha'' class attends to the provision an interpreter, thilk
     is the onus' recipient that concerns the accompassing of efficacy
     to a parsed DotSF program."))

;;; -------------------------------------------------------

(defun assemble-symbol-name (slot-prefix slot-name)
  "Creates and returns a fresh interned symbol whose name concatenates
   the SLOT-PREFIX and SLOT-NAME in this exact order, with no
   adscititious merist's involvement betwixt the moiety's twissel."
  (declare (type symbol slot-prefix))
  (declare (type symbol slot-name))
  (the symbol
    (intern
      (format NIL "~:@(~a~a~)" slot-prefix slot-name))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-bindings (agatha-variable slot-prefix)
  "Generates the symbol macro bindings for interpreter yclept by the
   AGATHA-VARIABLE in conjunction with the desiderated SLOT-PREFIX and
   returns the result as a list."
  (declare (type symbol agatha-variable))
  (declare (type symbol slot-prefix))
  (the (list-of T)
    (loop
      for (slot-name slot-type)
        of-type (symbol symbol)
        in      +AGATHA-SLOT-ATTRIBUTES+
      collect
        `(,(assemble-symbol-name slot-prefix slot-name)
             (the ,slot-type
               (slot-value ,agatha-variable ',slot-name))))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-declarations (slot-prefix)
  "Generates the type and ``ignorable'' declaration for the Agatha
   interpreter slots, derived from the SLOT-PREFIX, and returns the
   result as a list."
  (declare (type symbol slot-prefix))
  (the (list-of T)
    (loop
      for (slot-name slot-type)
        of-type (symbol symbol)
        in      +AGATHA-SLOT-ATTRIBUTES+
      for symbol-macro-name
        of-type symbol
        =       (assemble-symbol-name slot-prefix slot-name)
      append
        `((declare (type ,slot-type ,symbol-macro-name))
          (declare (ignorable       ,symbol-macro-name))))))

;;; -------------------------------------------------------

(defmacro with-agatha ((agatha &optional (slot-prefix 'agatha-))
                       &body body)
  "Evaluates the interpreter AGATHA, binds its slots to local symbol
   macros, their raw agnominations' prefixion accommodated by the
   SLOT-PREFIX, thilk defaults to ``agatha-'', evaluates the BODY forms,
   and returns the desinent form's results."
  (let ((the-real-agatha (gensym)))
    (declare (type symbol the-real-agatha))
    `(let ((,the-real-agatha ,agatha))
       (declare (type Agatha ,the-real-agatha))
       (declare (ignorable   ,the-real-agatha))
       (symbol-macrolet
         ,(build-symbol-macrolet-bindings the-real-agatha slot-prefix)
         ,@(build-symbol-macrolet-declarations slot-prefix)
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((agatha Agatha) &key)
  "Constructs the skip and label table for AGATHA's internally managed
   DotSF program and returns no value."
  (declare (type Agatha agatha))
  (with-agatha (agatha her-)
    (psetf
      her-skip-table (build-skip-table-for  her-program)
      her-labels     (build-label-table-for her-program)))
  (values))

;;; -------------------------------------------------------

(defun call-for-agatha (program)
  "Creates and returns a fresh ``Agatha'' interpreter instances, its
   dedication that to evaluate the DotSF PROGRAM."
  (declare (type dotsf-program program))
  (the Agatha
    (make-instance 'Agatha :program program)))

;;; -------------------------------------------------------

(defun agatha-has-handled-all-orders-p (agatha)
  "Determines whether the DotSF program consigned to the castaldy of the
   interpreter AGATHA has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Agatha agatha))
  (the boolean
    (with-agatha (agatha)
      (get-boolean-value-of
        (>= agatha-ip
            (length agatha-program))))))

;;; -------------------------------------------------------

(defun inquire-agatha-about-her-current-order (agatha)
  "Returns the currently selected instruction in the program consigned
   to the interpreter AGATHA's castaldy."
  (declare (type Agatha agatha))
  (the Instruction
    (with-agatha (agatha)
      (aref agatha-program agatha-ip))))

;;; -------------------------------------------------------

(defgeneric process-instruction (instruction agatha)
  (:documentation
    "Processes the INSTRUCTION in the interpreter AGATHA's context and
     returns no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    (instruction-class (instruction-variable agatha-variable)
     &body body)
  "Defines an implementation of the generic function
   ``process-instruction'', specializing its incipient formal parameter,
   agnominated via the INSTRUCTION-VARIABLE, on the INSTRUCTION-CLASS,
   its second, yclept by the AGATHA-VARIABLE, on the ``Agatha'' class,
   evaluates the BODY forms, and returns no value."
  `(defmethod process-instruction
       ((,instruction-variable ,instruction-class)
        (,agatha-Variable      Agatha))
     (declare (type ,instruction-class ,instruction-variable))
     (declare (ignorable               ,instruction-variable))
     (declare (type Agatha             ,agatha-variable))
     (declare (ignorable               ,agatha-variable))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-instruction-processor NOP-Instruction (instruction agatha))

;;; -------------------------------------------------------

(define-instruction-processor Push-0-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 0)))

;;; -------------------------------------------------------

(define-instruction-processor Push-1-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 1)))

;;; -------------------------------------------------------

(define-instruction-processor Push-2-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 2)))

;;; -------------------------------------------------------

(define-instruction-processor Push-3-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 3)))

;;; -------------------------------------------------------

(define-instruction-processor Push-4-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 4)))

;;; -------------------------------------------------------

(define-instruction-processor Push-5-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 5)))

;;; -------------------------------------------------------

(define-instruction-processor Push-6-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 6)))

;;; -------------------------------------------------------

(define-instruction-processor Push-7-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 7)))

;;; -------------------------------------------------------

(define-instruction-processor Push-8-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 8)))

;;; -------------------------------------------------------

(define-instruction-processor Push-9-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory 9)))

;;; -------------------------------------------------------

(define-instruction-processor Add-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (+ (pop-from-integer-stack agatha-memory)
         (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Subtract-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (- (pop-from-integer-stack agatha-memory)
         (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Multiply-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (* (pop-from-integer-stack agatha-memory)
         (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Divide-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (round
        (pop-from-integer-stack agatha-memory)
        (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Remainder-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (rem
        (pop-from-integer-stack agatha-memory)
        (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Duplicate-Instruction (instruction agatha)
  (with-agatha (agatha)
    (push-onto-integer-stack agatha-memory
      (peek-into-integer-stack agatha-memory))))

;;; -------------------------------------------------------

(define-instruction-processor Input-Integer-Instruction (instruction
                                                         agatha)
  (with-agatha (agatha)
    (format        *query-io* "~&Please enter an integer number: ")
    (finish-output *query-io*)
    (push-onto-integer-stack agatha-memory
      (parse-integer
        (read-line *query-io*)))
    (clear-input *query-io*)))

;;; -------------------------------------------------------

(define-instruction-processor Input-Character-Instruction (instruction
                                                           agatha)
  (with-agatha (agatha)
    (format        *query-io* "~&Please enter a character: ")
    (finish-output *query-io*)
    (push-onto-integer-stack agatha-memory
      (char-code
        (read-char *query-io* NIL
          (code-char 0))))
    (clear-input *query-io*)))

;;; -------------------------------------------------------

(define-instruction-processor Output-Integer-Instruction (instruction
                                                          agatha)
  (with-agatha (agatha)
    (format *query-io* "~&~d~%"
      (pop-from-integer-stack agatha-memory))))

;;; -------------------------------------------------------

(define-instruction-processor Output-Character-Instruction (instruction
                                                            agatha)
  (with-agatha (agatha)
    (format *query-io* "~c"
      (code-char
        (pop-from-integer-stack agatha-memory)))))

;;; -------------------------------------------------------

(define-instruction-processor Skip-Start-Instruction (instruction
                                                      agatha)
  (with-agatha (agatha)
    (when (zerop (pop-from-integer-stack agatha-memory))
      (setf agatha-ip
        (locate-skip-end-point agatha-skip-table agatha-ip)))))

;;; -------------------------------------------------------

(define-instruction-processor Skip-End-Instruction (instruction agatha))

;;; -------------------------------------------------------

(define-instruction-processor Define-Label-Instruction (instruction
                                                        agatha))

;;; -------------------------------------------------------

(define-instruction-processor Visit-Label-Instruction (instruction agatha)
  (with-agatha (agatha)
    (count-jump agatha-jump-counter
      (visit-label-instruction-name instruction))
    (setf agatha-ip
      (locate-label agatha-labels
        (visit-label-instruction-name instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor Tip-Instruction (instruction agatha))

;;; -------------------------------------------------------

(defun let-agatha-handle-the-current-order (agatha)
  "Executes the interpreter AGATHA's currently selected instruction and
   returns no value."
  (declare (type Agatha agatha))
  (process-instruction
    (inquire-agatha-about-her-current-order agatha)
    agatha)
  (values))

;;; -------------------------------------------------------

(defun let-agatha-move-to-the-next-order (agatha)
  "Advances the interpreter AGATHA's instruction pointer (IP) to the
   next instruction in its underlying program and returns no value."
  (declare (type Agatha agatha))
  (with-agatha (agatha agatha\'s-)
    (incf agatha\'s-ip))
  (values))

;;; -------------------------------------------------------

(defun let-agatha-do-her-job (agatha)
  "Interprets the DotSF program consigned to the interpreter AGATHA's
   castaldy and returns no value."
  (declare (type Agatha agatha))
  (loop until (agatha-has-handled-all-orders-p agatha) do
    (let-agatha-handle-the-current-order agatha)
    (let-agatha-move-to-the-next-order   agatha))
  (values))

;;; -------------------------------------------------------

(defun interpret-dotsf (code)
  "Interprets the piece of DotSF source CODE and returns no value."
  (declare (type string code))
  (let-agatha-do-her-job
    (call-for-agatha
      (parse-dotsf-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "HI" to the standard output conduit.
(interpret-dotsf "98*_;1+;")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-dotsf ".A__[:b$Ba]:")

;;; -------------------------------------------------------

;; Countdown from inclusive 99 to inclusive 1, printing the result to
;; the standard output conduit.
(interpret-dotsf "0A1+_92+9*1+-_[:a]$")

;;; -------------------------------------------------------

;; Infinitely repeating numeric cat program.
(interpret-dotsf "A.:bBa")

;;; -------------------------------------------------------

;; Repeating character-based cat program which terminates on a
;; "null character" input.
(interpret-dotsf "A,;bBa")

;;; -------------------------------------------------------

;; Infinitely repeating character-based cat program.
(interpret-dotsf "A,_[;bBa]")
