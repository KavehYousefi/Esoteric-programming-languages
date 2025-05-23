;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brainfuck²", invented by the Esolang user
;; "TedDidNothingWrong" and presented on November 18th, 2017,
;; constituting a derivation of Urban Mueller's language "brainfuck",
;; its emblem a reformulation of the entheus' instruction identifiers
;; to allude to other trivial brainfuck substitutions, including this
;; specimen itself.
;; 
;; 
;; Concept
;; =======
;; Edified upon the firmament of the brainfuck programming language,
;; Brainfuck²'s contribution wones in its reformulation of the entheus'
;; instruction identifiers in the mold of agnominations desumed from
;; other brainfuck derivatives.
;; 
;; == AN APERCU CONCERNING THE ALLUSIONS ==
;; The octuple operative contingency's molding into an equinumerant
;; account involving brainfuck derivatives shall, in their respective
;; subjects' elucidation, constitute the following tabulation's
;; cynosure:
;; 
;;   ------------------------------------------------------------------  
;;   Referenced | Haecceity
;;   language   | 
;;   -----------+------------------------------------------------------
;;   Alphuck    | Engages in a cambistry of brainfuck's symbolic
;;              | identifier design for alphabetic succedanea.
;;   ..................................................................
;;   Brainfuck² | Refers to this language itself, which alludes to the
;;              | remaining brainfuck variants.
;;   ..................................................................
;;   Fuckfuck   | Substitutes brainfuck's one-symbol identifiers by a
;;              | profane diction which homologates censorships and
;;              | repetitions.
;;   ..................................................................
;;   Ook!       | Represents operations by an Orangutan's belluine
;;              | vociferations.
;;   ..................................................................
;;   POGAACK    | Assigns to the instructions sounds akin siccan
;;              | emitted by chickens.
;;   ..................................................................
;;   Unibrain   | Encodes the instructions in the tally of a
;;              | substring's repetitions in a word.
;;   ..................................................................
;;   Wordfuck   | Encodes the instructions in the lengths of its
;;              | space-separated words, where character tallies must
;;              | obey the closed interval [2, 9] as a prerequisite to
;;              | their effective participation.
;;   ..................................................................
;;   zzz        | Employs combinations of the character "z" with the
;;              | symbols "+" and "-".
;;   ------------------------------------------------------------------
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of Brainfuck²'s recipiency does not elude brainfuck's
;; architecture, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; == A GREMIAL PROPAGATION OF THE PROVENANCE ==
;; It should be noted --- ensuing from the language author's alacrity
;; in siccan contubernal gnarity's distribution --- that Brainfuck²'s
;; conceptual accouchement conflated with an avail of the dornicker.
;; 
;; The compendious ecphrasis vouchsafed to limn this thaumaturgy shall
;; not decur deprived of notice; and we are always entalented with
;; gratitude upon these species of information's recipiency.
;; 
;; 
;; Instructions
;; ============
;; A paregal to its entheus, Brainfuck² wists neither of a curtailment
;; nor of a supererogation in the operative facilities that edify its
;; cleronomy's dation, merely assigning a different guise in the donat's
;; choice.
;; 
;; == OVERVIEW ==
;; The requisite acquaintance with the language's instruction set shall
;; constitute the following apercu's dever:
;; 
;;   ------------------------------------------------------------------
;;   Commands   | Effect
;;   -----------+------------------------------------------------------
;;   Ook!       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   Alphuck    | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   Fuckfuck   | Increments the current cell value by one (1). If the
;;              | new state transcends the upper extremum of 255, the
;;              | value relapses to the lower bourne of zero (0).
;;   ..................................................................
;;   POGAACK    | Decrements the current cell value by one (1). If the
;;              | new state transcends the lower extremum of zero (0),
;;              | the value wraps around to the upper bourne of 255.
;;   ..................................................................
;;   Unibrain   | Prints the character whose ASCII code corresponds to
;;              | the current cell value to the standard output.
;;   ..................................................................
;;   Wordfuck   | Queries the standard input for a character and stores
;;              | its ASCII code in the current cell.
;;   ..................................................................
;;   Brainfuck² | If the current cell contains zero (0), moves the
;;              | instruction pointer (IP) forward to the position
;;              | immediately succeeding the matching "ZZZ" token;
;;              | otherwise proceeds as usual.
;;   ..................................................................
;;   ZZZ        | If the current cell value does not contain zero (0),
;;              | moves the instruction pointer (IP) back to the
;;              | position immediately succeeding the matching
;;              | "Brainfuck²" token; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == Brainfuck² AND BRAINFUCK ==
;; The congruence of Brainfuck² and its stock-father in all aspects,
;; exempting the donet's designment, concludes in the gendrure of a
;; perfect equiparation's establishment, thilk shall be enjoy in the
;; following its ostention:
;; 
;;   -----------------------
;;   Brainfuck² | brainfuck
;;   -----------+-----------
;;   Ook!       | >
;;   .......................
;;   Alphuck    | <
;;   .......................
;;   Fuckfuck   | +
;;   .......................
;;   POGAACK    | -
;;   .......................
;;   Unibrain   | .
;;   .......................
;;   Wordfuck   | ,
;;   .......................
;;   Brainfuck² | [
;;   .......................
;;   ZZZ        | ]
;;   -----------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, the execution process succeeding
;; from the prevenience of a transformation applied upon the source
;; code string with the telos of its instruction's collation in a more
;; covenable format for the pursued evaluation tier.
;; 
;; == THE INTERPRETER STATE: A SET OF SPECIAL VARIABLES ==
;; A kenspeckle warklume's deployment relates to the interpreter
;; functionality's expression in special variables as a succedaneum for
;; the expected definition inwith the parsing routine's context or the
;; state variables' ensconcement in a class. This choice, entalented
;; with veridicous dubiety concerning its salubrious involvement,
;; installs its vindication in the endeictic as well as didascalic
;; teloi.
;; 
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; It constitutes a peisant element of gnarity to remember that special
;; variables, ligated into a consanguinity with global variables as a
;; general species, and exacerbated by their implicit and contingently
;; arbitrary declarations, merit the wite of encumbering programs with
;; superfluous complexity. For a more detailed treatise on the
;; contingency for detriments incurred by this feature please refer to
;; [stackoverflow2019q56725814].
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-05-11
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024Brainfuck²]
;;   The Esolang contributors, "Brainfuck²", April 29th, 2024
;;   URL: "https://esolangs.org/wiki/Brainfuck%C2%B2"
;;   
;;   [stackoverflow2019q56725814]
;;   The Stack Overflow contributors, "Using Local Special Variables",
;;     2019
;;   URL: "https://stackoverflow.com/questions/56725814/
;;         using-local-special-variables"
;;   Notes:
;;     - Discusses the disadvantages of special variables, which
;;       comprehend:
;;        o Lack of referential transparency, ...
;;          ... which renders it more difficult to reason functionally
;;          about one's code, meaning that functions may produce
;;          different results with syntactically equivalent calls.
;;        o Introduction of bugs, ...
;;          ... as lexical variable at other locations in the code,
;;          e.g. in a system function, will be overwritten.
;;        o Confusion ...
;;          .. for readers unacquainted with special (dynamic) binding
;;        o Dubious necessity, ...
;;          ... as lexical binding or even anaphoric macros may be
;;          utilized instead.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT as a \"generalized boolean\" and produces a
   veridical Boolean equivalent thereof, returning for a non-``NIL''
   input a ``boolean'' value of ``T''; otherwise, for a ``NIL'' OBJECT,
   answers with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-specifier-is-comprehensive-p (specifier)
  "Determines whether the type SPECIFIER homolgates any type's
   communication by its equiparation with the generic sentinel ``*'',
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type T specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp specifier)
           (eq      specifier '*)))))

;;; -------------------------------------------------------

(defun object-is-of-type-p (probed-object expected-type)
  "Determines whether the PROBED-OBJECT complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type T probed-object))
  (declare (type T expected-type))
  (the boolean
    (get-boolean-value-of
      (or (type-specifier-is-comprehensive-p expected-type)
          (typep probed-object expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list as a composition of zero or more
   elements, each such complying with the ELEMENT-TYPE, for which is
   specified the generic sentinel ``*'' as a default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (type-specifier-is-comprehensive-p element-type)
              (loop
                for current-element
                  of-type T
                  in      (the list candidate)
                always
                  (object-is-of-type-p
                    current-element
                    element-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the
   generic sentinel ``*'' as a default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (or
              (and
                (type-specifier-is-comprehensive-p key-type)
                (type-specifier-is-comprehensive-p value-type))
              (loop
                for current-key
                  of-type T
                  being the hash-keys in (the hash-table candidate)
                using
                  (hash-value current-value)
                always
                  (and
                    (object-is-of-type-p current-key   key-type)
                    (object-is-of-type-p current-value value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   Brainfuck² operations."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Brainfuck² program as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral mapping betwixt the
   forward and back jump points in a Brainfuck² program by adminiculum
   of their zero-based positions into the parsed instruction sequence,
   manifesting in a one-dimensional simple array of fixnum entries, each
   such either specifies for the instruction whose index conflates with
   the entry the position inside the same sequence of the matching
   destination jump point, or signifies via the sentinel -1 the presence
   of a non-jump instruction.
   ---
   With augmented formality's diction, for the entry jumps[i], at the
   index i, where 0 <= i < numberOfInstructions, it holds:
   
     jumps[i] represents the destination    }
              jumps[jumps[i]] of a forward  }-  , if jumps[i] >= 0
              or back jump instruction      }
     
     jumps[i] does not designate a forward  }-  , if jumps[i] <  0.
              or back jump instruction,     }"
  '(simple-array fixnum (*)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, thus constituting an integeral number occupying the
   closed interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   enhalsing in this diorism, without the claim of exhaustion, functions
   as ``format'' and ``write-char''."
  '(or null
       (eql T)
       stream
       string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   inwith whose diorism are enlisted the space, horizontal tab, and
   newline entities, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Linefeed #\Newline #\Space #\Tab)
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-if-satisfies (source start predicate)
  "Proceeding from the START position into the SOURCE, returns the index
   of the first character satisfying the PREDICATE, or, upon the
   search's failure, responds with the SOURCE's length."
  (declare (type string                   source))
  (declare (type fixnum                   start))
  (declare (type (function (character) *) predicate))
  (the fixnum
    (or (position-if predicate source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the
   nearest following word and returns its first character's index; or,
   upon its disrespondency, answers with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (locate-if-satisfies source start
      (complement #'whitespace-character-p))))

;;; -------------------------------------------------------

(defun demarcate-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the
   nearest following word and returns two values:
     (1) If a word could be detected, the index of its first character
         in the SOURCE, otherwise the SOURCE's length.
     (2) If a word could be detected, the index immediately succeeding
         its desinent character in the SOURCE, otherwise the SOURCE's
         length."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((word-start-position (locate-next-word source start)))
    (declare (type fixnum word-start-position))
    (the (values fixnum fixnum)
      (values
        word-start-position
        (locate-if-satisfies
          source
          word-start-position
          #'whitespace-character-p)))))

;;; -------------------------------------------------------

(defun substring-found-at-p (source start end expected-content)
  "Determines whether the characters in the SOURCE, commencing at the
   inclusive START position and ceasing before the exclusive END index,
   equal the EXPECTED-content, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum end))
  (declare (type string expected-content))
  (the boolean
    (get-boolean-value-of
      (string= source expected-content :start1 start :end1 end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction parser.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-instruction (source start end)
  "Attempts to parse the word demarcated by the inclusive START and the
   exclusive END position into the SOURCE, returning on success a
   connable ``instruction'' representation thereof, otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type fixnum end))
  (flet ((probed-word-equals-p (expected-word)
          "Determines whether the word commencing at the inclusive START
           position into the SOURCE and ceasing before the exclusive END
           index equals the EXPECTED-WORD, returning on confirmation a
           ``boolean'' value of ``T'', otherwise ``NIL''."
          (declare (type string expected-word))
          (the boolean
            (substring-found-at-p source start end expected-word))))
    (the (or null instruction)
      (cond
        ((probed-word-equals-p "Ook!")       :move-right)
        ((probed-word-equals-p "Alphuck")    :move-left)
        ((probed-word-equals-p "Fuckfuck")   :increment)
        ((probed-word-equals-p "POGAACK")    :decrement)
        ((probed-word-equals-p "Unibrain")   :output)
        ((probed-word-equals-p "Wordfuck")   :input)
        ((probed-word-equals-p "Brainfuck²") :jump-forward)
        ((probed-word-equals-p "ZZZ")        :jump-back)
        (T                                   NIL)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh ``program'' from the INSTRUCTIONS list."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *source-code*))
(declaim (type fixnum        *start-position-of-current-word*))
(declaim (type fixnum        *end-position-of-current-word*))
(declaim (type boolean       *more-words-follow-p*))

;;; -------------------------------------------------------

(defparameter *source-code* ""
  "The piece of Brainfuck² source code to evaluate.")

(defparameter *start-position-of-current-word* 0
  "The inclusive zero-based index into the *SOURCE-CODE* whence the
   currently detected word commences.")

(defparameter *end-position-of-current-word* 0
  "The exclusive zero-based index into the *SOURCE-CODE* where the
   currently detected word ceases.")

(define-symbol-macro *more-words-follow-p*
  (the boolean
    (get-boolean-value-of
      (< *start-position-of-current-word*
         (length *source-code*)))))

;;; -------------------------------------------------------

(defun set-source-code (new-source-code)
  "Sets the *SOURCE-CODE* to the NEW-SOURCE-CODE, resets all
   appertaining state variables to their inchoate values, and returns no
   value."
  (declare (type string new-source-code))
  (psetf
    *source-code*
      (coerce new-source-code 'simple-string)
    *start-position-of-current-word* 0
    *end-position-of-current-word*   0)
  (values))

;;; -------------------------------------------------------

(defun request-next-word ()
  "Commencing from the inclusive *START-POSITION-OF-CURRENT-WORD* into
   the *SOURCE*, locates the nearest following word, updates the
   lexer's with respect to its bournes, and returns no value."
  (multiple-value-setq
      (*start-position-of-current-word*
       *end-position-of-current-word*)
    (demarcate-next-word *source-code* *end-position-of-current-word*))
  (values))

;;; -------------------------------------------------------

(defun parse-current-word ()
  "Attempts to parse the word demarcated by the jumelle of the inclusive
   *START-POSITION-OF-CURRENT-WORD* and the exclusive
   *END-POSITION-OF-CURRENT-WORD*, returning on confirmation a covenable
   instruction representation; otherwise responds with the ``NIL''
   value."
  (the (or null instruction)
    (parse-instruction
      *source-code*
      *start-position-of-current-word*
      *end-position-of-current-word*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program parser.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-program (source)
  "Parses the piece of Brainfuck² SOURCE code and returns a ``program''
   representation amplecting its ensconced instructions in their
   encountered order."
  (declare (type string source))
  (set-source-code source)
  (the program
    (make-program
      (loop
        initially
          (request-next-word)
        
        while *more-words-follow-p*
        
        for current-instruction
          of-type (or null instruction)
          =       (parse-current-word)
        
        when current-instruction
          collect current-instruction
        end
        
        do (request-next-word)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-jump-table-of-size (number-of-entries)
  "Creates and returns a fresh ``jump-table'' accommodating the spatial
   dispansion for the NUMBER-OF-ENTRIES, each such compartment at its
   inchoation being set to the vacancy sentinel -1."
  (declare (type fixnum number-of-entries))
  (the jump-table
    (make-array number-of-entries
      :element-type    'fixnum
      :initial-element -1
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun prepare-jump-table-for-program (program)
  "Creates and returns a fresh ``jump-table'' whose capacity endows
   sufficient space for the Brainfuck² PROGRAM jump point's mappings."
  (declare (type program program))
  (the jump-table
    (prepare-jump-table-of-size
      (length program))))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the START-POINT and END-POINT in a bilaterally fashion in
   the JUMP-TABLE and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (aref jump-table start-point) end-point
    (aref jump-table end-point)   start-point)
  (values))

;;; -------------------------------------------------------

(defun locate-jump-destination (jump-table point-of-departure)
  "Returns for the POINT-OF-DEPARTURE the allied destination point as
   specified in the JUJMP-TABLE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (let ((destination-point (aref jump-table point-of-departure)))
    (declare (type fixnum destination-point))
    (if (minusp destination-point)
      (error "No destination point associated with the jump point ~d."
        point-of-departure)
      destination-point)))

;;; -------------------------------------------------------

(defun supputate-jump-table-for (program)
  "Creates and returns for the Brainfuck² PROGRAM a fresh jump table
   which maintains the vincula betwixt its forward and back jump points
   in a bidirectional fashion by adminiculum of their zero-based indices
   into the supplied instruction sequence."
  (declare (type program program))
  (let ((jump-table          (prepare-jump-table-for-program program))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for current-instruction of-type instruction across program
      and current-position    of-type fixnum      from   0 by 1
      
      if (eq current-instruction :jump-forward) do
        (push current-position forward-jump-points)
      else if (eq current-instruction :jump-back) do
        (if forward-jump-points
          (connect-jump-points jump-table
            (pop forward-jump-points)
            current-position)
          (error "Unmatched back jump point as instruction number ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape cell.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cell ()
  ((values
    :initform      0
    :accessor      cell-value
    :type          octet
    :documentation "The unsigned byte value stored in the cell.")
   (predecessor
    :initform      NIL
    :accessor      cell-predecessor
    :type          (or null Cell)
    :documentation "The cell's potential previous neighbor.")
   (successor
    :initform      NIL
    :accessor      cell-successor
    :type          (or null Cell)
    :documentation "The cell's potential following neighbor."))
  (:documentation
    "The ``Cell'' class serves in the encapsulation of a tape cell's
     proprium, which comprehends, besides the unsigned byte datum as its
     paravaunt item of significant, two references, one to its potential
     predecessor, another to an optional successor cell.
     ---
     These components of the diorism limn a doubly linked node's
     simulacrum, intended for the deployment in a ``Tape'' class, thilk
     itself aspires a doubly linked list's provision in octet-valued
     cells."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Tape Cell Cell) Cell) insert-cell-betwixt))

;;; -------------------------------------------------------

(defclass Tape ()
  ((header
    :accessor      tape-header
    :type          Cell
    :documentation "Being a sentinel node, rather than such utilized for
                    data storage, demarcates the tape's sinistral march,
                    preceding any other cell.")
   (trailer
    :accessor      tape-trailer
    :type          Cell
    :documentation "Being a sentinel node, rather than such utilized for
                    data storage, demarcates the tape's dextral march,
                    succeeding any other cell.")
   (pointer
    :accessor      tape-pointer
    :type          Cell
    :documentation "The current cell pointer position."))
  (:documentation
    "The ``Tape'' class applies itself to the furnishment of a
     bidirectionally infinite dispansion of unsigned byte-valued cells,
     realized in terms of a doubly linked list, and upon which operates
     a cell pointer as the currently active unit's signification."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tape Tape) &key)
  "Initializes and connects the TAPE's header and trailer sentinels and
   returns no value."
  (declare (type Tape tape))
  (with-slots (header trailer pointer) tape
    (declare (type Cell header))
    (declare (type Cell trailer))
    (declare (type Cell pointer))
    (setf header  (make-instance 'Cell))
    (setf trailer (make-instance 'Cell))
    (psetf
      (cell-successor   header)  trailer
      (cell-predecessor trailer) header)
    (setf pointer
      (insert-cell-betwixt tape header trailer)))
  (values))

;;; -------------------------------------------------------

(defun insert-cell-betwixt (tape predecessor successor)
  "Inserts a freshly created cell into the TAPE betwixt the PREDECESSOR
   and the SUCCESSOR and returns the thus produced cell instance."
  (declare (type Tape tape))
  (declare (ignore    tape))
  (declare (type Cell predecessor))
  (declare (type Cell successor))
  (let ((new-cell (make-instance 'Cell)))
    (declare (type Cell new-cell))
    (setf (cell-predecessor new-cell)    predecessor)
    (setf (cell-successor   predecessor) new-cell)
    (setf (cell-successor   new-cell)    successor)
    (setf (cell-predecessor successor)   new-cell)
    (the Cell new-cell)))

;;; -------------------------------------------------------

(defun insert-cell-at-leftmost-position (tape)
  "Inserts a new zero-valued cell at the leftmost position of the TAPE
   and returns the thus generated cell instance."
  (declare (type Tape tape))
  (the Cell
    (with-slots (header) tape
      (declare (type Cell header))
      (insert-cell-betwixt tape header
        (cell-successor header)))))

;;; -------------------------------------------------------

(defun insert-cell-at-rightmost-position (tape)
  "Inserts a new zero-valued cell at the rightmost position of the TAPE
   and returns the thus generated cell instance."
  (declare (type Tape tape))
  (the Cell
    (with-slots (trailer) tape
      (declare (type Cell trailer))
      (insert-cell-betwixt tape
        (cell-predecessor trailer)
        trailer))))

;;; -------------------------------------------------------

(defun cell-pointer-occupies-leftmost-cell-p (tape)
  "Determines whether the TAPE's cell pointer resides on the leftmost
   of the extant cells, that is, immediately dextral to the TAPE's
   header sentinel, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (with-slots (pointer header) tape
      (declare (type Cell pointer))
      (declare (type Cell header))
      (get-boolean-value-of
        (eq (cell-predecessor pointer)
            header)))))

;;; -------------------------------------------------------

(defun cell-pointer-occupies-rightmost-cell-p (tape)
  "Determines whether the TAPE's cell pointer resides on the rightmost
   of the extant cells, that is, immediately sinistral to the TAPE's
   trailer sentinel, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (with-slots (pointer trailer) tape
      (declare (type Cell pointer))
      (declare (type Cell trailer))
      (get-boolean-value-of
        (eq (cell-successor pointer)
            trailer)))))

;;; -------------------------------------------------------

(defun ensure-cell-left-of-pointer (tape)
  "Determines whether the TAPE's cell pointer resides on the leftmost
   cell, on confirmation inserting a fresh cell on its sinistral
   laterality, in any case returning no value."
  (declare (type Tape tape))
  (when (cell-pointer-occupies-leftmost-cell-p tape)
    (insert-cell-at-leftmost-position tape))
  (values))

;;; -------------------------------------------------------

(defun ensure-cell-right-of-pointer (tape)
  "Determines whether the TAPE's cell pointer resides on the rightmot
   cell, on confirmation inserting a fresh cell on its dextral
   laterality, in any case returning no value."
  (declare (type Tape tape))
  (when (cell-pointer-occupies-rightmost-cell-p tape)
    (insert-cell-at-rightmost-position tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell poiner one step to the left and returns no
   value."
  (declare (type Tape tape))
  (ensure-cell-left-of-pointer tape)
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf pointer
      (cell-predecessor pointer)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell poiner one step to the right and returns
   no value."
  (declare (type Tape tape))
  (ensure-cell-right-of-pointer tape)
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf pointer
      (cell-successor pointer)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte valued stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (cell-value
      (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceded by a wrapping of the same in order to accommodate the
   admissible byte range of [0, 255], and returns no value."
  (declare (type Tape tape))
  (with-slots (pointer) tape
    (declare (type Cell pointer))
    (setf (cell-value pointer)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p (tape)
  "Determines whether the TAPE's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value tape)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-instruction (instruction)
  (:documentation
    "Evaluates the INSTRUCTION utilizing the interpreter's
     contemporaneous state configuration and returns no value."))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :move-right)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (move-cell-pointer-right tape)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :move-left)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (move-cell-pointer-left tape)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :increment)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (incf (current-cell-value tape))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :decrement)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (decf (current-cell-value tape))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :output)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (format *standard-output* "~c"
    (code-char
      (current-cell-value tape)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :input)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          tape))
  (format        *standard-output* "~&>> ")
  (finish-output *standard-output*)
  (setf (current-cell-value tape)
    (char-code
      (read-char *standard-input* NIL #\Null)))
  (clear-input *standard-input*)
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :jump-forward)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          ip))
  (declare (special          jump-table))
  (declare (special          tape))
  (when (current-cell-contains-zero-p tape)
    (setf ip
      (locate-jump-destination jump-table ip)))
  (values))

;;; -------------------------------------------------------

(defmethod process-instruction ((instruction (eql :jump-back)))
  (declare (type instruction instruction))
  (declare (ignore           instruction))
  (declare (special          ip))
  (declare (special          jump-table))
  (declare (special          tape))
  (unless (current-cell-contains-zero-p tape)
    (setf ip
      (locate-jump-destination jump-table ip)))
  (values))

;;; -------------------------------------------------------

(defun execute-program (program)
  "Executes the Brainfuck² PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (supputate-jump-table-for program))
        (tape       (make-instance 'Tape)))
    (declare (type fixnum     ip)
             (special         ip))
    (declare (type jump-table jump-table)
             (special         jump-table))
    (declare (type Tape       tape)
             (special         tape))
    (loop while (< ip (length program)) do
      (process-instruction
        (aref program ip))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Brainfuck² (code)
  "Interprets the piece of Brainfuck² source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (parse-program code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to Brainfuck².    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-Brainfuck²
    (brainfuck-code
     &key (destination T))
  "Generates for the piece of BRAINFUCK-CODE a functionally tantamount
   Brainfuck² program, writes thilk to the DESTINATION, and returns for
   a non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a
   ``NIL'' DESTINATION, responds with a fresh string comprehending the
   output.
   ---
   Please heed that each twissel of consecutive instructions' sepiment
   is realized in a single newline character."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for current-token of-type character across brainfuck-code do
        (format destination "~@?"
          (case current-token
            (#\>       "~&Ook!")
            (#\<       "~&Alphuck")
            (#\+       "~&Fuckfuck")
            (#\-       "~&POGAACK")
            (#\.       "~&Unibrain")
            (#\,       "~&Wordfuck")
            (#\[       "~&Brainfuck²")
            (#\]       "~&ZZZ")
            (otherwise ""))))
      (with-output-to-string (brainfuck²-code)
        (declare (type string-stream brainfuck²-code))
        (translate-brainfuck-to-Brainfuck² brainfuck-code
          :destination brainfuck²-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-Brainfuck²
  "Wordfuck
   Brainfuck²
     Unibrain
     Wordfuck
   ZZZ")

;;; -------------------------------------------------------

;; Print "Hello, World!" to the standard output.
(interpret-Brainfuck²
  "
  Fuckfuck
  Brainfuck²
  POGAACK
  POGAACK
  Ook!
  POGAACK
  Brainfuck²
  Ook!
  Ook!
  Fuckfuck
  Ook!
  POGAACK
  POGAACK
  POGAACK
  POGAACK
  POGAACK
  Alphuck
  Alphuck
  ZZZ
  Alphuck
  POGAACK
  POGAACK
  Alphuck
  POGAACK
  POGAACK
  POGAACK
  ZZZ
  Ook!
  POGAACK
  Unibrain
  Ook!
  Ook!
  Ook!
  Fuckfuck
  Unibrain
  Ook!
  Ook!
  Unibrain
  Unibrain
  Fuckfuck
  Fuckfuck
  Fuckfuck
  Brainfuck²
  Unibrain
  Ook!
  ZZZ
  Alphuck
  Alphuck
  Alphuck
  Alphuck
  Unibrain
  Fuckfuck
  Fuckfuck
  Fuckfuck
  Unibrain
  POGAACK
  POGAACK
  POGAACK
  POGAACK
  POGAACK
  POGAACK
  Unibrain
  Alphuck
  Alphuck
  POGAACK
  Unibrain
  Ook!
  Ook!
  Ook!
  Ook!
  Fuckfuck
  Unibrain
  ")

;;; -------------------------------------------------------

;; Convert "Hello, World!" from brainfuck to Brainfuck² and print the
;; resulting code to the standard output.
(translate-brainfuck-to-Brainfuck²
  "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]
   <<<<.+++.------.<<-.>>>>+.")

;;; -------------------------------------------------------

;; Convert "Hello, World!" from brainfuck to Brainfuck² and execute the
;; resulting program.
(interpret-Brainfuck²
  (translate-brainfuck-to-Brainfuck²
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]
     <<<<.+++.------.<<-.>>>>+."
    :destination NIL))
