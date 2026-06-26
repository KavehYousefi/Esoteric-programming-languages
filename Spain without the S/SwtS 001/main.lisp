;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Spain without the S", also yclept, in a mode of enhaused
;; compendiousness, as "Pain", invented by the Esolang user
;; "Timothytomato" and presented on October 20th, 2024, its diorism's
;; woning the instructions' representation in a forbisen following
;; "Spain with the [...]", the same operate on a wheel of non-negative
;; integer scalar accumulators, capable of circination in a deasil
;; fashion.
;; 
;; 
;; Concept
;; =======
;; The "Spain without the S" programming language constitutes an
;; esoteric specimen, the entelechy of whose proprium harbors its
;; commorancy in the operations' encoding as "Spain without the [...]"
;; phrase variants, their dedication thilk as to operate on a wheel of
;; unsigned integer-valued accumulators, the segments of whom exercise
;; their insertions and navgiations in a deasil procession.
;; 
;; == INSTRUCTIONS ARE "SPAIN WITHOUT THE ..." ==
;; Each instruction identifier's conformation emerges from a variation
;; on the forbisen "Spain without the ", its perclose either a single
;; letter or a complete word.
;; 
;; Everichon among the tokens twissels entertains one or more
;; whitespaces as the merist.
;; 
;; == THE MEMORY: A WHEEL OF UNSIGNED INTEGER-VALUED ACCUMULATORS ==
;; The program memory's designment is edified upon a "wheel" plasmature,
;; a cingular arrangement of unsigned integer-valued accumulators, their
;; their tally never alow an aefauld member, the incipial specimen
;; accoutred during the program's inchoacy, but without any impositions
;; concerning the constitution's maximum admissibility.
;; 
;; == ACCUMULATORS INSERTIONS AND TRAVERSALS OCCUR CLOCKWISE ==
;; The conformation wists of a linkage to the accumulators' deasil
;; laterality, whence insertions and traversals transpire.
;; 
;; == AN ACCUMULATOR: A NON-NEGATIVE SCALAR INTEGER SALVATORY ==
;; An accumulator's capacity is measured by a scalar integer number
;; greater than or equal to zero (0), manumitted, however, from the
;; mickleness' bourne, and commencing at the program start in the
;; minimum values state of zero (0).
;; 
;; == A POINTER SELECTS THE CURRENTLY ACTIVE ACCUMULATOR ==
;; A pointer partakes of the wheel's operation thilk at any instant
;; designates the currently active accumulator, this constituting the
;; sole entity vouchsafed an amenability to perquisitions and
;; alterations. Conflating with the inchoate program state, this cursor
;; refers to the original accumulator, while insertions and traversals
;; along the clockwise direction impart a dynamic haecceity to its
;; toils.
;; 
;; == ERRONEOUS CIRCUMSTANCES TERMINATE THE PROGRAM ==
;; An anomalous circumstance's inroad, including, among others, a
;; lapsus in the admissible segregation of language tokens, or
;; unrecognized commands' trial, instigate the program's immediate
;; termination.
;; 
;; 
;; Instructions
;; ============
;; The "Spain without the S" programming language's instruction set
;; enumerates a septuple cardinality, the circumference of which
;; enhalses such warklumes as the memory's attrectation, input and
;; output commerce, as well as an iterance mechanism.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be patefied in a requisite mete
;; of gnarity's vouchsafement concerning the language's operative
;; avails.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ==================================================================
;;   MEMORY MANAGEMENT
;;   ------------------------------------------------------------------
;;   Spain without the p     | If the current accumulator's value is
;;                           | non-zero, decrements the state by a
;;                           | mountance of one (1); otherwise sets its
;;                           | value to 127.
;;   ..................................................................
;;   Wpain without the a     | Rotates the accumulator wheel one unit
;;                           | in the deasil airt; that is, selects
;;                           | the next accumulator in the clockwise
;;                           | direction.
;;   ..................................................................
;;   Spain without the n     | Creates a new zero-valued accumulator
;;                           | at the current one's deasil laterality;
;;                           | but does not relocate the cursor to the
;;                           | new member.
;;   ==================================================================
;;   INPUT AND OUTPUT INTERCOURSE
;;   ------------------------------------------------------------------
;;   Spain without the Spain | Prints the character whose ASCII code
;;                           | concurs with the current accumulator's
;;                           | numeric value to the standard output
;;                           | conduit.
;;   ..................................................................
;;   Spain without the pain  | Queries the standard input conduit for
;;                           | a character and stores its ASCII code
;;                           | in the current accumulator.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   Spain without the S     | Demarcates a loop's start point,
;;                           | intended for a contingent recourse by
;;                           | the matching "Spain without the i"
;;                           | instruction; which please consult alow.
;;   ..................................................................
;;   Spain without the i     | If the current accumulator contains a
;;                           | non-zero value, moves the instruction
;;                           | pointer (IP) back to the matching
;;                           | "Spain without the S" instruction;
;;                           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; A treatise's plasmature concerning the instruction syntaxis
;; vouchsafed a more pleasing visualization shall be ostended alow:
;; 
;;   Spain without the _
;;                     |
;;                     +--> p     ==> decrement accumulator
;;                     |
;;                     +--> a     ==> select next accumulator
;;                     |
;;                     +--> n     ==> create new accumulator
;;                     |
;;                     |
;;                     +--> Spain ==> print accumulator
;;                     |
;;                     +--> pain  ==> input into accumulator
;;                     |
;;                     |
;;                     +--> S     ==> start loop
;;                     |
;;                     +--> i     ==> repeat loop
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp; the procession from the "Spain with the S" source code
;; string to its epiphenomena's actuation limns a trisulk champarty's
;; designment:
;; 
;;   (1) SOURCE CODE NORMALIZATION:
;;       A reformatting stage is applied to the piece of
;;       "Spain without the S" source code in order to ascertain each
;;       token twain's separation by a single space character (ASCII
;;       code: 32), with a concomitant expungement of preceding and
;;       trailing whitespaces.
;;   
;;   (2) INSTRUCTION EXTRACTION:
;;       Perging from the normalized source code, the entailed
;;       instructions are extracted into a vector. Any
;;       non-recognized token's involvement, as an apostille furnished,
;;       immediately terminates the program.
;;   
;;   (3) INSTRUCTION EXECUTION:
;;       The instruction vector's processing accompasses actual
;;       entelechy to the code's behest.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-06-07
;; 
;; Sources:
;;   [esolang:2025:Spain without the S]
;;   The Esolang contributors, "Spain without the S",
;;     September 20th, 2025
;;   URL: "https://esolangs.org/wiki/Spain_without_the_S"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is desumed in an ipsissima
   verba fashion from the TYPE-NAME, and whose formal parameters
   accompts for the LAMBDA-LIST's dation, probing the docimasy's
   subject, nevened by adminiculum of the CANDIDATE-NAME, through the
   BODY forms, with a \"generalized boolean\" value of \"true\" as the
   desinent form's primary return value being administered the construe
   as the candidate's compatibility with the underlying type notion,
   while a \"false\" responds shall signify the match's failure.
   ---
   If the first BODY form assumes a string object, the same is imputed
   to contribute a documentation string to the type definition, as a
   consectary being reappropriated for this exact purpose."
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
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*)
                                             (size         '*))
  "The ``list-of'' type defines a linked list of elements, each member
   of which subsumes into the ELEMENT-TYPE, the sequence's cardinality
   either imposed by the SIZE, or, upon its omission, tolerating any
   accompt."
  (and
    (listp candidate)
    (or (eq size '*)
        (= size (length (the list candidate))))
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-a-predicated-type hash-table-of (candidate
                                         &optional (key-type   '*)
                                                   (value-type '*))
  "The ``hash-table-of'' type defines a hash table the conformation of
   which is edified upon a collection of zero or more entries, each such
   member's key complying with the KEY-TYPE, while the associated value
   subsumes into the VALUE-TYPE, for both holds the generic sentinel
   ``*'' as the default."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and
          (typep current-key   key-type)
          (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type   '*)
                                        (value-type '*)
                                        (size       '*))
  "The ``association-list--of'' type defines an assocation list, or
   alist, the keys of which adhere to the KEY-TYPE, while the allied
   value's ostend a lealty to the VALUE-TYPE, the list's cardinality,
   if supplied, being imposed by the SIZE."
  `(list-of (cons ,key-type ,value-type) ,size))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   \"Spain without the S\" operations in a symbolic ilk of plasmature."
  '(member
    :create-an-accumulator
    :decrement
    :rotate
    :print
    :input
    :start-a-loop
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable \"Spain without the S\"
   program as a one-dimensional simple array of ``instruction''s."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype instruction-table ()
  "The ``instruction-table'' type defines a unidirectional assocation
   atwixen a \"Spain without the S\" language keyword and its
   representative instruction symbol, realized in an association list's
   plasmature, the keys of which assumes simple strings, while their
   allied values subsume into the ``instruction'' type."
  '(association-list-of simple-string instruction 7))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a unidirectional association from
   the iterance construct's end positions to the allied start points,
   mediated by adminiculum of the respective instructions' zero-based
   indices into the ensconcing program vector."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-into-a-boolean-value (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   produces a veridicous Boolean paregal thereof, returning for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of fixnum 6) +WHITESPACE-CHARACTER-CODES+))
(declaim (type character          +NULL-CHARACTER+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTER-CODES+
  '(9 10 11 12 13 32)
  "Enhalses the whitespace characters' ASCII codes in a list.")

(defparameter +NULL-CHARACTER+
  (code-char 0)
  "The \"null character\", associated with the ASCII code of zero (0).")

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   the diorism appropriated by whom lays its amplection around the
   horizontal tab, newline or line feed, vertical tabulation, form feed,
   carriage return, and space specimens, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (translate-into-a-boolean-value
      (find
        (char-code candidate)
        +WHITESPACE-CHARACTER-CODES+
        :test #'=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (haystack needle start-index)
  "Determines whether the NEEDLE is entailed in the HAYSTACK, the
   former's incipient character conflating with the START-INDEX,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type simple-string haystack))
  (declare (type simple-string needle))
  (declare (type fixnum        start-index))
  (the boolean
    (translate-into-a-boolean-value
      (string= haystack needle
        :start1 start-index
        :end1   (min (+ start-index (length needle))
                     (length haystack))))))

;;; -------------------------------------------------------

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either by its
   own delivery, upon the input's subsumption into this particular
   species of string; or, if not already compatible with the
   desideratum, through a fresh simple string tantamount's production.
   ---
   The SOURCE will not be modulated."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the source code normalizing operations.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-word (source start-index)
  "Proceeding from the inclusive START-INDEX into the SOURCE, returns
   the index of the nearest following word's first character; or, upon
   its absence, responds with the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source
          :start start-index)
        (length source))))

;;; -------------------------------------------------------

(defun demarcate-the-next-word (source start-index)
  "Proceeding from the inclusive START-INDEX into the SOURCE, locates
   the nearest following word and returns two values:
     (1) If a word could be detected, its first character's index;
         otherwise, the SOURCE's length.
     (2) If a word could be detected, the index into the SOURCE
         immediately succeeding its desinent character; otherwise, the
         SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (the (values fixnum fixnum)
    (let ((word-start-index (locate-the-next-word source start-index)))
      (declare (type fixnum word-start-index))
      (values
        word-start-index
        (or (position-if #'whitespace-character-p source
              :start word-start-index)
            (length source))))))

;;; -------------------------------------------------------

(defmacro with-the-tokenized-string ((source driver-name) &body body)
  "Evaluates the SOURCE string, defines a local function norned by the
   DRIVER-NAME, which, accepting no argument, upon each request returns
   the next token from the SOURCE, or, upon its exhaustion, responds
   with a fresh empty string; evaluates the BODY forms with a
   concomitant adit to the DRIVER-NAME function, and returns the
   desinent form's results."
  (let ((evaluated-source (gensym))
        (start-index-name (gensym))
        (end-index-name   (gensym)))
    (declare (type symbol evaluated-source))
    (declare (type symbol start-index-name))
    (declare (type symbol end-index-name))
    `(let ((,evaluated-source ,source)
           (,start-index-name 0)
           (,end-index-name   0))
       (declare (type simple-string ,evaluated-source))
       (declare (type fixnum        ,start-index-name))
       (declare (type fixnum        ,end-index-name))
       (flet
           ((,driver-name ()
              "Returns the next token from the SOURCE, or, upon the
               same's exhaustion, responds with an empty string."
              (multiple-value-setq (,start-index-name ,end-index-name)
                (demarcate-the-next-word
                  ,evaluated-source
                  ,end-index-name))
              (the simple-string
                (subseq
                  ,evaluated-source
                  ,start-index-name
                  ,end-index-name))))
         ,@body))))

;;; -------------------------------------------------------

(defun normalize-the-string (source)
  "Returns a fresh simple string representing the SOURCE's canonical
   form for a \"Spain without the S\" program, the diorism imparted to
   whom imposes atwixen each jumelle of accolent words an aefauld space
   character, while removing from the SOURCE's front and rear all
   whitespace contents.
   ---
   The SOURCE will not be modulated."
  (declare (type string source))
  (the simple-string
    (convert-into-a-simple-string
      (with-output-to-string (normalized-source)
        (declare (type string-stream normalized-source))
        (with-the-tokenized-string (source request-the-next-token)
          (loop
            for next-token
              of-type simple-string
              =       (request-the-next-token)
            and previous-token-exists-p
              of-type boolean
              =       NIL
              then    T
            until (string= next-token "") do
              (format normalized-source "~@[ ~*~]~a"
                previous-token-exists-p
                next-token)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type instruction-table         +INSTRUCTION-TABLE+))
(declaim (type (list-of simple-string 7) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TABLE+
  '(("Spain without the Spain" . :print)
    ("Spain without the pain"  . :input)
    ("Spain without the S"     . :start-a-loop)
    ("Spain without the p"     . :decrement)
    ("Spain without the a"     . :rotate)
    ("Spain without the i"     . :jump-back)
    ("Spain without the n"     . :create-an-accumulator))
  "Associates the recognized \"Spain without the S\" language
   identifiers with representative instruction symbols, concomitantly
   imposing a sane kelter for their probing.")

(defparameter +IDENTIFIERS+
  (mapcar #'car +INSTRUCTION-TABLE+)
  "Lists the \"Spain without the S\" language's identifiers as simple
   strings.")

;;; -------------------------------------------------------

(defun look-up-the-instruction-for (identifier)
  "Returns the instruction allied to the IDENTIFIER according to the
   ``+INSTRUCTION-TABLE+'' table, or, upon its disrespondency, signals an
   error of an unspecified type."
  (declare (type simple-string identifier))
  (the instruction
    (or (cdr (assoc identifier +INSTRUCTION-TABLE+ :test #'string=))
        (error "No instruction associates with the identifier ~s."
          identifier))))

;;; -------------------------------------------------------

(defun probe-the-identifier (source identifier start-index)
  "Determines whether the IDENTIFER occurs in the SOURCE string, the
   former inchoating at the inclusive START-INDEX, and returns two
   values:
     (1) If the IDENTIFIER could be detected at the START-INDEX in the
         SOURCE, the affiliated \"Spain without the S\" instruction
         keyword; otherwise the ``NIL'' sentinel.
     (2) If the IDENTIFIER could be detected at the START-INDEX in the
         SOURCE, the position into the SOURCE immediately succeeding
         the tmema occupied by the IDENTIFIER; otherwise the
         START-INDEX itself."
  (declare (type simple-string source))
  (declare (type simple-string identifier))
  (declare (type fixnum        start-index))
  (the (values (or null instruction) fixnum)
    (if (substring-starts-at-p source identifier start-index)
      (values
        (look-up-the-instruction-for identifier)
        (+ start-index
           (length identifier)))
      (values NIL start-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-a-program-from (instructions)
  "Creates and returns a fresh ``program'' whose conformation ensues
   from the list of INSTRUCTIONS."
  (declare (type (list-of instruction *) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-attiguous-whitespaces (source start-index)
  "Avaunting from the inclusive START-INDEX into the SOURCE, skips a
   catena encompassing zero or more attiguous whitespace character and
   returns the position into the SOURCE immediately succeeding the thus
   peragrated tmema."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source
          :start start-index)
        (length source))))

;;; -------------------------------------------------------

(defun expect-one-or-more-whitespaces (source start-index)
  "Determines whether the SOURCE character empight at the START-INDEX
   subsumes into the whitespaces' species, on confirmation skipping any
   attiguous whitespaces, and returning the character into the SOURCE
   immediately succeeding the thus peragrated tmema; otherwise signals
   an error of an unspecified type."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (the fixnum
    (let ((probed-character (schar source start-index)))
      (declare (type character probed-character))
      (if (whitespace-character-p probed-character)
        (skip-attiguous-whitespaces source start-index)
        (error "One or more whitespace characters were expected ~
                commencing at the position ~d, but instead the ~
                character \"~c\" has been encountered."
          start-index probed-character)))))

;;; -------------------------------------------------------

(defun extract-the-next-instruction (source start-index)
  "Extracts the \"Spain without the S\" instruction commencing at the
   inclusive START-INDEX into the SOURCE and returns two values:
     (1) An ``instruction'' representation of the detected operation.
     (2) The position into the SOURCE immediately succeeding the
         extracted instruction's occupied tmema."
  (declare (type simple-string source))
  (declare (type fixnum        start-index))
  (the (values instruction fixnum)
    (loop
      with next-instruction of-type (or null instruction) = NIL
      and  new-index        of-type fixnum                = 0
      
      for current-identifier of-type simple-string in +IDENTIFIERS+ do
        (multiple-value-setq (next-instruction new-index)
          (probe-the-identifier source current-identifier start-index))
      
      when next-instruction do
        (return
          (values next-instruction new-index))
      
      finally
        (error "Invalid content has been detected in the code ~s ~
                commencing at the position ~d."
          (subseq source start-index)
          start-index))))

;;; -------------------------------------------------------

(defun extract-the-instructions (source)
  "Extracts from the piece of \"Spain without the S\" SOURCE code the
   entailed instructions and returns a fresh ``program'' representation
   thereof."
  (declare (type simple-string source))
  (let ((current-instruction NIL)
        (current-position    0))
    (declare (type (or null instruction) current-instruction))
    (declare (type fixnum                current-position))
    (the program
      (assemble-a-program-from
        (loop
          initially
            (setf current-position
              (skip-attiguous-whitespaces source current-position))
          
          while (< current-position (length source)) do
            (multiple-value-setq (current-instruction current-position)
              (extract-the-next-instruction source current-position))
          
          collect current-instruction
          
          do
            (if (array-in-bounds-p source current-position)
              (setf current-position
                (expect-one-or-more-whitespaces
                  source
                  current-position))
              (loop-finish)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-jump-table ()
  "Creates and returns an initially vacant ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (connections start-point end-point)
  "Associates the jump END-POINT with the START-POINT in a
   unidirectional fashion from the former to the latter, stores this
   vinculum in the CONNECTIONS table, and returns no value."
  (declare (type jump-table connections))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (setf (gethash end-point connections) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (program)
  "Builds and returns for the \"Spain without the S\" PROGRAM the jump
   table, thilk affiliates its iterance end points with the matching
   start positions into the same per procurationem of their zero-based
   indices into the PROGRAM vector."
  (declare (type program program))
  (let ((connections  (prepare-an-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table       connections))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-instruction of-type instruction across program
      and current-index       of-type fixnum      from   0 by 1
      
      if (eq current-instruction :start-a-loop) do
        (push current-index start-points)
      else if (eq current-instruction :jump-back) do
        (if start-points
          (connect-the-jump-points
            connections
            (pop start-points)
            current-index)
          (error "An unmatched back jump instruction has been ~
                  detected."))
      end
      
      finally
        (when start-points
          (error "One or more unmatched loop start points have been ~
                  detected.")))
    (the jump-table connections)))

;;; -------------------------------------------------------

(defun locate-the-loop-start-point (connections end-point)
  "Returns the loop start point affiliated with the END-POINT in the
   CONNECTIONS table; or, upon its disrespondency, signals an error of
   an unspecified type."
  (declare (type jump-table connections))
  (declare (type fixnum     end-point))
  (the fixnum
    (or (gethash end-point connections)
        (error "No start point could be detected for the loop ~
                end point with the index ~d."
          end-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the accumulator.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Accumulator
  (:constructor make-an-accumulator ()))
  "The ``Accumulator'' class implements a memory accumulator as a singly
   linked cells, its capacity meted at an aefauld unsigned integer
   number greater than or equal to zero (0)."
  (value     0   :type (integer 0 *)         :read-only NIL)
  (successor NIL :type (or null Accumulator) :read-only NIL))

;;; -------------------------------------------------------

(defun decrement-the-accumulator (accumulator)
  "If the ACCUMULATOR state is greater than zero (0), decrements the
   ACCUMULATOR value by a mountance of one (1); otherwise sets its state
   to a value of 127, in any case returning no value."
  (declare (type Accumulator accumulator))
  (if (zerop (accumulator-value accumulator))
    (setf (accumulator-value accumulator) 127)
    (decf (accumulator-value accumulator) 1))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory wheel.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Wheel ()
  ((head
    :initform      (make-an-accumulator)
    :type          Accumulator
    :documentation "The inchoate accumulator.")
   (pointer
    :type          Accumulator
    :documentation "A reference to the currently selected accumulator.")
   (size
    :initform      1
    :type          (integer 1 *)
    :documentation "The tally of accumulators comprising the wheel."))
  (:documentation
    "The ``Wheel'' class applies itself to the dever of the program
     memory's furnishment, its desigment ensuing from a wheel's
     simulacrum, the constituents of which are manifested in
     integer-valued accumulators, the current entity, exclusive
     entalented with an amenability to inquisitions and modulations,
     being referenced by a pointer."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((wheel Wheel) &key)
  "Alligates the WHEEL's inchoate accumulator with itself, empights the
   pointer to the same, and returns no value."
  (declare (type Wheel wheel))
  (with-slots (head pointer) wheel
    (declare (type Accumulator head))
    (declare (type Accumulator pointer))
    (psetf
      (accumulator-successor head) head
      pointer                      head))
  (values))

;;; -------------------------------------------------------

(defun make-a-wheel ()
  "Creates and returns a fresh ``Wheel'' assuming the default
   configuration of a single accumulator's participation."
  (the Wheel
    (make-instance 'Wheel)))

;;; -------------------------------------------------------

(defun current-accumulator-value (wheel)
  "Returns the value stored in the WHEEL's current accumulator."
  (declare (type Wheel wheel))
  (the integer
    (with-slots (pointer) wheel
      (declare (type Accumulator pointer))
      (accumulator-value pointer))))

;;; -------------------------------------------------------

(defun (setf current-accumulator-value) (new-value wheel)
  "Stores the NEW-VALUE in the WHEEL's current accumulator and returns
   no value."
  (declare (type Wheel wheel))
  (with-slots (pointer) wheel
    (declare (type Accumulator pointer))
    (setf (accumulator-value pointer) new-value))
  (values))

;;; -------------------------------------------------------

(defun decrement-the-current-accumulator (wheel)
  "If the the value of the WHEEL's currently active accumulator is
   greater than zero (0), decrements the same by a mountance of one (1);
   otherwise sets the state to the value of 127; and in any case returns
   no value."
  (declare (type Wheel wheel))
  (with-slots (pointer) wheel
    (declare (type Accumulator pointer))
    (decrement-the-accumulator pointer))
  (values))

;;; -------------------------------------------------------

(defun rotate-the-wheel (wheel)
  "Advances the WHEEL's pointer to the next accumulator following a
   deasil airt and returns no value."
  (declare (type Wheel wheel))
  (with-slots (pointer) wheel
    (declare (type Accumulator pointer))
    (setf pointer
      (accumulator-successor pointer)))
  (values))

;;; -------------------------------------------------------

(defun insert-an-accumulator-into-the-wheel (wheel)
  "Inserts a new accumulator abreast the currently selected one at its
   deasil laterality, without relocating the cursor itself, and returns
   no value."
  (declare (type Wheel wheel))
  (with-slots (pointer size) wheel
    (declare (type Accumulator   pointer))
    (declare (type (integer 0 *) size))
    (let ((new-accumulator (make-an-accumulator)))
      (declare (type Accumulator new-accumulator))
      (setf (accumulator-successor new-accumulator)
            (accumulator-successor pointer))
      (setf (accumulator-successor pointer)
            new-accumulator))
    (incf size 1))
  (values))

;;; -------------------------------------------------------

(defun desinent-accumulator-p (wheel candidate)
  "Determines whether the CANDIDATE represents the desinent accumulator
   partaking in the WHEEL, the diorism of which imposes its successor
   to conflate with the WHEEL's incipient constituent, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Wheel       wheel))
  (declare (type Accumulator candidate))
  (the boolean
    (translate-into-a-boolean-value
      (eq (accumulator-successor candidate)
          (slot-value wheel 'head)))))

;;; -------------------------------------------------------

(defmethod print-object ((wheel Wheel) (destination T))
  (declare (type Wheel  wheel))
  (declare (type stream destination))
  (with-slots (head pointer size) wheel
    (declare (type Accumulator   head))
    (declare (type Accumulator   pointer))
    (declare (type (integer 1 *) size))
    (loop
      initially
        (format destination "Wheel:")
      
      with angular-distance of-type (real 0 360) = (/ 360 size)
      
      for current-accumulator
        of-type Accumulator
        =       head
        then    (accumulator-successor current-accumulator)
      and current-angle
        of-type (real 0 360)
        =       0
        then    (+ current-angle angular-distance)
      and current-index
        of-type (integer 1 *)
        from    1
        by      1
      
      do
        (format destination "~&~2t[~d] ~a degrees = ~d~@[ (current)~]"
          current-index
          current-angle
          (accumulator-value current-accumulator)
          (eq current-accumulator pointer))
      
      until
        (desinent-accumulator-p wheel current-accumulator)))
  (the Wheel wheel))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-spain-without-the-s-program (program)
  "Executes the \"Spain without the S\" PROGRAM returns no value."
  (declare (type program program))
  (handler-case
    (let ((ip          0)
          (connections (supputate-the-jump-table-for program))
          (wheel       (make-a-wheel)))
      (declare (type fixnum     ip))
      (declare (type jump-table connections))
      (declare (type Wheel      wheel))
      (loop while (< ip (length program)) do
        (case (aref program ip)
          (:create-an-accumulator
            (insert-an-accumulator-into-the-wheel wheel))
          (:decrement
            (decrement-the-current-accumulator wheel))
          (:rotate
            (rotate-the-wheel wheel))
          (:print
            (format T "~c"
              (code-char
                (current-accumulator-value wheel)))
            (finish-output))
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf (current-accumulator-value wheel)
              (char-code
                (read-char NIL NIL +NULL-CHARACTER+)))
            (clear-input))
          (:start-a-loop
            NIL)
          (:jump-back
            (unless (zerop (current-accumulator-value wheel))
              (setf ip
                (locate-the-loop-start-point connections ip))))
          (otherwise
            (error "The instruction ~s cannot be processed."
              (aref program ip))))
        (incf ip)))
    
    (error (e)
      (format T "~&~a" e)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-spain-without-the-s-code (code)
  "Interprets the piece of \"Spain without the S\" source CODE and
   returns no value."
  (declare (type string code))
  (execute-the-spain-without-the-s-program
    (extract-the-instructions
      (normalize-the-string code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-the-spain-without-the-s-code
  "Spain without the pain Spain without the Spain")

;;; -------------------------------------------------------

;; Perpetually repeating cat program.
(interpret-the-spain-without-the-s-code
  "Spain without the n
   Spain without the p
   Spain without the S
   Spain without the a
   Spain without the pain
   Spain without the Spain
   Spain without the a 
   Spain without the i")

;;; -------------------------------------------------------

;; Infinite loop which prints the letter "A" perpetually.
(interpret-the-spain-without-the-s-code
  "Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the p
   Spain without the S
   Spain without the Spain
   Spain without the i")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-spain-without-the-s-code
  "Spain without the pain
Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p
Spain without the S
Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p 
Spain without the Spain
Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p Spain without the p
Spain without the i")
