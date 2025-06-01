;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "OIIAOIIA", invented by the Esolang user "Tommyaweosme" and
;; presented on February 20th, 2025, the kenspeckle designment of which
;; emerges from its consanguinity with Urban Mueller's "brainfuck",
;; encoding the octuple instruction set in the transitions betwixt its
;; characters, mapping to combinations of accolent "A", "I", and "O"
;; symbols operations from the entheus' realm.
;; 
;; 
;; Concept
;; =======
;; OIIAOIIA encodes brainfuck's octuple instruction set, as well as an
;; adscititious nineth commodity, in the transition betwixt the
;; characters from the recognized set "A", "I", and "O".
;; 
;; The decoded brainfuck program applies to the acquainted principles
;; commorant in the language's specification; scilicet, the operation
;; upon a bilaterally bourneless dispansion of unsigned byte-valued
;; cells, the currently active, and thus exclusively amenable, unit
;; among these being designated by a mobile cell pointer.
;; 
;; == OIIAOIIA: A BRAINFUCK ENCODING ==
;; The OIIAOIIA programming language's foundry is defined by an encoding
;; of brainfuck's instruction set in the transitions betwixt specific
;; letters, which as an ordered twain map unambiguously to certain
;; operations or operation sequences of the target language.
;; 
;; == OIIAOIIA MERELY CONSIDERS FOUR CHARACTERS ==
;; The effective OIIAOIIA symbol diorism amplects the quadruple of
;; "A", "I", "O", and the space character " ", the first triad among
;; these establishes the "transition characters", whose proximity
;; begets the decoded data; the space engages in the latreutical wike
;; of such a transition sequence's termination in order to establish a
;; fracture from the prevenient catena.
;; 
;; In a diction entalented with compendiousness the following symbols
;; partake of an elevated rank:
;; 
;;   ------------------------------------------------------------------
;;   Symbol    | Role
;;   ----------+-------------------------------------------------------
;;   A         | Transition character. If succeeded by any of the
;;             | letters "A", "I", or "O", resolves to a stipulated
;;             | brainfuck instruction.
;;   ..................................................................
;;   I         | Transition character. If succeeded by any of the
;;             | letters "A", "I", or "O", resolves to a stipulated
;;             | brainfuck instruction.
;;   ..................................................................
;;   O         | Transition character. If succeeded by any of the
;;             | letters "A", "I", or "O", resolves to a stipulated
;;             | brainfuck instruction.
;;   ..................................................................
;;     (space) | Transition terminator. Concludes the preceding
;;             | transition sequence; the next character from the set
;;             | encompassing "A", "I", and "O" does not produce an
;;             | effect in combination with the transition character
;;             | prevenient to the space, but rather inchoates a new
;;             | and independent transition sequence.
;;   ------------------------------------------------------------------
;; 
;; Any other content, irregardless of its dispansion and location, is
;; apportioned the ineffectuous agency of commentary supererogation.
;; In corollary, two transition characters segregated via one or more
;; adscititious non-operative content still coalesce into a transition
;; twain, perfectly disregarding the forinsecal contributions.
;; 
;; == TRANSITION CHARACTER PAIRS FORM BRAINFUCK INSTRUCTIONS ==
;; An OIIAOIIA program's execution proceeds in a sinistrodextral airt,
;; its conspectuity during each step applied to the current character
;; and its immediate successor, including in this progression merely
;; the transition characters, thilk intrine "A", "I", and "O", and the
;; resetting space (" ") symbol; any other content's contribution limns
;; a complete lack of consideration.
;; 
;; Given a transition twain, edified upon a combination of two "A", "I",
;; or "O" entities, obtained in siccan mode, the respective brainfuck
;; operation or catena of operations, desumed from a fixed definition,
;; is appended to the hitherto collated output code. These associative
;; diorisms, molded into a tabular format, establish following
;; nomothesia:
;; 
;;   ----------------------------------------------------------------
;;   Prevenient character | Current character | brainfuck equivalent
;;   ---------------------+-------------------+----------------------
;;   O                    | O                 | +
;;   ................................................................
;;   O                    | I                 | -
;;   ................................................................
;;   O                    | A                 | ++++++++++
;;   ................................................................
;;   I                    | O                 | <
;;   ................................................................
;;   I                    | I                 | [
;;   ................................................................
;;   I                    | A                 | ]
;;   ................................................................
;;   A                    | O                 | .
;;   ................................................................
;;   A                    | I                 | ,
;;   ................................................................
;;   A                    | A                 | >
;;   ----------------------------------------------------------------
;; 
;; Ensuing from this coalescence of the left and right transition
;; moeity, the dextral component is designated the new current
;; character, adhibiting the conspection upon the subsequent symbol as
;; the contingent new next twissel compartment.
;; 
;; Upon a space character's confrontation, the catena that forms the
;; contemporaneously active transition sequence immediately ceases,
;; disabling the next following transition character from its engagement
;; with the prevenient one, and a subsequent brainfuck equivalency's
;; gendrure. An ultimity begotten by this occasion, the transition
;; character succeeding the space serves to inchoate a fresh transition
;; sequence, eloigned from the previous state.
;; 
;; == A FORBISEN'S ELUCIDATION OF THE CONCEPT ==
;; The following example adduces in the upper echolon the illustrative
;; OIIAOIIA program, while the lower catena of digits assigns to the
;; symbols the occupied positions for later references:
;; 
;;   AIOO IA I
;;   123456789
;; 
;; Ensuing from the aboon produced diorism, the following tabulation's
;; dever shall be realized in the demonstration of the program's
;; execution. The first column enumerates the involved positions; the
;; second file stores the transition twains or other instruction
;; actuators; whereas the third and desinent compartment elucidates the
;; begotten causata.
;; 
;;   ------------------------------------------------------------------
;;   Pos. | Seq. | Effect
;;   -----+------+-----------------------------------------------------
;;   1--2 | AI   | Transition from "A" (1) to "I" (2).
;;   ..................................................................
;;   2--3 | IO   | Transition from "I" (2) to "O" (3).
;;   ..................................................................
;;   3--4 | OO   | Transition from "O" (3) to "O" (4).
;;   ..................................................................
;;   5    |      | Space (5): The current transition sequence ends. The
;;        |      | next character, expected at the position (6), will
;;        |      | commence a new transition, not considering the
;;        |      | prevenient "O" at position (4).
;;   ..................................................................
;;   6--7 | AO   | Transition from "I" (6) to "A" (7).
;;   ..................................................................
;;   8    |      | Space (8): The current transition sequence ends. The
;;        |      | next character, expected at the position (9), will
;;        |      | commence a new transition, not considering the
;;        |      | prevenient "A" at position (7).
;;   ..................................................................
;;   9    | I    | Solitary "I" (9): As no subsequent character exists
;;        |      | to establish a transition, no causatum applies to
;;        |      | this letter.
;;   ------------------------------------------------------------------
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of OIIAOIIA's recipiency does not elude brainfuck's
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
;; 
;; Instructions
;; ============
;; The OIIAOIIA instruction set enumerates an ennead of capacitities,
;; the preponderance among these an ipsissima verba appropration from
;; its brainfuck stock-father; however, the cleronomy's dation extends
;; into a supererogation founded upon the extant warklumes.
;; 
;; == OVERVIEW ==
;; The following apercu's wike shall be the enneadic transitions' and
;; the reseting behest's presentation.
;; 
;; Please heed that the instruction engaged in an alliance with the
;; space symbol (" "), and commorant in the table's desinent row,
;; is limned as
;; 
;;     (space)
;; 
;; forecause from its ostention does not emerge a visible perception.
;; 
;;   ------------------------------------------------------------------
;;   Transition | Effect
;;   -----------+------------------------------------------------------
;;   OO         | Increments the current cell value by one (1). If the
;;              | new state transcends the upper bourne of 255, the
;;              | value wraps around to the minimum of zero (0).
;;   ..................................................................
;;   OI         | Decrements the current cell value by one (1). If the
;;              | new state transcends the lower bourne of zero (0),
;;              | the value wraps around to the maximum of 255.
;;   ..................................................................
;;   OA         | Increments the current cell value by ten (10). If the
;;              | new state transcends the upper bourne of 255, the
;;              | value wraps around, commencing upwards from the
;;              | minimum of zero (0).
;;              |------------------------------------------------------
;;              | In a pseudocode diction, the following holds:
;;              |   tape[pointer] <- (tape[pointer] + 10) modulus 256
;;   ..................................................................
;;   IO         | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   II         | If the current cell value equals zero (0), moves the
;;              | instruction pointer (IP) forward to the position
;;              | immediately succeeding the matching back jump point,
;;              | signified by an "IA" transition; otherwise proceeds
;;              | as usual.
;;   ..................................................................
;;   IA         | If the current cell value does not equal zero (0),
;;              | moves the instruction pointer (IP) back to the
;;              | position immediately succeeding the matching forward
;;              | jump point, signified by an "II" transition;
;;              | otherwise proceeds as usual.
;;   ..................................................................
;;   AO         | Prints the character whose ASCII code corresponds to
;;              | the current cell value to the standard output
;;              | conduit.
;;   ..................................................................
;;   AI         | Queries the standard input conduit for a character
;;              | and stores its ASCII code in the current cell.
;;   ..................................................................
;;   AA         | Translates the cell pointer one step to the right.
;;   ..................................................................
;;     (space)  | Terminates the current transition block, thus
;;              | resetting the same. As a corollary, the next
;;              | transition character, "O", "A", or "I", commences a
;;              | new transition, in lieu of producing an immediate
;;              | operative effect with the prevenient one.
;;   ------------------------------------------------------------------
;; 
;; == OIIAOIIA AND BRAINFUCK ==
;; The equipollence partaking in the relationship betwixt OIIAOIIA and
;; brainfuck capacitates an immediate equiparation, juxtaposing each
;; enneadic transition twain from the former with one or more operations
;; commorant in the latter's vale. The alow tabulation's dever shall
;; be satisfied in this equivalency's ostention:
;; 
;;   -------------------------------------------------
;;   OIIAOIIA transition twain | brainfuck equivalent
;;   --------------------------+----------------------
;;   OO                        | +
;;   .................................................
;;   OI                        | -
;;   .................................................
;;   OA                        | ++++++++++
;;   .................................................
;;   IO                        | <
;;   .................................................
;;   II                        | [
;;   .................................................
;;   IA                        | ]
;;   .................................................
;;   AO                        | .
;;   .................................................
;;   AI                        | ,
;;   .................................................
;;   AA                        | >
;;   -------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the lucidity partaking of the explications and the adduced
;; examples, the protolog's circumference yet admits a few
;; ambivalencies' inroad; a subset of these shall establish the
;; following tmemata's cynosure.
;; 
;; == MAY ANY WHITESPACE ASSUME A THE SPACE CHARACTER'S WIKE? ==
;; The original specification relates of the space character (" ") as
;; the transition sequence's terminating symbol in explicit terms;
;; however, the participation of any other symbol, including the
;; whitespace species, its demarcation ensuing from the septuple of
;; 
;;   --------------------------------------------------------
;;   ASCII code | Whitespace character name | Escape sequence
;;   -----------+---------------------------+----------------
;;   9          | Tab                       | \\t
;;   ........................................................
;;   10         | Newline, Line Feed        | \\n
;;   ........................................................
;;   11         | Vertical Tabulation       | \\v
;;   ........................................................
;;   12         | Form Feed                 | \\f
;;   ........................................................
;;   13         | Carriage Return           | \\r
;;   ........................................................
;;   32         | Space                     | (none)
;;   --------------------------------------------------------
;; 
;; is inflicted with negligence. Hence, a lacuna in the inquisition of
;; the entire seven members as surrogates for the "space" definition
;; requires its attendance.
;; 
;; A twissel of contingencies may be proposed:
;; 
;;   (1) All whitespace characters conflate into an equivalence with
;;       the space's operative epiphenomenon.
;;   
;;   (2) Any whitespace character, except for the space, are treated as
;;       non-operative content, akin to the remaining set of undefined
;;       characters.
;; 
;; It has been adjudged to impute the first (1) option, which states
;; that any whitespace character may be incorporated into the program
;; as a succedaneum to the space symbol, limning an equipollent
;; causatum's influence.
;; 
;; == HOW ARE NON-OPERATIVE CHARACTERS HANDLED? ==
;; OIIAOIIA syntaxis wists merely of a quadruple operative warklumes in
;; the symbols "A", "I", "O", and the space --- or, by the latter's
;; extension, the whitespace realm. The remaining ASCII repertoire's
;; effect upon a program is excluded from an explicit treatise's
;; reception.
;; 
;; It has been adjudged, from the language author's stated claim in
;; OIIAOIIA's provenance as a brainfuck derivative, to administer to
;; all non-operative characters the agency of commentary value. Their
;; presence, even inwith transition sequences, shall be subjected to
;; a disregard tantamount in its patration with its enjoyed tolerance.
;; 
;; A forbisen of this policy adduced, the code tmema
;; 
;;   AxI
;; 
;; ostends a paregal interpretation as the purged transition twain
;; 
;;   AI
;; 
;; eschewing the advenient substance incurred by the letter "x".
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand has been implemented in the programming
;; language Common Lisp, preceding the actual execution process with a
;; parasceve that involves the OIIAOIIA source code's analyzation in
;; order to extract and collate the ensconced instructions in a more
;; connable format.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-05-25
;; 
;; Sources:
;;   [esolang2025OIIAOIIA]
;;   The Esolang contributors, "OIIAOIIA", March 9th, 2025
;;   URL: "https://esolangs.org/wiki/OIIAOIIA"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-type-which-satisfies
    (type-name (candidate-name &rest formal-parameters)
     &body body)
  "Defines a derived type whose agnomination is desumed from the
   TYPE-NAME and whose parameter list derives from the
   FORMAL-PARAMETERS, communicating the object to probe for its
   compatibility via the CANDIDATE-NAME, evaluates the BODY forms, and
   construes the desinent form's primary result as the docimasy's
   conclusion, with a \"generalized boolean\" value of \"true\" serving
   in the eligibility's signification, while a \"false\" response
   amounts to its rejection.
   ---
   The first BODY form, upon its resolution to a string object,
   entertains a conspection as the type's documentation string, and will
   hence be appropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,formal-parameters
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))

;;; -------------------------------------------------------

(define-type-which-satisfies list-of
    (candidate
     &optional (element-type '*)
               (size         '*))
  "The ``list-of'' type defines a list composed of SIZE elements, each
   member complying with the ELEMENT-TYPE, for both arguments is imposed
   the generic sentinel ``*'' as a default.
   ---
   If the SIZE is specified and resolves to a numeric value, the
   CANDIDATE as a list must exhibit an equinumerant account of items to
   this definition; otherwise, for a SIZE assuming the generic sentinel
   ``*'', any componency may be partaken of the probed list."
  (and
    (listp candidate)
    (or
      (eq size '*)
      (= (length (the list candidate))
         size))
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-type-which-satisfies hash-table-of
    (candidate
     &optional (key-type   '*)
               (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which complies to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the generic
   sentinel ``*'' as a default."
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

(define-type-which-satisfies property-list-of
    (candidate
     &optional (indicator-type '*)
               (value-type     '*))
  "The ``property-list-of'' type defines a property list composed of
   zero or more entries, each indicator, or key, of which complies with
   the INDICATOR-TYPE and answers to a associated value of the
   VALUE-TYPE, for both is specified the generic sentinel ``*'' as a
   default."
  (and
    (listp candidate)
    (evenp (length (the list candidate)))
    (loop
      for (current-indicator current-value)
        of-type (T T)
        on      (the list candidate)
        by      #'cddr
      always
        (and (typep current-indicator indicator-type)
             (typep current-value     value-type)))))

;;; -------------------------------------------------------

(deftype transition-twain ()
  "The ``transition-twain'' type defines a twissel composite of
   characters which may represent, by transitioning from the first
   moiety to the second, an operative behest."
  '(simple-string 2))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   OIIAOIIA instructions."
  '(member
    :increment-by-one
    :increment-by-ten
    :decrement-by-one
    :move-left
    :move-right
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable OIIAOIIA program as a
   one-dimensional simple array of instructions."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which, without the contingency for exhaustion,
   enumerates the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   matching jump points in an OIIAOIIA program, mediated by adminiculum
   of their zero-based positions into the same, and realized in a hash
   table's mold, the keys and values inwith thilk assume fixnum
   objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, the coefficiency of which forms an occupant of the
   closed integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-key-value-pairs-into-hash-table (recipient
                                               &rest key-value-pairs)
  "Inserts into the RECIPIENT hash table the entries whose diorism
   ensues from the property list of KEY-VALUE-PAIRS and returns the
   modified RECIPIENT.
   ---
   Please heed that extant entries in the receiving hash table will be
   superseded upon a concurrency of an indicator extracted from the
   KEY-VALUE-PAIRS list."
  (declare (type hash-table             recipient))
  (declare (type (property-list-of T T) key-value-pairs))
  (the hash-table
    (loop
      for (current-indicator current-value)
        of-type (T T)
        on      key-value-pairs
        by      #'cddr
      do
        (setf (gethash current-indicator recipient) current-value)
      finally
        (return recipient))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of (integer 9 32) 6) +WHITESPACE-CHARACTER-CODES+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTER-CODES+
  '(9 10 11 12 13 32)
  "Defines an unordered list comprehending the recognized whitespace
   entities' ASCII codes.
   ---
   In a diction entalented with concrete expressive fortitude and
   compendiousness, the following sextuple membership is imputed among
   the whitespace species' terminology:
     --------------------------------------------------------
     ASCII code | Whitespace character name | Escape sequence
     -----------+---------------------------+----------------
     9          | Tab                       | \\t
     ........................................................
     10         | Newline, Line Feed        | \\n
     ........................................................
     11         | Vertical Tabulation       | \\v
     ........................................................
     12         | Form Feed                 | \\f
     ........................................................
     13         | Carriage Return           | \\r
     ........................................................
     32         | Space                     | (none)
     --------------------------------------------------------")

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   inwith whose diorism are amplected the space, horizontal tab, and
   newline entities, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''.
   ---
   For this species' membership's definition, please consult the global
   constant +WHITESPACE-CHARACTER-CODES+."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate +WHITESPACE-CHARACTER-CODES+
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun transition-character-p (candidate)
  "Determines whether the CANDIDATE represents an OIIAOIIA character
   for which a transition behavior is specified, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "AIO" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-character-equals-p (source expected-character)
  "Determines whether the SOURCE string's first character equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string    source))
  (declare (type character expected-character))
  (the boolean
    (get-boolean-value-of
      (and
        (plusp (length source))
        (char= (char source 0)
               expected-character)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of transition-twain instruction)
               +DECODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +DECODING-TABLE+
  (insert-key-value-pairs-into-hash-table
    (make-hash-table :test #'equal)
    "OO" :increment-by-one
    "OI" :decrement-by-one
    "OA" :increment-by-ten
    "IO" :move-left
    "II" :jump-forward
    "IA" :jump-back
    "AO" :output
    "AI" :input
    "AA" :move-right)
  "Affiliates with the recognized OIIAOIIA transition twains, each
   such a twissel of characters desumed from the set
   {\"A\", \"I\", \"O\"}, a representative ``instruction'' instance.")

;;; -------------------------------------------------------

(defun assemble-transition-twain (first-character second-character)
  "Creates and returns a fresh ``transition-twain'' by concatenating
   the FIRST-CHARACTER and the SECOND-CHARACTER in this exact order."
  (the transition-twain
    (coerce
      (list first-character second-character)
      '(simple-string 2))))

;;; -------------------------------------------------------

(defun decode-instruction (first-character second-character)
  "Returns for the compound of the FIRST-CHARACTER extended by the
   SECOND-CHARACTER the corresponding OIIAOIIA instruction, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type character first-character))
  (declare (type character second-character))
  (let ((transition-twain
          (assemble-transition-twain
            first-character
            second-character)))
    (declare (type transition-twain transition-twain))
    (the instruction
      (or (gethash transition-twain +DECODING-TABLE+)
          (error "Unrecognized transition twain: ~s."
            transition-twain)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh OIIAOIIA program assembled from the
   INSTRUCTIONS list."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of OIIAOIIA parser.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-oiiaoiia-program (code)
  "Parses the piece of OIIAOIIA source CODE and returns a ``program''
   representation of its entailed instructions."
  (declare (type string code))
  (let ((instructions       NIL)
        (previous-character NIL))
    (declare (type (list-of instruction) instructions))
    (declare (type (or null character)   previous-character))
    (loop for current-character of-type character across code do
      (cond
        ;; Whitespace character?
        ;; => Reset transition.
        ((whitespace-character-p current-character)
          (setf previous-character NIL))
        ;; First member of a transition compound?
        ;; => Merely memorize the CURRENT-CHARACTER for future
        ;;    transitions.
        ((and (transition-character-p current-character)
              (null previous-character))
          (setf previous-character current-character))
        ;; Complete compound?
        ;; => Collect corresponding instruction.
        ((and (transition-character-p current-character)
              previous-character)
          (push
            (decode-instruction previous-character current-character)
            instructions)
          (setf previous-character current-character))
        ;; Any other content is ignored as commentary addition.
        (T
          NIL)))
    (the program
      (make-program
        (nreverse instructions)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-pristine-jump-table ()
  "Creates and returns a fresh, initially empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (table start-point end-point)
  "Stores a bidirectional vinculum betwixt the forward jump instruction
   located at the START-POINT and its matching back jump compernage,
   communicated in the END-POINT, in the jump TABLE and returns no
   value."
  (declare (type jump-table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point table) end-point
    (gethash end-point   table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table-for-program (program)
  "Creates and returns a fresh ``jump-table'' dedicated to the
   bidirectional ligation of the PROGRAM's jump points."
  (declare (type program program))
  (let ((jump-table          (prepare-pristine-jump-table))
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
          (error "Unmatched back jump point as instruction at ~
                  position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun locate-destination-jump-point (table point-of-departure)
  "Returns for the POINT-OF-DEPARTURE the opposite jump point in the
   jump TABLE; or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure table)
        (error "No jump point associated with the position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :type          unsigned-byte
    :documentation "The cell values encoded in a single unsigned integer
                    object, each eight accolent bits of which form one
                    cell's octet state, proceeding from the least
                    significant position (LSB) which conflates with the
                    lowest cell index sojourned by the cell POINTER.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer which designates the currently
                    amenable cell inside of the tape's BITS.")
   (smallest-accessed-cell-index
    :initform      0
    :type          integer
    :documentation "The smallest cell index assumed by the POINTER
                    during a program's execution, utible for the
                    translation of the signed inter cell index to an
                    non-negative position into the BITS."))
  (:documentation
    "The ``Tape'' class implements the OIIAOIIA program memory as a
     bilaterally infinite dispansion of unsigned byte-valued cells,
     operated upon by mobile a cell pointer which at any instant
     designates the currently active unit.
     ---
     This mode of realization's edification emerges from the cell
     contents' castaldy inwith a scalar unsigned integer number's bits,
     each octuple of accolent binary digits ensconcing a single cell's
     state."))

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Creates and returns a fresh and empty ``Tape''."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun translate-cell-index-to-bit-offset (tape cell-index)
  "Translates the signed integer CELL-INDEX into a non-negative bit
   offset compliant with the TAPE's configurations and returns thilk."
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (the (integer 0 *)
    (* 8
       (- cell-index
          (slot-value tape 'smallest-accessed-cell-index)))))

;;; -------------------------------------------------------

(defun get-byte-specifier-for-current-cell (tape)
  "Returns an implementation-dependent byte specifier capacitated to
   designate the eight bits into the TAPE's binary sequence which
   corresponds to the cell pointer's contemporaneously selected cell."
  (declare (type Tape tape))
  (the T
    (byte 8
      (translate-cell-index-to-bit-offset tape
        (slot-value tape 'pointer)))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte valued stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the octet
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (slot-value tape 'bits))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceded by a wrapping into the admissible byte range of [0, 255],
   and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (slot-value tape 'bits))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (when (< (slot-value tape 'pointer)
           (slot-value tape 'smallest-accessed-cell-index))
    (psetf
      (slot-value tape 'smallest-accessed-cell-index)
        (slot-value tape 'pointer)
      (slot-value tape 'bits)
        (ash (slot-value tape 'bits) 8)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :documentation "The OIIAOIIA program to execute, communicated in the
                    form of an instruction vector.")
   (jump-points
    :type          jump-table
    :documentation "A bidirectional and symmetric affiliation betwixt
                    the PROGRAM's jump instructions, mediated by their
                    zero-based indices into the PROGRAM.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The zero-based index into the PROGRAM, which
                    designates the currently processed instruction.")
   (tape
    :initform      (prepare-pristine-tape)
    :documentation "The program memory as a bilaterally infinite catena
                    of unsigned byte-valued cells."))
  (:documentation
    "The ``Interpreter'' class is encumbered with the wike of
     accompassing actual operative expression to an OIIAOIIA program
     specified in the form of a static sequence of instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Constructs the jump table for the OIIAOIIA program consigned to the
   INTERPRETER castaldy, stores thilk in the INTERPRETER itself, and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-points)
    (build-jump-table-for-program
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun prepare-interpreter-for-program (program)
  "Creates and returns a fresh ``Interpreter'', dedicated to the
   OIIAOIIA PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the OIIAOIIA program consigned to the
   INTERPRETER's castaldy has been processed to a status of exhaustion,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the INTERPRETER's currently processed instruction."
  (declare (type Interpreter interpreter))
  (the instruction
    (with-slots (program ip) interpreter
      (declare (type program program))
      (declare (type fixnum  ip))
      (aref program ip))))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type program program))
    (declare (type fixnum  ip))
    (when (< ip (length program))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun go-to-opposite-jump-point (interpreter)
  "Expecting the INTERPRETER's current instruction to reference a
   forward or back jump point, relocates its instruction pointer (IP) to
   the opposite point and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program jump-points ip) interpreter
    (declare (type program    program))
    (declare (type jump-table jump-points))
    (declare (type fixnum     ip))
    (setf ip
      (locate-destination-jump-point jump-points ip)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    (instruction (interpreter-name)
     &body body)
  "Establishes an implementation of the generic function
   ``process-instruction'', its first formal parameter's agnomination
   constituting an ipsissima verba appropration from the
   INTERPRETER-NAME's dation and dispatching on the ``Interpreter''
   type, its second receiving an automatically generated nevening,
   dispatching on an ``eql''-equiparation with the INSTRUCTION, the
   method body's definition being desumed from the BODY forms, concluded
   by a ``(values)'' invocation, which ascertains no return value for
   the method."
  (let ((instruction-name (gensym)))
    (declare (type symbol instruction-name))
    `(defmethod process-instruction
         ((,interpreter-name Interpreter)
          (,instruction-name (eql ,instruction)))
       (declare (type Interpreter ,interpreter-name)
                (ignorable        ,interpreter-name))
       (declare (type instruction ,instruction-name)
                (ignore           ,instruction-name))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-instruction-processor :increment-by-one (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (incf (current-cell-value tape))))

;;; -------------------------------------------------------

(define-instruction-processor :decrement-by-one (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (decf (current-cell-value tape))))

;;; -------------------------------------------------------

(define-instruction-processor :increment-by-ten (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (incf (current-cell-value tape) 10)))

;;; -------------------------------------------------------

(define-instruction-processor :move-right (interpreter)
  (move-cell-pointer-right
    (slot-value interpreter 'tape)))

;;; -------------------------------------------------------

(define-instruction-processor :move-left (interpreter)
  (move-cell-pointer-left
    (slot-value interpreter 'tape)))

;;; -------------------------------------------------------

(define-instruction-processor :output (interpreter)
  (format *standard-output* "~c"
    (code-char
      (current-cell-value
        (slot-value interpreter 'tape)))))

;;; -------------------------------------------------------

(define-instruction-processor :input (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (format        *standard-output* "~&>> ")
    (finish-output *standard-output*)
    (setf (current-cell-value tape)
      (char-code
        (read-char *standard-input* NIL #\Null)))
    (clear-input *standard-input*)))

;;; -------------------------------------------------------

(define-instruction-processor :jump-forward (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (when (zerop (current-cell-value tape))
      (go-to-opposite-jump-point interpreter))))

;;; -------------------------------------------------------

(define-instruction-processor :jump-back (interpreter)
  (with-slots (tape) interpreter
    (declare (type Tape tape))
    (unless (zerop (current-cell-value tape))
      (go-to-opposite-jump-point interpreter))))

;;; -------------------------------------------------------

(defun process-current-instruction (interpreter)
  "Evaluates the INTERPRETER's currently selected instruction and
   returns no value."
  (declare (type Interpreter interpreter))
  (process-instruction interpreter
    (get-current-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the OIIAOIIA program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-completed-p interpreter) do
    (process-current-instruction interpreter)
    (advance-to-next-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-oiiaoiia (code)
  "Interprets the piece of OIIAOIIA source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (prepare-interpreter-for-program
      (parse-oiiaoiia-program code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-character-p (candidate)
  "Determines whether the CANDIDATE represents a symbol affiliated with
   a brainfuck instruction, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "+-<>,.[]" :test #'char=))))

;;; -------------------------------------------------------

(defun remove-non-brainfuck-instructions (source)
  "Returns a fresh simple string retaining merely those characters in
   the SOURCE engaged in an alliance with brainfuck instruction
   identifiers."
  (declare (type string source))
  (the simple-string
    (coerce
      (remove-if-not #'brainfuck-command-character-p source)
      'simple-string)))

;;; -------------------------------------------------------

(defun extract-incrementation-sequence (source start)
  "Proceeding from the START position into the SOURCE, tallies the
   number of immediately accolent brainfuck incrementation behest
   identifiers (\"+\") and returns two values:
     (1) The number of immediately accolent \"+\" symbols.
     (2) The position into the SOURCE immediately succeeding the
         matched tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (integer 0 *) fixnum)
    (let ((end-position-of-sequence
            (or (position #\+ source :start start :test #'char/=)
                (length source))))
      (declare (type fixnum end-position-of-sequence))
      (values
        (- end-position-of-sequence start)
        end-position-of-sequence))))

;;; -------------------------------------------------------

(defun build-incrementation-instructions (number-of-incrementations)
  "Returns for the NUMBER-OF-INCREMENTATIONS, which represents an
   unbroken catena of brainfuck \"+\" operation invocations' tally, a
   list comprehending the ``instruction'' members ``:increment-by-ten''
   and ``:increment-by-one'' in a mete sufficient to replicate the
   incrementation count.
   ---
   As a forbisen, a NUMBER-OF-INCREMENTATIONS equal to 26 can be
   segregated into two tenfold incrementations (20 = 2 * 10) and six
   singular specimens (6 * 1); thus, the following octuple list is
   begotten:
     (:increment-by-ten
      :increment-by-ten
      :increment-by-one
      :increment-by-one
      :increment-by-one
      :increment-by-one
      :increment-by-one
      :increment-by-one)"
  (declare (type (integer 0 *) number-of-incrementations))
  (the (list-of instruction)
    (multiple-value-bind (number-of-decimal-incrementations
                          number-of-orra-incrementations)
        (floor number-of-incrementations 10)
      (declare (type (integer 0 *) number-of-decimal-incrementations))
      (declare (type (integer 0 *) number-of-orra-incrementations))
      (nconc
        (make-list number-of-decimal-incrementations
          :initial-element :increment-by-ten)
        (make-list number-of-orra-incrementations
          :initial-element :increment-by-one)))))

;;; -------------------------------------------------------

(defun parse-incrementation-sequence (source start)
  "Proceeding from the START position into the SOURCE, tallies the
   number of immediately accolent brainfuck incrementation invocations,
   that is, \"+\" symbols, and returns two values:
     (1) A fresh list comprehending the ``instruction'' members
         ``:increment-by-ten'' and ``:increment-by-one'' in a mete
         sufficient to replicate the detected incrementation count.
     (2) The position into the SOURCE immediately succeeding the
         detected incrementation catena's occupied tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (list-of instruction) fixnum)
    (multiple-value-bind (number-of-incrementations new-position)
        (extract-incrementation-sequence source start)
      (declare (type (integer 0 *) number-of-incrementations))
      (declare (type fixnum        new-position))
      (values
        (build-incrementation-instructions number-of-incrementations)
        new-position))))

;;; -------------------------------------------------------

(defun parse-simple-instruction (instruction position)
  "Simulates a single character's parsing and returns two values:
    (1) If the INSTRUCTION is non-``NIL'', a fresh singleton list
        enumerating as its aefauld member the INSTRUCTION itself;
        otherwise, for a ``NIL'' input, the ``NIL'' value.
    (2) The POSITION incremented by one (1), so as to simulate an
        advancement beyond this \"parsed\" token."
  (declare (type (or null instruction) instruction))
  (declare (type fixnum                position))
  (the (values (list-of instruction) fixnum)
    (values
      (list instruction)
      (1+ position))))

;;; -------------------------------------------------------

(defun parse-brainfuck-instruction (source position)
  "Parses the single character or character sequence commencing at the
   POSITION into the SOURCE as a brainfuck instruction and returns two
   values:
     (1) A list of the representative OIIAOIIA commands, each such
         specified as an ``instruction'' object. Please heed that
         non-operative characters produce an empty list.
     (2) The position into the SOURCE immediately succeeding the
         evaluated tmema."
  (declare (type string source))
  (declare (type fixnum position))
  (the (values (list-of instruction) fixnum)
    (case (char source position)
      (#\+       (parse-incrementation-sequence source position))
      (#\-       (parse-simple-instruction :decrement-by-one position))
      (#\>       (parse-simple-instruction :move-right       position))
      (#\<       (parse-simple-instruction :move-left        position))
      (#\.       (parse-simple-instruction :output           position))
      (#\,       (parse-simple-instruction :input            position))
      (#\[       (parse-simple-instruction :jump-forward     position))
      (#\]       (parse-simple-instruction :jump-back        position))
      (otherwise (parse-simple-instruction NIL             position)))))

;;; -------------------------------------------------------

(defun parse-brainfuck-program (code)
  "Parses the piece of brainfuck source CODE and returns an OIIAOIIA
   ``program'' representations of its entailed instructions."
  (declare (type string code))
  (the program
    (let ((uncommented-code (remove-non-brainfuck-instructions code))
          (current-position 0))
      (declare (type simple-string uncommented-code))
      (declare (type fixnum        current-position))
      (make-program
        (loop
          while
            (< current-position
               (length uncommented-code))
          append
            (multiple-value-bind (next-instructions new-position)
                (parse-brainfuck-instruction
                  uncommented-code
                  current-position)
              (declare (type (list-of instruction) next-instructions))
              (declare (type fixnum                new-position))
              (setf current-position new-position)
              next-instructions))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of encoding table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of instruction transition-twain)
               +ENCODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +ENCODING-TABLE+
  (insert-key-value-pairs-into-hash-table
    (make-hash-table :test #'eq)
    :increment-by-one "OO"
    :decrement-by-one "OI"
    :increment-by-ten "OA"
    :move-left        "IO"
    :jump-forward     "II"
    :jump-back        "IA"
    :output           "AO"
    :input            "AI"
    :move-right       "AA")
  "Affiliates with the recognized OIIAOIIA instructions the
   representative two-character transition twains, each such a twissel
   of members desumed from the set {\"A\", \"I\", \"O\"}.")

;;; -------------------------------------------------------

(defun encode-instruction (instruction)
  "Returns for the INSTRUCTION the representative two-character
   transition twain."
  (declare (type instruction instruction))
  (the transition-twain
    (or (gethash instruction +ENCODING-TABLE+)
        (error "Cannot encode the instruction ~s." instruction))))

;;; -------------------------------------------------------

(defun can-transition-into-instruction-p (desired-instruction
                                          current-character)
  "Determines whether the DESIRED-INSTRUCTION can be represented by a
   transition twain whose symbol jumelle's first character equals the
   CURRENT-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type instruction desired-instruction))
  (declare (type character   current-character))
  (the boolean
    (get-boolean-value-of
      (first-character-equals-p
        (encode-instruction desired-instruction)
        current-character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of OIIAOIIA code generator.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-oiiaoiia-code-from-program (program
                                            &key (destination NIL))
  "Generates for the OIIAOIIA PROGRAM's instructions the equivalent
   OIIAOIIA code and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, produces a fresh string comprehending the result."
  (declare (type program     program))
  (declare (type destination destination))
  (if destination
    (let ((last-character NIL))
      (declare (type (or null character) last-character))
      (loop
        for current-instruction
          of-type instruction
          across  program
        for current-transition-twain
          of-type transition-twain
          =       (encode-instruction current-instruction)
        do
          (cond
            ;; First instruction?
            ((null last-character)
              (format destination "~a" current-transition-twain))
            
            ;; CURRENT-INSTRUCTION can be replicated with the
            ;; LAST-CHARACTER?
            ;; => Append the consequent transition character.
            ((can-transition-into-instruction-p current-instruction
                                                last-character)
              (format destination "~c"
                (schar current-transition-twain 1)))
            
            ;; CURRENT-INSTRUCTION cannot be replicated with the
            ;; LAST-CHARACTER?
            ;; => Insert reset and start new transition block.
            (T
              (format destination " ")
              (format destination "~a" current-transition-twain)))
          
          (setf last-character
            (schar current-transition-twain 1))))
    (with-output-to-string (oiiaoiia-code)
      (declare (type string-stream oiiaoiia-code))
      (generate-oiiaoiia-code-from-program program
        :destination oiiaoiia-code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck code generator.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-brainfuck-code-from-program (program
                                             &key (destination NIL))
  "Generates for the OIIAOIIA PROGRAM the equivalent brainfuck code and
   writes thilk to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   produces a fresh string comrpehending the result."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from 0 by 1
        do
          (format destination "~a"
            (case current-instruction
              (:increment-by-one "+")
              (:increment-by-ten "++++++++++")
              (:decrement-by-one "-")
              (:move-right       ">")
              (:move-left        "<")
              (:output           ".")
              (:input            ",")
              (:jump-forward     "[")
              (:jump-back        "]")
              (otherwise
                (error "Invalid instruction ~s at position ~d."
                  current-instruction current-position)))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (generate-brainfuck-code-from-program program
          :destination brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to OIIAOIIA.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-oiiaoiia (brainfuck-code
                                        &key (destination NIL))
  "Converts the piece of BRAINFUCK-CODE into an equivalent OIIAOIIA
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, produces a fresh string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (generate-oiiaoiia-code-from-program
      (parse-brainfuck-program brainfuck-code)
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from OIIAOIIA to brainfuck.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-oiiaoiia-to-brainfuck (oiiaoiia-code
                                        &key (destination NIL))
  "Converts the piece of OIIAOIIA-CODE into an equivalent brainfuck
   program and writes thilk to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, produces a fresh string comprehending the result."
  (declare (type string      oiiaoiia-code))
  (declare (type destination destination))
  (the (or null string)
    (generate-brainfuck-code-from-program
      (parse-oiiaoiia-program oiiaoiia-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!" to the standard output.
(interpret-oiiaoiia
  "OO II OI OI AA OII AAA OO AA OI OI OI OI OIO IO IA IOI OIOI OI OIAA
   OI AO AAAA OO AO AAAO AOOOO II AO AA IA IO IO IO IO AOOOO AOI OI OI
   OI OI OI AO IO IOI AO AAAAA OO AO")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-oiiaoiia "AII AO AIA")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-oiiaoiia
  "AI AO II OI OI AA OO II AAA IA IO II AO IA IO IO IA")

;;; -------------------------------------------------------

;; Translate a repeating cat program from brainfuck to OIIAOIIA.
(translate-brainfuck-to-oiiaoiia ",[.,]")
