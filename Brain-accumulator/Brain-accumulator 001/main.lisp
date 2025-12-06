;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brain-accumulator", invented by the Esolang user "A()" and
;; presented on June 24th, 2025, the vouchsafement of its kenspeckle
;; conception an encoding of Urban Mueller's "brainfuck" in a trisulc
;; of effective behests, their operative cynosure defined in terms of
;; an accumulator whose octuple state contingency accoutres its telos
;; with a bidirectional vinculum to the entheus' equinumerant
;; instruction set.
;; 
;; 
;; Concept
;; =======
;; The Brain-accumulator programming language's designment entails an
;; encoding of brainfuck programs, their operative circumference
;; enumerating a tally of eight (8) members, in a trisulc encoding to
;; whom the virtue of superior compendiousness, without a concomitant
;; eloignment in the faculties' bailiwick, accompts as the dation.
;; 
;; == THE ACCUMULATOR: A CANGEANT CIPHER ==
;; The cynosure's role in the decoding process from a Brain-accumulator
;; provenance to the brainfuck equivalency is inclavated in the former's
;; accumulator, a salvatory of a scalar capacity, the aefault incolant
;; inwith which consigns to the closed integral interval [0, 7].
;; 
;; Its inchoacy's stature the minimum bourne of zero (0), the
;; accumulator's amenability intrines stillatim incrementations,
;; deductions inhabited by the same stipulation, and an actuation's
;; response, whence the storage state is transcripted into a brainfuck
;; instruction. During the arithmetic modulation exercises, upon any of
;; its marches' transgression, the value wraps around to the overthwart
;; extremum.
;; 
;; An esquisse's adduction will serve as a vaunt-courier to the later
;; treatise's amplified nimiety in the correspondences' exposition
;; thilk are informed with the governail over the decoding from
;; Brain-accumulator to brainfuck, and which entertain their
;; manifestation in the "Instructions" section further alow:
;; 
;;   ------------------------------------------------------------------
;;   Accumulator | brainfuck   | Causatum
;;   state       | instruction | 
;;   ------------+-------------+---------------------------------------
;;   0           |      <      | Sinistral cell pointer translation
;;   ..................................................................
;;   1           |      >      | Dextral cell pointer translation
;;   ..................................................................
;;   2           |      +      | Current cell incrementation
;;   ..................................................................
;;   3           |      -      | Current cell decrementation
;;   ..................................................................
;;   4           |      [      | Conditional forward jump
;;   ..................................................................
;;   5           |      ]      | Conditional backward jump
;;   ..................................................................
;;   6           |      .      | Current cell output
;;   ..................................................................
;;   7           |      ,      | Input into current cell
;;   ------------------------------------------------------------------
;; 
;; == THE DECODING PROCESS ==
;; An purlicue concerning a Brain-accumulator program's transcription
;; to the encoded brainfuck tantamount shall accompt for the following
;; pseudocode formulation:
;; 
;;   let accumulator   <- 0
;;   let brainfuckCode <- create an empty string
;;   
;;   for character currentToken in the Brain-accumulator code do
;;     if currentToken = "+" then
;;       accumulator <- (accumulator + 1) modulo 8
;;     else if currentToken = "-" then
;;       accumulator <- (accumulator - 1) modulo 8
;;     else if currentToken = "*" then
;;       if accumulator = 0 then
;;         append "<" to brainfuckCode
;;       else if accumulator = 1 then
;;         append ">" to brainfuckCode
;;       else if accumulator = 2 then
;;         append "+" to brainfuckCode
;;       else if accumulator = 3 then
;;         append "-" to brainfuckCode
;;       else if accumulator = 4 then
;;         append "[" to brainfuckCode
;;       else if accumulator = 5 then
;;         append "]" to brainfuckCode
;;       else if accumulator = 6 then
;;         append "." to brainfuckCode
;;       else if accumulator = 7 then
;;         append "," to brainfuckCode
;;       else
;;         error: invalid accumulator state
;;       end if
;;     else
;;       ignore the currentToken
;;     end if
;;   end for
;; 
;; == NON-OPERATIVE TOKENS ARE NEGLECTED ==
;; Any content whose participation wists of no compernage in an
;; epiphenomenal contribution constitutes an adiaphoron's owelty to the
;; syntaxis, recluding in its ultimity the incorporation of commentary
;; tmemata in the source code.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; A grateful recipient of its stockfather's cleronomy,
;; Brain-accumulator's memory adheres to the acquainted ipsissima verba
;; appropriation of brainfuck's data castaldy notion in a bilaterally
;; bourneless dispansion of unsigned byte-valued cells, apposted in a
;; seriatim ordonnance.
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
;; Brain-accumulator's instruction set, in its construe receiving the
;; highest mete of comprehensiveness' dation, establishes a proprium of
;; bifidity, the first moeity, latreutical in the wike of the encoding,
;; and its tally diminished to a triplet accompt, wones in the
;; compernage of the second component, a tantamount to the decoded
;; brainfuck's octuple diorism.
;; 
;; == OVERVIEW ==
;; A twifold exercise of one's conspectuity ought to be exerted upon the
;; operative aspect commorant in the Brain-accumulator language;
;; imprimis, the encoded guise, enumerating a treble cardinality in the
;; instruction set; and, as a parhedral consectary, its decoded format,
;; the octuple brainfuck operation diorism.
;; 
;; The componency's paravaunt and kenspeckle moeity, the indicial
;; trisulc Brain-accumulator behests, shall be the incipial treatise's
;; hyle:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the accumulator value by a quantity of
;;           | one (1). Upon a trangression of the upper bourne of
;;           | seven (7), the state wraps around to the minimum of zero
;;           | (0).
;;   ..................................................................
;;   -       | Decrements the accumulator value by a quantity of
;;           | one (1). Upon a trangression of the lower bourne of
;;           | zero (0), the state wraps around to the maximum of seven
;;           | (7).
;;   ..................................................................
;;   *       | Generates from the accumulator state's octuple
;;           | contingency the brainfuck instruction according to the
;;           | following equiparation:
;;           | 
;;           |   ------------------------------------------
;;           |   Accumulator state | brainfuck instruction
;;           |   ------------------+-----------------------
;;           |   0                 | <
;;           |   ..........................................
;;           |   1                 | >
;;           |   ..........................................
;;           |   2                 | +
;;           |   ..........................................
;;           |   3                 | -
;;           |   ..........................................
;;           |   4                 | [
;;           |   ..........................................
;;           |   5                 | ]
;;           |   ..........................................
;;           |   6                 | .
;;           |   ..........................................
;;           |   7                 | ,
;;           |   ------------------------------------------
;;   ------------------------------------------------------------------
;; 
;; The sequela affording the gendrure from the aboon scheme's
;; application, whose product comprises the brainfuck instructions,
;; will be the following description's subject.
;; 
;; The spatial constraints imposed by the documentation's compass
;; serves as certain abbreviations' provenance; the legends to the
;; compendiousness' warklumes, as well as any other file's caption,
;; shall be explicated in the next tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Column title | Interpretation
;;   -------------+----------------------------------------------------
;;   Acc.         | Abbreviation for "accumulator state", the integral
;;                | value stored in the Brain-accumulator accumulator.
;;   ..................................................................
;;   bf           | The brainfuck instruction identifier associated
;;                | with the accumulator state "Acc.".
;;   ..................................................................
;;   Effect       | The causatum commorant in the brainfuck instruction
;;                | represented in "bf".
;;   ------------------------------------------------------------------
;; 
;; Ensuing from the aboon elucidations, the correspondences betwixt the
;; Brain-accumulator accumulator states and the octuple brainfuck
;; operations shall be the following diorisms' cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Acc. | bf | Effect
;;   -----+----+-------------------------------------------------------
;;   0    | <  | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   1    | >  | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   2    | +  | Increments the current cell value by one (1). If the
;;        |    | new cell state transcends the admissible upper march
;;        |    | of 255, the value wraps around to the minimum of zero
;;        |    | (0).
;;   ..................................................................
;;   3    | -  | Decrements the current cell value by one (1). If the
;;        |    | new cell state transcends the admissible lower march
;;        |    | of zero (0), the value wraps around to the maximum of
;;        |    | 255.
;;   ..................................................................
;;   4    | [  | If the current cell value equals zero (0), moves the
;;        |    | instruction pointer (IP) forward to the matching "]"
;;        |    | token; otherwise proceeds as usual.
;;   ..................................................................
;;   5    | ]  | If the current cell value does not equal zero (0),
;;        |    | moves the instruction pointer (IP) backward to the
;;        |    | matching "[" token; otherwise proceeds as usual.
;;   ..................................................................
;;   6    | .  | Prints the character whose ASCII code corresponds to
;;        |    | the current cell value to the standard output conduit.
;;   ..................................................................
;;   7    | ,  | Queries the standard input conduit for a character and
;;        |    | stores its ASCII code in the current cell.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been actuated in the programming
;; language Common Lisp, the focus of its angariation the direct
;; evaluation of the Brain-accumulator, whence ensues a transcription
;; into a tantamount brainfuck program, the effort thus translating
;; into a siclike immediacy in the decoded content's consumption.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-02
;; 
;; Sources:
;;   [esolang2025Brain-accumulator]
;;   The Esolang contributors, "Brain-accumulator", June 24th, 2025
;;   URL: "https://esolangs.org/wiki/Brain-accumulator"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the types.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype accumulator-state ()
  "The ``accumulator-state'' type defines the valid Brain-accumulator
   accumulator state as an integral number in the closed interval
   [0, 7]."
  '(integer 0 7))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of whose parcery enumerates, without the capcitation for
   the diorism's exhaustion, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a singly linked list whose elements
   in their entirety partake of the ELEMENT-TYPE's compliance, for the
   same is imposed the generic sentinel ``*'' as the default's
   governail."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type' *))
  "The ``hash-table-of'' type defines a hash table whose componency's
   edification constitutes an inclavation upon zero or more entries,
   their keys complying with the KEY-TYPE, while the allied values
   subsume into the VALUE-TYPE, for both is imposed the nomothesy of
   the generic sentinel ``*'' as the default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype encoding-policy ()
  "The ``encoding-policy'' type enumerates the admissible variants on
   schemes according to which, during the translation of a brainfuck
   instruction to a Brain-accumulator command sequence, the latter's
   accumulator from its current to a desiderated new state shall be
   modulated.
   ---
   The quadruple contingency for the encoding process' actuation
   subsumes these diorisms:
     ------------------------------------------------------------------
     Policy                  | Causatum
     ------------------------+-----------------------------------------
     :reset-and-increment    | Issues a sufficient accompt of \"-\"
                             | instructions in order to reset the
                             | accumulator state to zero (0), ere
                             | incrementing via \"+\" symbols to the
                             | desideratum.
     ..................................................................
     :increment-only         | Constantly increments the accumulator
                             | via \"+\" instances, utilizing its
                             | wrapping deportment, until the current
                             | state segues into the desiderated one.
     ..................................................................
     :increment-or-decrement | If the optated accumulator state is
                             | greater than the current one, issues
                             | only \"+\" behests; in the overthwart
                             | case, adheres to \"-\" tokens.
     ..................................................................
     :economic               | Juxtaposes the tally of transition steps
                             | along each axis necessary to reach the
                             | target accumulator state from the
                             | current one, selects the smallest
                             | choice, and issues the requisite
                             | exclusive \"+\" or \"-\" catena.
     ------------------------------------------------------------------"
  '(member
    :reset-and-increment
    :increment-only
    :increment-or-decrement
    :economic))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   jump points in a brainfuck program, mediated by adminiculum of their
   zero-based positions into the source code string, and manifested in
   a hash table whose keys and values both assume ``fixnum'' indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value whose conformation
   lays its amplection around eight (8) accolent bits, thus covering
   the closed integral interval of [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Brain-accumulator memory as a sparse
   vector of unsigned byte-valued cells, amenable to addresses which
   represent the incolants of the bourneless signed integer vale, and
   being realized as a hash table whose integer keys furnish the indices
   into the allied values thilk accoutre the ``octet''-valued cell
   states."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the accumulator.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Accumulator
  (:constructor prepare-a-pristine-accumulator ()))
  "The ``Accumulator'' class serves in the ensconcement of a
   Brain-accumulator's aefauld salvatory, the accumulator, to whom the
   encoding of a brainfuck instruction sequence registers the
   vouchsafed parcery."
  (value 0 :type accumulator-state :read-only NIL))

;;; -------------------------------------------------------

(defun increment-the-accumulator (accumulator)
  "Increments the ACCUMULATOR's state by a quantity of one (1),
   contingently wrapping around along a violated upper bourne, and
   returns no value."
  (declare (type Accumulator accumulator))
  (setf (accumulator-value accumulator)
    (mod
      (1+ (accumulator-value accumulator))
      8))
  (values))

;;; -------------------------------------------------------

(defun decrement-the-accumulator (accumulator)
  "Decrements the ACCUMULATOR's state by a quantity of one (1),
   contingently wrapping around along a violated lower bourne, and
   returns no value."
  (declare (type Accumulator accumulator))
  (setf (accumulator-value accumulator)
    (mod
      (1- (accumulator-value accumulator))
      8))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the converter to brainfuck.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-the-Brain-accumulator-state (accumulator-state)
  "Returns the brainfuck instruction symbol corresponding with the
   Brain-accumulator ACCUMULATOR-STATE."
  (declare (type accumulator-state accumulator-state))
  (the standard-char
    (case accumulator-state
      (0         #\>)
      (1         #\<)
      (2         #\+)
      (3         #\-)
      (4         #\[)
      (5         #\])
      (6         #\.)
      (7         #\,)
      (otherwise
        (error "Invalid accumulator state: ~d." accumulator-state)))))

;;; -------------------------------------------------------

(defun decode-the-Brain-accumulator (accumulator)
  "Returns the brainfuck instruction symbol corresponding to the
   Brain-accumulator ensconced in the ACCUMULATOR object."
  (declare (type Accumulator accumulator))
  (the standard-char
    (decode-the-Brain-accumulator-state
      (accumulator-value accumulator))))

;;; -------------------------------------------------------

(defun decode-the-Brain-accumulator-code (brain-accumulator-code
                                          &key (destination NIL))
  "Decodes the piece of BRAIN-ACCUMULATOR-CODE into the tantamount
   brainfuck program, writes thilk to the DESTINATION, and returns for
   a non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, produces a fresh string comprehending the output."
  (declare (type string      brain-accumulator-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((accumulator (prepare-a-pristine-accumulator)))
        (declare (type Accumulator accumulator))
        (loop
          for current-token
            of-type character
            across  brain-accumulator-code
          do
            (case current-token
              (#\+
                (increment-the-accumulator accumulator))
              (#\-
                (decrement-the-accumulator accumulator))
              (#\*
                (format destination "~c"
                  (decode-the-brain-accumulator accumulator)))
              (otherwise
                NIL))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (decode-the-Brain-accumulator-code
          brain-accumulator-code
          :destination brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the converter to Brain-accumulator.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode-the-brainfuck-symbol (brainfuck-symbol)
  "Returns the Brain-accumulator accumulator state corresponding to the
   BRAINFUCK-SYMBOL designating one of its octuple operations."
  (declare (type standard-char brainfuck-symbol))
  (the accumulator-state
    (case brainfuck-symbol
      (#\<       0)
      (#\>       1)
      (#\+       2)
      (#\-       3)
      (#\[       4)
      (#\]       5)
      (#\.       6)
      (#\,       7)
      (otherwise
        (error "No brainfuck instruction symbol \"~c\"."
          brainfuck-symbol)))))

;;; -------------------------------------------------------

(defun print-several-times (content number-of-repetitions destination)
  "Prints the CONTENT the NUMBER-OF-REPETITIONS tally of times to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value; otherwise, for a ``NIL'' DESTINATION, responds with a fresh
   string comprehending the output."
  (declare (type T             content))
  (declare (type (integer 0 *) number-of-repetitions))
  (declare (type destination   destination))
  (the (or null string)
    (format destination "~v@{~a~:*~}" number-of-repetitions content)))

;;; -------------------------------------------------------

(defun count-the-accumulator-transition-steps (current-state
                                               target-state)
  "Supputates the requisite transitioning betwixt from the accumulator's
   CURRENT-STATE to the TARGET-STATE and returns two values:
     (1) The tally of incrementation steps, or \"+\" behests, necessary
         to reach the TARGET-STATE from the CURRENT-STATE.
     (2) The tally of decrementation steps, or \"-\" behests, necessary
         to reach the TARGET-STATE from the CURRENT-STATE."
  (declare (type accumulator-state current-state))
  (declare (type accumulator-state target-state))
  (the (values (integer 0 7) (integer 0 7))
    (cond
      ((< current-state target-state)
        (values
          (- target-state current-state)
          (+ current-state (- 8 target-state))))
      ((> current-state target-state)
        (values
          (+ target-state (- 8 current-state))
          (- current-state target-state)))
      (T
        (values 0 0)))))

;;; -------------------------------------------------------

(defgeneric replicate-the-brainfuck-instruction (encoding-policy
                                                 target-state
                                                 current-state
                                                 destination)
  (:documentation
    "Depending on the ENCODING-POLICY, issues the requisite accumulator
     incrementation (\"+\") and/or decrementation (\"-\") behests to
     the DESTINATION in order to transition from the accumulator's
     CURRENT-STATE to the TARGET-STATE, and returns no value.")
  
  (:method ((encoding-policy (eql :reset-and-increment))
            (target-state    integer)
            (current-state   integer)
            (destination     T))
    (declare (type encoding-policy   encoding-policy))
    (declare (type accumulator-state target-state))
    (declare (type accumulator-state current-state))
    (declare (type destination       destination))
    (print-several-times #\- current-state destination)
    (print-several-times #\+ target-state  destination)
    (values))
  
  (:method ((encoding-policy (eql :increment-only))
            (target-state    integer)
            (current-state   integer)
            (destination     T))
    (declare (type encoding-policy   encoding-policy))
    (declare (type accumulator-state target-state))
    (declare (type accumulator-state current-state))
    (declare (type destination       destination))
    (print-several-times #\+
      (count-the-accumulator-transition-steps
        current-state
        target-state)
      destination)
    (values))
  
  (:method ((encoding-policy (eql :increment-or-decrement))
            (target-state    integer)
            (current-state   integer)
            (destination     T))
    (declare (type encoding-policy   encoding-policy))
    (declare (type accumulator-state target-state))
    (declare (type accumulator-state current-state))
    (declare (type destination       destination))
    (cond
      ((< current-state target-state)
        (print-several-times
          #\+
          (- target-state current-state)
          destination))
      ((> current-state target-state)
        (print-several-times
          #\-
          (- current-state target-state)
          destination))
      (T
        NIL))
    (values))
  
  (:method ((encoding-policy (eql :economic))
            (target-state    integer)
            (current-state   integer)
            (destination     T))
    (declare (type encoding-policy   encoding-policy))
    (declare (type accumulator-state target-state))
    (declare (type accumulator-state current-state))
    (declare (type destination       destination))
    (multiple-value-bind
        (number-of-necessary-incrementations
         number-of-necessary-decrementations)
      (count-the-accumulator-transition-steps
        current-state
        target-state)
      (declare (type (integer 0 7) number-of-necessary-incrementations))
      (declare (type (integer 0 7) number-of-necessary-decrementations))
      (if (< number-of-necessary-incrementations
             number-of-necessary-decrementations)
        (print-several-times
          #\+
          number-of-necessary-incrementations
          destination)
        (print-several-times
          #\-
          number-of-necessary-decrementations
          destination))
      (values))))

;;; -------------------------------------------------------

(defun brainfuck-instruction-p (candidate)
  "Determines whether the CANDIDATE represents a member of the octuple
   brainfuck instruction identifiers, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (find candidate "<>+-[].," :test #'char=))))

;;; -------------------------------------------------------

(defun encode-the-brainfuck-code
    (brainfuck-code
     &key (policy      :reset-and-increment)
          (destination NIL))
  "Generates the Brain-accumulator program limning a tantamount to the
   piece of BRAINFUCK-CODE, the entelechia the deployed encoding
   POLICY's consectary, and writes the result to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh string comprehending
   the output."
  (declare (type string          brainfuck-code))
  (declare (type encoding-policy policy))
  (declare (type destination     destination))
  (the (or null string)
    (if destination
      (let ((current-accumulator 0))
        (declare (type accumulator-state current-accumulator))
        (loop
          for current-token
            of-type character
            across  brainfuck-code
          when (brainfuck-instruction-p current-token) do
            (let ((new-accumulator-state
                    (encode-the-brainfuck-symbol current-token)))
              (declare (type accumulator-state new-accumulator-state))
              (replicate-the-brainfuck-instruction
                policy
                new-accumulator-state
                current-accumulator
                destination)
              (setf current-accumulator new-accumulator-state))
            (format destination "*")
          finally
            (return NIL)))
      (with-output-to-string (brain-accumulator-code)
        (declare (type string-stream brain-accumulator-code))
        (encode-the-brainfuck-code brainfuck-code
          :policy      policy
          :destination brain-accumulator-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-jump-table ()
  "Creates and returns a ``jump-table'' whose state at its inchoacy
   ostends a perfect vacancy."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Associates the jump START-POINT with the END-POINT in a bidirectional
   fashion, stores the twissel in the jump TABLE, and returns no value."
  (declare (type jump-table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point table) end-point
    (gethash end-point   table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (brainfuck-code)
  "Creates and returns a fresh ``jump-table'' which associates the
   jumelles of jump points in the piece of BRAINFUCK-CODE by adminiculum
   of their zero-based positions in the source code."
  (declare (type string brainfuck-code))
  (let ((jump-table   (prepare-an-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-token    of-type character across brainfuck-code
      and current-position of-type fixnum    from   0 by 1
      do
        (case current-token
          (#\[
            (push current-position start-points))
          (#\]
            (if start-points
              (connect-the-jump-points
                jump-table
                (pop start-points)
                current-position)
              (error "Unmatched back jump point at position ~d."
                current-position)))
          (otherwise
            NIL))
      finally
        (when start-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points)
            start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun locate-the-jump-destination (jump-table point-of-origin)
  "Returns the obverse jump point associated with the POINT-OF-ORIGIN
   in the JUMP-TABLE; or, upon its disrespondency, signals an error of
   an unspecified type."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-origin))
  (the fixnum
    (or (gethash point-of-origin jump-table)
        (error "No destination jump point is associated with the ~
                position ~d."
          point-of-origin))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``memory'' instance whose state at this
   point of inchoacy resort to the default of zero-valued cells."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun cell-value-at (memory address)
  "Returns the byte value stored in the MEMORY cell amenable to the
   ADDRESS."
  (declare (type memory  memory))
  (declare (type integer address))
  (the octet
    (gethash address memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the ADDRESS,
   contingently yarked by a wrapping into the admissible unsigned byte
   range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer address))
  (setf (gethash address memory 0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type character +NULL-CHARACTER+))

;;; -------------------------------------------------------

(defconstant +NULL-CHARACTER+
  (code-char 0)
  "Represents the \"null character\", associated with the ASCII code of
   zero (0), in an implementation-indepedent fashion.")

;;; -------------------------------------------------------

(defun interpret-the-brainfuck-code (brainfuck-code)
  "Interprets the piece of BRAINFUCK-CODE and returns no value."
  (declare (type string brainfuck-code))
  (let ((ip           0)
        (jump-table   (supputate-the-jump-table-for brainfuck-code))
        (memory       (prepare-a-pristine-memory))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type memory     memory))
    (declare (type integer    cell-pointer))
    (symbol-macrolet
        ((current-cell-value
          (the (or octet integer)
            (cell-value-at memory cell-pointer)))
         (current-token
          (the character
            (char brainfuck-code ip)))
         (program-is-exhausted-p
          (the boolean
            (not
              (array-in-bounds-p brainfuck-code ip)))))
      (declare (type (or octet integer) current-cell-value))
      (declare (type character          current-token))
      (declare (type boolean            program-is-exhausted-p))
      (loop until program-is-exhausted-p do
        (case current-token
          (#\<
            (decf cell-pointer))
          (#\>
            (incf cell-pointer))
          (#\+
            (incf current-cell-value))
          (#\-
            (decf current-cell-value))
          (#\[
            (when (zerop current-cell-value)
              (setf ip
                (locate-the-jump-destination jump-table ip))))
          (#\]
            (unless (zerop current-cell-value)
              (setf ip
                (locate-the-jump-destination jump-table ip))))
          (#\.
            (format T "~c"
              (code-char current-cell-value)))
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf current-cell-value
              (char-code
                (read-char NIL NIL +NULL-CHARACTER+)))
            (clear-input))
          (otherwise
            NIL))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-Brain-accumulator-code (code)
  "Interprets the piece of Brain-accumulator source CODE and returns no
   value."
  (declare (type string code))
  (interpret-the-brainfuck-code
    (decode-the-Brain-accumulator-code code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-Brain-accumulator-code
  "+++++++*-------++++*----++++++*------+++++++*-------+++++*")

;;; -------------------------------------------------------

;; A more compendious variation on the aboon repeating cat program.
(interpret-the-Brain-accumulator-code
  "-*---*++*+*--*")

;;; -------------------------------------------------------

;; Truth-machine:
(interpret-the-Brain-accumulator-code
  "+++++++*-*--*-**--*+*++*---**++++*-----*++++*++*-*-----**+++++*")

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the repeating
;; brainfuck cat program as:
;; 
;;   +++++++*-------++++*----++++++*------+++++++*-------+++++*
(encode-the-brainfuck-code ",[.,]" :policy :reset-and-increment)

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the repeating
;; brainfuck cat program as:
;; 
;;   +++++++*---*++*+*--*
(encode-the-brainfuck-code ",[.,]" :policy :increment-or-decrement)

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the repeating
;; brainfuck cat program as:
;; 
;;   +++++++*+++++*++*+*++++++*
(encode-the-brainfuck-code ",[.,]" :policy :increment-only)

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the repeating
;; brainfuck cat program as:
;; 
;;   -*---*++*+*--*
(encode-the-brainfuck-code ",[.,]" :policy :economic)

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the brainfuck
;; truth-machine:
;; 
;;   +++++++*-*--*-**--*+*++*---**++++*-----*++++*++*-*-----**+++++*
(encode-the-brainfuck-code ",.[-->+[>>]<[.]<<]"
  :policy :increment-or-decrement)

;;; -------------------------------------------------------

;; Generate the Brain-accumulator equivalency to the brainfuck
;; truth-machine:
;; 
;;   -*-*--*-**--*+*++*---**----*+++*----*++*-*+++**---*
(encode-the-brainfuck-code ",.[-->+[>>]<[.]<<]" :policy :economic)
