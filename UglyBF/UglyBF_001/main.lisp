;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "UglyBF", invented by the Esolang user "92.148.109.67",
;; founded as a derivative of Urban Mueller's "brainfuck", and pursuing
;; the reduction of the source code by the introduction of two
;; meta-commands, one curtailing the instruction set's cardinality by an
;; inversion switch, the other permitting the repetition of operations.
;; 
;; Concept
;; =======
;; UglyBF's contribution to the realm of brainfuck derivatives
;; comprehends a curtailment of the inherited instructions to four
;; acquainted tokens, duplicated in their capabilities by an inversion
;; mode, which, in coefficiency with a repetitions operation, imparts a
;; holophrastic nature to the programs.
;; 
;; == UGLYBF: COMPACTNESS APPLIED TO BRAINFUCK ==
;; UglyBF proceeds from the strain of its entheus, brainfuck, however,
;; engaged in a cambistry of the stock-father's explicitness for an
;; intrinsically operating mode capable of reproducing the eight
;; facilities by a moeity in the quantity of tokens. Two additional
;; operations, exerting an influence as meta-commands, contribute to the
;; augmented compactness: an inversion mode that resorts to a
;; bidirectional construe of a command as its own counterpart, and a
;; repetition facility, capable of replicating an operation multiple
;; times.
;; 
;; == INVERSION: ONE SWITCH, ONE COMMAND, TWO EFFECTS ==
;; The bisection of UglyBF's effective instruction arsenal, from
;; brainfuck's octuple
;;   + - > < . , [ ]
;; to the legatee's
;;   +   >   .   [
;; founds upon the twifaced potential of these quadruple actors.
;; 
;; The claviger to this malleable deportment is realized in the
;; inversion operation, signified by the backslash character "\". Its
;; occurrence in a program temporarily activates the inversion mode,
;; which endures until the confrontation with a compatible instruction,
;; encompassing the four members enumerated aboon. Any encounter with a
;; kindred switches the mode; upon collision with the duplication
;; instruction "*" no effect applies, and the mode continues forward.
;; 
;; == DUPLICATION: REPETITION BY DOUBLING ==
;; A second warklume of UglyBF's enhanced compactness, the doubling or
;; duplication operator, manifested in the asterisk "*", induces a
;; twofold augmentation of the repetition count to be applied to the
;; next command. Any instruction implicitly partakes in a program by an
;; exertion of its operational duty with a factor of one (1), that is,
;; once. Proceeding from this cladestine attribute inherent in any
;; command token, an augmenting manipulation thereof would reverberate
;; in a multiplied application of its effect.
;; 
;; The duplication operation's onus resolves to this exact duty: Upon
;; its encounter the next command's effect is doubled. A consanguineous
;; junction doubles the current factor without resetting it; solely if
;; applied to one of the five distinguished members of the instruction
;; set, the multiplication takes effect, subsequently defaulting to the
;; initial value of one (1).
;; 
;; 
;; Architecture
;; ============
;; UglyBF's cleronomy, of course, propagates through its architectural
;; department, which, in its most liberal construe, deploys a tape-like
;; memory of cells, infinite in their sinistral and dextral extent, and
;; each a storage to a single scalar integer of any sign and magnitude.
;; A cell pointer designates at any instant one of these components as
;; the current or active cell, itself endowed with the amenability to
;; stepwise translations.
;; 
;; 
;; Data Types
;; ==========
;; The language's primary data type subscribes to unbounded signed
;; integers, employed in the program's storage and manipulated by the
;; instruction set's preponderance. Only at the interface betwixt
;; machine and user, that is, along the input and output conduits, do
;; characters register as participants.
;; 
;; 
;; Syntax
;; ======
;; A descendant of brainfuck, UglyBF's donat betokens an almost verbatim
;; appropriation: A particular set of single characters is reserved as
;; command names, where any non-instruction constituent may serve --- at
;; any position, even in the operational interstices --- as apostilles.
;; Merely the concrete membership deviates from the origin.
;; 
;; == INSTRUCTIONS ==
;; Each of the sextuple ingredients of the instruction set is
;; represented by a designated character.
;; 
;; == COMMENT ==
;; Any character not donated to the instruction representation
;; experiences tolerance and an interpretation as a descanting entity.
;; 
;; == GRAMMAR ==
;; The syntaxis shall be molded into an Extended Backus-Naur Form (EBNF)
;; by the following description:
;; 
;;   program := { command | comment } ;
;;   comment := character - command ;
;;   command := "+" | ">" | "." | "[" | "\" | "*" ;
;; 
;; 
;; Instructions
;; ============
;; UglyBF's instruction set aspires the foreshortening of its brainfuck
;; equivalent's roster by introduction of an inversion and a duplication
;; operator, the former switches the meaning of any of the four
;; principal capabilities,
;; 
;;   - incrementing/decrementing a cell value,
;;   - moving the cell pointer,
;;   - querying for input or output, and
;;   - conditional jumping,
;; 
;; whereas the latter serves to repeat the next instruction a specific
;; number of times.
;; 
;; == OVERVIEW ==
;; While UglyBF's instruction set partakes of an equinumerant tally when
;; contraposed to its inspirational original, the diacritical element
;; wones in the physical expression of the octuple constituents.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments/Decrements the current cell value by one.
;;           | --------------------------------------------------------
;;           | STANDARD: Increments the current cell's value by one.
;;           | --------------------------------------------------------
;;           | INVERTED: Decrements the current cell's value by one.
;;           | --------------------------------------------------------
;;           | Constitutes an original brainfuck command.
;;   ..................................................................
;;   >       | Moves the cell pointer left or right by one cell.
;;           | --------------------------------------------------------
;;           | STANDARD: Moves the cell pointer one cell to the right.
;;           | --------------------------------------------------------
;;           | INVERTED: Moves the cell pointer one cell to the left.
;;           | --------------------------------------------------------
;;           | Constitutes an original brainfuck command.
;;   ..................................................................
;;   .       | Commits output or input.
;;           | --------------------------------------------------------
;;           | STANDARD: Outputs the ASCII character corresponding to
;;           |           the current cell's value.
;;           | --------------------------------------------------------
;;           | INVERTED: Queries the user for an input character and
;;           |           stores its ASCII code in the current cell.
;;           | --------------------------------------------------------
;;           | Constitutes an original brainfuck command.
;;   ..................................................................
;;   [       | Jumps/Loops based upon the current cell value.
;;           | --------------------------------------------------------
;;           | STANDARD: If the current cell's value equals zero (0),
;;           |           moves forward to the position immediately
;;           |           following the matching inverted "[", that is,
;;           |           an instruction patterned like "\[".
;;           | --------------------------------------------------------
;;           | INVERTED: If the current cell's value does not equal
;;           |           zero (0), moves back to the position
;;           |           immediately following the matching
;;           |           non-inverted "[".
;;           | --------------------------------------------------------
;;           | Constitutes an original brainfuck command.
;;   ..................................................................
;;   *       | Duplicates the next command.
;;           | --------------------------------------------------------
;;           | STANDARD: Doubles the effect of the succeeding command,
;;           |           applying also to instances of "*" itself and
;;           |           to "\". Cannot be inverted by "\", but
;;           |           perpetuates its effect to the last member of
;;           |           the duplication chain, if having been preceded
;;           |           by a "\".
;;           | --------------------------------------------------------
;;           | INVERTED: Exactly the same as STANDARD.
;;   ..................................................................
;;   \       | Inverts the succeeding command.
;;           | --------------------------------------------------------
;;           | STANDARD: Sets the inversion mode to inverted for the
;;           |           next command.
;;           | --------------------------------------------------------
;;           | INVERTED: Sets the inversion mode to standard for the
;;           |           next command.
;;           | --------------------------------------------------------
;;           | If preceding one or more "*", the final doubled command
;;           | is affected, as the "*" operation itself cannot be
;;           | inverted in the sense of experiencing a modified effect;
;;           | instead, it propagates the inversion effect without any
;;           | manipulation.
;;   ------------------------------------------------------------------
;; 
;; == EFFECT OF APPLYING DUPLICATION ("*") ==
;; An adminiculum to the duplication operator's construe, the following
;; table describes its effect upon the same's application.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect by duplication via "*"
;;   --------+---------------------------------------------------------
;;   +       | Becomes "++".
;;   ..................................................................
;;   >       | Becomes ">>".
;;   ..................................................................
;;   .       | Becomes "..".
;;   ..................................................................
;;   [       | Becomes "[[".
;;   ..................................................................
;;   *       | Doubles the currently applying repetition factor.
;;   ..................................................................
;;   \       | None.
;;   ------------------------------------------------------------------
;; 
;; == EFFECT OF APPLYING INVERSION ("\") ==
;; A tabular elucidation shall be steadable as a treatise on the
;; inversion operator's implications.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect by inversion via "\"
;;   --------+---------------------------------------------------------
;;   +       | Becomes "-".
;;   ..................................................................
;;   >       | Becomes "<".
;;   ..................................................................
;;   .       | Becomes ",".
;;   ..................................................................
;;   [       | Becomes "]".
;;   ..................................................................
;;   *       | None itself; but propagates the inversion to the
;;           | desinent command in its chain.
;;   ..................................................................
;;   \       | Inverts the current state, either from normal to
;;           | inverted, or vice versa.
;;   ------------------------------------------------------------------
;; 
;; The inversion command "\" adheres to the following protocol:
;; 
;;   - Its occurrence temporarily sets the inversion mode to inverted.
;;   - Each time an instance of the "\" is encountered, the inversion
;;     mode is switched --- from inverted to standard and vice versa.
;;   - Upon encountering a doubling command "*", the inversion command
;;     does neither apply nor modify the mode; it simply propagates
;;     futher.
;;   - Upon collision with a receptive instruction, which enumerates
;;     "+", ">", ".", and "[", the currently operating inversion mode
;;     determines the concrete action:
;;     
;;       --------------------------------------------------------------
;;       UglyBF command | Inversion mode | Concrete command
;;       ---------------+----------------+-----------------------------
;;       +              | standard       | + (increment)
;;       +              | inverted       | - (decrement)
;;       ..............................................................
;;       >              | standard       | > (move right)
;;       >              | inverted       | < (move left)
;;       ..............................................................
;;       .              | standard       | . (output)
;;       .              | inverted       | , (input)
;;       ..............................................................
;;       [              | standard       | [ (jump forward if zero)
;;       [              | inverted       | ] (jump back if non-zero)
;;       --------------------------------------------------------------
;;     
;;     The inversion mode is subsequently reset to standard.
;; 
;; == COEFFICIENCY OF UGLYBF COMMANDS AND INVERSION MODES ==
;; An educative apercu concerning the vinculum betwixt the two inversion
;; modes and the available UglyBF command tokens shall be produced:
;; 
;;   ----------------------------------------------------------------
;;   UglyBF command | Inversion mode | Concrete command
;;   ---------------+----------------+-------------------------------
;;   +              | standard       | Increment current cell
;;   +              | inverted       | Decrement current cell
;;   ................................................................
;;   >              | standard       | Move cell pointer right
;;   >              | inverted       | Move cell pointer left
;;   ................................................................
;;   .              | standard       | Output cell value as character
;;   .              | inverted       | Input character, store code
;;   ................................................................
;;   [              | standard       | jump forward if zero
;;   [              | inverted       | jump back if non-zero
;;   ----------------------------------------------------------------
;; 
;; == INTERPLAY BETWIXT DUPLICATION "*" AND INVERSION "\" ==
;; It is of utmost importance to agnize the non-commutative nature of
;; the meta-command pair "*" (duplication) and "\" (inversion), which
;; renders the order of their application a significant criterion.
;; 
;; In eath expression, this signifies that
;;   * \
;; is NOT equal to
;;   \ *
;; 
;; In particular, the original UglyBF specification stresses the general
;; approach to multiplying an inverted instruction by preceding the
;; chain of duplication operations ("*") by an odd-numbered tally of
;; inversion commands ("\") --- most optimally, and reasonably, a single
;; specimen suffices.
;; 
;; The rationality behind this ostentatiously diminutive facette, albeit
;; peisant in its ramifications, resides in the fact that the inversion
;; command "\" does not affect the duplication operation "*" at all, and
;; instead simply travels through its occurrences barred from any
;; manipulations, until a token capable of respondency is met.
;; 
;; The duplication command "*", as a counterdistinguishment, very well
;; applies to the inversion "\", duplicating its effect. Yielded as a
;; consectary, an inversion, if committed an even number of times,
;; naturally neutralizes its own effect; for instance, this two
;; operations
;;   \ \
;; are perfectly tantamount to no "\" at all.
;; 
;; Let us consign ourselves to an exemplary elucidation on a twain of
;; cases.
;; 
;; Example 1:
;;   The UglyBF command sequence
;;     \ * +
;;   expands to
;;     * -
;;   and finally produces the brainfuck equivalent
;;     - -
;; 
;; Example 2:
;;   On the other hand, the sequence
;;     * \ +
;;   expands to
;;     \ \ +
;;   and thus yields in brainfuck denomination
;;     +
;; 
;; == COEFFICIENCY OF UGLYBF COMMANDS AND INVERSION MODES ==
;; The following table shall impart some gnarity regarding the UglyBF
;; inversion modes, the command tokens in the same provenance, and the
;; equivalencies to brainfuck's instruction set.
;; 
;;   ------------------------------------------------------
;;   UglyBF command | Inversion mode | brainfuck equivalent
;;   ---------------+----------------+---------------------
;;   +              | standard       | +
;;   +              | inverted       | -
;;   ......................................................
;;   >              | standard       | >
;;   >              | inverted       | <
;;   ......................................................
;;   .              | standard       | .
;;   .              | inverted       | ,
;;   ......................................................
;;   [              | standard       | [
;;   [              | inverted       | ]
;;   ------------------------------------------------------
;; 
;; == EMULATION OF BRAINFUCK IN UGLYBF ==
;; The octuple instruction set of brainfuck is capable of replication in
;; its derivation by the inversion command'S ("\") adminiculum, as shall
;; be the coming illustration's subject:
;; 
;;   brainfuck | UglyBF
;;   ----------+-------
;;   +         | +
;;   -         | \+
;;   >         | >
;;   <         | \>
;;   .         | .
;;   ,         | \.
;;   [         | [
;;   ]         | \[
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The nearly impeccable treatise applied to the language in the
;; original specification renders the same eludible to any dubious
;; perquisitions.
;; 
;; 
;; Implementation
;; ==============
;; This rather convolute implementation in Common Lisp transpiles the
;; UglyBF source code into a sequence of brainfuck instructions, whence
;; a more moderate effort serves in the new form's interpretation.
;; 
;; Several conversion warklumes complement this program's capabilities,
;; in particular such to generate brainfuck code from an UglyBF
;; provenance, and an analogue in the athwart direction.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-10
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/UglyBF"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   entailing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (loop
            for key
              of-type T
              being the hash-keys in (the hash-table object)
            using
              (hash-value value)
            always
              (and (typep key   key-type)
                   (typep value value-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of jump start positions in
   some instruction sequence to their matching end positions, and vice
   versa, manifested in the form of a hash table which represents these
   indices as fixnums."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a linear arrangement of cells compatible
   for the use as an UglyBF program's storage entity in terms of a hash
   table, the keys of which serve as cell indices, associated with the
   cell values, both being unbounded integers."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype uglyBF-command ()
  "The ``uglyBF-command'' enumerates the valid UglyBF instruction
   types."
  '(member
    :inc/dec
    :move
    :io
    :jump
    :double
    :invert))

;;; -------------------------------------------------------

(deftype brainfuck-command ()
  "The ``brainfuck-command'' type enumerates the valid brainfuck
   instruction types."
  '(member
    :increment
    :decrement
    :move-right
    :move-left
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype inversion-mode ()
  "The ``inversion-mode'' type enumerates the possible states for an
   inversion flag."
  '(member :standard :inverted))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number in the range
   [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype remainder ()
  "The ``remainder'' type defines an integer number in the range [0, 1],
   utible specially for the representation of the result yielded by
   searching for the amount of doubling operations in a tally of
   repetitions, which might leave an orra item incapable of covered by
   duplication."
  '(integer 0 1))

;;; -------------------------------------------------------

(deftype bf-command-group ()
  "The ``bf-command-group'' type defines a compound representation of a
   continuous piece of a brainfuck program which comprehends a sequence
   of one or more identical instructions, represented by a cons whose
   left compartment contains the brainfuck command, whereas the second
   moeity tallies the series' length."
  '(cons brainfuck-command positive-integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of UglyBF parser.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uglyBF-command-token-p (token)
  "Checks whether the TOKEN represents an UglyBF command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token "+>.[*\\" :test #'char=)))))

;;; -------------------------------------------------------

(defun get-UglyBF-command-for-token (token)
  "Returns the UglyBF command associated with the TOKEN, or signals an
   error of an unspecified type upon the absence of a correspondence."
  (declare (type character token))
  (the uglyBF-command
    (case token
      (#\+       :inc/dec)
      (#\>       :move)
      (#\[       :jump)
      (#\.       :io)
      (#\*       :double)
      (#\\       :invert)
      (otherwise (error "No command token: ~s." token)))))

;;; -------------------------------------------------------

(defun extract-UglyBF-instructions (code)
  "Extracts and returns from the piece of UglyBF CODE a one-dimensional
   array of instructions."
  (declare (type string code))
  (the (simple-array uglyBF-command (*))
    (coerce
      (loop
        for token of-type character across code
        when (uglyBF-command-token-p token)
          collect (get-UglyBF-command-for-token token))
      '(simple-array uglyBF-command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of UglyBF interpreter.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-brainfuck-command-in-mode (mode uglyBF-command)
  "Returns the brainfuck command corresponding to the UGLYBF-COMMAND in
   the MODE."
  (declare (type inversion-mode mode))
  (declare (type uglyBF-command uglyBF-command))
  (the brainfuck-command
    (case mode
      (:standard
        (case uglyBF-command
          (:inc/dec  :increment)
          (:move     :move-right)
          (:jump     :jump-forward)
          (:io       :output)
          (otherwise (error "Invalid UglyBF command ~s in mode ~s."
                       uglyBF-command mode))))
      (:inverted
        (case uglyBF-command
          (:inc/dec  :decrement)
          (:move     :move-left)
          (:jump     :jump-back)
          (:io       :input)
          (otherwise (error "Invalid UglyBF command ~s in mode ~s."
                       uglyBF-command mode))))
      (otherwise
        (error "Invalid mode ~s." mode)))))

;;; -------------------------------------------------------

(defun convert-UglyBF-instructions-to-brainfuck (instructions)
  "Converts the UglyBF INSTRUCTIONS, defined by meta-commands and
   general command families, to brainfuck-compatible instructions, and
   returns a one-dimensional simple array tasked with their
   comprehension."
  (declare (type (vector uglyBF-command *) instructions))
  
  (let ((brainfuck-instructions NIL))
    (declare (type (list-of brainfuck-command) brainfuck-instructions))
    
    (when (plusp (length instructions))
      (let ((ip             0)
            (uglyBF-command (aref instructions 0))
            (mode           :standard)
            (factor         1))
        (declare (type fixnum                   ip))
        (declare (type (or null uglyBF-command) uglyBF-command))
        (declare (type inversion-mode           mode))
        (declare (type (integer 0 *)            factor))
        
        (labels
            ((advance ()
              "Moves the instruction pointer IP to the next position in
               the INSTRUCTIONS, if possible, updates the current
               UGLYBF-COMMAND, and returns no value."
              (setf uglyBF-command
                (when (array-in-bounds-p instructions (1+ ip))
                  (aref instructions (incf ip))))
              (values))
             
             (invert-mode ()
              "Inverts the inversion MODE and returns no value."
              (setf mode
                (case mode
                  (:standard :inverted)
                  (:inverted :standard)
                  (otherwise
                    (error "Invalid inversion mode: ~s." mode))))
              (values))
             
             (reset-mode ()
              "Resets the inversion MODE to the standard state and
               returns no value."
              (setf mode :standard)
              (values))
             
             (increase-factor ()
              "Doubles the multiplication FACTOR and returns no value."
              (setf factor (* factor 2))
              (values))
             
             (reset-factor ()
              "Resets the multiplication FACTOR and returns no value."
              (setf factor 1)
              (values))
             
             (collect-instruction (new-instruction repetitions)
              "Inserts the NEW-INSTRUCTION a REPETITIONS tally of times
               at the front of the BRAINFUCK-INSTRUCTIONS and returns no
               value."
              (declare (type brainfuck-command new-instruction))
              (declare (type (integer 1 *)     repetitions))
              (loop repeat repetitions do
                (push new-instruction brainfuck-instructions))
              (values)))
          
          (loop while uglyBF-command do
            (case uglyBF-command
              ((NIL)
                (loop-finish))
              
              (:double
                (increase-factor)
                (advance))
              
              (:invert
                (loop repeat factor do
                  (invert-mode))
                (reset-factor)
                (advance))
              
              (:inc/dec
                (collect-instruction
                  (get-brainfuck-command-in-mode mode uglyBF-command)
                  factor)
                (reset-mode)
                (reset-factor)
                (advance))
              
              (:move
                (collect-instruction
                  (get-brainfuck-command-in-mode mode uglyBF-command)
                  factor)
                (reset-mode)
                (reset-factor)
                (advance))
              
              (:io
                (collect-instruction
                  (get-brainfuck-command-in-mode mode uglyBF-command)
                  factor)
                (reset-mode)
                (reset-factor)
                (advance))
              
              (:jump
                (collect-instruction
                  (get-brainfuck-command-in-mode mode uglyBF-command)
                  factor)
                (reset-mode)
                (reset-factor)
                (advance))
              
              (otherwise
                (error "Invalid command family ~s at position ~d."
                  uglyBF-command ip)))))))
    
    (the (simple-array brainfuck-command (*))
      (coerce (nreverse brainfuck-instructions)
        '(simple-array brainfuck-command (*))))))

;;; -------------------------------------------------------

(defun build-jump-table (brainfuck-instructions)
  "Creates and returns a jump table for the BRAINFUCK-INSTRUCTIONS which
   associates each position of a jump start command to the matching end,
   and vice versa."
  (declare (type (vector brainfuck-command *) brainfuck-instructions))
  (let ((jump-table  (make-hash-table :test #'eql))
        (jump-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-starts))
    (loop
      for command
        of-type brainfuck-command
        across  brainfuck-instructions
      and position
        of-type fixnum
        from    0
      do
        (case command
          (:jump-forward
            (push position jump-starts))
          (:jump-back
            (let ((matching-start (pop jump-starts)))
              (declare (type (or null fixnum) matching-start))
              (cond
                (matching-start
                  (setf (gethash matching-start jump-table) position)
                  (setf (gethash position jump-table) matching-start))
                (T
                  (error "Unmatched \"\\]\" instruction at ~
                          position ~d."
                    position)))))
          (otherwise
            NIL))
      finally
        (when jump-starts
          (error "Unterminated \"[\" instructions at ~
                  positions ~{~d~^, ~}."
            jump-starts)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-brainfuck-instructions (brainfuck-instructions)
  "Processes the BRAINFUCK-INSTRUCTIONS and returns no value."
  (declare (type (vector brainfuck-command *) brainfuck-instructions))
  
  (when (plusp (length brainfuck-instructions))
    (let ((ip          0)
          (instruction (aref brainfuck-instructions 0))
          (jump-table  (build-jump-table brainfuck-instructions))
          (memory      (make-hash-table :test #'eql))
          (pointer     0))
      (declare (type fixnum                      ip))
      (declare (type (or null brainfuck-command) instruction))
      (declare (type jump-table                  jump-table))
      (declare (type memory                      memory))
      (declare (type integer                     pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the BRAINFUCK-INSTRUCTIONS, updates the current
             INSTRUCTION, and returns no value."
            (setf instruction
              (when (array-in-bounds-p brainfuck-instructions (1+ ip))
                (aref brainfuck-instructions (incf ip))))
            (values))
           
           (jump-to-opposite-point ()
            "Expecting to be on a jump instruction, relocates the
             instruction pointer IP to the position of the matching jump
             start or end, updates the current INSTRUCTION, and returns
             no value."
            (setf ip (gethash ip jump-table))
            (setf instruction
              (when (array-in-bounds-p brainfuck-instructions ip)
                (aref brainfuck-instructions ip)))
            (values))
           
           (current-cell ()
            "Returns the current cell's value."
            (the integer (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell and returns no
             value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while instruction do
          (case instruction
            (:increment
              (incf (current-cell)))
            
            (:decrement
              (decf (current-cell)))
            
            (:move-right
              (incf pointer))
            
            (:move-left
              (decf pointer))
            
            (:input
              (format T "~&Please input an ASCII character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (current-cell) (char-code input))))
            
            (:output
              (write-char (code-char (current-cell))))
            
            (:jump-forward
              (when (zerop (current-cell))
                (jump-to-opposite-point)))
            
            (:jump-back
              (unless (zerop (current-cell))
                (jump-to-opposite-point)))
            
            (otherwise
              (error "Invalid brainfuck instruction ~s at position ~d."
                instruction ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun process-UglyBF-instructions (uglyBF-instructions)
  "Processes the UGLYBF-INSTRUCTIONS and returns no value."
  (declare (type (vector uglyBF-command *) uglyBF-instructions))
  (process-brainfuck-instructions
    (convert-UglyBF-instructions-to-brainfuck uglyBF-instructions))
  (values))

;;; -------------------------------------------------------

(defun interpret-UglyBF (code)
  "Interprets the piece of UglyBF CODE and returns no value."
  (declare (type string code))
  (process-UglyBF-instructions
    (extract-UglyBF-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of UglyBF code generator.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-UglyBF-token-for-command (uglyBF-command)
  "Returns the character associated with the UGLYBF-COMMAND, or signals
   an error of an unspecified type if no affiliation exists."
  (declare (type uglyBF-command uglyBF-command))
  (the character
    (case uglyBF-command
      (:inc/dec #\+)
      (:move    #\>)
      (:io      #\.)
      (:jump    #\[)
      (:invert  #\\)
      (:double  #\*)
      (otherwise (error "Invalid UglyBF command: ~s."
                   uglyBF-command)))))

;;; -------------------------------------------------------

(defun generate-UglyBF-code (uglyBF-instructions
                             &key (destination NIL))
  "Generates the piece of UglyBF source code equivalent to the
   UGLYBF-INSTRUCTIONS and writes it to the DESTINATION, and returns for
   a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responds with
   a fresh string containing the output."
  (declare (type (vector uglyBF-command *) uglyBF-instructions))
  (declare (type destination               destination))
  (the (or null string)
    (if destination
      (loop
        for command of-type uglyBF-command across uglyBF-instructions
        do
          (format destination "~c"
            (get-UglyBF-token-for-command command)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-UglyBF-code uglyBF-instructions
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-token-p (token)
  "Checks whether the TOKEN represents a brainfuck command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token "+-<>.,[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun get-brainfuck-command-for-token (token)
  "Returns the brainfuck command associated with the TOKEN, or signals
   an error upon a lack of correspondence."
  (declare (type character token))
  (the brainfuck-command
    (case token
      (#\+ :increment)
      (#\- :decrement)
      (#\< :move-left)
      (#\> :move-right)
      (#\, :input)
      (#\. :output)
      (#\[ :jump-forward)
      (#\] :jump-back)
      (otherwise
        (error "Invalid brainfuck command token: ~s." token)))))

;;; -------------------------------------------------------

(defun extract-brainfuck-instructions (brainfuck-code)
  "Extracts from the piece of BRAINFUCK-CODE its instructions and
   returns these in a one-dimensional simple array."
  (declare (type string brainfuck-code))
  (the (simple-array brainfuck-command (*))
    (coerce
      (loop
        for token of-type character across brainfuck-code
        when (brainfuck-command-token-p token)
          collect (get-brainfuck-command-for-token token))
      '(simple-array brainfuck-command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck code generator.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-brainfuck-token-for-command (brainfuck-command)
  "Returns the character associated with the BRAINFUCK-COMMAND, or
   signals an error of an unspecified type if no affiliation is
   present."
  (declare (type brainfuck-command brainfuck-command))
  (the character
    (case brainfuck-command
      (:increment    #\+)
      (:decrement    #\-)
      (:move-left    #\<)
      (:move-right   #\>)
      (:input        #\,)
      (:output       #\.)
      (:jump-forward #\[)
      (:jump-back    #\])
      (otherwise     (error "No brainfuck command: ~s."
                       brainfuck-command)))))

;;; -------------------------------------------------------

(defun generate-brainfuck-code (brainfuck-instructions
                                &key (destination NIL))
  "Generates the piece of brainfuck source code equivalent to the
   BRAINFUCK-INSTRUCTIONS and writes it to the DESTINATION, returning
   ``NIL'' for a non-``NIL'' DESTINATION, otherwise responding with a
   fresh string comprehending the output."
  (declare (type (vector brainfuck-command *) brainfuck-instructions))
  (declare (type destination                  destination))
  (the (or null string)
    (if destination
      (loop
        for instruction
          of-type brainfuck-command
          across  brainfuck-instructions
        do
          (format destination "~c"
            (get-brainfuck-token-for-command instruction)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-brainfuck-code brainfuck-instructions
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of UglyBF-to-brainfuck converter.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-UglyBF-code-to-brainfuck (uglyBF-code
                                         &key (destination NIL))
  "Converts the piece of UGLYBF-CODE into an equivalent brainfuck
   program and writes the result to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the output."
  (declare (type string      uglyBF-code))
  (declare (type destination destination))
  (the (or null string)
    (generate-brainfuck-code
      (convert-UglyBF-instructions-to-brainfuck
        (extract-UglyBF-instructions uglyBF-code))
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-UglyBF converter.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-UglyBF-command-for-brainfuck (brainfuck-command)
  "Returns for the BRAINFUCK-COMMAND the UglyBF equivalent as two
   values:
     (1) the ``uglyBF-command'' and
     (2) an ``inversion-mode'' determining whether the UglyBF command
         must be inverted to establish a correct affiliation."
  (declare (type brainfuck-command brainfuck-command))
  (the (values uglyBF-command inversion-mode)
    (case brainfuck-command
      (:increment    (values :inc/dec :standard))
      (:decrement    (values :inc/dec :inverted))
      (:move-right   (values :move    :standard))
      (:move-left    (values :move    :inverted))
      (:output       (values :io      :standard))
      (:input        (values :io      :inverted))
      (:jump-forward (values :jump    :standard))
      (:jump-back    (values :jump    :inverted))
      (otherwise
        (error "Invalid brainfuck command: ~s." brainfuck-command)))))

;;; -------------------------------------------------------

(defun extract-multiples (repetitions)
  "Determines the number of doublings constituting the REPETITIONS count
   and returns two values:
     (1) a list of zero or more positive integers, each designating the
         tally of doublings which when accumulated resolve to or
         approximately to the REPETITIONS's value
     (2) an integer in the range [0, 1] equal to the remainder which,
         while necessary to achieve the exact repetitions, cannot be
         contributed by a doubling.
   ---
   Example:
     The invocation
       (extract-multiples 105)
     yields the two values:
       ((6 5 3)
        1)
     
     The first value
       (6 5 3)
     determines the number of independent doublings which contribute to
     the complete or partial value of the REPETITIONS, that is:
       2^6 = 64
       2^5 = 32
       2^3 =  8
     By accumulation we obtain:
       64 + 32 + 8 = 104
     
     It becomes patent that a discrepancy exists:
       104 != 105
     
     The second return value
       1
     provides us with the intelligence anenst the remainder which eludes
     the capabilities of the doubling:
       104 + 1 = 105."
  (declare (type positive-integer repetitions))
  (the (values (list-of positive-integer) remainder)
    (if (<= repetitions 1)
      (values NIL repetitions)
      (let ((factors               NIL)
            (remaining-repetitions repetitions))
        (declare (type (list-of positive-integer) factors))
        (declare (type (integer 0 *)              remaining-repetitions))
        (loop do
          (let ((number-of-doubles
                  (floor (log remaining-repetitions 2))))
            (declare (type positive-integer number-of-doubles))
            
            (push number-of-doubles factors)
            
            (decf remaining-repetitions
              (expt 2 number-of-doubles))
            
            (when (<= remaining-repetitions 1)
              (loop-finish))))
        (values (nreverse factors) remaining-repetitions)))))

;;; -------------------------------------------------------

(defun generate-UglyBF-instructions (uglyBF-command
                                     inversion-mode
                                     repetitions)
  "Returns a list of UglyBF instructions which reflect the
   UGLYBF-COMMAND applied in the INVERSION-MODE the REPETITIONS tally of
   times under the use of the most duplication operations possible."
  (declare (type uglyBF-command   uglyBF-command))
  (declare (type inversion-mode   inversion-mode))
  (declare (type positive-integer repetitions))
  
  (let ((uglyBF-instructions NIL))
    (declare (type (list-of uglyBF-command) uglyBF-instructions))
    
    (multiple-value-bind (doublings remainder)
        (extract-multiples repetitions)
      (declare (type (list-of positive-integer) doublings))
      (declare (type remainder                  remainder))
      
      (loop for doubling of-type positive-integer in doublings do
        ;; Inverted command?
        ;; => Prepend backslash "\".
        (when (eq inversion-mode :inverted)
          (push :invert uglyBF-instructions))
        
        ;; Add the tally of doubling operations "*".
        (loop repeat doubling do
          (push :double uglyBF-instructions))
        
        ;; Add the UglyBF command.
        (push uglyBF-command uglyBF-instructions))
      
      ;; Remainder extant which cannot be accomplished by doubling?
      ;; => Optionally prepend a "\" in the case of inversion; but in
      ;;    any case add the UglyBF command.
      (when (plusp remainder)
        (when (eq inversion-mode :inverted)
          (push :invert uglyBF-instructions))
        (push uglyBF-command uglyBF-instructions)))
    
    (the (list-of uglyBF-command)
      (nreverse uglyBF-instructions))))

;;; -------------------------------------------------------

(defun get-UglyBF-commands-for-bf-command-group (bf-command-group)
  "Returns a list of UglyBF instructions which harnesses the power of
   the duplication operation in the most efficient manner to represent
   the brainfuck instruction group BF-COMMAND-GROUP, a cons which
   conjoins the a brainfuck command and its consecutive repetitions."
  (declare (type bf-command-group bf-command-group))
  (destructuring-bind (brainfuck-command . group-size) bf-command-group
    (declare (type brainfuck-command brainfuck-command))
    (declare (type positive-integer  group-size))
    (the (list-of uglyBF-command)
      (multiple-value-bind (uglyBF-command inversion-mode)
          (get-UglyBF-command-for-brainfuck brainfuck-command)
        (declare (type uglyBF-command uglyBF-command))
        (declare (type inversion-mode inversion-mode))
        (generate-UglyBF-instructions
          uglyBF-command inversion-mode group-size)))))

;;; -------------------------------------------------------

(defun coalesce-brainfuck-instructions (brainfuck-instructions)
  "Returns a list separating the BRAINFUCK-INSTRUCTIONS into groups,
   with each such compound being a cons, the left part of which contains
   the command, while the right moeity stores the tally of consecutive
   occurrences.
   ---
   For example, the brainfuck code
     +++ < ++ >>>>
   will yield the group list
     ((+ 3) (< 1) (+ 2) (> 4))"
  (declare (type (vector brainfuck-command *) brainfuck-instructions))
  (let ((groups NIL))
    (declare (type (list-of bf-command-group) groups))
    (loop
      with group
        of-type (or null bf-command-group)
        =       NIL
      for instruction
        of-type brainfuck-command
        across  brainfuck-instructions
      and first-instruction-p
        of-type boolean
        =       T
        then    NIL
      do
        (cond
          (first-instruction-p
            (setf group (cons instruction 1))
            (push group groups))
          ;; The current INSTRUCTION matches the previous one?
          ;; => Increment the GROUP's counter.
          ((eq (car group) instruction)
            (incf (cdr group)))
          ;; The current INSTRUCTION does not match the previous one?
          ;; => Create a new group.
          (T
            (setf group (cons instruction 1))
            (push group groups))))
    (the (list-of bf-command-group)
      (nreverse groups))))

;;; -------------------------------------------------------

(defun convert-brainfuck-groups-to-UglyBF-instructions
    (bf-command-groups)
  "Creates and returns a one-dimensional simple array of UglyBF
   instructions equivalent to the brainfuck command groups
   BF-COMMAND-GROUPS, a list of cons composed of
     (brainfuck-command . repetitions)."
  (declare (type (list-of bf-command-group) bf-command-groups))
  (the (simple-array uglyBF-command (*))
    (coerce
      (mapcan
        #'get-UglyBF-commands-for-bf-command-group
        bf-command-groups)
      '(simple-array uglyBF-command (*)))))

;;; -------------------------------------------------------

(defun convert-brainfuck-code-to-UglyBF-instructions (brainfuck-code)
  "Creates and returns a one-dimensional simple array of UglyBF
   instructions equivalent to the piece of BRAINFUCK-CODE."
  (declare (type string brainfuck-code))
  (the (simple-array uglyBF-command (*))
    (convert-brainfuck-groups-to-UglyBF-instructions
      (coalesce-brainfuck-instructions
        (extract-brainfuck-instructions brainfuck-code)))))

;;; -------------------------------------------------------

(defun convert-brainfuck-code-to-UglyBF
    (brainfuck-code
     &key (destination NIL))
  "Converts the piece of BRAINFUCK-CODE into an equivalent UglyBF
   program and writes the result to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (generate-UglyBF-code
      (convert-brainfuck-code-to-UglyBF-instructions brainfuck-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "BF".
(interpret-UglyBF "******+*+.>******+**+++.")

;;; -------------------------------------------------------

;; Print "HELLO WORLD".
(interpret-UglyBF "******+***+.\\*+\\+.**+*++..*++.>*****+.\\>***+.\\***+.*++.\\**+\\*+.\\***+.")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-UglyBF "\\..")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-UglyBF "\\..[\\..\\[")

;;; -------------------------------------------------------

;; Generate the ASCII code for the letter "A" (= 65) and print it.
;; The code is using the formula
;;   65 = (1 * 2 * 2 * 2 * 2 * 2 * 2) + 1
;;      = 64                          + 1.
(interpret-UglyBF "******++.")

;;; -------------------------------------------------------

;; Extract and return the UglyBF instructions for an example containing
;; inversion and duplication.
(extract-UglyBF-instructions "\\**+")

;;; -------------------------------------------------------

;; Generate and return the brainfuck instructions for a complex piece of
;; UglyBF code.
(convert-UglyBF-instructions-to-brainfuck
  (extract-UglyBF-instructions "\\.*+[\\+>*+\\>\\[>[\\>+>\\+\\[\\>."))

;;; -------------------------------------------------------

;; Generate the equivalent to the brainfuck code
;;   ,++[->++<]>[<+>-]<.
;; in UglyBF, which comprehends
;;   \.*+[\+>*+\>\[>[\>+>\+\[\>.
;; and return it as a string.
(generate-UglyBF-code
  (convert-brainfuck-code-to-UglyBF-instructions ",++[->++<]>[<+>-]<."))

;;; -------------------------------------------------------

;; Convert the infinite cat program from brainfuck's
;;   ,.[,.]
;; into the UglyBF equivalent
;;   \..[\..\[
;; and execute the same.
(interpret-UglyBF
  (convert-brainfuck-code-to-UglyBF ",.[,.]"))
