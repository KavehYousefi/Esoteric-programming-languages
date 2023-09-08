;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "imput", invented by the Esolang user "Cinnamony" and
;; presented on June 20th, 2023, the kenspeckle attribute of which
;; maintains its abode in the enhancement of the programming language
;; "brainfuck", designed by Urban Mueller, to more potency in the input
;; and output bailiwicks.
;; 
;; 
;; Concept
;; =======
;; The imput programming language's cynosure entails the provision of
;; enhanced input and output facilities, while retaining brainfuck's
;; Turing-complete nature, including the operations, and memory model.
;; 
;; == "imput", AN ERRONEOUS "input" ==
;; The language's agnomination, "imput", ensues from a cacography
;; inflicting the correct "input" diction.
;; 
;; 
;; Architecture
;; ============
;; imput's architecture constitutes a verbatim appropriation of its
;; entheus brainfuck, operating a type of cells with infinite expansion
;; along both axes, each storage unit's capacity comprehending a scalar
;; unsigned byte from the range [0, 255], the value of which wraps
;; around upon a march's transcendence in its pursuit to ascertain the
;; state's integrity.
;; 
;; A movable cell pointer selects at any instant the currently active
;; cell, the only entity from the memory entalented with an amenability
;; to behests.
;; 
;; 
;; Instructions
;; ============
;; imput's instruction set amplects the octuple brainfuck members and
;; enhances their tally by a quintuple of advenient operations, the same
;; appertaining to input and output exclusively.
;; 
;; == MOST BRAINFUCK OPERATIONS RETAIN THEIR ORIGINAL CAUSATA ==
;; Besides the brainfuck input command, to whom the parergal onus of the
;; input conduit response's transmission to the input buffer is
;; assigned, those elements desumed from the entheus do not alter their
;; deportment.
;; 
;; == OVERVIEW ==
;; A foundational nortelry's dation shall proceed by the following
;; apercu's mediation:
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   +              | Increments the current memory cell by one.
;;                  | If the resulting value exceeds the upper bourne
;;                  | of 255, the state is reset to the minimum of zero
;;                  | (0).
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   -              | Decrements the current memory cell by one.
;;                  | If the resulting value exceeds the lower bourne
;;                  | of zero (0), the state wraps around to the
;;                  | maximum of 255.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   <              | Translates the cell pointer one step to the left.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   >              | Translates the cell pointer one step to the
;;                  | right.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   ,              | Queries the standard input for an ASCII character
;;                  | and stores its character code in the current,
;;                  | while concomitantly append it to the input
;;                  | buffer.
;;                  |--------------------------------------------------
;;                  | This operation constitutes an augmented
;;                  | acquisition from brainfuck, the input buffer's
;;                  | involvement establishing a supererogation.
;;   ..................................................................
;;   .              | Prints the character whose ASCII code equals the
;;                  | current cell value to the standard output.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   [              | If the current cell value equals zero (0), moves
;;                  | the instruction pointer (IP) forward to the
;;                  | position immediately succeeding the matching "]"
;;                  | command. Otherwise proceeds as usual.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   ]              | If the current cell value does not equal zero
;;                  | (0), moves the instruction pointer (IP) back to
;;                  | the position immediately succeeding the matching
;;                  | "[" command. Otherwise proceeds as usual.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a verbatim acquisition
;;                  | from brainfuck.
;;   ..................................................................
;;   imput          | Queries the user for a line of input and appends
;;                  | the same to the input buffer.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a novelty not present
;;                  | in brainfuck.
;;   ..................................................................
;;   oumput         | Prints the most recent input stored in the input
;;                  | buffer to the standard output.
;;                  | If no such element exists, no output is issued.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a novelty not present
;;                  | in brainfuck.
;;   ..................................................................
;;   ounput         | Prints the first input stored in the input buffer
;;                  | to the standard output.
;;                  | If no such element exists, no output is issued.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a novelty not present
;;                  | in brainfuck.
;;   ..................................................................
;;   kiwi           | Prints the message "kiwi" to the standard output.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a novelty not present
;;                  | in brainfuck.
;;   ..................................................................
;;   ouput(message) | Prints the {message} to the standard output.
;;         *******  |--------------------------------------------------
;;                  | The {message} must be a string literal or a
;;                  | signed or unsigned integer number.
;;                  |--------------------------------------------------
;;                  | This operation constitutes a novelty not present
;;                  | in brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; imput's protolog, the language founded upon simplicity, yet tholes
;; from a number of ambiguous passage, the most salient of which shall
;; now be elucidated.
;; 
;; == WHICH EXACT REGULATIONS APPLY TO THE DONET? ==
;; The concoction of brainfuck and a novel produce, imput's syntactical
;; design yet retains some mete of adumbration.
;; 
;; A treble of facts ought to be adhibited deeper perpension:
;; 
;;   (a) brainfuck programs admit any content, accommodating no causata
;;       to non-command constituents. Such as conception, while not
;;       excluded from sensibility, ostends a rather forinsecal
;;       contingency in imput, as the language, maugre its inspiration
;;       by brainfuck, does not relate of an ipsissima verba mimicry.
;;   
;;   (b) Resorting to the otherward extreme, Proscrustean rigor in
;;       insignificant tokens' banishment would condemn the language to
;;       severely impaired legibility.
;;   
;;   (c) If, on the other hand, the requisitum's imposition would govern
;;       that command tokens are necessitated to experience some ilk of
;;       sepiment, for instance whitespaces, an imput program could only
;;       accept a design in concord with such:
;;         [ , . ]
;;       As counterdistinguished from the acquainted
;;         [,.]
;; 
;; It has been adjudged that imput programs do not require separators
;; betwixt command; if such are desiderated, merely whitespaces, which
;; comprehend the space, tab, and newline, are homologated.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-08
;; 
;; Sources:
;;   [esolang2023imput]
;;   The Esolang contributors, "imput", June 20th, 2023
;;   URL: "https://esolangs.org/wiki/Imput"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(deftype command-type ()
  "The ``command-type'' enumerates the recognized variants of imput
   commands."
  '(member
    :increment
    :decrement
    :move-left
    :move-right
    :input
    :output
    :jump-forward
    :jump-back
    :imput
    :oumput
    :ounput
    :kiwi
    :ouput))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping from commands names
   to their representative types, realized in a hash table which
   associates strings to ``command-type'' objects."
  '(hash-table-of string command-type))

;;; -------------------------------------------------------

(deftype imput-program ()
  "The ``imput-program'' type defines an executable imput program as a
   vector of zero or more ``Command'' instances."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from forward jump command
   positions in an imput program to that locations of the matching back
   jump instructions, and vice versa, realized as a hash table which
   associates the fixnum positions among each other."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype input-buffer ()
  "The ``input-buffer'' type defines a structre reponsible for the
   castaldy of the standard input responses for later access, reified in
   the form of an adjustable vector of string objects."
  '(vector string *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits, and thus a commorant of the integral range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the imput program memory as an
   association of integer-valued cell indices with byte-valued cells,
   its manifestation a hash table that maps integers to ``octets''."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface serves as the common foundry of all Imput
   command classes.")

;;; -------------------------------------------------------

(defmacro define-command (name &rest slots)
  "Defines a new ``Command'' subclass identified by the NAME and endowed
   with the SLOTS, the first of which, if constituting a string, is
   construed as a documentation string and thus applied, any other case
   expects such a slot definition to match the ``defstruct'' regulations
   for slot specifications."
  `(defstruct (,name
     (:include Command))
     ,(if (stringp (first slots))
        (pop slots)
        (format NIL "The ~s class." name))
     ,@slots))

;;; -------------------------------------------------------

(define-command Increment-Command
  "The ``Increment-Command'' represents the brainfuck instruction \"+\",
   serving to increment the current memory cell by one.")

;;; -------------------------------------------------------

(define-command Decrement-Command
  "The ``Decrement-Command'' represents the brainfuck instruction \"-\",
   serving to decrement the current memory cell by one.")

;;; -------------------------------------------------------

(define-command Move-Left-Command
  "The ``Move-Left-Command'' represents the brainfuck instruction
   \"<\", serving to translate the cell pointer one step to the left.")

;;; -------------------------------------------------------

(define-command Move-Right-Command
  "The ``Move-Right-Command'' represents the brainfuck instruction
   \">\", serving to translate the cell pointer one step to the right.")

;;; -------------------------------------------------------

(define-command Input-Command
  "The ``Input-Command'' represents the brainfuck instruction \",\",
   serving to query the standard input for an ASCII character in order
   to transfer its character code into the current cell.")

;;; -------------------------------------------------------

(define-command Output-Command
  "The ``Output-Command'' represents the brainfuck instruction \".\",
   serving to print the ASCII character corresponding to the current
   cell value to the standard output.")

;;; -------------------------------------------------------

(define-command Jump-Forward-Command
  "The ``Jump-Forward-Command'' represents the brainfuck instruction
   \"[\", serving a conditional forward jump construct.")

;;; -------------------------------------------------------

(define-command Jump-Back-Command
  "The ``Jump-Back-Command'' represents the brainfuck instruction \"]\",
   serving a conditional back jump construct.")

;;; -------------------------------------------------------

(define-command Imput-Command
  "The ``Imput-Command'' represents the \"imput\" instruction, serving
   to query the standard input for an arbitrary response for later
   utilization.")

;;; -------------------------------------------------------

(define-command Oumput-Command
  "The ``Oumput-Command'' represents the \"oumput\" instruction, serving
   to print the last input to the standard output.")

;;; -------------------------------------------------------

(define-command Ounput-Command
  "The ``Ounput-Command'' represents the \"ounput\" instruction, serving
   to print the first input to the standard output.")

;;; -------------------------------------------------------

(define-command Kiwi-Command
  "The ``Kiwi-Command'' represents the \"kiwi\" instruction, serving to
   print the message \"kiwi\" to the standard output.")

;;; -------------------------------------------------------

(define-command Ouput-Command
  "The ``Ouput-Command'' represents the \"ouput\" instruction, capable
   of printing a specified message to the standard output."
  (message (error "Missing message.") :type string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized command names with ``command-type''
   representatives.")

;;; -------------------------------------------------------

(flet ((register-identifier (name command-type)
        "Associates the command NAME with the COMMAND-TYPE and returns
         no value."
        (declare (type string       name))
        (declare (type command-type command-type))
        (setf (gethash name +IDENTIFIERS+) command-type)
        (values)))
  (register-identifier "+"      :increment)
  (register-identifier "-"      :decrement)
  (register-identifier "<"      :move-left)
  (register-identifier ">"      :move-right)
  (register-identifier ","      :input)
  (register-identifier "."      :output)
  (register-identifier "["      :jump-forward)
  (register-identifier "]"      :jump-back)
  (register-identifier "imput"  :imput)
  (register-identifier "oumput" :oumput)
  (register-identifier "ounput" :ounput)
  (register-identifier "kiwi"   :kiwi)
  (register-identifier "ouput"  :ouput)
  (values))

;;; -------------------------------------------------------

(defun get-command-type (identifier)
  "Returns the command type affiliated with the IDENTIFIER, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type string identifier))
  (the command-type
    (or (gethash identifier +IDENTIFIERS+)
        (error "Unrecognized identifier ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'eq)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more adjacent whitespaces, and returns the position in the
   SOURCE immedidately succeeding the skipped portion."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p
                (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun expect-character (source position expected-character)
  "Determines whether the character POSITION in the SOURCE, with
   potential whitespaces omitted, equals the EXPECTED-CHARACTER, on
   confirmation returning the location in the SOURCE succeeding the
   EXPECTED-CHARACTER's position, and contingently skipped whitespaces,
   otherwise signaling an error of an unspecified type."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the fixnum
    (let ((position-after-whitespaces
            (skip-whitespaces source position)))
      (declare (type fixnum position-after-whitespaces))
      (cond
        ((not (array-in-bounds-p source position-after-whitespaces))
          (error "Expected the character \"~c\" at position ~d, ~
                  but found the source exhausted."
            expected-character position))
        ((char/= (char source position-after-whitespaces)
                 expected-character)
          (error "Expected the character \"~c\" at position ~d, ~
                  but encountered \"~c\" at position ~d."
            expected-character position
            (char source position-after-whitespaces)
            position-after-whitespaces))
        (T
          (1+ (skip-whitespaces source position-after-whitespaces)))))))

;;; -------------------------------------------------------

(defun read-string (source start)
  "Proceeding from the START position into the SOURCE, reads a string,
   delimited by double quotation marks, and contingently surrounded by
   whitespaces, and returns two values:
     (1) The content betwixt the double quotation marks as a string.
     (2) The position into the SOURCE succeeding the concluding
         quotation mark, and contingently any accolent whitespace
         sequence."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (text (make-string-output-stream))
      (declare (type string-stream text))
      (loop
        for position
          of-type fixnum
          from    (expect-character source start #\")
        for current-character
          of-type (or null character)
          =       (when (array-in-bounds-p source position)
                    (char source position))
        if (null current-character) do
          (error "Unterminated string literal at position ~d."
            position)
        else if (char= current-character #\") do
          (return
            (values
              (get-output-stream-string text)
              (skip-whitespaces source
                (1+ position))))
        else do
          (write-char current-character text)))))

;;; -------------------------------------------------------

(defun read-number (source start)
  "Proceeding from the START position into the SOURCE, reads a signed or
   unsigned integer number, potentially surrounded by whitespaces, and
   returns two values:
     (1) The consumed number as a string.
     (2) The position in the SOURCE succeeding the consumed number and
         any accolent whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (let ((new-position (skip-whitespaces source start)))
      (declare (type fixnum new-position))
      (with-open-stream (digits (make-string-output-stream))
        (declare (type string-stream digits))
        (when (sign-character-p (char source new-position))
          (write-char (char source new-position) digits)
          (incf new-position))
        (loop
          while
            (and (< new-position (length source))
                 (digit-char-p (char source new-position)))
          do
            (write-char (char source new-position) digits)
            (incf new-position))
        (values
          (get-output-stream-string digits)
          new-position)))))

;;; -------------------------------------------------------

(defun read-ouput-message (source start)
  "Proceeding from the START position into the SOURCE, reads an
   \"ouput\" command invocation's argument list, composed of a twain of
   parentheses that surround a single integer or string literal, and
   returns two values:
     (1) The consumed integer or string literal as a string.
     (2) The position into the SOURCE succeeding the concluding closing
         parenthesis, and a contingent sequence of accolent
         whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((new-position (expect-character source start #\())
        (message     NIL))
    (declare (type fixnum           new-position))
    (declare (type (or null string) message))
    (let ((current-character
            (when (array-in-bounds-p source new-position)
              (char source new-position))))
      (declare (type (or null character) current-character))
      (cond
        ;; End of file?
        ;; => Error.
        ((null current-character)
          (error "Expected an \"ouput\" command argument at ~
                  position ~d, but found the argument list incomplete."
            new-position))
        ;; Signed or unsigned integer?
        ((or (sign-character-p current-character)
             (digit-char-p current-character))
          (multiple-value-setq
            (message new-position)
            (read-number source new-position)))
        ;; Quoted string?
        ((char= current-character #\")
          (multiple-value-setq
            (message new-position)
            (read-string source new-position)))
        ;; Empty argument list?
        ((char= current-character #\))
          NIL)
        ;; Any other ouput message is prohibited.
        (T
          (error "Invalid character \"~c\" at position ~d of the ~
                  ouput argument list."
            current-character new-position))))
    (the (values string fixnum)
      (values message
        (skip-whitespaces source
          (expect-character source new-position #\)))))))

;;; -------------------------------------------------------

(defun identifier-found-p (source start identifier)
  "Determines whether, proceeding from the START position in the SOURCE,
   the IDENTIFIER immediately follows, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type string identifier))
  (the boolean
    (not (null
      (string= source identifier
        :start1 start
        :end1   (min (+ start (length identifier))
                     (length source)))))))

;;; -------------------------------------------------------

(defun search-identifier (source start)
  "Proceeding from the START position into the SOURCE, probes for the
   next command name, contingently preceded by an omission of leading
   whitespaces, on confirmation returning two values:
     (1) The detected command's ``command-type'' representation.
     (2) The position following the detected command name, with any
         leading whitespaces skipped.
   If no command name could be detected, an error of an unspecified type
   is signaled."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (or null command-type) fixnum)
    (let ((position (skip-whitespaces source start)))
      (declare (type fixnum position))
      (loop
        for identifier
          of-type string
          being the hash-keys in +IDENTIFIERS+
        when (identifier-found-p source position identifier) do
          (return
            (values
              (get-command-type identifier)
              (skip-whitespaces source
                (+ start (length identifier)))))
        finally
          (error "No command found at position ~d." start)))))

;;; -------------------------------------------------------

(defun read-command (source position command-type)
  "Proceeding from the POSITION into the SOURCE, following a consumed
   command name token whose COMMAND-TYPE is communiated, produces and
   returns a ``Command'' representation for the latter.
   ---
   Please note that the POSITION succeeds the already consumed command
   name token, whence the COMMAND-TYPE is begotten, inteded to capacitate
   potential arguments' extrication."
  (declare (type string       source))
  (declare (type fixnum       position))
  (declare (type command-type command-type))
  (let ((new-position (skip-whitespaces source position)))
    (declare (type fixnum new-position))
    (the (values Command fixnum)
      (values
        (case command-type
          (:increment    (make-increment-command))
          (:decrement    (make-decrement-command))
          (:move-left    (make-move-left-command))
          (:move-right   (make-move-right-command))
          (:input        (make-input-command))
          (:output       (make-output-command))
          (:jump-forward (make-jump-forward-command))
          (:jump-back    (make-jump-back-command))
          (:imput        (make-imput-command))
          (:oumput       (make-oumput-command))
          (:ounput       (make-ounput-command))
          (:kiwi         (make-kiwi-command))
          (:ouput
            (multiple-value-bind (message end-position)
                (read-ouput-message source new-position)
              (declare (type string message))
              (declare (type fixnum end-position))
              (setf new-position end-position)
              (make-ouput-command :message message))))
          (skip-whitespaces source new-position)))))

;;; -------------------------------------------------------

(defun extract-commands (source)
  "Extracts and returns from the SOURCE a one-dimensional simple array
   comprehending its commands."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((process-command-type (command-type new-position)
            "Evaluates the COMMAND-TYPE, the identifier of which has
             terminated at the NEW-POSITION in the SOURCE, and returns
             a ``Command'' representation of the former, while
             concomitantly updating the POSITION to command's end
             location in the SOURCE."
            (declare (type command-type command-type))
            (declare (type fixnum       new-position))
            (multiple-value-bind (command end-position)
                (read-command source new-position command-type)
              (declare (type Command command))
              (declare (type fixnum  end-position))
              (setf position end-position)
              (the Command command))))
      (the imput-program
        (coerce
          (loop
            while (< position (length source))
            collect
              (multiple-value-call #'process-command-type
                (search-identifier source position)))
          '(simple-array Command (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-jump-table ()
  "Returns a new empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Creates and returns for the imput PROGRAM a jump table which connects
   the forward and back jump positions in the same by mediation of their
   indices."
  (declare (type imput-program program))
  (let ((jump-table          (make-jump-table))
        (forward-jump-points
          (make-array 0
            :element-type    'fixnum
            :initial-element 0
            :adjustable      T
            :fill-pointer    0)))
    (declare (type jump-table        jump-table))
    (declare (type (vector fixnum *) forward-jump-points))
    (loop
      for command  of-type Command across program
      and position of-type fixnum  from   0
      
      if (jump-forward-command-p command) do
        (vector-push-extend position forward-jump-points)
      else if (jump-back-command-p command) do
        (if (plusp (length forward-jump-points))
          (let ((start-position
                  (aref forward-jump-points
                    (1- (fill-pointer forward-jump-points))))
                (end-position position))
            (declare (type fixnum start-position))
            (declare (type fixnum end-position))
            (decf (fill-pointer forward-jump-points))
            (setf (gethash start-position jump-table) end-position)
            (setf (gethash end-position   jump-table) start-position))
          (error "Unmatched back jump command at position ~d."
            position))
      finally
        (unless (zerop (length forward-jump-points))
          (error "Unmatched forward jump commands at positions ~s."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table from-position)
  "Returns the opposite jump point location associated with the
   FROM-POSITION in the JUMP-TABLE, or signals an error of an
   unspecified type upon its absence."
  (declare (type jump-table jump-table))
  (declare (type fixnum     from-position))
  (the fixnum
    (or (gethash from-position jump-table)
        (error "No destination defined for the jump position ~d."
          from-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input buffer.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-input-buffer ()
  "Creates and returns an initially empty ``input-buffer''."
  (the input-buffer
    (make-array 0
      :element-type    'string
      :initial-element ""
      :adjustable      T
      :fill-pointer    0)))

;;; -------------------------------------------------------

(defun memorize-input (input-buffer new-input)
  "Appends the NEW-INPUT to the INPUT-BUFFER and returns no value."
  (declare (type input-buffer input-buffer))
  (declare (type string       new-input))
  (vector-push-extend new-input input-buffer)
  (values))

;;; -------------------------------------------------------

(defun input-buffer-empty-p (input-buffer)
  "Determines whether the INPUT-BUFFER is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type input-buffer input-buffer))
  (the boolean
    (zerop (length input-buffer))))

;;; -------------------------------------------------------

(defun get-first-input (input-buffer)
  "Returns the first input committed to the INPUT buffer, or an empty
   string upon its vacancy."
  (declare (type input-buffer input-buffer))
  (the string
    (if (input-buffer-empty-p input-buffer)
      ""
      (aref input-buffer 0))))

;;; -------------------------------------------------------

(defun get-last-input (input-buffer)
  "Returns the last input committed to the INPUT-BUFFER, or an empty
   string upon its vacancy."
  (declare (type input-buffer input-buffer))
  (the string
    (if (input-buffer-empty-p input-buffer)
      ""
      (aref input-buffer
        (1- (fill-pointer input-buffer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns an infinite ``memory'' object, all its cells
   initialized to zero (0)."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the byte value stored in the MEMORY cell addressed by the
   INDEX."
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell at the specific index,
   contingently preceded by an adjustment of the input pursuing to
   respect the unsigned byte range [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer index))
  (setf (gethash index memory 0) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
                  (program
                   &aux (jump-table (build-jump-table program)))))
  "The ``Interpreter'' class applies itself to the execution of an imput
   program conveyed in the form of a command vector."
  (program      (error "Missing program.")    :type imput-program)
  (ip           0                             :type fixnum)
  (jump-table   (error "Missing jump table.") :type jump-table)
  (memory       (make-memory)                 :type memory)
  (cell-pointer 0                             :type integer)
  (input-buffer (make-input-buffer)           :type input-buffer))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the program operated upon by the INTERPRETER is
   exhausted, that is, has been completely processed, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-program interpreter)))))))

;;; -------------------------------------------------------

(defun current-cell (interpreter)
  "Returns the byte stored in the INTERPRETER's current memory cell."
  (declare (type Interpreter interpreter))
  (the octet
    (memory-cell-at
      (interpreter-memory       interpreter)
      (interpreter-cell-pointer interpreter))))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value interpreter)
  "Stores the NEW-VALUE in the INTERPRETER's current memory cell,
   contingently wrapping the datum around in order to accommodate the
   unsigned byte range [0, 255], and returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf
    (memory-cell-at
      (interpreter-memory       interpreter)
      (interpreter-cell-pointer interpreter))
    new-value)
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Processes the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmacro define-command-processor
    (command-class (interpreter-variable command-variable)
     &body body)
  "Defines an implementation of the generic function
   ``process-command'', employing the INTERPRETER-VARIABLE as the first
   parameter name, the COMAMND-VARIABLE as the second one's, associated
   with the COMMAND-CLASS as that parameter's dispatching parameter
   specializer, whereas the method body is composed of the BODY forms,
   and succeeded by a form which returns no values."
  `(defmethod process-command ((,interpreter-variable Interpreter)
                               (,command-variable     ,command-class))
     (declare (type Interpreter    ,interpreter-variable))
     (declare (ignorable           ,interpreter-variable))
     (declare (type ,command-class ,command-variable))
     (declare (ignorable           ,command-variable))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-command-processor Increment-Command (interpreter command)
  (incf
    (current-cell interpreter)))

;;; -------------------------------------------------------

(define-command-processor Decrement-Command (interpreter command)
  (decf
    (current-cell interpreter)))

;;; -------------------------------------------------------

(define-command-processor Move-Left-Command (interpreter command)
  (decf
    (interpreter-cell-pointer interpreter)))

;;; -------------------------------------------------------

(define-command-processor Move-Right-Command (interpreter command)
  (incf
    (interpreter-cell-pointer interpreter)))

;;; -------------------------------------------------------

(define-command-processor Input-Command (interpreter command)
  (finish-output)
  (let ((input (read-char NIL NIL #\Null)))
    (declare (type character input))
    (setf (current-cell interpreter)
      (char-code input))
    (memorize-input
      (interpreter-input-buffer interpreter)
      (string input)))
  (clear-input))

;;; -------------------------------------------------------

(define-command-processor Output-Command (interpreter command)
  (format T "~c"
    (code-char
      (current-cell interpreter))))

;;; -------------------------------------------------------

(define-command-processor Jump-Forward-Command (interpreter command)
  (when (zerop (current-cell interpreter))
    (setf (interpreter-ip interpreter)
      (get-jump-destination
        (interpreter-jump-table interpreter)
        (interpreter-ip         interpreter)))))

;;; -------------------------------------------------------

(define-command-processor Jump-Back-Command (interpreter command)
  (unless (zerop (current-cell interpreter))
    (setf (interpreter-ip interpreter)
      (get-jump-destination
        (interpreter-jump-table interpreter)
        (interpreter-ip         interpreter)))))

;;; -------------------------------------------------------

(define-command-processor Imput-Command (interpreter command)
  (finish-output)
  (memorize-input
    (interpreter-input-buffer interpreter)
    (read-line))
  (clear-input))

;;; -------------------------------------------------------

(define-command-processor Oumput-Command (interpreter command)
  (format T "~a"
    (get-last-input
      (interpreter-input-buffer interpreter))))

;;; -------------------------------------------------------

(define-command-processor Ounput-Command (interpreter command)
  (format T "~a"
    (get-first-input
      (interpreter-input-buffer interpreter))))

;;; -------------------------------------------------------

(define-command-processor Kiwi-Command (interpreter command)
  (format T "kiwi"))

;;; -------------------------------------------------------

(define-command-processor Ouput-Command (interpreter command)
  (format T "~a"
    (ouput-command-message command)))

;;; -------------------------------------------------------

(defun execute-interpreter (interpreter)
  "Processes the imput program under the INTERPRETER's governance and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-exhausted-p interpreter) do
    (process-command interpreter
      (aref (interpreter-program interpreter)
        (interpreter-ip interpreter)))
    (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-imput (code)
  "Interprets the piece of imput source CODE and returns no value."
  (declare (type string code))
  (execute-interpreter
    (make-interpreter
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-imput "ouput(\"Hello, world!\")")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-imput
  "imput
   oumput")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-imput
  "+[imput oumput]")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a null character
;; input.
(interpret-imput
  ",.[,.]")

;;; -------------------------------------------------------

;; Kiwiscript: Print the message "kiwi".
(interpret-imput "kiwi")

;;; -------------------------------------------------------

;; Query the user for their name, here denoted as "{userName}", and
;; print the greeting
;;   Hello, {userName}. How are you?
(interpret-imput
  "ouput(\"Please enter your name: \")
   imput
   ouput(\"Hello, \") oumput ouput(\". How are you?\")")
