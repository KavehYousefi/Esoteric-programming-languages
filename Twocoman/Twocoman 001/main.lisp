;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Twocoman", invented by the Esolang user "Cinnamony" and
;; presented on June 22nd, 2023, the foundry of which is edified upon
;; Urban Mueller's "brainfuck", extended in its communicative faculties,
;; and, most kenspeckle in its design, bartering the immediate statement
;; of its commands by a series of eleven modie whose transitions and
;; executions furnish a tantamount of the respective causata.
;; 
;; 
;; Concept
;; =======
;; In their standard format, Twocoman program's assume a series of zero
;; or more bits, the zero-valued among which perpetuate the transitions
;; athwart the ordered modes, while the one-valued tokens administer
;; effect to the thus delineated state.
;; 
;; == TWOCOMAN = [TWO][COM]M[AN]DS? ==
;; The language's agnomination, maugre an official appreciation's
;; lacuna, might be weened as a portmanteau of "TWO" and "COMmANds",
;; that is, "two commands", the same reassures the cardinality of its
;; instruction set.
;; 
;; == TWOCOMAN: A CHAMPARTY OF MODES AND EXECUTIONS ==
;; Eleven modes, each a correspondent to a Twocoman instruction, govern
;; the program's entelechy, their selection, a transition in their
;; natural, proceeds by means of zero (0) bits, whereas their actual
;; effectivity ensues from a one (1) bit's occurrency.
;; 
;; == THE CURRENT MODE DEFINES THE ACTION TO PERFORM ==
;; Twocoman's operational roster amplects a legend of eleven members,
;; everichon's agency concurs with a causatum intended for application.
;; 
;; While the membership's entirety shall be illustrated in a table, its
;; status does not protrude beyond a cursory limn, and will be a more
;; meticulous treatise's cynosure in the section "Instructions", which
;; please consule below:
;; 
;;   ------------------------------------------------------------------
;;   Number | Mode | Effect
;;   -------+------+---------------------------------------------------
;;   1      | x    | initial state
;;   ..................................................................
;;   2      | +    | increment current cell
;;   ..................................................................
;;   3      | -    | decrement current cell
;;   ..................................................................
;;   4      | .    | output character
;;   ..................................................................
;;   5      | ,    | input character
;;   ..................................................................
;;   6      | ?    | output number
;;   ..................................................................
;;   7      | !    | input number
;;   ..................................................................
;;   8      | [    | jump forward if current cell is zero
;;   ..................................................................
;;   9      | ]    | jump back if current cell is not zero
;;   ..................................................................
;;   10     | <    | move cell pointer sinistrally
;;   ..................................................................
;;   11     | >    | move cell pointer dextrally
;;   ------------------------------------------------------------------
;; 
;; At the program's inchoation, the first mode (1), the initialization
;; step, is mandated to be executed at least once, preceding any other
;; mode's application.
;; 
;; == COMMANDS: BITS THAT SELECT AND EXECUTE MODES ==
;; The "command" terminology as a euonym is embued with consilience to
;; the common apprehension as an effective agent in Twocoman in the
;; context of its source code.
;; 
;; The standard format reserves for this purpose only a series of zero
;; or more bit values, which embraces exclusively zero (0) or (1)
;; digits, the former of which, commencing with the initial mode of 1
;; (initialization) transition to the respective successor, contingently
;; wrapping around at the desinence; meanwhile, the latter bit value, 1,
;; executes the thus selected current mode. With compendiousness in
;; relations:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | switch to next command
;;   ..................................................................
;;   1       | execute current command
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; Maugre its enhanced versatility, Twocoman yet subscribes to an
;; architecture identical in all aspects to its brainfuck stock-father:
;; deploying a bilaterally infinite expanse of unsigned bytes in cells,
;; the active unit of which is designated by the motile cell pointer.
;; 
;; A cell's capacity imposes an imperative, and its stringency enforces
;; any march's transcendes to a wrapping around; that is, a violation of
;; the lower bourne of zero (0) returns to the upper post of 255, while
;; a transcendence beyond the maximum of 255 restores the procession
;; commencing from the bottom extremum of zero (0).
;; 
;; 
;; Data Types
;; ==========
;; The factor of its equiparation with brainfuck, despite the advenient
;; potence woning in Twocoman, preserves the cleronomy's type system:
;; Unsigned byte values, the substratum of the memory, appropriate a
;; paravaunt rank, covering the integral range of [0, 255], and wrap
;; around at the bournes in order to ascertain the capacity's sanity,
;; while ASCII characters, as a paravail warklume, operate along the
;; communication conduits.
;; 
;; 
;; Syntax
;; ======
;; Twocoman occur as compositions of the bit values zero (0) and one
;; (1), the former element of which changes betwixt the modes, whereas
;; the latter serves in the current selection's execution.
;; 
;; == INSTRUCTIONS ==
;; All instructions are incorporated in the bit sequence that comprises
;; the entire source code.
;; 
;; == WHITESPACES ==
;; A composition of bits only, whitespaces may not appear in the code.
;; 
;; == COMMENTS ==
;; A counterdistinguishing indicium from its entheus, Twocoman withdraws
;; its tolerance of any other than binary digits from a program, thus
;; excluding the contingency of commentary passage.
;; 
;; == GRAMMAR ==
;; An expression of the donet in the Extended Backus-Naur Form (ENBF)
;; notation shall be suppplied:
;; 
;;   program := "1" , { "0" | "1" } ;
;; 
;; 
;; Instructions
;; ============
;; Twocoman's instruction set tallies eleven members, applied the
;; nomenclature of "modes", whose selection and execution constitutes
;; the bailiwick of the actual command tokens, the binary objects zero
;; (0) and one (1).
;; 
;; == OVERVIEW ==
;; The following tabular illustration shall serve in a compendious ilk
;; of nortelry's adhibition concerning the Twocoman instruction set
;; when furnished in its standard binary guise:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Switches from the current to the next mode.
;;   ..................................................................
;;   1       | Executes the command associated with the current mode.
;;   ------------------------------------------------------------------
;; 
;; A second apercu, now in the conext of the modes, in their natural
;; order, and empighted in their causata's compernage, shall be
;; afforded:
;; 
;;   ------------------------------------------------------------------
;;   No. | Mode | Effect
;;   ----+------+------------------------------------------------------
;;   1   | x    | Initializes the program, and as thus must constitute
;;       |      | the first executed operation in a Twocoman program.
;;       |      |------------------------------------------------------
;;       |      | This mode establishes the initial state.
;;       |      |------------------------------------------------------
;;       |      | This operation an advenient operation not desumed
;;       |      | from brainfuck.
;;   ..................................................................
;;   2   | +    | Increments the current cell value by one.
;;       |      | If the new value would transcend the valid maximum of
;;       |      | 255, it is wrapped around to the lower bourne of zero
;;       |      | (0).
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   3   | -    | Decrements the current cell value by one.
;;       |      | If the new value would descend below the valid
;;       |      | minimum of zero (0), it is wrapped around to the
;;       |      | upper bourne of 255.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   4   | .    | Prints the character whose ASCII code equals the
;;       |      | current cell value to the standard output.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   5   | ,    | Queries the standard input for an ASCII character and
;;       |      | stores its character code in the current cell.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   6   | ?    | Prints the current cell value in its verbatim numeric
;;       |      | form to the standard output.
;;       |      |------------------------------------------------------
;;       |      | This operation an advenient operation not desumed
;;       |      | from brainfuck.
;;   ..................................................................
;;   7   | !    | Queries the standard input for a signed or unsigned
;;       |      | integer number, contingently wraps it around into the
;;       |      | unsigned byte range [0, 255], and stores the octet
;;       |      | value in the current cell.
;;       |      |------------------------------------------------------
;;       |      | This operation an advenient operation not desumed
;;       |      | from brainfuck.
;;   ..................................................................
;;   8   | [    | If the current cell value equals zero (0), moves the
;;       |      | instruction pointer (IP) forward to the position
;;       |      | immediately succeeding the matching "[" command.
;;       |      | Otherwise proceeds as usual.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   9   | ]    | If the current cell value does not equal zero (0),
;;       |      | moves the instruction pointer (IP) back to the
;;       |      | position immediately succeeding the matching "["
;;       |      | command. Otherwise proceeds as usual.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   10  | <    | Moves the cell pointer one step to the left.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ..................................................................
;;   11  | >    | Moves the cell pointer one step to the right.
;;       |      |------------------------------------------------------
;;       |      | This operation constitutes a verbatim appropriation
;;       |      | from brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The simplicity of Twocoman and its forbisens being vouchsafed by its
;; brainfuck heritage vanquish most ambiguities' imminences; a subset of
;; the few remnants, however, shall yet be adduced.
;; 
;; == WHICH FORMATS MAY THE SOURCE CODE ASSUME? ==
;; Twocoman's binary command system, its bifurcation realized in the
;; transition betwixt modes and the execution of the currently active
;; one, redes a natural commission in a bit string format. On the other
;; hand, the protolog's examples furnish for any binary program an
;; paregal in a hexadecimal formulation.
;; 
;; The inquisition is thus begotten into whether both, or even
;; additional, expressions of the binary substrate are homologated.
;; 
;; It has been adjudged, with a conspectuity upon the binary and
;; hexadecimal forbisens, as well as the presence of the mode
;; representations in a token guise akin to brainfuck, to administer
;; tolerance to three source code formats, their distinguishment at the
;; time of evaluation constituting the implementation-dependent ambitus
;; of the responsible interpreter:
;; 
;;   (1) BINARY FORM:
;;       The Twocoman's acrolet, all commands are expressed as a pure
;;       sequence of binary digits, without any further content's
;;       participation.
;;   
;;   (2) HEXADECIMAL FORM:
;;       The command bits may be expressed as a sequence of zero or more
;;       hexadecimal digits, replicating in a sinistrodexal procession
;;       from the most significant bits (MSB) to the least significant
;;       ones (LSB) the binary digits.
;;       No whitespaces nor other content is homologated to intervene in
;;       this representation, too.
;;   
;;   (3) MODE IDENTIFIER FORM:
;;       The commands may be expressed in the form of their eleven mode
;;       identifiers, proceeding similiter to brainfuck, and admitting
;;       commentary tokens by non-command characters.
;; 
;; Merely the first variant's (1) adit imposes a requisite for any
;; conformant implementation to pursuit, as the orra twain attains at
;; best a convenience's role.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-26
;; 
;; Sources:
;;   [esolang2023Twocoman]
;;   The Esolang contributors, "Twocoman", June 22nd, 2023
;;   URL: "https://esolangs.org/wiki/Twocoman"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type specifier macros.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest type-parameters) &body body)
  "Defines a new derived type based upon the ``satisfies'' mechanism,
   the agnomination being specified by the TYPE-NAME, the parameter list
   through the optional TYPE-PARAMETERS, and whose implementation is
   realized by the BODY forms, the same are admitted access to the
   probed object norned by the CANDIDATE-VARIABLE, where the desinent
   BODY form's primary result is expected to return a non-``NIL'' object
   if the subject fulfils the predicate, otherwise to respond with
   ``NIL''."
  (let ((predicate-variable-name (gensym)))
    (declare (type symbol predicate-variable-name))
    `(deftype ,type-name (,@type-parameters)
      ,(if (stringp (first body))
         (pop body)
         (format NIL "Defines the derived type ``~a''." type-name))
       (let ((,predicate-variable-name (gensym)))
         (declare (type symbol ,predicate-variable-name))
         (setf (symbol-function ,predicate-variable-name)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype mode ()
  "The ``mode'' type enumerates the recognized variants of Twocoman
   program modes."
  '(member
    :initial
    :increment
    :decrement
    :output-character
    :input-character
    :output-number
    :input-number
    :jump-forward
    :jump-back
    :move-left
    :move-right))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which conforms to the ELEMENT-TYPE, defaulting to the
   generic placeholder ``*'''."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (loop
        for    element of-type T in (the list candidate)
        always (typep element element-type)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``hash-table'' of type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic placeholder ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and
          (or (eq    key-type   '*)
              (typep key        key-type))
          (or (eq    value-type '*)
              (typep value      value-type))))))

;;; -------------------------------------------------------

(deftype command-provider ()
  "The ``command-provider'' type defines a niladic function responsible
   for the delivery of Twocoman commands in the form of bits, with the
   ``NIL'' value acting as a sentinel for the Twocoman source code's
   exhaustion, thus conforming to the signature:
     lambda () => (or null bit)
   ---
   A tantamount of an iterator, a ``command-provider'' is usually
   elicited by a lexer's inquisition."
  '(function () (or null bit)))

;;; -------------------------------------------------------

(deftype command-provider-function ()
  "The ``command-provider-function'' type defines a ``command-provider''
   alternative for contexts inadmissible to detailed functional type
   specifiers, such as return values and ``loop'' type designators."
  'function)

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, and thus spanning the integral range [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of mode operations.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-next-mode (current-mode)
  (:documentation
    "Returns the subsequent mode to the CURRENT-MODE."))

;;; -------------------------------------------------------

(defmacro define-next-mode (current-mode next-mode)
  "Defines an implementation of the generic function ``get-next-mode''
   in a convenient manner, utilizing for the aefauld parameter an
   automatically generated name, accompanied by the dispatching
   CURRENT-MODE as an ``eql'' specializer, and returning the NEXT-MODE
   as the assembled operation's result."
  (let ((current-mode-variable (gensym)))
    (declare (type symbol current-mode-variable))
    `(defmethod get-next-mode
         ((,current-mode-variable (eql ,current-mode)))
       (declare (type mode ,current-mode-variable))
       (declare (ignore    ,current-mode-variable))
       (the mode ,next-mode))))

;;; -------------------------------------------------------

(define-next-mode :initial          :increment)
(define-next-mode :increment        :decrement)
(define-next-mode :decrement        :output-character)
(define-next-mode :output-character :input-character)
(define-next-mode :input-character  :output-number)
(define-next-mode :output-number    :input-number)
(define-next-mode :input-number     :jump-forward)
(define-next-mode :jump-forward     :jump-back)
(define-next-mode :jump-back        :move-left)
(define-next-mode :move-left        :move-right)
(define-next-mode :move-right       :initial)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of mode-to-binary converter.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mode-name-p (candidate)
  "Determines whether the CANDIDATE represents a Twocoman mode name,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "x+-.,?![]<>" :test #'char=)))))

;;; -------------------------------------------------------

(defun get-distance-betwixt-modes (source-mode destination-mode)
  "Returns the number of switches necessary to change from the
   SOURCE-MODE to the DESTINATION-MODE."
  (declare (type mode source-mode))
  (declare (type mode destination-mode))
  (the (integer 0 11)
    (loop
      for current-mode
        of-type mode
        =       source-mode
        then    (get-next-mode current-mode)
      until
        (eq current-mode destination-mode)
      count 1
        into distance
      finally
        (return distance))))

;;; -------------------------------------------------------

(defun binary-encode-modes (mode-sequence)
  "Encodes the MODE-SEQUENCE, composed of mode identifiers, into the an
   equivalent binary string and returns the same."
  (declare (type string mode-sequence))
  (the string
    (with-output-to-string (binary-string)
      (declare (type string-stream binary-string))
      (let ((current-mode :initial))
        (declare (type mode current-mode))
        (flet
            ((switch-to-mode (destination-mode)
              "Switches from the CURRENT-MODE to the DESTINATION-MODE,
               while concomitantly printing an equinumerant instances of
               the zero-bit character \"0\" to the BINARY-STRING, and
               returns no value."
              (declare (type mode destination-mode))
              (loop
                repeat
                  (get-distance-betwixt-modes
                    current-mode destination-mode)
                do
                  (write-char #\0 binary-string))
              (setf current-mode destination-mode)
              (values))
             
             (execute-command ()
              "Prints the one-bit charcter \"1\", accompassing the
               CURRENT-MODE's execution, to the BINARY-STRING, and
               returns no value."
              (write-char #\1 binary-string)
              (values)))
          
          (loop for token of-type character across mode-sequence do
            (case token
              (#\x
                (switch-to-mode :initial)
                (execute-command))
              
              (#\+
                (switch-to-mode :increment)
                (execute-command))
              
              (#\-
                (switch-to-mode :decrement)
                (execute-command))
              
              (#\.
                (switch-to-mode :output-character)
                (execute-command))
              
              (#\,
                (switch-to-mode :input-character)
                (execute-command))
              
              (#\?
                (switch-to-mode :output-number)
                (execute-command))
              
              (#\!
                (switch-to-mode :input-number)
                (execute-command))
              
              (#\[
                (switch-to-mode :jump-forward)
                (execute-command))
              
              (#\]
                (switch-to-mode :jump-back)
                (execute-command))
              
              (#\<
                (switch-to-mode :move-left)
                (execute-command))
              
              (#\>
                (switch-to-mode :move-right)
                (execute-command))
              
              (otherwise
                NIL))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Twocoman program.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Twocoman-Program ()
  ((commands
    :initarg       :commands
    :initform      (error "Missing commands.")
    :reader        get-commands
    :type          (vector mode *)
    :documentation "A vector composed of zero or more commands, each
                    represented by a mode state."))
  (:documentation
    "The ``Twocoman-Program'' class represents an executable sequence of
     Twocoman commands based upon a vector."))

;;; -------------------------------------------------------

(defun make-twocoman-program (commands)
  "Creates and returns a new ``Twocoman-Program'' composed of the
   COMMANDS list."
  (declare (type (list-of mode) commands))
  (the Twocoman-Program
    (make-instance 'Twocoman-Program
      :commands
        (coerce commands
          '(simple-array mode (*))))))

;;; -------------------------------------------------------

(defun get-command-at (program position)
  "Returns the command in the Twocoman PROGRAM located at the POSITION,
   or ``NIL'' none such exists."
  (declare (type Twocoman-Program program))
  (declare (type fixnum           position))
  (with-slots (commands) program
    (declare (type (vector mode *) commands))
    (the (or null mode)
      (when (array-in-bounds-p commands position)
        (aref commands position)))))

;;; -------------------------------------------------------

(defun valid-program-position-p (program position)
  "Determines whether the POSITION designates a valid location in the
   Twocoman PROGRAM's command sequence, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (array-in-bounds-p
        (slot-value program 'commands)
        position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Lexer" interface.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ()
  (:documentation
    "The ``Lexer'' interface establishes a common foundry for all
     classes accommodated to the extraction of Twocoman commands from a
     source code string."))

;;; -------------------------------------------------------

(defgeneric get-command-provider (lexer source)
  (:documentation
    "Returns for the LEXER a niladic function which upon every
     invocation either returns the next binary command designator from
     the underlying Twocoman SOURCE, or ``NIL'' value, if the source is
     exhausted"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary string lexer.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Binary-String-Lexer (Lexer)
  ()
  (:documentation
    "The ``Binary-String-Lexer'' class accoutres a lexical analyzer
     capable of construing a sequence of bits as Twocoman commands."))

;;; -------------------------------------------------------

(defun make-binary-string-lexer ()
  "Creates and returns a new ``Binary-String-Lexer'', capable of
   extracting from a piece of Twocoman source code its bit-valued
   commands."
  (the Binary-String-Lexer
    (make-instance 'Binary-String-Lexer)))

;;; -------------------------------------------------------

(defmethod get-command-provider ((lexer  Binary-String-Lexer)
                                 (source string))
  (declare (type Binary-String-Lexer lexer))
  (declare (ignore                   lexer))
  (declare (type string              source))
  (let ((position 0))
    (declare (type fixnum position))
    (the command-provider-function
      #'(lambda ()
          (the (or null bit)
            (when (array-in-bounds-p source position)
              (prog1
                (digit-char-p (char source position) 2)
                (incf position))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hexadecimal string lexer.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Hexadecimal-String-Lexer (Lexer)
  ()
  (:documentation
    "The ``Hexadecimal-String-Lexer'' provides a lexical analyzer whose
     competence entails the evaluation of a hexadecimal digit sequence
     in its binary form as Twocoman source code."))

;;; -------------------------------------------------------

(defun make-hexadecimal-string-lexer ()
  "Creates and returns a new ``Hexadecimal-String-Lexer''."
  (the Hexadecimal-String-Lexer
    (make-instance 'Hexadecimal-String-Lexer)))

;;; -------------------------------------------------------

(defmethod get-command-provider ((lexer  Hexadecimal-String-Lexer)
                                 (source string))
  (declare (type Hexadecimal-String-Lexer lexer))
  (declare (ignore                        lexer))
  (declare (type string                   source))
  (the command-provider-function
    (get-command-provider
      (make-binary-string-lexer)
      (format NIL "~b"
        (parse-integer source :radix 16)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation mode sequence lexer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Mode-Sequence-Lexer (Lexer)
  ()
  (:documentation
    "The ``Mode-Sequence-Lexer'' class establishes a lexical analyzer
     endowed with the competence to recognize mode identifiers, akin to
     brainfuck programs, and evaluate these in lieu of the traditional
     binary representation."))

;;; -------------------------------------------------------

(defun make-mode-sequence-lexer ()
  "Creates and returns a new ``Mode-Sequence-Lexer''."
  (the Mode-Sequence-Lexer
    (make-instance 'Mode-Sequence-Lexer)))

;;; -------------------------------------------------------

(defmethod get-command-provider ((lexer  Mode-Sequence-Lexer)
                                 (source string))
  (declare (type Mode-Sequence-Lexer lexer))
  (declare (ignore                   lexer))
  (declare (type string              source))
  (the command-provider-function
    (get-command-provider
      (make-binary-string-lexer)
      (binary-encode-modes source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-program (lexer source)
  "Parses the Twocoman SOURCE code by the LEXER's adminiculum, the same
   is responsible for the token's interpretation as mode transition and
   execution instructions, and returns a ``Twocoman-Program''
   representation of the assembled commands."
  (declare (type Lexer  lexer))
  (declare (type string source))
  (let ((command-provider (get-command-provider lexer source))
        (current-mode     :initial))
    (declare (type command-provider command-provider))
    (declare (type mode             current-mode))
    (loop
      for command
        of-type (or null bit)
        =       (funcall command-provider)
      
      ;; No more commands available?
      ;; => SOURCE is exhausted.
      if (null command) do
        (loop-finish)
      
      ;; "0" command?
      ;; => Transition to the next state.
      else if (= command 0) do
        (setf current-mode
          (get-next-mode current-mode))
      
      ;; "1" command?
      ;; => Execute the current mode.
      else if (= command 1)
        collect current-mode
        into    commands
      
      ;; Any other command identifier is invalid.
      else do
        (error "Invalid command: ~s." command)
      
      finally
        (return
          (make-twocoman-program commands)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump-table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of fixnum fixnum)
    :documentation "Maps each forward jump command's position in the
                    respective Twocoman program to the matching back
                    jump location, and vice versa."))
  (:documentation
    "The ``Jump-Table'' class furnishes a bilateral mapping betwixt a
     Twocoman program's forward and back jump points, realized in terms
     of their positions among the instruction sequence, and governed by
     a hash table's castaldy."))

;;; -------------------------------------------------------

(defun make-jump-table ()
  "Creates and returns a new empty ``Jump-Table''."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table
                            forward-jump-position
                            back-jump-position)
  "Associates the FORWARD-JUMP-POSITION with the BACK-JUMP-POSITION, and
   vice versa, stores this vincula in the JUMP-TABLE, and returns no
   value."
  (declare (type Jump-Table jump-table))
  (with-slots (connections) jump-table
    (declare (type (hash-table-of fixnum fixnum) connections))
    (setf (gethash forward-jump-position connections)
          back-jump-position)
    (setf (gethash back-jump-position connections)
          forward-jump-position))
  (values))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table source-point)
  "Returns for the SOURCE-POINT the destination position as maintained
   by the JUMP-TABLE, or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (or (gethash source-point
          (slot-value jump-table 'connections))
        (error "No destination associated with the jump point ~d."
          source-point))))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Supputates and returns for the Twocoman PROGRAM the jump table."
  (declare (type Twocoman-Program program))
  (let ((jump-table          (make-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type mode   across (get-commands program)
      for position of-type fixnum from   0       by 1
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (connect-jump-points jump-table
            (pop forward-jump-points)
            position)
          (error "Unmatched back jump command at position ~d."
            position))
      finally
        (when forward-jump-points
          (error "Unmatched forward jump commands at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector of cells, realized by a hash table
                    whose keys provide the cell indices, while the
                    values maintain the byte data.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, responsible for selecting the
                    currently active unit among the CELLS by adminiculum
                    of the hash table key as the cell index."))
  (:documentation
    "The ``Memory'' class models the program memory, a bilaterally
     expanding linear sequence of unsigned-byte-valued cells, in a
     sparse fashion by aide of a hash table, the keys of which assume
     the cell indices, mapping to the octet-valued cell content."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory''."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer octet) cells))
    (declare (type integer                       pointer))
    (the octet
      (nth-value 0
        (gethash pointer cells 0)))))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value memory)
  "Stores the NEW-VALUE, contingently succeeding a wrapping into the
   valid unsigned byte range of [0, 255], in the MEMORY's current cell
   and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type (hash-table-of integer octet) cells))
    (declare (type integer                       pointer))
    (setf (gethash pointer cells 0)
          (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory)
  "Increments the MEMORY's current cell, contingently wrapping around to
   the minimum of zero (0) if the upper bourne of 255 is transcended,
   and returns no value."
  (declare (type Memory memory))
  (incf (current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory)
  "Decrements the MEMORY's current cell, contingently wrapping around to
   the maximum of 255 if the lower extremum of zero (0) is violated, and
   returns no value."
  (declare (type Memory memory))
  (decf (current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun current-cell-zero-p (memory)
  "Determines whether the MEMORY's current cell value equals zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop (current-cell memory))))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (incf (slot-value memory 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (decf (slot-value memory 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Twocoman program.")
    :reader        get-program
    :type          Twocoman-Program
    :documentation "The Twocoman program to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP) maintains the position
                    of the current command in the PROGRAM vector.")
   (jump-table
    :initform      (make-jump-table)
    :reader        get-jump-table
    :type          Jump-Table
    :documentation "Connects the forward and back jump end points in the
                    Twocoman PROGRAM.")
   (memory
    :initform      (make-memory)
    :reader        get-memory
    :type          Memory
    :documentation "Provides the program memory.")
   (program-initialized-p
    :initform      NIL
    :accessor      program-initialized-p
    :type          boolean
    :documentation "Determines whether the initial mode (\"x\") has been
                    executed at the program's inchoation --- a
                    prerequisite for the correct execution of the
                    same."))
  (:documentation
    "The ``Interpreter'' class' bailiwick encompasses the provision of
     a context for a Twocoman program's execution, as well as the latter
     wike itself."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Initializes the INTERPRETER's jump table based upon its Twocoman
   program and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-table)
    (build-jump-table
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' which operates on the
   Twocoman PROGRAM."
  (declare (type Twocoman-Program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's internally managed Twocoman
   program is exhausted, which is the case for the instruction pointer's
   transgression of the command sequence's bournes, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (valid-program-position-p
        (slot-value interpreter 'program)
        (slot-value interpreter 'ip)))))

;;; -------------------------------------------------------

(defun advance-to-next-command (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip program) interpreter
    (declare (type fixnum           ip))
    (declare (type Twocoman-Program program))
    (if (valid-program-position-p program (1+ ip))
      (incf ip)
      (setf ip
        (length
          (get-commands program)))))
  (values))

;;; -------------------------------------------------------

(defun jump-to-destination (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to currently
   reside on a jump end point, relocates the same to the opposite march,
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (ip jump-table) interpreter
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (setf ip
      (get-jump-destination jump-table ip)))
  (values))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the currently processed command from the INTERPRETER's
   program, or ``NIL'' if the same is exhausted."
  (declare (type Interpreter interpreter))
  (the (or null mode)
    (get-command-at
      (slot-value interpreter 'program)
      (slot-value interpreter 'ip))))

;;; -------------------------------------------------------

(defun ensure-initialized-program (interpreter)
  "Determines whether the INTERPRETER's program has been initialized
   already, that is, processed by initial mode, identified by the \"x\"
   token, on confirmation simply returning no value and accompassing no
   further causatum; otherwise an error of an unspecified type is
   signaled."
  (declare (type Interpreter interpreter))
  (unless (slot-value interpreter 'program-initialized-p)
    (error "You cannot execute the command ~s, ~
            as the program is not initialized."
      (get-current-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric apply-mode (mode interpreter)
  (:documentation
    "Applies the causatum associated with the MODE onto the context of
     the INTERPRETER and returns no value."))

;;; -------------------------------------------------------

(defmacro define-mode-effect (mode (mode-variable interpreter-variable)
                              &body body)
  "Defines an implementation of the generic function ``apply-mode'',
   nevening the first parameter with the MODE-VARIABLE, whose
   specializer proceeds by ``eql''-equality with the MODE, and whose
   second parameter is norned via the INTERPRETER-VARIABLE, entailing
   the BODY, and returning no value."
  `(defmethod apply-mode ((,mode-variable        (eql ,mode))
                          (,interpreter-variable Interpreter))
     (declare (type mode        ,mode-variable))
     (declare (ignorable        ,mode-variable))
     (declare (type Interpreter ,interpreter-variable))
     (declare (ignorable        ,interpreter-variable))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-mode-effect :initial (mode interpreter)
  (unless (program-initialized-p interpreter)
    (setf (program-initialized-p interpreter) T)))

;;; -------------------------------------------------------

(define-mode-effect :increment (mode interpreter)
  (ensure-initialized-program interpreter)
  (increment-current-cell
    (get-memory interpreter)))

;;; -------------------------------------------------------

(define-mode-effect :decrement (mode interpreter)
  (ensure-initialized-program interpreter)
  (decrement-current-cell
    (get-memory interpreter)))

;;; -------------------------------------------------------

(define-mode-effect :output-character (mode interpreter)
  (ensure-initialized-program interpreter)
  (write-char
    (code-char
      (current-cell
        (get-memory interpreter)))))

;;; -------------------------------------------------------

(define-mode-effect :input-character (mode interpreter)
  (ensure-initialized-program interpreter)
  (format T "~&Please input an ASCII character: ")
  (finish-output)
  (setf (current-cell
          (get-memory interpreter))
        (char-code
          (read-char)))
  (clear-input))

;;; -------------------------------------------------------

(define-mode-effect :output-number (mode interpreter)
  (ensure-initialized-program interpreter)
  (format T " ~d"
    (current-cell
      (get-memory interpreter))))

;;; -------------------------------------------------------

(define-mode-effect :input-number (mode interpreter)
  (ensure-initialized-program interpreter)
  (format T "Please input an integer number: ")
  (finish-output)
  (setf (current-cell
          (get-memory interpreter))
        (parse-integer
          (read-line)))
  (clear-input))

;;; -------------------------------------------------------

(define-mode-effect :jump-forward (mode interpreter)
  (ensure-initialized-program interpreter)
  (when (current-cell-zero-p
          (get-memory interpreter))
    (jump-to-destination interpreter)))

;;; -------------------------------------------------------

(define-mode-effect :jump-back (mode interpreter)
  (ensure-initialized-program interpreter)
  (unless (current-cell-zero-p
            (get-memory interpreter))
    (jump-to-destination interpreter)))

;;; -------------------------------------------------------

(define-mode-effect :move-left (mode interpreter)
  (ensure-initialized-program interpreter)
  (move-cell-pointer-left
    (get-memory interpreter)))

;;; -------------------------------------------------------

(define-mode-effect :move-right (mode interpreter)
  (ensure-initialized-program interpreter)
  (move-cell-pointer-right
    (get-memory interpreter)))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (program-exhausted-p interpreter) do
    (apply-mode
      (get-current-command interpreter)
      interpreter)
    (advance-to-next-command interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of main operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Binary-String-Lexer      +BINARY-LEXER+))
(declaim (type Hexadecimal-String-Lexer +HEXADECIMAL-LEXER+))
(declaim (type Mode-Sequence-Lexer      +MODE-LEXER+))

;;; -------------------------------------------------------

(defparameter +BINARY-LEXER+
  (make-binary-string-lexer)
  "The default Twocoman source code lexer, based upon the consumption
   of a binary string.")

(defparameter +HEXADECIMAL-LEXER+
  (make-hexadecimal-string-lexer)
  "An alternative Twocoman source code lexer, based upon the program's
   provision in the form of hexadecimal digits.")

(defparameter +MODE-LEXER+
  (make-mode-sequence-lexer)
  "A Twocoman source code lexer capable of resolving the mode
   identifiers directly, akin to a brainfuck interpreter, in order to
   accompass a simulacrum of the original binary program design.")

;;; -------------------------------------------------------

(defun interpret-Twocoman (code &optional (lexer +BINARY-LEXER+))
  "Interprets the piece of Twocoman source CODE, optionally superseding
   the default binary string lexical analyzer by a custom LEXER, and
   returns no value."
  (declare (type string code))
  (declare (type Lexer  lexer))
  (interpret-program
    (make-interpreter
      (parse-program lexer code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!" following the standard binary format.
(interpret-Twocoman
  "1010000001000000110000000010001000001000110010000000001000111110000000110000000000101000011000000010000111000000100100010100000001110010010000000110000110000000001110000001000000010000000100000000010111100000100000000011100100000000001111110100000011000010100000001111001001")

;;; -------------------------------------------------------

;; Print "Hello, World!" utilizing the hexadecimal format.
(interpret-Twocoman
  "A040C02208C8023E0300286021C09140724061803810101005E08039003F40C280F24"
  +HEXADECIMAL-LEXER+)

;;; -------------------------------------------------------

;; Print "Hello, World!" utilizing the immediate mode identifiers.
(interpret-twocoman
  "x+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."
  +MODE-LEXER+)

;;; -------------------------------------------------------

;; Repeating cat program which terminates for a "null character" input.
(interpret-Twocoman "101000000100000000100000000001000001")

;;; -------------------------------------------------------

;; Repeating cat program which terminates for a "null character" input,
;; committed as a hexadecimal string.
(interpret-Twocoman "A04020041" +HEXADECIMAL-LEXER+)

;;; -------------------------------------------------------

;; Repeating cat program which terminates for a "null character" input,
;; committed as a sequence of mode identifiers.
(interpret-Twocoman "x+[,.]" +MODE-LEXER+)

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Twocoman "100000010100000000010001000000001")

;;; -------------------------------------------------------

;; Truth-machine provides in hexadecimal form.
(interpret-Twocoman "814011008" +HEXADECIMAL-LEXER+)

;;; -------------------------------------------------------

;; Truth-machine provided as a mode sequence.
(interpret-Twocoman "x![?]?" +MODE-LEXER+)
