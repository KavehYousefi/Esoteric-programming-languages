;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Turin", invented by the Esolang user "Xyzzy" and presented
;; on July 1st, 2022, the haecceity commorant in the same subscribes to
;; the mimicry of a throughout rudimentary Turing machine deviation,
;; its homologation's compass an aefauld state in conjunction with a
;; bilateral infinite tape of bit-valued cells, on which operates a
;; quadruple repertoire of instructions.
;; 
;; 
;; Concept
;; =======
;; The Turin programming language's provenance is begotten from a very
;; curtailed ilk of the Turing machine concept's entelechy, siccan
;; specimen maintains an aefauld state's participation, a bilaterally
;; infinite tape of bit-valued cells, and, for this singularity in
;; status, a sequence of operations desumed from a quartet of
;; contingencies.
;; 
;; == A TURIN PROGRAM IS COMPOSED OF HEADER AND RULES ==
;; A Turin program's composition enumerates a twissel of components:
;; the header section, necessitated for the input and output mode
;; configurations, and the rule segment, whose existence accommodates
;; the commorancy of the state-symbol combinations as antecedents for
;; their specified instructions' execution.
;; 
;; == THE HEADER DEFINES THE INPUT AND OUTPUT MODES ==
;; The dever appertaining to the input and output data's interpretation
;; constitutes the first program section's bailiwick: the header parcel.
;; 
;; The input mode, incited in its recognition by the "IN" keyword',
;; appertains to the format and content recognized during an optional
;; input request stage ere a Turin program's execution, invested in the
;; telos of the binary tape's initialization. The respecitvely construed
;; response string experiences a transformation into a binary sequence,
;; pevenient as the ultimity of its transfer unto the tape.
;; 
;; The following input mode configurations enjoy their admissibility:
;; 
;;   ------------------------------------------------------------------
;;   Input mode | Effect
;;   -----------+------------------------------------------------------
;;   IN ASCII   | The user input is interpreted as a sequence of zero
;;              | or more ASCII characters, the character codes of
;;              | which, in their 8-bit binary form, are to be
;;              | transferred into the tape.
;;   ..................................................................
;;   IN BIN     | The user input is interpreted as a sequence of zero
;;              | or more binary digits, intended for their immediate
;;              | transfer into the tape.
;;   ..................................................................
;;   IN HEX     | The user input is interpreted as a sequence of zero
;;              | or more hexadecimal digits, the 4-bit binary
;;              | equivalents of which are to be transferred into the
;;              | tape.
;;   ------------------------------------------------------------------
;; 
;; The athwart communication's foundry ensues from the output mode
;; diorism, incited by the "OUT" keyword, and intended for an optional
;; output action --- usually as an optional step ere the program's
;; closure ---, inwith which context the tape's content is interpreted
;; in accordance with a specific format and displayed on the standard
;; output.
;; 
;; The tabulation below intrines the possible output mode settings:
;; 
;;   ------------------------------------------------------------------
;;   Input mode | Effect
;;   -----------+------------------------------------------------------
;;   OUT ASCII  | Each eight consecutive tape bits shall be interpreted
;;              | as an unsigned byte ASCII code, replicating the thus
;;              | produced character message on the standard output.
;;   ..................................................................
;;   OUT BIN    | The tape bits shall be printed in an ipsissima verba
;;              | fashion, as binary digits, to the standard output.
;;   ..................................................................
;;   OUT HEX    | Each four consecutive tape bits shall be interpreted
;;              | as a binary representation of a hexadecimal digit,
;;              | replicating the catena of these base-16 symbols on
;;              | the standard output.
;;   ------------------------------------------------------------------
;; 
;; == RULES: STATE-SYMBOL COMBINATIONS THAT INSTIGATE OPERATIONS ==
;; The second moeity partaking in a program's patration relate to the
;; Turin machine rules' specifications.
;; 
;; A rule, in compliance with this language's construe, imposes the
;; coefficiency of a state name and a bit-valued symbol, ensuing from
;; its activation a sequence of zero or more operations shall actuate.
;; 
;; The thus requisite forbisen limns the following:
;; 
;;   {stateName}~{symbol}:{instructions}
;; 
;; Where {stateName} norns the state whose concurrence with the current
;; state machine's state shall provide one moiety of the antecedent's
;; fulfilment; while the {symbol}, a bit, realizes the predicate's
;; patration if assuming the same value as the current tape cell's
;; content. A succedent from the state-symbol conflation's
;; ascertainment, the {instructions} will be execute.
;; 
;; The Turin language, as its current iteration, wists of one default
;; state only, whose modulation potential's lacuna renders it a fact of
;; inevitable acceptance: the "START" state. As a consectary, if no
;; rule is specified with this parcel of the requisitum, any causatum
;; is forfeited.
;; 
;; == THE PROGRAM MEMORY: AN INFINITE TAPE OF BITS ==
;; The tape aspect of the abstract Turing machine in this language
;; answers to a bilaterally infinite tape of bit-valued cells, upon
;; the same a mobile head, or "pointer", operates in order to designate
;; the currently active unit, its capacitations' circumference the
;; perquisition and modification of the content with the tokens of
;; currency embracing "0" and "1" digits.
;; 
;; 
;; Syntax
;; ======
;; Turin's syntactical department enumerates a twofold componency,
;; producing a header and a rule set.
;; 
;; == GRAMMAR ==
;; A more stringent tier of formality shall be imparted on the
;; language's donat by adminiculum of an Extended Backus-Naur Form
;; (EBNF) description:
;; 
;;   program            := header , ruleSet ;
;;   header             := ( inputMode  , outputMode )
;;                      |  ( outputMode , inputMode  )
;;                      ;
;;   inputMode          := "IN"  , ioMode ;
;;   outputMode         := "OUT" , ioMode ;
;;   ioMode             := "ASCII" | "BIN" | "HEX" ;
;;   ruleSet            := { rule } ;
;;   rule               := stateName , "~" , bit , commandSection ;
;;   commandSection     := ":" , { command } ;
;;   command            := ">" | "<" | "0" | "1" ;
;;   stateName          := firstStateNameChar , { innerStateNameChar } ;
;;   firstStateNameChar := letter | nonBinaryDigit | "_" ;
;;   innerStateNameChar := letter | digit          | "_" ;
;;   letter             := "a" | ... | "z" | "A" | ... | "Z" ;
;;   nonBinaryDigit     := "2" | "3" | "4" "5" | "6" | "7" | "8" | "9" ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;; 
;; 
;; Instructions
;; ============
;; A quadruple membership already exhausts the operative contingency
;; apportioned to Turin's competences, and accommodated for the pursuit
;; of the tape's navigation and its bit-valued cells' modulation.
;; 
;; == OVERVIEW ==
;; The following apercu's investment shall be the communication of the
;; operations' causata.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the tape's pointer one step to the right.
;;   ..................................................................
;;   <       | Translates the tape's pointer one step to the left.
;;   ..................................................................
;;   0       | Writes the bit value zero (0) into the tape cell
;;           | amenable to the tape's pointer.
;;   ..................................................................
;;   1       | Writes the bit value zero (0) into the tape cell
;;           | amenable to the tape's pointer.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-06-01
;; 
;; Sources:
;;   [esolang2022Turin]
;;   The Esolang contributors, "Turin", July 1st, 2022
;;   URL: "https://esolangs.org/wiki/Turin"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype io-mode ()
  "The ``io-mode'' type enumerates the recognized variation on input and
   output interpretation modes."
  '(member :ascii :binary :hexadecimal))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism appertaining to which enumerates, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variants on Turin
   operations."
  '(member
    :move-pointer-right
    :move-pointer-left
    :write-0
    :write-1))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the same
   defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq candidate '*)
              (every
                #'(lambda (element)
                    (declare (type T element))
                    (typep element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-list ()
  "The ``command-list'' type defines a list of Turin commands."
  '(list-of command))

;;; -------------------------------------------------------

(deftype rule-set ()
  "The ``rule-set'' type defines a sequence of zero or more Turin rules
   as a list of ``Rule'' instances."
  '(list-of Rule))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, both defaulting to the generic sentinel
   ``*''."
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
                (and
                  (or (eq key-type '*)
                      (typep key key-type))
                  (or (eq value-type '*)
                      (typep value value-type)))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a veridical Boolean equivalent of the OBJECT, returning for
   a non-``NIL'' input a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-integer-bit (integer bit-position)
  "Returns the bit value stored in the INTEGER-encoded bit sequence
   amenable to the BIT-POSITION."
  (declare (type integer       integer))
  (declare (type (integer 0 *) bit-position))
  (the bit
    (or (and (logbitp bit-position integer) 1)
        0)))

;;; -------------------------------------------------------

(defun make-empty-bit-vector (size)
  "Creates and returns a fresh simple bit vector of the specified SIZE,
   its constituents' entirety being initialized to zero (0) bits."
  (declare (type fixnum size))
  (the simple-bit-vector
    (make-array size
      :element-type    'bit
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun copy-integer-bits (target-vector
                          target-start-index
                          source-bits
                          number-of-elements)
  "Transfers the lowest NUMBER-OF-ELEMENTS tally of elements from the
   SOURCE-BITS into the TARGET-VECTOR, proceeding in the latter from the
   inclusive TARGET-START-INDEX and advancing along the positive axis,
   and returns the modified TARGET-VECTOR."
  (declare (type bit-vector    target-vector))
  (declare (type fixnum        target-start-index))
  (declare (type integer       source-bits))
  (declare (type (integer 0 *) number-of-elements))
  (loop
    for target-index
      of-type fixnum
      from    target-start-index
    and source-index
      of-type fixnum
      from    (1- number-of-elements)
      downto  0
    do
      (setf (bit target-vector target-index)
        (get-integer-bit source-bits source-index)))
  (the bit-vector target-vector))

;;; -------------------------------------------------------

(defun copy-character-code-bits (target-vector
                                 target-start-index
                                 character)
  "Copies the bits of the CHARACTER's ASCII code in its binary
   representation to the TARGET-VECTOR, proceeding from the inclusive
   TARGET-START-INDEX in the latter, while advancing along the positive
   index range, and returns the modified TARGET-VECTOR."
  (declare (type bit-vector target-vector))
  (declare (type fixnum     target-start-index))
  (declare (type character  character))
  (the bit-vector
    (copy-integer-bits target-vector target-start-index
      (char-code character)
      8)))

;;; -------------------------------------------------------

(defun copy-string-code-bits (target-vector
                              target-start-index
                              text)
  "Copies the bits of the TEXT characters' ASCII codes in their binary
   representations to the TARGET-VECTOR, proceeding from the inclusive
   TARGET-START-INDEX in the latter, while advancing along the positive
   index range, and returns the modified TARGET-VECTOR."
  (declare (type bit-vector target-vector))
  (declare (type fixnum     target-start-index))
  (declare (type string     text))
  (loop
    for input-character
      of-type character
      across  text
    and target-index
      of-type fixnum
      =       target-start-index
      then    (+ target-index 8)
    do
      (copy-character-code-bits target-vector
                                target-index
                                input-character))
  (the bit-vector target-vector))

;;; -------------------------------------------------------

(defun copy-hexadecimal-string-bits (target-vector
                                     target-start-index
                                     hexadecimal-string)
  "Copies the bits of the HEXADECIMAL-STRING in its binary
   representation to the TARGET-VECTOR, proceeding from the inclusive
   TARGET-START-INDEX in the latter, while advancing along the positive
   index range, and returns the modified TARGET-VECTOR."
  (declare (type bit-vector target-vector))
  (declare (type fixnum     target-start-index))
  (declare (type string     hexadecimal-string))
  (loop
    for hexadecimal-digit
      of-type character
      across  hexadecimal-string
    and target-index
      of-type fixnum
      =       target-start-index
      then    (+ target-index 4)
    do
      (copy-integer-bits target-vector target-index
        (digit-char-p hexadecimal-digit 16)
        4))
  (the bit-vector target-vector))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of header.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Header ()
  ((input-mode
    :initarg       :input-mode
    :type          io-mode
    :documentation "The format in concord with which input shall be
                    interpreted and subsequently transferred unto the
                    program tape.")
   (output-mode
    :initarg       :output-mode
    :type          io-mode
    :documentation "The format in concord with which the program tape's
                    content shall be interpreted and subsequently
                    printed."))
  (:documentation
    "The ``Header'' class encapsulates the configurations established
     in a Turin program's header, scilicet, the input and output
     modes."))

;;; -------------------------------------------------------

(defun header-complete-p (header)
  "Determines whether the Turin HEADER is complete, which partakes of
   veridicality if both the input and output modes are set, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Header header))
  (the boolean
    (get-boolean-value-of
      (and
        (slot-boundp header 'input-mode)
        (slot-boundp header 'output-mode)))))

;;; -------------------------------------------------------

(defun validate-header (header)
  "Determines whether the Turin HEADER is complete, returning on
   confirmation the HEADER itself; otherwise signals an error of an
   unspecified type."
  (declare (type Header header))
  (the Header
    (or (and (header-complete-p header) header)
        (error "The program header is incomplete."))))

;;; -------------------------------------------------------

(defun get-input-mode (header)
  "Returns the HEADER's input mode, or signals an error of an
   unspecified type upon its state being undefined."
  (declare (type Header header))
  (the io-mode
    (if (slot-boundp header 'input-mode)
      (slot-value header 'input-mode)
      (error "No input mode is set for the header."))))

;;; -------------------------------------------------------

(defun set-input-mode (header new-input-mode)
  "Sets the HEADER's input mode to the NEW-INPUT-MODE, if not already
   defined, and returns no value; otherwise signals an error of an
   unspecified type."
  (declare (type Header  header))
  (declare (type io-mode new-input-mode))
  (if (slot-boundp header 'input-mode)
    (error "Cannot redefined the header's input mode with ~s."
      new-input-mode)
    (setf (slot-value header 'input-mode) new-input-mode))
  (values))

;;; -------------------------------------------------------

(defun get-output-mode (header)
  "Returns the HEADER's output mode, or signals an error of an
   unspecified type upon its state being undefined."
  (declare (type Header header))
  (the io-mode
    (if (slot-boundp header 'output-mode)
      (slot-value header 'output-mode)
      (error "No output mode is set for the header."))))

;;; -------------------------------------------------------

(defun set-output-mode (header new-output-mode)
  "Sets the HEADER's output mode to the NEW-OUTPUT-MODE, if not already
   defined, and returns no value; otherwise signals an error of an
   unspecified type."
  (declare (type Header  header))
  (declare (type io-mode new-output-mode))
  (if (slot-boundp header 'output-mode)
    (error "Cannot redefined the header's output mode with ~s."
      new-output-mode)
    (setf (slot-value header 'output-mode) new-output-mode))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((header Header) (stream T))
  (declare (type Header      header))
  (declare (type destination stream))
  (format stream "(Header :input-mode ~s :output-mode ~s)"
    (slot-value header 'input-mode)
    (slot-value header 'output-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Rule ()
  ((activating-state
    :type          string
    :documentation "The state requisite for this rule's activation.")
   (activating-symbol
    :type          bit
    :documentation "The symbol requisite for this rule's activation.")
   (commands
    :initform      NIL
    :type          command-list
    :documentation "The Turin commands to execute upon this rule's
                    activation."))
  (:documentation
    "The ``Rule'' class provides an encapsulation of a Turin rule,
     compact of its activating state and symbol twissel, in conjunction
     with a sequence of zero or more commands to execute upon the
     rule's activation instigation."))

;;; -------------------------------------------------------

(defun get-activating-state (rule)
  "Returns the state capacitated to active the RULE."
  (declare (type Rule rule))
  (the string
    (slot-value rule 'activating-state)))

;;; -------------------------------------------------------

(defun set-activating-state (rule activating-state)
  "Sets the RULE's expected state to the ACTIVATING-STATE and returns no
   value."
  (declare (type Rule   rule))
  (declare (type string activating-state))
  (setf (slot-value rule 'activating-state) activating-state)
  (values))

;;; -------------------------------------------------------

(defun get-activating-symbol (rule)
  "Returns the symbol capacitated to active the RULE."
  (declare (type Rule rule))
  (the bit
    (slot-value rule 'activating-symbol)))

;;; -------------------------------------------------------

(defun set-activating-symbol (rule activating-symbol)
  "Sets the RULE's expected symbol to the ACTIVATING-SYMBOL and returns
   no value."
  (declare (type Rule rule))
  (declare (type bit  activating-symbol))
  (setf (slot-value rule 'activating-symbol) activating-symbol)
  (values))

;;; -------------------------------------------------------

(defun get-activated-commands (rule)
  "Returns the commands whose execution ensues from the RULE's
   activation."
  (declare (type Rule rule))
  (the command-list
    (slot-value rule 'commands)))

;;; -------------------------------------------------------

(defun set-activated-commands (rule commands)
  "Specifies the COMMANDS to execute upon the RULE's activation and
   returns no value."
  (declare (type Rule         rule))
  (declare (type command-list commands))
  (setf (slot-value rule 'commands) commands)
  (values))

;;; -------------------------------------------------------

(defun rule-matches-p (rule probed-state probed-symbol)
  "Determines whether the RULE's activating state and symbol comply with
   the PROBED-STATE and PROBED-SYMBOL, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Rule   rule))
  (declare (type string probed-state))
  (declare (type bit    probed-symbol))
  (the boolean
    (get-boolean-value-of
      (and
        (string= (get-activating-state  rule) probed-state)
        (=       (get-activating-symbol rule) probed-symbol)))))

;;; -------------------------------------------------------

(defmethod print-object ((rule Rule) (stream T))
  (declare (type Rule        rule))
  (declare (type destination stream))
  (format stream "(Rule :activating-state ~s ~
                        :activating-symbol ~d ~
                        :commands ~a)"
    (slot-value rule 'activating-state)
    (slot-value rule 'activating-symbol)
    (slot-value rule 'commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Turin program.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Program ()
  ((header
    :initarg       :header
    :initform      (error "Missing program header.")
    :type          Header
    :documentation "Imposes the input and output conduit modes.")
   (rules
    :initarg       :rules
    :initform      (error "Missing program rule set.")
    :type          rule-set
    :documentation "Establishes the activation rules as an aggregate of
                    activating state and symbol engaged in a champarty
                    with the consequent commands to execute."))
  (:documentation
    "The ``Program'' class serves in the ensconcement of a Turin
     program's configurations and rules."))

;;; -------------------------------------------------------

(defun get-program-header (program)
  "Returns the Turin PROGRAM's header."
  (declare (type Program program))
  (the Header
    (slot-value program 'header)))

;;; -------------------------------------------------------

(defun get-program-rules (program)
  "Returns the Turin PROGRAM's rule set."
  (declare (type Program program))
  (the rule-set
    (slot-value program 'rules)))

;;; -------------------------------------------------------

(defun find-rule (program current-state current-symbol)
  "Returns the first rule in the PROGRAM whose activation stipulations
   concur with the CURRENT-STATE and CURRENT-SYMBOL, or returns ``NIL''
   upon its disrespondency."
  (declare (type Program program))
  (declare (type string  current-state))
  (declare (type bit     current-symbol))
  (the (or null Rule)
    (find-if
      #'(lambda (probed-rule)
          (declare (type Rule probed-rule))
          (rule-matches-p probed-rule current-state current-symbol))
      (slot-value program 'rules))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Tape integer bit) (values)) set-tape-symbol))

;;; -------------------------------------------------------

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer bit)
    :documentation "A sparse vector of bit-valued cells, infinite along
                    both axes.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The tape head, designated by the current key into
                    the cells hash table.")
   (minimum-index
    :initform      0
    :type          integer
    :documentation "The smallest cell index actually modified, utilized
                    for output purposes.")
   (maximum-index
    :initform      0
    :type          integer
    :documentation "The largest cell index actually modified, utilized
                    for output purposes."))
  (:documentation
    "The ``Tape'' class implements a bilaterally infinite tape of
     bit-valued cells, operating upon by a pointer."))

;;; -------------------------------------------------------

(defun make-empty-tape ()
  "Creates and returns a fresh ``Tape'' the cells of which all assume
   the default symbol of zero (0)."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun make-tape-of (start-index initial-symbols)
  "Creates and returns a fresh ``Tape'' whose cells, proceeding from the
   inclusive START-INDEX and advancing along the positive axis, assume
   the INITIAL-SYMBOLS, while the area unoccupied by this content
   retains the default state of zero (0)."
  (declare (type integer    start-index))
  (declare (type bit-vector initial-symbols))
  (let ((tape (make-empty-tape)))
    (declare (type Tape tape))
    (loop
      for input-symbol of-type bit     across initial-symbols
      and cell-index   of-type integer from   start-index by 1
      do  (set-tape-symbol tape cell-index input-symbol))
    (the Tape tape)))

;;; -------------------------------------------------------

(defun include-tape-cell-index (tape new-index)
  "Adjusts the TAPE's minimum and maximum cell indices with respect to
   the NEW-INDEX to include and returns no value."
  (declare (type Tape    tape))
  (declare (type integer new-index))
  (with-slots (minimum-index maximum-index) tape
    (declare (type integer minimum-index))
    (declare (type integer maximum-index))
    (setf minimum-index (min minimum-index new-index))
    (setf maximum-index (max maximum-index new-index)))
  (values))

;;; -------------------------------------------------------

(defun update-tape-bournes (tape)
  "Adjusts the TAPE's minimum and maximum cell indices with respect to
   the current pointer location and returns no value."
  (declare (type Tape tape))
  (include-tape-cell-index tape
    (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defun move-tape-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'pointer))
  (update-tape-bournes tape)
  (values))

;;; -------------------------------------------------------

(defun move-tape-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (slot-value tape 'pointer))
  (update-tape-bournes tape)
  (values))

;;; -------------------------------------------------------

(defun get-tape-symbol (tape index)
  "Returns the bit stored in the TAPE cell located at the INDEX."
  (declare (type Tape     tape))
  (declare (type integer index))
  (include-tape-cell-index tape index)
  (the bit
    (gethash index (slot-value tape 'cells) 0)))

;;; -------------------------------------------------------

(defun set-tape-symbol (tape index new-symbol)
  "Stores the NEW-SYBMOL in the TAPE cell located at the INDEX and
   returns no value."
  (declare (type Tape     tape))
  (declare (type integer index))
  (declare (type bit     new-symbol))
  (include-tape-cell-index tape index)
  (setf (gethash index (slot-value tape 'cells) 0) new-symbol)
  (values))

;;; -------------------------------------------------------

(defun set-tape-symbols (tape start-index new-symbols)
  "Stores the NEW-SYMBOLS in the TAPE cells commencing with the
   inclusive START-INDEX and advancing along the positive axis and
   returns no value."
  (declare (type Tape       tape))
  (declare (type integer    start-index))
  (declare (type bit-vector new-symbols))
  (loop
    for cell-index     of-type integer from   start-index by 1
    for current-symbol of-type bit     across new-symbols
    do  (set-tape-symbol tape cell-index current-symbol))
  (values))

;;; -------------------------------------------------------

(defun get-current-tape-symbol (tape)
  "Returns the bit stored at the TAPE's pointer location."
  (declare (type Tape tape))
  (the bit
    (get-tape-symbol tape
      (slot-value tape 'pointer))))

;;; -------------------------------------------------------

(defun set-current-tape-symbol (tape new-symbol)
  "Stores the NEW-SYMBOL in the TAPE cell located at the pointer
   location and returns no value."
  (declare (type Tape tape))
  (declare (type bit  new-symbol))
  (set-tape-symbol tape (slot-value tape 'pointer) new-symbol)
  (values))

;;; -------------------------------------------------------

(defun extract-tape-bits (tape start-index number-of-bits)
  "Extracts from the TAPE the NUMBER-OF-BITS tally of consecutive bits
   proceeding from the inclusive START-INDEX and returns an
   integer-encoded bit sequence representation thereof."
  (declare (type integer       start-index))
  (declare (type (integer 0 *) number-of-bits))
  (the unsigned-byte
    (parse-integer
      (with-output-to-string (bits)
        (declare (type string-stream bits))
        (loop
          for cell-index
            of-type integer
            from    start-index
            below   (+ start-index number-of-bits)
          do
            (format bits "~d"
              (get-tape-symbol tape cell-index))))
      :radix 2)))

;;; -------------------------------------------------------

(defgeneric print-formatted-tape (tape output-mode)
  (:documentation
    "Prints the TAPE's content in compliance with the OUTPUT-MODE to the
     standard output and returns no value.")
  
  (:method ((tape Tape) (output-mode (eql :ascii)))
    (declare (type Tape    tape))
    (declare (type io-mode output-mode))
    (declare (ignore       output-mode))
    (loop
      for cell-index
        of-type integer
        from    (slot-value tape 'minimum-index)
        to      (slot-value tape 'maximum-index)
        by      8
      do
        (write-char
          (code-char
            (extract-tape-bits tape cell-index 8))))
    (values))
  
  (:method ((tape Tape) (output-mode (eql :binary)))
    (declare (type Tape    tape))
    (declare (type io-mode output-mode))
    (declare (ignore       output-mode))
    (loop
      for cell-index
        of-type integer
        from    (slot-value tape 'minimum-index)
        to      (slot-value tape 'maximum-index)
      do
        (format T "~d"
          (get-tape-symbol tape cell-index)))
    (values))
  
  (:method ((tape Tape) (output-mode (eql :hexadecimal)))
    (declare (type Tape    tape))
    (declare (type io-mode output-mode))
    (declare (ignore       output-mode))
    (loop
      for cell-index
        of-type integer
        from    (slot-value tape 'minimum-index)
        to      (slot-value tape 'maximum-index)
        by      4
      do
        (format T "~x"
          (extract-tape-bits tape cell-index 4)))
    (values)))

;;; -------------------------------------------------------

(defgeneric input-into-tape (tape start-index input input-mode)
  (:documentation
    "Modifies the TAPE cells, proceeding from the inclusive START-INDEX
     and advancing along the positive axis, by the INPUT obtained
     from the standard input conduit and interpreted in concord with the
     INPUT-MODE, and returns no value.")
  
  (:method ((tape        Tape)
            (start-index integer)
            (input       string)
            (input-mode  (eql :ascii)))
    (declare (type Tape    tape))
    (declare (type integer start-index))
    (declare (type string  input))
    (declare (type io-mode input-mode))
    (declare (ignore       input-mode))
    (set-tape-symbols tape start-index
      (copy-string-code-bits
        (make-empty-bit-vector (* (length input) 8))
        0
        input))
    (values))
  
  (:method ((tape        Tape)
            (start-index integer)
            (input       string)
            (input-mode  (eql :binary)))
    (declare (type Tape    tape))
    (declare (type integer start-index))
    (declare (type string  input))
    (declare (type io-mode input-mode))
    (declare (ignore       input-mode))
    (set-tape-symbols tape start-index
      (map 'simple-bit-vector #'digit-char-p input))
    (values))
  
  (:method ((tape        Tape)
            (start-index integer)
            (input       string)
            (input-mode  (eql :hexadecimal)))
    (declare (type Tape    tape))
    (declare (type integer start-index))
    (declare (type string  input))
    (declare (type io-mode input-mode))
    (declare (ignore       input-mode))
    (set-tape-symbols tape start-index
      (copy-hexadecimal-string-bits
        (make-empty-bit-vector (* (length input) 4))
        0
        input))
    (values)))

;;; -------------------------------------------------------

(defmethod print-object ((tape Tape) (stream T))
  (declare (type Tape        tape))
  (declare (type destination stream))
  (format stream "~&Tape:")
  (loop
    for cell-index
      of-type integer
      from    (slot-value tape 'minimum-index)
      to      (slot-value tape 'maximum-index)
    do
      (format stream "~&~2t[~d] = ~d~@[*~]" cell-index
        (get-tape-symbol tape cell-index)
        (= cell-index (slot-value tape 'pointer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents an identifier character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (and (not (whitespace-character-p candidate))
           (not (find candidate "~:><" :test #'char=))))))

;;; -------------------------------------------------------

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a Turin operation
   identifier, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "><01" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-case-key-p (case-key)
  "Determines whether the CASE-KEY represents the default case sentinel
   for a ``string-case'' macro invocation's case compound, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (or string symbol) case-key))
  (the boolean
    (get-boolean-value-of
      (and (symbolp case-key)
           (member case-key '(otherwise T) :test #'eq)))))

;;; -------------------------------------------------------

(defmacro string-case (subject &rest cases)
  "Conditionally executes at most one of the CASES based upon the
   case key's equality to the evaluated SUBJECT string and returns the
   activated case body's desinent form's results.
   ---
   The SUBJECT must be any expression which resolve to a string object.
   ---
   The CASES must be a list of zero or more cases, each such itself a
   list of one or more more items, the first being interpreted as the
   case key, expected to yield a string, intended for its equiparation
   with the SUBJECT. With the parity being ascertained, the respective
   case's body forms, comprised of the zero or more elements succeeding
   the first, are evaluated in order, and its final form's results
   returned.
     As a special circumstance, at most one case key must assume the
   default sentinel ``otherwise'' or ``T'', which, upon the specific
     cases' failure, will be activated.
   A case's conformation, as a corollary, must admit the following:
     (case-key case-body-form-1 ... case-body-form-N)
   where N >= 0."
  (let ((evaluated-subject (gensym)))
    (declare (type symbol evaluated-subject))
    `(let ((,evaluated-subject ,subject))
       (declare (type string ,evaluated-subject))
       (cond
       ,@(loop for case of-type (list-of T) in cases collect
           (destructuring-bind (case-form &rest case-body) case
             (declare (type (or string symbol) case-form))
             (declare (type (list-of T)        case-body))
             (if (default-case-key-p case-form)
               `(T
                  ,@case-body)
               `((string= ,case-form ,evaluated-subject)
                  ,@case-body))))))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION into the SOURCE, or ``NIL'' if
   the index violates the admissible bournes."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position
   immediately succeeding the skipped segment."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun expect-character (source position expected-character)
  "Determines whether the character at the POSITION into the SOURCE
   exists and equals the EXPECTED-CHARACTER, returning on confirmation
   the location in the SOURCE succeeding the character and any
   contingently trailing whitespaces; otherwise signals an error of an
   unspecified type."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (let ((actual-character (get-character-at source position)))
    (declare (type (or null character) actual-character))
    (the fixnum
      (cond
        ((null actual-character)
          (error "Expected the character \"~c\" at position ~d, ~
                  but none exists at such location."
            expected-character position))
        ((char/= actual-character expected-character)
          (error "Expected the character \"~c\" at position ~d, ~
                  but encountered \"~c\"."
            expected-character position actual-character))
        (T
          (skip-whitespaces source (1+ position)))))))

;;; -------------------------------------------------------

(defun locate-end-of-identifier (source start)
  "Proceeding from the START position into the SOURCE, determines and
   returns the location succeeding the intersecting identifier in the
   latter, skipping any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'identifier-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-identifier (source start)
  "Proceeding from the START position into the SOURCE, reads an
   identifier and returns two values:
     (1) A string representation of the consumed identifier.
     (2) The position into the SOURCE succeeding the extracted
         identifier and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (locate-end-of-identifier source start)))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (subseq source start end)
        (skip-whitespaces source end)))))

;;; -------------------------------------------------------

(defun read-bit (source start)
  "Proceeding from the START position into the SOURCE, reads a binary
   number and returns two values:
     (1) A ``bit'' representation of the consumed number.
     (2) The position into the SOURCE succeeding the extracted bit
         value and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (bit-value position)
      (parse-integer source :radix 2 :start start :end (1+ start))
    (declare (type bit    bit-value))
    (declare (type fixnum position))
    (the (values bit fixnum)
      (values bit-value
        (skip-whitespaces source position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of header parser.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-io-mode (source start)
  "Proceeding from the START position into the SOURCE, consumes an
   input/output mode identifier and returns two values:
     (1) An ``io-mode'' representation of the consumed identifier.
     (2) The position into the SOURCE succeeding the detected mode
         identifier and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (identifier end)
      (read-identifier source start)
    (declare (type string identifier))
    (declare (type fixnum end))
    (the (values io-mode fixnum)
      (values
        (string-case identifier
          ("ASCII"   :ascii)
          ("BIN"     :binary)
          ("HEX"     :hexadecimal)
          (otherwise (error "No input/output mode identifier: ~s."
                       identifier)))
        end))))

;;; -------------------------------------------------------

(defun read-header-entry (source start header)
  "Proceeding from the START position into the SOURCE, reads a single
   header configuration entry, stores the respective setting in the
   HEADER, and returns the position succeeding the consumed entry and
   any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type Header header))
  (let ((direction-identifier "")
        (new-position         0))
    (declare (type string direction-identifier))
    (declare (type fixnum new-position))
    (multiple-value-setq (direction-identifier new-position)
      (read-identifier source start))
    (flet ((process-io-mode (io-mode end-position)
            "Returns the IO-MODE in its verbatim conformation, while
             concomitantly updates the NEW-POSITION to the END-POSITION,
             skipping any adjacent trailing whitespaces."
            (declare (type io-mode io-mode))
            (declare (type fixnum  end-position))
            (the io-mode
              (prog1 io-mode
                (setf new-position end-position)))))
      (string-case direction-identifier
        ("IN"
          (set-input-mode header
            (multiple-value-call #'process-io-mode
              (read-io-mode source new-position))))
        ("OUT"
          (set-output-mode header
            (multiple-value-call #'process-io-mode
              (read-io-mode source new-position))))
        (otherwise
          (error "Invalid header direction: ~s."
            direction-identifier))))
    (the fixnum new-position)))

;;; -------------------------------------------------------

(defun read-header (source start)
  "Proceeding from the START position into the SOURCE, reads the Turin
   program's header configuration and returns two values:
     (1) A fresh ``Header'' instance comprehending the input and ouput
         mode settings.
     (2) The position into the SOURCE succeeding the header section and
         any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((header   (make-instance 'Header))
        (position start))
    (declare (type Header header))
    (declare (type fixnum position))
    (setf position (read-header-entry source position header))
    (setf position (read-header-entry source position header))
    (the (values Header fixnum)
      (values header position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of rule parser.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-activating-state (source start rule)
  "Proceeding from the START position into the SOURCE, reads the
   activating state configuration and returns two values:
     (1) A string representation of the activating state.
     (2) The position into the SOURCE succeeding the activating state
         name and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type Rule   rule))
  (multiple-value-bind (state-name position)
      (read-identifier source start)
    (declare (type string state-name))
    (declare (type fixnum position))
    (set-activating-state rule state-name)
    (the fixnum position)))

;;; -------------------------------------------------------

(defun read-activating-symbol (source start rule)
  "Proceeding from the START position into the SOURCE, reads the
   activating state configuration and returns two values:
     (1) A string representation of the activating state.
     (2) The position into the SOURCE succeeding the activating state
         name and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type Rule   rule))
  (multiple-value-bind (activating-symbol position)
      (read-bit source
        (expect-character source start #\~))
    (declare (type bit    activating-symbol))
    (declare (type fixnum position))
    (set-activating-symbol rule activating-symbol)
    (the fixnum
      (skip-whitespaces source position))))

;;; -------------------------------------------------------

(defun read-command (source start)
  "Proceeding from the START position into the SOURCE, reads a Turin
   command identifier and returns two values:
     (1) The ``command'' object corresponding to the consumed Turin
         operation identifier.
     (2) The position into the SOURCE succeeding the consumed command
         token and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values command fixnum)
    (values
      (case (char source start)
        (#\> :move-pointer-right)
        (#\< :move-pointer-left)
        (#\0 :write-0)
        (#\1 :write-1)
        (otherwise
          (error "Unrecognized command token \"~c\" at position ~d."
            (char source start) start)))
      (skip-whitespaces source (1+ start)))))

;;; -------------------------------------------------------

(defun read-commands (source start rule)
  "Proceeding from the START position into the SOURCE, reads a sequence
   of zero or more Turin commands, stores the same in the RULE, and
   returns the position into the SOURCE succeeding the command section
   and any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type Rule   rule))
  (let ((position (expect-character source start #\:)))
    (declare (type fixnum position))
    (symbol-macrolet
        ((command-follows-p
          (the boolean
            (get-boolean-value-of
              (and
                (array-in-bounds-p source position)
                (command-character-p
                  (char source position)))))))
      (declare (type boolean command-follows-p))
      (loop
        while command-follows-p
        collect
          (multiple-value-bind (next-command new-position)
              (read-command source position)
            (declare (type command next-command))
            (declare (type fixnum  new-position))
            (prog1 next-command
              (setf position new-position)))
          into commands
        finally
          (set-activated-commands rule commands)
          (return position)))))

;;; -------------------------------------------------------

(defun read-rule (source start)
  "Proceeding from the START position into the SOURCE, reads a Turin
   rule and returns two values:
     (1) A fresh ``Rule'' instance comprehending the activating state
         and symbol, as well as a sequence of zero or more consequent
         commands.
     (2) The position into the SOURCE succeeding the consumed rule and
         any contingently trailing whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((rule     (make-instance 'Rule))
        (position start))
    (declare (type Rule   rule))
    (declare (type fixnum position))
    (setf position (read-activating-state  source position rule))
    (setf position (read-activating-symbol source position rule))
    (setf position (read-commands          source position rule))
    (the (values Rule fixnum)
      (values rule position))))

;;; -------------------------------------------------------

(defun read-rule-set (source start)
  "Proceeding from the START position into the SOURCE, reads a sequence
   of zero or more rules and returns an ordered list of their
   representative ``Rule'' objects."
  (declare (type string source))
  (declare (type fixnum start))
  (the rule-set
    (loop
      with position of-type fixnum = start
      
      while (< position (length source))
      
      if (identifier-character-p (char source position)) collect
        (multiple-value-bind (next-rule new-position)
            (read-rule source position)
          (declare (type Rule   next-rule))
          (declare (type fixnum new-position))
          (prog1 next-rule
            (setf position new-position)))
      else do
        (error "Unexpected character \"~c\" at position ~d."
          (char source position) position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program parser.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-program (source)
  "Parses the piece of Turin SOURCE code and returns a covenable
  ``Program'' representation of its configurations."
  (declare (type string source))
  (let ((position (skip-whitespaces source 0))
        (header   NIL))
    (declare (type fixnum           position))
    (declare (type (or null Header) header))
    (setf (values header position)
      (read-header source position))
    (the Program
      (make-instance 'Program
        :header (validate-header header)
        :rules  (read-rule-set source position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Turin program.")
    :type          Program
    :documentation "The Turin program to evaluate.")
   (current-state
    :initform      "START"
    :type          (or null string)
    :documentation "The current Turing machine state.")
   (active-rule
    :type          (or null Rule)
    :documentation "The currently active rule.")
   (tape
    :initform      (make-instance 'Tape)
    :type          Tape
    :documentation "The infinite tape of bits."))
  (:documentation
    "The ``Interpreter'' class applies itself to the furnishment of a
     context for a Turin program's execution."))

;;; -------------------------------------------------------

(defun select-next-rule (interpreter)
  "Searches in the INTERPRETER's Turin for the next covenable rule,
   memorizes the same, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'active-rule)
    (when (slot-value interpreter 'current-state)
      (find-rule
        (slot-value interpreter 'program)
        (slot-value interpreter 'current-state)
        (get-current-tape-symbol
          (slot-value interpreter 'tape)))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Selects the initial rule to active for the INTERPRETER's execution
   and returns no value."
  (declare (type Interpreter interpreter))
  (select-next-rule interpreter)
  (values))

;;; -------------------------------------------------------

(defun request-input (interpreter)
  "Queries the standard input for a line, issuing a prompt message in
   compliance with the INTERPRETER program's input mode, and returns a
   string representation of the thus obtained response."
  (declare (type Interpreter interpreter))
  (let ((input-mode
          (get-input-mode
            (get-program-header
              (slot-value interpreter 'program)))))
    (declare (type io-mode input-mode))
    (case input-mode
      (:ascii
        (format T "~&Please input an ASCII string: "))
      (:binary
        (format T "~&Please input a binary string: "))
      (:hexadecimal
        (format T "~&Please input a hexadecimal string: "))
      (otherwise
        (error "Invalid input mode: ~s." input-mode)))
    (finish-output)
    (the string
      (prog1
        (read-line)
        (clear-input)))))

;;; -------------------------------------------------------

(defun initialize-program-tape (interpreter)
  "Initializes the INTERPRETER's internally managed tape by quering the
   standard input for content conformant with the INTERPRETER program's
   input mode, which is subsequently transferred unto the storage
   medium, and returns no value."
  (declare (type Interpreter interpreter))
  (input-into-tape
    (slot-value interpreter 'tape)
    0
    (request-input interpreter)
    (get-input-mode
      (get-program-header
        (slot-value interpreter 'program))))
  (values))

;;; -------------------------------------------------------

(defun print-program-tape (interpreter)
  "Prints the INTERPRETER tape's content to the standard output in
   compliance with the Turin program's output configuration and returns
   no value."
  (declare (type Interpreter interpreter))
  (print-formatted-tape
    (slot-value interpreter 'tape)
    (get-output-mode
      (get-program-header
        (slot-value interpreter 'program))))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the INTERPRETER's Turin program is completed,
   which constitutes a veridical state if no eligible rule can be
   detected for a subsequent execution, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (null
        (slot-value interpreter 'active-rule)))))

;;; -------------------------------------------------------

(defun execute-active-rule (interpreter)
  "Executes the INTERPRETER's active rule, if such exists, and returns
   no value."
  (declare (type Interpreter interpreter))
  (with-slots (active-rule tape) interpreter
    (declare (type (or null Rule) active-rule))
    (declare (type Tape           tape))
    (when active-rule
      (loop
        for command
          of-type command
          in      (get-activated-commands active-rule)
        do
          (case command
            (:move-pointer-right
              (move-tape-pointer-right tape))
            (:move-pointer-left
              (move-tape-pointer-left tape))
            (:write-0
              (set-current-tape-symbol tape 0))
            (:write-1
              (set-current-tape-symbol tape 1))
            (otherwise
              (error "Invalid command: ~s." command))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Turin program maintained by the INTERPRETER, prints
   its tape immediately prior to its conclusion, and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (execute-active-rule interpreter)
    (setf (slot-value interpreter 'active-rule) NIL))
  (print-program-tape interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpret-Turin (code &key (initialize-tape-p NIL))
  "Interprets the piece of Turin source CODE, contingently initializing
   the program tape via user input if INITIALIZE-TAPE-P is positive,
   prints its tape immediately prior to its conclusion, and returns no
   value."
  (declare (type string  code))
  (declare (type boolean initialize-tape-p))
  (let ((interpreter
          (make-instance 'Interpreter :program
            (parse-program code))))
    (declare (type Interpreter interpreter))
    (when initialize-tape-p
      (initialize-program-tape interpreter))
    (interpret-program interpreter))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello,world!".
(interpret-Turin
  "OUT ASCII
   IN ASCII
    START~0:0>1>0>0>1>0>0>0>0>1>1>0>0>1>0>1>0>1>1>0>1>1>0>0>0>1>1>0>1>1>0>0>0>1>1>0>1>1>1>1>0>0>1>0>1>1>0>0>0>1>1>1>0>1>1>1>0>1>1>0>1>1>1>1>0>1>1>1>0>0>1>0>0>1>1>0>1>1>0>0>0>1>1>0>0>1>0>0>0>0>1>0>0>0>0>1")

;;; -------------------------------------------------------

;; One-time line-based cat program.
(interpret-Turin
  "OUT ASCII
   IN  ASCII
   START~0:"
  :initialize-tape-p T)

;;; -------------------------------------------------------

;; Replicate the text "FACE" as a sequence of hexadecimal digits.
;; 
;; The underlying tape adheres to the following principles:
;; 
;;   -------------------------------
;;   Hexadecimal | Decimal | Binary
;;   ------------+---------+--------
;;   F           | 15      | 1111
;;   A           | 10      | 1010
;;   C           | 12      | 1100
;;   E           | 14      | 1110
;;   -------------------------------
(interpret-Turin
  "OUT HEX
   IN  ASCII
   START~0:1>1>1>1>
           1>0>1>0>
           1>1>0>0>
           1>1>1>0")
