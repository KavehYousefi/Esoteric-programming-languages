;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "CDILOI", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 13th, 2023, the haecceity apportioned to whom
;; capacitates the manipulation of strings in an exclusive manner,
;; amplified in its competences by a label-based control flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The CDILOI programming language's concept is based upon the
;; perquisition and manipulation of strings, both in a literal form and
;; augmented by variable constituents, and aided in this enterprise by
;; a label-based goto facility.
;; 
;; == [C]ONCATENATE, [D]ELETE, [I]F, [L]ABEL, [O]UTPUT, [I]NPUT ==
;; The CDILOI programming language's agnomation, in its dispanded form,
;; bewrays its capacities' circumference, relating of its services to
;; "[C]oncatenate, [D]elete, [I]f, [L]abel, [O]utput, [I]nput".
;; 
;; == STRINGS: THE LANGUAGE CYNOSURE ==
;; The entirety of CDILOI's diorism centers around strings as the
;; subject of its cynosure, eschewing numeric objects, as well as any
;; other species of types.
;; 
;; == CDILOI IS CASE-INSENSITIVE ==
;; A kenspeckle proprium incorporated in this language constitutes its
;; case-insensitive nature, that is, majuscles and minuscles may be
;; delivered to one's conspection in a perfectly paregal mode.
;; 
;; == VARIABLES STORE ALL DATA AS STRINGS ==
;; The entire program data's castaldy is actuated in the form of
;; variables, each amenable to a unique identifier, and entalented with
;; the capacity for exactly one datum's harborage.
;; 
;; If a variable yet absent is perquired for its content, a new entry
;; for such, associated with an initially empty string, is automatically
;; accommodated, the inchoate production concomitantly incarnating its
;; response.
;; 
;; == OPERANDS: EITHER LITERALS OR INDIRECTIONS ==
;; Two tiers of operands rede their distinguishment in a program:
;; 
;;   (1) DIRECT SPECIFICATION:
;;       Its subsumption of both string literals and variable
;;       identifiers in the agency of a sink for data reception is
;;       realized in terms of unquoted character sequences.
;;       The demarcation symbols' abstinence installs a parvipotent
;;       species in the circumference of admissible content; in concrete
;;       diction, the whitespace characters' discriminating purpose
;;       excludes their contingency for a participation in such strings.
;;   
;;   (2) INDIRECTION:
;;       A variable value may be requested by the identifier's prefixion
;;       with the at sign ("@").
;; 
;; == CONTROL FLOW: LABELS AND CONDITIONAL JUMPING ==
;; The aefauld control flow mechanism in acquainted to CDILOI is
;; exhausted by a label-based goto facility.
;; 
;; Labels may be defined at any instant in the program and affiliate
;; with an arbitrary identifier by naiting the "LBL" instruction,
;; ensuing therefrom, the interpreting entity maintains an mapping
;; atwixt the label name and location in the code.
;; 
;; Upon an "IF" operation antecedent's satisfaction, the respective
;; label, identified by its agnomination, is requested, and the
;; program's instruction pointer (IP) redirected to the answering
;; position.
;; 
;; 
;; Instructions
;; ============
;; CDILOI's instruction set tallies a sextuple membership, the foundry
;; of which, and preponderance, contributes to the string manipulation
;; operations and their indagative as well as manipulative relationship
;; with the variables; a remnant entertains itself with a conditional
;; control flow mechanism, its instrument accounting for the labels.
;; 
;; == OVERVIEW ==
;; The operational sextuple of CDILOI's capacities shall be a cursory
;; species of communication's cynosure.
;; 
;; Please heed that placeholder segments are underlined by a series of
;; asterisks ("*") and intended to be substituted by actual CDILOI code
;; in the program.
;; 
;;   ------------------------------------------------------------------
;;   Command              | Effect
;;   ---------------------+--------------------------------------------
;;   CONC left right dest | Concatenates the {left} and {right} strings
;;        **** ***** **** | and stores the result in the variable
;;                        | {dest}.
;;   ..................................................................
;;   DEL source head tail | Splits the {source} string into two parts,
;;       ****** **** **** | one being the first character, which is
;;                        | stored in the variable {head}, and a,
;;                        | contingently empty, rest, intended for the
;;                        | transfer into the {tail} variable.
;;   ..................................................................
;;   IF left right dest   | If the {left} and {right} strings are
;;      **** ***** ****   | equal, the instruction pointer (IP) is
;;                        | relocated to the position {dest}. Otherwise
;;                        | proceeds as usual.
;;                        |-------------------------------------------
;;                        | Labels are defined by adminiculum of the
;;                        | "LBL" command, which please see below.
;;   ..................................................................
;;   INP destination      | Queries the standard input for a line of
;;       ***********      | input and stores the thus produced string
;;                        | in the {destination} variable.
;;   ..................................................................
;;   LBL name             | Defines a label in the code which
;;       ****             | associates the current instruction pointer
;;                        | (IP) location in the program with the
;;                        | label {name}.
;;                        |--------------------------------------------
;;                        | Such labels are intended for targeting by
;;                        | the "IF" command, which please see above.
;;   ..................................................................
;;   OUT argument         | Prints the {argument} string to the
;;       ********         | standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been actuated in the programming language
;; Common Lisp, its production accomplished by intrining the stages of
;; lexical analyzation, parsing, and actual interpretation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-23
;; 
;; Sources:
;;   [esolang2023CDILOI]
;;   The Esolang contributors, "CDILOI", May 25th, 2023
;;   URL: "https://esolangs.org/wiki/CDILOI"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest parameters)
     &body body)
  "Defines a derived type, proceeding from the ``deftype''
   infrastructure in conjunction with the ``satisfies'' predicate, in a
   more convenient fashion, by naiting the TYPE-NAME for the
   agnomination and the PARAMETERS to specify the lambda list, while the
   implementation may access the probed OBJECT under the
   CANDIDATE-VARIABLE's name, evaluating the BODY, and returning the
   desinent BODY form's results, the derived type's predicate being
   satisfies for a returned primary value of non-``NIL'', otherwise its
   eligibility is denoted as an inadequacy.
   ---
   In the case of the BODY form's first member being a string, the same
   is construed as the type definition's documentation string, and is
   subsequently removed for this purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@parameters)
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Defines the derived type ``~a''." type-name))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   '*)
                                                 (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic ``*'' sentinel."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of CDILOI language
   keywords as identifier strings to representative tokens, manifesting
   in a hash table whose string keys exhibit an amenability to ``Token''
   values."
  '(hash-table-of string Token))

;;; -------------------------------------------------------

(deftype string-mapping ()
  "The ``string-mapping'' type defines an association of variable names
   to string values, its reification that of a hash table whose string
   keys assume the placeholders and affiliate with the string content in
   their values."
  '(hash-table-of string string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class serves in the representation of a significant
   CDILOI object extracted during the lexical analyzation stage."
  (type  (error "Missing type.")  :type keyword)
  (value (error "Missing value.") :type T))

;;; -------------------------------------------------------

(defun token-of-type-p (candidate expected-type)
  "Determines whether the CANDIDATE token conforms to the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token   candidate))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type candidate) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equalp)
  "Affiliates the recognized CDILOI language identifier strings with
   representative tokens.")

;;; -------------------------------------------------------

(flet ((register-identifier (name token)
        "Affiliates the identifier NAME with a representative TOKEN in
         the +IDENTIFIERS+ table and returns no value.
         ---
         Any entry already extant and answering to the NAME in the
         +IDENTIFIERS+ table will be superseded by the new twain."
        (declare (type string name))
        (declare (type Token  token))
        (setf (gethash name +IDENTIFIERS+) token)
        (values)))
  (register-identifier "CONC" (make-token :CONC "CONC"))
  (register-identifier "DEL"  (make-token :DEL  "DEL"))
  (register-identifier "IF"   (make-token :IF   "IF"))
  (register-identifier "INP"  (make-token :INP  "INP"))
  (register-identifier "LBL"  (make-token :LBL  "LBL"))
  (register-identifier "OUT"  (make-token :OUT  "OUT"))
  (values))

;;; -------------------------------------------------------

(defun get-identifier (name)
  "Returns the token associated with the NAME, either such corresponding
   to a CDILOI language keyword, or generating a fresh ``:identifier''
   token for the NAME."
  (declare (type string name))
  (multiple-value-bind (token contains-name-p)
      (gethash name +IDENTIFIERS+)
    (declare (type (or null Token) token))
    (declare (type T               contains-name-p))
    (the Token
      (if contains-name-p
        token
        (make-token :identifier name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          string
    :documentation "The piece of CDILOI source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The currently processed location into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE.
                    ---
                    The ``NIL'' value designates an exhausted SOURCE,
                    tantamount to an end-of-file (EOF) marker."))
  (:documentation
    "The ``Lexer'' class' wike amplects the lexical analyzation of a
     piece of CDILOI source code into its tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Initializes the LEXER's character to the first position in its
   source and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' whose onus involves the
   analyzation of the CDILOI SOURCE code."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defmacro with-lexer (lexer (&optional
                              (character-variable '$character)
                              (position-variable  '$position)
                              (source-variable    '$source))
                      &body body)
  "Defines a commodity for handling the LEXER instance with respect to
   its state by binding with local symbol macros its ``character'' slot
   to the CHARACTER-VARIABLE, defaulting to ``$position'', the
   ``position'' to the POSITION-VARIABLE, ``$position'' upon its
   omission, and the ``source'' slot to the SOURCE-VARIABLE, the same
   assumes a default of ``$source'', executing the BODY forms with adit
   to these bindings, and returning the desinent form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (with-slots ((,character-variable character)
                    (,position-variable  position)
                    (,source-variable    source))
           ,evaluated-lexer
         (declare (type (or null character) ,character-variable))
         (declare (ignorable                ,character-variable))
         (declare (type fixnum              ,position-variable))
         (declare (ignorable                ,position-variable))
         (declare (type string              ,source-variable))
         (declare (ignorable                ,source-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun advance-lexer (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value.
   ---
   The LEXER's internally managed character may, as a consequence of
   this operation, be rendered the ``NIL'' value, in which circumstance
   the end of the source has been passed."
  (declare (type Lexer lexer))
  (with-lexer lexer ()
    (setf $character
      (when (array-in-bounds-p $source (1+ $position))
        (char $source
          (incf $position)))))
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (lexer)
  "Proceeding from the current position into the LEXER's source, skips a
   sequence of zero or more accolent whitespaces, and returns no value."
  (declare (type Lexer lexer))
  (with-lexer lexer ()
    (loop while (and $character (whitespace-character-p $character)) do
      (advance-lexer lexer)))
  (values))

;;; -------------------------------------------------------

(defun read-word (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   single word, demarcated either by the next occurrence of a whitespace
   or the end of the source, and returns a string representation of the
   thus consumed content in a verbatim form."
  (declare (type Lexer lexer))
  (the string
    (with-output-to-string (word)
      (declare (type string-stream word))
      (with-lexer lexer ($character)
        (loop
          until (or (null $character)
                    (whitespace-character-p $character))
          do
            (write-char $character word)
            (advance-lexer lexer))))))

;;; -------------------------------------------------------

(defun read-identifier (lexer)
  "Proceeding from the current position into the LEXER's source, reads
   an identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier
      (read-word lexer))))

;;; -------------------------------------------------------

(defun read-indirection (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   variable value accessor, introduced by the at sign \"@\", and returns
   a ``:variable'' token representation thereof."
  (declare (type Lexer lexer))
  ;; Skip the introducing at sign, "@".
  (advance-lexer lexer)
  (let ((word (read-word lexer)))
    (declare (type string word))
    (the Token
      (if (plusp (length word))
        (make-token :indirection word)
        (make-token :identifier "@")))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer lexer ($character)
      (cond
        ((null $character)
          (make-token :EOF NIL))
        
        ((whitespace-character-p $character)
          (skip-whitespaces lexer)
          (get-next-token   lexer))
        
        ((char= $character #\@)
          (read-indirection lexer))
        
        (T
          (read-identifier lexer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operands.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface establishes a foundry for all concrete
   classes pursing the representation of an instruction operand.")

;;; -------------------------------------------------------

(defstruct (Direct-Operand
  (:include Operand))
  "The ``Direct-Operand'' class defines an instruction operand actuating
   in its verbatim form, that is, either as a literal object or a
   variable name, but abstinent from an indirection context."
  (value (error "Missing value.") :type string))

;;; -------------------------------------------------------

(defstruct (Indirection-Operand
  (:include Operand))
  "The ``Indirection-Operand'' class defines an instruction operand
   intended to communicate the request for a variable value by its name,
   in the CDILOI programming language introduced via an at sign, \"@\"."
  (name (error "Missing value.") :type string))

;;; -------------------------------------------------------

(defgeneric get-operand-string (operand)
  (:documentation
    "Returns the OPERAND's verbatim form.")
  
  (:method ((operand Direct-Operand))
    (declare (type Direct-Operand operand))
    (the string
      (direct-operand-value operand)))
  
  (:method ((operand Indirection-Operand))
    (declare (type Indirection-Operand operand))
    (the string
      (format NIL "@~a"
        (indirection-operand-name operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface establishes a foundry for all concrete
   classes pursuing the representation of a CDILOI operation.")

;;; -------------------------------------------------------

(defstruct (CONC-Instruction
  (:include Instruction))
  "The ``CONC-Instruction'' class embodies the CDILOI instruction
   \"CONC\", intended for the concatenation of two strings and the
   result's transmission into a variable."
  (first-source  (error "Missing first source.")  :type Operand)
  (second-source (error "Missing second source.") :type Operand)
  (destination   (error "Missing destination.")   :type Operand))

;;; -------------------------------------------------------

(defstruct (DEL-Instruction
  (:include Instruction))
  "The ``DEL-Instruction'' class embodies the CDILOI instruction
   \"DEL\", intended for the separation of a string into a
   single-character head and the remaining tail, and their transmission
   into a twissel of responsible variables."
  (source           (error "Missing source.")           :type Operand)
  (head-destination (error "Missing head destination.") :type Operand)
  (tail-destination (error "Missing tail destination.") :type Operand))

;;; -------------------------------------------------------

(defstruct (IF-Instruction
  (:include Instruction))
  "The ``IF-Instruction'' class embodies the CDILOI instruction \"IF\",
   intended for the conditional goto redirection based upon its two
   operands' equality."
  (left-operand  (error "Missing left operand.")  :type Operand)
  (right-operand (error "Missing right operand.") :type Operand)
  (destination   (error "Missing destination.")   :type Operand))

;;; -------------------------------------------------------

(defstruct (INP-Instruction
  (:include Instruction))
  "The ``INP-Instruction'' class embodies the CDILOI instruction
   \"INP\", its furnishment the reception of user input for the
   transmission into a variable."
  (destination (error "Missing destination.") :type Operand))

;;; -------------------------------------------------------

(defstruct (LBL-Instruction
  (:include Instruction))
  "The ``LBL-Instruction'' class embodies the CDILOI instruction
   \"LBL\", its contribution that of a label declaration facility for
   contingent later references by the \"IF\" command's goto
   redirections."
  (label (error "Missing label.") :type Operand))

;;; -------------------------------------------------------

(defstruct (OUT-Instruction
  (:include Instruction))
  "The ``OUT-Instruction'' class embodies the CDILOI instruction
   \"OUT\", justified by its dedication to the transmission of its
   argument to the standard output."
  (argument (error "Missing argument.") :type Operand))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of CDILOI program.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program
  (:constructor make-program (instructions)))
  "The ``Program'' class encapsulates the concept of an executable
   CDILOI program as a vector of zero or more instructions."
  (instructions (error "Missing instructions.")
                :type (vector Instruction *)))

;;; -------------------------------------------------------

(defun valid-program-position-p (program position)
  "Determines whether the POSITION specifies a valid index into the
   PROGRAM, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  position))
  (the boolean
    (not (null
      (array-in-bounds-p (program-instructions program) position)))))

;;; -------------------------------------------------------

(defun get-instruction-at (program position)
  "Returns the instruction located at the zero-based POSITION into the
   PROGRAM's instruction sequence, or signals an error of an unspecified
   type upon its violation of the established bournes."
  (the Instruction
    (if (valid-program-position-p program position)
      (aref (program-instructions program) position)
      (error "Invalid position for the program: ~d." position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :type          Lexer
    :documentation "The lexer whose responsibility accounts for the
                    provision of tokens to assemble into an executable
                    program.")
   (current-token
    :initform      (make-token :EOF NIL)
    :type          Token
    :documentation "The most recently obtained token from the LEXER."))
  (:documentation
    "The ``Parser'' class is endowed with the onus of assembling an
     executable CDILOI program as a sequence of instructions from a
     token stream."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Obtains from the PARSER's internally managed lexer the first token
   and returns no value."
  (declare (type Parser parser))
  (setf (slot-value parser 'current-token)
    (get-next-token
      (slot-value parser 'lexer)))
  (values))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'', the requisite tokens of which
   are obtained by the LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defmacro with-parser (parser
                       (&optional (token-variable '$current-token)
                                  (lexer-variable '$lexer))
                       &body body)
  "Accommodates a commodity for the eath handling of a PARSER in regard
   to its slots by first evaluating the PARSER, ere binding, via local
   symbol macros, its ``current-token'' slot to the TOKEN-VARIABLE, its
   ``lexer'' to the LEXER-VARIABLE, subsequently evaluating the BODY
   forms with access to the bindings, and returning the desinent form's
   results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (declare (ignorable   ,evaluated-parser))
       (with-slots ((,token-variable current-token)
                    (,lexer-variable lexer))
           ,evaluated-parser
         (declare (type Token ,token-variable))
         (declare (ignorable  ,token-variable))
         (declare (type Lexer ,lexer-variable))
         (declare (ignorable  ,lexer-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun eat-current-token (parser)
  "Returns the PARSER's current token, while concomitantly requesting
   the subsequent one from the internally managed lexer, and superseding
   the token information by the same."
  (declare (type Parser parser))
  (the Token
    (with-parser parser ($current-token $lexer)
      (prog1 $current-token
        (setf $current-token
          (get-next-token $lexer))))))

;;; -------------------------------------------------------

(defun parse-operand (parser)
  "Parses a CDILOI instruction operand while employing the PARSER's
   services and returns a suitable ``Operand'' representation thereof.
   ---
   An error of an unspecified type is signaled in the case of no
   discernable eligibility among the recognized operands."
  (declare (type Parser parser))
  (the Operand
    (with-parser parser ($current-token)
      (case (token-type $current-token)
        (:eof
          (error "Expected an operand token, but encountered ~
                  end of file (EOF)."))
        
        (:identifier
          (make-direct-operand :value
            (token-value
              (eat-current-token parser))))
        
        (:indirection
          (make-indirection-operand :name
            (token-value
              (eat-current-token parser))))
        
        (otherwise
          (make-direct-operand :value
            (token-value
              (eat-current-token parser))))))))

;;; -------------------------------------------------------

(defun parse-instruction (parser)
  "Parses an aefauld CDILOI instruction while employing the PARSER's
   services and returns a suitable ``Instruction'' representation
   thereof.
   ---
   An error of an unspecified type is signaled if no instruction could
   be selected."
  (declare (type Parser parser))
  (the Instruction
    (with-parser parser ($current-token)
      (case (token-type $current-token)
        (:CONC
          (eat-current-token parser)
          (make-conc-instruction
            :first-source  (parse-operand parser)
            :second-source (parse-operand parser)
            :destination   (parse-operand parser)))
        
        (:DEL
          (eat-current-token parser)
          (make-del-instruction
            :source           (parse-operand parser)
            :head-destination (parse-operand parser)
            :tail-destination (parse-operand parser)))
        
        (:IF
          (eat-current-token parser)
          (make-if-instruction
            :left-operand  (parse-operand parser)
            :right-operand (parse-operand parser)
            :destination   (parse-operand parser)))
        
        (:INP
          (eat-current-token parser)
          (make-inp-instruction
            :destination (parse-operand parser)))
        
        (:LBL
          (eat-current-token parser)
          (make-lbl-instruction
            :label (parse-operand parser)))
        
        (:OUT
          (eat-current-token parser)
          (make-out-instruction
            :argument (parse-operand parser)))
        
        (otherwise
          (error "No instruction token: ~s." $current-token))))))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Parses a CDILOI program by adminiculum of the PARSER's services and
   returns a ``Program'' representation of its consumed instructions."
  (declare (type Parser parser))
  (the Program
    (with-parser parser ($current-token)
      (make-program
        (coerce
          (loop
            until   (token-of-type-p $current-token :eof)
            collect (parse-instruction parser))
          '(simple-array Instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-strings (first-string second-string)
  "Concatenates the FIRST-STRING and the SECOND-STRING in this exact
   order, destitute of any intervening sepiment, and returns a fresh
   string comprehending the catenation result."
  (declare (type string first-string))
  (declare (type string second-string))
  (the string
    (format NIL "~a~a" first-string second-string)))

;;; -------------------------------------------------------

(defun cleave-string (string)
  "Rends the STRING atwain into its head, comprehending at most one
   character, and a tail compact of the zero or more orra entities, and
   returns the result in two values:
     (1) A fresh string containing first character of the STRING, or,
         upon the STRING's vacancy, an empty string.
     (2) A fresh string containing all but the first character of the
         STRING, or, upon either the STRING's vacancy or its
         insufficient extent, an empty string."
  (declare (type string string))
  (the (values string string)
    (if (plusp (length string))
      (values
        (subseq string 0 1)
        (subseq string 1))
      (values "" ""))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Label-Table ()
  ((labels
    :initform     (make-hash-table :test #'equalp)
    :type         (hash-table-of string fixnum)
    :documentation "Maps the label names to their locations in the
                    executed CDILOI program."))
  (:documentation
    "The ``Label-Table'' class applies itself to the wike of the label
     definitions' castaldy, affiliating with each label name its
     location in the CDILOI program's command sequence."))

;;; -------------------------------------------------------

(defun make-label-table ()
  "Creates and returns a new empty ``Label-Table''."
  (the Label-Table
    (make-instance 'Label-Table)))

;;; -------------------------------------------------------

(defun register-label (labels label-name position)
  "Associates the LABEL-NAME with the POSITION in the executed program
   in the LABELS table and returns no value."
  (declare (type Label-Table labels))
  (declare (type string      label-name))
  (declare (type fixnum      position))
  (setf (gethash label-name (slot-value labels 'labels))
        position)
  (values))

;;; -------------------------------------------------------

(defun contains-label-p (labels label-name)
  "Determines whether the LABELS table contains an entry for the
   LABEL-NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Label-Table labels))
  (declare (type string      label-name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash label-name
          (slot-value labels 'labels)))))))

;;; -------------------------------------------------------

(defun get-label-destination (labels label-name)
  "Returns the position in the CDILOI instruction sequence responding to
   the LABEL-NAME's definition in the LABELS table, or signals an error
   of an unspecified type upon its disrespondency."
  (declare (type Label-Table labels))
  (declare (type string      label-name))
  (multiple-value-bind (destination contains-name-p)
      (gethash label-name
        (slot-value labels 'labels))
    (declare (type (or null fixnum) destination))
    (declare (type T                contains-name-p))
    (the fixnum
      (if contains-name-p
        destination
        (error "The label table does not contain entry for the name ~s."
          label-name)))))

;;; -------------------------------------------------------

(defun build-labels (program)
  "Calculates and returns for the CDILOI PROGRAM the label table."
  (declare (type Program program))
  (let ((labels (make-label-table)))
    (declare (type Label-Table labels))
    (loop
      for instruction
        of-type Instruction
        across  (program-instructions program)
      and position
        of-type fixnum
        from    0
        by      1
      when (lbl-instruction-p instruction) do
        (register-label labels
          (get-operand-string
            (lbl-instruction-label instruction))
          position))
    (the Label-Table labels)))

;;; -------------------------------------------------------

(defmethod print-object ((labels Label-Table) stream)
  (declare (type Label-Table labels))
  (declare (type destination stream))
  (format stream "(Label-Table")
  (loop
    for label-name
      of-type string
      being the hash-keys in (slot-value labels 'labels)
    using
      (hash-value label-position)
    do
      (format stream "~&~2t~s => ~d" label-name label-position))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Table ()
  ((variables
    :initform      (make-hash-table :test #'equalp)
    :type          string-mapping
    :documentation "Maps variable names to the stored strings."))
  (:documentation
    "The ``Variable-Table'' class' wike encompasses the variables'
     castaldy, realizing this duty by a hash table's adminiculum, whose
     keys accommodate the variable names, and whose values contribute
     the affiliated string contents."))

;;; -------------------------------------------------------

(defun make-variable-table ()
  "Creates and returns a new empty ``Variable-Table''."
  (the Variable-Table
    (make-instance 'Variable-Table)))

;;; -------------------------------------------------------

(defun contains-variable-p (variables variable-name)
  "Determines whether the VARIABLES table contains an entry for the
   VARIABLE-NAME, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Variable-Table variables))
  (declare (type string         variable-name))
  (the boolean
    (not (null
      (nth-value 1
        (gethash variable-name
          (slot-value variables 'variables)))))))

;;; -------------------------------------------------------

(defun ensure-variable (variables variable-name)
  "Ensures that the VARIABLES table contains an entry for the
   VARIABLE-NAME by accommodating such, associated with an empty string,
   upon its absence, otherwise abstaining for a furhter causatum's
   actuation, and in any returns no value."
  (declare (type Variable-Table variables))
  (declare (type string         variable-name))
  (unless (contains-variable-p variables variable-name)
    (setf (gethash variable-name
            (slot-value variables 'variables))
          ""))
  (values))

;;; -------------------------------------------------------

(defun get-variable-value (variables variable-name)
  "Returns the value affiliated with the VARIABLE-NAME in the VARIABLES
   table, upon necessity accommodating a new entry with an empty string
   for an absent VARIABLE-NAME."
  (declare (type Variable-Table variables))
  (declare (type string         variable-name))
  (ensure-variable variables variable-name)
  (the string
    (gethash variable-name
      (slot-value variables 'variables))))

;;; -------------------------------------------------------

(defun set-variable (variables variable-name new-value)
  "Associates the NEW-VALUE with the VARIABLE-NAME in the VARIABLES
   table, contingently superseding any extant entry with the name as the
   key, and returns no value."
  (declare (type Variable-Table variables))
  (declare (type string         variable-name))
  (declare (type string         new-value))
  (setf (gethash variable-name
          (slot-value variables 'variables))
        new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          Program
    :documentation "The CDILOI program to execute.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer (IP) position of the
                    currently executed instruction among the PROGRAM's
                    sequence.")
   (labels
    :initform      (make-label-table)
    :type          Label-Table
    :documentation "Maintains the labels and their positions in the
                    CDILOI PROGRAM.")
   (variables
    :initform      (make-variable-table)
    :type          Variable-Table
    :documentation "Maintains a mapping of variable names to their
                    string values."))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluation of an
     executable CDILOI program by accompassing actual effect to its
     instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Builds the label table for the INTERPRETER's CDILOI program and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'labels)
    (build-labels
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'', dedicated to the
   evaluation of the CDILOI PROGRAM."
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defmacro with-interpreter (interpreter
                            (&optional
                              (program-variable   '$program)
                              (ip-variable        '$ip)
                              (labels-variable    '$labels)
                              (variables-variable '$variables))
                            &body body)
  "Defines a convenience commodity for the access of an
   ``Interpreter'''s slots by evaluating the INTERPRETER and binding its
   slot ``program'' to the PROGRAM-VARIABLE, defaulting to ``$program'',
   its ``ip'' to the IP-VARIABLE, defaulting to ``$ip'', the ``labels''
   to the LABELS-VARIABLE, with a default of ``$labels'', and the
   ``variables'' to the VARIABLES-VARIABLE, assuming ``$variables'' upon
   its omission, executing the BODY forms, and returning the desinent
   form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (with-slots ((,program-variable   program)
                    (,ip-variable        ip)
                    (,labels-variable    labels)
                    (,variables-variable variables))
           ,evaluated-interpreter
         (declare (type Program        ,program-variable))
         (declare (ignorable           ,program-variable))
         (declare (type fixnum         ,ip-variable))
         (declare (ignorable           ,ip-variable))
         (declare (type Label-Table    ,labels-variable))
         (declare (ignorable           ,labels-variable))
         (declare (type Variable-Table ,variables-variable))
         (declare (ignorable           ,variables-variable))
         ,@body))))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter interpreter ()
    (when (valid-program-position-p $program $ip)
      (incf $ip)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-label (interpreter label-name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the position
   associated with the LABEL-NAME and returns no value.
   ---
   An error of an unspecified type is signaled if the LABEL-NAME does
   not correspond to any position."
  (declare (type Interpreter interpreter))
  (declare (type string      label-name))
  (with-interpreter interpreter ()
    (setf $ip
      (get-label-destination $labels label-name)))
  (values))

;;; -------------------------------------------------------

(defun execution-finished-p (interpreter)
  "Determines whether the INTERPRETER's execution has halted as a
   consequence of its instruction pointer's (IP) advancement beyond the
   desinent program instruction, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (the boolean
    (with-interpreter interpreter ()
      (not (valid-program-position-p $program $ip)))))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the instruction located at the INTERPRETER's instruction
   pointer (IP) position, or signals an error of an unspecified type
   upon its program bournes' transgression."
  (the Instruction
    (with-interpreter interpreter ()
      (get-instruction-at $program $ip))))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Returns the instruction OPERAND's value in the INTERPRETER's
     context.")
  
  (:method ((interpreter Interpreter)
            (operand     Direct-Operand))
    "Ignores the INTERPRETER and returns the OPERAND's value."
    (declare (type Interpreter    interpreter))
    (declare (ignore              interpreter))
    (declare (type Direct-Operand operand))
    (the string
      (direct-operand-value operand)))
  
  (:method ((interpreter Interpreter)
            (operand     Indirection-Operand))
    "Returns the value of the variable referenced by the OPERAND's
     destination name as registered at the INTERPRETER's variable
     table."
    (declare (type Interpreter         interpreter))
    (declare (type Indirection-Operand operand))
    (the string
      (get-variable-value
        (slot-value interpreter 'variables)
        (indirection-operand-name operand)))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor
    (instruction-class
     (&optional (interpreter-variable '$interpreter)
                (instruction-variable '$instruction))
     &body body)
  "Furnishes a convenient commodity for the definition of a
   ``process-instruction'' generic function implementation by utilizing
   for the ``defmethod'' establishment as the first parameter the
   INTERPRETER-VARIABLE, the same defaults to the symbol
   ``$interpreter'', for the second the INSTRUCTION-VARIABLE, resorting
   upon its omission to ``$instruction'', and whose dispatching type is
   desumed from the INSTRUCTION-CLASS, and inserting the BODY forms into
   the method body, returning the desinent form's results."
  `(defmethod process-instruction
       ((,interpreter-variable Interpreter)
        (,instruction-variable ,instruction-class))
     (declare (type Interpreter        ,interpreter-variable))
     (declare (ignorable               ,interpreter-variable))
     (declare (type ,instruction-class ,instruction-variable))
     (declare (ignorable               ,instruction-variable))
     ,@body))

;;; -------------------------------------------------------

(define-instruction-processor CONC-Instruction ()
  (with-interpreter $interpreter ()
    (set-variable $variables
      (resolve-operand $interpreter
        (conc-instruction-destination $instruction))
      (concatenate-strings
        (resolve-operand $interpreter
          (conc-instruction-first-source $instruction))
        (resolve-operand $interpreter
          (conc-instruction-second-source $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor DEL-Instruction ()
  (multiple-value-bind (head tail)
      (cleave-string
        (resolve-operand $interpreter
          (del-instruction-source $instruction)))
    (declare (type string head))
    (declare (type string tail))
    (with-interpreter $interpreter ()
      (set-variable $variables
        (resolve-operand $interpreter
          (del-instruction-head-destination $instruction))
        head)
      (set-variable $variables
        (resolve-operand $interpreter
          (del-instruction-tail-destination $instruction))
        tail))))

;;; -------------------------------------------------------

(define-instruction-processor IF-Instruction ()
  (let ((left-value
          (resolve-operand $interpreter
            (if-instruction-left-operand $instruction)))
        (right-value
          (resolve-operand $interpreter
            (if-instruction-right-operand $instruction))))
    (declare (type string left-value))
    (declare (type string right-value))
    (when (string-equal left-value right-value)
      (jump-to-label $interpreter
        (resolve-operand $interpreter
          (if-instruction-destination $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor INP-Instruction ()
  (with-interpreter $interpreter ()
    (set-variable $variables
      (resolve-operand $interpreter
        (inp-instruction-destination $instruction))
      (read-line)))
  (clear-input))

;;; -------------------------------------------------------

(define-instruction-processor LBL-Instruction ())

;;; -------------------------------------------------------

(define-instruction-processor OUT-Instruction ()
  (format T "~&~a~%"
    (resolve-operand $interpreter
      (out-instruction-argument $instruction)))
  (finish-output))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Evaluates the CDILOI program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (execution-finished-p interpreter) do
    (process-instruction interpreter
      (get-current-instruction interpreter))
    (advance-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-CDILOI (code)
  "Interprets the piece of CDILOI source CODE and returns no value."
  (interpret-program
    (make-interpreter
      (parse-program
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello,World!".
(interpret-CDILOI "OUT Hello,World!")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-CDILOI
  "INP x
   OUT @x")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-CDILOI
  "
  LBL repeat
  INP input
  OUT @input
  IF  shallRepeat shallRepeat repeat
  ")

;;; -------------------------------------------------------

;; Reverse cat.
;; 
;; This program queries the user for a line of input, reverses the thus
;; committed string, and outputs the result.
(interpret-CDILOI
  "INP x
   LBL notempty
   DEL @x char x
   CONC @char @out out
   IF @y @x end
   IF a a notempty
   LBL end
   OUT @out")

;;; -------------------------------------------------------

;; Unary counter.
;; 
;; Infinite counter which, during each cycle, prints a tally of
;; asterisks ("*") tantamount to the current cycle number, starting with
;; one (1), that is:
;;   ------------------
;;   Cycle no. | Output
;;   ----------+-------
;;   1         | *
;;   ..................
;;   2         | **
;;   ..................
;;   3         | ***
;;   ..................
;;   4         | ****
;;   ..................
;;   [...]     | [...]
;;   ------------------
(interpret-CDILOI
  "LBL count
   CONC @x * x
   OUT @x
   IF a a count")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-CDILOI
  "OUT Please_input_0_or_1:
   
   INP choice
   IF @choice 0 input_of_zero
   IF @choice 1 input_of_one
   
   LBL input_of_one
     OUT @choice
     IF @choice @choice input_of_one
   
   LBL input_of_zero
     OUT @choice")

;;; -------------------------------------------------------

;; Query the user for a string and print its characters in a gradual
;; fashion, character by character, via string splitting.
;; 
;; Please heed that this example also serves as a forbisen for the
;; case-insensitive manner limning the CDILOI language's trajectory;
;; as a consectary, I apologize if the endeictic purpose should confound
;; the reader.
(interpret-CDILOI
  "
  inp userInput
  
  LBL split
  DeL @userInput firstInputCharacter restOfInput
  
  LBL testForEmptyString
  If @firstInputCharacter @emptyString ENDOFPROGRAM
  ouT @firstinputcharacter
  dEl @restOfInput FIRSTINPUTCHARACTER restOfInput
  If @restOfInput @restOfInput TESTforEMPTYstring
  
  LBL endOfProgram
  ")
