;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "S.B.M.F.B", invented by the Esolang user "Yayimhere" and
;; presented on April 23rd, 2024, its hacceity reified in the castaldy
;; of an arbitrary tally of "brainfuck" code fragments, the latter
;; appertains to a programming language conceived by Urban Mueller and
;; endowed with the entelechy of Turing-completeness through a mere
;; octuple instruction set.
;; 
;; 
;; Concept
;; =======
;; The S.B.M.F.B programming language is empight on the foundry of a
;; stack dedicated to the maintenance of brainfuck code fragments,
;; permitting their execution on a shared tape.
;; 
;; == S.B.M.F.B: A "STACK-BASED MEMORY FOR BRAINFUCK" ==
;; A bewrayment of its provenance, purpose, and potentials, the
;; agnomination "S.B.M.F.B" decompresses to "Stack-Based Memory For
;; Brainfuck".
;; 
;; == THE ARCHITECTURE: A STACK OF CODE AND A SHARED BYTE TAPE ==
;; A twifold componency governs the entirety of S.B.M.F.B's
;; architecture, enumerating the code stack as the immediately pertinent
;; salvatory accommodated for the brainfuck code fragments' maintenance,
;; and complemented by the brainfuck memory as a bilaterally infinite
;; tape of unsigned bytes, shared across all code fragment executions.
;; 
;; == THE CODE STACK: A COLLECTION OF BRAINFUCK CODE ==
;; The brainfuck programs intended for their execution are consigned to
;; a stack's castaldy, the same does not wist of any surfeiture in its
;; attendance.
;; 
;; As a parergon to the standard stack operations, namely the pushing to
;; the top and this position's exclusive indagation, the S.B.M.F.B
;; programming language imposes an adscititous set of competences,
;; scilicet, the bottom member's access, and the rearrangement of its
;; elements' ordonnance via gradual shifting.
;; 
;; == THE BYTE TAPE: BRAINFUCK'S SHARED MEMORY ==
;; The vicarious entity commorant in the system testifies the brainfuck
;; memory's contribution, established as a bilaterally infinite tape of
;; cells, everichon among these an aefauld unsigned byte's woning,
;; measuring its capacity by the closed integral interval of [0, 255],
;; and wrapping around upon any of its bournes' transgression.
;; 
;; A mobile cell pointer is apportioned the dever of the currently
;; active cell's discrimination, selecting at any instant that sole
;; member to whom the perquisition and modulation procedures appertain.
;; Its stepwise progression along both lateralities capacitates this
;; cursor's unconstrained perambulation along the tape.
;; 
;; A unifying principle, the tape remains shared in its state across all
;; brainfuck code fragments' executions.
;; 
;; 
;; Instructions
;; ============
;; S.B.M.F.B's instruction set metes a sextuple cardinality in its
;; membership, the participants' dedication realized exclusively in the
;; castaldy of an arbitrary account of brainfuck code fragments via a
;; stack's adminiculum.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall be a cursory grade of gnarity's adhibition
;; anent the language's operative commodities.
;; 
;; Please heed the demarcation of succedaneous parcels by underlining
;; catenas of asterisks ("*"), intended for their supersession by actual
;; S.B.M.F.B code fragments in the final program.
;; 
;;   ------------------------------------------------------------------
;;   Command   | Effect
;;   ----------+-------------------------------------------------------
;;   €(bfCode) | Pushes the piece of brainfuck code {bfCode} onto the
;;     ******  | top of the code stack.
;;             |-------------------------------------------------------
;;             | {bfCode} must be a sequence of zero or more
;;             | characters, exempted from this set being the right
;;             | parenthesis (")"), a servant to the command's closure.
;;   ..................................................................
;;   run^      | Executes the piece of brainfuck code located at the
;;             | top of the code stack.
;;   ..................................................................
;;   runV      | Executes the piece of brainfuck code located at the
;;             | bottom of the code stack.
;;   ..................................................................
;;   run       | Executes the piece of brainfuck code located at the
;;             | top of the code stack.
;;   ..................................................................
;;   shift^    | Moves the piece of brainfuck code located at the top
;;             | of the code stack one position down.
;;   ..................................................................
;;   shiftV    | Moves the piece of brainfuck code located at the
;;             | bottom of the code stack one position down.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK INSTRUCTIONS ==
;; An investment of supererogation shall be procured by the following
;; tabular exposition of the brainfuck instructions, its presentation
;; entalented with compendiousness as a paravaunt characteristic:
;; 
;;   ------------------------------------------------------------------
;;   brainfuck command | Effect
;;   ------------------+-----------------------------------------------
;;   +                 | Increments the value of the cell designated by
;;                     | the cell pointer by one (1). If the result
;;                     | exceeds the upper march of 255, wraps the
;;                     | around to the lower bourne of zero (0).
;;   ..................................................................
;;   -                 | Decrements the value of the cell designated by
;;                     | the cell pointer by one (1). If the result
;;                     | descends alow the minimum of zero (0), wraps
;;                     | the state around to the upper extremum of 255.
;;   ..................................................................
;;   >                 | Translates the cell pointer one step to the
;;                     | right.
;;   ..................................................................
;;   <                 | Translates the cell pointer one step to the
;;                     | left.
;;   ..................................................................
;;   ,                 | Queries the standard input for a character and
;;                     | stores its ASCII code in the cell designated
;;                     | by the cell pointer.
;;   ..................................................................
;;   .                 | Prints the character whose ASCII code equals
;;                     | the value of the cell designated by the cell
;;                     | pointer to the standard output.
;;   ..................................................................
;;   [                 | If the value of the cell designated by the
;;                     | cell pointer equals zero (0), moves the
;;                     | instruction pointer (IP) forward to the
;;                     | position of the matching "]" token; otherwise
;;                     | proceeds as usual.
;;   ..................................................................
;;   ]                 | If the value of the cell designated by the
;;                     | cell pointer does not equal zero (0), moves
;;                     | the instruction pointer (IP) back to the
;;                     | position of the matching "[" token; otherwise
;;                     | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
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
;; Date:   2024-10-12
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024_6bytesofuselesselement]
;;   Th Esolang contributors, "6 bytes of useless element",
;;     October 4th, 2024
;;   URL: "https://esolangs.org/wiki/6_bytes_of_useless_element"
;;   
;;   [esolang2024SBMFB]
;;   The Esolang contributors, "S.B.M.F.B", September 23rd, 2024
;;   URL: "https://esolangs.org/wiki/S.B.M.F.B"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (type-name
                              (candidate-name &rest lambda-list)
                              &body body)
  "Defines a bespoke derived type, the agnomination of which is
   begotten by the TYPE-NAME's ipsissima verba appropriation, the
   formal parameters' diorism being actuated similiter from the
   LAMBDA-LIST, while the docimasy's subject receives its stevening
   from the CANDIDATE-NAME, being accessible, siclike to the
   LAMBDA-LIST, to the BODY forms, which are evaluated, the desinent
   form's primary return value contributing the assessment's
   conclusion, responding with a generalized boolean \"true\" value for
   the candidate's covenableness in regard to the defined type,
   otherwise expecting to respond with a \"false\" output.
   ---
   The first BODY form, upon its resolution to a string object, is
   adhibited the construe as the derived type's documentation string,
   as a corollary being appropriated for this telos."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (declare (ignorable   ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   T)
                                             (value-type T))
  "Defines a hash table compact of zero or more entries, every key of
   which adheres to the KEY-TYPE and responds with a value of the
   VALUE-TYPE, for both is defined the ``T'' type as the default."
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

(define-custom-type list-of (candidate &optional (element-type T))
  "Defines a list compact of zero or more elements, each member of
   which adheres to the ELEMENT-TYPE, for which is defined the ``T''
   type as the default."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, in this being a commorant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype opcode ()
  "The ``opcode'' type enumerates the recognized variation on S.B.M.F.B
   opcodes."
  '(member
    :push-brainfuck-code
    :run-top-of-stack
    :run-bottom-of-stack
    :run-top-of-stack-as-brainfuck
    :shift-top-down
    :shift-bottom-up))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which subsumes, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\"
   designator and returns a veridical Boolean tantamount thereof,
   producing for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   intrining in this diorism the actual space, the horizontal tab, as
   well as the newline specimens, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)
          (char= candidate #\Newline)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for
   an S.B.M.F.B keyword, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (alpha-char-p candidate)
          (find candidate "€^V" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string opcode) +OPCODES+))

;;; -------------------------------------------------------

(defparameter +OPCODES+
  (make-hash-table :test #'equal)
  "Associates the recognized S.B.M.F.B keywords as strings with their
   ``opcode'' representations.")

;;; -------------------------------------------------------

(flet ((register-opcode (identifier opcode)
        "Associates the S.B.M.F.B OPCODE with the IDENTIFIER and stores
         the combination in the +OPCODES+ table and returns no value."
        (declare (type string identifier))
        (declare (type opcode opcode))
        (setf (gethash identifier +OPCODES+) opcode)
        (values)))
  (register-opcode "€"      :push-brainfuck-code)
  (register-opcode "run^"   :run-top-of-stack)
  (register-opcode "runV"   :run-bottom-of-stack)
  (register-opcode "run"    :run-top-of-stack-as-brainfuck)
  (register-opcode "shiftV" :shift-bottom-up)
  (register-opcode "shift^" :shift-top-down)
  (values))

;;; -------------------------------------------------------

(defun parse-opcode (identifier)
  "Parses the IDENTIFIER and returns the represented opcode."
  (declare (type string identifier))
  (the opcode
    (or (gethash identifier +OPCODES+)
        (error "Unrecognized opcode identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tokenizer".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tokenizer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source for the tokenizer.")
    :reader        tokenizer-source
    :type          string
    :documentation "The string to analyze.")
   (position
    :initform      0
    :accessor      tokenizer-position
    :type          fixnum
    :documentation "The current position into the SOURCE."))
  (:documentation
    "The ``Tokenizer'' class vouchsafes a commodity nuncupated to the
     eath extraction of characters and tokens from a source string."))

;;; -------------------------------------------------------

(defun tokenizer-contains-index-p (tokenizer probed-index)
  "Determines whether the TOKENIZER comprehends the PROBED-INDEX in its
   marches, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tokenizer tokenizer))
  (declare (type fixnum    probed-index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (tokenizer-source tokenizer)
        probed-index))))

;;; -------------------------------------------------------

(defun tokenizer-has-more-content-p (tokenizer)
  "Determines whether the TOKENIZER comprehends one or more characters
   not yet processed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Tokenizer tokenizer))
  (the boolean
    (tokenizer-contains-index-p tokenizer
      (tokenizer-position tokenizer))))

;;; -------------------------------------------------------

(defun tokenizer-character (tokenizer)
  "Returns the character located at the current position into the
   TOKENIZER's source; or responds with ``NIL'' upon its marches'
   transgression."
  (declare (type Tokenizer tokenizer))
  (the (or null character)
    (when (tokenizer-has-more-content-p tokenizer)
      (char (tokenizer-source tokenizer)
        (tokenizer-position tokenizer)))))

;;; -------------------------------------------------------

(defun advance-to-next-character (tokenizer)
  "Moves the TOKENIZER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Tokenizer tokenizer))
  (when (tokenizer-has-more-content-p tokenizer)
    (incf (tokenizer-position tokenizer)))
  (values))

;;; -------------------------------------------------------

(defun expect-character (tokenizer expected-character)
  "Determines whether the TOKENIZER's current character equals the
   EXPECTED-CHARACTER, on confirmation advancing beyond the same, while
   returning no value; otherwise signals an error of an unspecified
   type."
  (declare (type Tokenizer tokenizer))
  (declare (type character expected-character))
  (cond
    ((not (tokenizer-has-more-content-p tokenizer))
      (error "Expected the character \"~c\" at position ~d, but ~
              encountered the source exhausted."
        expected-character
        (tokenizer-position tokenizer)))
    ((char/= (tokenizer-character tokenizer) expected-character)
      (error "Expected the character \"~c\" at position ~d, but ~
              encountered \"~c\"."
        expected-character
        (tokenizer-position  tokenizer)
        (tokenizer-character tokenizer)))
    (T
      (advance-to-next-character tokenizer)))
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   skips a sequence spanning zero or more accolent whitespaces and
   returns no value."
  (declare (type Tokenizer tokenizer))
  (setf (tokenizer-position tokenizer)
    (or
      (position-if-not
        #'whitespace-character-p
        (tokenizer-source tokenizer)
        :start (tokenizer-position tokenizer))
      (length
        (tokenizer-source tokenizer))))
  (values))

;;; -------------------------------------------------------

(defun read-brainfuck-code (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   reads a sequence of zero or more brainfuck-compatible tokens,
   concluded either with a closing parenthesis (\")\") or the source's
   exhaustion, and returns the thus consumed content as a fresh string."
  (declare (type Tokenizer tokenizer))
  (the string
    (subseq
      (tokenizer-source   tokenizer)
      (tokenizer-position tokenizer)
      (progn
        (setf (tokenizer-position tokenizer)
          (or
            (position #\)
              (tokenizer-source tokenizer)
              :start (tokenizer-position tokenizer)
              :test  #'char=)
            (length
              (tokenizer-source tokenizer))))
        (tokenizer-position tokenizer)))))

;;; -------------------------------------------------------

(defun read-identifier (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   reads an identifier and returns its string representation."
  (declare (type Tokenizer tokenizer))
  (the string
    (subseq
      (tokenizer-source   tokenizer)
      (tokenizer-position tokenizer)
      (progn
        (setf (tokenizer-position tokenizer)
          (or
            (position-if-not #'identifier-character-p
              (tokenizer-source tokenizer)
              :start (tokenizer-position tokenizer))
            (length
              (tokenizer-source tokenizer))))
        (tokenizer-position tokenizer)))))

;;; -------------------------------------------------------

(defun read-opcode (tokenizer)
  "Proceeding from the current position into the TOKENIZER's source,
   reads an identifier and returns its opcode representation."
  (declare (type Tokenizer tokenizer))
  (the opcode
    (parse-opcode
      (read-identifier tokenizer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "SBMFB-Instruction".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (SBMFB-Instruction
  (:constructor make-sbmfb-instruction (opcode
                                        &optional (operand NIL))))
  "The ``SBMFB-Instruction'' class applies itself to the encapsulation
   of an S.B.M.F.B operation as a twifold composition, comprehending a
   categorizing opcode an an optional string argument."
  (opcode  (error "Missing opcode.")
           :type      T
           :read-only T)
  (operand NIL
           :type      (or null string)
           :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "SBMFB-Program".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SBMFB-Program ()
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing instructions.")
    :reader        sbmfp-program-instructions
    :type          (list-of Instruction)
    :documentation "A list of the S.B.M.F.B operations."))
  (:documentation
    "The ``SBMFB-Program'' class serves in the encapsulation of a parsed
     S.B.M.F.B instruction sequence."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of S.B.M.F.B parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-sbmfb-program (tokenizer)
  "Employs the TOKENIZER to parse a piece of S.B.M.F.B source and
   returns an ``SBMFB-Program'' representation of the extracted
   instructions."
  (declare (type Tokenizer tokenizer))
  (the SBMFB-Program
    (make-instance 'SBMFB-Program :instructions
      (loop
        initially
          (skip-whitespaces tokenizer)
        while
          (tokenizer-has-more-content-p tokenizer)
        collect
          (prog1
            (let ((opcode (read-opcode tokenizer)))
              (declare (type opcode opcode))
              (case opcode
                (:push-brainfuck-code
                  (skip-whitespaces tokenizer)
                  (expect-character tokenizer #\()
                  (prog1
                    (make-sbmfb-instruction :push-brainfuck-code
                      (read-brainfuck-code tokenizer))
                    (expect-character tokenizer #\))))
                (otherwise
                  (make-sbmfb-instruction opcode))))
            (skip-whitespaces tokenizer))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Code-Stack".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Code-Stack ()
  ((elements
    :initform      NIL
    :accessor      code-stack-elements
    :type          (list-of string)
    :documentation "The brainfuck code fragments as a list-based
                    stack."))
  (:documentation
    "The ``Code-Stack'' class is apportioned the dever of an ordered
     sequence of brainfuck code fragments' castaldy, obeying in this
     onus the principles of the stack, natheless, extended to permit
     both container ends' indagation and manipulation."))

;;; -------------------------------------------------------

(defun make-empty-code-stack ()
  "Creates and returns a fresh and vacant ``Code-Stack''."
  (the Code-Stack
    (make-instance 'Code-Stack)))

;;; -------------------------------------------------------

(defun push-onto-code-stack (stack new-code-fragment)
  "Inserts the NEW-CODE-FRAGMENT at the code STACK's top and returns no
   value."
  (declare (type Code-Stack stack))
  (declare (type string     new-code-fragment))
  (push new-code-fragment
    (code-stack-elements stack))
  (values))

;;; -------------------------------------------------------

(defun get-top-code-stack-element (stack)
  "Returns the brainfuck code fragment commorant on the code STACK's
   top position."
  (declare (type Code-Stack stack))
  (the string
    (or (first (code-stack-elements stack))
        (error "The brainfuck program stack is empty."))))

;;; -------------------------------------------------------

(defun get-bottom-code-stack-element (stack)
  "Returns the brainfuck code fragment commorant on the code STACK's
   bottom position."
  (declare (type Code-Stack stack))
  (the string
    (or (first (last (code-stack-elements stack)))
        (error "The brainfuck program stack is empty."))))

;;; -------------------------------------------------------

(defun code-stack-contains-at-least-two-elements-p (stack)
  "Determines whether the code STACK comprehends at least two elements,
   which imposes a requisitum for several operations performs on its
   content, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Code-Stack stack))
  (the boolean
    (get-boolean-value-of
      (>= (length (code-stack-elements stack)) 2))))

;;; -------------------------------------------------------

(defun shift-code-stack-top-down (stack)
  "Moves the element at the top of the code STACK one position down, if
   possible, and returns no value."
  (declare (type Code-Stack stack))
  (when (code-stack-contains-at-least-two-elements-p stack)
    (rotatef
      (first  (code-stack-elements stack))
      (second (code-stack-elements stack))))
  (values))

;;; -------------------------------------------------------

(defun shift-code-stack-bottom-up (stack)
  "Moves the element at the bottom of the code STACK one position up, if
   possible, and returns no value."
  (declare (type Code-Stack stack))
  (when (code-stack-contains-at-least-two-elements-p stack)
    (let ((tail (last (code-stack-elements stack) 2)))
      (declare (type (list-of string) tail))
      (rotatef (first tail) (second tail))))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((stack Code-Stack) (sink T))
  (declare (type Code-Stack  stack))
  (declare (type destination sink))
  (format sink "~&Code-Stack with ~d element~:p:~{~%~2t~a~}"
    (length (code-stack-elements stack))
    (code-stack-elements stack)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck jump table.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Brainfuck-Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :reader        brainfuck-jump-table-connections
    :type          (hash-table-of fixnum fixnum)
    :documentation "Maps forward and back jump points in a bilateral
                    mode."))
  (:documentation
    "The ``Brainfuck-Jump-Table'' class furnishes a mapping betwixt
     jump points in a brainfuck program, communicated by mediation of
     their zero-based positions into the respective code."))

;;; -------------------------------------------------------

(defun make-empty-brainfuck-jump-table ()
  "Creates and returns a fresh and initially vacant
   ``Brainfuck-Jump-Table''."
  (the Brainfuck-Jump-Table
    (make-instance 'Brainfuck-Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (table start-point end-point)
  "Connects the jump START-POINT and the matching END-POINT in a
   bilateral fashion in the brainfuck jump TABLE and returns no value."
  (declare (type Brainfuck-Jump-Table table))
  (with-slots (connections) table
    (declare (type (hash-table-of fixnum fixnum) connections))
    (psetf (gethash start-point connections) end-point
           (gethash end-point   connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun get-jump-destination (table departure-point)
  "Returns the destination position for the jump DEPARTURE-POINT as
   registered in the brainfuck jump TABLE, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type Brainfuck-Jump-Table table))
  (declare (type fixnum               departure-point))
  (the fixnum
    (or (gethash departure-point
          (brainfuck-jump-table-connections table))
        (error "No destination point exists for the jump point ~d."
          departure-point))))

;;; -------------------------------------------------------

(defun calculate-brainfuck-jump-table (brainfuck-code)
  "Supputates and returns a jump table for the piece of BRAINFUCK-CODE."
  (declare (type string brainfuck-code))
  (let ((jump-table        (make-empty-brainfuck-jump-table))
        (jump-start-points NIL))
    (declare (type Brainfuck-Jump-Table jump-table))
    (declare (type (list-of fixnum)     jump-start-points))
    (loop
      for token            of-type character across brainfuck-code
      and current-position of-type fixnum    from   0 by 1
      
      if (char= token #\[) do
        (push current-position jump-start-points)
      else if (char= token #\]) do
        (if jump-start-points
          (connect-jump-points jump-table
            (pop jump-start-points)
            current-position)
          (error
            "Unmatched brainfuck \"]\" instruction at position ~d."
              current-position))
      end
      
      finally
        (when jump-start-points
          (error "Unmatched brainfuck \"[\" instruction~p at ~
                  position~:p ~{~d~^, ~}."
            (length jump-start-points)
            jump-start-points)))
    (the Brainfuck-Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck tape.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Brainfuck-Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :reader        brainfuck-tape-cells
    :type          (hash-table-of integer fixnum)
    :documentation "A sparse vector of unsigned byte-valued cells, each
                    such component amenable to a signed integer index.")
   (pointer
    :initform      0
    :accessor      brainfuck-tape-pointer
    :type          integer
    :documentation "The cell pointer which selects at any instant the
                    currently active unit by adminiculum of its
                    reference to the respective key into the CELLS."))
  (:documentation
    "The ``Brainfuck-Tape'' class realizes a brainfuck program's memory
     as a bilaterally infinite dispansion of unsigned byte-valued cells,
     operated upon by a mobile cell pointer."))

;;; -------------------------------------------------------

(defun make-empty-brainfuck-tape ()
  "Creates and returns a fresh ``Brainfuck-Tape''."
  (the Brainfuck-Tape
    (make-instance 'Brainfuck-Tape)))

;;; -------------------------------------------------------

(defun current-brainfuck-tape-cell (tape)
  "Returns the byte value stored in the brainfuck TAPE's current cell."
  (declare (type Brainfuck-Tape tape))
  (the octet
    (gethash
      (brainfuck-tape-pointer tape)
      (brainfuck-tape-cells   tape)
      0)))

;;; -------------------------------------------------------

(defun (setf current-brainfuck-tape-cell) (new-value tape)
  "Stores the NEW-VALUE in the brainfuck TAPE's current cell,
   contingently preceded by a wrapping of the same into the unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer        new-value))
  (declare (type Brainfuck-Tape tape))
  (setf (gethash
          (brainfuck-tape-pointer tape)
          (brainfuck-tape-cells   tape)
        0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-brainfuck-tape-pointer-right (tape)
  "Translates the brainfuck TAPE's cells pointer one step to the right
   and returns no value."
  (declare (type Brainfuck-Tape tape))
  (incf (brainfuck-tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-brainfuck-tape-pointer-left (tape)
  "Translates the brainfuck TAPE's cells pointer one step to the left
   and returns no value."
  (declare (type Brainfuck-Tape tape))
  (decf (brainfuck-tape-pointer tape))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "SBMFB-Interpreter".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SBMFB-Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing S.B.M.F.B program.")
    :reader        sbmfb-interpreter-program
    :type          SBMFB-Program
    :documentation "The S.B.M.F.B instructions to execute.")
   (code-stack
    :initform      (make-empty-code-stack)
    :reader        sbmfb-interpreter-code-stack
    :type          Code-Stack
    :documentation "Maintains the brainfuck code fragments in a stack.")
   (tape
    :initform      (make-empty-brainfuck-tape)
    :reader        sbmfb-interpreter-tape
    :documentation "The brainfuck memory tape maintained betwixt code
                    fragment execution instances."))
  (:documentation
    "The ``SBMFB-Interpreter'' applies itself to the castaldy of an
     S.B.M.F.B program and its execution stage."))

;;; -------------------------------------------------------

(defun execute-brainfuck-code (interpreter brainfuck-code)
  "Executes the piece of BRAINFUCK-CODE in the INTERPRETER's context
   and returns no value."
  (declare (type SBMFB-Interpreter interpreter))
  (declare (type string            brainfuck-code))
  (let ((ip         0)
        (jump-table (calculate-brainfuck-jump-table brainfuck-code)))
    (declare (type fixnum               ip))
    (declare (type Brainfuck-Jump-Table jump-table))
    (symbol-macrolet
        ((execution-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length brainfuck-code)))))
         (current-token
          (the character
            (char brainfuck-code ip))))
      (declare (type boolean   execution-completed-p))
      (declare (type character current-token))
      (with-slots (tape) interpreter
        (declare (type Brainfuck-Tape tape))
        (loop until execution-completed-p do
          (case current-token
            (#\+
              (incf (current-brainfuck-tape-cell tape)))
            (#\-
              (decf (current-brainfuck-tape-cell tape)))
            (#\>
              (move-brainfuck-tape-pointer-right tape))
            (#\<
              (move-brainfuck-tape-pointer-left tape))
            (#\,
              (format T "~&>> ")
              (finish-output)
              (setf (current-brainfuck-tape-cell tape)
                (char-code
                  (read-char NIL NIL #\Null)))
              (clear-input))
            (#\.
              (format T "~c"
                (code-char
                  (current-brainfuck-tape-cell tape))))
            (#\[
              (when (zerop (current-brainfuck-tape-cell tape))
                (setf ip
                  (get-jump-destination jump-table ip))))
            (#\]
              (unless (zerop (current-brainfuck-tape-cell tape))
                (setf ip
                  (get-jump-destination jump-table ip))))
            (otherwise NIL))
          (incf ip)))))
  (values))

;;; -------------------------------------------------------

(defun execute-sbmfb-program (interpreter)
  "Executes the S.B.M.F.B program governed by the INTERPRETER's castaldy
   and returns no value."
  (declare (type SBMFB-Interpreter interpreter))
  (with-slots (program code-stack) interpreter
    (declare (type SBMFB-Program program))
    (declare (type Code-Stack    code-stack))
    (dolist (current-instruction (sbmfp-program-instructions program))
      (declare (type SBMFB-Instruction current-instruction))
      (case (sbmfb-instruction-opcode current-instruction)
        
        (:push-brainfuck-code
          (push-onto-code-stack code-stack
            (sbmfb-instruction-operand current-instruction)))
        
        (:run-top-of-stack
          (execute-brainfuck-code interpreter
            (get-top-code-stack-element code-stack)))
        
        (:run-bottom-of-stack
          (execute-brainfuck-code interpreter
            (get-bottom-code-stack-element code-stack)))
        
        (:run-top-of-stack-as-brainfuck
          (execute-brainfuck-code interpreter
            (get-top-code-stack-element code-stack)))
        
        (:shift-top-down
          (shift-code-stack-top-down code-stack))
        
        (:shift-bottom-up
          (shift-code-stack-bottom-up code-stack))
        
        (otherwise
          (error "Unrecognized instruction: ~a."
            current-instruction)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-S.B.M.F.B (code)
  "Interprets the piece of S.B.M.F.B source CODE and returns no value."
  (declare (type string code))
  (execute-sbmfb-program
    (make-instance 'SBMFB-Interpreter :program
      (parse-sbmfb-program
        (make-instance 'Tokenizer :source code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-S.B.M.F.B
  "€(+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.)
   run")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
;; 
;; The brainfuck program
;;   ,[.,]
;; is spread across two S.B.M.F.B stack elements, which are assembled
;; in their apropos order.
(interpret-S.B.M.F.B
  "€(,)
   run
   shift^
   €([.,])
   run")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language
;; "6 bytes of useless element".
(interpret-S.B.M.F.B "€(--[----->+<]>.,,[.,])run")
