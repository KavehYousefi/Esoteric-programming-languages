;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NASAL", invented by the Esolang user "ChuckEsoteric08" and
;; presented on May 19th, 2023, the foundry of which derives from the
;; manipulation of a stack holding an arbitrary tally of nasal
;; characters from the set "m", "ɱ", "ɳ", "ɲ", "ŋ" and "ɴ", admitting
;; as the sole control flow construct a "while" loop.
;; 
;; 
;; Concept
;; =======
;; The NASAL programming language applies itself to its programs'
;; expressions via nasal consonents as warklooms for the designation of
;; one-character instruction names, the same operate on a memory compact
;; of a single stack admitting merely the sextuple character repertoire
;; "m", "ɱ", "ɳ", "ɲ", "ŋ", and "ɴ".
;; 
;; 
;; Architecture
;; ============
;; NASAL's architectural design is founded upon the stack as its aefauld
;; component, the same may store the sextuple character group "m", "ɱ",
;; "ɳ", "ɲ", "ŋ", and "ɴ".
;; 
;; 
;; Data Types
;; ==========
;; The type system of NASAL is exhausted by a singleton species: the
;; six characters "m", "ɱ", "ɳ", "ɲ", "ŋ", and "ɴ".
;; 
;; 
;; Syntax
;; ======
;; NASAL program's, when assuming a syntactical vista, are comprised of
;; single-letter instructions whose agnominations are desumed from
;; grammatical nasals, which concomitantly, in certain contexts, act in
;; the agency of arguments. Any other content, even whitespaces, does
;; not enjoy the language's tolerance and will serve as an error's
;; etiology.
;; 
;; == INSTRUCTIONS ==
;; All instructions are norned through singleton nasal constants, in a
;; daimen tally of occasions reliant upon operands whose elements are
;; desumed from the same repertoire "m", "ɱ", "ɳ", "ɲ", "ŋ", and "ɴ".
;; 
;; Any non-instruction and non-argument content is prohibited, which
;; amplects a sensible banishment of whitespaces.
;; 
;; == ARGUMENTS ==
;; The push operation and "while" loop header depend upon an aefauld
;; argument, accoutred via one of the nasals "m", "ɱ", "ɳ", "ɲ", "ŋ",
;; and "ɴ" for their completion.
;; 
;; == WHITESPACES ==
;; Whitespaces, as all tokens not amenable to a role as instruction or
;; argument identifiers, are inflicted with a strict interdiction.
;; 
;; == COMMENTS ==
;; No provision for comments is offered in the current language
;; rendition.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (ENBF) formulation shall
;; vouchsafe a more stringent ambience to the syntaxis' treatise:
;; 
;;   program        := commandList ;
;;   commandList    := { command } ;
;;   command        := push
;;                  |  pop
;;                  |  moveToBottom
;;                  |  moveToTop
;;                  |  whileLoop
;;                  ; 
;;   push           := "m" , nasalCharacter ;
;;   pop            := "ɱ" ;
;;   moveToBottom   := "ŋ" ;
;;   moveToTop      := "ɴ" ;
;;   whileLoop      := "ɳ" , nasalCharacter , comandList , "ɲ" ;
;;   nasalCharacter := "m" | "ɱ" | "ɳ" | "ɲ" | "ŋ" | "ɴ" ;
;; 
;; 
;; Instructions
;; ============
;; NASAL's instruction set tallies a sextuple membership, ordained to
;; the wikes of direct stack manipulation and a conditional control flow
;; helming mechanism, but deprived of any input and output conduits.
;; 
;; == OVERVIEW ==
;; An essential mete of acquaintance with the NASAL language's
;; operational facilities shall be the following table's dation.
;; 
;; Please heed that placeholder sections are underlined with asterisks
;; ("*"), and intended to be substituted by valid NASAL code in the
;; actual program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   mVALUE  | Pushes the {VALUE} unto the stack.
;;    *****  |---------------------------------------------------------
;;           | {VALUE} must be a valid character.
;;   ..................................................................
;;   ɱ       | Pops the top stack element and discards it.
;;           |---------------------------------------------------------
;;           | An error of the type "EmptyStackError" is elicited if
;;           | stack, at the invocation's moment, is empty.
;;   ..................................................................
;;   ŋ       | Relocates the stack's top element to the bottom.
;;           |---------------------------------------------------------
;;           | An error of the type "EmptyStackError" is elicited if
;;           | stack, at the invocation's moment, is empty.
;;   ..................................................................
;;   ɴ       | Relocates the stack's bottom element to the top.
;;           |---------------------------------------------------------
;;           | An error of the type "EmptyStackError" is elicited if
;;           | stack, at the invocation's moment, is empty.
;;   ..................................................................
;;   ɳVALUE  | Designates the start of a "while" loop, the same
;;    *****  | perpetuates until the stack's top element equals the
;;           | {VALUE}.
;;           |---------------------------------------------------------
;;           | {VALUE} must be a valid character.
;;           |---------------------------------------------------------
;;           | During the continuation criterion's probing, the top
;;           | stack element is peeked, but not removed. If the stack
;;           | is empty at this perquisition's instant, an error of the
;;           | type "EmptyStackError" is elicited.
;;   ..................................................................
;;   ɲ       | Designates the end of the matching "while" loop's start
;;           | ("n").
;;   ------------------------------------------------------------------
;;   
;; 
;; Lacunae in the Specification
;; ============================
;; The pellucid treatise afforded by the NASAL language's protolog
;; vanquishes almost all contingencies of ambiguous factors.
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
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-01
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023NASAL]
;;   The Esolang contributors, "NASAL", May 19th, 2023
;;   URL: "https://esolangs.org/wiki/NASAL"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   that conform to the ELEMENT-TYPE, the same default to the generic
   ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T predicate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype nasal-program ()
  "The ``nasal-program'' type defines an executable series of NASAL
   instructions as a vector of zero or more ``Instruction'' objects."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic ``*'' marker."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral connection betwixt while
   loop start and end instructions, realized as a hash table which maps
   the fixnum-typed locations of the same inside of a NASAL command
   sequence."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype nasal-character ()
  "The ``nasal-character'' type defines the set of valid characters"
  '(member #\m #\ɱ #\ɳ #\ɲ #\ŋ #\ɴ))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a stack composed of zero or more nasals,
   represented by ``nasal-character''s, realized as a list."
  '(list-of nasal-character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class encapsulates a significant object detected during
   the lexical analyzation of a piece of NASAL source code."
  (type  (error "Missing token type.")  :type keyword)
  (value (error "Missing token value.") :type T))

;;; -------------------------------------------------------

(defun token-of-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The piece of NASAL source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current location into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class furnishes a lexical analyzer, tasked with the
     recognition and extraction of significant objects from a piece of
     NASAL source code."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Sets the LEXER's current character to the first location into its
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
  "Creates and returns a new ``Lexer'' intended to operate on the NASAL
   SOURCE code."
  (declare (type string source))
  (the Lexer
    (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Advances the LEXER's position cursor to the next character in its
   source, if possible, and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source
          (incf position)))))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer token-type)
  "Consumed the LEXER's current character and returns a new token
   composed of the TOKEN-TYPE and the read character as its value."
  (declare (type Lexer   lexer))
  (declare (type keyword token-type))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token token-type
        (prog1 character
          (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-slots (character position) lexer
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (cond
      ((null character)
        (make-token :eof NIL))
      
      ((char= character #\m)
        (lexer-read-symbol lexer :push))
      
      ((char= character #\ɱ)
        (lexer-read-symbol lexer :pop))
      
      ((char= character #\ɳ)
        (lexer-read-symbol lexer :while))
      
      ((char= character #\ɲ)
        (lexer-read-symbol lexer :end-while))
      
      ((char= character #\ŋ)
        (lexer-read-symbol lexer :move-to-bottom))
      
      ((char= character #\ɴ)
        (lexer-read-symbol lexer :move-to-top))
      
      (T
        (error "Invalid character \"~c\" at position ~d."
          character position)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface furnishes a common foundry for all
   classes in their pursuit of modeling NASAL language features.")

;;; -------------------------------------------------------

(defstruct (Push-Instruction
  (:include Instruction))
  "The ``Push-Instruction'' class models the \"m\" command, the same
   pushes a nasal character unto the stack."
  (value (error "Missing value.") :type nasal-character))

;;; -------------------------------------------------------

(defstruct (Pop-Instruction
  (:include Instruction))
  "The ``Pop-Instruction'' class models the \"ɱ\" command, the same pops
   and discards the top stack element.")

;;; -------------------------------------------------------

(defstruct (While-Instruction
  (:include Instruction))
  "The ``While-Instruction'' class models the \"ɳ\" command, the same
   defines the header of a while loop, juxtaposing a nasal character
   with the top stack element for its continuation criterion's
   establishment."
  (guard (error "Missing guard.") :type nasal-character))

;;; -------------------------------------------------------

(defstruct (End-While-Instruction
  (:include Instruction))
  "The ``End-While-Instruction'' class models the \"ɲ\" command, the
   same serves to delineate the preceding while loop header's (\"ɳ\")
   concluding boundary.")

;;; -------------------------------------------------------

(defstruct (Move-To-Bottom-Instruction
  (:include Instruction))
  "The ``Move-To-Bottom-Instruction'' class models the \"ŋ\" command,
   the same serves to relocate the stack top element to the bottom.")

;;; -------------------------------------------------------

(defstruct (Move-To-Top-Instruction
  (:include Instruction))
  "The ``Move-To-Top-Instruction'' class models the \"ɴ\" commands, the
   same serves to relocate the stack bottom element to the top.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :type          Lexer
    :documentation "The lexer responsible for the token provision.")
   (current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recently acquired token from the lexer."))
  (:documentation
    "The ``Parser'' class' bailiwick comprehends the assemblage of an
     instruction sequence from a series of tokens provided by a lexer."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  "Initializes the PARSER's current token by querying its internally
   managed lexer for the same and returns no value."
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (setf current-token
      (lexer-get-next-token lexer)))
  (values))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' whose tokens' obtention is
   guaranteed by the LEXER."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while replacing it in the PARSER by the next token queried from the
   internally managed lexer; upon a mismatch, signaling an error of an
   unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Lexer lexer))
    (declare (type Token current-token))
    (the Token
      (if (token-of-type-p current-token expected-token-type)
        (prog1 current-token
          (setf current-token
            (lexer-get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parse-nasal-argument (parser)
  "Parses a nasal character utilizing the PARSER and returns its value."
  (the nasal-character
    (with-slots (current-token) parser
      (declare (type Token current-token))
      (case (token-type current-token)
        ((:push :pop :while :end-while :move-to-bottom :move-to-top)
          (token-value
            (parser-eat parser
              (token-type current-token))))
        (otherwise
          (error "Expected a nasal character token, but encountered ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-push (parser)
  "Parses a stack push instruction utilizing the PARSER and returns a
   ``Push-Instruction'' representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :push)
    (the Push-Instruction
      (make-push-instruction :value (parse-nasal-argument parser)))))

;;; -------------------------------------------------------

(defun parser-parse-while (parser)
  "Parses a while header instruction utilizing the PARSER and returns a
   ``While-Instruction'' representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (parser-eat parser :while)
    (the While-Instruction
      (make-while-instruction :guard (parse-nasal-argument parser)))))

;;; -------------------------------------------------------

(defun parser-parse-instruction (parser)
  "Parses a single NASAL instruction utilizing the PARSER and returns an
   ``Instruction'' representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Instruction
      (case (token-type current-token)
        (:push
          (parser-parse-push parser))
        (:pop
          (prog1
            (make-pop-instruction)
            (parser-eat parser :pop)))
        (:while
          (parser-parse-while parser))
        (:end-while
          (prog1
            (make-end-while-instruction)
            (parser-eat parser :end-while)))
        (:move-to-bottom
          (prog1
            (make-move-to-bottom-instruction)
            (parser-eat parser :move-to-bottom)))
        (:move-to-top
          (prog1
            (make-move-to-top-instruction)
            (parser-eat parser :move-to-top)))
        (otherwise
          (error "Invalid instruction token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-program (parser)
  "Assembles a NASAL program from the tokens acquired by the PARSER and
   returns the thus produced command sequence."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the nasal-program
      (coerce
        (loop
          until   (token-of-type-p current-token :eof)
          collect (parser-parse-instruction parser))
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (program)
  "Creates and returns a jump table for the PROGRAM which connects its
   while loop start and end instruction locations in the same."
  (let ((jump-table         (make-hash-table :test #'eql))
        (while-start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) while-start-points))
    (loop
      for instruction of-type Instruction across program
      and position    of-type fixnum      from   0 by 1
      
      if (while-instruction-p instruction) do
        (push position while-start-points)
      else if (end-while-instruction-p instruction) do
        (if while-start-points
          (let ((start-point (pop while-start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched while end instruction at position ~d."
            position))
      
      finally
        (when while-start-points
          (error "Unmatched while start instructions at positions ~
                  ~{~d~^, ~}."
            while-start-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Stack-Error (simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition serves to signal an anomalous
     situation instigated by the attempt to indagate or modify an empty
     stack."))

;;; -------------------------------------------------------

(defun signal-empty-stack-error (attempted-operation)
  "Signals an ``Empty-Stack-Error'' which apprizes about the
   ATTEMPTED-OPERATION's failure, the etiology of which constitutes an
   empty stack as the activity's target."
  (declare (type string attempted-operation))
  (error 'Empty-Stack-Error
    :format-control   "Cannot ~a an empty stack."
    :format-arguments (list attempted-operation)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing NASAL program.")
    :type          nasal-program
    :documentation "The NASAL instructions to process.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current location of the instruction pointer (IP)
                    in the PROGRAM.")
   (jump-table
    :initform      (make-hash-table :test #'eql)
    :type          jump-table
    :documentation "Associates the loop start and end locations in the
                    PROGRAM.")
   (stack
    :initform      NIL
    :type          stack
    :documentation "The program memory, realized as a stack of zero or
                    more nasal characters.")
   (print-debug-information-p
    :initarg       :print-debug-information-p
    :initform      T
    :type          boolean
    :documentation "Determines whether, succeeding each instruction's
                    invocation, the interpreter status shall be printed
                    to the standard output in order to monitor the
                    program state.")
   (execution-delay
    :initarg       :execution-delay
    :initform      0.6
    :type          (real 0.0 *)
    :documentation "Specifies the number of seconds to wait betwixt each
                    two consecutive instruction invocations, in
                    particular accommodated as a warklume for a
                    facilitated debugging status observation."))
  (:documentation
    "The ``Interpreter'' class is apportioned the wike of accompassing
     utility to a parsed NASAL program."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Populates the INTERPRETER's jump table with the contingent while loop
   bournes and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program jump-table) interpreter
    (declare (type nasal-program program))
    (declare (type jump-table    jump-table))
    (setf jump-table
      (build-jump-table program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program
                        &key (print-debug-information-p T)
                             (execution-delay           0.6))
  "Creates and returns a new ``Interpreter'', configured via the
   PRINT-DEBUG-INFORMATION-P and EXECUTION-DELAY options, the former of
   which enables the status monitoring, while the latter determines a
   delay betwixt two instructions' execution."
  (declare (type nasal-program program))
  (declare (type boolean       print-debug-information-p))
  (declare (type (real 0.0 *)  execution-delay))
  (the Interpreter
    (make-instance 'Interpreter
      :program                   program
      :print-debug-information-p print-debug-information-p
      :execution-delay           execution-delay)))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction in its program and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type nasal-program program))
    (declare (type fixnum        ip))
    (when (array-in-bounds-p program ip)
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpreter-jump-to (interpreter new-position)
  "Relocates the INTERPRETER's instruction pointer (IP) to the
   NEW-POSITION in its program and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-position))
  (with-slots (ip) interpreter
    (declare (type fixnum ip))
    (setf ip new-position))
  (values))

;;; -------------------------------------------------------

(defun interpreter-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's internally managed NASAL program
   has been entirely processed, which is imputed in the case of the
   instruction pointer (IP) having transcended the PROGRAM's bournes,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type nasal-program program))
    (declare (type fixnum        ip))
    (the boolean
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun interpreter-get-current-instruction (interpreter)
  "Returns the instruction referenced by the INTERPRETER's instruction
   pointer (IP) in its PROGRAM, or signals an error of an unspecified
   type if the same designates an invalid location."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type nasal-program program))
    (declare (type fixnum        ip))
    (the Instruction
      (aref program ip))))

;;; -------------------------------------------------------

(defun interpreter-peek-stack (interpreter)
  "Returns without removing the INTERPRETER's top stack element, or
   signals an error of the type ``Empty-Stack-Error'' upon its vacancy."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (the nasal-character
      (or (first stack)
          (signal-empty-stack-error "peek into")))))

;;; -------------------------------------------------------

(defun interpreter-pop-from-stack (interpreter)
  "Pops the top element from the INTERPRETER's stack and returns no
   value, or signals an error of the type ``Empty-Stack-Error'' upon its
   vacancy."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (or (pop stack)
        (signal-empty-stack-error "pop from")))
  (values))

;;; -------------------------------------------------------

(defun interpreter-push-unto-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter     interpreter))
  (declare (type nasal-character new-value))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (push new-value stack))
  (values))

;;; -------------------------------------------------------

(defun interpreter-move-to-bottom-of-stack (interpreter)
  "Relocates the element at the INTERPRETER stack's top to the bottom
   and returns no value, or signals an error of the type
   ``Empty-Stack-Error'' upon its vacancy."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (if stack
      (let ((top-element (pop stack)))
        (declare (type nasal-character top-element))
        (setf stack
          (append stack
            (list top-element))))
      (signal-empty-stack-error "rearrange")))
  (values))

;;; -------------------------------------------------------

(defun interpreter-move-to-top-of-stack (interpreter)
  "Relocates the element at the INTERPRETER stack's bottom to the top
   and returns no value, or signals an error of the type
   ``Empty-Stack-Error'' upon its vacancy"
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (if stack
      (let ((bottom-element (first (last stack))))
        (declare (type nasal-character bottom-element))
        (setf stack (nbutlast stack))
        (push bottom-element stack))
      (signal-empty-stack-error "rearrange")))
  (values))

;;; -------------------------------------------------------

(defun interpreter-delay (interpreter)
  "Waits the number of seconds specified in the INTERPRETER's execution
   delay configuration and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (execution-delay) interpreter
    (declare (type (real 0.0 *) execution-delay))
    (sleep execution-delay))
  (values))

;;; -------------------------------------------------------

(defun interpreter-print-debug-information (interpreter)
  "If configured in this fashion, prints the INTERPRETER's debug
   information, comprehending in particular its stack's content, to the
   standard output and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (print-debug-information-p stack) interpreter
    (declare (type boolean print-debug-information-p))
    (declare (type stack   stack))
    (declare (ignorable    stack))
    (when print-debug-information-p
      (format T "~&[top> ~{~a~^, ~} <bottom]" stack)
      (finish-output)))
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-process-instruction (interpreter instruction)
  (:documentation
    "Processes the INSTRUCTION in the INTERPRETER's context and returns
     no value."))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Push-Instruction))
  "Pushes the value stored in the push INSTRUCTION unto the
   INTERPRETER's stack and returns no value."
  (declare (type Interpreter      interpreter))
  (declare (type Push-Instruction instruction))
  (interpreter-push-unto-stack interpreter
    (push-instruction-value instruction))
  (interpreter-advance interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Pop-Instruction))
  "Ignores the pop INSTRUCTION, pops the INTERPRETER's top stack
   element, and returns no value."
  (declare (type Interpreter     interpreter))
  (declare (type Pop-Instruction instruction))
  (declare (ignore               instruction))
  (interpreter-pop-from-stack interpreter)
  (interpreter-advance        interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction While-Instruction))
  "Based upon the while loop INSTRUCTION's guard and the INTERPRETER's
   top stack element, either jumps beyond the end-while instruction,
   whose location in the NASAL program is specified by the INTERPRETER,
   or simply advances to the next instruction, thus executing the loop
   body, in any case returning no value."
  (declare (type Interpreter       interpreter))
  (declare (type While-Instruction instruction))
  (with-slots (jump-table ip) interpreter
    (declare (type jump-table jump-table))
    (declare (ignorable       jump-table))
    (declare (type fixnum     ip))
    (declare (ignorable       ip))
    (let ((guard     (while-instruction-guard instruction))
          (stack-top (interpreter-peek-stack  interpreter)))
      (declare (type nasal-character guard))
      (declare (type nasal-character stack-top))
      (if (char= guard stack-top)
        (interpreter-jump-to interpreter
          (1+ (gethash ip jump-table)))
        (interpreter-advance interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction End-While-Instruction))
  "Ignores the end-while INSTURCTION, unconditionally jumps back to the
   matching while loop header's position, as specified by the
   INTERPRETER's jump table, and returns no value."
  (declare (type Interpreter           interpreter))
  (declare (type End-While-Instruction instruction))
  (declare (ignore                     instruction))
  (with-slots (jump-table ip) interpreter
    (declare (type jump-table jump-table))
    (declare (type fixnum     ip))
    (interpreter-jump-to interpreter
      (gethash ip jump-table)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Move-to-Bottom-Instruction))
  "Ignores the move-to-bottom INSTRUCTION and shifts the INTERPRETER's
   top stack element to the bottom, finally returning no value."
  (declare (type Interpreter                interpreter))
  (declare (type Move-To-Bottom-Instruction instruction))
  (declare (ignore                          instruction))
  (interpreter-move-to-bottom-of-stack interpreter)
  (interpreter-advance                 interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-instruction
    ((interpreter Interpreter)
     (instruction Move-to-Top-Instruction))
  "Ignores the move-top-top INSTRUCTION and shifts the INTERPRETER's
   bottom stack element unto the top, finally returning no value."
  (declare (type Interpreter             interpreter))
  (declare (type Move-To-Top-Instruction instruction))
  (declare (ignore                       instruction))
  (interpreter-move-to-top-of-stack interpreter)
  (interpreter-advance              interpreter)
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Process the INTERPRETER's internally managed NASAL program and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-exhausted-p interpreter) do
    (interpreter-process-instruction interpreter
      (interpreter-get-current-instruction interpreter))
    (interpreter-delay                   interpreter)
    (interpreter-print-debug-information interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-NASAL (code
                        &key (print-debug-information-p T)
                             (execution-delay           0.6))
  "Interprets the piece of NASAL source CODE, configured via the
   PRINT-DEBUG-INFORMATION-P and EXECUTION-DELAY options, the former of
   which enables the status monitoring, while the latter determines a
   delay betwixt two instructions' execution, and returns no value."
  (declare (type string       code))
  (declare (type boolean      print-debug-information-p))
  (declare (type (real 0.0 *) execution-delay))
  (interpreter-interpret
    (make-interpreter
      (parser-parse-program
        (make-parser
          (make-lexer code)))
      :print-debug-information-p print-debug-information-p
      :execution-delay           execution-delay))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Push the characters "m", "ɴ", "ŋ" and "m", in this exact order, unto
;; the program stack, and pop the same inside of a while loop until the
;; "m" member's occasion terminates the iteration.
(interpret-NASAL "mmmɴmŋɳmɱɲ")

;;; -------------------------------------------------------

;; Repeatedly rearrange the stack elements "ŋ", "ɴ", "ɲ" and "m" until
;; "m" is located at the top.
(interpret-NASAL "mmmɲmɴmŋɳmŋɴŋɲ" :execution-delay 0.1)
