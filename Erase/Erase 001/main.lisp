;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Erase", invented by the Esolang user "BestCoder" and
;; presented on October 13th, 2025, its proprium's commorancy the
;; potential for its own program code's indagation and manipulation in
;; a character-wise fashion, the compass of which enumerates deletion
;; and insertion instruments, operating in conjunction with a character-
;; or line-number-based control flow mechanism.
;; 
;; 
;; Concept
;; =======
;; The Erase programming language subsumes itself into the specific
;; species to whom the capacitation for a program's self-modification
;; registers an inalienable constitution of the haecceity, operating in
;; this concrete on a character level in order delete or inserts
;; content, issue output, and navigate via conditional or unconditional
;; index-based control flow mechanisms across the code.
;; 
;; == ERASE: ERASE, INSERT, PRINT, AND NAVIGATE ==
;; Honored by a lealty to its agnomination, the Erase programming
;; language capacitates its own program code's modulation by adminiculum
;; of instructions dedicated to the sere character's erasure, insertion,
;; printing, as well as a peragration through the code via conditional
;; and unconditional "goto" constructs.
;; 
;; == ERASE: AN ADUNATION OF CODE AND DATA ==
;; In Erase, the relation's governal atwixen the program code and the
;; data patefies a veridicous and enkerly actuated symphytism, as both
;; the cynosure of a program's execution and the data castaldy's subject
;; conflate in the source code.
;; 
;; The provenance to these conjoined facilitations' pleroma wones in a
;; quintuple mountance of operative warklumes.
;; 
;; A corollary whose gendrure is obtained from these champarty states
;; that no sere memory partakes of the language's perimeter.
;; 
;; 
;; Syntax
;; ======
;; An Erase program's designment emerges from the foundation of a
;; catena of whitespace-separated tokens, such entalented with operative
;; potential listing their operands in immediate succeesion to the
;; command identifier's agnomination.
;; 
;; == PROGRAMS: INSTRUCTIONS + OPERANDS OR NO-OPERATIVE TOKENS ==
;; From the conspectuity's exercise upon the language's donet, an Erase
;; program's conformation ostends a sequence tallying zero or more
;; instructions or no-operative content, the merist to each attiguous
;; token twissel's segregation one or more whitespaces.
;; 
;; == INSTRUCTIONS: AN IDENTIFIER TOKEN SUCCEEDED BY ARGUMENTS ==
;; An instruction's parasceve experiences its patefaction in an
;; unambiguous word, composed of Latin minuscules, edifying a
;; prevenience to one or more operands, the complex itself designed with
;; one or more whitespaces in each interstices.
;; 
;; == OPERANDS: LITERALS, CHARACTER INDICES, LINE NUMBERS ==
;; The options for the instruction operands constitutes a trisulk inwith
;; whose compass are enhalsed literal characters, zero-based character
;; indices, and line numbers whose enumeration proceed from the same
;; nomothesy.
;; 
;; == OPERANDS: CHARACTER LITERALS ==
;; The aefauld argument choice whose specification ensues from the
;; notion of a immediate exposition, rather than an indirection's
;; resolution, is attested in the manifestation of a single character.
;; 
;; The admission to this species does not wist of any imposition in its
;; content, including whitespaces.
;; 
;; == OPERANDS: CHARACTER INDICES ==
;; Tacitly enumerating each character in the program code with an
;; integral subscript commencing in the index zero (0), a very
;; fine-grained control's vouchsafement constitutes the language's
;; dation for modifications and navigations.
;; 
;; The syntaxis as this capacitation's compernage imposes a character
;; location's specification via an unsigned integer number, its
;; componency amplecting one or more decimal digits in an unbroken
;; catena.
;; 
;; Expressed in an Extended Backus-Naur Form's (EBNF) plasmature:
;; 
;;   characterIndex := digit , { digit }
;; 
;; Please heed that any character, including newline entities,
;; contributes to this accompt, and as such limns a valid target for any
;; request.
;; 
;; == OPERANDS: LINE NUMBERS ==
;; A second option for a code position's designation is realized in the
;; line numbers, enumerated with zero (0) as the first row's subscript,
;; and signified by a dollar sign ("$") prefixion, immediately succeeded
;; by one or more decimal digits.
;; 
;; Expressed in an Extended Backus-Naur Form's (EBNF) plasmature:
;; 
;;   lineNumber := "$" , digit , { digit }
;; 
;; Its usance as a navigation target serves to relocate the instruction
;; pointer (IP) to the line's first character; siclike, in use cases
;; involving deletions and insertions, the first position, zero (0),
;; defines the desideratum.
;; 
;; == UNRECOGNIZED TOKENS COMMUNICATE NO-OPERATIONS (NOPS) ==
;; A token to whom no epiphenomenal causatum is adhibited forms a
;; no-operation, the vallidom of its deprehension neither entalented
;; with an influence on the program, nor a fault's instigation.
;; 
;; == GRAMMAR ==
;; An auctificial epexegesis' ilk in the bailiwick of the donat's
;; formulation shall be the following Extended Backus-Naur Form (EBNF)
;; tmema's vouchsafement:
;; 
;;   program        := [ command , { whitespace , command } ] ;
;;   command        := eraseCommand
;;                  |  printCommand
;;                  |  writeCommand
;;                  |  gotoCommand
;;                  |  ifCommand
;;                  ;
;;   eraseCommand   := "erase" , location ;
;;   printCommand   := "print" , location ;
;;   writeCommand   := "write" , character , location ;
;;   gotoCommand    := "goto"  , location ;
;;   ifCommand      := "if"    , location , character , location ;
;;   
;;   location       := characterIndex | lineNumber ;
;;   characterIndex := integer ;
;;   lineNumber     := "$" , integer ;
;;   
;;   integer        := digit , { digit } ;
;;   digit          := "0" | "1" | "2" | "3" | "4"
;;                  |  "5" | "6" | "7" | "8" | "9"
;;                  ;
;;   whitespace     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Erase's instruction set establishes a quintuple accompt's governance,
;; the warklumes inwith whose amplection such as to conduce the
;; capacitation for the program code's modulation by character-wise
;; deletions and insertions, such unit's output, as well as the
;; conditional and unconditional navigation to specific character
;; indices or line numbers.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be satisfied in a requisite mete
;; of gnarity's apprizal concerning the operative vouchsafements.
;; 
;; Please heed the demarcation of succedaneous tmemata via asterisks
;; ("*") apposted into an underlining catena's plasmature, their
;; occurrencies intended for the supersession by actual Erase code in
;; the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ==================================================================
;;   CONTENT MODIFICATION
;;   ------------------------------------------------------------------
;;   erase position    | Deletes the character at the {position} into
;;         ********    | the program code by substituting it with a
;;                     | space (" ") character.
;;                     |-----------------------------------------------
;;                     | {position} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the line's
;;                     |       first character is expunged.
;;   ..................................................................
;;   write char pos    | Replaces the character at the position {pos}
;;         **** ***    | into the program code by the character {char}.
;;                     |-----------------------------------------------
;;                     | {char} must be a character literal specifying
;;                     | the datum to insert at the index {pos} into
;;                     | the program code.
;;                     |-----------------------------------------------
;;                     | {pos} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the
;;                     |       insertion is peracted at the start of
;;                     |       this line.
;;   ==================================================================
;;   OUTPUT ISSUANCE
;;   ------------------------------------------------------------------
;;   print position    | Prints the character at the {position} into
;;         ********    | the program code to the standard output
;;                     | conduit.
;;                     |-----------------------------------------------
;;                     | {position} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the line's
;;                     |       first character is printed.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   goto destination  | Relocates the instruction pointer (IP) to the
;;        ***********  | {destination} position into the program code.
;;                     |-----------------------------------------------
;;                     | {destination} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the
;;                     |       instruction pointer is moved to the
;;                     |       specified line's first character.
;;   ..................................................................
;;   if pos guard dest | If the character located at the position {pos}
;;      *** ***** **** | equals the character {guard}, relocates the
;;                     | instruction pointer (IP) to the position
;;                     | {dest} in the program code; otherwise
;;                     | accompasses no causatum.
;;                     |-----------------------------------------------
;;                     | {pos} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the line's
;;                     |       first character is indagated.
;;                     |-----------------------------------------------
;;                     | {guard} must be a character literal specifying
;;                     | the datum to equiparate the character at the
;;                     | {pos} with.
;;                     |-----------------------------------------------
;;                     | {dest} must be either of:
;;                     |   (a) A zero-based index into the program
;;                     |       code.
;;                     |   (b) A zero-based line number into the
;;                     |       program code, in which case the
;;                     |       instruction pointer is moved to the
;;                     |       specified line's first character.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's development was peracted in the programming
;; language Common Lisp, its execution proceeding on a directum from
;; the source code to its causata's accompassing.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-03-24
;; 
;; Sources:
;;   [esolang:2025:Erase]
;;   The Esolang contributors, "Erase", October 14th, 2025
;;   URL: "https://esolangs.org/wiki/Erase"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   returns a veridicous Boolean truth value based upon the same,
   producing for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type standard-char +SPACE-CHARACTER+))

;;; -------------------------------------------------------

(defparameter +SPACE-CHARACTER+
  (code-char 32)
  "The space character, associated with the ASCII code 32, defined as a
   portable global constant.")

;;; -------------------------------------------------------

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (<= 10 (char-code candidate) 13))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (find candidate '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string  (source)
  "Converts the SOURCE into a simple string, either returning, for an
   input not already subsumed into this specialized species, a fresh
   ``simple-string'' object; otherwise, for a SOURCE compliant with the
   type, delivers the same in an ipsissima verba fashion."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun empty-string-p (source)
  "Determines whether the SOURCE represents a \"null string\", or empty
   string, thilk metes a length of exactly zero (0), returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-value
      (string= source ""))))

;;; -------------------------------------------------------

(defun integer-string-p (source)
  "Determines whether the SOURCE represents an integer literal, its
   componency a catena enumerating one or more decimal digits, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-value
      (and
        (not (empty-string-p source))
        (every #'digit-char-p source)))))

;;; -------------------------------------------------------

(defun line-number-string-p (source)
  "Determines whether the SOURCE represents a line number designator,
   its componency a prefixion by the dollar sign \"$\", succeeded by a
   catena enumerating one or more decimal digits, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (convert-into-a-boolean-value
      (and
        (not (empty-string-p source))
        (string= source #\$ :start1 0 :end1 1)
        (integer-string-p
          (subseq source 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token class.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-a-token (value start-position end-position)))
  "The ``Token'' class furnishes an encapsulation of a word extracted
   from a piece of Erase source code, usually demarcated from its
   environment by whitespaces, its componency a trisulc aggregation of
   the inclusive start position into the source, its exclusive end
   index, and the substring thus demarcated."
  (value          (error "No token content has been communicated.")
                  :type      T
                  :read-only T)
  (start-position (error "No start position has been communicated.")
                  :type      fixnum
                  :read-only T)
  (end-position   (error "No end position has been communicated.")
                  :type      fixnum
                  :read-only T))

;;; -------------------------------------------------------

(defun token-value-equals-p (token expected-content)
  "Determines whether the TOKEN's value equals the EXPECTED-CONTENT,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token         token))
  (declare (type simple-string expected-content))
  (the boolean
    (convert-into-a-boolean-value
      (string= (token-value token) expected-content))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command operand classes.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface establishes the common foundry for all
   classes pursuing the representation of Erase command arguments.")

;;; -------------------------------------------------------

(defstruct (Positional-Operand
  (:include Operand))
  "The ``Positional-Operand'' abstract class serves in the ensconcement
   of a command operand whose dever wones in the communication of an
   operand signifying a position into the Erase program code."
  (index (error "No index has been specified.")
         :type      fixnum
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Character-Index-Operand
  (:include     Positional-Operand)
  (:constructor make-a-character-index-operand (index)))
  "The ``Character-Index-Operand'' class serves in the representation
   of a command operand dedicated to the communication of a
   zero-based character position in the Erase program code.")

;;; -------------------------------------------------------

(defstruct (Line-Number-Operand
  (:include     Positional-Operand)
  (:constructor make-a-line-number-operand (index)))
  "The ``Line-Number-Operand'' class serves in the representation of a
   command operand dedicated to the communication of a zero-based line
   number in the Erase program code.")

;;; -------------------------------------------------------

(defstruct (Literal-Character-Operand
  (:include     Operand)
  (:constructor make-a-literal-character-operand (character)))
  "The ``Literal-Character-Operand'' class serves in the representation
   of a literal character as a command operand."
  (character (error "No character has been specified.")
             :type      character
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command classes.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface edifies a common foundry for all classes
   whose dever patefies in the representation of an Erase command.")

;;; -------------------------------------------------------

(defstruct (Erase-Command
  (:include     Command)
  (:constructor make-an-erase-command (position)))
  "The ``Erase-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the Erase command \"erase\", the diorism's
   compass amplecting the position in the program to expunge."
  (position (error "No test position has been specified.")
            :type      Positional-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Goto-Command
  (:include     Command)
  (:constructor make-a-goto-command (destination)))
  "The ``Goto-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the Erase command \"goto\", the diorism's
   compass amplecting the position in the program to relocate to in an
   unconditional fashion."
  (destination (error "No destination has been specified.")
               :type      Positional-Operand
               :read-only T))

;;; -------------------------------------------------------

(defstruct (If-Command
  (:include     Command)
  (:constructor make-an-if-command (test-position guard destination)))
  "The ``If-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the Erase command \"if\", the diorism's
   compass amplecting the position in the program to relocate to in the
   case of an specified antecedent's satisfaction."
  (test-position (error "No test position has been specified.")
                 :type      Positional-Operand
                 :read-only T)
  (guard         (error "No guard has been specified.")
                 :type      Literal-Character-Operand
                 :read-only T)
  (destination   (error "No destination has been specified.")
                 :type      Positional-Operand
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (NOP-Command
  (:include     Command)
  (:constructor make-a-nop-command ()))
  "The ``NOP-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the representation of a non-operative
   command, commonly affiliated with a token in eloignment from any
   epiphenomenal vallidom.")

;;; -------------------------------------------------------

(defstruct (Print-Command
  (:include     Command)
  (:constructor make-a-print-command (position)))
  "The ``Print-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the Erase command \"print\", the diorism's
   compass amplecting the position serving as the provenance of the
   desiderated character to print."
  (position (error "No test position has been specified.")
            :type      Positional-Operand
            :read-only T))

;;; -------------------------------------------------------

(defstruct (Write-Command
  (:include     Command)
  (:constructor make-a-write-command (character position)))
  "The ``Write-Command'' class applies itself to the furnishment of a
   plasmature dedicated to the Erase command \"write\", the diorism's
   compass amplecting the position in the program to substitute by a
   new character."
  (character (error "No character has been specified.")
             :type      Literal-Character-Operand
             :read-only T)
  (position  (error "No position has been specified.")
             :type      Positional-Operand
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Erase-Error (simple-error)
  ()
  (:documentation
    "The ``Erase-Error'' condition type serves as a firmament
     entreparted by all conditions concredited with the apprizal about
     anomalous circumstances emerging during an Erase program's
     interpretation."))

;;; -------------------------------------------------------

(define-condition Invalid-Line-Number-Error (Erase-Error)
  ()
  (:documentation
    "The ``Invalid-Line-Number-Error'' condition type applies itself to
     the apprizal about an anomalous circumstance whose etiology wones
     in the attempt to query or navigate to a line designated by an
     invalid number."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the condition type operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-an-invalid-line-number-error (offending-line-number)
  "Signals an error of the type ``Invalid-Line-Number-Error'', apprizing
   about the OFFENDING-LINE-NUMBER as the anomalous circumstance's
   etiology."
  (declare (type (integer 0 *) offending-line-number))
  (error 'Invalid-Line-Number-Error
    :format-control
      "The line number ~d exceeds the program's bournes."
    :format-arguments
      (list offending-line-number)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the interpreter's global variables.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type NOP-Command   +NOP-COMMAND+))

(declaim (type simple-string *program*))
(declaim (type fixnum        *ip*))
(declaim (type character     *current-character*))
(declaim (type boolean       *program-is-exhausted-p*))
(declaim (type fixnum        *program-size*))

;;; -------------------------------------------------------

(defparameter +NOP-COMMAND+
  (make-a-nop-command)
  "The global ``NOP-Command'' instance, representative of an
   unrecognized token or the Erase program's exhaustion.")

(defparameter *program* ""
  "The piece of Erase source code to operate upon.")

(defparameter *ip* 0
  "The current zero-based position into the Erase ``*PROGRAM*''.")

(define-symbol-macro *current-character*
  (the character
    (schar *program* *ip*)))

(define-symbol-macro *program-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *program* *ip*))))

(define-symbol-macro *program-size*
  (the fixnum
    (length *program*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program code traversal operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-a-new-program (new-program)
  "Sets the interpreter's program code to the NEW-PROGRAM, resets all
   appertaining state variables, and returns no value."
  (declare (type string new-program))
  (psetf
    *program* (convert-into-a-simple-string new-program)
    *ip*      0)
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-character ()
  "Advances to the next character in the ``*PROGRAM*'', if possible,
   and returns no value."
  (incf *ip*)
  (setf *ip* (min *ip* *program-size*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the token extraction operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-non-whitespace (start-position)
  "Proceeding from the inclusive START-POSITION into the ``*PROGRAM*'',
   locates the nearest following non-whitespace character and returns
   the same; or, upon its carency, responds with the
   ``*PROGRAM-SIZE*''."
  (declare (type fixnum start-position))
  (the fixnum
    (or (position-if-not #'whitespace-character-p *program*
          :start start-position)
        *program-size*)))

;;; -------------------------------------------------------

(defun locate-the-next-whitespace (start-position)
  "Proceeding from the inclusive START-POSITION into the ``*PROGRAM*'',
   locates the nearest following whitespace character and returns the
   same; or, upon its carency, responds with the ``*PROGRAM-SIZE*''."
  (declare (type fixnum start-position))
  (the fixnum
    (or (position-if #'whitespace-character-p *program*
          :start start-position)
        *program-size*)))

;;; -------------------------------------------------------

(defun demarcate-the-next-token (start-position)
  "Proceeding from the inclusive START-POSITIION into the ``*PROGRAM*'',
   locates the nearest following token and returns two values:
     (1) The substring comprehended in the next token.
     (2) The inclusive start index of  the next token.
     (3) The exclusive end   index of  the next token."
  (declare (type fixnum start-position))
  (the (values simple-string fixnum fixnum)
    (let* ((start-of-next-token
            (locate-the-next-non-whitespace start-position))
           (end-of-next-token
            (locate-the-next-whitespace start-of-next-token)))
      (declare (type fixnum start-of-next-token))
      (declare (type fixnum end-of-next-token))
      (values
        (subseq *program* start-of-next-token end-of-next-token)
        start-of-next-token
        end-of-next-token))))

;;; -------------------------------------------------------

(defun extract-the-next-token ()
  "Proceeding from the inclusive START-POSITIION into the ``*PROGRAM*'',
   locates the nearest following token and returns the same, while
   concomitantly advancing the instruction pointer (IP) ayond the
   occupied tmema."
  (the Token
    (multiple-value-bind (content start-position end-position)
        (demarcate-the-next-token *ip*)
      (declare (type simple-string content))
      (declare (type fixnum        start-position))
      (declare (type fixnum        end-position))
      (prog1
        (make-a-token content start-position end-position)
        (setf *ip* end-position)))))

;;; -------------------------------------------------------

(defun expect-a-single-whitespace ()
  "Determines whether the currently selected character in the
   ``*PROGRAM*'' constitutes a whitespace, on confirmation moving ayond
   the same, while returning no value; otherwise an error of an
   unspecified type is signaled."
  (cond
    (*program-is-exhausted-p*
      (error "A single whitespace was expected commencing at the ~
              position ~d, but instead the program has been found
              exhausted."
        *ip*))
    ((whitespace-character-p *current-character*)
      (advance-to-the-next-character))
    (T
      (error "A single whitespace was expected commencing at the ~
              position ~d, but instead the character \"~c\" has ~
              been detected."
        *ip* *current-character*)))
  (values))

;;; -------------------------------------------------------

(defun expect-one-or-more-whitespaces ()
  "Determines whether the currently selected character in the
   ``*PROGRAM*'' constitutes a whitespace, on confirmation moving ayond
   the same and any attiguous entity belonging to this species, while
   returning no value; otherwise an error of an unspecified type is
   signaled."
  (cond
    (*program-is-exhausted-p*
      (error "One or more whitespaces were expected commencing at the ~
              position ~d, but instead the program has been found
              exhausted."
        *ip*))
    ((whitespace-character-p *current-character*)
      (setf *ip*
        (locate-the-next-non-whitespace *ip*)))
    (T
      (error "One or more whitespaces were expected commencing at the ~
              position ~d, but instead the character \"~c\" has ~
              been detected."
        *ip* *current-character*)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parsing operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attempt-to-parse-the-token-as-a-character-index (token)
  "Attempts to parse the TOKEN as a character index, returning on
   success a covenable ``Character-Index-Operand'' encapsulation
   thereof; otherwise responds with the ``NIL'' sentinel."
  (declare (type Token token))
  (the (or null Character-Index-Operand)
    (when (integer-string-p (token-value token))
      (make-a-character-index-operand
        (parse-integer
          (token-value token))))))

;;; -------------------------------------------------------

(defun attempt-to-parse-the-token-as-a-line-number (token)
  "Attempts to parse the TOKEN as a line number, returning on success
   a covenable ``Line-Number-Operand'' encapsulation thereof; otherwise
   responds with the ``NIL'' sentinel."
  (declare (type Token token))
  (the (or null Line-Number-Operand)
    (when (line-number-string-p (token-value token))
      (make-a-line-number-operand
        (parse-integer
          (token-value token)
          :start 1)))))

;;; -------------------------------------------------------

(defun parse-a-positional-operand ()
  "Parses the current token desumed from the ``*PROGRAM*'' either as a
   character index or a line number and returns a covenable subtype of
   the ``Positional-Operand'' class."
  (the Positional-Operand
    (let ((token (extract-the-next-token)))
      (declare (type Token token))
      (or (attempt-to-parse-the-token-as-a-character-index token)
          (attempt-to-parse-the-token-as-a-line-number     token)
          (error "The token ~s, commencing at the position ~d and ~
                  ceasing before the index ~d, cannot be parsed as a ~
                  positional operand."
            (token-value          token)
            (token-start-position token)
            (token-end-position   token))))))

;;; -------------------------------------------------------

(defun parse-a-literal-character-operand ()
  "Parses the current token desumed from the ``*PROGRAM*'' as a literal
   character and returns a ``Literal-Character-Operand'' representation
   thereof."
  (the Literal-Character-Operand
    (cond
      (*program-is-exhausted-p*
        (error "A character literal were expected at the position ~d, ~
                but instead the program has been found exhausted."
          *ip*))
      (T
        (prog1
          (make-a-literal-character-operand *current-character*)
          (advance-to-the-next-character))))))

;;; -------------------------------------------------------

(defun parse-an-erase-command ()
  "Parses an Erase \"erase\" operation and returns a conable
   ``Erase-Command'' representation thereof."
  (the Erase-Command
    (make-an-erase-command
      (parse-a-positional-operand))))

;;; -------------------------------------------------------

(defun parse-a-goto-command ()
  "Parses an Erase \"goto\" operation and returns a conable
   ``Goto-Command'' representation thereof."
  (the Goto-Command
    (make-a-goto-command
      (parse-a-positional-operand))))

;;; -------------------------------------------------------

(defun parse-an-if-command ()
  "Parses an Erase \"if\" operation and returns a conable ``If-Command''
   representation thereof."
  (the If-Command
    (make-an-if-command
      (parse-a-positional-operand)
      (progn
        (expect-a-single-whitespace)
        (parse-a-literal-character-operand))
      (progn
        (expect-one-or-more-whitespaces)
        (parse-a-positional-operand)))))

;;; -------------------------------------------------------

(defun parse-a-print-command ()
  "Parses an Erase \"print\" operation and returns a conable
   ``Print-Command'' representation thereof."
  (the Print-Command
    (make-a-print-command
      (parse-a-positional-operand))))

;;; -------------------------------------------------------

(defun parse-a-write-command ()
  "Parses an Erase \"write\" operation and returns a conable
   ``Write-Command'' representation thereof."
  (the Write-Command
    (make-a-write-command
      (progn
        (expect-a-single-whitespace)
        (parse-a-literal-character-operand))
      (progn
        (expect-one-or-more-whitespaces)
        (parse-a-positional-operand)))))

;;; -------------------------------------------------------

(defun parse-a-nop-command ()
  "Returns a ``NOP-Command'' and advances the instruction pointer (IP)
   to the next position in the ``*PROGRAM*''."
  (the NOP-Command
    (prog1 +NOP-COMMAND+
      (advance-to-the-next-character))))

;;; -------------------------------------------------------

(defun extract-the-next-command ()
  "Proceeding from the current position into the ``*PROGRAM*'', consumes
   the next operation and returns a conable ``Command'' representation
   thereof."
  (the Command
    (let ((next-token (extract-the-next-token)))
      (declare (type Token next-token))
      (cond
        ((token-value-equals-p next-token "")
          +NOP-COMMAND+)
        ((token-value-equals-p next-token "erase")
          (parse-an-erase-command))
        ((token-value-equals-p next-token "goto")
          (parse-a-goto-command))
        ((token-value-equals-p next-token "if")
          (parse-an-if-command))
        ((token-value-equals-p next-token "print")
          (parse-a-print-command))
        ((token-value-equals-p next-token "write")
          (parse-a-write-command))
        (T
          (parse-a-nop-command))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the line number locator operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-newline (start-position)
  "Proceeding from the inclusive the START-POSITION into the
   ``*PROGRAM*'', locates the nearest following newline character and
   returns its zero-based position; or, upon its carency, responds with
   the ``NIL'' sentinel."
  (declare (type fixnum start-position))
  (the (or null fixnum)
    (position-if #'newline-character-p *program*
      :start start-position)))

;;; -------------------------------------------------------

(defun locate-a-specific-line (line-number)
  "Returns the zero-based index of the line's start point amenable to
   the zero-based LINE-NUMBER; or, upon its disrespondency, signals an
   error of the type ``Invalid-Line-Number-Error''."
  (declare (type (integer 0 *) line-number))
  (the fixnum
    (if (zerop line-number)
      0
      (loop
        repeat line-number
        
        for newline-position
          of-type (or null fixnum)
          =       (locate-the-next-newline 0)
          then    (locate-the-next-newline (1+ newline-position))
        
        unless newline-position do
          (signal-an-invalid-line-number-error line-number)
        
        finally
          (return
            (1+ newline-position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program modification operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erase-the-character-at (index)
  "Removes the character at the zero-based INDEX into the ``*PROGRAM*''
   and returns no value."
  (declare (type fixnum index))
  (setf (schar *program* index) +SPACE-CHARACTER+)
  (values))

;;; -------------------------------------------------------

(defun insert-a-character-at (index new-character)
  "Inserts the NEW-CHARACTER at the zero-based INDEX into the
   ``*PROGRAM*'' and returns no value."
  (declare (type fixnum    index))
  (declare (type character new-character))
  (setf (schar *program* index) new-character)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character index resolution operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extract-the-character-index (operand)
  (:documentation
    "Returns the character index into the ``*PROGRAM*'' communicated by
     the positional OPERAND.")
  
  (:method ((operand Character-Index-Operand))
    (declare (type Character-Index-Operand operand))
    (the fixnum
      (positional-operand-index operand)))
  
  (:method ((operand Line-Number-Operand))
    (declare (type Line-Number-Operand operand))
    (the fixnum
      (locate-a-specific-line
        (positional-operand-index operand)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the command evaluation operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-the-command (command)
  (:documentation
    "Evaluates the COMMAND in the contemporaneous ``*PROGRAM*'' state
     and returns no value.")
  
  (:method ((command Erase-Command))
    (declare (type Erase-Command command))
    (erase-the-character-at
      (extract-the-character-index
        (erase-command-position command)))
    (values))
  
  (:method ((command Goto-Command))
    (declare (type Goto-Command command))
    (setf *ip*
      (extract-the-character-index
        (goto-command-destination command)))
    (values))
  
  (:method ((command If-Command))
    (declare (type If-Command command))
    (let ((probed-character
            (schar *program*
              (extract-the-character-index
                (if-command-test-position command))))
          (guard-character
            (literal-character-operand-character
              (if-command-guard command))))
      (declare (type character probed-character))
      (declare (type character guard-character))
      (when (char= probed-character guard-character)
        (setf *ip*
          (extract-the-character-index
            (if-command-destination command)))))
    (values))
  
  (:method ((command NOP-Command))
    (declare (type NOP-Command command))
    (declare (ignore           command))
    (values))
  
  (:method ((command Print-Command))
    (declare (type Print-Command command))
    (format T "~c"
      (schar *program*
        (extract-the-character-index
          (print-command-position command))))
    (values))
  
  (:method ((command Write-Command))
    (declare (type Write-Command command))
    (insert-a-character-at
      (extract-the-character-index
        (write-command-position command))
      (literal-character-operand-character
        (write-command-character command)))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpretation operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-erase-code (code)
  "Interprets the piece of Erase source CODE and returns no value."
  (declare (type string code))
  (initialize-a-new-program code)
  (loop until *program-is-exhausted-p* do
    (process-the-command
      (extract-the-next-command)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "hello world" to the standard output conduit.
(interpret-the-erase-code
  "helo wrd
   print 0
   print 1
   print 2
   print 2
   print 3
   print 4
   print 5
   print 3
   print 6
   print 2
   print 7")

;;; -------------------------------------------------------

;; Pretended truth-machine which operates on a simulated zero (0)
;; input, located in the eighth column (zero-based index: 7) of the
;; first line.
(interpret-the-erase-code
  "input: 0
print 7
if 7 1 9")

;;; -------------------------------------------------------

;; Pretended truth-machine which operates on a simulated one (1)
;; input, located in the eighth column (zero-based index: 7) of the
;; first line.
(interpret-the-erase-code
  "input: 1
print 7
if 7 1 9")

;;; -------------------------------------------------------

;; "100 10 1" program.
(interpret-the-erase-code
  "100
print 0
print 1
print 2
print 3
if 2 0 $9
if 1 0 $11
if 0 0 $13
goto $15
erase 2
goto $1
erase 1
goto $1
erase 0
goto $1
end")
