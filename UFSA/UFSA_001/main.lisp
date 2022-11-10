;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "UFSA", invented by the Esolang user
;; "PythonshellDebugwindow" in the year 2020, and based upon a
;; finite-state automaton whose configuration, encompassing the states
;; and transitions, is subject to definition by the program code, while
;; the trigger symbols, responding to modifications of the automaton's
;; state, must be supplied in precedence of the program's execution.
;; 
;; Concept
;; =======
;; UFSA operates an a finite-state automaton, conceptually a graph
;; composed of states as its vertices and transitions forming the
;; connecting edges. A program in this language requires the definition
;; of the initial state and the halt or accept states, signifying the
;; execution's successful termination, followed by the statements of its
;; transitions, each connecting a source to a destination state,
;; activated by a trigger symbol or, as a special case, via the input
;; string's exhaustion.
;; 
;; == UFSA IS BASED UPON A FINITE-STATE AUTOMATON ==
;; The language's central component is realized in its name's donor, the
;; finite-state automaton.
;; 
;; This type of abstractly conceived machine, also in currency under its
;; abbreviated form "FSA", is delineated by a set of states and
;; transitions. A state serves to define the automaton's current mode or
;; character. In order to administer modifications to the same, it may
;; be connected to any other state, including the option of itself, by a
;; transition, a connection identified by a symbol whose conincidence
;; with an input results in its activation. Whereas the states and
;; transitions tally among the incorporations of the system, the input
;; signal stream arrives from an enthetic source.
;; 
;; The commorant of exactly one state at any instant, the reception of
;; an input signal incites the automaton's transition from the current
;; to the affiliated destination state. The input source's exhaustion
;; signifies the machine's finalization.
;; 
;; == UFSA EMPLOYS A VARIATION OF THE FINITE-STATE AUTOMATON ==
;; The UFSA programming language introduces a kenspeckle variety of the
;; finite-state automaton.
;; 
;; Concomitant to its verbatim appropriation of the states and
;; transitions, as well as their interplay, and the adventitious nature
;; of the input source, an empty symbol, if specified for the state,
;; associates with an unconditional respondency to the input's
;; exhaustion, activating immediately in such a case.
;; 
;; A second derivation from the abstract concept, the presence and
;; contingencies of the input symbols are voided of their effect if any
;; of an a priori determined set of states, known as the halt or accept
;; states, has been reached, in which circumstance the program is
;; regarded as successfully concluded.
;; 
;; == UFSA PROGRAMS ARE STRUCTURED INTO LINES ==
;; UFSA programs are distributed along lines, discriminating two tiers:
;; the initialization line and the transition lines, the former of the
;; same fixates the initial state and the terminating ones, whereas the
;; latter category applies itself to the delineation of the transitions.
;; 
;; == THE INITIALIZATION LINE DEFINES INITIAL STATE AND HALT STATES ==
;; Comprising the incipient horizontal extent in the program --- apart
;; from vacant lines ---, the initialization line perforce mandates a
;; precedence of any extant transition line. Its kenspeckle nature
;; duplicates its deviating structure, with its first token availing to
;; establish the automaton's initial state, and any subsequent token,
;; separated from the first one as well as from each other by at least
;; one space, establishing the recognized halt or accept states, whose
;; assumption during runtime shall terminate the automaton. In
;; counterdistinction from other lines, the initialization form ought
;; to be unique to each program.
;; 
;; == TRANSITION LINES DEFINE STATES AND THEIR TRANSITIONS ==
;; More common than the singleton initialization specimen, transition
;; lines may occur an arbitrary tally of times, including the acceptance
;; of their omission. Their service resolves to the definition of the
;; states and their connecting transitions. To this end, each such row
;; is compact of two to four constituents:
;; 
;;   (1) The mandatory source state or "original state".
;;   (2) The mandatory destination stage or "transitional state".
;;   (3) An potentially empty "trigger symbol" which activates the
;;       transition.
;;   (4) An optional sequence of zero or more "output tokens" to print
;;       during the transition.
;; 
;; Its incipient token defines the state to depart from, known as the
;; "original state", leading into the "transitional state", determined
;; by the mandatory second token.
;; 
;; The third entity designates the input character which, when
;; encountered, leads from the original to the transitional state, known
;; as the "trigger symbol". If empty, the transition's activation does
;; not depend on a certain symbol, but instead transpires automatically
;; via the exhaustion of the input string.
;; 
;; Any further tokens are coalesced into a string yclept the
;; "output tokens", intended to be printed during the just defined
;; transition's activation. A dedicated parsing rule applies to this
;; portion's constituents, extending the literal characters' construe by
;; two variants of escaped entities, both introduced through a single
;; backslash ("\"):
;; 
;;   - Backslash escaping:
;;     A backslash immediately followed by another instance of the same
;;     is replaced by a single backslash; that is, "\\" produces the
;;     literal "\".
;;   
;;   - ASCII escape codes:
;;     A backslash immediately followed by one or more base-8 digits,
;;     that is, integers in the range [0, 7], is transformed into its
;;     decimal value and substituted by the ASCII character
;;     corresponding to this numeric code.
;; 
;; A backslash followed by no other character or succeeded by any other
;; instance not defined aboon is not recognized and concludes in an
;; error.
;; 
;; == AN EXTERNALLY SUPPLIED INPUT STRING CONDUCTS THE PROGRAM ==
;; Whereas a piece of UFSA source code defines the underlying state
;; machine, this portion of the whole actually only contributes a static
;; component to the complete infrastructure. The code itself does not
;; encompass activating instructions, such would determine the
;; occurrence and order of symbols to trigger transitions.
;; 
;; A consectary thereof, the input symbols must be provided in an
;; adventitious manner. Beside the establishment of a string form
;; regarding the same, the language standard abstains from imposing a
;; prescription in the matters of the source and conveyance.
;; 
;; 
;; Syntax
;; ======
;; UFSA founds upon a line-based format, with a bipartite division among
;; their types into a single initialization and an arbitrary tally of
;; transition lines, both species conceptually united by their division
;; into space-separated tokens.
;; 
;; == GRAMMAR ==
;; An expression of UFSA's donat shall be furnished in the following
;; Extended Backus-Naur Form (EBNF) formulation:
;; 
;;   program            := [ linebreaks ]
;;                      ,  [ initializationLine ]
;;                      ,  { linebreaks , transitionLine }
;;                      ,  [ linebreaks ]
;;                      ;
;;   
;;   initializationLine := [ spaces ] , state
;;                      ,  { spaces , state }
;;                      ,  [ spaces ]
;;                      ;
;;   transitionLine     := [ spaces ]
;;                      ,  state , spaces , state
;;                      ,  [ space , symbol , [ space , outputTokens ] ]
;;                      ,  [ spaces ]
;;                      ;
;;   
;;   outputTokens       := { character , escapedEntity } ;
;;   escapedEntity      := asciiEscapeCode | "\\" ;
;;   asciiEscapeCode    := "\" , octalDigit , { octalDigit } ;
;;   octalDigit         := "0" | "1" | "2" | "3"
;;                      |  "4" | "5" | "6" | "7"
;;                      ;
;;   state              := character , { character } ;
;;   linebreaks         := linebreak , { linebreak } ;
;;   linebreak          := "\n" ;
;;   character          := asciiCharacter - space ;
;;   spaces             := space , { space } ;
;;   space              := " " ;
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-10-26
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/UFSA"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation-specific declarations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(declaim (sb-ext:muffle-conditions style-warning))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype transition-table ()
  "The ``transition-table'' defines a mapping of trigger symbols to the
   transitions encapsulating the participating components in the form of
   a hash table from optional characters to ``Transition'' objects, with
   the special ``NIL'' sentinel as a representative of the empty trigger
   symbol."
  '(hash-table-of (or null character) Transition))

;;; -------------------------------------------------------

(deftype prompt-mode ()
  '(member :repeating :once))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-character-p (character)
  "Checks whether the CHARACTER represents a constituent admissive for
   tokens, returning on conformation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not
      (member character '(#\Space #\Tab #\Newline #\Linefeed)
        :Test #'char=))))

;;; -------------------------------------------------------

(defstruct (Lexer
  (:constructor initialize-lexer (source)))
  "The ``Lexer'' class provides a lexical analyzer specialized on a
   linewise processing of a piece of UFSA source code."
  (source        ""  :type string)
  (source-stream NIL :type (or null string-stream))
  (line          NIL :type (or null string))
  (position      0   :type fixnum)
  (character     NIL :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its accessors to abbreviated symbol macros
   for generalized access, evaluates the BODY forms, and returns the
   desinent form's results.
   ---
   The following correspondences hold betwixt the inherent lexer
   accessor functions and the adventitious symbol macros:
   
     ------------------------------------------------------------------
     Lexer accessor      | Local symbol macro
     --------------------+---------------------------------------------
     lexer-source        | source
     ..................................................................
     lexer-source-stream | source-stream
     ..................................................................
     lexer-line          | line
     ..................................................................
     lexer-position      | position
     ..................................................................
     lexer-character     | character
     ------------------------------------------------------------------
   
   As an act of supererogation, three symbol macros has been arranged as
   warklumes for enhanced convenience:
   
     ------------------------------------------------------------------
     Symbol macro | Effect
     -------------+----------------------------------------------------
     in-space-p   | Checks whether the character at the current line
                  | resides on a space.
     ..................................................................
     in-token-p   | Checks whether the character at the current line
                  | resides in a token.
     ..................................................................
     empty-line-p | Checks whether the current line is composed of
                  | spaces only.
     ------------------------------------------------------------------
   
   A set of four local functions contribute to a more convenient usance
   of the lexer:
   
     ------------------------------------------------------------------
     Local function | Effect
     ---------------+--------------------------------------------------
     read-next-line | Reads the next line from the lexer stores it.
     ..................................................................
     advance        | Moves to the next character in the current line.
     ..................................................................
     expect-space   | Expects the current character to be a space and
                    | skips it.
     ..................................................................
     skip-spaces    | Skips zero or more adjacent spaces.
     ------------------------------------------------------------------"
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            
            (source-stream
             (the (or null string-stream)
               (lexer-source-stream ,evaluated-lexer)))
            
            (line
             (the (or null string)
               (lexer-line ,evaluated-lexer)))
            
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer)))
            
            (in-space-p
             (the boolean
               (not (null
                 (and (lexer-character ,evaluated-lexer)
                      (char= (lexer-character ,evaluated-lexer)
                             #\Space))))))
            
            (in-token-p
             (the boolean
               (and (lexer-character ,evaluated-lexer)
                    (token-character-p
                      (lexer-character ,evaluated-lexer)))))
            
            (empty-line-p
             (the boolean
               (not (null
                 (and (lexer-line ,evaluated-lexer)
                      (every
                        #'(lambda (line-character)
                            (declare (type character line-character))
                            (char= line-character #\Space))
                        (lexer-line ,evaluated-lexer))))))))
         
         (labels
             ((read-next-line ()
               "Reads the next line from the SOURCE, if possible,
                updates the POSITION and CHARACTER, and returns no
                value."
               (setf line     (read-line source-stream NIL NIL))
               (setf position 0)
               (setf character
                 (when (and line (array-in-bounds-p line position))
                   (char line position)))
               (values))
              
              (advance ()
               "Moves the POSITION cursor to the next character in the
                line, if possible, updates the current CHARACTER, and
                returns no value."
               (setf character
                 (when (array-in-bounds-p line (1+ position))
                   (char line (incf position))))
               (values))
              
              (expect-space ()
               "Checks whether the current CHARACTER represents a space,
                on confirmation advancing to the next position and
                updating the current CHARACTER, otherwise signaling an
                error of an unspecified type."
               (if in-space-p
                 (advance)
                 (error "Expected a space, but encoutered ~s at ~
                         position ~d."
                   character position))
               (values))
              
              (skip-spaces ()
               "Skips a sequence of zero or more adjacent spaces,
                updates the current CHARACTER, and returns no value."
               (loop while in-space-p do
                 (advance))
               (values))
              
              (close-source ()
               "Closes the SOURCE-STREAM, thus rendering the line
                reading process invalid, and returns no value."
               (close source-stream)
               (values)))
           
           ,@body)))))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a new ``Lexer'' which serves to analyze the
   SOURCE."
  (declare (type string source))
  (let ((lexer (initialize-lexer source)))
    (declare (type Lexer lexer))
    (with-lexer (lexer)
      (setf source-stream (make-string-input-stream source))
      (read-next-line))
    (the Lexer lexer)))

;;; -------------------------------------------------------

(defun lexer-read-token (lexer)
  "Starting at the current position into the LEXER's source, reads a
   token composed of one or more characters and returns a string
   representation thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the string
      (if in-token-p
        (with-output-to-string (token)
          (declare (type string-stream token))
          (loop while in-token-p do
            (write-char character token)
            (advance)))
        (error "Expected a token character, but encountered ~s at ~
                position ~d."
          character position)))))

;;; -------------------------------------------------------

(defun lexer-read-symbol (lexer)
  "Starting at the current position into the LEXER's source, reads a
   single character and returns it."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the character
      (if in-token-p
        (prog1 character
          (advance))
        (error "Expected a symbol character, but encountered ~s at ~
                position ~d."
          character position)))))

;;; -------------------------------------------------------

(defun lexer-read-ascii-escape-code (lexer)
  "Starting at the current position into the LEXER's source, reads a
   sequence of one or more base-8 digits, interprets the resulting
   number as an ASCII code, and returns the corresponding character."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the character
      (code-char
        (parse-integer line
          :start position
          :end
            (loop
              while   (and character (digit-char-p character 8))
              do      (advance)
              finally (return position))
          :radix 8)))))

;;; -------------------------------------------------------

(defun lexer-read-rest-of-line (lexer)
  "Reads the LEXER's line content from the current position until the
   end of the same and returns the thus consumed characters entailed in
   a string."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the string
      (with-output-to-string (token)
        (declare  (type string-stream token))
        (loop while character do
          (case character
            ((NIL)
              (loop-finish))
            
            (#\\
              (advance)
              
              (cond
                ;; Backslash at end of the line.
                ((null character)
                  (error "Unterminated escape sequence at position ~d."
                    position))
                
                ;; \\ (escaped backslash "\")
                ((char= character #\\)
                  (write-char character token)
                  (advance))
                
                ;; \N (base-8 ASCII escape code)
                ((digit-char-p character 8)
                  (lexer-read-ascii-escape-code lexer))
                
                ;; Escaped standard character.
                (T
                  (write-char character token)
                  (advance))))
            
            (otherwise
              (write-char character token)
              (advance))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Transition".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Transition
  (:constructor make-transition (target-state output-tokens)))
  "The ``Transition'' class encapsulates the information associated with
   an outgoing connection from a state, excluding the source entity
   itself.
   ---
   Intended as an adminicular participant, this class lends a harbor to
   a very scant quantity of information --- that absolute requisite for
   affiliating a transition description for a state."
  (target-state
    (error "Missing transition state.")
    :type State)
  (output-tokens
    NIL
    :type (or null string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Transition-Error".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Transition-Error (error)
  ((offended-state
    :initarg       :offended-state
    :initform      (error "No offended state specified.")
    :reader        transition-error-offended-state
    :type          State
    :documentation "The state which should be departed employing the
                    FAILED-SYMBOL.")
   (failed-symbol
    :initarg       :failed-symbol
    :initform      NIL
    :reader        transition-error-failed-symbol
    :type          (or null character)
    :documentation "The trigger symbol which was attempted to be used
                    for transitioning from the OFFENDED-STATE."))
  (:report
    (lambda (condition stream)
      (declare (type Transition-Error condition))
      (declare (type destination      stream))
      (format stream "No transition associated with the trigger ~
                      symbol ~s for the state ~s."
        (transition-error-failed-symbol  condition)
        (transition-error-offended-state condition))))
  (:documentation
    "The ``Transition-Error'' represents an error whose signaling the
     attempt to transition from a state utilizing an undefined trigger
     symbol."))

;;; -------------------------------------------------------

(defun signal-transition-error (offended-state failed-symbol)
  "Signals a ``Transition-Error'' apprizing about the OFFENDED-STATE
   which could not be departed from using the FAILED-SYMBOL that is not
   defined for this state."
  (declare (type State               offended-state))
  (declare (type (or null character) failed-symbol))
  (error 'Transition-Error
    :offended-state offended-state
    :failed-symbol  failed-symbol))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "State".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (State
  (:constructor make-state (name)))
  "The ``State'' class represents a state in a state machine."
  (name        (error "Missing state name.") :type string)
  (transitions (make-hash-table :test #'eql) :type transition-table))

;;; -------------------------------------------------------

(defun state-add-transition (original-state
                             trigger-symbol
                             transition-state
                             &optional (output-tokens NIL))
  "Associates an outgoing transition from the ORIGINAL-STATE into the
   TRANSITION-STATE, activated by the TRIGGER-SYMBOL, and optionally
   accompanied by the OUTPUT-TOKENS as additional data, and returns the
   thus generated ``Transition'' object.
   ---
   A TRIGGER-SYMBOL of ``NIL'', that is, the empty symbol, is construed
   as to respond to an exhausted input symbol sequence."
  (declare (type State               original-state))
  (declare (type (or null character) trigger-symbol))
  (declare (type State               transition-state))
  (declare (type (or null string)    output-tokens))
  (the Transition
    (setf (gethash trigger-symbol (state-transitions original-state))
          (make-transition transition-state output-tokens))))

;;; -------------------------------------------------------

(defun state-get-transition (original-state trigger-symbol)
  "Returns the transition from the ORIGINAL-STATE activated by the
   TRIGGER-SYMBOL, or signals an error of the type ``Transition-Error''
   if no such affiliation is present."
  (declare (type State               original-state))
  (declare (type (or null character) trigger-symbol))
  (the Transition
    (or (gethash trigger-symbol (state-transitions original-state))
        (signal-transition-error original-state trigger-symbol))))

;;; -------------------------------------------------------

(defmethod print-object ((state State) stream)
  (declare (type State       state))
  (declare (type destination stream))
  (format stream "(STATE ~s" (state-name state))
  (maphash
    #'(lambda (trigger-symbol transition)
        (declare (type (or null character) trigger-symbol))
        (declare (type Transition          transition))
        (format stream ", ~a=>~a" trigger-symbol
          (state-name
            (transition-target-state transition))))
    (state-transitions state))
  (format stream ")"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "State-Machine".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass State-Machine ()
  ((initial-state
    :initarg       :initial-state
    :initform      NIL
    :type          (or null State)
    :documentation "The state  to assume during the start of the UFSA
                    program.")
   (halt-states
    :initarg       :halt-state
    :initform      NIL
    :type          (list-of string)
    :documentation "The state names defined as halting (accepting)
                    states, their entering designates the program's
                    successful termination.")
   (current-state
    :initarg       :current-state
    :initform      NIL
    :type          (or null string)
    :documentation "The name of the currently active state.")
   (states
    :initarg       :states
    :initform      (make-hash-table :test #'equalp)
    :type          (hash-table-of string State)
    :documentation "Associates the recognized state names with the
                    represented ``State'' objects in order to maintain
                    and access their transitions."))
  (:documentation
    "The ``State-Machine'' class models an automaton based upon the
     nexus betwixt states by transitions, optionally associated with an
     additional datum in the form of output tokens to print during the
     motion from one state to another."))

;;; -------------------------------------------------------

(defun make-state-machine ()
  "Creates and returns an initially empty ``State-Machine''."
  (the State-Machine
    (make-instance 'State-Machine)))

;;; -------------------------------------------------------

(defun ensure-state (state-machine state-name)
  "Ensures the presence of a ``State'' corresponding to the STATE-NAME
   in the STATE-MACHINE, upon necessity producing and persisting such a
   correspondence, finally returning either the extent or freshly
   established ``State'' for the identifier."
  (declare (type State-Machine state-machine))
  (declare (type string        state-name))
  (the State
    (with-slots (states) state-machine
      (declare (type (hash-table-of string State) states))
      (unless (gethash state-name states)
        (setf (gethash state-name states)
              (make-state state-name)))
      (gethash state-name states))))

;;; -------------------------------------------------------

(defun state-machine-set-initial-state (state-machine initial-state)
  "Sets the initial state to assume by the STATE-MACHINE to the
   INITIAL-STATE and returns the modified STATE-MACHINE."
  (declare (type State-Machine state-machine))
  (declare (type string        initial-state))
  (setf (slot-value state-machine 'initial-state)
        (ensure-state state-machine initial-state))
  (setf (slot-value state-machine 'current-state)
        initial-state)
  (the State-Machine state-machine))

;;; -------------------------------------------------------

(defun state-machine-set-halt-states (state-machine halt-states)
  "Defines the state names to be considered as halt (accept) states in
   the STATE-MACHINE and returns the modified STATE-MACHINE."
  (declare (type State-Machine    state-machine))
  (declare (type (list-of string) halt-states))
  ;; Store the halt state names.
  (setf (slot-value state-machine 'halt-states) halt-states)
  ;; Register each halt state as a ``State'' object, identified by its
  ;; name.
  (dolist (halt-state halt-states)
    (declare (type string halt-state))
    (ensure-state state-machine halt-state))
  (the State-Machine state-machine))

;;; -------------------------------------------------------

(defun state-machine-add-rule (state-machine
                               original-state
                               transition-state
                               trigger-symbol
                               &optional (output-tokens NIL))
  "Creates and registers in the STATE-MACHINE a new transition from the
   ORIGINAL-STATE to the TRANSITION-STATE, activated by an input of the
   TRIGGER-SYMBOL if residing in the former, optionally eventuating the
   OUTPUT-TOKENS upon the transition."
  (declare (type State-Machine       state-machine))
  (declare (type string              original-state))
  (declare (type string              transition-state))
  (declare (type (or null character) trigger-symbol))
  (declare (type (or null string)    output-tokens))
  (let ((origin (ensure-state state-machine original-state))
        (target (ensure-state state-machine transition-state)))
    (declare (type State origin))
    (declare (type State target))
    (the Transition
      (state-add-transition
        origin trigger-symbol target output-tokens))))

;;; -------------------------------------------------------

(defun state-machine-process-input (state-machine input)
  "Processes the INPUT symbol, attempting to transition from the
   STATE-MACHINE's current state into the target state, and returns the
   thus yielded transition object."
  (declare (type State-Machine       state-machine))
  (declare (type (or null character) input))
  (with-slots (current-state) state-machine
    (declare (type (or null string) current-state))
    (let ((transition
            (state-get-transition
              (ensure-state state-machine current-state)
              input)))
      (declare (type Transition transition))
      (setf current-state
            (state-name (transition-target-state transition)))
      (the Transition transition))))

;;; -------------------------------------------------------

(defun state-machine-in-halt-state-p (state-machine)
  "Checks whether the STATE-MACHINE's current state enumerates among the
   recognized halt (accept) states, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type State-Machine state-machine))
  (with-slots (current-state halt-states) state-machine
    (declare (type (or null string) current-state))
    (declare (type (list-of string) halt-states))
    (the boolean
      (not (null
        (and current-state
             (member current-state halt-states :test #'string=)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for the parser.")
    :type          Lexer
    :documentation "The lexical analyzer employed for the scanning of
                    the separated UFSA program lines.")
   (state-machine
    :initarg       :state-machine
    :initform      (make-state-machine)
    :type          State-Machine
    :documentation "The state machine generated from the UFSA program's
                    definitions."))
  (:documentation
    "The ``Parser'' class provides a unit responsible for the assemblage
     of a state machine from the tokens supplied by a lexer."))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which assembles the LEXER's
   tokens into a state machine."
  (declare (type Lexer lexer))
  (the Parser
    (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-skip-empty-lines (parser)
  "Skips zero or more adjacent empty lines encountered by the PARSR's
   internally managed lexer and returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (lexer) parser
    (declare (type Lexer lexer))
    (with-lexer (lexer)
      (loop while (and line empty-line-p) do
        (read-next-line))))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-initialization-line (parser)
  "Parses the initialization line using the PARSER and stores in its
   state machine the initial state and the halt states, ere returning
   the modified PARSER."
  (declare (type Parser parser))
  (with-slots (lexer) parser
    (declare (type Lexer lexer))
    (with-lexer (lexer)
      (let ((initial-state NIL)
            (halt-states   NIL))
        (declare (type (or null string) initial-state))
        (declare (type (list-of string) halt-states))
        
        (skip-spaces)
        
        (setf initial-state (lexer-read-token lexer))
        
        (loop while in-space-p do
          (skip-spaces)
          (push (lexer-read-token lexer) halt-states))
        
        (setf halt-states (nreverse halt-states))
        
        (with-slots (state-machine) parser
          (declare (type State-Machine state-machine))
          (state-machine-set-initial-state state-machine initial-state)
          (state-machine-set-halt-states state-machine halt-states)))))
  
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-transition-line (parser)
  "Parses a transition line using the PARSER and stores in its state
   machine the contained transition information from the original to the
   transition state, ere finally returning the modified PARSER."
  (declare (type Parser parser))
  (with-slots (lexer) parser
    (declare (type Lexer lexer))
    (with-lexer (lexer)
      (let ((original-state   NIL)
            (transition-state NIL)
            (trigger-symbol   NIL)
            (output-tokens    NIL))
        (declare (type (or null string)    original-state))
        (declare (type (or null string)    transition-state))
        (declare (type (or null character) trigger-symbol))
        (declare (type (or null string)    output-tokens))
        
        ;; Skip potential leading spaces.
        (skip-spaces)
        
        (setf original-state   (lexer-read-token lexer))
        (expect-space)
        (skip-spaces)
        (setf transition-state (lexer-read-token lexer))
        
        (when in-space-p
          (advance)
          
          (when in-token-p
            (setf trigger-symbol (lexer-read-symbol lexer))))
        
        ;; Maintain the provision for contingent output tokens by
        ;; skipping a present space.
        (advance)
        
        ;; Any further content is coalesced into the output tokens.
        (when character
          (setf output-tokens (lexer-read-rest-of-line lexer)))
        
        (state-machine-add-rule
          (slot-value parser 'state-machine)
          original-state
          transition-state
          trigger-symbol
          output-tokens))))
  
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the UFSA code maintained by the PARSER and returns a new state
   machine containing the stated configuration."
  (declare (type Parser parser))
  (with-slots (lexer) parser
    (declare (type Lexer lexer))
    (with-lexer (lexer)
      (loop
        while line
        with  first-line-p of-type boolean = T
        do
          (cond
            (empty-line-p
              (parser-skip-empty-lines parser))
            (first-line-p
              (setf first-line-p NIL)
              (parser-parse-initialization-line parser)
              (read-next-line))
            (T
              (parser-parse-transition-line parser)
              (read-next-line))))
      (close-source)))
  (the State-Machine
    (slot-value parser 'state-machine)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Input".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Input)
  "The ``Input'' interface defines a common foundation for objects
   intended for the representation of an input sequence of symbols for
   operating a UFSA program's transitions.")

;;; -------------------------------------------------------

(defgeneric input-get-next-symbol (input)
  (:documentation
    "Returns the next event symbol from the INPUT in the form of a
     character or the ``NIL'' value, the latter of which represents the
     empty symbol, a tantamount to the INPUT's exhaustion."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "String-Input".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (String-Input
  (:include     Input)
  (:constructor make-string-input (characters)))
  "The ``String-Input'' class defines an input sequence in the form of a
   string, the single characters of which compose the event symbols."
  (characters (error "Missing string input characters.") :type string)
  (position   0                                          :type fixnum))

;;; -------------------------------------------------------

(defmethod input-get-next-symbol ((input String-Input))
  (declare (type String-Input input))
  (the (or null character)
    (when (array-in-bounds-p (string-input-characters input)
                             (string-input-position   input))
      (prog1
        (char (string-input-characters input)
              (string-input-position   input))
        (incf (string-input-position input))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interactive-Input".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interactive-Input
  (:include     Input)
  (:constructor make-interactive-input
                  (&optional (prompt-text NIL)
                             (mode        :repeating))))
  "The ``Interactive-Input'' class implements an input sequence supply
   based on querying the user through the standard input for the event
   symbols.
   ---
   Upon each invocation of the ``input-get-next-symbol'' function, the
   user is prompted for a character to return as the next symbol. An
   user object equal to ``NIL'' or the space character is construed as
   the empty symbol, a tantamount to the input's exhaustion.
   ---
   The ``Interactive-Input'' class may operate in one of two modes: The
   \"repeating\" policy asks the user an indefinite tally of times for
   their next symbol; whereas the \"once\" mode signifies an exhaustion
   immediately after the first committed query."
  (prompt-text NIL        :type (or null string))
  (mode        :repeating :type prompt-mode)
  (finished-p  NIL        :type boolean))

;;; -------------------------------------------------------

(defmethod input-get-next-symbol ((input Interactive-Input))
  (declare (type Interactive-Input input))
  (the (or null character)
    (unless (interactive-input-finished-p input)
      ;; If in the once-mode, designate this execution as the desinent
      ;; one.
      (when (eq (interactive-input-mode input) :once)
        (setf (interactive-input-finished-p input) T))
      
      ;; Print the prompt message if specified.
      (when (interactive-input-prompt-text input)
        (format T "~&~a" (interactive-input-prompt-text input)))
      
      ;; Query and return an input character.
      (let ((user-input (read-char NIL NIL)))
        (declare (type (or null character) user-input))
        (clear-input)
          (case user-input
            ((NIL)               NIL)
            ((#\Space #\Newline) NIL)
            (otherwise           user-input))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((state-machine
    :initarg       :state-machine
    :initform      (error "Missing state machine for the interpreter.")
    :type          State-Machine
    :documentation "The state machine which represents the underlying
                    automaton.")
   (input
    :initarg       :input
    :initform      (error "Missing input for the interpreter.")
    :type          Input
    :documentation "The input stream that determines the sequence of
                    trigger symbols to apply in turn to the
                    STATE-MACHINE."))
  (:documentation
    "The ``Interpreter'' class constitutes the entity responsible for
     the adhibition of actual effect to an externally provided state
     machine and an input sequence whose elements define the trigger
     symbols to apply to the same."))

;;; -------------------------------------------------------

(defun make-interpreter (state-machine input)
  "Creates and returns a new ``Interpreter'' which applies the INPUT
   symbols unto the STATE-MACHINE."
  (declare (type State-Machine state-machine))
  (declare (type Input         input))
  (the Interpreter
    (make-instance 'Interpreter
      :state-machine state-machine
      :input         input)))

;;; -------------------------------------------------------

(defun interpreter-read-next-symbol (interpreter)
  (declare (type Interpreter interpreter))
  (the (or null character)
    (input-get-next-symbol
      (slot-value interpreter 'input))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the UFSA code represented by the compound of the
   INTERPRETER's internally managed state machine and input symbol
   sequence and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (state-machine) interpreter
    (declare (type State-Machine state-machine))
    (loop
      until
        (state-machine-in-halt-state-p state-machine)
      for input-character
        of-type (or null character)
        =       (interpreter-read-next-symbol interpreter)
      do
        (let ((transition (state-machine-process-input
                            state-machine
                            input-character)))
          (declare (type Transition transition))
          (when (transition-output-tokens transition)
            (format T "~a"
              (transition-output-tokens transition))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-UFSA (code input)
  "Interprets the piece of UFSA CODE, using the INPUT trigger symbols
   and returns no value."
  (declare (type string code))
  (declare (type Input  input))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))
      input))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-UFSA
  "i h
   i h  Hello, World!"
  (make-interactive-input "Please enter an input string: " :once))

;;; -------------------------------------------------------

;; One-time binary cat program.
(interpret-UFSA
  "c e
   c c 0 0
   c c 1 1
   c e"
  (make-interactive-input "Please enter either 0 or 1: " :once))

;;; -------------------------------------------------------

;; A one-time cat program which responds to the ASCII characters
;; covering the range [33, 255].
(interpret-UFSA
  "origin end
   origin end
   origin origin ! !
   origin origin \" \"
   origin origin # #
   origin origin $ $
   origin origin % %
   origin origin & &
   origin origin ' '
   origin origin ( (
   origin origin ) )
   origin origin * *
   origin origin + +
   origin origin , ,
   origin origin - -
   origin origin . .
   origin origin / /
   origin origin 0 0
   origin origin 1 1
   origin origin 2 2
   origin origin 3 3
   origin origin 4 4
   origin origin 5 5
   origin origin 6 6
   origin origin 7 7
   origin origin 8 8
   origin origin 9 9
   origin origin : :
   origin origin ; ;
   origin origin < <
   origin origin = =
   origin origin > >
   origin origin ? ?
   origin origin @ @
   origin origin A A
   origin origin B B
   origin origin C C
   origin origin D D
   origin origin E E
   origin origin F F
   origin origin G G
   origin origin H H
   origin origin I I
   origin origin J J
   origin origin K K
   origin origin L L
   origin origin M M
   origin origin N N
   origin origin O O
   origin origin P P
   origin origin Q Q
   origin origin R R
   origin origin S S
   origin origin T T
   origin origin U U
   origin origin V V
   origin origin W W
   origin origin X X
   origin origin Y Y
   origin origin Z Z
   origin origin [ [
   origin origin \\ \\\\
   origin origin ] ]
   origin origin ^ ^
   origin origin _ _
   origin origin ` `
   origin origin a a
   origin origin b b
   origin origin c c
   origin origin d d
   origin origin e e
   origin origin f f
   origin origin g g
   origin origin h h
   origin origin i i
   origin origin j j
   origin origin k k
   origin origin l l
   origin origin m m
   origin origin n n
   origin origin o o
   origin origin p p
   origin origin q q
   origin origin r r
   origin origin s s
   origin origin t t
   origin origin u u
   origin origin v v
   origin origin w w
   origin origin x x
   origin origin y y
   origin origin z z
   origin origin { {
   origin origin | |
   origin origin } }
   origin origin ~ ~
   origin origin  
   origin origin ? ?
   origin origin Å Å
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ç ç
   origin origin ? ?
   origin origin è è
   origin origin ê ê
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ù ù
   origin origin ? ?
   origin origin ? ?
   origin origin † †
   origin origin ° °
   origin origin ¢ ¢
   origin origin £ £
   origin origin § §
   origin origin • •
   origin origin ¶ ¶
   origin origin ß ß
   origin origin ® ®
   origin origin © ©
   origin origin ™ ™
   origin origin ´ ´
   origin origin ¨ ¨
   origin origin ≠ ≠
   origin origin Æ Æ
   origin origin Ø Ø
   origin origin ∞ ∞
   origin origin ± ±
   origin origin ≤ ≤
   origin origin ≥ ≥
   origin origin ¥ ¥
   origin origin µ µ
   origin origin ∂ ∂
   origin origin ∑ ∑
   origin origin ∏ ∏
   origin origin π π
   origin origin ∫ ∫
   origin origin ª ª
   origin origin º º
   origin origin Ω Ω
   origin origin æ æ
   origin origin ø ø
   origin origin ¿ ¿
   origin origin ¡ ¡
   origin origin ¬ ¬
   origin origin √ √
   origin origin ƒ ƒ
   origin origin ≈ ≈
   origin origin ∆ ∆
   origin origin « «
   origin origin » »
   origin origin … …
   origin origin    
   origin origin À À
   origin origin Ã Ã
   origin origin Õ Õ
   origin origin Œ Œ
   origin origin œ œ
   origin origin – –
   origin origin — —
   origin origin “ “
   origin origin ” ”
   origin origin ‘ ‘
   origin origin ’ ’
   origin origin ÷ ÷
   origin origin ◊ ◊
   origin origin ÿ ÿ
   origin origin Ÿ Ÿ
   origin origin ⁄ ⁄
   origin origin € €
   origin origin ‹ ‹
   origin origin › ›
   origin origin ﬁ ﬁ
   origin origin ﬂ ﬂ
   origin origin ‡ ‡
   origin origin · ·
   origin origin ‚ ‚
   origin origin „ „
   origin origin ‰ ‰
   origin origin Â Â
   origin origin Ê Ê
   origin origin Á Á
   origin origin Ë Ë
   origin origin È È
   origin origin Í Í
   origin origin Î Î
   origin origin Ï Ï
   origin origin Ì Ì
   origin origin Ó Ó
   origin origin Ô Ô
   origin origin  
   origin origin Ò Ò
   origin origin Ú Ú
   origin origin Û Û
   origin origin Ù Ù
   origin origin ı ı
   origin origin ˆ ˆ
   origin origin ˜ ˜
   origin origin ¯ ¯
   origin origin ˘ ˘
   origin origin ˙ ˙
   origin origin ˚ ˚
   origin origin ¸ ¸
   origin origin ˝ ˝
   origin origin ˛ ˛
   origin origin ˇ ˇ"
  (make-interactive-input "Please enter an ASCII character: " :once))

;;; -------------------------------------------------------

;; A repeating cat program which responds to the ASCII characters
;; covering the range [33, 255], terminating only if the input is
;; exhausted, signified by an empty value or a space.
(interpret-UFSA
  "origin end
   origin end
   origin origin ! !
   origin origin \" \"
   origin origin # #
   origin origin $ $
   origin origin % %
   origin origin & &
   origin origin ' '
   origin origin ( (
   origin origin ) )
   origin origin * *
   origin origin + +
   origin origin , ,
   origin origin - -
   origin origin . .
   origin origin / /
   origin origin 0 0
   origin origin 1 1
   origin origin 2 2
   origin origin 3 3
   origin origin 4 4
   origin origin 5 5
   origin origin 6 6
   origin origin 7 7
   origin origin 8 8
   origin origin 9 9
   origin origin : :
   origin origin ; ;
   origin origin < <
   origin origin = =
   origin origin > >
   origin origin ? ?
   origin origin @ @
   origin origin A A
   origin origin B B
   origin origin C C
   origin origin D D
   origin origin E E
   origin origin F F
   origin origin G G
   origin origin H H
   origin origin I I
   origin origin J J
   origin origin K K
   origin origin L L
   origin origin M M
   origin origin N N
   origin origin O O
   origin origin P P
   origin origin Q Q
   origin origin R R
   origin origin S S
   origin origin T T
   origin origin U U
   origin origin V V
   origin origin W W
   origin origin X X
   origin origin Y Y
   origin origin Z Z
   origin origin [ [
   origin origin \\ \\\\
   origin origin ] ]
   origin origin ^ ^
   origin origin _ _
   origin origin ` `
   origin origin a a
   origin origin b b
   origin origin c c
   origin origin d d
   origin origin e e
   origin origin f f
   origin origin g g
   origin origin h h
   origin origin i i
   origin origin j j
   origin origin k k
   origin origin l l
   origin origin m m
   origin origin n n
   origin origin o o
   origin origin p p
   origin origin q q
   origin origin r r
   origin origin s s
   origin origin t t
   origin origin u u
   origin origin v v
   origin origin w w
   origin origin x x
   origin origin y y
   origin origin z z
   origin origin { {
   origin origin | |
   origin origin } }
   origin origin ~ ~
   origin origin  
   origin origin ? ?
   origin origin Å Å
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ç ç
   origin origin ? ?
   origin origin è è
   origin origin ê ê
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ? ?
   origin origin ù ù
   origin origin ? ?
   origin origin ? ?
   origin origin † †
   origin origin ° °
   origin origin ¢ ¢
   origin origin £ £
   origin origin § §
   origin origin • •
   origin origin ¶ ¶
   origin origin ß ß
   origin origin ® ®
   origin origin © ©
   origin origin ™ ™
   origin origin ´ ´
   origin origin ¨ ¨
   origin origin ≠ ≠
   origin origin Æ Æ
   origin origin Ø Ø
   origin origin ∞ ∞
   origin origin ± ±
   origin origin ≤ ≤
   origin origin ≥ ≥
   origin origin ¥ ¥
   origin origin µ µ
   origin origin ∂ ∂
   origin origin ∑ ∑
   origin origin ∏ ∏
   origin origin π π
   origin origin ∫ ∫
   origin origin ª ª
   origin origin º º
   origin origin Ω Ω
   origin origin æ æ
   origin origin ø ø
   origin origin ¿ ¿
   origin origin ¡ ¡
   origin origin ¬ ¬
   origin origin √ √
   origin origin ƒ ƒ
   origin origin ≈ ≈
   origin origin ∆ ∆
   origin origin « «
   origin origin » »
   origin origin … …
   origin origin    
   origin origin À À
   origin origin Ã Ã
   origin origin Õ Õ
   origin origin Œ Œ
   origin origin œ œ
   origin origin – –
   origin origin — —
   origin origin “ “
   origin origin ” ”
   origin origin ‘ ‘
   origin origin ’ ’
   origin origin ÷ ÷
   origin origin ◊ ◊
   origin origin ÿ ÿ
   origin origin Ÿ Ÿ
   origin origin ⁄ ⁄
   origin origin € €
   origin origin ‹ ‹
   origin origin › ›
   origin origin ﬁ ﬁ
   origin origin ﬂ ﬂ
   origin origin ‡ ‡
   origin origin · ·
   origin origin ‚ ‚
   origin origin „ „
   origin origin ‰ ‰
   origin origin Â Â
   origin origin Ê Ê
   origin origin Á Á
   origin origin Ë Ë
   origin origin È È
   origin origin Í Í
   origin origin Î Î
   origin origin Ï Ï
   origin origin Ì Ì
   origin origin Ó Ó
   origin origin Ô Ô
   origin origin  
   origin origin Ò Ò
   origin origin Ú Ú
   origin origin Û Û
   origin origin Ù Ù
   origin origin ı ı
   origin origin ˆ ˆ
   origin origin ˜ ˜
   origin origin ¯ ¯
   origin origin ˘ ˘
   origin origin ˙ ˙
   origin origin ˚ ˚
   origin origin ¸ ¸
   origin origin ˝ ˝
   origin origin ˛ ˛
   origin origin ˇ ˇ"
  (make-interactive-input
    "Please enter an ASCII character: "
    :repeating))

;;; -------------------------------------------------------

;; A truth-machine.
(interpret-UFSA
  "start end
   start end 0 0
   start repeat 1 1
   repeat repeat  1"
  (make-interactive-input "Please enter either 0 or 1: " :once))
