;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "MM1char", invented by the Esolang user "ChuckEsoteric08"
;; and presented on May 28th, 2023, the foundation of which constitutes
;; a derivation from the two-register Minsky machine, however,
;; introducing a kenspeckle set of control flow mechanisms, the same
;; involves the conditional skipping based on the register state and a
;; label-based goto commodity, in lieu of Minsky's automatic relocation
;; in the case of decrementation.
;; 
;; 
;; Concept
;; =======
;; Founded upon a particular scheme of the Minsky machine, employing two
;; registers of unbounded mickleness and any polarity, MM1char adhibits
;; two deviating elements from the entheus' concept, which incorporates
;; a state transition upon an attempted decrementation of a zero-valued
;; register.
;; 
;; Imprimis, labels may be defined and visited without impounding
;; stipulations; furthermore, an instruction's conditional omission is
;; capacitated its accompassing in response to an explicit request.
;; 
;; 
;; Architecture
;; ============
;; Two registers, designated by the indices one (1) and two (2), already
;; exhaust the available program memory, the contents of the same
;; defined as a scalar integer of any sign and magnitude.
;; 
;; 
;; Data Types
;; ==========
;; MM1char wists of an aefauld data type: the signed integer whose
;; mickleness is unbridled.
;; 
;; 
;; Syntax
;; ======
;; An MM1char program's donat is limned by a sequence of commands, the
;; superior tally of which identifies with an aefauld character and
;; abstains from an argument reception; solely two specimens,
;; appertaining to the label definition and sojourn, involve a more
;; intricate design's imposition.
;; 
;; == INSTRUCTIONS ==
;; A preponderance among the commands participates in a single-character
;; form; an exemption is established by the twain of label declaration
;; and redirection, both ensconcing an identifier betwixt two marches,
;; the former of which resorting to a jumelle of brackets, "[" and "]",
;; the latter to parentheses, "(" and ")".
;; 
;; == LABEL NAMES ==
;; Label names, their length homologated to zero or more characters, may
;; appropriate any symbol except for their ensconcement's prerogatives,
;; namely the bracket pair "[" and "]" and the parentheses "(" and ")".
;; 
;; == WHITESPACES ==
;; The interspersion of whitespaces, a term whose diorism lays its
;; amplectation around spaces, horizontal tabs, and newlines, depends
;; merely on the programmer's personal delectation.
;; 
;; == COMMENT ==
;; No provision for comments is offered in the current language
;; rendition.
;; 
;; == GRAMMAR ==
;; A formulation in the Extended Backus-Naur Form (EBNF) shall provide
;; a more sophisticated insight into the language's donet:
;; 
;;   program               := { command | whitespaces } ;
;;   command               := incrementRegister1
;;                         |  decrementRegister1
;;                         |  incrementRegister2
;;                         |  decrementRegister2
;;                         |  skipIfRegister1IsZero
;;                         |  skipIfRegister2IsZero
;;                         |  declareLabel
;;                         |  gotoLabel
;;                         ;
;;   incrementRegister1    := "+" ;
;;   decrementRegister1    := "-" ;
;;   incrementRegister2    := "*" ;
;;   decrementRegister2    := "/" ;
;;   skipIfRegister1IsZero := "~" ;
;;   skipIfRegister2IsZero := "`" ;
;;   declareLabel          := "[" , labelName , "]" ;
;;   gotoLabel             := "(" , labelName , ")" ;
;;   labelName             := { labelCharacter } ;
;;   labelCharacter        := character - ( "[" | "]" | "(" | ")" ) ;
;;   whitespaces           := { whitespace } ;
;;   whitespace            := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; MM1char's instruction set is composed of an octuple membership, the
;; perimeter of the same amplects register manipulation, conditional
;; execution, and a label-based goto facility.
;; 
;; == OVERVIEW ==
;; An apercu shall administer a basic acquaintance with the commands to
;; one's avail.
;; 
;; Please heed that placeholder segments are distinguished by an
;; underlines composed of equality signs ("="), and intended to be
;; supplanted by valid MM1char code.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Effect
;;   ------------+-----------------------------------------------------
;;   +           | Increments the register 1 by one (1).
;;   ..................................................................
;;   -           | Decrements the register 1 by one (1).
;;   ..................................................................
;;   *           | Increments the register 2 by one (1).
;;   ..................................................................
;;   /           | Decrements the register 2 by one (1).
;;   ..................................................................
;;   ~           | If the value of the register 1 equals zero (0), the
;;               | next command is skipped.
;;   ..................................................................
;;   `           | If the value of the register 2 equals zero (0), the
;;               | next command is skipped.
;;   ..................................................................
;;   [labelName] | Declares a label identified by the {labelName} and
;;    =========  | associates it with the instruction pointer (IP)
;;               | position of its encounter in the program.
;;   ..................................................................
;;   (labelName) | Relocates the instruction pointer (IP) to the
;;    =========  | position associated with the {labelName}.
;;               | If no such entry exists, an error of an unspecified
;;               | type will be signaled.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its capacity to relay a preponderance among the issues to the
;; Minsky machine that has begotten its presence, MM1char's curtailed
;; treatise ostends a few ambiguous passages, a subset therefrom shall
;; be desumed by this section.
;; 
;; == ARE NEGATIVE REGISTER VALUES HOMOLOGATED? ==
;; MM1char, as a scion proceeding from the Minsky machine, assumes the
;; two integer registers whose imposition accompasses an equiparation
;; with the Turing machine's perimeter. Counterdistinguished from the
;; entheus' regulation that states a register's decrementing to be
;; effective only if differentiated from the zero (0) state, which
;; aliter resorts to a jumping operation, MM1char extricates these two
;; responses into unconditional decrementing and label-based control
;; flow. An explicit sanctioning or interdiction regarding the
;; registers' deduction into the negative moeity is wanting.
;; 
;; It has been adjudged that registers may be decremented below zero
;; (0).
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp relies on a conflation of the
;; lexical analyzation and the parsing stages, the cynosure of the
;; project, however, is delineated by an endeictic element, the
;; employment of macros for facilitated Lisp code generation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-21
;; 
;; Sources:
;;   [esolang2020Minskymachine]
;;   The Esolang contributors, "Minsky machine", July 2nd, 2020
;;   URL: "https://esolangs.org/wiki/Minsky_machine"
;;   Notes:
;;     - Presents the Minsky machine
;;   
;;   [esolang2023MM1char]
;;   The Esolang contributors, "MM1char", May 28th, 2023
;;   URL: "https://esolangs.org/wiki/MM1char"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (name (candidate-variable &rest parameter-names)
     &body body)
  "Creates a new derived type of the NAME using the ``deftype''
   infrastructure in coefficacy with the ``satisfies'' predicate,
   specifying the PARAMETER-NAMES as a part of its signature, the probed
   object committed to a docimasy being communicated to the predicate by
   mediation of the CANDIDATE-VARIABLE, while the BODY forms are
   evaluated, and the desinent form's results returned."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,name (,@parameter-names)
       ,(when (stringp (first body))
          (pop body))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
            #'(lambda (,candidate-variable)
                (declare (type T ,candidate-variable))
                ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-predicated-type property-list-of
    (candidate &optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   composed of zero or more entries, each indicator, or key, of which
   conforms to the INDICATOR-TYPE, associated with a value of the
   VALUE-TYPE, "
  (and
    (listp candidate)
    (evenp (length (the list candidate)))
    (loop
      for (indicator value)
        of-type (T T)
        on      (the list candidate)
        by      #'cddr
      always
        (and (typep indicator indicator-type)
             (typep value     value-type)))))

;;; -------------------------------------------------------

(define-predicated-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a list comprehending an equinumerant
   tally of elements as the ELEMENT-TYPES, with each member e[i] at the
   list position i conforming to the type t[i] from the EXPECTED-TYPES."
  (and
    (listp candidate)
    (every
      #'(lambda (element expected-type)
          (declare (type T element))
          (declare (type T expected-type))
          (typep element expected-type))
      (the list candidate)
      (the list element-types))))

;;; -------------------------------------------------------

(deftype slot-specification ()
  "The ``slot-specification'' type defines a slot description utible for
   use in the ``define-command'' macro as a tuple of either one, two, or
   three elements, the first item of which must be symbol, whereas the
   remaining members may assume any type."
  '(or (tuple-of symbol T T)
       (tuple-of symbol T)
       (tuple-of symbol)))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype register-index ()
  "The ``register-index'' type defines the valid indices of the MM1char
   program memory's registers."
  '(integer 1 2))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable MM1char program as a
   vector of zero or more ``Command'' objects."
  '(vector Command *))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype label-table ()
  "The ``label-table'' type defines a mapping of label names to the
   position in the program of their declaration, realized as a hash
   table that affiliates string identifiers with fixnum locations."
  '(hash-table-of string fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the register twain commorant in any
   MM1char program as a one-dimensional simple array of two integers."
  '(simple-array integer (2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command class generator.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-defclass-slot (class-name slot-specification)
  "Assembles a ``defclass''-compatible slot specification, in reference
   to the CLASS-NAME, and founded upon the accommodated
   SLOT-SPECIFICATION format.
   ---
   Given the CLASS-NAME and the SLOT-SPECIFICATION, which might assume
   one of the three formats
     (slot-name type init-value)
     (slot-name type)
     (slot-name)
   where the
     ``slot-name''  determines the slot's identifier
     ``type''       specifies the type of the slot value
     ``init-value'' refers to the slot's default state
   the following treble of possibilities exists for a slot specification
   (sinistral column) to produce a ``defclass'' slot equivalency
   (dextral column):
     ------------------------------------------------------------------
     Input slot specification    | Resulting defclass slot specific.
     ----------------------------+-------------------------------------
     (slot-name type init-value) | ({slot-name}
                                 |  :initarg  :{slot-name}
                                 |  :initform {init-value}
                                 |  :accessor {class-name}-{slot-name}
                                 |  :type     {type})
     ..................................................................
     (slot-name type)            | ({slot-name}
                                 |  :initarg  :{slot-name}
                                 |  :accessor {class-name}-{slot-name}
                                 |  :type     {type})
     ..................................................................
     (slot-name)                 | ({slot-name}
                                 |  :initarg  :{slot-name}
                                 |  :accessor {class-name}-{slot-name})
     ------------------------------------------------------------------"
  (declare (type symbol             class-name))
  (declare (type slot-specification slot-specification))
  (let ((slot-options NIL))
    (declare (type (list-of T) slot-options))
    
    (flet ((add-slot-option (option-name option-value)
            "Appends to the SLOT-OPTIONS the two items OPTION-NAME and
             OPTION-VALUE, which in conjunction represent a
             ``defclass''-conformant slot option, and returns no value."
            (declare (type keyword option-name))
            (declare (type T       option-value))
            (setf slot-options
              (nconc slot-options
                (list option-name option-value)))
            (values)))
      
      (let ((slot-name (first slot-specification)))
        (declare (type symbol slot-name))
        
        (push slot-name slot-options)
        
        (add-slot-option :initarg
          (intern
            (string-upcase
              (symbol-name slot-name))
            :keyword))
        
        (add-slot-option :accessor
          (intern
            (format NIL "~a-~a"
              (string-upcase class-name)
              (string-upcase slot-name))))
        
        (when (>= (length slot-specification) 2)
          (add-slot-option :type
            (pop slot-specification)))
        
        (when (>= (length slot-specification) 3)
          (add-slot-option :initform
            (pop slot-specification)))))
    
    (the (list-of T) slot-options)))

;;; -------------------------------------------------------

(defun build-defclass-slots (class-name slot-specifications)
  "Generates and returns for the SLOT-SPECIFICATIONS in the CLASS-NAME's
   context a list of ``defclass''-compatible slot specifications."
  (declare (type symbol                       class-name))
  (declare (type (list-of slot-specification) slot-specifications))
  (the (list-of T)
    (loop
      for slot-specification
        of-type slot-specification
        in      slot-specifications
      collect
        (build-defclass-slot class-name slot-specification))))

;;; -------------------------------------------------------

(defmacro define-command (name doc-string &rest slot-specifications)
  "Defines a new class utilizing the ``defclass'' infrastructure,
   designated by the NAME, and inheriting solely from the ``Command''
   interface, a documentation string of which must be supplied in the
   DOC-STRING, whereas the slots generated from the SLOT-SPECIFICATIONS,
   ultimately returning the thus defined class object.
   ---
   The SLOT-SPECIFICATION must be a list of one, two, or three items,
   the componence of the same embraces the definitions
     (slot-name slot-type slot-initial-value)"
  `(defclass ,name (Command)
     ,(build-defclass-slots name slot-specifications)
     (:documentation ,doc-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command classes.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command ()
  ()
  (:documentation
    "The ``Command'' interface applies itself to a common foundation's
     provision for all classes pursuing an MM1char's representation."))

;;; -------------------------------------------------------

(define-command Increment-Register-Command
  "The ``Increment-Register-Command'' class serves in the modeling of a
   register incrementation instruction."
  (register register-index (error "Missing register index.")))

;;; -------------------------------------------------------

(define-command Decrement-Register-Command
  "The ``Decrement-Register-Command'' class serves in the modeling of a
   register decrementation instruction."
  (register register-index (error "Missing register index.")))

;;; -------------------------------------------------------

(define-command Declare-Label-Command
  "The ``Declare-Label-Command'' class serves in the modeling of a label
   declaration instruction."
  (name string (error "Missing label name.")))

;;; -------------------------------------------------------

(define-command Goto-Label-Command
  "The ``Goto-Label-Command'' class serves in the modeling of a label
   redirection instruction."
  (name string (error "Missing label name.")))

;;; -------------------------------------------------------

(define-command Skip-Command
  "The ``Skip-Command'' class serves in the modeling of a conditional
   command skipping instruction based upon a selected register."
  (register register-index (error "Missing register index.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun label-name-character-p (candidate)
  "Determines the CANDIDATE's eligibility as a label name constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (find candidate "[]()" :test #'char=))))

;;; -------------------------------------------------------

(defun expect-character (source position expected-character)
  "Determines whether the character at the POSITION in the SOURCE exists
   and equals the EXPECTED-CHARACTER, returning on confirmation the
   position into the SOURCE immediately succeeding it; otherwise an
   error of an unspecified type is signaled."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the fixnum
    (cond
      ((not (array-in-bounds-p source position))
        (error "Expected the character ~s at position ~d, ~
                but encountered end of file instead."
          expected-character position))
      ((char/= (char source position) expected-character)
        (error "Expected the character ~s at position ~d, ~
                but encountered ~s instead."
          expected-character position
          (char source position)))
      (T
        (1+ position)))))

;;; -------------------------------------------------------

(defun read-label-name (source start)
  "Proceeding from the START position into the SOURCE, reads a label
   name and returns two values:
     (1) The consumed label name as a string.
     (2) The position in the SOURCE immediately succeeding the desinent
         label name character."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (label-name (make-string-output-stream))
      (declare (type string-stream label-name))
      (loop
        for position
          of-type fixnum
          from    start
          below   (length source)
        for token
          of-type character
          =       (char source position)
        while
          (label-name-character-p token)
        do
          (write-char token label-name)
        finally
          (return
            (values
              (get-output-stream-string label-name)
              position))))))

;;; -------------------------------------------------------

(defun read-label-declaration (source start)
  "Proceeding from the START position into the SOURCE, reads a label
   declaration command and returns two values:
     (1) A ``Declare-Label-Command'' object representing the consumed
         command.
     (2) The position into the SOURCE immediately succeeding the
         consumed label declaration command."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Declare-Label-Command fixnum)
    (multiple-value-bind (label-name end)
        (read-label-name source
          (expect-character source start #\[))
      (declare (type string label-name))
      (declare (type fixnum end))
      (values
        (make-instance 'Declare-Label-Command :name label-name)
        (expect-character source end #\])))))

;;; -------------------------------------------------------

(defun read-label-goto (source start)
  "Proceeding from the START position into the SOURCE, reads a goto
   label command and returns two values:
     (1) A ``Goto-Label-Command'' object representing the consumed
         command.
     (2) The position into the SOURCE immediately succeeding the
         consumed goto label command."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Goto-Label-Command fixnum)
    (multiple-value-bind (label-name end)
        (read-label-name source
          (expect-character source start #\())
      (declare (type string label-name))
      (declare (type fixnum end))
      (values
        (make-instance 'Goto-Label-Command :name label-name)
        (expect-character source end #\))))))

;;; -------------------------------------------------------

(defun read-simple-command (position class &rest initargs)
  "Creates an instance of the CLASS by applying the same, in conjunction
   with the INITARGS, to the ``make-instance'' function, and returns two
   values:
     (1) The instance of the CLASS created.
     (2) The location immediately following the POSITION, that is, the
         sum of POSITION + 1."
  (declare (type fixnum position))
  (declare (type symbol class))
  (declare (type list   initargs))
  (the (values Command fixnum)
    (values
      (apply #'make-instance class initargs)
      (1+ position))))

;;; -------------------------------------------------------

(defun read-command (source start)
  "Proceeding from the START position into the SOURCE, reads an MM1char
   command and returns two values:
     (1) A ``Command'' object representing the consumed command.
     (2) The position into the SOURCE immediately succeeding the
         consumed command."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Command fixnum)
    (case (char source start)
      (#\+
        (read-simple-command start
          'Increment-Register-Command :register 1))
      (#\-
        (read-simple-command start
          'Decrement-Register-Command :register 1))
      (#\*
        (read-simple-command start
          'Increment-Register-Command :register 2))
      (#\/
        (read-simple-command start
          'Decrement-Register-Command :register 2))
      (#\[
        (read-label-declaration source start))
      (#\(
        (read-label-goto source start))
      (#\~
        (read-simple-command start 'Skip-Command :register 1))
      (#\`
        (read-simple-command start 'Skip-Command :register 2))
      (otherwise
        (error "The character ~s at position ~d does not introduce ~
                a command."
          (char source start) start)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns from the piece of MM1char source CODE a
   one-dimensional simple array of its commands."
  (declare (type string code))
  (the program
    (coerce
      (loop
        with position of-type fixnum = 0
        while (< position (length code))
        if (whitespace-character-p (char code position)) do
          (incf position)
        else collect
          (multiple-value-bind (command new-position)
              (read-command code position)
            (declare (type Command command))
            (declare (type fixnum  new-position))
            (prog1 command
              (setf position new-position))))
      '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-label-table ()
  "Creates and returns an empty label table."
  (the label-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun label-table-define (label-table name position)
  "Associates the label NAME with its POSITION in a program and
   registers the same at the LABEL-TABLE, returning no value.
   ---
   Any extant entry of prior correspondence with the NAME is tacitly
   supplanted."
  (declare (type label-table label-table))
  (declare (type string      name))
  (declare (type fixnum      position))
  (setf (gethash name label-table) position)
  (values))

;;; -------------------------------------------------------

(defun label-table-get-position (label-table name)
  "Returns the position in a program associated with the label NAME in
   the LABEL-TABLE, or signals an error of an unspecified type if no
   such correspondence exists."
  (declare (type label-table label-table))
  (declare (type string      name))
  (the fixnum
    (or (gethash name label-table)
        (error "No label table entry found for the name ~s." name))))

;;; -------------------------------------------------------

(defun compute-label-table (program)
  "Calculates and returns for the PROGRAM a label table that contains
   the declared label names and their positions in the same."
  (declare (type program program))
  (let ((labels (make-label-table)))
    (declare (type label-table labels))
    (loop
      for command  of-type Command across program
      and position of-type fixnum  from   0 by 1
      when (typep command 'Declare-Label-Command) do
        (label-table-define labels
          (declare-label-command-name command)
          position))
    (the label-table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns a new program memory instance, composed of two
   integer-valued registers."
  (the memory
    (make-array 2
      :element-type    'integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun memory-register (memory index)
  "Returns the value stored in the MEMORY register amenable to the
   INDEX."
  (declare (type memory         memory))
  (declare (type register-index index))
  (the integer
    (aref memory
      (1- index))))

;;; -------------------------------------------------------

(defun (setf memory-register) (new-value memory index)
  "Stores the NEW-VALUE In the MEMORY register addressed by the INDEX
   and returns no value."
  (declare (type integer        new-value))
  (declare (type memory         memory))
  (declare (type register-index index))
  (setf (aref memory (1- index)) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :accessor      interpreter-program
    :type          program
    :documentation "The MM1char commands to execute.")
   (ip
    :initform      0
    :accessor      interpreter-ip
    :type          fixnum
    :documentation "The instruction pointer (IP), responsible for
                    referencing the current instruction in the
                    PROGRAM.")
   (labels
    :initform      (make-label-table)
    :accessor      interpreter-labels
    :type          label-table
    :documentation "Maintains the label definitions.")
   (memory
    :initform      (make-memory)
    :accessor      interpreter-memory
    :type          memory
    :documentation "Maintains the two program registers."))
  (:documentation
    "The ``Interpreter'' class' wike encumbers it with the necessity to
     accompass a sequence of MM1char commands with actual effect."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Computes the label table for the INTERPRETER's program, stores it in
   the same, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (setf (interpreter-labels interpreter)
    (compute-label-table
      (interpreter-program interpreter)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-advance (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program and returns no value."
  (declare (type Interpreter interpreter))
  (incf (slot-value interpreter 'ip))
  (values))

;;; -------------------------------------------------------

(defun interpreter-goto (interpreter new-position)
  "Moves the INTERPRETER's instruction pointer (IP) to the NEW-POSITION
   and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-position))
  (setf (slot-value interpreter 'ip) new-position)
  (values))

;;; -------------------------------------------------------

(defun interpreter-get-current-command (interpreter)
  "Returns command designated by the INTERPRETER's instruction pointer
   (IP)."
  (declare (type Interpreter interpreter))
  (the Command
    (aref (slot-value interpreter 'program)
      (slot-value interpreter 'ip))))

;;; -------------------------------------------------------

(defun interpreter-finished-p (interpreter)
  "Determines whether the INTERPRETER's operation has been terminated
   as a causatum of its program's exhaustion, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length (interpreter-program interpreter)))))))

;;; -------------------------------------------------------

(defun interpreter-print-registers (interpreter)
  "Prints the state of the INTERPRETER's registers to the standard
   output and returns no value."
  (declare (type Interpreter interpreter))
  (format T "~&register_1 = ~d, register_2 = ~d"
    (memory-register (interpreter-memory interpreter) 1)
    (memory-register (interpreter-memory interpreter) 2))
  (values))

;;; -------------------------------------------------------

(defgeneric interpreter-process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmethod interpreter-process-command
    ((interpreter Interpreter)
     (command     Increment-Register-Command))
  (declare (type Interpreter                interpreter))
  (declare (type Increment-Register-Command command))
  (incf
    (memory-register
      (interpreter-memory interpreter)
      (increment-register-command-register command)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-command
    ((interpreter Interpreter)
     (command     Decrement-Register-Command))
  (declare (type Interpreter                interpreter))
  (declare (type Decrement-Register-Command command))
  (decf
    (memory-register
      (interpreter-memory interpreter)
      (decrement-register-command-register command)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-command
    ((interpreter Interpreter)
     (command     Declare-Label-Command))
  (declare (type Interpreter           interpreter))
  (declare (ignore                     interpreter))
  (declare (type Declare-Label-Command command))
  (declare (ignore                     command))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-command
    ((interpreter Interpreter)
     (command     Goto-Label-Command))
  (declare (type Interpreter        interpreter))
  (declare (type Goto-Label-Command command))
  (setf (interpreter-ip interpreter)
    (label-table-get-position
      (interpreter-labels      interpreter)
      (goto-label-command-name command)))
  (values))

;;; -------------------------------------------------------

(defmethod interpreter-process-command
    ((interpreter Interpreter)
     (command     Skip-Command))
  (declare (type Interpreter  interpreter))
  (declare (type Skip-Command command))
  (when (zerop
          (memory-register
            (interpreter-memory interpreter)
            (skip-command-register command)))
    (interpreter-advance interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Executes the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (interpreter-finished-p interpreter) do
    (interpreter-process-command interpreter
      (interpreter-get-current-command interpreter))
    (interpreter-advance interpreter)
    
    (interpreter-print-registers interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-MM1char (code)
  "Interprets the piece of MM1char source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-instance 'Interpreter :program
      (extract-commands code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set register one to the value 5, and subseqently count down to 0.
(interpret-MM1char
  "
  +++++
  [decrement_register]
  -
  ~
  (decrement_register)
  ")

;;; -------------------------------------------------------

;; Set register one to the value 5, and move its state to the register
;; two, concomitantly reducing the former to 0.
(interpret-MM1char
  "
  +++++
  [transfer_register_values]
  -
  *
  ~
  (transfer_register_values)
  ")
