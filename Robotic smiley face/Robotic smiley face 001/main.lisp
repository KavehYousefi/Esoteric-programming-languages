;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language ":]", also extended into the more elucidating agnomination
;; "Robotic smiley face", invented by the Esolang user "Cinnamony" and
;; presented on June 14th, 2023, the code of which bears the dioristic
;; attribute of a nearly perfect representation in variants of smileys.
;; 
;; 
;; Concept
;; =======
;; The :] programming language, also nevened, for purposes which do not
;; homologate its symbols' induction, "Robotic smiley face", adheres in
;; its design to the almost exclusive employment of smileys for the
;; communication of instructions.
;; 
;; == SMILEYS TALK TO THE PROGRAM ==
;; All instructions are introduced by smileys and may accept the same
;; in order to prosecute the desiderated effects.
;; 
;; == PROGRAMS OPERATE LINEWISE ==
;; The syntaxis' susceptibility to ambiguous eisegesis, whose etiology
;; wones in the sequences of smileys and the lack of a palpable sepiment
;; mechanism, conditions the necessity to segregate each two commands
;; by at least one linebreak.
;; 
;; == AN AEFAULD VARIABLE AS A STORAGE ENTITY ==
;; The complete memory is exhausted by a single variable whose
;; competence extends to both data types admitted to the language:
;; signed integers of unbridled size and strings of any length.
;; 
;; 
;; Architecture
;; ============
;; An aefauld salvatory's dation of competence exhausts the :] program
;; memory, the content of which embraces a scalar value, either a signed
;; integer of any magnitude and size or a string.
;; 
;; 
;; Data Types
;; ==========
;; A dichotomy governs :] type system, co-locating signed integers with
;; strings.
;; 
;; == INTEGERS ==
;; Integer numbers are neither impounded by their magnitude nor their
;; sign, their expression conforming the a sequence of one or more
;; decimal digits.
;; 
;; == STRINGS ==
;; Strings are ensconced in a jumelle of opening and closing double
;; quotation marks, that is, "“" and "”", the tally of entities
;; contained being a thing of one's own deliberation. Strings may be
;; nested, such that
;; 
;;   “The bunny says: “Hello,” and leaves.”
;; 
;; produces
;; 
;;   The bunny says: “Hello,” and leaves.
;; 
;; 
;; Syntax
;; ======
;; A :] program is composed of zero or more lines, each comprehend at
;; most one command, expressed as a series of smileys for operations and
;; either the same species for the operands, or, depending upon the
;; instruction's diorism, an integer or string literal.
;; 
;; == PROGRAMS: LINES OF COMMANDS ==
;; Each non-blank line accommodates an aefauld command's woning,
;; segregated from its near-dweller by at least one newline character.
;; 
;; == COMMANDS ==
;; All command names, in conjunction with the variable identifier, find
;; their expression in the mold of smileys.
;; 
;; == INTEGERS ==
;; Integers are introduced with an optional mathematical sign, that is,
;; either plus ("+") or minus ("-"), and succeeded by one or more
;; decimal digits.
;; 
;; == STRINGS ==
;; The diorism of strings encompasses character sequences compact of
;; zero or more elements ensconced in a jumelle of opening ("“") and
;; closing ("”") double quotation marks. The two distinct components'
;; differentiating nature permits a nesting of quoted sections to any
;; level.
;; 
;; An exemption from the stringency in linebreaks as programmatic
;; conductors, strings may cross several lines in their prosecution of
;; newline characters' replication.
;; 
;; == WHITESPACES ==
;; While horizontal spacing characters, a diorism that amplects both the
;; actual space and the horizontal tab, may be apportioned liberally,
;; newline entities must be present as an adminiculum for each
;; subsequent command lines' segregation. A single command may not
;; spread across more than one line.
;; 
;; == COMMENTS ==
;; Comments are admissive as a dedicated species of command, introduced
;; by the ">:0" identifier and bounded to the dextral laterality by the
;; end of the appropriated line. In a manner sui generis, comments may
;; share a horizontal spatiality with another instruction, given that
;; they reside to the effective peer's succession.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (ENBF) formulation shall
;; occupy the wike of an elucidating agent for the syntax:
;; 
;;   program            := { innerLine } , [ terminalLine ] ; 
;;   
;;   innerLine          := [ statement ] , newlines ;
;;   terminalLine       := [ statement ] , [ newlines ] ;
;;   
;;   statementList      := { innerLine } ;
;;   statement          := command , optionalComment ;
;;   command            := input
;;                      |  output
;;                      |  setVariable
;;                      |  setVariableToBlank
;;                      |  setVariableToZero
;;                      |  increaseVariable
;;                      |  decreaseVariable
;;                      |  loop
;;                      |  endScript
;;                      ;
;;   expression         := number
;;                      |  string
;;                      |  variable
;;                      |  input
;;                      ;
;;   
;;   optionalComment    := ">:0" , { character - newline } ;
;;   output             := ":0" , expression ;
;;   input              := ":|" ;
;;   setVariable        := variable , expression ;
;;   setVariableToBlank := variable ;
;;   setVariableToZero  := "):" ;
;;   increaseVariable   := ":D" ;
;;   decreaseVariable   := "D:" ;
;;   variable           := ":)" ;
;;   loop               := ( loopUntil | loopTimes) , newlines
;;                      ,  [ statementList , newlines ]
;;                      ,  loopEnd
;;                      ;
;;   loopUntil          := startLoop
;;                      ,  variable
;;                      ,  relation
;;                      ,  expression
;;                      ;
;;   loopTimes          := startLoop , ( number | variable ) ;
;;   startLoop          := ":]]" ;
;;   endLoop            := ":[[" ;
;;   relation           := "=="
;;                      |  "!="
;;                      |  "<"
;;                      |  "<="
;;                      |  ">"
;;                      |  ">="
;;                      ;
;;   endScript          := ":(" ;
;;   
;;   newlines           := newline , { newline } ;
;;   newline            := "\n" ;
;;   
;;   number             := [ "+" | "-" ] , digit , { digit } ;
;;   digit              := "0" | "1" | "2" | "3" | "4"
;;                      |  "5" | "6" | "7" | "8" | "9"
;;                      ;
;; 
;; 
;; Instructions
;; ============
;; A very generous repertoire of instructions redes its availability to
;; the programmer, the amplectation of the same enumerates basic
;; arithmetics, an aefauld variable for storing an integer or string,
;; numeric and character input and output, as well as control flow
;; mechanisms that comprehend several iteration and an unconditional
;; termination facility.
;; 
;; == OVERVIEW ==
;; An apercu shall serve in a cursory gnarity's administration
;; concerning the language's available operations.
;; 
;; Please note that placeholder sections are underlined via asterisks
;; ("*"), intended to be replaced by valid :] code.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Effect
;;   ------------+-----------------------------------------------------
;;   :0 number   | Prints the {number} to the standard output.
;;      ******   | 
;;   ..................................................................
;;   :0 string   | Prints the {string} to the standard output.
;;      ******   | 
;;   ..................................................................
;;   :0 :)       | Prints the variable's value to the standard output.
;;   ..................................................................
;;   :0 :|       | Queries the user for an input and prints it to the
;;               | standard output.
;;   ..................................................................
;;   :) number   | Stores the {number} in the variable.
;;      ******   | 
;;   ..................................................................
;;   :) string   | Stores the {string} in the variable.
;;      ******   | 
;;   ..................................................................
;;   :) :|       | Queries the user for an input and stores it in the
;;               | variable.
;;   ..................................................................
;;   :) :)       | Sets the variable value to itself.
;;   ..................................................................
;;   :)          | Sets the variable value to blank.
;;   ..................................................................
;;   ):          | Sets the variable value to zero (0).
;;   ..................................................................
;;   :D          | Increments the variable value by one (1).
;;   ..................................................................
;;   D:          | Decrements the variable value by one (1).
;;   ..................................................................
;;   :]] number  | Repeats the commands embraced betwixt this ":]]" and
;;       ******  | the matching ":[[" the {number} tally of times.
;;   ..................................................................
;;   :]] :)      | Repeats the commands embraced betwixt this ":]]" and
;;               | the matching ":[[" the number of times specified by
;;               | the variable.
;;   ..................................................................
;;   :]] :|      | Repeats the commands embraced betwixt this ":]]" and
;;               | the matching ":[[" a tally of times equal to the
;;               | number provided by the standard input.
;;               | The user input must resolve to a signed integer
;;               | value.
;;   ..................................................................
;;   :]] :) == i | Repeats the commands embraced betwixt this ":]]" and
;;             * | the matching ":[[" until the variable value equals
;;               | {i}.
;;               | {i} may be either of:
;;               |   - a string
;;               |   - a number
;;               |   - the variable ":)"
;;               |   - the input command ":|".
;;               | Please note that the test value {i} is evaluated
;;               | anew during each iteration's prelude; thus an input
;;               | expression, ":|", will issue a query upon every
;;               | cycle's inchoation.
;;   ..................................................................
;;   :]] :) < i  | Repeats the commands embraced betwixt this ":]]" and
;;            *  | the matching ":[[" until the variable value is less
;;               | than {i}.
;;               | {i} may be either of:
;;               |   - a string
;;               |   - a number
;;               |   - the variable ":)"
;;               |   - the input command ":|".
;;               | Please note that the test value {i} is evaluated
;;               | anew during each iteration's prelude; thus an input
;;               | expression, ":|", will issue a query upon every
;;               | cycle's inchoation.
;;   ..................................................................
;;   :]] :) <= i | Repeats the commands embraced betwixt this ":]]" and
;;             * | the matching ":[[" until the variable value is less
;;               | than or equal to {i}.
;;               | {i} may be either of:
;;               |   - a string
;;               |   - a number
;;               |   - the variable ":)"
;;               |   - the input command ":|".
;;               | Please note that the test value {i} is evaluated
;;               | anew during each iteration's prelude; thus an input
;;               | expression, ":|", will issue a query upon every
;;               | cycle's inchoation.
;;   ..................................................................
;;   :]] :) > i  | Repeats the commands embraced betwixt this ":]]" and
;;            *  | the matching ":[[" until the variable value is
;;               | greater than {i}.
;;               | {i} may be either of:
;;               |   - a string
;;               |   - a number
;;               |   - the variable ":)"
;;               |   - the input command ":|".
;;               | Please note that the test value {i} is evaluated
;;               | anew during each iteration's prelude; thus an input
;;               | expression, ":|", will issue a query upon every
;;               | cycle's inchoation.
;;   ..................................................................
;;   :]] :) >= i | Repeats the commands embraced betwixt this ":]]" and
;;             * | the matching ":[[" until the variable value is
;;               | greater than or equal to {i}.
;;               | {i} may be either of:
;;               |   - a string
;;               |   - a number
;;               |   - the variable ":)"
;;               |   - the input command ":|".
;;               | Please note that the test value {i} is evaluated
;;               | anew during each iteration's prelude; thus an input
;;               | expression, ":|", will issue a query upon every
;;               | cycle's inchoation.
;;   ..................................................................
;;   :[[         | Demarcates a loop section's end.
;;   ..................................................................
;;   :(          | Immediately terminates the program.
;;   ..................................................................
;;   >:0 text    | Defines a comment comprehending the {text}, and
;;       ****    | which extends until the end of the respective line.
;;               | This command may also succeed a command on the same
;;               | line.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its extensive mete, the :] protolog is inflicted with a few
;; uncertain attributes, a subset of which shall be subjected to an
;; instructive docimasy.
;; 
;; == ARE LINEBREAKS MANDATORY? ==
;; The specification's lacuna regarding the syntactical differentiation
;; of accolent commands encumbers the language with an instance of
;; ambivalence, forecause certain combinations of tokens are capacitated
;; to yield discrepant results when empighted in succession.
;; 
;; For instance, the command sequence
;; 
;;   :) :) :)
;; 
;; operating on variables, may be a vehicle to at a treble of
;; interpretations, two among the same --- (a) and (b) --- conclude with
;; equiparation, whereas the third --- (c) --- veers in its causatum:
;; 
;;   (a) Blanking, then setting to identity:
;;       :)
;;       :) :)
;;   
;;   (b) Setting to identity, then blanking:
;;       :) :)
;;       :)
;;   
;;   (c) Setting to identity thrice:
;;       :)
;;       :)
;;       :)
;; 
;; An implication of its potential for confounding command sequences, it
;; has been adjudged to mandate the segregation of commands into lines.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation's incarnation manifests in the programming
;; language Common Lisp, naiting a very popular collaboration of the
;; three components lexer, parser, and interpreter along well-defined
;; interfaces.
;; 
;; == A TREBLE STAGES' COEFFICIENCY ==
;; The interpretation's entirety amplects a triad's componency of
;; lexer, parser, and actual interpreter:
;; 
;;   (1) The lexical analyzer, also nevened lexer or scanner, assumes
;;       the wike of a token generator by recognizing significant
;;       objects in a piece of :] source code supplied as a string,
;;       their extraction, and delivery.
;;   (2) The parser commits itself to the onus of assembling from the
;;       lexer's token stream a hierarchical representation of the :]
;;       program's string form, compact of nodes and subtrees, and known
;;       as the abstract syntax tree (AST).
;;   (3) The interpreter accepts the parser's AST in order to prosecute
;;       its nodes' evaluation, thus accompassing actual causata to the
;;       static objects.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-09
;; 
;; Sources:
;;   [esolang2023RoboticSmileyFace]
;;   The Esolang contributors, "Robotic Smiley Face", June 24th, 2023
;;   URL: "https://esolangs.org/wiki/Robotic_smiley_face"
;;   
;;   [lambdatheultimate2006accsyntax]
;;   Lambda the Ultimate, "Accidental Syntax | Lambda the Ultimate",
;;     2006
;;   URL: "http://lambda-the-ultimate.org/node/1309"
;;   Notes:
;;     - Mentions "accidental syntax" in Perl.
;;     - Discusses nested quasiquotation.
;;     - Demonstrates an example of a triple nesting:
;;         (defmacro define-namespace (prefix-to-name operator)
;;           `(defmacro ,(intern (format NIL "~a-NAMES" prefix-to-name)
;;                               (symbol-package prefix-to-name))
;;                (names &body code)
;;              
;;              (flet ((make-macro-def (name)
;;                       (declare (type symbol name))
;;                       `(,name (&rest args)
;;                          `(,',',operator ,',name ,@args))))
;;                
;;                `(macrolet ,(mapcar #'make-macro-def names)
;;                   ,@code))))
;;   
;;   [reddit2018inscommaimt]
;;   reddit, "[SBCL] inserting comma in macro transformation", 2018
;;   URL: "https://www.reddit.com/r/learnlisp/comments/98gssu/
;;         sbcl_inserting_comma_in_macro_transformation/"
;;   
;;   [stackoverflow2011q4575993]
;;   The Stackoverflow contributors,
;;     "how can i reimplement backquote in common lisp?", 2011
;;   URL: "https://stackoverflow.com/questions/4575993/
;;         how-can-i-reimplement-backquote-in-common-lisp"
;;   
;;   [stackoverflow2013q17429521]
;;   The Stackoverflow contributors,
;;     "Common Lisp Double-Backquote, Unquote, Quote, Unquote sequence?",
;;     2013
;;   URL: "https://stackoverflow.com/questions/17429521/
;;         common-lisp-double-backquote-unquote-quote-unquote-sequence"
;;   Notes:
;;     - Exposes the following equivalency listing for a global variable
;;       "a" of the value 42:
;;         ------------------------
;;         Quoted form | Equivalent
;;         ------------+-----------
;;         ``(,,a)     | '(list 42)
;;         ........................
;;         ``(,a)      | '(list a)
;;         ........................
;;         ``(,',a)    | ''(42)
;;         ........................
;;         ``(a)       | ''(a)
;;         ------------------------
;;   
;;   [stackoverflow2019q55972371]
;;   The Stackoverflow contributors,
;;     "Macro : How to output a comma in a backquoted generated code?",
;;     2019
;;   URL: "https://stackoverflow.com/questions/55972371/
;;         macro-how-to-output-a-comma-in-a-backquoted-generated-code"
;;   
;;   [stackoverflow2020q60378335]
;;   The Stackoverflow contributors, "', (quote-comma) in common lisp",
;;     2020
;;   URL: "https://stackoverflow.com/questions/60378335/
;;         quote-comma-in-common-lisp"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest deftype-parameters)
     &body body)
  "Defines a new type naiting the ``deftype'' facility, nevened by the
   TYPE-NAME and endowed with the DEFTYPE-PARAMETERS as its parameter
   list, the tested object of which is designated by the
   CANDIDATE-VARIABLE, utible for the BODY forms, which constitute an
   anonymous function intended for the communication in a ``satisfies''
   type specifier."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@deftype-parameters)
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

(define-predicated-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a tuple composed of the same tally of
   elements as the ELEMENT-TYPE, with each element e[i] conforming to
   the type t[i], for i in [0, |ELEMENT-TYPES| - 1]."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length (the list element-types)))
    (every
      #'(lambda (element expected-type)
          (declare (type T element))
          (declare (type T expected-type))
          (typep element expected-type))
      (the list candidate)
      (the list element-types))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   which conform to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype slot-specifier ()
  "The ``slot-specifier'' type defines a slot specification compatible
   with the ``define-quick-class'' macro and its adminicular operations,
   either represented by a tuple of three elements, these being
     (slot-name, slot-type, initial-value)
   where the ``slot-name'' assumes a symbol object, whereas the
   remaining twain of components may be contributed in any form, or by
   a string as a special case of a documentation string, permissive in
   many cases for descriptive purposes."
  '(or (tuple-of symbol T T)
       string))

;;; -------------------------------------------------------

(deftype slot-specifications ()
  "The ``slot-specifications'' type defines a list of zero or more
   ``slot-specifier''s, conformant with the macro ``define-quick-class''
   and its adminicular operations."
  '(list-of slot-specifier))

;;; -------------------------------------------------------

(define-predicated-type association-list-of
    (candidate
     &optional (indicator-type T)
               (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each indicator, or key, of
   which conforms to the INDICATOR-TYPE and associates with a value of
   the VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element `(cons ,indicator-type ,value-type)))))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of identifier names
   to representative tokens, manifesting as an association list that
   associates the name strings to ``Token'' instances."
  '(association-list-of string Token))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
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

(deftype attribute-map ()
  "The ``attribute-map'' type defines an associative collection of node
   attributes, realized as a hash table that maps attribute names as
   keywords to arbitrarily typed values."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(define-predicated-type property-list-of
    (candidate
     &optional (indicator-type T) (value-type T))
  "The ``property-list-of'' type defines a property list, or plist,
   compact of zero or more entries, the indicators, or keys, of which
   conform to the INDICATOR-TYPE and associate with a value of the
   VALUE-TYPE, both defaulting to the comprehensive ``T''."
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

(deftype attribute-list ()
  "The ``attribute-list'' type defines a property list nait for the
   conveyance of node attributes, representing with its indicators, or
   keys, the attribute names, associated with the attribute values."
  '(property-list-of keyword T))

;;; -------------------------------------------------------

(deftype relation ()
  "The ``relation'' type enumerates the recognized relational
   operators."
  '(member
    :equal-to
    :not-equal-to
    :less-than
    :less-than-or-equal-to
    :greater-than
    :greater-than-or-equal-to))

;;; -------------------------------------------------------

(deftype rsf-object ()
  "The ``rsf-object'' type defines the recognized types of objects in
   currency during a :] program's execution, comprehending both signed
   integers and strings of arbitrary extent."
  '(or integer string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class generator macro.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-defstruct (class-name constructor slots)
  "Generates the Common Lisp code capable of defining a structure norned
   CLASS-NAME, affiliated with an accommodated constructor option
   bearing the CONSTRUCTOR signature, if not ``NIL'', and the list of
   SLOTS, each such a triple (slot-name, slot-type, initial-value)."
  (declare (type symbol              class-name))
  (declare (type list                constructor))
  (declare (type slot-specifications slots))
  (let ((class-signature
          (if constructor
            `(,class-name
               (:constructor
                 ,(intern (format NIL "MAKE-~:@(~a~)" class-name))
                 ,constructor))
            `(,class-name))))
    (declare (type list class-signature))
    `(defstruct (,@class-signature)
       ,(when (stringp (first slots))
          (pop slots))
       ,@(loop
           for (slot-name slot-type slot-initial-value)
             of-type (T T T)
             in      slots
           collect
             `(,slot-name ,slot-initial-value :type ,slot-type)))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-bindings (class-name
                                       evaluated-subject
                                       slots)
  "Generates and returns the Common Lisp code that establishes for each
   of the SLOTS of the EVALUATED-SUBJECT, being of the type specified by
   the CLASS-NAME, a local symbol macro binding that affiliates the
   slot name with the invoking accessor.
   ---
   Each element of the SLOTS list constitutes a three-item list
     (slot-name, slot-type, initial-value)"
  (declare (type symbol              class-name))
  (declare (type T                   evaluated-subject))
  (declare (type slot-specifications slots))
  (loop
    for slot of-type slot-specifier in slots
    unless (stringp slot)
      collect
        (destructuring-bind (slot-name slot-type &rest other-options)
            slot
          (declare (type symbol slot-name))
          (declare (type T      slot-type))
          (declare (ignore      other-options))
          `(,slot-name
            (the ,slot-type
              (,(intern
                  (format NIL "~:@(~a~)-~:@(~a~)" class-name slot-name))
               ,evaluated-subject))))))

;;; -------------------------------------------------------

(defun build-symbol-macrolet-declarations (slots)
  "Generates and returns the Common Lisp code that establishes for each
   of the SLOTS a twain of declarations, the incipient of which declares
   its type, the parhedral second its state as ``ignorable''.
   ---
   Each element of the SLOTS list constitutes a three-item list
     (slot-name, slot-type, initial-value)"
  (declare (type slot-specifications slots))
  (loop
    for slot of-type slot-specifier in slots
    unless (stringp slot)
      collect
        (destructuring-bind (slot-name slot-type &rest other-options)
            slot
          (declare (type symbol slot-name))
          (declare (type T      slot-type))
          (declare (ignore      other-options))
          `(declare (type ,slot-type ,slot-name)))
      and
        collect
          `(declare (ignorable ,(first slot)))))

;;; -------------------------------------------------------

(defmacro define-quick-class (class-name constructor-arguments
                              &rest slots)
  "Generates and evaluates the Common Lisp code intended for the
   convenient definition of a structure of the CLASS-NAME, its
   constructor's signature specified by the CONSTRUCTOR-ARGUMENTS, and
   invested with the SLOTS, the class establishment of which is
   succeeded by a dedicated ``with-CLASS-NAME'' accommodated to permit
   access to the class' slots by their names.
   ---
   The CLASS-NAME must be a symbol appropriate as a ``defstruct''
   designator.
   ---
   The CONSTRUCTOR-ARGUMENTS must be a valid argument list for the
   ``defstruct'' macro's ``:constructor'' option, which is supplied to
   the automatically produced ``make-CLASS-NAME'' constructor.
   ---
   Each element of the SLOTS list constitutes a three-item list
     (slot-name, slot-type, initial-value)
   ---
   A facility of additional convenience, the parergon established in the
   ``with-CLASS-NAME'' macro binds each of the SLOTS' slot names to an
   eponymous local symbol macro, recluded to access by its ensconced
   body forms."
  (let ((evaluated-subject (gensym))
        (with-macro-name   (intern
                             (format NIL "WITH-~:@(~a~)" class-name))))
    (declare (type symbol evaluated-subject))
    `(progn
       ,(build-defstruct class-name constructor-arguments slots)
       
       (defmacro ,with-macro-name ((subject) &body body)
         (let ((,evaluated-subject (gensym)))
           (declare (type symbol ,evaluated-subject))
           `(let ((,,evaluated-subject ,subject))
             (declare (type ,(quote ,class-name) ,,evaluated-subject))
             (declare (ignorable                 ,,evaluated-subject))
             (symbol-macrolet
                 (,@(build-symbol-macrolet-bindings
                       (quote ,class-name)
                       ,evaluated-subject
                       '(,@slots)))
               ,@(build-symbol-macrolet-declarations '(,@slots))
               ,@body)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-quick-class Token (type value)
  "The ``Token'' class implements a significant object extracted during
   the lexical analyzation stage from a piece of :] source code
   specified in string form."
  (type  keyword (error "Missing token type."))
  (value T       (error "Missing token type.")))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))

;;; -------------------------------------------------------

(defun expression-token-p (token)
  "Determines whether the TOKEN represents an expression, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token)
        '(:number :string :input :variable)
        :test #'eq)))))

;;; -------------------------------------------------------

(defun relation-token-p (token)
  "Determines whether the TOKEN represents a relational operator,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (member (token-type token)
        '(:equal-to
          :not-equal-to
          :less-than
          :less-than-or-equal-to
          :greater-than
          :greater-than-or-equal-to)
        :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIER-TABLE+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIER-TABLE+ NIL
  "Associates the recognized identifier strings with representative
   ``Token'' objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (identifier token-type)
        (declare (type string  identifier))
        (declare (type keyword token-type))
        "Associates the IDENTIFIER string with a new token of the
         TOKEN-TYPE and the IDENTIFIER as its value in the
         +IDENTIFIER-TABLE+ and returns no value."
        (push (cons identifier (make-token token-type identifier))
              +IDENTIFIER-TABLE+)
        (values)))
  (register-identifier ":0"  :output)
  (register-identifier ":|"  :input)
  (register-identifier ":)"  :variable)
  (register-identifier ":D"  :increase)
  (register-identifier "D:"  :decrease)
  (register-identifier "):"  :set-to-zero)
  (register-identifier ":]]" :start-loop)
  (register-identifier ":[[" :end-loop)
  (register-identifier ":("  :end-script)
  (register-identifier ">:0" :comment)
  (register-identifier "=="  :equal-to)
  (register-identifier "!="  :not-equal-to)
  (register-identifier "<="  :less-than-or-equal-to)
  (register-identifier "<"   :less-than)
  (register-identifier ">="  :greater-than-or-equal-to)
  (register-identifier ">"   :greater-than)
  (setf +IDENTIFIER-TABLE+
    (nreverse +IDENTIFIER-TABLE+))
  (values))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token representing the IDENTIFIER, or ``NIL'' if no such
   correspondence could be detected."
  (declare (type string identifier))
  (the (or null Token)
    (cdr (assoc identifier +IDENTIFIER-TABLE+ :test #'string=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-character-predicate
    (function-name (character-variable &rest further-parameters)
     &body body)
  "Declares a ``defun'' function implementation accommodated specially
   for the encheson of a character's docimasy concerning some predicate,
   the thus produced function is identified by the FUNCTION-NAME,
   employing the probed character as a parameter name denoted by the
   CHARACTER-VARIABLE, with optional FURTHER-PARAMETERS admissive to the
   argument list, when executing the BODY forms with access to all such
   inputs, and returning a ``boolean'' value of ``T'' for a non-``NIL''
   desinent BODY form's result, otherwise responding with ``NIL''."
  `(defun ,function-name (,character-variable ,@further-parameters)
     ,(when (stringp (first body))
        (pop body))
     (declare (type character ,character-variable))
     (declare (ignorable      ,character-variable))
     (the boolean
      (not (null
        (progn
          ,@body))))))

;;; -------------------------------------------------------

(define-character-predicate space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space character, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (member candidate '(#\Space #\Tab) :test #'char=))

;;; -------------------------------------------------------

(define-character-predicate newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline character, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (char= candidate #\Newline))

;;; -------------------------------------------------------

(define-character-predicate sign-character-p (candidate)
  "Determines whether the CANDIDATE represents a mathematical sign
   character, that is, either \"+\" or \"-\", returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (find candidate "+-" :test #'char=))

;;; -------------------------------------------------------

(define-character-predicate identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent admissible
   to an identifier, which comprehends both smileys and relational
   operators, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (find candidate ":)D=<>" :test #'char=))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-quick-class Lexer
    (source
      &aux (position 0)
           (character
             (when (array-in-bounds-p source position)
               (char source position))))
  "The ``Lexer'' class provides a lexical analyzer, the telos of which
   comprehends the recognition and extraction of significant objects,
   the tokens, from a piece of processed :] source code specified in
   string form."
  (source    string              (error "Missing lexer source."))
  (position  fixnum              0)
  (character (or null character) NIL))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Returns the LEXER's current character, while concomitantly moving its
   position cursor to the next character in its SOURCE, if possible, and
   updating its state."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the (or null character)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source
              (incf position))))))))

;;; -------------------------------------------------------

(defun lexer-move-to (lexer new-position)
  "Relocates the LEXER's position cursor to the NEW-POSITION, updates
   its internal state, and returns no value."
  (declare (type Lexer  lexer))
  (declare (type fixnum new-position))
  (with-lexer (lexer)
    (setf position new-position)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (values))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   signed or unsigned integer number and returns its ``:number'' token
   representation."
  (declare (type Lexer lexer))
  (the Token
    (make-token :number
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (with-lexer (lexer)
            (when (and character (sign-character-p character))
              (write-char character digits)
              (lexer-advance lexer))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-digit-follows-p (lexer)
  "Determines whether the character succeeding the LEXER's position
   cursor represents a decimal digit, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Lexer lexer))
  (the boolean
    (with-lexer (lexer)
      (let ((next-character
              (when (array-in-bounds-p source (1+ position))
                (char source (1+ position)))))
        (declare (type (or null character) next-character))
        (the boolean
          (not (null
            (and next-character
                 (digit-char-p next-character)))))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Proceeding from the current position into the LEXER's source, reads a
   string literal and returns its ``:string'' token representation."
  (declare (type Lexer lexer))
  (the Token
    (make-token :string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (with-lexer (lexer)
          (lexer-advance lexer)
          (loop with level of-type integer = 0 do
            (case character
              ((NIL)
                (error "Unterminated string literal at postiion ~d."
                  position))
              (#\”
                (cond
                  ((zerop level)
                    (lexer-advance lexer)
                    (loop-finish))
                  ((plusp level)
                    (decf level)
                    (write-char character content)
                    (lexer-advance lexer))
                  (T
                    (error "Unmatched closing double quotation mark ~
                            \"“\" in string at position ~d."
                      position))))
              (#\“
                (incf level)
                (write-char character content)
                (lexer-advance lexer))
              (otherwise
                (write-char character content)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-sequence-follows-p (lexer expected-sequence)
  "Determines whether, proceeding from the current position into the
   LEXER's source, the consecutive characters replicate the
   EXPECTED-SEQUENCE, on confirmation relocating the LEXER's position
   cursor to the first position immediately succeeding the matching
   section and returning a ``boolean'' value of ``T'', otherwise
   retaining its state at the moment of this operation's invocation,
   while returning ``NIL''."
  (declare (type Lexer  lexer))
  (declare (type string expected-sequence))
  (the boolean
    (with-lexer (lexer)
      (let ((start-position position))
        (declare (type fixnum start-position))
        (loop
          for expected-character
            of-type character
            across  expected-sequence
          if (and character (char= character expected-character)) do
            (lexer-advance lexer)
          else do
            (lexer-move-to lexer start-position)
            (return NIL)
          finally
            (return T))))))

;;; -------------------------------------------------------

(defun lexer-probe-for-identifier (lexer)
  "Proceeding from the current position into the LEXER's source,
   attempts to detect an identifier token, upon success returning the
   recognized token, otherwise responding with ``NIL''."
  (declare (type Lexer lexer))
  (the (or null Token)
    (with-lexer (lexer)
      (loop
        for (identifier . identifier-token)
          of-type (string . Token)
          in      +IDENTIFIER-TABLE+
        when (lexer-sequence-follows-p lexer identifier) do
          (return identifier-token)
        finally
          (return NIL)))))

;;; -------------------------------------------------------

(defun lexer-skip-comment (lexer)
  "Proceeding from the current position into the LEXER's source, skips
   all content on the current line until either a newline character or
   the end of the source is reached, in the former case not consuming
   the linebreak entity, in any case returning no value."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop
      while (and character (not (newline-character-p character)))
      do    (lexer-advance lexer)))
  (values))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to every request
   with a fresh end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (the Token
    (with-lexer (lexer)
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((space-character-p character)
          (lexer-advance        lexer)
          (lexer-get-next-token lexer))
        
        ((newline-character-p character)
          (prog1
            (make-token :newline character)
            (lexer-advance lexer)))
        
        ((or (digit-char-p character)
             (and (sign-character-p character)
                  (lexer-digit-follows-p lexer)))
          (lexer-read-number lexer))
        
        ((char= character #\“)
          (lexer-read-string lexer))
        
        ((identifier-character-p character)
          (let ((token NIL))
            (declare (type (or null Token) token))
            (setf token
              (lexer-probe-for-identifier lexer))
            (cond
              ;; No identifier recognized?
              ;; => Return general character token.
              ((null token)
                (prog1
                  (make-token :character character)
                  (lexer-advance lexer)))
              ;; Comment detected?
              ;; => Skip the same in conjunction with the complete line.
              ;; => Search for next token.
              ((token-type-p token :comment)
                (lexer-skip-comment   lexer)
                (lexer-get-next-token lexer))
              ;; Non-comment identifier recognized?
              ;; => Return the same.
              (T
                token))))
        
        (T
          (error "Unrecognized character \"~c\" at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-quick-class Node
    (type
     &rest initial-attributes
     &aux
      (attributes
        (let ((attribute-map (make-hash-table :test #'eq)))
          (declare (type attribute-map attribute-map))
          (loop
            for (attribute-name attribute-value)
              of-type (keyword T)
              on      (the attribute-list initial-attributes)
              by      #'cddr
            do
              (setf (gethash attribute-name attribute-map)
                    attribute-value))
          (the attribute-map attribute-map))))
  "The ``Node'' class applies itself to the representation of an
   abstract syntax tree (AST) node, describing a facility in a parsed :]
   program."
  (type       keyword       (error "Missing node type."))
  (attributes attribute-map (error "Missing node attributes.")))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the NODE attribute associates with the ATTRIBUTE-NAME, or
   signals an error of an unspecified type upon its absence."
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (the T
    (multiple-value-bind (attribute-value contains-name-p)
        (gethash attribute-name (node-attributes node))
      (if contains-name-p
        attribute-value
        (error "Unrecognized attribute name: ~s." attribute-name)))))

;;; -------------------------------------------------------

(defun (setf node-attribute) (new-value node attribute-name)
  "Associates the NEW-VALUE with the ATTRIBUTE-NAME in the NODE and
   returns no value."
  (declare (type T       new-value))
  (declare (type Node    node))
  (declare (type keyword attribute-name))
  (setf (gethash attribute-name (node-attributes node))
        new-value)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (loop
    initially
      (format stream "(Node ~s"
        (node-type node))
    
    for attribute-name
      of-type keyword
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    do
      (format stream ", ~s=~s" attribute-name attribute-value)
    
    finally
      (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Parser) Node) parser-parse-statement))

;;; -------------------------------------------------------

(define-quick-class Parser
    (lexer
     &aux (current-token (lexer-get-next-token lexer)))
  "The ``Parser'' class occupies the wike of an abstract syntax tree's
   (AST) assemblage from a series of tokens provided by a lexer."
  (lexer         Lexer (error "Missing lexer."))
  (current-token Token (make-token :eof NIL)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation the current token,
   while the next token is queried from the internally managed lexer and
   stored in its stead; otherwise an error of an unspecified type is
   signaled."
  (declare (type Parser parser))
  (the Token
    (with-parser (parser)
      (prog1 current-token
        (if (token-type-p current-token expected-token-type)
          (setf current-token
            (lexer-get-next-token lexer))
          (error "Expected a token of the type ~s, but encountered ~s."
            expected-token-type current-token))))))

;;; -------------------------------------------------------

(defun parser-skip-empty-lines (parser)
  "Proceeding from the current token in the PARSER, skips a sequence of
   zero or more accolent newline tokens and returns no value."
  (declare (type Parser parser))
  (with-parser (parser)
    (loop while (token-type-p current-token :newline) do
      (parser-eat parser :newline))
    (values)))

;;; -------------------------------------------------------

(defun parser-expect-end-of-line (parser)
  "Determines whether, proceeding from the current token in the PARSER,
   either at least one newline token follows, on confirmation skipping
   all accolent tokens of this ilk, or an end-of-file token is presents,
   in both cases returning no value; otherwise an error of an
   unspecified type is signaled."
  (declare (type Parser parser))
  (with-parser (parser)
    (unless (token-type-p current-token :eof)
      (parser-eat parser :newline)
      (parser-skip-empty-lines parser)))
  (values))

;;; -------------------------------------------------------

(defun parser-parse-string (parser)
  "Proceeding from the current token in the PARSER, consumes the token
   and returns a ``:string'' node representation thereof."
  (declare (type Parser parser))
  (the Node
    (make-node :string :value
      (token-value
        (parser-eat parser :string)))))

;;; -------------------------------------------------------

(defun parser-parse-number (parser)
  "Proceeding from the current token in the PARSER, consumes the token
   and returns a ``:number'' node representation thereof."
  (declare (type Parser parser))
  (the Node
    (make-node :number :value
      (token-value
        (parser-eat parser :number)))))

;;; -------------------------------------------------------

(defun parser-parse-input (parser)
  "Proceeding from the current token in the PARSER, consumes the token
   and returns an ``:input'' node representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :input)
  (the Node
    (make-node :input)))

;;; -------------------------------------------------------

(defun parser-parse-variable (parser)
  "Proceeding from the current token in the PARSER, consumes the token
   and returns a ``:variable'' node representation thereof."
  (declare (type Parser parser))
  (parser-eat parser :variable)
  (the Node
    (make-node :variable)))

;;; -------------------------------------------------------

(defun parser-parse-expression (parser)
  "Proceeding from the current token in the PARSER, parses an expression
   and returns a node representation thereof."
  (declare (type Parser parser))
  (the Node
    (with-parser (parser)
      (case (token-type current-token)
        (:string
          (parser-parse-string parser))
        (:number
          (parser-parse-number parser))
        (:input
          (parser-parse-input parser))
        (:variable
          (parser-parse-variable parser))
        (otherwise
          (error "No expression token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-relational-operator (parser)
  "Evaluates the PARSER's current token in order to obtain a relational
   operator which is subsequently returned."
  (declare (type Parser parser))
  (the relation
    (with-parser (parser)
      (if (relation-token-p current-token)
        (prog1
          (token-type current-token)
          (parser-eat parser (token-type current-token)))
        (error "No relation token: ~s." current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-loop-condition (parser loop-node)
  "Proceeding from the PARSER's current token, evaluates the stated
   loop condition, defines the respective attributes in the LOOP-NODE,
   and returns the same."
  (declare (type Parser parser))
  (declare (type Node   loop-node))
  (with-parser (parser)
    (case (token-type current-token)
      ;; :]] number
      (:number
        (setf (node-attribute loop-node :kind) :counting)
        (setf (node-attribute loop-node :repetitions)
          (parser-parse-number parser)))
      ;; :]] :|
      (:input
        (setf (node-attribute loop-node :kind) :counting)
        (setf (node-attribute loop-node :repetitions)
          (parser-parse-input parser)))
      ;; :]] :)
      ;; :]] :) relation testValue
      (:variable
        (parser-eat parser :variable)
        (cond
          ;; :]] :) relation testValue
          ((relation-token-p current-token)
            (setf (node-attribute loop-node :kind) :until)
            (setf (node-attribute loop-node :relation)
              (parser-parse-relational-operator parser))
            (setf (node-attribute loop-node :test-value)
              (parser-parse-expression parser)))
          ;; :]] :)
          (T
            (setf (node-attribute loop-node :kind) :counting)
            (setf (node-attribute loop-node :repetitions)
              (make-node :variable)))))
      (otherwise
        (error "No valid loop condition token: ~s."
          current-token))))
  (the Node loop-node))

;;; -------------------------------------------------------

(defun parser-parse-loop-body (parser loop-node)
  "Proceeding from the PARSER's current token, collects the currently
   processed loop's body statements, stores these as an attribute in the
   LOOP-NODE, and returns the same."
  (declare (type Parser parser))
  (declare (type Node   loop-node))
  (setf (node-attribute loop-node :body)
    (with-parser (parser)
      (loop
        until   (token-type-p current-token :end-loop)
        do      (parser-skip-empty-lines parser)
        collect (prog1
                  (parser-parse-statement    parser)
                  (parser-expect-end-of-line parser)))))
  (the Node loop-node))

;;; -------------------------------------------------------

(defun parser-parse-loop (parser)
  "Proceeding from the current token in the PARSER, parses a loop
   construct and returns a ``:loop'' node representation thereof."
  (declare (type Parser parser))
  (the Node
    (let ((loop-node (make-node :loop)))
      (declare (type Node loop-node))
      (parser-eat                  parser :start-loop)
      (parser-parse-loop-condition parser loop-node)
      (parser-expect-end-of-line   parser)
      (parser-parse-loop-body      parser loop-node)
      (parser-eat                  parser :end-loop)
      (the Node loop-node))))

;;; -------------------------------------------------------

(defun parser-parse-statement (parser)
  "Proceeding from the current token in the PARSER, parses a single
   statement and returns a node representation thereof."
  (declare (type Parser parser))
  (the Node
    (with-parser (parser)
      (case (token-type current-token)
        (:output
          (parser-eat parser :output)
          (make-node :output :argument
            (parser-parse-expression parser)))
        (:input
          (parser-parse-input parser))
        (:variable
          (parser-eat parser :variable)
          (if (expression-token-p current-token)
            (make-node :set-variable :value
              (parser-parse-expression parser))
            (make-node :blank-variable)))
        (:increase
          (parser-eat parser :increase)
          (make-node :increase))
        (:decrease
          (parser-eat parser :decrease)
          (make-node :decrease))
        (:set-to-zero
          (parser-eat parser :set-to-zero)
          (make-node :set-to-zero))
        (:start-loop
          (parser-parse-loop parser))
        (:end-script
          (parser-eat parser :end-script)
          (make-node :end-script))
        (otherwise
          (error "Invalid statement token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Assembles the tokens supplied to the PARSER by its internally managed
   lexer and returns a ``:program'' node comprehending the abstract
   syntax tree (AST) root."
  (declare (type Parser parser))
  (the Node
    (with-parser (parser)
      (make-node :program :statements
        (loop
          until (token-type-p current-token :eof)
          do    (parser-skip-empty-lines parser)
          collect
            (prog1
              (parser-parse-statement    parser)
              (parser-expect-end-of-line parser)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of unary operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rsf-object-increment (object)
  (:documentation
    "Returns the OBJECT's response to an incrementation attempt."))

;;; -------------------------------------------------------

(defgeneric rsf-object-decrement (object)
  (:documentation
    "Returns the OBJECT's response to a decrementation attempt."))

;;; -------------------------------------------------------

(defmethod rsf-object-increment ((object integer))
  (declare (type integer object))
  (the integer (+ object 1)))

;;; -------------------------------------------------------

(defmethod rsf-object-increment ((object string))
  (declare (type string object))
  (error "Cannot increment the string ~s." object))

;;; -------------------------------------------------------

(defmethod rsf-object-decrement ((object integer))
  (declare (type integer object))
  (the integer (- object 1)))

;;; -------------------------------------------------------

(defmethod rsf-object-decrement ((object string))
  (declare (type string object))
  (error "Cannot decrement the string ~s." object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of relation operators.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric relation-satisfied-p (relation-operator
                                  left-operand
                                  right-operand)
  (:documentation
    "Applies the LEFT-OPERAND and the RIGHT-OPERAND to the RELATION
     operator and returns a ``boolean'' value of ``T'' upon the
     combination's affirmation, otherwise ``NIL''."))

;;; -------------------------------------------------------

(defmacro define-relation-test (relation-operator
                                (left-operand-type right-operand-type)
                                &body body)
  (let ((relation-operator-variable (gensym)))
    (declare (type symbol relation-operator-variable))
    `(defmethod relation-satisfied-p
         ((,relation-operator-variable (eql ,relation-operator))
          (left-operand                ,left-operand-type)
          (right-operand               ,right-operand-type))
       (declare (type relation            ,relation-operator-variable))
       (declare (ignore                   ,relation-operator-variable))
       (declare (type ,left-operand-type  left-operand))
       (declare (ignorable                left-operand))
       (declare (type ,right-operand-type right-operand))
       (declare (ignorable                right-operand))
       (the boolean
        (not (null
          (progn ,@body)))))))

;;; -------------------------------------------------------

(define-relation-test :equal-to (integer integer)
  (= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :equal-to (integer string)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :equal-to (string integer)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :equal-to (string string)
  (string= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :not-equal-to (integer integer)
  (/= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :not-equal-to (integer string)
  T)

;;; -------------------------------------------------------

(define-relation-test :not-equal-to (string integer)
  T)

;;; -------------------------------------------------------

(define-relation-test :not-equal-to (string string)
  (string/= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :less-than (integer integer)
  (< left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :less-than (integer string)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :less-than (string integer)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :less-than (string string)
  (string< left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :less-than-or-equal-to (integer integer)
  (<= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :less-than-or-equal-to (integer string)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :less-than-or-equal-to (string integer)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :less-than-or-equal-to (string string)
  (string<= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :greater-than (integer integer)
  (> left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :greater-than (integer string)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :greater-than (string integer)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :greater-than (string string)
  (string> left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :greater-than-or-equal-to (integer integer)
  (>= left-operand right-operand))

;;; -------------------------------------------------------

(define-relation-test :greater-than-or-equal-to (integer string)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :greater-than-or-equal-to (string integer)
  NIL)

;;; -------------------------------------------------------

(define-relation-test :greater-than-or-equal-to (string string)
  (string>= left-operand right-operand))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-input (input)
  "Evaluates the INPUT and returns either a integer or string
   representation thereof."
  (declare (type string input))
  (the rsf-object
    (cond
      ((null input)
        "")
      ((zerop (length input))
        input)
      (T
        (or (ignore-errors (parse-integer input))
            input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Halt-Condition".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ()
  (:documentation
    "The ``Halt-Condition'' serves to signal the intention to terminate
     the program immediately."))

;;; -------------------------------------------------------

(defun signal-halt-condition ()
  "Signals a program termination behest in order to communicate the
   desire to halt the same."
  (signal 'Halt-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-quick-class Interpreter (program)
  "The ``Interpreter'' class occupies the wike of accompassing to an
   abstract syntax tree (AST) actual operation."
  (program  Node       (error "Missing program."))
  (variable rsf-object 0))

;;; -------------------------------------------------------

(defgeneric interpreter-dispatch-node (interpreter node-type node)
  (:documentation
    "Processes the NODE, dispatched on its NODE-TYPE, in the
     INTERPRETER's context and returns a value appropriate for this
     combination."))

;;; -------------------------------------------------------

(defun interpreter-visit-node (interpreter node)
  "Processes the NODE in the INTERPRETER's context by dispatching on the
   NODE's type and invoking the respective ``interpreter-dispatch-node''
   generic function implementation, whose result is returned."
  (declare (type Interpreter interpreter))
  (declare (type Node        node))
  (the T
    (interpreter-dispatch-node interpreter (node-type node) node)))

;;; -------------------------------------------------------

(defmacro define-node-dispatch
    (node-type (interpreter-variable node-variable)
     &body body)
  "Defines an implementation of the generic function
   ``interpreter-dispatch-node'' whose first parameter is designated by
   the INTERPRETER-VARIABLE, the third by the NODE-VARIABLE, and whose
   automatically norned second argument, dispatches on an
   ``eql''-equality with the NODE-TYPE, executing the BODY forms, and
   returning the desinent form's results."
  (let ((node-type-variable (gensym)))
    (declare (type symbol node-type-variable))
    `(defmethod interpreter-dispatch-node
         ((,interpreter-variable Interpreter)
          (,node-type-variable   (eql ,node-type))
          (,node-variable        Node))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type keyword     ,node-type-variable))
       (declare (ignore           ,node-type-variable))
       (declare (type Node        ,node-variable))
       (declare (ignorable        ,node-variable))
       ,@body)))

;;; -------------------------------------------------------

(define-node-dispatch :program (interpreter node)
  (handler-case
    (dolist (statement (node-attribute node :statements))
      (declare (type Node statement))
      (interpreter-visit-node interpreter statement))
    (Halt-Condition
      NIL))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :number (interpreter node)
  (the integer
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :string (interpreter node)
  (the string
    (node-attribute node :value)))

;;; -------------------------------------------------------

(define-node-dispatch :variable (interpreter node)
  (the rsf-object
    (interpreter-variable interpreter)))

;;; -------------------------------------------------------

(define-node-dispatch :input (interpreter node)
  (format T "~&>> ")
  (the rsf-object
    (evaluate-input
      (read-line *standard-input* NIL NIL))))

;;; -------------------------------------------------------

(define-node-dispatch :output (interpreter node)
  (format T "~a "
    (interpreter-visit-node interpreter
      (node-attribute node :argument)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :set-variable (interpreter node)
  (setf (interpreter-variable interpreter)
    (interpreter-visit-node interpreter
      (node-attribute node :value)))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :increase (interpreter node)
  (incf (interpreter-variable interpreter))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :decrease (interpreter node)
  (decf (interpreter-variable interpreter))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :set-to-zero (interpreter node)
  (setf (interpreter-variable interpreter) 0)
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :blank-variable (interpreter node)
  (setf (interpreter-variable interpreter) "")
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :loop (interpreter node)
  (case (node-attribute node :kind)
    (:counting
      (let ((repetitions (node-attribute node :repetitions)))
        (declare (type Node repetitions))
        (loop
          repeat (interpreter-visit-node interpreter repetitions)
          do
            (dolist (statement (node-attribute node :body))
              (declare (type Node statement))
              (interpreter-visit-node interpreter statement)))))
    (:until
      (let ((relation   (node-attribute node :relation))
            (test-value (node-attribute node :test-value)))
        (declare (type relation relation))
        (declare (type Node     test-value))
        (loop
          until
            (relation-satisfied-p relation
              (interpreter-variable   interpreter)
              (interpreter-visit-node interpreter test-value))
          do
            (dolist (statement (node-attribute node :body))
              (declare (type Node statement))
              (interpreter-visit-node interpreter statement)))))
    (otherwise
      (error "Invalid loop kind: ~s."
        (node-attribute node :kind))))
  (values))

;;; -------------------------------------------------------

(define-node-dispatch :end-script (interpreter node)
  (signal-halt-condition))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the program stored in the INTERPRETER as an abstract
   syntax tree (AST) and returns no value."
  (declare (type Interpreter interpreter))
  (interpreter-visit-node interpreter
    (interpreter-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Robotic-smiley-face (code)
  "Interprets the piece of :] source CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (parser-parse
        (make-parser
          (make-lexer code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hello world".
(interpret-Robotic-smiley-face
  ":0 “hello world”
   :(")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Robotic-smiley-face
  ":0 :|
   :(")

;;; -------------------------------------------------------

;; Counter program that counts up from zero (0) to infinity.
(interpret-Robotic-smiley-face
  "):
  :]] :) == -1
  :0 :)
  :D
  :[[
  :(")

;;; -------------------------------------------------------

;; Count from 1 to 100 while printing these states.
(interpret-Robotic-smiley-face
  "):
   :D
   :]] :) > 100
   :0 :)
   :D
   :[[
   :(")

;;; -------------------------------------------------------

;; Looping counter that prints asterisks in the closed range [1, 15].
(interpret-Robotic-smiley-face
  "):
   :D
   :]] :) == 15
     :]] :)
       :0 “*”
     :[[
     :0 “
”
     :D
   :[[
   :(")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Robotic-smiley-face
  ":)
   :) :|
   :]] :) == 0
     :0 :)
   :[[
   :0 :)
   :(")
