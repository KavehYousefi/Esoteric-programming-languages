;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Targs", invented by the Esolang user "SpaceByte" and
;; presented on June 16th, 2022, its dioristic haecceity having its
;; woning in the source code's structure, which assigns to the
;; incorporated instructions the ubiquity of character twains, therein
;; appending to the operation type identifier its sole parameter, in
;; this operating on a unilateral infinite tape of signed integer cells.
;; 
;; 
;; Concept
;; =======
;; The Targs programming language's kenspeckle design emanates from the
;; expression of its commands as two-character compounds, allocating the
;; command type sinistrally to the immediately succeeding operand.
;; 
;; == TARGS: [T]WO [ARGS] ==
;; The choice of "Targs" as the language's agnomination is begotten from
;; its commands' conformation, constantly forming a compound of two
;; significant letters at a token's commencement. These items bear, in
;; the protolog's diction, the stevening as "arguments", whence proceeds
;; "[t]wo [arg]ument[s]" or "[t]wo [args]".
;; 
;; An alternative name exists in "EsoTargs", which precedes the
;; portmanteau by an intimation of its [ESO]teric linguistic
;; subsumption.
;; 
;; == TOKENS ARE SEGREGATED VIA WHITESPACES ==
;; Each two tokens' sepiment is realized by a sequence of one or more
;; whitespaces; ensuing from this diorism.
;; 
;; == INSTRUCTIONS ARE EXPRESSED IN CHARACTER JUMELLES ==
;; An instruction's conformation tallies an exact account of two
;; characters, the sinistral among the twain communicating the command
;; type, while the dextral participant contributes the argument.
;; 
;; == NON-INSTRUCTION TOKENS FORM NEGLIGIBLE CONTENT ==
;; Only a nonadic subset of the representable character repertoire,
;; equinumerant with the operative competences, is registered as a
;; command type, namely, the Latin letter "c", "d", "i", "l", "n", "p",
;; "r", "u", and "w". Siccan words whose inchoation exhibits a deviation
;; from this contingency are ignored and may be appropriated for
;; commentary purposes.
;; 
;; == INSTRUCTION OPERANDS ARE EITHER DIGITS OR "P" ==
;; The command argument's composition itself admits merely decimal
;; digits, either in the agency of a magnitude or a cell index, or, in
;; some contexts, the keyword "p", the same connotes the memory's cell
;; pointer position as an alternative to the literal specification.
;; 
;; == ADDITIONAL CONTENT TO AN INSTRUCTION IS IGNORED ==
;; If further non-whitespace characters form an adscititious catena to
;; the recognized command's significant twissel, their presence's
;; recipiency of tolerance measures commensurately with a tacit
;; neglect's adhibition.
;; 
;; == THE MEMORY: A UNILATERALLY INFINITE TAPE OF INTEGERS ==
;; The Targs program memory presents a vector of signed integer-valued
;; cells, enumerated starting with the index zero (0) and extending
;; without a natural bourne along the positive axis.
;; 
;; At the program's incipiency, every cell assumes the default state of
;; zero (0), while carrying the potential for incrementation and
;; deductions.
;; 
;; A cursor, the "cell pointer", partakes of a parhedral role in its
;; selection, at any instant of the program, of the currently active
;; cell, which, in conjunction with the random access nature proffered
;; by the architecture, may be utilized for perquisitions and
;; modulations. Its capacitation for gradual translations along the
;; tape's entirety homologates the cell pointer's motility.
;; 
;; == TARGS SOURCE FILES ARE IDENTIFIED WITH "TGS" ==
;; The file extension engaged in an affiliation with the Targs source
;; code provision constitutes the "tgs" character sequence.
;; 
;; 
;; Instructions
;; ============
;; The Targs programming language's instruction set tallies an ennead
;; membership, its amplectation only exhausted by the subjects of memory
;; management, input, output, and an aefauld conditional command
;; omission operator.
;; 
;; == OVERVIEW ==
;; The following tabulation shall adhibit a cursory mete of gnarity
;; anenst the language's operative competences.
;; 
;; Please heed that succedaneous segments, which always denote command
;; arguments, are ensconced in braces, "{...}", and intended for their
;; supersession by actual Targs source code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   r{x}    | Translates the cell pointer {x} steps to the right.
;;           |---------------------------------------------------------
;;           | {x} must be a decimal integer digit.
;;   ..................................................................
;;   l{x}    | Translates the cell pointer {x} steps to the left. If
;;           | the cell pointer would transcend below the minimum index
;;           | of zero (0) through this translation, the cell pointer
;;           | is actually empight on this first cell at the position
;;           | zero (0).
;;           |---------------------------------------------------------
;;           | {x} must be a decimal integer digit.
;;   ..................................................................
;;   u{x}    | Increments the value of the current cell by {x}.
;;           |---------------------------------------------------------
;;           | {x} must be a decimal integer digit.
;;   ..................................................................
;;   d{x}    | Decrements the value of the current cell by {x}.
;;           |---------------------------------------------------------
;;           | {x} must be a decimal integer digit.
;;   ..................................................................
;;   w{x}    | Prints the character whose ASCII code corresponds to the
;;           | value of the cell at the index {x} on the current line.
;;           |---------------------------------------------------------
;;           | {x} must be either of:
;;           |   - A decimal integer digit.
;;           |   - The "p" keyword, which identifies the cell pointer's
;;           |     current position.
;;   ..................................................................
;;   p{x}    | Prints a newline character, immediately succeeded by the
;;           | character whose ASCII code corresponds to the value of
;;           | the cell at the index {x}.
;;           |---------------------------------------------------------
;;           | {x} must be either of:
;;           |   - A decimal integer digit.
;;           |   - The "p" keyword, which identifies the cell pointer's
;;           |     current position.
;;   ..................................................................
;;   n{x}    | Queries the standard input for an unsigned integer
;;           | number and stores the same in the cell at the index {x}.
;;           |---------------------------------------------------------
;;           | {x} must be either of:
;;           |   - A decimal integer digit.
;;           |   - The "p" keyword, which identifies the cell pointer's
;;           |     current position.
;;   ..................................................................
;;   c{x}    | Queries the standard input for a character and stores
;;           | its ASCII code in the cell at the index {x}.
;;           |---------------------------------------------------------
;;           | {x} must be either of:
;;           |   - A decimal integer digit.
;;           |   - The "p" keyword, which identifies the cell pointer's
;;           |     current position.
;;   ..................................................................
;;   i{x}    | If the cell at the index {x} contains the value zero
;;           | (0), skips the subsequent command; otherwise executes it
;;           | as usual.
;;           |---------------------------------------------------------
;;           | {x} must be either of:
;;           |   - A decimal integer digit.
;;           |   - The "p" keyword, which identifies the cell pointer's
;;           |     current position.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp, its paravaunt telos' fulfilment, the execution of Targs
;; programs, registering its foundry upon an immediate operation upon
;; the source code; while, concomitantly, a parergal pursuit is
;; accompassed in the definition of a bespoke function construct,
;; ``define-function'', in order to reformulate and facilitate the
;; implementation of operations offering rich type information.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-13
;; 
;; Sources:
;;   [esolang2022Targs]
;;   The Esolang contributors, "Targs", June 22nd, 2022
;;   URL: "https://esolangs.org/wiki/Targs"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type declaration operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type (type-name
                              (candidate-variable &rest lambda-list)
                              &body body)
  "Defines a derived type using the ``deftype'' infrastructure, its
   agnomination desumed from the TYPE-NAME and its formal parameters
   specified vai the LAMBDA-LIST, the eligiblity of which probed by the
   BODY forms, admitted adit to the perquired candidate by the name of
   the CANDIDATE-VARIABLE, returning a non-``NIL'' value in its desinent
   BODY form's primary value upon the subject's conformance to the
   type's imposed requirements, otherwise ``NIL''.
   ---
   A string conveyed as the first BODY form is always construed as the
   type definition's documentation string and reappropriated for this
   purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-custom-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies to the ELEMENT-TYPE, its
   default constituting the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(define-custom-type property-list-of (candidate
                                      &optional (value-type T))
  "The ``property-list-of'' type defines a property list whose
   indicators constitute keyword symbols, answering to values of the
   VALUE-TYPE, the same defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (evenp (length (the list candidate)))
    (loop
      for (indicator value)
        of-type (T T)
        on      candidate
        by      #'cddr
      always
        (and (keywordp indicator)
             (typep    value value-type)))))

;;; -------------------------------------------------------

(deftype argument-list ()
  "The ``argument-list'' type defines a formal parameter list as a list
   of ``KYFunction-Argument'' instances."
  '(list-of KYFunction-Argument))

;;; -------------------------------------------------------

(deftype decimal-digit ()
  "The ``decimal-digit'' type defines a decimal digit as an unsigned
   integer number encompassed by the closed interval [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype command-argument ()
  "The ``command-argument'' type defines a valid species of arguments in
   a command type's compernage, namely, the ``NIL'' value, decimal
   integer digits, or the ``:pointer'' keyword sentinel."
  '(or null
       decimal-digit
       (eql :pointer)))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized specimens of
   Targs commands."
  '(member
    :move-pointer-right
    :move-pointer-left
    :increment
    :decrement
    :print-on-fresh-line
    :print-on-same-line
    :skip-if-zero
    :input-number
    :input-character
    :nop))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type defines a Targs command as a composition of a
   command species and its argument, manifesting in a cons cell, the
   sinistral compartment of which lends a commorancy to the type, while
   the dextral moiety amplects the argument."
  '(cons command-type command-argument))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Targs program memory as a
   one-dimensional simple array of integer elements."
  '(simple-array integer (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "KYFunction-Result".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct KYFunction-Result
  "The ``KYFunction-Result'' encapsulates a ``define-function''
   definition's return value or values."
  (type                  (error "Missing result type.")
                         :type      T
                         :read-only T)
  (has-multiple-values-p (error "Missing multiple values flag.")
                         :type boolean
                         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "KYFunction-Argument".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct KYFunction-Argument
  "The ``KYFunction-Argument'' encapsulates the properties of a
   ``define-function'' definition's single formal parameter."
  (name       (error "Missing name.")
              :type      symbol
              :read-only T)
  (type       (error "Missing type.")
              :type      T
              :read-only T)
  (default    (error "Missing default value.")
              :type      T
              :read-only T)
  (required-p (error "Missing required flag.")
              :type      boolean
              :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "KYFunction".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct KYFunction
  "The ``KYFunction'' class encapsulates the entirety of a custom
   function diorism, which enumerates in its compass the operation name,
   an optional documentation, a list of zero or more formal parameters,
   the return values, and the body forms.
   ---
   Ensuing from this conglomerated information, a connable ``defun''
   equivalency may be extracted."
  (name          (error "Missing name.")
                 :type      symbol
                 :read-only T)
  (documentation (error "Missing documentation.")
                 :type      string
                 :read-only T)
  (arguments     (error "Missing arguments.")
                 :type      argument-list
                 :read-only T)
  (result        (error "Missing result.")
                 :type      KYFunction-Result
                 :read-only T)
  (body          (error "Missing body.")
                 :type      T
                 :read-only T))

;;; -------------------------------------------------------

(defun build-return-type-specification (function)
  "Returns for the FUNCTION a type specifier compatible with the ``the''
   special operator."
  (declare (type KYFunction function))
  (let ((result (kyfunction-result function)))
    (declare (type KYFunction-Result result))
    (the T
      (if (kyfunction-result-has-multiple-values-p result)
        `(values ,@(kyfunction-result-type result))
        (kyfunction-result-type result)))))

;;; -------------------------------------------------------

(defun build-required-lambda-list (required-arguments)
  "Returns for the REQUIRED-ARGUMENTS the ``defun''-compatible moiety of
   a lambda list for mandatory arguments."
  (declare (type argument-list required-arguments))
  (the (list-of T)
    (mapcar #'kyfunction-argument-name required-arguments)))

;;; -------------------------------------------------------

(defun build-optional-lambda-list (optional-arguments)
  "Returns for the OPTIONAL-ARGUMENTS either a ``defun''-compatible
   optional segment of a lambda list, comprehending also the
   parasceuastic ``&optional'' sentinel, if the same is not vacant;
   otherwise responds with an empty list."
  (declare (type argument-list optional-arguments))
  (the (list-of T)
    (when optional-arguments
      (append '(&optional)
        (mapcar
          #'(lambda (argument)
              (declare (type KYFunction-Argument argument))
              `(,(kyfunction-argument-name    argument)
                ,(kyfunction-argument-default argument)))
          optional-arguments)))))

;;; -------------------------------------------------------

(defun build-lambda-list (function)
  "Returns for the FUNCTION a ``defun''-compatible lambda list."
  (declare (type KYFunction function))
  (the (list-of T)
    (append
      (build-required-lambda-list
        (remove-if-not
          #'kyfunction-argument-required-p
          (kyfunction-arguments function)))
      (build-optional-lambda-list
        (remove-if
          #'kyfunction-argument-required-p
          (kyfunction-arguments function))))))

;;; -------------------------------------------------------

(defun build-argument-declarations (function)
  "Returns for the FUNCTION's arguments a list of declarations."
  (declare (type KYFunction function))
  (the (list-of T)
    (mapcan
      #'(lambda (argument)
          (declare (type KYFunction-Argument argument))
          (let ((argument-name (kyfunction-argument-name argument))
                (argument-type (kyfunction-argument-type argument)))
            (declare (type symbol argument-name))
            (declare (type T      argument-type))
            (list
              `(declare (type ,argument-type ,argument-name))
              `(declare (ignorable           ,argument-name)))))
      (kyfunction-arguments function))))

;;; -------------------------------------------------------

(defun build-defun-declaration (function)
  "Returns for the FUNCTION a convenable ``defun'' declaration."
  (declare (type KYFunction function))
  `(defun ,(kyfunction-name function) ,(build-lambda-list function)
     ,(kyfunction-documentation function)
     ,@(build-argument-declarations function)
     (the ,(build-return-type-specification function)
       (progn
         ,@(kyfunction-body function)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of function declaration macro.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-list (object)
  "Ensures the OBJECT to constitute a list by either returning the same
   verbatim, if it already represents such, or by responding with a
   fresh singleton list comprehending as its aefauld member the OBJECT."
  (declare (type T object))
  (the (list-of T)
    (if (listp object)
      object
      (list object))))

;;; -------------------------------------------------------

(defun extract-result-specifications (signature)
  "Returns from the function SIGNATURE specification a list of its
   returns types, recognized by their ensconcement in a
   ``(:returns {types})'' sublist, and coalesced into a single sequence
   if several such definitions transpire."
  (declare (type list signature))
  (the (list-of T)
    (mapcan
      #'(lambda (definition)
          (declare (type list definition))
          (when (eq (first definition) :returns)
            (ensure-list
              (second definition))))
      signature)))

;;; -------------------------------------------------------

(defun empty-list-p (list)
  "Determines whether the LIST is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (list-of T) list))
  (the boolean
    (null list)))

;;; -------------------------------------------------------

(defun singleton-list-p (list)
  "Determines whether the LIST constitutes a singleton list, that is,
   siccan object that comprehends an aefauld member oncly, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (list-of T) list))
  (the boolean
    (not (null
      (= (length list) 1)))))

;;; -------------------------------------------------------

(defun parse-return-types (specification)
  "Parses the function return values SPECIFICATION and returns a
   ``KYFunction-Result'' representation of its diorism."
  (declare (type list specification))
  (the KYFunction-Result
    (cond
      ((empty-list-p specification)
        (make-kyfunction-result
          :type                  T
          :has-multiple-values-p NIL))
      ((singleton-list-p specification)
        (make-kyfunction-result
          :type                  (first specification)
          :has-multiple-values-p NIL))
      (T
        (make-kyfunction-result
          :type                  specification
          :has-multiple-values-p T)))))

;;; -------------------------------------------------------

(defun check-argument-properties-type (properties)
  "Determines whether the PROPERTIES represent a valid property list for
   ``define-function'' arguments, returning on confirmation no value,
   otherwise signaling an error of an unspecified type."
  (declare (type (list-of T) properties))
  (unless (typep properties '(property-list-of T))
    (error "No valid argument property list: ~s." properties))
  (values))

;;; -------------------------------------------------------

(defun argument-property-name-p (property-name)
  "Determines whether the PROPERTY-NAME constitutes siccan keyword
   admissible to a ``define-function'' argument property, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T property-name))
  (the boolean
    (not (null
      (and (keywordp property-name)
           (member property-name '(:default :type) :test #'eq))))))

;;; -------------------------------------------------------

(defun check-argument-property-names (properties)
  "Determines whether the PROPERTIES contain exclusively valid
   ``define-function'' property name, returning on confirmation no
   value, otherwise signaling an error of an unspecified type."
  (declare (type (property-list-of T) properties))
  (loop
    for property-name of-type keyword in properties by #'cddr
    unless (argument-property-name-p property-name) do
      (error "No valid argument property name: ~s." property-name))
  (values))

;;; -------------------------------------------------------

(defun count-argument-property-name (properties property-name)
  "Returns the tally of times that the PROPERTY-NAME is entailed as an
   indicator among the PROPERTIES."
  (declare (type (property-list-of T) properties))
  (declare (type keyword              property-name))
  (the (integer 0 *)
    (loop
      for current-property-name of-type keyword in properties by #'cddr
      when (eq current-property-name property-name)
        count 1)))

;;; -------------------------------------------------------

(defun check-argument-property-counts (properties)
  "Determines whether the PROPERTIES contain each valid argument
   property name for a ``define-function'' at most once, returning on
   confirmation no value, otherwise signaling an error of an unspecified
   type."
  (declare (type (property-list-of T) properties))
  (loop
    for probed-property-name
      of-type keyword
      in      '(:default :type)
    for number-of-occurrences
      of-type (integer 0 *)
      =       (count-argument-property-name properties
                                            probed-property-name)
    unless (<= number-of-occurrences 1) do
      (error "The argument property ~s may only occur at most once, ~
              but has been specified ~d times."
        probed-property-name number-of-occurrences))
  (values))

;;; -------------------------------------------------------

(defun validate-argument-properties (properties)
  "Determines whether the ``define-function'' argument PROPERTIES comply
   to the requisites' imposition concerning conformation and content,
   on confirmation returning no value, otherwise signaling an error of
   an unspecified type."
  (declare (type (list-of T) properties))
  (check-argument-properties-type properties)
  (check-argument-property-names  properties)
  (check-argument-property-counts properties)
  (values))

;;; -------------------------------------------------------

(defun contains-argument-property (properties property-name)
  "Determines whether the ``define-function'' argument PROPERTIES entail
   an entry designated by the PROPERTY-NAME, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (property-list-of T) properties))
  (declare (type keyword              property-name))
  (the boolean
    (not (null
      (loop
        for current-property-name
          of-type keyword
          in      properties
          by      #'cddr
        thereis (eq current-property-name property-name))))))

;;; -------------------------------------------------------

(defun parse-function-argument (specification)
  "Parses the function argument SPECIFICATION and returns a
   ``KYFunction-Argument'' representation of its diorism."
  (declare (type (or symbol (list-of T)) specification))
  (let ((properties (rest specification)))
    (declare (type (list-of T) properties))
    (validate-argument-properties (rest specification))
    (the KYFunction-Argument
      (if (listp specification)
        (let ((name       (pop  specification))
              (type       (getf specification :type    T))
              (default    (getf specification :default NIL)))
          (declare (type symbol name))
          (declare (type T       type))
          (declare (type T       default))
          (make-kyfunction-argument
            :name       name
            :type       type
            :default    default
            :required-p
              (not (contains-argument-property properties :default))))
        (make-kyfunction-argument
          :name       specification
          :type       T
          :default    NIL
          :required-p T)))))

;;; -------------------------------------------------------

(defun extract-argument-specifications (signature)
  "Returns from the function SIGNATURE specification a list of its
   argument specification, recognized by their ensconcement in a
   ``(:accepts {types})'' sublist."
  (declare (type (list-of T) signature))
  (the (list-of T)
    (mapcan
      #'(lambda (definition)
          (declare (type list definition))
          (when (eq (first definition) :accepts)
            (rest definition)))
      signature)))

;;; -------------------------------------------------------

(defun extract-arguments (signature)
  "Extracts and returns from the ``define-function'' SIGNATURE a list of
   its arguments."
  (declare (type (list-of T) signature))
  (the argument-list
    (mapcar #'parse-function-argument
      (extract-argument-specifications signature))))

;;; -------------------------------------------------------

(defun interface-attribute-type-p (candidate)
  "Determines whether the CANDIDATE constitutes a recognized
   ``define-function'' interface attribute, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (the boolean
    (not (null
      (and (keywordp candidate)
           (member candidate '(:accepts :returns) :test #'eq))))))

;;; -------------------------------------------------------

(defun validate-function-interface-attributes (interface-attributes)
  "Determines whether the ``define-function'' INTERFACE-ATTRIBUTES
   comply to the requisites' impositions concerning their conformation
   and content, returning on confirmation no value, otherwise signaling
   an error of an unspecified type."
  (declare (type (list-of T) interface-attributes))
  (loop
    for attribute of-type T in interface-attributes
    unless (interface-attribute-type-p (first attribute)) do
      (error "Invalid function interface attribute: ~s." attribute))
  (values))

;;; -------------------------------------------------------

(defmacro define-function (function-name (&rest interface-attributes)
                           &body body)
  "Defines a new named function, designated by the FUNCTION-NAME, and
   whose both formal parameters and return value types are specified via
   the INTERFACE-ATTRIBUTES, the implementations being accommodated by
   the BODY forms, which, wrapped into an implicit ``progn'' form,
   respond with their desinent form's returns values.
   ---
   INTERFACE-ATTRIBUTES must constitutes a list composed of zero or more
   input or output declarations:
     ------------------------------------------------------------------
     Interface atttribute pattern | Causatum
     -----------------------------+------------------------------------
     (:accepts {argument-spec}*)  | Defines zero or more arguments
                                  | delineated by the {ARGUMENT-SPEC}
                                  | sequence, whose single items'
                                  | conformation please extract below.
     ..................................................................
     (:returns {type-specifiers}) | Defines one or more return values,
                                  | any such either a symbolic atom or
                                  | a list of such and committed
                                  | through the {TYPE-SPECIFIERS}.
     ------------------------------------------------------------------
   
   For the {ARGUMENT-SPEC}, apportioned the dever of providing a formal
   parameter's diorism, the following contingencies exist:
     ------------------------------------------------------------------
     Argument specifier pattern             | Causatum
     ---------------------------------------+--------------------------
     {name}                                 | Defines a required
                                            | argument of the {name},
                                            | admitting any type.
     ..................................................................
     {name} :type {type}                    | Defines a required
                                            | argument of the {name},
                                            | conforming to the {type}.
     ..................................................................
     {name} :default {default}              | Defines an optional
                                            | argument of the {name},
                                            | admitting any type, and
                                            | assuming the initial
                                            | {default} value.
     ..................................................................
     {name} :type {type} :default {default} | Defines an optional
                                            | argument of the {name},
                                            | conforming to the {type},
                                            | and assuming the initial
                                            | {default} value.
     ------------------------------------------------------------------
   
   Any of the interface attribute species, ``:accepts'' and
   ``:returns'', may be associated with zero or more specifications.
   During the parsing stage, these are, obeying their statement order,
   coalseced into a single list.
   
   Proceeding from this diorism, the following argument specifiers
   exhibit a paregal causatum:
     ((:accepts (source   :type string))
      (:accepts (position :type fixnum :default 0)))
     
     ((:accepts (source   :type string)
                (position :type fixum :default 0)))
   
   Siclike, the equivalency applies to the result types:
     ((:returns string)
      (:returns (integer boolean)))
     
     ((:returns (string integer boolean)))"
  (validate-function-interface-attributes interface-attributes)
  (build-defun-declaration
    (make-kyfunction
      :name function-name
      :documentation
        (if (stringp (first body))
          (pop body)
          "")
      :result
        (parse-return-types
          (extract-result-specifications interface-attributes))
      :arguments
        (extract-arguments interface-attributes)
      :body body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function get-boolean-value-of
  ((:returns boolean)
   (:accepts (object :type T)))
  "Returns a veridical ``boolean'' tantamount of the specified OBJECT,
   its derivation produced from the notion of a \"generalized boolean\",
   returning for a non-``NIL'' OBJECT ``T'', otherwise ``NIL''."
  (not (null object)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function whitespace-character-p
  ((:returns boolean)
   (:accepts (candidate :type character)))
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (get-boolean-value-of
    (or (char= candidate #\Newline)
        (char= candidate #\Space)
        (char= candidate #\Tab))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function locate-start-of-word
  ((:accepts (source :type string))
   (:accepts (start  :type fixnum))
   (:returns fixnum))
  "Proceeding from the START position into the SOURCE, returns the
   location of the next non-whitespace character, or, if lacking in
   such, responds with the SOURCE's length."
  (or (position-if-not #'whitespace-character-p source :start start)
      (length source)))

;;; -------------------------------------------------------

(define-function locate-end-of-word
  ((:accepts (source :type string)
             (start  :type fixnum))
   (:returns fixnum))
  "Proceeding from the START position into the SOURCE, returns the
   location of the next whitespace character, or, if lacking in such,
   responds with the SOURCE's length."
  (or (position-if #'whitespace-character-p source :start start)
      (length source)))

;;; -------------------------------------------------------

(define-function locate-next-word
  ((:accepts (source :type string))
   (:accepts (start  :type fixnum))
   (:returns (fixnum fixnum)))
  "Proceeding from the START position into the SOURCE, seeks the next
   whitespace-delimited word and returns two values:
     (1) The position of the word's first character in the SOURCE.
     (2) The position into the SOURCE immediately succeeding the
         detected word's occupied segment."
  (let* ((start-of-word (locate-start-of-word source start))
         (end-of-word   (locate-end-of-word   source start-of-word)))
    (declare (type fixnum start-of-word))
    (declare (type fixnum end-of-word))
    (values start-of-word end-of-word)))

;;; -------------------------------------------------------

(define-function parse-command-type
  ((:accepts (source   :type string))
   (:accepts (position :type fixnum))
   (:returns command-type))
  "Parses the character at the POSITION into the SOURCE as a command
   type specifier and returns a covenable representation thereof."
  (if (array-in-bounds-p source position)
    (case (char source position)
      (#\r       :move-pointer-right)
      (#\l       :move-pointer-left)
      (#\u       :increment)
      (#\d       :decrement)
      (#\p       :print-on-fresh-line)
      (#\w       :print-on-same-line)
      (#\i       :skip-if-zero)
      (#\n       :input-number)
      (#\c       :input-character)
      (otherwise :nop))
    (error "Missing command type at position ~d." position)))

;;; -------------------------------------------------------

(define-function parse-index-argument
  ((:accepts (source   :type string))
   (:accepts (position :type fixnum))
   (:returns command-argument))
  "Parses the character at the POSITION into the SOURCE as a cell index
   specifier and returns a covenable object representation thereof."
  (if (array-in-bounds-p source position)
    (let ((identifier (char source position)))
      (declare (type character identifier))
      (or (digit-char-p identifier)
          (and (char= identifier #\p) :pointer)
          (error "Invalid command argument at position ~d." position)))
    (error "Missing command argument at position ~d." position)))

;;; -------------------------------------------------------

(define-function parse-numeric-argument
  ((:accepts (source   :type string))
   (:accepts (position :type fixnum))
   (:returns ((integer 0 9))))
  "Parses the character at the POSITION into the SOURCE as a numeric
   command argument and returns a convenable object representation
   thereof."
  (if (array-in-bounds-p source position)
    (let ((identifier (char source position)))
      (declare (type character identifier))
      (or (digit-char-p identifier)
          (error "Invalid command argument at position ~d." position)))
    (error "Missing command argument at position ~d." position)))

;;; -------------------------------------------------------

(define-function parse-command
  ((:accepts (source :type string))
   (:accepts (start  :type fixnum))
   (:returns command))
  "Proceeding from the START position into the SOURCE, parses a Targs
   command and returns a ``command'' representation thereof."
  (let ((command-type (parse-command-type source start)))
    (declare (type command-type command-type))
    (case command-type
      ;; Command without an operand?
      (:nop
        (cons :nop NIL))
      ;; Command with a strictly numeric operand?
      ((:move-pointer-right :move-pointer-left :increment :decrement)
        (cons command-type
          (parse-numeric-argument source
            (1+ start))))
      ;; Command with a cell index as an operand?
      ((:print-on-fresh-line :print-on-same-line
        :input-number        :input-character
        :skip-if-zero)
          (cons command-type
            (parse-index-argument source
              (1+ start))))
      ;; Unrecognized command?
      (otherwise
        (error "Unrecognized command type: ~s." command-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function make-memory
  ((:returns memory))
  "Creates and returns a fresh ``memory'' object with the default
   capacity."
  (make-array 10
    :element-type    'integer
    :initial-element 0
    :adjustable      NIL
    :fill-pointer    NIL))

;;; -------------------------------------------------------

(define-function cell-pointer-p
  ((:accepts (index :type command-argument))
   (:returns boolean))
  "Determines whether the INDEX designates the memory cell pointer,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (get-boolean-value-of
    (and (keywordp index)
         (eq index :pointer))))

;;; -------------------------------------------------------

(define-function interpret-Targs
  ((:accepts (source :type string))
   (:returns null))
  "Interprets the piece of Targs SOURCE code and returns no value."
  (let ((position             0)
        (skips-next-command-p NIL)
        (memory               (make-memory))
        (cell-pointer         0))
    (declare (type fixnum        position))
    (declare (type boolean       skips-next-command-p))
    (declare (type memory        memory))
    (declare (type (integer 0 *) cell-pointer))
    
    (labels
        ((resize-memory-if-necessary ()
          "Ensures that the CELL-POINTER designates a valid index by
           doubling its capacity if necessary, in any case returning
           no value."
          (when (>= cell-pointer (length memory))
            (setf memory
              (adjust-array memory
                (* (length memory) 2)
                :initial-element 0)))
          (values))
         
         (cell-value-at (index)
          "Returns the numeric value stored in the MEMORY cell at the
           INDEX."
          (declare (type command-argument index))
          (the integer
            (cond
              ((integerp index)
                (aref memory index))
              ((cell-pointer-p index)
                (aref memory cell-pointer))
              (T
                (error "Invalid memory index: ~s." index)))))
         
         ((setf cell-value-at) (new-value index)
          "Stores the NEW-VALUE in the MEMORY cell at the INDEX and
           returns no value."
          (declare (type integer          new-value))
          (declare (ignorable             new-value))
          (declare (type command-argument index))
          (cond
            ((integerp index)
              (setf (aref memory index) new-value))
            ((cell-pointer-p index)
              (setf (aref memory cell-pointer) new-value))
            (T
              (error "Invalid memory index: ~s." index)))
          (values)))
      
      (loop while (< position (length source)) do
        (multiple-value-bind (word-start word-end)
            (locate-next-word source position)
          (declare (type fixnum word-start))
          (declare (type fixnum word-end))
          
          (destructuring-bind (command-type . command-argument)
              (parse-command source word-start)
            (declare (type command-type     command-type))
            (declare (type command-argument command-argument))
            (declare (ignorable             command-argument))
            
            (cond
              ((eq command-type :nop)
                NIL)
              
              (skips-next-command-p
                (setf skips-next-command-p NIL))
              
              (T
                (case command-type
                  (:move-pointer-right
                    (incf cell-pointer command-argument)
                    (resize-memory-if-necessary))
                  
                  (:move-pointer-left
                    (setf cell-pointer
                      (max 0
                        (- cell-pointer command-argument))))
                  
                  (:increment
                    (incf (cell-value-at cell-pointer)
                          command-argument))
                  
                  (:decrement
                    (decf (cell-value-at cell-pointer)
                          command-argument))
                  
                  (:print-on-fresh-line
                    (format T "~%~c"
                      (code-char
                        (cell-value-at command-argument))))
                  
                  (:print-on-same-line
                    (format T "~c"
                      (code-char
                        (cell-value-at command-argument))))
                  
                  (:input-number
                    (format T "~&Please input an integer: ")
                    (finish-output)
                    (clear-input)
                    (setf (cell-value-at command-argument)
                      (handler-case
                        (parse-integer
                          (read-line))
                        (error () 0)))
                    (clear-input))
                  
                  (:input-character
                    (format T "~&Please input a character: ")
                    (finish-output)
                    (setf (cell-value-at command-argument)
                      (char-code
                        (read-char)))
                    (clear-input))
                  
                  (:skip-if-zero
                    (when (zerop (cell-value-at command-argument))
                      (setf skips-next-command-p T)))
                  
                  (:nop
                    NIL)
                  
                  (otherwise
                    (error "Invalid command type ~s at position ~d."
                      command-type word-start))))))
            
            (setf position word-end)))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program loaders.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function load-Targs-program
  ((:accepts (source :type (or pathname stream string)))
   (:returns null))
  "Loads the Targs program from the SOURCE, interprets it, and returns
   no value."
  (with-open-file (input-stream source
                   :direction         :input
                   :element-type      'character
                   :if-does-not-exist :error)
    (declare (type file-stream input-stream))
    (interpret-Targs
      (with-output-to-string (file-content)
        (declare (type string-stream file-content))
        (loop
          for character
            of-type (or null character)
            =       (read-char input-stream NIL NIL)
          while character
          do (write-char character file-content)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Targs
  "u9 u9 u9 u9 u9 u9 u9 u9 u9 u9 u9 u5 p0 u1 i0 w0 r1 u9 u9 u9 u9 u9 u9 u9 u9 p1 l1 d4 w0 u7 w0 w0 u3 w0")

;;; -------------------------------------------------------

;; One-time character-based cat program.
(interpret-Targs "c0 w0")

;;; -------------------------------------------------------

;; One-time numeric cat program which outputs the corresponding ASCII
;; character.
(interpret-Targs "n0 w0")

;;; -------------------------------------------------------

;; One-time numeric cat program which print the user input's character
;; form only if the same does not constitute the zero (0) value.
(interpret-Targs "n0 i0 w0")
